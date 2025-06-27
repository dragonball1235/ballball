ProgramVersion := "NEW 1.1"
#SingleInstance ignore
#NoEnv
#Persistent
#KeyHistory 0
#NoTrayIcon
#Warn All, Off
ListLines, OFF
SetBatchLines, -1
SetWinDelay, 0
SetControlDelay, 0
DetectHiddenText, On
DetectHiddenWindows, On
CoordMode, Mouse, Client
CoordMode, pixel, Client
SetKeyDelay, -1
SetMouseDelay, -1
SetDefaultMouseSpeed, 0
SetTitleMatchMode, 3
if not A_IsAdmin {
Run *RunAs "%A_ScriptFullPath%"
ExitApp
}
ON:
global TotalPhys,TotalPhy
VarSetCapacity( MEMORYSTATUSEX,64,0 ), NumPut( 64,MEMORYSTATUSEX )
DllCall( "GlobalMemoryStatusEx", UInt,&MEMORYSTATUSEX )
TotalPhys := NumGet( MEMORYSTATUSEX,8,"Int64"),  VarSetCapacity( PhysMem,16,0 )
DllCall( "shlwapi.dll\StrFormatByteSize64A", Int64,TotalPhys, Str,PhysMem, UInt,16 )
Global WinVersion := GetOSVersion()
PROCESS,Priority,,High
DllCall("psapi.dll\EmptyWorkingSet", "Ptr", -1)
Sleep, 100
IfNotExist,%A_ScriptDir%\CashMemory.exe
{
FileInstall,CashMemory.exe, %A_ScriptDir%\CashMemory.exe
Loop, %A_ScriptDir%\CashMemory.exe
{
break
}
}
class Chrome
{
	static DebugPort := 9222

	/*
		Escape a string in a manner suitable for command line parameters
	*/
	CliEscape(Param)
	{
		return """" RegExReplace(Param, "(\\*)""", "$1$1\""") """"
	}

	/*
		Finds instances of chrome in debug mode and the ports they're running
		on. If no instances are found, returns a false value. If one or more
		instances are found, returns an associative array where the keys are
		the ports, and the values are the full command line texts used to start
		the processes.

		One example of how this may be used would be to open chrome on a
		different port if an instance of chrome is already open on the port
		you wanted to used.

		```
		; If the wanted port is taken, use the largest taken port plus one
		DebugPort := 9222
		if (Chromes := Chrome.FindInstances()).HasKey(DebugPort)
			DebugPort := Chromes.MaxIndex() + 1
		ChromeInst := new Chrome(ProfilePath,,,, DebugPort)
		```

		Another use would be to scan for running instances and attach to one
		instead of starting a new instance.

		```
		if (Chromes := Chrome.FindInstances())
			ChromeInst := {"base": Chrome, "DebugPort": Chromes.MinIndex()}
		else
			ChromeInst := new Chrome(ProfilePath)
		```
	*/
	FindInstances()
	{
		static Needle := "--remote-debugging-port=(\d+)"
		Out := {}
		for Item in ComObjGet("winmgmts:")
			.ExecQuery("SELECT CommandLine FROM Win32_Process"
			. " WHERE Name = 'chrome.exe'")
			if RegExMatch(Item.CommandLine, Needle, Match)
				Out[Match1] := Item.CommandLine
		return Out.MaxIndex() ? Out : False
	}

	/*
		ProfilePath - Path to the user profile directory to use. Will use the standard if left blank.
		URLs        - The page or array of pages for Chrome to load when it opens
		Flags       - Additional flags for chrome when launching
		ChromePath  - Path to chrome.exe, will detect from start menu when left blank
		DebugPort   - What port should Chrome's remote debugging server run on
	*/
	__New(ProfilePath:="", URLs:="about:blank", Flags:="", ChromePath:="", DebugPort:="", headless:=False)
	{
		; Verify ProfilePath
		if (ProfilePath != "" && !InStr(FileExist(ProfilePath), "D"))
			throw Exception("The given ProfilePath does not exist")
		this.ProfilePath := ProfilePath

		; Verify ChromePath
		if (ChromePath == "")
			FileGetShortcut, %A_StartMenuCommon%\Programs\Google Chrome.lnk, ChromePath
		if (ChromePath == "")
			RegRead, ChromePath, HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\chrome.exe
		if !FileExist(ChromePath)
			throw Exception("Chrome could not be found")
		this.ChromePath := ChromePath

		; Verify DebugPort
		if (DebugPort != "")
		{
			if DebugPort is not integer
				throw Exception("DebugPort must be a positive integer")
			else if (DebugPort <= 0)
				throw Exception("DebugPort must be a positive integer")
			this.DebugPort := DebugPort
		}

		if(headless)
		{
			headless_option := " --headless --disable-gpu --window-size=1920x1080"
		}
		Else
		{
			headless_option := ""
		}

		; Escape the URL(s)
		for Index, URL in IsObject(URLs) ? URLs : [URLs]
			URLString .= " " this.CliEscape(URL)

		Run, % this.CliEscape(ChromePath)
		. headless_option . " --remote-debugging-port=" this.DebugPort
		. (ProfilePath ? " --user-data-dir=" this.CliEscape(ProfilePath) : "")
		. (Flags ? " " Flags : "")
		. URLString
		,,, OutputVarPID
		this.PID := OutputVarPID
	}

	/*
		End Chrome by terminating the process.
	*/
	Kill()
	{
		Process, Close, % this.PID
	}

	/*
		Queries chrome for a list of pages that expose a debug interface.
		In addition to standard tabs, these include pages such as extension
		configuration pages.
	*/
	GetPageList()
	{
		http := ComObjCreate("WinHttp.WinHttpRequest.5.1")
		http.open("GET", "http://127.0.0.1:" this.DebugPort "/json")
		http.send()
		return this.Jxon_Load(http.responseText)
	}

	/*
		Returns a connection to the debug interface of a page that matches the
		provided criteria. When multiple pages match the criteria, they appear
		ordered by how recently the pages were opened.

		Key        - The key from the page list to search for, such as "url" or "title"
		Value      - The value to search for in the provided key
		MatchMode  - What kind of search to use, such as "exact", "contains", "startswith", or "regex"
		Index      - If multiple pages match the given criteria, which one of them to return
		fnCallback - A function to be called whenever message is received from the page
	*/
	GetPageBy(Key, Value, MatchMode:="exact", Index:=1, fnCallback:="", fnClose:="")
	{
		Count := 0
		for n, PageData in this.GetPageList()
		{
			if (((MatchMode = "exact" && PageData[Key] = Value) ; Case insensitive
			|| (MatchMode = "contains" && InStr(PageData[Key], Value))
			|| (MatchMode = "startswith" && InStr(PageData[Key], Value) == 1)
			|| (MatchMode = "regex" && PageData[Key] ~= Value))
			&& ++Count == Index)
				return new this.Page(PageData.webSocketDebuggerUrl, fnCallback, fnClose)
		}
	}

	/*
		Shorthand for GetPageBy("url", Value, "startswith")
	*/
	GetPageByURL(Value, MatchMode:="startswith", Index:=1, fnCallback:="", fnClose:="")
	{
		return this.GetPageBy("url", Value, MatchMode, Index, fnCallback, fnClose)
	}

	/*
		Shorthand for GetPageBy("title", Value, "startswith")
	*/
	GetPageByTitle(Value, MatchMode:="startswith", Index:=1, fnCallback:="", fnClose:="")
	{
		return this.GetPageBy("title", Value, MatchMode, Index, fnCallback, fnClose)
	}

	/*
		Shorthand for GetPageBy("type", Type, "exact")

		The default type to search for is "page", which is the visible area of
		a normal Chrome tab.
	*/
	GetPage(Index:=1, Type:="page", fnCallback:="", fnClose:="")
	{
		return this.GetPageBy("type", Type, "exact", Index, fnCallback, fnClose)
	}

	/*
		Connects to the debug interface of a page given its WebSocket URL.
	*/
	class Page
	{
		Connected := False
		ID := 0
		Responses := []

		/*
			wsurl      - The desired page's WebSocket URL
			fnCallback - A function to be called whenever message is received
			fnClose    - A function to be called whenever the page connection is lost
		*/
		__New(wsurl, fnCallback:="", fnClose:="")
		{
			this.fnCallback := fnCallback
			this.fnClose := fnClose
			this.BoundKeepAlive := this.Call.Bind(this, "Browser.getVersion",, False)

			; TODO: Throw exception on invalid objects
			if IsObject(wsurl)
				wsurl := wsurl.webSocketDebuggerUrl

			wsurl := StrReplace(wsurl, "localhost", "127.0.0.1")
			this.ws := {"base": this.WebSocket, "_Event": this.Event, "Parent": this}
			this.ws.__New(wsurl)

			while !this.Connected
				Sleep, 50
		}

		/*
			Calls the specified endpoint and provides it with the given
			parameters.

			DomainAndMethod - The endpoint domain and method name for the
				endpoint you would like to call. For example:
				PageInst.Call("Browser.close")
				PageInst.Call("Schema.getDomains")

			Params - An associative array of parameters to be provided to the
				endpoint. For example:
				PageInst.Call("Page.printToPDF", {"scale": 0.5 ; Numeric Value
					, "landscape": Chrome.Jxon_True() ; Boolean Value
					, "pageRanges: "1-5, 8, 11-13"}) ; String value
				PageInst.Call("Page.navigate", {"url": "https://autohotkey.com/"})

			WaitForResponse - Whether to block until a response is received from
				Chrome, which is necessary to receive a return value, or whether
				to continue on with the script without waiting for a response.
		*/
		Call(DomainAndMethod, Params:="", WaitForResponse:=True)
		{
			if !this.Connected
				throw Exception("Not connected to tab")

			; Use a temporary variable for ID in case more calls are made
			; before we receive a response.
			ID := this.ID += 1
			this.ws.Send(Chrome.Jxon_Dump({"id": ID
			, "params": Params ? Params : {}
			, "method": DomainAndMethod}))

			if !WaitForResponse
				return

			; Wait for the response
			this.responses[ID] := False
			while !this.responses[ID]
				Sleep, 50

			; Get the response, check if it's an error
			response := this.responses.Delete(ID)
			if (response.error)
				throw Exception("Chrome indicated error in response",, Chrome.Jxon_Dump(response.error))

			return response.result
		}

		/*
			Run some JavaScript on the page. For example:

			PageInst.Evaluate("alert(""I can't believe it's not IE!"");")
			PageInst.Evaluate("document.getElementsByTagName('button')[0].click();")
		*/
		Evaluate(JS)
		{
			response := this.Call("Runtime.evaluate",
			( LTrim Join
			{
				"expression": JS,
				"objectGroup": "console",
				"includeCommandLineAPI": Chrome.Jxon_True(),
				"silent": Chrome.Jxon_False(),
				"returnByValue": Chrome.Jxon_False(),
				"userGesture": Chrome.Jxon_True(),
				"awaitPromise": Chrome.Jxon_False()
			}
			))

			if (response.exceptionDetails)
				throw Exception(response.result.description, -1
					, Chrome.Jxon_Dump({"Code": JS
					, "exceptionDetails": response.exceptionDetails}))

			return response.result
		}

		/*
			Waits for the page's readyState to match the DesiredState.

			DesiredState - The state to wait for the page's ReadyState to match
			Interval     - How often it should check whether the state matches
		*/
		WaitForLoad(DesiredState:="complete", Interval:=100)
		{
			while this.Evaluate("document.readyState").value != DesiredState
				Sleep, Interval
		}

		/*
			Internal function triggered when the script receives a message on
			the WebSocket connected to the page.
		*/
		Event(EventName, Event)
		{
			; If it was called from the WebSocket adjust the class context
			if this.Parent
				this := this.Parent

			; TODO: Handle Error events
			if (EventName == "Open")
			{
				this.Connected := True
				BoundKeepAlive := this.BoundKeepAlive
				SetTimer, %BoundKeepAlive%, 15000
			}
			else if (EventName == "Message")
			{
				data := Chrome.Jxon_Load(Event.data)

				; Run the callback routine
				fnCallback := this.fnCallback
				if (newData := %fnCallback%(data))
					data := newData

				if this.responses.HasKey(data.ID)
					this.responses[data.ID] := data
			}
			else if (EventName == "Close")
			{
				this.Disconnect()
				fnClose := this.fnClose
				%fnClose%(this)
			}
		}

		/*
			Disconnect from the page's debug interface, allowing the instance
			to be garbage collected.

			This method should always be called when you are finished with a
			page or else your script will leak memory.
		*/
		Disconnect()
		{
			if !this.Connected
				return

			this.Connected := False
			this.ws.Delete("Parent")
			this.ws.Disconnect()

			BoundKeepAlive := this.BoundKeepAlive
			SetTimer, %BoundKeepAlive%, Delete
			this.Delete("BoundKeepAlive")
		}

		class WebSocket
{
	__New(WS_URL)
	{
		static wb

		; Create an IE instance
		Gui, +hWndhOld
		Gui, New, +hWndhWnd
		this.hWnd := hWnd
		Gui, Add, ActiveX, vWB, Shell.Explorer
		Gui, %hOld%: Default

		; Write an appropriate document
		WB.Navigate("about:<!DOCTYPE html><meta http-equiv='X-UA-Compatible'"
		. "content='IE=edge'><body></body>")
		while (WB.ReadyState < 4)
			sleep, 50
		this.document := WB.document

		; Add our handlers to the JavaScript namespace
		this.document.parentWindow.ahk_savews := this._SaveWS.Bind(this)
		this.document.parentWindow.ahk_event := this._Event.Bind(this)
		this.document.parentWindow.ahk_ws_url := WS_URL

		; Add some JavaScript to the page to open a socket
		Script := this.document.createElement("script")
		Script.text := "ws = new WebSocket(ahk_ws_url);`n"
		. "ws.onopen = function(event){ ahk_event('Open', event); };`n"
		. "ws.onclose = function(event){ ahk_event('Close', event); };`n"
		. "ws.onerror = function(event){ ahk_event('Error', event); };`n"
		. "ws.onmessage = function(event){ ahk_event('Message', event); };"
		this.document.body.appendChild(Script)
	}

	; Called by the JS in response to WS events
	_Event(EventName, Event)
	{
		this["On" EventName](Event)
	}

	; Sends data through the WebSocket
	Send(Data)
	{
		this.document.parentWindow.ws.send(Data)
	}

	; Closes the WebSocket connection
	Close(Code:=1000, Reason:="")
	{
		this.document.parentWindow.ws.close(Code, Reason)
	}

	; Closes and deletes the WebSocket, removing
	; references so the class can be garbage collected
	Disconnect()
	{
		if this.hWnd
		{
			this.Close()
			Gui, % this.hWnd ": Destroy"
			this.hWnd := False
		}
	}
}

	}

	Jxon_Load(ByRef src, args*)
{
	static q := Chr(34)

	key := "", is_key := false
	stack := [ tree := [] ]
	is_arr := { (tree): 1 }
	next := q . "{[01234567890-tfn"
	pos := 0
	while ( (ch := SubStr(src, ++pos, 1)) != "" )
	{
		if InStr(" `t`n`r", ch)
			continue
		if !InStr(next, ch, true)
		{
			ln := ObjLength(StrSplit(SubStr(src, 1, pos), "`n"))
			col := pos - InStr(src, "`n",, -(StrLen(src)-pos+1))

			msg := Format("{}: line {} col {} (char {})"
			,   (next == "")      ? ["Extra data", ch := SubStr(src, pos)][1]
			  : (next == "'")     ? "Unterminated string starting at"
			  : (next == "\")     ? "Invalid \escape"
			  : (next == ":")     ? "Expecting ':' delimiter"
			  : (next == q)       ? "Expecting object key enclosed in double quotes"
			  : (next == q . "}") ? "Expecting object key enclosed in double quotes or object closing '}'"
			  : (next == ",}")    ? "Expecting ',' delimiter or object closing '}'"
			  : (next == ",]")    ? "Expecting ',' delimiter or array closing ']'"
			  : [ "Expecting JSON value(string, number, [true, false, null], object or array)"
			    , ch := SubStr(src, pos, (SubStr(src, pos)~="[\]\},\s]|$")-1) ][1]
			, ln, col, pos)

			throw Exception(msg, -1, ch)
		}

		is_array := is_arr[obj := stack[1]]

		if i := InStr("{[", ch)
		{
			val := (proto := args[i]) ? new proto : {}
			is_array? ObjPush(obj, val) : obj[key] := val
			ObjInsertAt(stack, 1, val)

			is_arr[val] := !(is_key := ch == "{")
			next := q . (is_key ? "}" : "{[]0123456789-tfn")
		}

		else if InStr("}]", ch)
		{
			ObjRemoveAt(stack, 1)
			next := stack[1]==tree ? "" : is_arr[stack[1]] ? ",]" : ",}"
		}

		else if InStr(",:", ch)
		{
			is_key := (!is_array && ch == ",")
			next := is_key ? q : q . "{[0123456789-tfn"
		}

		else ; string | number | true | false | null
		{
			if (ch == q) ; string
			{
				i := pos
				while i := InStr(src, q,, i+1)
				{
					val := StrReplace(SubStr(src, pos+1, i-pos-1), "\\", "\u005C")
					static end := A_AhkVersion<"2" ? 0 : -1
					if (SubStr(val, end) != "\")
						break
				}
				if !i ? (pos--, next := "'") : 0
					continue

				pos := i ; update pos

				  val := StrReplace(val,    "\/",  "/")
				, val := StrReplace(val, "\" . q,    q)
				, val := StrReplace(val,    "\b", "`b")
				, val := StrReplace(val,    "\f", "`f")
				, val := StrReplace(val,    "\n", "`n")
				, val := StrReplace(val,    "\r", "`r")
				, val := StrReplace(val,    "\t", "`t")

				i := 0
				while i := InStr(val, "\",, i+1)
				{
					if (SubStr(val, i+1, 1) != "u") ? (pos -= StrLen(SubStr(val, i)), next := "\") : 0
						continue 2

					; \uXXXX - JSON unicode escape sequence
					xxxx := Abs("0x" . SubStr(val, i+2, 4))
					if (A_IsUnicode || xxxx < 0x100)
						val := SubStr(val, 1, i-1) . Chr(xxxx) . SubStr(val, i+6)
				}

				if is_key
				{
					key := val, next := ":"
					continue
				}
			}

			else ; number | true | false | null
			{
				val := SubStr(src, pos, i := RegExMatch(src, "[\]\},\s]|$",, pos)-pos)

			; For numerical values, numerify integers and keep floats as is.
			; I'm not yet sure if I should numerify floats in v2.0-a ...
				static number := "number", integer := "integer"
				if val is %number%
				{
					if val is %integer%
						val += 0
				}
			; in v1.1, true,false,A_PtrSize,A_IsUnicode,A_Index,A_EventInfo,
			; SOMETIMES return strings due to certain optimizations. Since it
			; is just 'SOMETIMES', numerify to be consistent w/ v2.0-a
				else if (val == "true" || val == "false")
					val := %value% + 0
			; AHK_H has built-in null, can't do 'val := %value%' where value == "null"
			; as it would raise an exception in AHK_H(overriding built-in var)
				else if (val == "null")
					val := ""
			; any other values are invalid, continue to trigger error
				else if (pos--, next := "#")
					continue

				pos += i-1
			}

			is_array? ObjPush(obj, val) : obj[key] := val
			next := obj==tree ? "" : is_array ? ",]" : ",}"
		}
	}

	return tree[1]
}

Jxon_Dump(obj, indent:="", lvl:=1)
{
	static q := Chr(34)

	if IsObject(obj)
	{
		static Type := Func("Type")
		if Type ? (Type.Call(obj) != "Object") : (ObjGetCapacity(obj) == "")
			throw Exception("Object type not supported.", -1, Format("<Object at 0x{:p}>", &obj))

		prefix := SubStr(A_ThisFunc, 1, InStr(A_ThisFunc, ".",, 0))
		fn_t := prefix "Jxon_True",  obj_t := this ? %fn_t%(this) : %fn_t%()
		fn_f := prefix "Jxon_False", obj_f := this ? %fn_f%(this) : %fn_f%()

		if (&obj == &obj_t)
			return "true"
		else if (&obj == &obj_f)
			return "false"

		is_array := 0
		for k in obj
			is_array := k == A_Index
		until !is_array

		static integer := "integer"
		if indent is %integer%
		{
			if (indent < 0)
				throw Exception("Indent parameter must be a postive integer.", -1, indent)
			spaces := indent, indent := ""
			Loop % spaces
				indent .= " "
		}
		indt := ""
		Loop, % indent ? lvl : 0
			indt .= indent

		this_fn := this ? Func(A_ThisFunc).Bind(this) : A_ThisFunc
		lvl += 1, out := "" ; Make #Warn happy
		for k, v in obj
		{
			if IsObject(k) || (k == "")
				throw Exception("Invalid object key.", -1, k ? Format("<Object at 0x{:p}>", &obj) : "<blank>")

			if !is_array
				out .= ( ObjGetCapacity([k], 1) ? %this_fn%(k) : q . k . q ) ;// key
				    .  ( indent ? ": " : ":" ) ; token + padding
			out .= %this_fn%(v, indent, lvl) ; value
			    .  ( indent ? ",`n" . indt : "," ) ; token + indent
		}

		if (out != "")
		{
			out := Trim(out, ",`n" . indent)
			if (indent != "")
				out := "`n" . indt . out . "`n" . SubStr(indt, StrLen(indent)+1)
		}

		return is_array ? "[" . out . "]" : "{" . out . "}"
	}

	; Number
	else if (ObjGetCapacity([obj], 1) == "")
		return obj

	; String (null -> not supported by AHK)
	if (obj != "")
	{
		  obj := StrReplace(obj,  "\",    "\\")
		, obj := StrReplace(obj,  "/",    "\/")
		, obj := StrReplace(obj,    q, "\" . q)
		, obj := StrReplace(obj, "`b",    "\b")
		, obj := StrReplace(obj, "`f",    "\f")
		, obj := StrReplace(obj, "`n",    "\n")
		, obj := StrReplace(obj, "`r",    "\r")
		, obj := StrReplace(obj, "`t",    "\t")

		static needle := (A_AhkVersion<"2" ? "O)" : "") . "[^\x20-\x7e]"
		while RegExMatch(obj, needle, m)
			obj := StrReplace(obj, m[0], Format("\u{:04X}", Ord(m[0])))
	}

	return q . obj . q
}

Jxon_True()
{
	static obj := {}
	return obj
}

Jxon_False()
{
	static obj := {}
	return obj
}
}

class _ClassMemory
{
static baseAddress, hProcess, PID, currentProgram
, insertNullTerminator := True
, readStringLastError := False
, isTarget64bit := False
, ptrType := "UInt"
, aTypeSize := {    "UChar":    1,  "Char":     1
,   "UShort":   2,  "Short":    2
,   "UInt":     4,  "Int":      4
,   "UFloat":   4,  "Float":    4
,   "Int64":    8,  "Double":   8}
, aRights := {  "PROCESS_ALL_ACCESS": 0x001F0FFF
,   "PROCESS_CREATE_PROCESS": 0x0080
,   "PROCESS_CREATE_THREAD": 0x0002
,   "PROCESS_DUP_HANDLE": 0x0040
,   "PROCESS_QUERY_INFORMATION": 0x0400
,   "PROCESS_QUERY_LIMITED_INFORMATION": 0x1000
,   "PROCESS_SET_INFORMATION": 0x0200
,   "PROCESS_SET_QUOTA": 0x0100
,   "PROCESS_SUSPEND_RESUME": 0x0800
,   "PROCESS_TERMINATE": 0x0001
,   "PROCESS_VM_OPERATION": 0x0008
,   "PROCESS_VM_READ": 0x0010
,   "PROCESS_VM_WRITE": 0x0020
,   "SYNCHRONIZE": 0x00100000}
__new(program, dwDesiredAccess := "", byRef handle := "", windowMatchMode := 3)
{
if this.PID := handle := this.findPID(program, windowMatchMode)
{
if dwDesiredAccess is not integer
dwDesiredAccess := this.aRights.PROCESS_QUERY_INFORMATION | this.aRights.PROCESS_VM_OPERATION | this.aRights.PROCESS_VM_READ | this.aRights.PROCESS_VM_WRITE
dwDesiredAccess |= this.aRights.SYNCHRONIZE
if this.hProcess := handle := this.OpenProcess(this.PID, dwDesiredAccess)
{
this.pNumberOfBytesRead := DllCall("GlobalAlloc", "UInt", 0x0040, "Ptr", A_PtrSize, "Ptr")
this.pNumberOfBytesWritten := DllCall("GlobalAlloc", "UInt", 0x0040, "Ptr", A_PtrSize, "Ptr")
this.readStringLastError := False
this.currentProgram := program
if this.isTarget64bit := this.isTargetProcess64Bit(this.PID, this.hProcess, dwDesiredAccess)
this.ptrType := "Int64"
else this.ptrType := "UInt"
if (A_PtrSize != 4 || !this.isTarget64bit)
this.BaseAddress := this.getModuleBaseAddress()
if this.BaseAddress < 0 || !this.BaseAddress
this.BaseAddress := this.getProcessBaseAddress(program, windowMatchMode)
Return, this
}
}
return
}
__delete()
{
this.closeHandle(this.hProcess)
if this.pNumberOfBytesRead
DllCall("GlobalFree", "Ptr", this.pNumberOfBytesRead)
if this.pNumberOfBytesWritten
DllCall("GlobalFree", "Ptr", this.pNumberOfBytesWritten)
return
}
findPID(program, windowMatchMode := "3")
{
if RegExMatch(program, "i)\s*AHK_PID\s+(0x[[:xdigit:]]+|\d+)", pid)
Return, pid1
if windowMatchMode
{
mode := A_TitleMatchMode
StringReplace, windowMatchMode, windowMatchMode, 0x
SetTitleMatchMode, %windowMatchMode%
}
WinGet, pid, pid, %program%
if windowMatchMode
SetTitleMatchMode, %mode%
if (!pid && RegExMatch(program, "i)\bAHK_EXE\b\s*(.*)", fileName))
{
filename := RegExReplace(filename1, "i)\bahk_(class|id|pid|group)\b.*", "")
filename := trim(filename)
SplitPath, fileName, fileName
if (fileName)
{
Process, Exist, %fileName%
pid := ErrorLevel
}
}
Return, pid ? pid : 0
}
isHandleValid()
{
Return, 0x102 = DllCall("WaitForSingleObject", "Ptr", this.hProcess, "UInt", 0)
}
openProcess(PID, dwDesiredAccess)
{
r := DllCall("OpenProcess", "UInt", dwDesiredAccess, "Int", False, "UInt", PID, "Ptr")
if (!r && A_LastError = 5)
{
this.setSeDebugPrivilege(true)
if (r2 := DllCall("OpenProcess", "UInt", dwDesiredAccess, "Int", False, "UInt", PID, "Ptr"))
Return, r2
DllCall("SetLastError", "UInt", 5)
}
Return, r ? r : ""
}
closeHandle(hProcess)
{
Return, DllCall("CloseHandle", "Ptr", hProcess)
}
numberOfBytesRead()
{
Return, !this.pNumberOfBytesRead ? -1 : NumGet(this.pNumberOfBytesRead+0, "Ptr")
}
numberOfBytesWritten()
{
Return, !this.pNumberOfBytesWritten ? -1 : NumGet(this.pNumberOfBytesWritten+0, "Ptr")
}
read(address, type := "UInt", aOffsets*)
{
if !this.aTypeSize.hasKey(type)
Return, "", ErrorLevel := -2
if DllCall("ReadProcessMemory", "Ptr", this.hProcess, "Ptr", aOffsets.maxIndex() ? this.getAddressFromOffsets(address, aOffsets*) : address, type "*", result, "Ptr", this.aTypeSize[type], "Ptr", this.pNumberOfBytesRead)
Return, result
return
}
readRaw(address, byRef buffer, bytes := 4, aOffsets*)
{
VarSetCapacity(buffer, bytes)
Return, DllCall("ReadProcessMemory", "Ptr", this.hProcess, "Ptr", aOffsets.maxIndex() ? this.getAddressFromOffsets(address, aOffsets*) : address, "Ptr", &buffer, "Ptr", bytes, "Ptr", this.pNumberOfBytesRead)
}
readString(address, sizeBytes := 0, encoding := "UTF-8", aOffsets*)
{
bufferSize := VarSetCapacity(buffer, sizeBytes ? sizeBytes : 100, 0)
this.ReadStringLastError := False
if aOffsets.maxIndex()
address := this.getAddressFromOffsets(address, aOffsets*)
if !sizeBytes
{
if (encoding = "utf-16" || encoding = "cp1200")
encodingSize := 2, charType := "UShort", loopCount := 2
else encodingSize := 1, charType := "Char", loopCount := 4
Loop
{
if !DllCall("ReadProcessMemory", "Ptr", this.hProcess, "Ptr", address + ((outterIndex := A_index) - 1) * 4, "Ptr", &buffer, "Ptr", 4, "Ptr", this.pNumberOfBytesRead) || ErrorLevel
Return, "", this.ReadStringLastError := True
else loop, %loopCount%
{
if NumGet(buffer, (A_Index - 1) * encodingSize, charType) = 0
{
if (bufferSize < sizeBytes := outterIndex * 4 - (4 - A_Index * encodingSize))
VarSetCapacity(buffer, sizeBytes)
Break, 2
}
}
}
}
if DllCall("ReadProcessMemory", "Ptr", this.hProcess, "Ptr", address, "Ptr", &buffer, "Ptr", sizeBytes, "Ptr", this.pNumberOfBytesRead)
Return, StrGet(&buffer,, encoding)
Return, "", this.ReadStringLastError := True
}
executable(address, RegionSize) {
PAGE_EXECUTE_READWRITE := 0x40
DllCall("VirtualProtectEx", "Ptr", this.hProcess, "Ptr", address, "Ptr", RegionSize, "Ptr", PAGE_EXECUTE_READWRITE, "Ptr*", oldProtection)
}
write7bytes(address, value) {
return DllCall("WriteProcessMemory", "UInt", this.hProcess, "UInt", address, "Uint*", value, "Uint", 07, "Uint*", 0)
}
writeString(address, string, encoding := "utf-8", aOffsets*)
{
encodingSize := (encoding = "utf-16" || encoding = "cp1200") ? 2 : 1
requiredSize := StrPut(string, encoding) * encodingSize - (this.insertNullTerminator ? 0 : encodingSize)
VarSetCapacity(buffer, requiredSize)
StrPut(string, &buffer, StrLen(string) + (this.insertNullTerminator ?  1 : 0), encoding)
Return, DllCall("WriteProcessMemory", "Ptr", this.hProcess, "Ptr", aOffsets.maxIndex() ? this.getAddressFromOffsets(address, aOffsets*) : address, "Ptr", &buffer, "Ptr", requiredSize, "Ptr", this.pNumberOfBytesWritten)
}
write(address, value, type := "Uint", aOffsets*)
{
if !this.aTypeSize.hasKey(type)
Return, "", ErrorLevel := -2
Return, DllCall("WriteProcessMemory", "Ptr", this.hProcess, "Ptr", aOffsets.maxIndex() ? this.getAddressFromOffsets(address, aOffsets*) : address, type "*", value, "Ptr", this.aTypeSize[type], "Ptr", this.pNumberOfBytesWritten)
}
writeRaw(address, pBuffer, sizeBytes, aOffsets*)
{
Return, DllCall("WriteProcessMemory", "Ptr", this.hProcess, "Ptr", aOffsets.maxIndex() ? this.getAddressFromOffsets(address, aOffsets*) : address, "Ptr", pBuffer, "Ptr", sizeBytes, "Ptr", this.pNumberOfBytesWritten)
}
writeBytes(address, hexStringOrByteArray, aOffsets*)
{
if !IsObject(hexStringOrByteArray)
{
if !IsObject(hexStringOrByteArray := this.hexStringToPattern(hexStringOrByteArray))
Return, hexStringOrByteArray
}
sizeBytes := this.getNeedleFromAOBPattern("", buffer, hexStringOrByteArray*)
Return, this.writeRaw(address, &buffer, sizeBytes, aOffsets*)
}
pointer(address, finalType := "UInt", offsets*)
{
For index, offset in offsets
address := this.Read(address, this.ptrType) + offset
Return, this.Read(address, finalType)
}
getAddressFromOffsets(address, aOffsets*)
{
Return, aOffsets.Remove() + this.pointer(address, this.ptrType, aOffsets*)
}
getProcessBaseAddress(windowTitle, windowMatchMode := "3")
{
if (windowMatchMode && A_TitleMatchMode != windowMatchMode)
{
mode := A_TitleMatchMode
StringReplace, windowMatchMode, windowMatchMode, 0x
SetTitleMatchMode, %windowMatchMode%
}
WinGet, hWnd, ID, %WindowTitle%
if mode
SetTitleMatchMode, %mode%
if !hWnd
return
Return, DllCall(A_PtrSize = 4 ? "GetWindowLong" : "GetWindowLongPtr", "Ptr", hWnd, "Int", -6, A_Is64bitOS ? "Int64" : "UInt")
}
getModuleBaseAddress(moduleName := "", byRef aModuleInfo := "")
{
aModuleInfo := ""
if (moduleName = "")
moduleName := this.GetModuleFileNameEx(0, True)
if r := this.getModules(aModules, True) < 0
Return, r
Return, aModules.HasKey(moduleName) ? (aModules[moduleName].lpBaseOfDll, aModuleInfo := aModules[moduleName]) : -1
}
getModuleFromAddress(address, byRef aModuleInfo, byRef offsetFromModuleBase := "")
{
aModuleInfo := offsetFromModule := ""
if result := this.getmodules(aModules) < 0
Return, result
for k, module in aModules
{
if (address >= module.lpBaseOfDll && address < module.lpBaseOfDll + module.SizeOfImage)
Return, 1, aModuleInfo := module, offsetFromModuleBase := address - module.lpBaseOfDll
}
Return, -1
}
setSeDebugPrivilege(enable := True)
{
h := DllCall("OpenProcess", "UInt", 0x0400, "Int", false, "UInt", DllCall("GetCurrentProcessId"), "Ptr")
DllCall("Advapi32.dll\OpenProcessToken", "Ptr", h, "UInt", 32, "PtrP", t)
VarSetCapacity(ti, 16, 0)
NumPut(1, ti, 0, "UInt")
DllCall("Advapi32.dll\LookupPrivilegeValue", "Ptr", 0, "Str", "SeDebugPrivilege", "Int64P", luid)
NumPut(luid, ti, 4, "Int64")
if enable
NumPut(2, ti, 12, "UInt")
r := DllCall("Advapi32.dll\AdjustTokenPrivileges", "Ptr", t, "Int", false, "Ptr", &ti, "UInt", 0, "Ptr", 0, "Ptr", 0)
DllCall("CloseHandle", "Ptr", t)
DllCall("CloseHandle", "Ptr", h)
Return, r
}
isTargetProcess64Bit(PID, hProcess := "", currentHandleAccess := "")
{
if !A_Is64bitOS
Return, False
else if !hProcess || !(currentHandleAccess & (this.aRights.PROCESS_QUERY_INFORMATION | this.aRights.PROCESS_QUERY_LIMITED_INFORMATION))
closeHandle := hProcess := this.openProcess(PID, this.aRights.PROCESS_QUERY_INFORMATION)
if (hProcess && DllCall("IsWow64Process", "Ptr", hProcess, "Int*", Wow64Process))
result := !Wow64Process
Return, result, closeHandle ? this.CloseHandle(hProcess) : ""
}
suspend()
{
Return, DllCall("ntdll\NtSuspendProcess", "Ptr", this.hProcess)
}
resume()
{
Return, DllCall("ntdll\NtResumeProcess", "Ptr", this.hProcess)
}
getModules(byRef aModules, useFileNameAsKey := False)
{
if (A_PtrSize = 4 && this.IsTarget64bit)
Return, -4
aModules := []
if !moduleCount := this.EnumProcessModulesEx(lphModule)
Return, -3
loop % moduleCount
{
this.GetModuleInformation(hModule := numget(lphModule, (A_index - 1) * A_PtrSize), aModuleInfo)
aModuleInfo.Name := this.GetModuleFileNameEx(hModule)
filePath := aModuleInfo.name
SplitPath, filePath, fileName
aModuleInfo.fileName := fileName
if useFileNameAsKey
aModules[fileName] := aModuleInfo
else aModules.insert(aModuleInfo)
}
Return, moduleCount
}
getEndAddressOfLastModule(byRef aModuleInfo := "")
{
if !moduleCount := this.EnumProcessModulesEx(lphModule)
Return, -3
hModule := numget(lphModule, (moduleCount - 1) * A_PtrSize)
if this.GetModuleInformation(hModule, aModuleInfo)
Return, aModuleInfo.lpBaseOfDll + aModuleInfo.SizeOfImage
Return, -5
}
GetModuleFileNameEx(hModule := 0, fileNameNoPath := False)
{
VarSetCapacity(lpFilename, 2048 * (A_IsUnicode ? 2 : 1))
DllCall("psapi\GetModuleFileNameEx", "Ptr", this.hProcess, "Ptr", hModule, "Str", lpFilename, "Uint", 2048 / (A_IsUnicode ? 2 : 1))
if fileNameNoPath
SplitPath, lpFilename, lpFilename
Return, lpFilename
}
EnumProcessModulesEx(byRef lphModule, dwFilterFlag := 0x03)
{
lastError := A_LastError
size := VarSetCapacity(lphModule, 4)
loop
{
DllCall("psapi\EnumProcessModulesEx", "Ptr", this.hProcess, "Ptr", &lphModule, "Uint", size, "Uint*", reqSize, "Uint", dwFilterFlag)
if ErrorLevel
Return, 0
else if (size >= reqSize)
break
else size := VarSetCapacity(lphModule, reqSize)
}
DllCall("SetLastError", "UInt", lastError)
Return, reqSize // A_PtrSize
}
GetModuleInformation(hModule, byRef aModuleInfo)
{
VarSetCapacity(MODULEINFO, A_PtrSize * 3), aModuleInfo := []
Return, DllCall("psapi\GetModuleInformation", "Ptr", this.hProcess, "Ptr", hModule, "Ptr", &MODULEINFO, "UInt", A_PtrSize * 3), aModuleInfo := {  lpBaseOfDll: numget(MODULEINFO, 0, "Ptr"),   SizeOfImage: numget(MODULEINFO, A_PtrSize, "UInt"),   EntryPoint: numget(MODULEINFO, A_PtrSize * 2, "Ptr") }
}
hexStringToPattern(hexString)
{
AOBPattern := []
hexString := RegExReplace(hexString, "(\s|0x)")
StringReplace, hexString, hexString, ?, ?, UseErrorLevel
wildCardCount := ErrorLevel
if !length := StrLen(hexString)
Return, -1
else if RegExMatch(hexString, "[^0-9a-fA-F?]")
Return, -2
else if Mod(wildCardCount, 2)
Return, -3
else if Mod(length, 2)
Return, -4
loop, % length/2
{
value := "0x" SubStr(hexString, 1 + 2 * (A_index-1), 2)
AOBPattern.Insert(value + 0 = "" ? "?" : value)
}
Return, AOBPattern
}
stringToPattern(string, encoding := "UTF-8", insertNullTerminator := False)
{
if !length := StrLen(string)
Return, -1
AOBPattern := []
encodingSize := (encoding = "utf-16" || encoding = "cp1200") ? 2 : 1
requiredSize := StrPut(string, encoding) * encodingSize - (insertNullTerminator ? 0 : encodingSize)
VarSetCapacity(buffer, requiredSize)
StrPut(string, &buffer, length + (insertNullTerminator ?  1 : 0), encoding)
loop, % requiredSize
AOBPattern.Insert(NumGet(buffer, A_Index-1, "UChar"))
Return, AOBPattern
}
modulePatternScan(module := "", aAOBPattern*)
{
MEM_COMMIT := 0x1000, MEM_MAPPED := 0x40000, MEM_PRIVATE := 0x20000, PAGE_NOACCESS := 0x01, PAGE_GUARD := 0x100
if (result := this.getModuleBaseAddress(module, aModuleInfo)) <= 0
Return, "", ErrorLevel := result
if !patternSize := this.getNeedleFromAOBPattern(patternMask, AOBBuffer, aAOBPattern*)
Return, -10
if (result := this.PatternScan(aModuleInfo.lpBaseOfDll, aModuleInfo.SizeOfImage, patternMask, AOBBuffer)) >= 0
Return, result
address := aModuleInfo.lpBaseOfDll
endAddress := address + aModuleInfo.SizeOfImage
loop
{
if !this.VirtualQueryEx(address, aRegion)
Return, -9
if (aRegion.State = MEM_COMMIT
&& !(aRegion.Protect & (PAGE_NOACCESS | PAGE_GUARD))
&& aRegion.RegionSize >= patternSize
&& (result := this.PatternScan(address, aRegion.RegionSize, patternMask, AOBBuffer)) > 0)
Return, result
}
until (address += aRegion.RegionSize) >= endAddress
Return, 0
}
addressPatternScan(startAddress, sizeOfRegionBytes, aAOBPattern*)
{
if !this.getNeedleFromAOBPattern(patternMask, AOBBuffer, aAOBPattern*)
Return, -10
Return, this.PatternScan(startAddress, sizeOfRegionBytes, patternMask, AOBBuffer)
}
processPatternScan(startAddress := 0, endAddress := "", aAOBPattern*)
{
address := startAddress
if endAddress is not integer
endAddress := this.isTarget64bit ? (A_PtrSize = 8 ? 0x7FFFFFFFFFF : 0xFFFFFFFF) : 0x7FFFFFFF
MEM_COMMIT := 0x1000, MEM_MAPPED := 0x40000, MEM_PRIVATE := 0x20000
PAGE_NOACCESS := 0x01, PAGE_GUARD := 0x100
if !patternSize := this.getNeedleFromAOBPattern(patternMask, AOBBuffer, aAOBPattern*)
Return, -10
while address <= endAddress
{
if !this.VirtualQueryEx(address, aInfo)
Return, -1
if A_Index = 1
aInfo.RegionSize -= address - aInfo.BaseAddress
if (aInfo.State = MEM_COMMIT)
&& !(aInfo.Protect & (PAGE_NOACCESS | PAGE_GUARD))
&& aInfo.RegionSize >= patternSize
&& (result := this.PatternScan(address, aInfo.RegionSize, patternMask, AOBBuffer))
{
if result < 0
Return, -2
else if (result + patternSize - 1 <= endAddress)
Return, result
else return 0
}
address += aInfo.RegionSize
}
Return, 0
}
rawPatternScan(byRef buffer, sizeOfBufferBytes := "", startOffset := 0, aAOBPattern*)
{
if !this.getNeedleFromAOBPattern(patternMask, AOBBuffer, aAOBPattern*)
Return, -10
if (sizeOfBufferBytes + 0 = "" || sizeOfBufferBytes <= 0)
sizeOfBufferBytes := VarSetCapacity(buffer)
if (startOffset + 0 = "" || startOffset < 0)
startOffset := 0
Return, this.bufferScanForMaskedPattern(&buffer, sizeOfBufferBytes, patternMask, &AOBBuffer, startOffset)
}
getNeedleFromAOBPattern(byRef patternMask, byRef needleBuffer, aAOBPattern*)
{
patternMask := "", VarSetCapacity(needleBuffer, aAOBPattern.MaxIndex())
for i, v in aAOBPattern
patternMask .= (v + 0 = "" ? "?" : "x"), NumPut(round(v), needleBuffer, A_Index - 1, "UChar")
Return, round(aAOBPattern.MaxIndex())
}
VirtualQueryEx(address, byRef aInfo)
{
if (aInfo.__Class != "_ClassMemory._MEMORY_BASIC_INFORMATION")
aInfo := new this._MEMORY_BASIC_INFORMATION()
Return, aInfo.SizeOfStructure = DLLCall("VirtualQueryEx", "Ptr", this.hProcess, "Ptr", address, "Ptr", aInfo.pStructure, "Ptr", aInfo.SizeOfStructure, "Ptr")
}
patternScan(startAddress, sizeOfRegionBytes, byRef patternMask, byRef needleBuffer)
{
if !this.readRaw(startAddress, buffer, sizeOfRegionBytes)
Return, -1
if (offset := this.bufferScanForMaskedPattern(&buffer, sizeOfRegionBytes, patternMask, &needleBuffer)) >= 0
Return, startAddress + offset
else return 0
}
bufferScanForMaskedPattern(hayStackAddress, sizeOfHayStackBytes, byRef patternMask, needleAddress, startOffset := 0)
{
static p
if !p
{
if A_PtrSize = 4
p := this.MCode("1,x86:8B44240853558B6C24182BC5568B74242489442414573BF0773E8B7C241CBB010000008B4424242BF82BD8EB038D49008B54241403D68A0C073A0A740580383F750B8D0C033BCD74174240EBE98B442424463B74241876D85F5E5D83C8FF5BC35F8BC65E5D5BC3")
else
p := this.MCode("1,x64:48895C2408488974241048897C2418448B5424308BF2498BD8412BF1488BF9443BD6774A4C8B5C24280F1F800000000033C90F1F400066660F1F840000000000448BC18D4101418D4AFF03C80FB60C3941380C18740743803C183F7509413BC1741F8BC8EBDA41FFC2443BD676C283C8FF488B5C2408488B742410488B7C2418C3488B5C2408488B742410488B7C2418418BC2C3")
}
if (needleSize := StrLen(patternMask)) + startOffset > sizeOfHayStackBytes
Return, -1
if (sizeOfHayStackBytes > 0)
Return, DllCall(p, "Ptr", hayStackAddress, "UInt", sizeOfHayStackBytes, "Ptr", needleAddress, "UInt", needleSize, "AStr", patternMask, "UInt", startOffset, "cdecl int")
Return, -2
}
MCode(mcode)
{
static e := {1:4, 2:1}, c := (A_PtrSize=8) ? "x64" : "x86"
if !regexmatch(mcode, "^([0-9]+),(" c ":|.*?," c ":)([^,]+)", m)
return
if !DllCall("crypt32\CryptStringToBinary", "str", m3, "uint", 0, "uint", e[m1], "ptr", 0, "uint*", s, "ptr", 0, "ptr", 0)
return
p := DllCall("GlobalAlloc", "uint", 0, "ptr", s, "ptr")
DllCall("VirtualProtect", "ptr", p, "ptr", s, "uint", 0x40, "uint*", op)
if DllCall("crypt32\CryptStringToBinary", "str", m3, "uint", 0, "uint", e[m1], "ptr", p, "uint*", s, "ptr", 0, "ptr", 0)
Return, p
DllCall("GlobalFree", "ptr", p)
return
}
class _MEMORY_BASIC_INFORMATION
{
__new()
{
if !this.pStructure := DllCall("GlobalAlloc", "UInt", 0, "Ptr", this.SizeOfStructure := A_PtrSize = 8 ? 48 : 28, "Ptr")
Return, ""
Return, this
}
__Delete()
{
DllCall("GlobalFree", "Ptr", this.pStructure)
}
__get(key)
{
static aLookUp := A_PtrSize = 8
?   {   "BaseAddress": {"Offset": 0, "Type": "Int64"}
,    "AllocationBase": {"Offset": 8, "Type": "Int64"}
,    "AllocationProtect": {"Offset": 16, "Type": "UInt"}
,    "RegionSize": {"Offset": 24, "Type": "Int64"}
,    "State": {"Offset": 32, "Type": "UInt"}
,    "Protect": {"Offset": 36, "Type": "UInt"}
,    "Type": {"Offset": 40, "Type": "UInt"}	}
:   {  "BaseAddress": {"Offset": 0, "Type": "UInt"}
,   "AllocationBase": {"Offset": 4, "Type": "UInt"}
,   "AllocationProtect": {"Offset": 8, "Type": "UInt"}
,   "RegionSize": {"Offset": 12, "Type": "UInt"}
,   "State": {"Offset": 16, "Type": "UInt"}
,   "Protect": {"Offset": 20, "Type": "UInt"}
,   "Type": {"Offset": 24, "Type": "UInt"} }
if aLookUp.HasKey(key)
Return, numget(this.pStructure+0, aLookUp[key].Offset, aLookUp[key].Type)
}
__set(key, value)
{
static aLookUp := A_PtrSize = 8
?   {   "BaseAddress": {"Offset": 0, "Type": "Int64"}
,    "AllocationBase": {"Offset": 8, "Type": "Int64"}
,    "AllocationProtect": {"Offset": 16, "Type": "UInt"}
,    "RegionSize": {"Offset": 24, "Type": "Int64"}
,    "State": {"Offset": 32, "Type": "UInt"}
,    "Protect": {"Offset": 36, "Type": "UInt"}
,    "Type": {"Offset": 40, "Type": "UInt"}	}
:   {  "BaseAddress": {"Offset": 0, "Type": "UInt"}
,   "AllocationBase": {"Offset": 4, "Type": "UInt"}
,   "AllocationProtect": {"Offset": 8, "Type": "UInt"}
,   "RegionSize": {"Offset": 12, "Type": "UInt"}
,   "State": {"Offset": 16, "Type": "UInt"}
,   "Protect": {"Offset": 20, "Type": "UInt"}
,   "Type": {"Offset": 24, "Type": "UInt"} }
if aLookUp.HasKey(key)
{
NumPut(value, this.pStructure+0, aLookUp[key].Offset, aLookUp[key].Type)
Return, value
}
}
Ptr()
{
Return, this.pStructure
}
sizeOf()
{
Return, this.SizeOfStructure
}
}
}
KeyClick(Key,PID := "")
{
if (PID = "")
PID := jPID
if(Key = "Enter"){
loop, 1 {
PostMessage, 0x100, 13, 1835009,, ahk_pid %PID%
PostMessage, 0x101, 13, 1835009,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "Shift"){
loop, 1 {
PostMessage, 0x100, 16, 2752513,, ahk_pid %PID%
PostMessage, 0x101, 16, 2752513,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "ㄱ"){
loop, 1 {
PostMessage, 0x100, 229, 1245185,, ahk_pid %PID%
PostMessage, 0x101, 229, 1245185,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "a"){
loop, 1 {
PostMessage, 0x100, 65, 1966081,, ahk_pid %PID%
PostMessage, 0x101, 65, 1966081,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "b"){
loop, 1 {
PostMessage, 0x100, 66, 3145729,, ahk_pid %PID%
PostMessage, 0x101, 66, 3145729,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "c"){
loop, 1 {
PostMessage, 0x100, 67, 3014657,, ahk_pid %PID%
PostMessage, 0x101, 67, 3014657,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "d"){
loop, 1 {
PostMessage, 0x100, 68, 2097153,, ahk_pid %PID%
PostMessage, 0x101, 68, 2097153,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "e"){
loop, 1 {
PostMessage, 0x100, 69, 1179649,, ahk_pid %PID%
PostMessage, 0x101, 69, 1179649,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "f"){
loop, 1 {
PostMessage, 0x100, 70, 2162689,, ahk_pid %PID%
PostMessage, 0x101, 70, 2162689,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "g"){
loop, 1 {
PostMessage, 0x100, 71, 2228225,, ahk_pid %PID%
PostMessage, 0x101, 71, 2228225,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "h"){
loop, 1 {
PostMessage, 0x100, 72, 2293761,, ahk_pid %PID%
PostMessage, 0x101, 72, 2293761,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "i"){
loop, 1 {
PostMessage, 0x100, 73, 1507329,, ahk_pid %PID%
PostMessage, 0x101, 73, 1507329,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "j"){
loop, 1 {
PostMessage, 0x100, 74, 2359297,, ahk_pid %PID%
PostMessage, 0x101, 74, 2359297,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "k"){
loop, 1 {
PostMessage, 0x100, 75, 2424833,, ahk_pid %PID%
PostMessage, 0x101, 75, 2424833,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "l"){
loop, 1 {
PostMessage, 0x100, 76, 2490369,, ahk_pid %PID%
PostMessage, 0x101, 76, 2490369,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "m"){
loop, 1 {
PostMessage, 0x100, 77, 3276801,, ahk_pid %PID%
PostMessage, 0x101, 77, 3276801,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "n"){
loop, 1 {
PostMessage, 0x100, 78, 3211265,, ahk_pid %PID%
PostMessage, 0x101, 78, 3211265,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "o"){
loop, 1 {
PostMessage, 0x100, 79, 1572865,, ahk_pid %PID%
PostMessage, 0x101, 79, 1572865,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "p"){
loop, 1 {
PostMessage, 0x100, 80, 1638401,, ahk_pid %PID%
PostMessage, 0x101, 80, 1638401,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "q"){
loop, 1 {
PostMessage, 0x100, 81, 1048577,, ahk_pid %PID%
PostMessage, 0x101, 81, 1048577,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "r"){
loop, 1 {
PostMessage, 0x100, 82, 1245185,, ahk_pid %PID%
PostMessage, 0x101, 82, 1245185,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "s"){
loop, 1 {
PostMessage, 0x100, 83, 2031617,, ahk_pid %PID%
PostMessage, 0x101, 83, 2031617,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "t"){
loop, 1 {
PostMessage, 0x100, 84, 1310721,, ahk_pid %PID%
PostMessage, 0x101, 84, 1310721,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "u"){
loop, 1 {
PostMessage, 0x100, 85, 1441793,, ahk_pid %PID%
PostMessage, 0x101, 85, 1441793,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "v"){
loop, 1 {
PostMessage, 0x100, 86, 3080193,, ahk_pid %PID%
PostMessage, 0x101, 86, 3080193,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "w"){
loop, 1 {
PostMessage, 0x100, 87, 1114113,, ahk_pid %PID%
PostMessage, 0x101, 87, 1114113,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "x"){
loop, 1 {
PostMessage, 0x100, 88, 2949121,, ahk_pid %PID%
PostMessage, 0x101, 88, 2949121,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "y"){
loop, 1 {
PostMessage, 0x100, 89, 1376257,, ahk_pid %PID%
PostMessage, 0x101, 89, 1376257,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "z"){
loop, 1 {
PostMessage, 0x100, 90, 2883585,, ahk_pid %PID%
PostMessage, 0x101, 90, 2883585,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "AltR"){
loop, 1 {
PostMessage, 0x100, 18, 540540929,, ahk_pid %PID%
PostMessage, 0x100, 82, 1245185,, ahk_pid %PID%
PostMessage, 0x101, 82, 1245185,, ahk_pid %PID%
PostMessage, 0x101, 18, 540540929,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "AltB"){
loop, 1 {
PostMessage, 0x100, 18, 540540929,, ahk_pid %PID%
PostMessage, 0x100, 66, 3145729,, ahk_pid %PID%
PostMessage, 0x101, 66, 3145729,, ahk_pid %PID%
PostMessage, 0x101, 18, 540540929,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "AltV"){
loop, 1 {
PostMessage, 0x100, 18, 540540929,, ahk_pid %PID%
PostMessage, 0x100, 86, 3080193,, ahk_pid %PID%
PostMessage, 0x101, 86, 3080193,, ahk_pid %PID%
PostMessage, 0x101, 18, 540540929,, ahk_pid %PID%
sleep, 1
}
}
else if(Key = "Space"){
loop, 1 {
PostMessage, 0x100, 32, 3735553,, ahk_pid %PID%
PostMessage, 0x101, 32, 3735553,, ahk_pid %PID%
}
}
else if(Key = "Tab"){
loop, 1 {
PostMessage, 0x100, 9, 983041,, ahk_pid %PID%
PostMessage, 0x101, 9, 983041,, ahk_pid %PID%
}
}
else if(Key = "Alt2"){
loop, 1 {
PostMessage, 0x100, 18, 540540929,, ahk_pid %PID%
postmessage, 0x100, 50, 196609, ,ahk_pid %PID%
postmessage, 0x101, 50, 196609, ,ahk_pid %PID%
PostMessage, 0x101, 18, 540540929,, ahk_pid %PID%
sleep, 1
}
}
else if (Key>=0&&KEY<=9){
if (key=0)
key := 10
NUM := key - 1
jelan.write(0x0058D301, NUM, "Char", aOffsets*)
RunMemory("퀵슬롯사용")
}
else if(Key="F1"){
loop, 1 {
postmessage, 0x100, 112, 3866625, ,ahk_pid %PID%
postmessage, 0x101, 112, 3866625, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="F2"){
loop, 1 {
postmessage, 0x100, 113, 3932161, ,ahk_pid %PID%
postmessage, 0x101, 113, 3932161, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="F3"){
loop, 1 {
postmessage, 0x100, 114, 3997697, ,ahk_pid %PID%
postmessage, 0x101, 114, 3997697, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="k1"){
loop, 1 {
postmessage, 0x100, 49, 131073, ,ahk_pid %PID%
postmessage, 0x101, 49, 131073, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="k2") {
loop, 1 {
postmessage, 0x100, 50, 196609, ,ahk_pid %PID%
postmessage, 0x101, 50, 196609, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="k3") {
loop, 1 {
postmessage, 0x100, 51, 262145, ,ahk_pid %PID%
postmessage, 0x101, 51, 262145, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="k4") {
loop, 1 {
postmessage, 0x100, 52, 327681, ,ahk_pid %PID%
postmessage, 0x101, 52, 327681, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="k5"){
loop, 1{
postmessage, 0x100, 53, 393217, ,ahk_pid %PID%
postmessage, 0x101, 53, 393217, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="k6"){
loop, 1{
postmessage, 0x100, 54, 458753, ,ahk_pid %PID%
postmessage, 0x101, 54, 458753, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="k7"){
loop, 1{
postmessage, 0x100, 55, 524289, ,ahk_pid %PID%
postmessage, 0x101, 55, 524289, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="k8"){
loop, 1{
postmessage, 0x100, 56, 589825, ,ahk_pid %PID%
postmessage, 0x101, 56, 589825, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="k9"){
loop, 1{
postmessage, 0x100, 57, 655361, ,ahk_pid %PID%
postmessage, 0x101, 57, 655361, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="w0"){
loop, 1{
postmessage, 0x100, 48, 720897, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="w1"){
loop, 1 {
postmessage, 0x100, 49, 131073, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="w2") {
loop, 1 {
postmessage, 0x100, 50, 196609, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="w3") {
loop, 1 {
postmessage, 0x100, 51, 262145, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="w4") {
loop, 1 {
postmessage, 0x100, 52, 327681, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="w5"){
loop, 1{
postmessage, 0x100, 53, 393217, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="w6"){
loop, 1{
postmessage, 0x100, 54, 458753, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="w7"){
loop, 1{
postmessage, 0x100, 55, 524289, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="w8"){
loop, 1{
postmessage, 0x100, 56, 589825, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="w9"){
loop, 1{
postmessage, 0x100, 57, 655361, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="w0"){
loop, 1{
postmessage, 0x100, 48, 720897, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="CTRL1"){
loop, 1 {
postmessage, 0x100, 17, 1900545, ,ahk_pid %PID%
postmessage, 0x100, 49, 131073, ,ahk_pid %PID%
postmessage, 0x101, 49, 131073, ,ahk_pid %PID%
postmessage, 0x101, 17, 1900545, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="CTRL2"){
loop, 1 {
postmessage, 0x100, 17, 1900545, ,ahk_pid %PID%
postmessage, 0x100, 50, 196609, ,ahk_pid %PID%
postmessage, 0x101, 50, 196609, ,ahk_pid %PID%
postmessage, 0x101, 17, 1900545, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="CTRL3") {
loop, 1 {
postmessage, 0x100, 17, 1900545, ,ahk_pid %PID%
postmessage, 0x100, 51, 262145, ,ahk_pid %PID%
postmessage, 0x101, 51, 262145, ,ahk_pid %PID%
postmessage, 0x101, 17, 1900545, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="CTRL4") {
loop, 1 {
postmessage, 0x100, 17, 1900545, ,ahk_pid %PID%
postmessage, 0x100, 52, 327681, ,ahk_pid %PID%
postmessage, 0x101, 52, 327681, ,ahk_pid %PID%
postmessage, 0x101, 17, 1900545, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="CTRL5") {
loop, 1 {
postmessage, 0x100, 17, 1900545, ,ahk_pid %PID%
postmessage, 0x100, 53, 393217, ,ahk_pid %PID%
postmessage, 0x101, 53, 393217, ,ahk_pid %PID%
postmessage, 0x101, 17, 1900545, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="CTRL6") {
loop, 1 {
postmessage, 0x100, 17, 1900545, ,ahk_pid %PID%
postmessage, 0x100, 54, 458753, ,ahk_pid %PID%
postmessage, 0x101, 54, 458753, ,ahk_pid %PID%
postmessage, 0x101, 17, 1900545, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="CTRL7") {
loop, 1 {
postmessage, 0x100, 17, 1900545, ,ahk_pid %PID%
postmessage, 0x100, 55, 524289, ,ahk_pid %PID%
postmessage, 0x101, 55, 524289, ,ahk_pid %PID%
postmessage, 0x101, 17, 1900545, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="CTRL8") {
loop, 1 {
postmessage, 0x100, 17, 1900545, ,ahk_pid %PID%
postmessage, 0x100, 56, 589825, ,ahk_pid %PID%
postmessage, 0x101, 56, 589825, ,ahk_pid %PID%
postmessage, 0x101, 17, 1900545, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="CTRL9") {
loop, 1 {
postmessage, 0x100, 17, 1900545, ,ahk_pid %PID%
postmessage, 0x100, 57, 655361, ,ahk_pid %PID%
postmessage, 0x101, 57, 655361, ,ahk_pid %PID%
postmessage, 0x101, 17, 1900545, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="CTRL0") {
loop, 1 {
postmessage, 0x100, 17, 1900545, ,ahk_pid %PID%
postmessage, 0x100, 48, 720897, ,ahk_pid %PID%
postmessage, 0x101, 48, 720897, ,ahk_pid %PID%
postmessage, 0x101, 17, 1900545, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="DownArrow") {
loop, 1 {
postmessage, 0x100, 40, 22020097, ,ahk_pid %PID%
postmessage, 0x101, 40, 22020097, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="UpArrow") {
loop, 1 {
postmessage, 0x100, 38, 21495809, ,ahk_pid %PID%
postmessage, 0x101, 38, 21495809, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="RightArrow") {
loop, 1 {
postmessage, 0x100, 39, 21823489, ,ahk_pid %PID%
postmessage, 0x101, 39, 21823489, ,ahk_pid %PID%
sleep, 1
}
}
else if(Key="LeftArrow") {
loop, 1 {
postmessage, 0x100, 37, 21692417, ,ahk_pid %PID%
postmessage, 0x101, 37, 21692417, ,ahk_pid %PID%
sleep, 1
}
}
}
TargetSkillList := ["현혹","폭검","독침","무기공격"]
Global 아이콘 := chr(9829)
Global 경계선 := chr(3500)
global jelan,jPID,jTitle,pwb,MapNumber,RunDirect,NowDate,Version,lov,identt,bann,byte,bytes
global Location,jjc,MsgMacro,State,Inven,Buy,Repair,Ras,SelectRas,Map,AAD,MapSize,GAD,Weapon,Chat,Attack,Mount,NPCMenu,AAS,PosX,PosY,MovePosX,MCC,BAI,MovePosY,NowHP,HCC,AAI,MaxHP,NowMP,MaxMP,NowFP,MaxFP,Gold,Gold1,AGI,FormNumber,NPCMsg,NPCMenuBuyPosX,SCC,CTC,NPCMenuBuyPosY,DCC,NPCMenuRepairPosX,BAD,NPCMenuRepairPosY,rCTC,AbilityNameADD,SSC,AbilityValueADD,BAS,AbilityName,SSS,AbilityValue,Moving,Slot1Ability,GAI,SST,Slot2Ability,Slot3Ability,GAS,Slot4Ability,HPPercent,MPPercent,FPPercent,Shield,NowInven,invenstack = 0,invenError = 0,StatePosX,StatePosY,CheckFirstHP,CheckUPHP,RunningTime,ChangeValue,MagicN,CritHP,CritMP,Get_CharOID,CharID_1,CharID_2,CharID_3,CharID_4,CharID_5,CharID_6,ChangeValue,pP1,pP2,pP3,pP4,pP5,pP6,P1,P2,P3,P4,P5,P6,loady,ProgramStartTime,RPST,RPST,BasicWValue0,BasicWValue1,BasicWValue2,BasicWValue3,BWValue0,BWValue1,BWValue2,BWValue3,RMNS,MNS,RMNN,MLimit,incinerateitem,RowNumber,inciNumber = 1,inciItem,CCD,CheckPB,newTime1,newTime,nowtime1,nowtime,RCC,pbtalkcheck,pbtalkcheck1,pbtalkcheck2
global Slot1Ability,Slot2Ability,Slot3Ability,Slot4Ability,Slot5Ability,Slot6Ability,Slot7Ability,Slot8Ability,Slot9Ability,Slot10Ability,Slot11Ability,Slot12Ability,Slot13Ability,Slot14Ability,Slot15Ability,Slot16Ability,Slot17Ability,Slot18Ability,Slot19Ability,Slot20Ability,Slot21Ability,Slot22Ability,Slot23Ability,Slot24Ability,Slot25Ability,Slot26Ability,Slot27Ability,Slot28Ability,Slot29Ability,Slot30Ability,Slot31Ability,Slot32Ability,Slot33Ability,Slot34Ability,Slot35Ability,Slot36Ability,Slot37Ability,Slot38Ability,Slot39Ability,Slot40Ability,Slot41Ability,Slot42Ability,Slot43Ability,Slot44Ability,Slot45Ability,Slot46Ability,Slot47Ability,Slot48Ability,Slot49Ability,Slot50Ability,Slot51Ability,Slot52Ability,Slot53Ability,Slot54Ability,Slot55Ability,Slot56Ability
global Slot1AN,Slot2AN,Slot3AN,Slot4AN,Slot5AN,Slot6AN,Slot7AN,Slot8AN,Slot9AN,Slot10AN,Slot11AN,Slot12AN,Slot13AN,Slot14AN,Slot15AN,Slot16AN,Slot17AN,Slot18AN,Slot19AN,Slot20AN,Slot21AN,Slot22AN,Slot23AN,Slot24AN,Slot25AN,Slot26AN,Slot27AN,Slot28AN,Slot29AN,Slot30AN,Slot31AN,Slot32AN,Slot33AN,Slot34AN,Slot35AN,Slot36AN,Slot37AN,Slot38AN,Slot39AN,Slot40AN,Slot41AN,Slot42AN,Slot43AN,Slot44AN,Slot45AN,Slot46AN,Slot47AN,Slot48AN,Slot49AN,Slot50AN,Slot51AN,Slot52AN,Slot53AN,Slot54AN,Slot55AN,Slot56AN
global Slot3Magic,Slot4Magic,Slot5Magic,Slot6Magic,Slot7Magic,Slot8Magic,Slot9Magic,Slot10Magic,Slot11Magic,Slot12Magic,Slot13Magic,Slot14Magic,Slot15Magic,Slot16Magic,Slot17Magic,Slot18Magic
global Slot3MN,Slot4MN,Slot5MN,Slot6MN,Slot7MN,Slot8MN,Slot9MN,Slot10MN,Slot11MN,Slot12MN,Slot13MN,Slot14MN,Slot15MN,Slot16MN,Slot17MN,Slot18MN
Global 몬스터ID
global NPC_STARTversion
global A리노아,A동파,A서파,B리노아,B동파,B서파,G리노아,G동파,G서파,A길잃파,B길잃파,G길잃파
global SA리노아,SA동파,SA서파,SB리노아,SB동파,SB서파,SG리노아,SG동파,SG서파,SA길잃파,SB길잃파,SG길잃파
global monitor
global Start_Time
Global 아이템갯수 := {}
Global 라깃카운트,정보카운트,결정갯수,나무갯수,가루갯수,골드바카운트
global 게임시작x,게임시작y
global 좌표고정
global 점검 = 0
global 업데이트체크
Global StartTime := A_TickCount
Global RunThreadCounter := A_TickCount
Global 서버상태,CountPortal,차원
Global 빵,몸찌방지,식빵갯수,절반FP,몸찌방지시작
Global 몸찌체크 = 0 , 상점밖이동 = 0
global 실행창위치 = 0 , 시작탭사이즈 = 0
global 서버팅김,파라스감지,수천감지,파라스방해감지
Global 현혹번호, 폭검번호, 독침번호, 무기공격번호
Global 대화번호, 명상번호, 더블어택번호, 체력향상번호, 집중번호, 회피번호, 몸통지르기번호, 리무브아머번호, 민첩향상번호, 활방어번호, 마력향상번호, 마력방어번호
Global 빛의갑옷번호, 물의갑옷번호, 스톤스킨번호
Global 포북시작, 초기마을이동, 현재차원
Global 포북생콩섭취, 포남생콩섭취, 가방수량체크
Global VMRESET
Global 상승체력평균값, 상승체력평균치
Global jean, Cap, Top, Shoes
Global 호출대상
Global WaaponLimit
Global UsePunch
Global 무기_Coin
Global hit1 := jelan.read(0x0058dad4,"UINT",0x1a5)
Global 무바딜레이 :=300
Global 맵이동속도 := 300
Global 무기종류
Global RecentWeapons := []
Global Attacking , AttackingCount
Global 파라스타이머시작, 파라스타이머카운트, 파라스타이머값 := 3600, 파라스대기값 := 3600000
FileReadLine, NPC_STARTversion, C:\NPC좌표.ini , 1
SysGet,monitor,16
Gui, color, ffffff
Gui, -MaximizeBox -MinimizeBox -Caption -Border
Gui, Font
Gui, Font, s8 Bold,Arial
Gui, Add, StatusBar, , 시작대기중
SB_SetParts(210,260,310)
Gui, Font
Gui, Font,s8 ,Arial
Gui, Add, Tab3, v탭2 x442 y42 w150 h240 AltSubmit cBlack, 퀵슬롯||잠수어빌|타겟어빌|포남설정|
Gui, tab, 퀵슬롯
Gui, Font
Gui, Font, s8 Bold,Arial
Gui, Add, Text, x+15 y+15 h20 c205375, ★ 1 ~ 3번 무기설정
Gui, Add, checkbox, xp yp+30 h20 v4번사용 c112b3c, 　4번
Gui, Add, checkbox, xp yp+30 h20 v5번사용 c112b3c, 　5번
Gui, Add, checkbox, xp yp+30 h20 v6번사용 c112b3c, 　6번
Gui, Add, checkbox, xp yp+30 h20 v7번사용 c112b3c, 　7번
Gui, Add, Text, xp yp+30 h20 c205375, ★ 8 ~ 0번 고정
Gui, tab, 잠수어빌
Gui, Font
Gui, Font, s8 Bold,Arial
Gui, Add, checkbox, x+5 y+15 h20 v대화사용 cff6666, 대화
Gui, Add, checkbox, xp yp+31 h20 v명상사용 cff6666, 명상
Gui, Add, checkbox, xp yp+31 h20 v몸통지르기사용 cff6666, 몸찌
Gui, Add, checkbox, xp yp+31 h20 v집중사용 cff6666, 집중
Gui, Add, checkbox, xp yp+31 h20 v회피사용 cff6666, 회피
Gui, Add, checkbox, xp yp+31 h20 v활방어사용 cff6666, 활방어
Gui, Add, checkbox, xp+65 yp-155 h20 v리무브아머사용 cff6666, 리뭅아머
Gui, Add, checkbox, xp yp+31 h20 v체력향상사용 cff6666, 체력향상
Gui, Add, checkbox, xp yp+31 h20 v민첩향상사용 cff6666, 민첩향상
Gui, Add, checkbox, xp yp+31 h20 v더블어택사용 cff6666, 더블어택
Gui, Add, checkbox, xp yp+31 h20 v마력향상사용 c0099ff, 마법향상
Gui, Add, checkbox, xp yp+31 h20 v마력방어사용 c0099ff, 마력방어
Gui, tab, 타겟어빌
Gui, Font
Gui, Font, s8 Bold,Arial
Gui, Add, checkbox, x+15 y+15 h20 v현혹사용 cff0066, 현혹
Gui, Add, checkbox, xp yp+30 h20 v폭검사용 cff0066, 폭검
Gui, Add, checkbox, xp yp+30 h20 v독침사용 cff0066, 독침
Gui, Add, checkbox, xp yp+30 h20 v무기공격사용 cff0066, 무기공격
Gui, tab, 포남설정
Gui, Font
Gui, Font, s8 Bold,Arial
Gui, Add, Radio, x+15 y+15 h15 Checked vGui_Ent gCheckMob cff6666, 엔트
Gui, Add, Radio, xp+55 yp h15 vGui_Rockey gCheckMob cff6666, 록키
Gui, Add, Radio, xp-55 yp+30 h15 vGui_Mand gCheckMob cff6666, 만드
Gui, Add, Radio, xp+55 yp h15 vGui_MobMagic gCheckMob c0099ff, 마법
Gui, Add, Radio, xp-55 yp+30 h15 vGui_EntRockey gCheckMob cff0066, 엔트록키
Gui, Add, Radio, xp yp+30 h15 vGui_AllMobAND gCheckMob cff0066, 수련어빌 모두
Gui, Add, Radio, xp yp+30 h15 Checked vGui_AllMobOR gCheckMob cff0066, 수련어빌 1개라도
Gui, Add, Edit, xp-4 yp+30 w33 +Right Limit4 number Disabled cblack vGui_AllMobLimit, 9200
Gui, Add, Text, x+1 yp+5 +Left v포남설정, 이상 만드만 공격
Gui, Font
Gui, Font,s8 ,Arial
Gui, Add, Tab3, v탭3 x600 y42 w165 h240 AltSubmit cBlack, 소각설정|세부설정||
Gui, tab, 소각설정
Gui, Font
Gui, Font, s8,Arial
Gui, Add, ListView, x+5 y+5 h130 w150 -LV0x20 -Multi -HDR v포프레스네소각, 아이템
Gui, add, Edit, xp yp+140 w150 vGui_incinerateitem
Gui, Add, Button, xp yp+30 w60 h25 g소각갱신, 새로갱신
Gui, Add, Button, xp+65 yp w40 h25 gAddincinerate, 추가
Gui, Add, Button, xp+45 yp w40 h25 gDelincinerate, 삭제
LV_ModifyCol(1, 320)
Gui, tab, 세부설정
Gui, Font
Gui, Font, s8  Bold,Arial
Gui, Add, GroupBox, x605 y80 w155 h50 +Center c343a40, [포북 생콩설정]
Gui, Font
Gui, Font, s8,Arial
Gui, Add, Text, xp+5 yp+23 , 생콩섭취
Gui, Add, DropDownList, xp+60 yp-4 h100 w85 +Center v포북생콩설정 g생콩설정, FP 500이하||상시사용
Gui, Font
Gui, Font, s8  Bold,Arial
Gui, Add, GroupBox, x605 y135 w155 h50 +Center c343a40, [포남 생콩설정]
Gui, Font
Gui, Font, s8,Arial
Gui, Add, Text, xp+5 yp+23 , 생콩섭취
Gui, Add, DropDownList, xp+60 yp-4 h100 w85 +Center v포남생콩설정 g생콩설정, FP 500이하||상시사용
Gui, Font
Gui, Font, s8  Bold,Arial
Gui, Add, GroupBox, x605 y190 w155 h50 +Center c343a40, [줍줍 가방설정]
Gui, Font
Gui, Font, s8,Arial
Gui, Add, Text, xp+5 yp+23 , 가방갯수
Gui, Add, DropDownList, xp+60 yp-4 h100 w85 +Center v가방설정 g가방수량설정, 줍줍끄기|40개 초과시||45개 초과시|제한없음
Gui, Font
Gui, Font,s8 ,Arial
Gui, Add, Tab3, v탭4 x432 y300 w340 h191 AltSubmit cBlack, 캐릭터정보|무바설정|마법설정|안전설정|파티설정|
Gui, Tab, 캐릭터정보
Gui, Font
Gui, Font, s9 ,Arial
Gui, Add, Text, x442 yp+30 w70 +Right, 캐릭터명 :
Gui, Add, Text, x442 yp+27 w70 +Right, 갈리드 :
Gui, Add, Text, x442 yp+27 w70 +Right, 골드바 :
Gui, Add, Text, x442 yp+27 w70 +Right, 라스의깃 :
Gui, Add, Text, x442 yp+27 w70 +Right, 정령의보석 :
Gui, Font
Gui, Font, s8  Bold, Arial
Gui, Add, Text, x492 yp+27 w100 +Left vNPC_version cf66b0e,%NPC_STARTversion%
Gui, Font
Gui, Font, s9 ,Arial
Gui, Add, Text, x532 y330 w200 +Left vGui_CharName
Gui, Add, Text, x532 yp+27 w200 +Left vGui_NowGold
Gui, Add, Text, x532 yp+27 w200 +Left vGui_NowGoldbar
Gui, Add, Text, x532 yp+27 w200 +Left CBlue vGui_RasCount
Gui, Add, Text, x532 yp+27 w200 +Left CRed vGui_정보Count
Gui, Font
Gui, Font, s8 cGray Bold,Arial
Gui, Add, Text, x592 yp+27 w120 +Right ,마개조 똘룸체잠 %ProgramVersion%
Gui, Add, Groupbox, x442 y454 w320 h30,
Gui, Tab, 무바설정
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, GroupBox, x440 y417 w325 h70 +Center c343a40,
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, Radio, x457 y330 h15 Checked vGui_1Muba gSelectMuba c000000, 1무바
Gui, Add, Radio, xp+100 yp h15 vGui_2Muba gSelectMuba c000000, 2무바
Gui, Add, Radio, xp+100 yp h15 vGui_3Muba gSelectMuba c000000, 3무바
Gui, Add, Radio, xp-200 yp+27 h15 vGui_2ButMuba gSelectMuba c000000, 2벗무바
Gui, Add, Radio, xp+100 yp h15 vGui_3ButMuba gSelectMuba c000000, 3벗무바
Gui, Add, Radio, xp+100 yp h15 vGui_4ButMuba gSelectMuba c000000, 4벗무바
Gui, Add, Text, xp-195 yp+27 +Left, [1번 무기어빌]
Gui, Add, Text, xp+100 yp +Left, [2번 무기어빌]
Gui, Add, Text, xp+100 yp +Left, [3번 무기어빌]
Gui, Add, DropDownList, xp-202 yp+15 w80 +Left cblack vGui_Weapon1 gSelectAbility, 검|단검|도|도끼|대검|대도|창, 특수창|봉, 해머|현금|활|거대검|거대도|거대도끼|양손단검|양손도끼|스태프
Gui, Add, DropDownList, xp+100 yp w80 +Left cblack Disabled vGui_Weapon2 gSelectAbility, 검|단검|도|도끼|대검|대도|창, 특수창|봉, 해머|현금|활|거대검|거대도|거대도끼|양손단검|양손도끼|스태프
Gui, Add, DropDownList, xp+100 yp w80 +Left cblack Disabled vGui_Weapon3 gSelectAbility, 검|단검|도|도끼|대검|대도|창, 특수창|봉, 해머|현금|활|거대검|거대도|거대도끼|양손단검|양손도끼|스태프
Gui, Font
Gui, Font, s8  ,Arial
Gui, Add, Text, x490 yp+30 w27 h30 +Center, 격투
Gui, Font
Gui, Font, s8  ,Arial
Gui, Add, Text, xp+64 yp w25 +Center vGui_WeaponLimit1, 1번
Gui, Add, Text, xp+64 yp w25 +Center vGui_WeaponLimit2, 2번
Gui, Add, Text, xp+64 yp w25 +Center vGui_WeaponLimit3, 3번
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, Edit, xp-200 yp+15 w40 h17 +Center Limit4 number Disabled cblack vGui_LimitAbility0, 9200
Gui, Add, Edit, xp+64 yp w40 h17 +Center Limit4 number cblack vGui_LimitAbility1, 9200
Gui, Add, Edit, xp+64 yp w40 h17 +Center Limit4 number Disabled cblack vGui_LimitAbility2, 9200
Gui, Add, Edit, xp+64 yp w40 h17 +Center Limit4 number Disabled cblack vGui_LimitAbility3, 9200
Gui, Add, Text, xp-192 yp+22 , 장소 "자동" 선택시 설정값에 도달하면 포북이동
Gui, tab, 마법설정
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, Text, x480 y345 +Left,원격마법 사용
Gui, Add, Checkbox, xp-20 yp-2 w15 h15 gCheckUseMagic vGui_CheckUseMagic c000000
Gui, Add, Text, xp+20 yp+30 +Right, HP가
Gui, Add, Edit, xp+30 yp-4 w80 +Right Limit7 number cRed vGui_CHP, 0
Gui, Add, Text, xp+85 yp+4, 되면 리메듐 사용
Gui, Add, Text, xp-135 yp+26 +Right, 스펠슬롯
Gui, Add, DropDownList, xp+50 yp-4 w80 +Left vGui_MagicNStack, 3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20
Gui, Add, Text, xp+85 yp+4 +Left, 번 까지 전부 사용
Gui, Add, Text, xp-135 yp+30 c6666ff +Left, (스펠슬롯1번) (엘)리메듐 고정
Gui, Add, Text, xp yp+24 C6666ff +Left, (스펠슬롯2번) 브렐 고정
Gui, tab, 안전설정
Gui, Font, s8  Bold,Arial
Gui, Add, GroupBox, x450 y340 w230 h65 c343a40, [설정 #1]
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, Checkbox, xp10 yp+20 w15 h15 gCheckUseHPExit vGui_CheckUseHPExit c000000
Gui, Add, Text, xp18 yp+1 +Left, 체력이
Gui, Add, Edit, xp35 yp-2 w50 h17 +Right Limit6 number Disabled cRed vGui_HPExit, 0
Gui, Add, Text, xp53 yp+2 +Left, 이하 종료 (재접속x)
Gui, Add, Checkbox, xp-106 yp+20 w15 h15 gCheckUseHPPortal vGui_CheckUseHPPortal c000000
Gui, Add, Text, xp18 yp+1 +Left, 체력이
Gui, Add, Edit, xp35 yp-2 w50 h17 +Right Limit6 number Disabled cRed vGui_HPPortal, 0
Gui, Add, Text, xp53 yp+2 +Left, 이하 차원이동
Gui, Font, s8  Bold,Arial
Gui, Add, GroupBox, x450 y420 w230 h48 c343a40, [설정 #2]
Gui, Font
Gui, Font, s8 Bold,Arial
Gui, Add, Radio, xp+27 yp+22 w90 h15 Checked v방어구방지ON c205375, 장비체크ON
Gui, Add, Radio, xp+100 yp w95 h15 v방어구방지OFF c6977a9, 장비체크OFF
Gui, tab, 파티설정
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, Checkbox, x660 y332 w15 h15 vGui_CheckUseParty g원격파티사용 c000000
Gui, Add, Text, xp+20 yp +Left , 원격파티 사용
Gui, Add, Button, xp-20 yp+20 w80 h30 g원격파티설명서, 사용법
Gui, Add, Text, x450 y337, 파티1 -
Gui, add, Edit, xp+40 yp-5 w100 cblack vName1
Gui, Add, DropDownList, xp+110 yp w38 +Left vGui_P1CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Add, Text, xp-150 yp+30, 파티2 -
Gui, add, Edit, xp+40 yp-5 w100 cblack vName2
Gui, Add, DropDownList, xp+110 yp w38 +Left vGui_P2CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Add, Text, xp-150 yp+30, 파티3 -
Gui, add, Edit, xp+40 yp-5 w100 cblack vName3
Gui, Add, DropDownList, xp+110 yp w38 +Left vGui_P3CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Add, Text, xp-150 yp+30, 파티4 -
Gui, add, Edit, xp+40 yp-5 w100 cblack vName4
Gui, Add, DropDownList, xp+110 yp w38 +Left vGui_P4CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Add, Text, xp-150 yp+30, 파티5 -
Gui, add, Edit, xp+40 yp-5 w100 cblack vName5
Gui, Add, DropDownList, xp+110 yp w38 +Left vGui_P5CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Add, Text, xp-150 yp+30, 파티6 -
Gui, add, Edit, xp+40 yp-5 w100 cblack vName6
Gui, Add, DropDownList, xp+110 yp w38 +Left vGui_P6CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Font
Gui, Font, s8 Bold,Arial
Gui, Add, Radio, x660 y400 h15 vGui_PartyOn c205375 g원격파티사용, 파티ON
Gui, Add, Radio, xp yp+25 h15 Checked vGui_PartyOff g원격파티사용 c6977a9, 파티OFF
Gui, Font
Gui, Font,s8 ,Arial
Gui, Add, Tab, vTabsize x0 y0 w780 h680 AltSubmit gTab, 기본설정|스펠그레이드 / 목록스캔|어빌리티그레이드|
Gui, Font
Gui, Font, s8 Bold,Arial
Gui, Add, GroupBox, x432 y29 w340 h265 +Center c343a40,
gui, tab, 기본설정

Gui, Font
Gui, Font, s9 Bold,Arial
Gui, Add, GroupBox, x245 y29 w185 h375 c343a40, [NPCOID설정]
Gui, Font
Gui, Font, s8

; 기준: 알파 리노아 텍스트 (x260 y60), Edit (x340 y55)
Gui, Add, Text,  x260 y60 +Left , 알파 리노아   :
Gui, Add, Edit,  x340 y55 w80 Disabled va리노아 , 0

Gui, Add, Text,  x260 y85 +Left , 알파 동파   :
Gui, Add, Edit,  x340 y80 w80 Disabled va동파, 0

Gui, Add, Text,  x260 y110 +Left , 알파 서파   :
Gui, Add, Edit,  x340 y105 w80 Disabled vA서파, 0

Gui, Add, Text,  x260 y135 +Left , 베타 리노아  :
Gui, Add, Edit,  x340 y130 w80 Disabled vB리노아, 0

Gui, Add, Text,  x260 y160 +Left , 베타 동파  :
Gui, Add, Edit,  x340 y155 w80 Disabled vB동파, 0

Gui, Add, Text,  x260 y185 +Left , 베타 서파  :
Gui, Add, Edit,  x340 y180 w80 Disabled vB서파, 0

Gui, Add, Text,  x260 y210 +Left , 감마 리노아  :
Gui, Add, Edit,  x340 y205 w80 Disabled vG리노아, 0

Gui, Add, Text,  x260 y235 +Left , 감마 동파  :
Gui, Add, Edit,  x340 y230 w80 Disabled vG동파, 0

Gui, Add, Text,  x260 y260 +Left , 감마 서파  :
Gui, Add, Edit,  x340 y255 w80 Disabled vG서파, 0

Gui, Add, Text,  x260 y285 +Left , 알파 길잃파  :
Gui, Add, Edit,  x340 y280 w80 Disabled vA길잃파, 0

Gui, Add, Text,  x260 y310 +Left , 베타 길잃파  :
Gui, Add, Edit,  x340 y305 w80 Disabled vB길잃파, 0

Gui, Add, Text,  x260 y335 +Left , 감마 길잃파  :
Gui, Add, Edit,  x340 y330 w80 Disabled vG길잃파, 0

Gui, Add, Button, x260 y360 w155 h30 , NPCOID리셋

Gui, ADD, Edit, x280 y420 Disabled w120 h18 +Center v현재타겟OID값,
Gui, ADD, Edit, xp yp+20 Disabled w120 h18 +Center v호출대상이름,
Gui, ADD, Edit, xp yp+20 Disabled w120 h18 +Center v파라스타이머,
Gui, Font
Gui, Font, s8  Bold,Arial
Gui, Add, GroupBox, x12 y29 w230 h150 center v로그인 c343a40, [로그인 세팅]
Gui, Font
Gui, Font, s9 ,Arial
Gui, Add, Edit, x22 y49 w210 h20 vGui_NexonID c000000,
Gui, Add, Edit, x22 y79 w210 h20 Password vGui_NexonPassWord c000000,
Gui, Add, DropDownList, x22 y109 w50 h150 +Center Disabled vGui_Server, 엘||
Gui, Add, DropDownList, x77 y109 w50 h200 +Center vGui_CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Add, DropDownList, x132 y109 w100 +Center vGui_Login gCheck로그인, 인터넷||넥슨플러그
Gui, Add, Button, x82 y139 w90 h30 +Center  vGui_StartButton gStart , 실행
Gui, Font
Gui, Font, s8  Bold,Arial
Gui, Add, GroupBox, x12 y180 w230 h65 +Center c343a40, [로그인 상태]
Gui, Font
Gui, Font, s8,Arial
Gui, Add, Text, xp10 yp20 w210 h20 +Center v로그인상태정보, 실행 준비 중
Gui, Add, Text, xp yp20 w60 h20 , 실행시간:
Gui, Add, Text, xp50 yp w150 h20 +Center v실행시간,
Gui, Font
Gui, Font, s9,Arial
Gui, Add, Text, x30 y70 h20 v넥슨x, 좌표X :
Gui, Add, Text, x140 y70 h20 v넥슨y, 좌표Y :
Gui, Add, Text, x80 y70 w50 h20 v좌표x, Ctrl
Gui, Add, Text, x190 y70 w50 h20 v좌표y, Q
Gui, Font
Gui, Font, s8 Bold,Arial
Gui, Add, GroupBox, x25 y398 w205 h33 center c343a40,
Gui, Font
Gui, Font, s9 cBlue Bold
Gui, Add, Text, x35 y410 w60 , 감응:
Gui, Add, Radio, x80 y410 w50 vGui_KON, ON
Gui, Font
Gui, Font, s9 cRed Bold
Gui, Add, Radio, x130 y410 w50 vGui_KOFF, OFF
Gui, Add, GroupBox, x25 y427 w205 h33 center c343a40,
Gui, Add, Radio, x30 y440 h15 Checked vGui_HuntAuto gSelectHuntPlace c205375, 자동
Gui, Add, Radio, xp+50 yp h15 vGui_HuntPonam gSelectHuntPlace c6977a9, 포남
Gui, Add, Radio, xp+50 yp h15 vGui_HuntPobuk gSelectHuntPlace cc197d2, 포북
Gui, Font
Gui, Font, s8 Bold,Arial
Gui, Add, GroupBox, x25 y457 w205 h33 center c343a40,
Gui, Font
Gui, Font, S8  Bold,Arial
Gui, Add, Radio, x30 y470 h15 Checked v랜덤차원 g차원체크 c205375, 랜덤
Gui, Add, Radio, xp+50 yp h15 v알파차원 g차원체크 c6977a9, 알파
Gui, Add, Radio, xp+50 yp h15 v베타차원 g차원체크 cc197d2, 베타
Gui, Add, Radio, xp+50 yp h15 v감마차원 g차원체크 cd3b1c2, 감마
Gui, Font
Gui, Font, s8  Bold,Arial
Gui, Add, GroupBox, x275 y240 w120 h50 center c343a40 v줍줍설정, [줍줍/소각 설정]
Gui, Font
Gui, Font, s8  Bold ,Arial
Gui, Add, Radio, xp+15 yp+22 w40 h15 vGui_jjON c205375, ON
Gui, Font
Gui, Font, s8  Bold ,Arial
Gui, Add, Radio, xp+50 yp w50 h15 vGui_jjOFF c6977a9, OFF
Gui, Font
Gui, Font, S8  Bold,Arial
GuiControl,HIDE,줍줍설정
GuiControl,HIDE,Gui_jjON
GuiControl,HIDE,Gui_jjOFF
Gui, Font
Gui, Font, s9  Bold,Arial
Gui, Add, GroupBox, x12 y515 w760 h138 center v그룹모니터링 c343a40,[모니터링]
Gui, Font
Gui, Font, s8  Bold, Arial
Gui, Add, Button, x672 y530 w90 h30 Disabled vGui_StopButton g일시정지, 일시정지
Gui, Add, Button, xp yp w90 h30 Disabled vGui_RestartButton g재시작, 정지해제
Gui, Add, Button, xp y572 w90 h30 Disabled vGui_Resetting gResetting, 재실행
Gui, Add, Button, xp y614 w90 h30 g종료, 종료
GuiControl,HIDE,Gui_RestartButton
Gui, Font
Gui, Font, s8 cSilver,Verdana
Gui, Add, Text, x35 y635 w270 +Left, Made With %아이콘% and ING %A_Year%.%A_MM%.%A_DD%,by 꼴똘룸
Gui, Font
Gui, Font, s9,Arial
Gui, Add, Progress, x15 y495 w218 h15 CE64740 Background555555 vPro_NoWHP ,
Gui, Add, Text, xp yp w218 h15 0x201 Cwhite BackgroundTrans vGui_NowHP,
Gui, Add, Progress, x283 y495 w218 h15 cC1CD0E Background555555 vPro_NoWMP ,
Gui, Add, Text, xp yp w218 0x201 Cwhite BackgroundTrans vGui_NowMP,
Gui, Add, Progress, x551 y495 w218 h15 c9594D5 Background555555 vPro_NoWFP ,
Gui, Add, Text, xp yp w218 0x201 Cwhite BackgroundTrans vGui_NowFP,
Gui, Font
Gui, Font, s9 ,Arial
Gui, Add, Text, x15 y545 w80 +Center v격투, [격투]
Gui, Add, Text, xp+71 y545 w80 +Center v1번, [1번]
Gui, Add, Text, xp+71 y545 w80 +Center v2번, [2번]
Gui, Add, Text, xp+71 y545 w80 +Center v3번, [3번]
Gui, Font
Gui, Font, s9 ,Arial
Gui, Add, Text, x15 y565 w80 +Center vGui_BasicWName0
Gui, Add, Text, xp+71 y565 w80 +Center vGui_BasicWName1
Gui, Add, Text, xp+71 y565 w80 +Center vGui_BasicWName2
Gui, Add, Text, xp+71 y565 w80 +Center vGui_BasicWName3
Gui, Font
Gui, Font, s9 ,Arial
Gui, Add, Text, x15 y585 w80 +Center vGui_BasicWValue0
Gui, Add, Text, xp+71 y585 w80 +Center vGui_BasicWValue1
Gui, Add, Text, xp+71 y585 w80 +Center vGui_BasicWValue2
Gui, Add, Text, xp+71 y585 w80 +Center vGui_BasicWValue3
Gui, Add, Text, x15 y605 w80 +Center vGui_Grade0
Gui, Add, Text, xp+71 y605 w80 +Center vGui_Grade1
Gui, Add, Text, xp+71 y605 w80 +Center vGui_Grade2
Gui, Add, Text, xp+71 y605 w80 +Center vGui_Grade3
Gui, Font
Gui, Font, s11  Bold,Arial
Gui, Add, Text, x312 y535 +Left ,시작체력 :
Gui, Add, Text, xp y565 +Left ,상승체력 :
Gui, Add, Text, xp y595 +Left ,경과시간 :
Gui, Font
Gui, Font, s11  Bold,Arial
Gui, Add, Text, xp y625 +Left, 진행상황 :
Gui, Font
Gui, Font, s15  Bold,Arial
Gui, Add, Text, x400 y532 w105 +Left v시작체력 c121013
Gui, Add, Text, xp y562 w165 +Left v상승체력 cEB596E
Gui, Add, Text, xp y592 w165 +Left v경과시간
Gui, Font
Gui, Font, s9  Bold,Arial
Gui, Add, Text, xp y625 w270 +Left vGui_NowState
Gui, Tab, 스펠그레이드 / 목록스캔
Gui, Font
Gui, Font, s8  Bold,Arial
Gui, Add, text, x20 y30 w335 h510 c343a40, [스펠 그레이드 설정]-------------------------------------------
Gui, Font
Gui, Font, s8  ,Arial
Gui, Add, Text, x35 y55 +Center, 스펠 3슬롯
Gui, Add, Text, xp+80 yp +Center, 스펠 4슬롯
Gui, Add, Text, xp+80 yp +Center, 스펠 5슬롯
Gui, Add, Text, xp+80 yp +Center, 스펠 6슬롯
Gui, Add, Text, xp+110 yp +Center, 스펠 7슬롯
Gui, Add, Text, xp+80 yp +Center, 스펠 8슬롯
Gui, Add, Text, xp+80 yp +Center, 스펠 9슬롯
Gui, Add, Text, xp+80 yp +Center, 스펠 10슬롯
Gui, Add, Text, x35 yp+70 +Center, 스펠 11슬롯
Gui, Add, Text, xp+80 yp +Center, 스펠 12슬롯
Gui, Add, Text, xp+80 yp +Center, 스펠 13슬롯
Gui, Add, Text, xp+80 yp +Center, 스펠 14슬롯
Gui, Add, Text, xp+110 yp +Center, 스펠 15슬롯
Gui, Add, Text, xp+80 yp +Center, 스펠 16슬롯
Gui, Add, Text, xp+80 yp +Center, 스펠 17슬롯
Gui, Add, Text, xp+80 yp +Center, 스펠 18슬롯
Gui, Font
Gui, Font, s8  ,Arial
Gui, Add, edit, xp-605 yp-50 w80 +Left Disabled vGui_MagicName3
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicName4
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicName5
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicName6
Gui, Add, edit, xp+110 yp w80 +Left Disabled vGui_MagicName7
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicName8
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicName9
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicName10
Gui, Add, edit, xp-590 yp+70 w80 +Left Disabled vGui_MagicName11
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicName12
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicName13
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicName14
Gui, Add, edit, xp+110 yp w80 +Left Disabled vGui_MagicName15
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicName16
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicName17
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicName18
Gui, Font
Gui, Font, s8  ,Arial
Gui, Add, edit, xp-590 yp-47 w80 +Left Disabled vGui_MagicValue3
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicValue4
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicValue5
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicValue6
Gui, Add, edit, xp+110 yp w80 +Left Disabled vGui_MagicValue7
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicValue8
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicValue9
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicValue10
Gui, Add, edit, xp-590 yp+70 w80 +Left Disabled vGui_MagicValue11
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicValue12
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicValue13
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicValue14
Gui, Add, edit, xp+110 yp w80 +Left Disabled vGui_MagicValue15
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicValue16
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicValue17
Gui, Add, edit, xp+80 yp w80 +Left Disabled vGui_MagicValue18
Gui, Font
Gui, Font, s8  c000000,Arial
Gui, Add, Checkbox, xp-590 yp-115 w15 h15 gCheckM3 vGui_MagicCheck3
Gui, Add, Checkbox, xp+80 yp w15 h15 gCheckM4 vGui_MagicCheck4
Gui, Add, Checkbox, xp+80 yp w15 h15 gCheckM5 vGui_MagicCheck5
Gui, Add, Checkbox, xp+80 yp w15 h15 gCheckM6 vGui_MagicCheck6
Gui, Add, Checkbox, xp+110 yp w15 h15 gCheckM7 vGui_MagicCheck7
Gui, Add, Checkbox, xp+80 yp w15 h15 gCheckM8 vGui_MagicCheck8
Gui, Add, Checkbox, xp+80 yp w15 h15 gCheckM9 vGui_MagicCheck9
Gui, Add, Checkbox, xp+80 yp w15 h15 gCheckM10 vGui_MagicCheck10
Gui, Add, Checkbox, xp-590 yp+70 w15 h15 gCheckM11 vGui_MagicCheck11
Gui, Add, Checkbox, xp+80 yp w15 h15 gCheckM12 vGui_MagicCheck12
Gui, Add, Checkbox, xp+80 yp w15 h15 gCheckM13 vGui_MagicCheck13
Gui, Add, Checkbox, xp+80 yp w15 h15 gCheckM14 vGui_MagicCheck14
Gui, Add, Checkbox, xp+110 yp w15 h15 gCheckM15 vGui_MagicCheck15
Gui, Add, Checkbox, xp+80 yp w15 h15 gCheckM16 vGui_MagicCheck16
Gui, Add, Checkbox, xp+80 yp w15 h15 gCheckM17 vGui_MagicCheck17
Gui, Add, Checkbox, xp+80 yp w15 h15 gCheckM18 vGui_MagicCheck18
Gui,Font
Gui, Font, s8 ,Arial
Gui, Add, Listview, x20 y200 w320 h320 -Multi v어빌리티리스트, 어빌순서 | 어빌명 | 그레이드 | 어빌리티
Gui, listview , 어빌리티리스트
LV_ModifyCol(1,"68 Right")
LV_ModifyCol(2,"90 Center")
LV_ModifyCol(3,"70 Center")
LV_ModifyCol(4,"87 Left")
Gui, Add, Button, xp+110 y+1 w90 g어빌리티리스트갱신, 어빌목록스캔
Gui, Font
Gui, Font, s8,Arial
Gui, Add, ListView, x370 y200 w320 h320 v마법리스트,스펠순서 | 스펠명 | 그레이드 | 스펠레벨
LV_ModifyCol(1,"68 Right")
LV_ModifyCol(2,"90 Center")
LV_ModifyCol(3,"70 Center")
LV_ModifyCol(4,"88 Left")
Gui, Add, Button, xp+110 y+1 w90 g마법갱신,마법목록스캔
Gui, Font
Gui, Font, s8 cSilver,Verdana
Gui, Add, Text, x35 y560 w270 +Left, Made With %아이콘% and ING %A_Year%.%A_MM%.%A_DD%,by 꼴똘룸
Gui, Tab, 어빌리티그레이드
Gui, Font
Gui, Font, s8  Bold,Arial
Gui, Add, text, x20 y30 w335 h510 c343a40, [어빌리티 그레이드 설정]-------------------------------------------
Gui, Add, Button, x370 y29 w70 h20 g어빌선택,전체선택
Gui, Add, Button, x450 y29 w70 h20 g어빌해제,전체해제
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, Text, x35 y55 +Center, 어빌 1슬롯
Gui, Add, Text, x115 y55 +Center, 어빌 2슬롯
Gui, Add, Text, x195 y55 +Center, 어빌 3슬롯
Gui, Add, Text, x275 y55 +Center, 어빌 4슬롯
Gui, Add, Text, x35 y125 +Center, 어빌 5슬롯
Gui, Add, Text, x115 y125 +Center, 어빌 6슬롯
Gui, Add, Text, x195 y125 +Center, 어빌 7슬롯
Gui, Add, Text, x275 y125 +Center, 어빌 8슬롯
Gui, Add, Text, x35 y195 +Center, 어빌 9슬롯
Gui, Add, Text, x115 y195 +Center, 어빌 10슬롯
Gui, Add, Text, x195 y195 +Center, 어빌11슬롯
Gui, Add, Text, x275 y195 +Center, 어빌12슬롯
Gui, Add, Text, x35 y265 +Center, 어빌13슬롯
Gui, Add, Text, x115 y265 +Center, 어빌14슬롯
Gui, Add, Text, x195 y265 +Center, 어빌15슬롯
Gui, Add, Text, x275 y265 +Center, 어빌16슬롯
Gui, Add, Text, x35 y335 +Center, 어빌17슬롯
Gui, Add, Text, x115 y335 +Center, 어빌18슬롯
Gui, Add, Text, x195 y335 +Center, 어빌19슬롯
Gui, Add, Text, x275 y335 +Center, 어빌20슬롯
Gui, Add, Text, x35 y405 +Center, 어빌21슬롯
Gui, Add, Text, x115 y405 +Center, 어빌22슬롯
Gui, Add, Text, x195 y405 +Center, 어빌23슬롯
Gui, Add, Text, x275 y405 +Center, 어빌24슬롯
Gui, Add, Text, x35 y475 +Center, 어빌25슬롯
Gui, Add, Text, x115 y475 +Center, 어빌26슬롯
Gui, Add, Text, x195 y475 +Center, 어빌27슬롯
Gui, Add, Text, x275 y475 +Center, 어빌28슬롯
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, Text, x385 y55 +Center, 어빌29슬롯
Gui, Add, Text, x465 y55 +Center, 어빌30슬롯
Gui, Add, Text, x545 y55 +Center, 어빌31슬롯
Gui, Add, Text, x625 y55 +Center, 어빌32슬롯
Gui, Add, Text, x385 y125 +Center, 어빌33슬롯
Gui, Add, Text, x465 y125 +Center, 어빌34슬롯
Gui, Add, Text, x545 y125 +Center, 어빌35슬롯
Gui, Add, Text, x625 y125 +Center, 어빌36슬롯
Gui, Add, Text, x385 y195 +Center, 어빌37슬롯
Gui, Add, Text, x465 y195 +Center, 어빌38슬롯
Gui, Add, Text, x545 y195 +Center, 어빌39슬롯
Gui, Add, Text, x625 y195 +Center, 어빌40슬롯
Gui, Add, Text, x385 y265 +Center, 어빌41슬롯
Gui, Add, Text, x465 y265 +Center, 어빌42슬롯
Gui, Add, Text, x545 y265 +Center, 어빌43슬롯
Gui, Add, Text, x625 y265 +Center, 어빌44슬롯
Gui, Add, Text, x385 y335 +Center, 어빌45슬롯
Gui, Add, Text, x465 y335 +Center, 어빌46슬롯
Gui, Add, Text, x545 y335 +Center, 어빌47슬롯
Gui, Add, Text, x625 y335 +Center, 어빌48슬롯
Gui, Add, Text, x385 y405 +Center, 어빌49슬롯
Gui, Add, Text, x465 y405 +Center, 어빌50슬롯
Gui, Add, Text, x545 y405 +Center, 어빌51슬롯
Gui, Add, Text, x625 y405 +Center, 어빌52슬롯
Gui, Add, Text, x385 y475 +Center, 어빌53슬롯
Gui, Add, Text, x465 y475 +Center, 어빌54슬롯
Gui, Add, Text, x545 y475 +Center, 어빌55슬롯
Gui, Add, Text, x625 y475 +Center, 어빌56슬롯
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, Edit, x20 y75 w80 +Left  Disabled vGui_WeaponName1
Gui, Add, edit, x100 y75 w80 +Left Disabled vGui_WeaponName2
Gui, Add, edit, x180 y75 w80 +Left Disabled vGui_WeaponName3
Gui, Add, edit, x260 y75 w80 +Left Disabled vGui_WeaponName4
Gui, Add, edit, x20 y145 w80 +Left Disabled vGui_WeaponName5
Gui, Add, edit, x100 y145 w80 +Left Disabled vGui_WeaponName6
Gui, Add, edit, x180 y145 w80 +Left Disabled vGui_WeaponName7
Gui, Add, edit, x260 y145 w80 +Left Disabled vGui_WeaponName8
Gui, Add, edit, x20 y215 w80 +Left Disabled vGui_WeaponName9
Gui, Add, edit, x100 y215 w80 +Left Disabled vGui_WeaponName10
Gui, Add, edit, x180 y215 w80 +Left Disabled vGui_WeaponName11
Gui, Add, edit, x260 y215 w80 +Left Disabled vGui_WeaponName12
Gui, Add, edit, x20 y285 w80 +Left Disabled vGui_WeaponName13
Gui, Add, edit, x100 y285 w80 +Left Disabled vGui_WeaponName14
Gui, Add, edit, x180 y285 w80 +Left Disabled vGui_WeaponName15
Gui, Add, edit, x260 y285 w80 +Left Disabled vGui_WeaponName16
Gui, Add, edit, x20 y355 w80 +Left Disabled vGui_WeaponName17
Gui, Add, edit, x100 y355 w80 +Left Disabled vGui_WeaponName18
Gui, Add, edit, x180 y355 w80 +Left Disabled vGui_WeaponName19
Gui, Add, edit, x260 y355 w80 +Left Disabled vGui_WeaponName20
Gui, Add, edit, x20 y425 w80 +Left Disabled vGui_WeaponName21
Gui, Add, edit, x100 y425 w80 +Left Disabled vGui_WeaponName22
Gui, Add, edit, x180 y425 w80 +Left Disabled vGui_WeaponName23
Gui, Add, edit, x260 y425 w80 +Left Disabled vGui_WeaponName24
Gui, Add, edit, x20 y495 w80 +Left Disabled vGui_WeaponName25
Gui, Add, edit, x100 y495 w80 +Left Disabled vGui_WeaponName26
Gui, Add, edit, x180 y495 w80 +Left Disabled vGui_WeaponName27
Gui, Add, edit, x260 y495 w80 +Left Disabled vGui_WeaponName28
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, Edit, x370 y75 w80 +Left Disabled vGui_WeaponName29
Gui, Add, edit, x450 y75 w80 +Left Disabled vGui_WeaponName30
Gui, Add, edit, x530 y75 w80 +Left Disabled vGui_WeaponName31
Gui, Add, edit, x610 y75 w80 +Left Disabled vGui_WeaponName32
Gui, Add, edit, x370 y145 w80 +Left Disabled vGui_WeaponName33
Gui, Add, edit, x450 y145 w80 +Left Disabled vGui_WeaponName34
Gui, Add, edit, x530 y145 w80 +Left Disabled vGui_WeaponName35
Gui, Add, edit, x610 y145 w80 +Left Disabled vGui_WeaponName36
Gui, Add, edit, x370 y215 w80 +Left Disabled vGui_WeaponName37
Gui, Add, edit, x450 y215 w80 +Left Disabled vGui_WeaponName38
Gui, Add, edit, x530 y215 w80 +Left Disabled vGui_WeaponName39
Gui, Add, edit, x610 y215 w80 +Left Disabled vGui_WeaponName40
Gui, Add, edit, x370 y285 w80 +Left Disabled vGui_WeaponName41
Gui, Add, edit, x450 y285 w80 +Left Disabled vGui_WeaponName42
Gui, Add, edit, x530 y285 w80 +Left Disabled vGui_WeaponName43
Gui, Add, edit, x610 y285 w80 +Left Disabled vGui_WeaponName44
Gui, Add, edit, x370 y355 w80 +Left Disabled vGui_WeaponName45
Gui, Add, edit, x450 y355 w80 +Left Disabled vGui_WeaponName46
Gui, Add, edit, x530 y355 w80 +Left Disabled vGui_WeaponName47
Gui, Add, edit, x610 y355 w80 +Left Disabled vGui_WeaponName48
Gui, Add, edit, x370 y425 w80 +Left Disabled vGui_WeaponName49
Gui, Add, edit, x450 y425 w80 +Left Disabled vGui_WeaponName50
Gui, Add, edit, x530 y425 w80 +Left Disabled vGui_WeaponName51
Gui, Add, edit, x610 y425 w80 +Left Disabled vGui_WeaponName52
Gui, Add, edit, x370 y495 w80 +Left Disabled vGui_WeaponName53
Gui, Add, edit, x450 y495 w80 +Left Disabled vGui_WeaponName54
Gui, Add, edit, x530 y495 w80 +Left Disabled vGui_WeaponName55
Gui, Add, edit, x610 y495 w80 +Left Disabled vGui_WeaponName56
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, edit, x20 y98 w80 +Left Disabled vGui_WeaponValue1
Gui, Add, edit, x100 y98 w80 +Left Disabled vGui_WeaponValue2
Gui, Add, edit, x180 y98 w80 +Left Disabled vGui_WeaponValue3
Gui, Add, edit, x260 y98 w80 +Left Disabled vGui_WeaponValue4
Gui, Add, edit, x20 y168 w80 +Left Disabled vGui_WeaponValue5
Gui, Add, edit, x100 y168 w80 +Left Disabled vGui_WeaponValue6
Gui, Add, edit, x180 y168 w80 +Left Disabled vGui_WeaponValue7
Gui, Add, edit, x260 y168 w80 +Left Disabled vGui_WeaponValue8
Gui, Add, edit, x20 y238 w80 +Left Disabled vGui_WeaponValue9
Gui, Add, edit, x100 y238 w80 +Left Disabled vGui_WeaponValue10
Gui, Add, edit, x180 y238 w80 +Left Disabled vGui_WeaponValue11
Gui, Add, edit, x260 y238 w80 +Left Disabled vGui_WeaponValue12
Gui, Add, edit, x20 y308 w80 +Left Disabled vGui_WeaponValue13
Gui, Add, edit, x100 y308 w80 +Left Disabled vGui_WeaponValue14
Gui, Add, edit, x180 y308 w80 +Left Disabled vGui_WeaponValue15
Gui, Add, edit, x260 y308 w80 +Left Disabled vGui_WeaponValue16
Gui, Add, edit, x20 y378 w80 +Left Disabled vGui_WeaponValue17
Gui, Add, edit, x100 y378 w80 +Left Disabled vGui_WeaponValue18
Gui, Add, edit, x180 y378 w80 +Left Disabled vGui_WeaponValue19
Gui, Add, edit, x260 y378 w80 +Left Disabled vGui_WeaponValue20
Gui, Add, edit, x20 y448 w80 +Left Disabled vGui_WeaponValue21
Gui, Add, edit, x100 y448 w80 +Left Disabled vGui_WeaponValue22
Gui, Add, edit, x180 y448 w80 +Left Disabled vGui_WeaponValue23
Gui, Add, edit, x260 y448 w80 +Left Disabled vGui_WeaponValue24
Gui, Add, edit, x20 y518 w80 +Left Disabled vGui_WeaponValue25
Gui, Add, edit, x100 y518 w80 +Left Disabled vGui_WeaponValue26
Gui, Add, edit, x180 y518 w80 +Left Disabled vGui_WeaponValue27
Gui, Add, edit, x260 y518 w80 +Left Disabled vGui_WeaponValue28
Gui, Add, edit, x370 y98 w80 +Left Disabled vGui_WeaponValue29
Gui, Add, edit, x450 y98 w80 +Left Disabled vGui_WeaponValue30
Gui, Add, edit, x530 y98 w80 +Left Disabled vGui_WeaponValue31
Gui, Add, edit, x610 y98 w80 +Left Disabled vGui_WeaponValue32
Gui, Add, edit, x370 y168 w80 +Left Disabled vGui_WeaponValue33
Gui, Add, edit, x450 y168 w80 +Left Disabled vGui_WeaponValue34
Gui, Add, edit, x530 y168 w80 +Left Disabled vGui_WeaponValue35
Gui, Add, edit, x610 y168 w80 +Left Disabled vGui_WeaponValue36
Gui, Add, edit, x370 y238 w80 +Left Disabled vGui_WeaponValue37
Gui, Add, edit, x450 y238 w80 +Left Disabled vGui_WeaponValue38
Gui, Add, edit, x530 y238 w80 +Left Disabled vGui_WeaponValue39
Gui, Add, edit, x610 y238 w80 +Left Disabled vGui_WeaponValue40
Gui, Add, edit, x370 y308 w80 +Left Disabled vGui_WeaponValue41
Gui, Add, edit, x450 y308 w80 +Left Disabled vGui_WeaponValue42
Gui, Add, edit, x530 y308 w80 +Left Disabled vGui_WeaponValue43
Gui, Add, edit, x610 y308 w80 +Left Disabled vGui_WeaponValue44
Gui, Add, edit, x370 y378 w80 +Left Disabled vGui_WeaponValue45
Gui, Add, edit, x450 y378 w80 +Left Disabled vGui_WeaponValue46
Gui, Add, edit, x530 y378 w80 +Left Disabled vGui_WeaponValue47
Gui, Add, edit, x610 y378 w80 +Left Disabled vGui_WeaponValue48
Gui, Add, edit, x370 y448 w80 +Left Disabled vGui_WeaponValue49
Gui, Add, edit, x450 y448 w80 +Left Disabled vGui_WeaponValue50
Gui, Add, edit, x530 y448 w80 +Left Disabled vGui_WeaponValue51
Gui, Add, edit, x610 y448 w80 +Left Disabled vGui_WeaponValue52
Gui, Add, edit, x370 y518 w80 +Left Disabled vGui_WeaponValue53
Gui, Add, edit, x450 y518 w80 +Left Disabled vGui_WeaponValue54
Gui, Add, edit, x530 y518 w80 +Left Disabled vGui_WeaponValue55
Gui, Add, edit, x610 y518 w80 +Left Disabled vGui_WeaponValue56
Gui, Font
Gui, Font, s8 c000000,Arial
Gui, Add, Checkbox, x20 y53 w15 h15 gCheckW1 vGui_WeaponCheck1
Gui, Add, Checkbox, x100 y53 w15 h15 gCheckW2 vGui_WeaponCheck2
Gui, Add, Checkbox, x180 y53 w15 h15 gCheckW3 vGui_WeaponCheck3
Gui, Add, Checkbox, x260 y53 w15 h15 gCheckW4 vGui_WeaponCheck4
Gui, Add, Checkbox, x20 y123 w15 h15 gCheckW5 vGui_WeaponCheck5
Gui, Add, Checkbox, x100 y123 w15 h15 gCheckW6 vGui_WeaponCheck6
Gui, Add, Checkbox, x180 y123 w15 h15 gCheckW7 vGui_WeaponCheck7
Gui, Add, Checkbox, x260 y123 w15 h15 gCheckW8 vGui_WeaponCheck8
Gui, Add, Checkbox, x20 y193 w15 h15 gCheckW9 vGui_WeaponCheck9
Gui, Add, Checkbox, x100 y193 w15 h15 gCheckW10 vGui_WeaponCheck10
Gui, Add, Checkbox, x180 y193 w15 h15 gCheckW11 vGui_WeaponCheck11
Gui, Add, Checkbox, x260 y193 w15 h15 gCheckW12 vGui_WeaponCheck12
Gui, Add, Checkbox, x20 y263 w15 h15 gCheckW13 vGui_WeaponCheck13
Gui, Add, Checkbox, x100 y263 w15 h15 gCheckW14 vGui_WeaponCheck14
Gui, Add, Checkbox, x180 y263 w15 h15 gCheckW15 vGui_WeaponCheck15
Gui, Add, Checkbox, x260 y263 w15 h15 gCheckW16 vGui_WeaponCheck16
Gui, Add, Checkbox, x20 y333 w15 h15 gCheckW17 vGui_WeaponCheck17
Gui, Add, Checkbox, x100 y333 w15 h15 gCheckW18 vGui_WeaponCheck18
Gui, Add, Checkbox, x180 y333 w15 h15 gCheckW19 vGui_WeaponCheck19
Gui, Add, Checkbox, x260 y333 w15 h15 gCheckW20 vGui_WeaponCheck20
Gui, Add, Checkbox, x20 y403 w15 h15 gCheckW21 vGui_WeaponCheck21
Gui, Add, Checkbox, x100 y403 w15 h15 gCheckW22 vGui_WeaponCheck22
Gui, Add, Checkbox, x180 y403 w15 h15 gCheckW23 vGui_WeaponCheck23
Gui, Add, Checkbox, x260 y403 w15 h15 gCheckW24 vGui_WeaponCheck24
Gui, Add, Checkbox, x20 y473 w15 h15 gCheckW25 vGui_WeaponCheck25
Gui, Add, Checkbox, x100 y473 w15 h15 gCheckW26 vGui_WeaponCheck26
Gui, Add, Checkbox, x180 y473 w15 h15 gCheckW27 vGui_WeaponCheck27
Gui, Add, Checkbox, x260 y473 w15 h15 gCheckW28 vGui_WeaponCheck28
Gui, Add, Checkbox, x370 y53 w15 h15 gCheckW29 vGui_WeaponCheck29
Gui, Add, Checkbox, x450 y53 w15 h15 gCheckW30 vGui_WeaponCheck30
Gui, Add, Checkbox, x530 y53 w15 h15 gCheckW31 vGui_WeaponCheck31
Gui, Add, Checkbox, x610 y53 w15 h15 gCheckW32 vGui_WeaponCheck32
Gui, Add, Checkbox, x370 y123 w15 h15 gCheckW33 vGui_WeaponCheck33
Gui, Add, Checkbox, x450 y123 w15 h15 gCheckW34 vGui_WeaponCheck34
Gui, Add, Checkbox, x530 y123 w15 h15 gCheckW35 vGui_WeaponCheck35
Gui, Add, Checkbox, x610 y123 w15 h15 gCheckW36 vGui_WeaponCheck36
Gui, Add, Checkbox, x370 y193 w15 h15 gCheckW37 vGui_WeaponCheck37
Gui, Add, Checkbox, x450 y193 w15 h15 gCheckW38 vGui_WeaponCheck38
Gui, Add, Checkbox, x530 y193 w15 h15 gCheckW39 vGui_WeaponCheck39
Gui, Add, Checkbox, x610 y193 w15 h15 gCheckW40 vGui_WeaponCheck40
Gui, Add, Checkbox, x370 y263 w15 h15 gCheckW41 vGui_WeaponCheck41
Gui, Add, Checkbox, x450 y263 w15 h15 gCheckW42 vGui_WeaponCheck42
Gui, Add, Checkbox, x530 y263 w15 h15 gCheckW43 vGui_WeaponCheck43
Gui, Add, Checkbox, x610 y263 w15 h15 gCheckW44 vGui_WeaponCheck44
Gui, Add, Checkbox, x370 y333 w15 h15 gCheckW45 vGui_WeaponCheck45
Gui, Add, Checkbox, x450 y333 w15 h15 gCheckW46 vGui_WeaponCheck46
Gui, Add, Checkbox, x530 y333 w15 h15 gCheckW47 vGui_WeaponCheck47
Gui, Add, Checkbox, x610 y333 w15 h15 gCheckW48 vGui_WeaponCheck48
Gui, Add, Checkbox, x370 y403 w15 h15 gCheckW49 vGui_WeaponCheck49
Gui, Add, Checkbox, x450 y403 w15 h15 gCheckW50 vGui_WeaponCheck50
Gui, Add, Checkbox, x530 y403 w15 h15 gCheckW51 vGui_WeaponCheck51
Gui, Add, Checkbox, x610 y403 w15 h15 gCheckW52 vGui_WeaponCheck52
Gui, Add, Checkbox, x370 y473 w15 h15 gCheckW53 vGui_WeaponCheck53
Gui, Add, Checkbox, x450 y473 w15 h15 gCheckW54 vGui_WeaponCheck54
Gui, Add, Checkbox, x530 y473 w15 h15 gCheckW55 vGui_WeaponCheck55
Gui, Add, Checkbox, x610 y473 w15 h15 gCheckW56 vGui_WeaponCheck56
Gui, Font
Gui, Font, s8 cSilver,Verdana
Gui, Add, Text, x35 y560 w270 +Left, Made With %아이콘% and ING %A_Year%.%A_MM%.%A_DD%,by 꼴똘룸
Gui, Font
Gui, Font, S8  Bold,Arial
Gui, Add, GroupBox, x360 y540 w335 h45 c343a40, [그레이드 설정]
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, Checkbox, x370 y560 +Left Checked vGui_Grade c000000, 자동그레이드 설정
Gui, Add, Text, x505 y560 +Center, 강제GRADE
Gui, Add, Button, x651 y556 w40 h21 +Center vGui_FG gforcegrade, 실행
Gui, Font
Gui, Font, s8 ,Arial
Gui, Add, DropDownList, x568 y556 w80 +Left cblack vGui_forceweapon, 선택|격투|검|단검|도|도끼|거대도끼|대검|대도|창, 특수창|봉, 해머|현금|활|거대검|거대도|양손단검|양손도끼|스태프
Gui, Font
Gui, Font, s15 ,Arial
Gui, Add, Text, x495 y552 +Center, |
Gui, Font
Gui, Show, xCenter yCenter w780 h680, 마개조 똘룸체잠%Program%
GuiControl, , Name1, 파티원
GuiControl, , Name2, 파티원
GuiControl, , Name3, 파티원
GuiControl, , Name4, 파티원
GuiControl, , Name5, 파티원
GuiControl, , Name6, 파티원
GuiControl, , Name1, 파티원
GuiControl, , Name2, 파티원
GuiControl, , Name3, 파티원
GuiControl, , Name4, 파티원
GuiControl, , Name5, 파티원
GuiControl, , Name6, 파티원
GuiControl,, A리노아, %A리노아%
GuiControl,, A동파, %A동파%
GuiControl,, A서파, %A서파%
GuiControl,, B리노아, %B리노아%
GuiControl,, B동파, %B동파%
GuiControl,, B서파, %B서파%
GuiControl,, G리노아, %G리노아%
GuiControl,, G동파, %G동파%
GuiControl,, G서파, %G서파%
GuiControl,, A길잃파, %A길잃파%
GuiControl,, B길잃파, %B길잃파%
GuiControl,, G길잃파, %G길잃파%
RegRead, 업데이트체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, S업데이트체크
RegRead, A리노아, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA리노아
RegRead, A동파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA동파
RegRead, A서파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA서파
RegRead, B리노아, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB리노아
RegRead, B동파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB동파
RegRead, B서파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB서파
RegRead, G리노아, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG리노아
RegRead, G동파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG동파
RegRead, G서파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG서파
RegRead, A길잃파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA길잃파
RegRead, B길잃파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB길잃파
RegRead, G길잃파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG길잃파
RegRead, RegUseHPHospital, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPHospital
RegRead, RegVMRE, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, VMRE
RegRead, Reg포탈, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포탈
RegRead, RegUseHPExit, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPExit
RegRead, RegUseMagic, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMagic
RegRead, RegHPExit, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, HPExit
RegRead, RegUseHPPortal, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPPortal
RegRead, RegHPPortal, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, HPPortal
RegRead, RegUseHPLimited, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPLimited
RegRead, RegHPLimited, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, HPLimited
RegRead, RegCritHP, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, CrittHP
RegRead, RegMuba, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba
RegRead, RegWeapon1, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Weapon1
RegRead, RegWeapon2, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Weapon2
RegRead, RegWeapon3, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Weapon3
RegRead, RegUseParty, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseParty
RegRead, RegP1, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P1
RegRead, RegP2, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P2
RegRead, RegP3, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P3
RegRead, RegP4, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P4
RegRead, RegP5, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P5
RegRead, RegP6, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P6
RegRead, RegN1, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N1
RegRead, RegN2, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N2
RegRead, RegN3, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N3
RegRead, RegN4, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N4
RegRead, RegN5, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N5
RegRead, RegN6, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N6
RegRead, RegID, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, ID
RegRead, RegRelog, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, relog
RegRead, RegUseWC1, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC1
RegRead, RegUseWC2, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC2
RegRead, RegUseWC3, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC3
RegRead, RegUseWC4, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC4
RegRead, RegUseWC5, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC5
RegRead, RegUseWC6, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC6
RegRead, RegUseWC7, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC7
RegRead, RegUseWC8, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC8
RegRead, RegUseWC9, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC9
RegRead, RegUseWC10, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC10
RegRead, RegUseWC11, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC11
RegRead, RegUseWC12, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC12
RegRead, RegUseWC13, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC13
RegRead, RegUseWC14, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC14
RegRead, RegUseWC15, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC15
RegRead, RegUseWC16, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC16
RegRead, RegUseWC17, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC17
RegRead, RegUseWC18, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC18
RegRead, RegUseWC19, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC19
RegRead, RegUseWC20, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC20
REGREAD, RegUseWC21, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC21
REGREAD, RegUseWC22, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC22
REGREAD, RegUseWC23, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC23
REGREAD, RegUseWC24, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC24
REGREAD, RegUseWC25, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC25
RegRead, RegUseWC26, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC26
RegRead, RegUseWC27, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC27
RegRead, RegUseWC28, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC28
RegRead, RegUseWC29, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC29
RegRead, RegUseWC30, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC30
RegRead, RegUseWC31, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC31
RegRead, RegUseWC32, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC32
RegRead, RegUseWC33, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC33
RegRead, RegUseWC34, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC34
RegRead, RegUseWC35, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC35
RegRead, RegUseWC36, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC36
RegRead, RegUseWC37, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC37
RegRead, RegUseWC38, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC38
RegRead, RegUseWC39, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC39
RegRead, RegUseWC40, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC40
RegRead, RegUseWC41, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC41
RegRead, RegUseWC42, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC42
RegRead, RegUseWC43, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC43
RegRead, RegUseWC44, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC44
RegRead, RegUseWC45, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC45
REGREAD, RegUseWC46, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC46
REGREAD, RegUseWC47, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC47
REGREAD, RegUseWC48, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC48
REGREAD, RegUseWC49, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC49
REGREAD, RegUseWC50, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC50
REGREAD, RegUseWC51, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC51
REGREAD, RegUseWC52, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC52
REGREAD, RegUseWC53, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC53
REGREAD, RegUseWC54, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC54
REGREAD, RegUseWC55, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC55
REGREAD, RegUseWC56, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC56
REGREAD, RegUseAA1, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA1
REGREAD, RegUseAA2, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA2
REGREAD, RegUseAA3, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA3
REGREAD, RegUseAA4, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA4
REGREAD, RegUseAA5, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA5
REGREAD, RegUseAA6, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA6
REGREAD, RegUseAA7, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA7
REGREAD, RegUseAA8, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA8
REGREAD, RegUseTAA1, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseTAA1
REGREAD, RegUseTAA2, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseTAA2
REGREAD, RegUseTAA3, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseTAA3
REGREAD, RegUseTAAA1, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseTAAA1
RegRead, RegUseMC3, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC3
RegRead, RegUseMC4, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC4
RegRead, RegUseMC5, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC5
RegRead, RegUseMC6, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC6
RegRead, RegUseMC7, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC7
RegRead, RegUseMC8, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC8
RegRead, RegUseMC9, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC9
RegRead, RegUseMC10, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC10
RegRead, RegUseMC11, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC11
RegRead, RegUseMC12, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC12
RegRead, RegUseMC13, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC13
RegRead, RegUseMC14, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC14
RegRead, RegUseMC15, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC15
RegRead, RegUseMC16, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC16
RegRead, RegUseMC17, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC17
RegRead, RegUseMC18, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC18
RegRead, RegPass, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Pass
RegRead, RegServer, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Server
RegRead, RegLogin, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Login
RegRead, RegCharNumber, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, CharNumber
RegRead, RegMNS, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MNS
RegRead, RegEvade, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Evade
RegRead, RegDirect, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Direct
RegRead, RegKONOFF, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, KONOFF
RegRead, Reg방어구방지ONOFF, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 방어구방지ONOFF
RegRead, RegjjONOFF, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, jjONOFF
RegRead, RegMonster, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster
RegRead, RegAllMobLimit, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, AllMobLimit
RegRead, RegPlace, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Place
RegRead, RegLimit0, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Limit0
RegRead, RegLimit1, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Limit1
RegRead, RegLimit2, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Limit2
RegRead, RegLimit3, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Limit3
RegRead, RegParty, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Party
RegRead, RegGrade, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Grade
RegRead, Regloady, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, loady
RegRead, RegPST, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, StartTime
RegRead, RegCFH, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, CFH
RegRead, Reg게임시작x, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 게임시작x
RegRead, Reg게임시작y, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 게임시작y
RegRead, Reg업데이트체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 업데이트체크
RegRead, Reg서버팅김, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 서버팅김
RegRead, Reg파라스감지, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 파라스감지
RegRead, Reg수천감지, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 수천감지
RegRead, Reg파라스방해감지, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 파라스방해감지
RegRead, Reg실행시간, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 실행시간
RegRead, Reg현혹체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 현혹체크
RegRead, Reg폭검체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 폭검체크
RegRead, Reg독침체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 독침체크
RegRead, Reg무기공격체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 무기공격체크
RegRead, Reg대화체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 대화체크
RegRead, Reg명상체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 명상체크
RegRead, Reg더블어택체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 더블어택체크
RegRead, Reg체력향상체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 체력향상체크
RegRead, Reg집중체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 집중체크
RegRead, Reg회피체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 회피체크
RegRead, Reg몸통지르기체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 몸통지르기체크
RegRead, Reg리무브아머체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 리무브아머체크
RegRead, Reg민첩향상체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 민첩향상체크
RegRead, Reg활방어체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 활방어체크
RegRead, Reg마력향상체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 마력향상체크
RegRead, Reg마력방어체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 마력방어체크
RegRead, Reg퀵슬롯4번, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯4번
RegRead, Reg퀵슬롯5번, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯5번
RegRead, Reg퀵슬롯6번, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯6번
RegRead, Reg퀵슬롯7번, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯7번
RegRead, Reg포북생콩, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포북생콩
RegRead, Reg포남생콩, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포남생콩
RegRead, Reg가방, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 가방
if(RegVMRE = 1)
{
VMRESET := 1
}
if(RegVMRE = 0)
{
VMRESET := 0
}
if(Reg퀵슬롯4번 = 1)
{
GuiControl, , 4번사용, 1
}
if(Reg퀵슬롯4번 = 0)
{
GuiControl, , 4번사용, 0
}
if(Reg퀵슬롯5번 = 1)
{
GuiControl, , 5번사용, 1
}
if(Reg퀵슬롯5번 = 0)
{
GuiControl, , 5번사용, 0
}
if(Reg퀵슬롯6번 = 1)
{
GuiControl, , 6번사용, 1
}
if(Reg퀵슬롯6번 = 0)
{
GuiControl, , 6번사용, 0
}
if(Reg퀵슬롯7번 = 1)
{
GuiControl, , 7번사용, 1
}
if(Reg퀵슬롯7번 = 0)
{
GuiControl, , 7번사용, 0
}
if(Reg현혹체크 = 1)
{
GuiControl, , 현혹사용, 1
}
if(Reg현혹체크 = 0)
{
GuiControl, , 현혹사용, 0
}
if(Reg폭검체크 = 1)
{
GuiControl, , 폭검사용, 1
}
if(Reg폭검체크 = 0)
{
GuiControl, , 폭검사용, 0
}
if(Reg독침체크 = 1)
{
GuiControl, , 독침사용, 1
}
if(Reg독침체크 = 0)
{
GuiControl, , 독침사용, 0
}
if(Reg무기공격체크 = 1)
{
GuiControl, , 무기공격사용, 1
}
if(Reg무기공격체크 = 0)
{
GuiControl, , 무기공격사용, 0
}
if(Reg대화체크 = 1)
{
GuiControl, , 대화사용, 1
}
if(Reg대화체크 = 0)
{
GuiControl, , 대화사용, 0
}
if(Reg명상체크 = 1)
{
GuiControl, , 명상사용, 1
}
if(Reg명상체크 = 0)
{
GuiControl, , 명상사용, 0
}
if(Reg더블어택체크 = 1)
{
GuiControl, , 더블어택사용, 1
}
if(Reg더블어택체크 = 0)
{
GuiControl, , 더블어택사용, 0
}
if(Reg체력향상체크 = 1)
{
GuiControl, , 체력향상사용, 1
}
if(Reg체력향상체크 = 0)
{
GuiControl, , 체력향상사용, 0
}
if(Reg집중체크 = 1)
{
GuiControl, , 집중사용, 1
}
if(Reg집중체크 = 0)
{
GuiControl, , 집중사용, 0
}
if(Reg회피체크 = 1)
{
GuiControl, , 회피사용, 1
}
if(Reg회피체크 = 0)
{
GuiControl, , 회피사용, 0
}
if(Reg몸통지르기체크 = 1)
{
GuiControl, , 몸통지르기사용, 1
}
if(Reg몸통지르기체크 = 0)
{
GuiControl, , 몸통지르기사용, 0
}
if(Reg리무브아머체크 = 1)
{
GuiControl, , 리무브아머사용, 1
}
if(Reg리무브아머체크 = 0)
{
GuiControl, , 리무브아머사용, 0
}
if(Reg민첩향상체크 = 1)
{
GuiControl, , 민첩향상사용, 1
}
if(Reg민첩향상체크 = 0)
{
GuiControl, , 민첩향상사용, 0
}
if(Reg활방어체크 = 1)
{
GuiControl, , 활방어사용, 1
}
if(Reg활방어체크 = 0)
{
GuiControl, , 활방어사용, 0
}
if(Reg마력향상체크 = 1)
{
GuiControl, , 마력향상사용, 1
}
if(Reg마력향상체크 = 0)
{
GuiControl, , 마력향상사용, 0
}
if(Reg마력방어체크 = 1)
{
GuiControl, , 마력방어사용, 1
}
if(Reg마력방어체크 = 0)
{
GuiControl, , 마력방어사용, 0
}
if( Reg실행시간 != 0 )
{
GuiControl, , 실행시간, %Reg실행시간% %아이콘%
}
if(Reg업데이트체크 = 1)
{
업데이트체크 := 1
}
if(Reg업데이트체크 = 0)
{
업데이트체크 := 0
}
ttgm = 0
if(Reg서버팅김 = "" or Reg서버팅김 == 0)
{
서버팅김 := 0
}else{
서버팅김 := Reg서버팅김
}
if(Reg파라스감지 = "" or Reg파라스감지 == 0)
{
파라스감지 := 0
}else{
파라스감지 := Reg파라스감지
}
if(Reg수천감지 = "" or Reg수천감지 == 0)
{
수천감지 := 0
}else{
수천감지 := Reg수천감지
}
if(Reg파라스방해감지 = "" or Reg파라스방해감지 == 0)
{
파라스방해감지 := 0
}else{
파라스방해감지 := Reg파라스방해감지
}
SB_SetText("섭팅 : " . 서버팅김 . "  파라스방해 : " . 파라스감지 . "  수천방해 : " . 수천감지,3)
if( Reg포탈 = 0 )
{
CountPortal := 0
차원 = 0
GuiControl,,알파차원,1
}
if( Reg포탈 = 1 )
{
CountPortal := 1
차원 = 0
GuiControl,,베타차원,1
}
if( Reg포탈 = 2 )
{
CountPortal := 2
차원 = 0
GuiControl,,감마차원,1
}
if( Reg포탈 = 3 )
{
차원 = 1
GuiControl,,랜덤차원,1
}
if( Reg게임시작x != 0 and Reg게임시작y != 0)
{
게임시작x := Reg게임시작x
게임시작y := Reg게임시작y
GuiControl, , 좌표x, %게임시작x%
GuiControl, , 좌표y, %게임시작y%
}
if( Reg게임시작x == 0 and Reg게임시작y == 0 and Reg게임시작x = "" and Reg게임시작y = "")
{
GuiControl, , 좌표x, Ctrl +
GuiControl, , 좌표y, Q
}
GuiControl, , Gui_NexonID, %RegID%
GuiControl, , Gui_NexonPassWord, %RegPass%
GuiControl, Choose, Gui_CharNumber, %RegCharNumber%
GuiControl, Choose, Gui_Server, %RegServer%
GuiControl, Choose, Gui_Login, %RegLogin%
GuiControl, Choose, 포북생콩설정, %Reg포북생콩%
GuiControl, Choose, 포남생콩설정, %Reg포남생콩%
GuiControl, Choose, 가방설정, %Reg가방%
if(Reg포북생콩 = "FP 500이하")
{
포북생콩섭취 := 500
}
if(Reg포북생콩 = "상시사용")
{
포북생콩섭취 := 1000
}
if(Reg포남생콩 = "FP 500이하")
{
포남생콩섭취 := 500
}
if(Reg포남생콩 = "상시사용")
{
포남생콩섭취 := 1000
}
if(Reg가방 = "줍줍끄기")
{
GuiControl,,Gui_jjOFF,1
jelan.write(0x0047B3EC, 0x4D, "Char", aOffsets*)
}
if(Reg가방 = "제한없음")
{
GuiControl,,Gui_jjON,1
가방수량체크 := 50
jelan.write(0x0047B3EC, 0x02, "Char", aOffsets*)
}
if(Reg가방 = "40개 초과시")
{
GuiControl,,Gui_jjON,1
가방수량체크 := 40
jelan.write(0x0047B3EC, 0x02, "Char", aOffsets*)
}
if(Reg가방 = "45개 초과시")
{
GuiControl,,Gui_jjON,1
가방수량체크 := 45
jelan.write(0x0047B3EC, 0x02, "Char", aOffsets*)
}
AA1 := RegUseAA1 + 1
AA2 := RegUseAA2 + 1
AA3 := RegUseAA3 + 1
AA4 := RegUseAA4 + 1
AA5 := RegUseAA5 + 1
AA6 := RegUseAA6 + 1
AA7 := RegUseAA7 + 1
AA8 := RegUseAA8 + 1
TAA1 := RegUseTAA1 + 1
TAA2 := RegUseTAA2 + 1
TAA3 := RegUseTAA3 + 1
TAAA1 := RegUseTAAA1 + 1
TAAA2 := RegUseTAAA2 + 1
TAAA3 := RegUseTAAA3 + 1
TAAA4 := RegUseTAAA4 + 1
if(Regloady = 1)
{
RPST := RegPST
RCFH := RegCFH
ttgm = 1
loady = 2
}
if(Reglogin = "넥슨플러그")
{
GuiControl, HIDE, Gui_NexonID
GuiControl, HIDE, Gui_NexonPassWord
GuiControl, SHOW, 넥슨x
GuiControl, SHOW, 넥슨y
GuiControl, SHOW, 좌표x
GuiControl, SHOW, 좌표y
좌표고정 := 1
}
if(Reglogin = "인터넷")
{
GuiControl, SHOW, Gui_NexonID
GuiControl, SHOW, Gui_NexonPassWord
GuiControl, HIDE, 넥슨x
GuiControl, HIDE, 넥슨y
GuiControl, HIDE, 좌표x
GuiControl, HIDE, 좌표y
좌표고정 := 0
}
if(RegUseHPExit = 1)
{
GuiControl, , Gui_CheckUseHPExit, 1
GuiControl, Enable, Gui_HPExit
}
if(RegUseHPLimited = 1)
{
GuiControl, , Gui_CheckUseHPLimited, 1
GuiControl, Enable, Gui_HPLimited
}
if(RegUseMagic = 1)
{
GuiControl, , Gui_CheckUseMagic, 1
GuiControl, Enabled, Gui_CHP
GuiControl, Enabled, Gui_MagicNStack
}
if(RegUseMagic = 0)
{
GuiControl, , Gui_CheckUseMagic, 0
GuiControl, Disabled, Gui_CHP
GuiControl, Disabled, Gui_MagicNStack
}
if(RegUseParty = 1)
{
GuiControl, , Gui_CheckUseParty, 1
}
if(RegUseParty = 0)
{
GuiControl, , Gui_CheckUseParty, 0
}
if(RegUseHPPortal = 1)
{
GuiControl, , Gui_CheckUseHPPortal, 1
GuiControl, Enable, Gui_HPPortal
}
if(RegHPLimited != "")
{
GuiControl, , Gui_HPLimited, %RegHPLimited%
}
if(RegHPExit != "")
{
GuiControl, , Gui_HPExit, %RegHPExit%
}
if(RegCritHP != "")
{
GuiControl, , Gui_CHP, %RegCritHP%
}
if(RegHPPortal != "")
{
GuiControl, , Gui_HPPortal, %RegHPPortal%
}
if(RegMuba = 1 or RegMuba = 4)
{
if(RegMuba = 1)
{
GuiControl, , Gui_1Muba, 1
}
if(RegP1 != "")
{
GuiControl, , Name1, %RegP1%
GuiControl, Choose, Gui_P1CharNumber, %RegN1%
}
if(RegP2 != "")
{
GuiControl, , Name2, %RegP2%
GuiControl, Choose, Gui_P2CharNumber, %RegN2%
}
if(RegP3 != "")
{
GuiControl, , Name3, %RegP3%
GuiControl, Choose, Gui_P3CharNumber, %RegN3%
}
if(RegP4 != "")
{
GuiControl, , Name4, %RegP4%
GuiControl, Choose, Gui_P4CharNumber, %RegN4%
}
if(RegP5 != "")
{
GuiControl, , Name5, %RegP5%
GuiControl, Choose, Gui_P5CharNumber, %RegN5%
}
if(RegP6 != "")
{
GuiControl, , Name6, %RegP6%
GuiControl, Choose, Gui_P6CharNumber, %RegN6%
}
if(RegMuba = 4)
{
GuiControl, , Gui_2ButMuba, 1
GuiControl,, Gui_BasicWName0, 격투
}
GuiControl,, Gui_BasicWName1, %RegWeapon1%
}
if(RegMuba = 2 or RegMuba = 5)
{
if(RegMuba = 2)
{
GuiControl, , Gui_2Muba, 1
}
if(RegMuba = 5)
{
GuiControl, , Gui_3ButMuba, 1
}
GuiControl, Enable, Gui_Weapon2
if(RegMuba = 5)
{
GuiControl,, Gui_BasicWName0, 격투
}
GuiControl,, Gui_BasicWName1, %RegWeapon1%
GuiControl,, Gui_BasicWName2, %RegWeapon2%
}
if(RegMuba = 3 or RegMuba = 6)
{
if(RegMuba = 3)
{
GuiControl, , Gui_3Muba, 1
}
if(RegMuba = 6)
{
GuiControl, , Gui_4ButMuba, 1
}
GuiControl, Enable, Gui_Weapon2
GuiControl, Enable, Gui_Weapon3
if(RegMuba = 6)
{
GuiControl, , Gui_BasicWName0, 격투
}
GuiControl,, Gui_BasicWName1, %RegWeapon1%
GuiControl,, Gui_BasicWName2, %RegWeapon2%
GuiControl,, Gui_BasicWName3, %RegWeapon3%
}
GuiControl, Choose, Gui_Weapon1, %RegWeapon1%
GuiControl, Choose, Gui_Weapon2, %RegWeapon2%
GuiControl, Choose, Gui_Weapon3, %RegWeapon3%
if(RegEvade = 1)
{
GuiControl, , Gui_EvadeMand, 1
}
if(RegDirect = 2)
{
GuiControl, , Gui_MoveLoute2, 1
}
if(RegDirect = 3)
{
GuiControl, , Gui_MoveLoute3, 1
}
if(RegDirect = 4)
{
GuiControl, , Gui_MoveLoute4, 1
}
if(RegKONOFF = 1)
{
GuiControl, , Gui_KON, 1
}
if(RegKONOFF = 2)
{
GuiControl, , Gui_KOFF, 1
}
if(Reg방어구방지ONOFF = 1)
{
GuiControl, , 방어구방지ON, 1
}
if(Reg방어구방지ONOFF = 2)
{
GuiControl, , 방어구방지OFF, 1
}
if(RegjjONOFF = 1)
{
GuiControl, , Gui_jjON, 1
}
if(RegjjONOFF = 2)
{
GuiControl, , Gui_jjOFF, 1
}
if(RegMonster = 1)
{
GuiControl, , Gui_Ent, 1
}
if(RegMonster = 2)
{
GuiControl, , Gui_Rockey, 1
}
if(RegMonster = 3)
{
GuiControl, , Gui_EntRockey, 1
}
if(RegMonster = 4)
{
GuiControl, , Gui_Mand, 1
}
if(RegMonster = 5)
{
GuiControl, , Gui_AllMobAND, 1
}
if(RegMonster = 6)
{
GuiControl, , Gui_AllMobOR, 1
}
if(RegMonster = 7)
{
GuiControl, , Gui_MobMagic, 1
}
if(RegAllMobLimit != "")
{
GuiControl, , Gui_AllMobLimit, %RegAllMobLimit%
}
if(RegMonster >= 4 and RegMonster <= 7)
{
GuiControl, , Gui_EvadeMand, 0
GuiControl, Disable, Gui_EvadeMand
}
if(RegMonster >= 5 and RegMonster <= 7)
{
GuiControl, Enable, Gui_AllMobLimit
}
if(RegPlace = 1)
{
if(RegMuba = 2)
{
GuiControl, Enable, Gui_LimitAbility2
}
if(RegMuba = 3)
{
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Enable, Gui_LimitAbility3
}
if(RegMuba = 4)
{
GuiControl, Enable, Gui_LimitAbility0
}
if(RegMuba = 5)
{
GuiControl, Enable, Gui_LimitAbility0
GuiControl, Enable, Gui_LimitAbility2
}
if(RegMuba = 6)
{
GuiControl, Enable, Gui_LimitAbility0
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Enable, Gui_LimitAbility3
}
}
if(RegPlace = 2)
{
GuiControl, , Gui_HuntPonam, 1
GuiControl, Disable, Gui_LimitAbility1
}
if(RegPlace = 3)
{
GuiControl, , Gui_HuntPobuk, 1
GuiControl, Disable, Gui_LimitAbility1
}
if(RegLimit0 != "")
{
GuiControl, , Gui_LimitAbility0, %RegLimit0%
GuiControl, , Gui_LimitAbility1, %RegLimit1%
GuiControl, , Gui_LimitAbility2, %RegLimit2%
GuiControl, , Gui_LimitAbility3, %RegLimit3%
}
if(RegParty = 1)
{
GuiControl, , Gui_PartyOn, 1
}
if(RegParty = 2)
{
GuiControl, , Gui_PartyOff, 1
}
if(RegGrade = 1)
{
GuiControl, , Gui_Grade, 1
}
if(RegP1 != "")
{
GuiControl, , Name1, %RegP1%
GuiControl, Choose, Gui_P1CharNumber, %RegN1%
}
if(RegP2 != "")
{
GuiControl, , Name2, %RegP2%
GuiControl, Choose, Gui_P2CharNumber, %RegN2%
}
if(RegP3 != "")
{
GuiControl, , Name3, %RegP3%
GuiControl, Choose, Gui_P3CharNumber, %RegN3%
}
if(RegP4 != "")
{
GuiControl, , Name4, %RegP4%
GuiControl, Choose, Gui_P4CharNumber, %RegN4%
}
if(RegP5 != "")
{
GuiControl, , Name5, %RegP5%
GuiControl, Choose, Gui_P5CharNumber, %RegN5%
}
if(RegP6 != "")
{
GuiControl, , Name6, %RegP6%
GuiControl, Choose, Gui_P6CharNumber, %RegN6%
}
RMNS := RegMNS
RMNSF := RMNS - 2
GuiControl, Choose, Gui_MagicNStack, %RMNSF%
GuiControlGet, RMNN, , Gui_MagicNStack
if(RegUseWC1 = 1)
{
GuiControl, , Gui_WeaponCheck1, 1
}
if(RegUseWC1 = 0)
{
GuiControl, , Gui_WeaponCheck1, 0
}
if(RegUseWC2 = 1)
{
GuiControl, , Gui_WeaponCheck2, 1
}
if(RegUseWC2 = 0)
{
GuiControl, , Gui_WeaponCheck2, 0
}
if(RegUseWC3 = 1)
{
GuiControl, , Gui_WeaponCheck3, 1
}
if(RegUseWC3 = 0)
{
GuiControl, , Gui_WeaponCheck3, 0
}
if(RegUseWC4 = 1)
{
GuiControl, , Gui_WeaponCheck4, 1
}
if(RegUseWC4 = 0)
{
GuiControl, , Gui_WeaponCheck4, 0
}
if(RegUseWC5 = 1)
{
GuiControl, , Gui_WeaponCheck5, 1
}
if(RegUseWC5 = 0)
{
GuiControl, , Gui_WeaponCheck5, 0
}
if(RegUseWC6 = 1)
{
GuiControl, , Gui_WeaponCheck6, 1
}
if(RegUseWC6 = 0)
{
GuiControl, , Gui_WeaponCheck6, 0
}
if(RegUseWC7 = 1)
{
GuiControl, , Gui_WeaponCheck7, 1
}
if(RegUseWC7 = 0)
{
GuiControl, , Gui_WeaponCheck7, 0
}
if(RegUseWC8 = 1)
{
GuiControl, , Gui_WeaponCheck8, 1
}
if(RegUseWC8 = 0)
{
GuiControl, , Gui_WeaponCheck8, 0
}
if(RegUseWC9 = 1)
{
GuiControl, , Gui_WeaponCheck9, 1
}
if(RegUseWC9 = 0)
{
GuiControl, , Gui_WeaponCheck9, 0
}
if(RegUseWC10 = 1)
{
GuiControl, , Gui_WeaponCheck10, 1
}
if(RegUseWC10 = 0)
{
GuiControl, , Gui_WeaponCheck10, 0
}
if(RegUseWC11 = 1)
{
GuiControl, , Gui_WeaponCheck11, 1
}
if(RegUseWC11 = 0)
{
GuiControl, , Gui_WeaponCheck11, 0
}
if(RegUseWC12 = 1)
{
GuiControl, , Gui_WeaponCheck12, 1
}
if(RegUseWC12 = 0)
{
GuiControl, , Gui_WeaponCheck12, 0
}
if(RegUseWC13 = 1)
{
GuiControl, , Gui_WeaponCheck13, 1
}
if(RegUseWC13 = 0)
{
GuiControl, , Gui_WeaponCheck13, 0
}
if(RegUseWC14 = 1)
{
GuiControl, , Gui_WeaponCheck14, 1
}
if(RegUseWC14 = 0)
{
GuiControl, , Gui_WeaponCheck14, 0
}
if(RegUseWC15 = 1)
{
GuiControl, , Gui_WeaponCheck15, 1
}
if(RegUseWC15 = 0)
{
GuiControl, , Gui_WeaponCheck15, 0
}
if(RegUseWC16 = 1)
{
GuiControl, , Gui_WeaponCheck16, 1
}
if(RegUseWC16 = 0)
{
GuiControl, , Gui_WeaponCheck16, 0
}
if(RegUseWC17 = 1)
{
GuiControl, , Gui_WeaponCheck17, 1
}
if(RegUseWC17 = 0)
{
GuiControl, , Gui_WeaponCheck17, 0
}
if(RegUseWC18 = 1)
{
GuiControl, , Gui_WeaponCheck18, 1
}
if(RegUseWC18 = 0)
{
GuiControl, , Gui_WeaponCheck18, 0
}
if(RegUseWC19 = 1)
{
GuiControl, , Gui_WeaponCheck19, 1
}
if(RegUseWC19 = 0)
{
GuiControl, , Gui_WeaponCheck19, 0
}
if(RegUseWC20 = 1)
{
GuiControl, , Gui_WeaponCheck20, 1
}
if(RegUseWC20 = 0)
{
GuiControl, , Gui_WeaponCheck20, 0
}
if(RegUseWC21= 1)
{
GuiControl, , Gui_WeaponCheck21, 1
}
if(RegUseWC21 = 0)
{
GuiControl, , Gui_WeaponCheck21, 0
}
if(RegUseWC22 = 1)
{
GuiControl, , Gui_WeaponCheck22, 1
}
if(RegUseWC22 = 0)
{
GuiControl, , Gui_WeaponCheck22, 0
}
if(RegUseWC23 = 1)
{
GuiControl, , Gui_WeaponCheck23, 1
}
if(RegUseWC23 = 0)
{
GuiControl, , Gui_WeaponCheck23, 0
}
if(RegUseWC24 = 1)
{
GuiControl, , Gui_WeaponCheck24, 1
}
if(RegUseWC24 = 0)
{
GuiControl, , Gui_WeaponCheck24, 0
}
if(RegUseWC25 = 1)
{
GuiControl, , Gui_WeaponCheck25, 1
}
if(RegUseWC25 = 0)
{
GuiControl, , Gui_WeaponCheck25, 0
}
if(RegUseWC26 = 1)
{
GuiControl, , Gui_WeaponCheck26, 1
}
if(RegUseWC26 = 0)
{
GuiControl, , Gui_WeaponCheck26, 0
}
if(RegUseWC27 = 1)
{
GuiControl, , Gui_WeaponCheck27, 1
}
if(RegUseWC27 = 0)
{
GuiControl, , Gui_WeaponCheck27, 0
}
if(RegUseWC28 = 1)
{
GuiControl, , Gui_WeaponCheck28, 1
}
if(RegUseWC28 = 0)
{
GuiControl, , Gui_WeaponCheck28, 0
}
if(RegUseWC29 = 1)
{
GuiControl, , Gui_WeaponCheck29, 1
}
if(RegUseWC29 = 0)
{
GuiControl, , Gui_WeaponCheck29, 0
}
if(RegUseWC30 = 1)
{
GuiControl, , Gui_WeaponCheck30, 1
}
if(RegUseWC30 = 0)
{
GuiControl, , Gui_WeaponCheck30, 0
}
if(RegUseWC31 = 1)
{
GuiControl, , Gui_WeaponCheck31, 1
}
if(RegUseWC31 = 0)
{
GuiControl, , Gui_WeaponCheck31, 0
}
if(RegUseWC32 = 1)
{
GuiControl, , Gui_WeaponCheck32, 1
}
if(RegUseWC32 = 0)
{
GuiControl, , Gui_WeaponCheck32, 0
}
if(RegUseWC33 = 1)
{
GuiControl, , Gui_WeaponCheck33, 1
}
if(RegUseWC33 = 0)
{
GuiControl, , Gui_WeaponCheck33, 0
}
if(RegUseWC34 = 1)
{
GuiControl, , Gui_WeaponCheck34, 1
}
if(RegUseWC34 = 0)
{
GuiControl, , Gui_WeaponCheck34, 0
}
if(RegUseWC35 = 1)
{
GuiControl, , Gui_WeaponCheck35, 1
}
if(RegUseWC35 = 0)
{
GuiControl, , Gui_WeaponCheck35, 0
}
if(RegUseWC36 = 1)
{
GuiControl, , Gui_WeaponCheck36, 1
}
if(RegUseWC36 = 0)
{
GuiControl, , Gui_WeaponCheck36, 0
}
if(RegUseWC37 = 1)
{
GuiControl, , Gui_WeaponCheck37, 1
}
if(RegUseWC37 = 0)
{
GuiControl, , Gui_WeaponCheck37, 0
}
if(RegUseWC38= 1)
{
GuiControl, , Gui_WeaponCheck38, 1
}
if(RegUseWC38 = 0)
{
GuiControl, , Gui_WeaponCheck38, 0
}
if(RegUseWC39 = 1)
{
GuiControl, , Gui_WeaponCheck39, 1
}
if(RegUseWC39 = 0)
{
GuiControl, , Gui_WeaponCheck39, 0
}
if(RegUseWC40 = 1)
{
GuiControl, , Gui_WeaponCheck40, 1
}
if(RegUseWC40 = 0)
{
GuiControl, , Gui_WeaponCheck40, 0
}
if(RegUseWC41 = 1)
{
GuiControl, , Gui_WeaponCheck41, 1
}
if(RegUseWC41 = 0)
{
GuiControl, , Gui_WeaponCheck41, 0
}
if(RegUseWC42 = 1)
{
GuiControl, , Gui_WeaponCheck42, 1
}
if(RegUseWC42 = 0)
{
GuiControl, , Gui_WeaponCheck42, 0
}
if(RegUseWC43 = 1)
{
GuiControl, , Gui_WeaponCheck43, 1
}
if(RegUseWC43 = 0)
{
GuiControl, , Gui_WeaponCheck43, 0
}
if(RegUseWC44 = 1)
{
GuiControl, , Gui_WeaponCheck44, 1
}
if(RegUseWC44 = 0)
{
GuiControl, , Gui_WeaponCheck44, 0
}
if(RegUseWC45 = 1)
{
GuiControl, , Gui_WeaponCheck45, 1
}
if(RegUseWC45 = 0)
{
GuiControl, , Gui_WeaponCheck45, 0
}
if(RegUseWC46 = 1)
{
GuiControl, , Gui_WeaponCheck46, 1
}
if(RegUseWC46 = 0)
{
GuiControl, , Gui_WeaponCheck46, 0
}
if(RegUseWC47 = 1)
{
GuiControl, , Gui_WeaponCheck47, 1
}
if(RegUseWC47 = 0)
{
GuiControl, , Gui_WeaponCheck47, 0
}
if(RegUseWC48 = 1)
{
GuiControl, , Gui_WeaponCheck48, 1
}
if(RegUseWC48 = 0)
{
GuiControl, , Gui_WeaponCheck48, 0
}
if(RegUseWC49 = 1)
{
GuiControl, , Gui_WeaponCheck49, 1
}
if(RegUseWC49 = 0)
{
GuiControl, , Gui_WeaponCheck49, 0
}
if(RegUseWC50 = 1)
{
GuiControl, , Gui_WeaponCheck50, 1
}
if(RegUseWC50 = 0)
{
GuiControl, , Gui_WeaponCheck50, 0
}
if(RegUseWC51 = 1)
{
GuiControl, , Gui_WeaponCheck51, 1
}
if(RegUseWC51 = 0)
{
GuiControl, , Gui_WeaponCheck51, 0
}
if(RegUseWC52 = 1)
{
GuiControl, , Gui_WeaponCheck52, 1
}
if(RegUseWC52 = 0)
{
GuiControl, , Gui_WeaponCheck52, 0
}
if(RegUseWC53 = 1)
{
GuiControl, , Gui_WeaponCheck53, 1
}
if(RegUseWC53 = 0)
{
GuiControl, , Gui_WeaponCheck53, 0
}
if(RegUseWC54 = 1)
{
GuiControl, , Gui_WeaponCheck54, 1
}
if(RegUseWC54 = 0)
{
GuiControl, , Gui_WeaponCheck54, 0
}
if(RegUseWC55 = 1)
{
GuiControl, , Gui_WeaponCheck55, 1
}
if(RegUseWC55 = 0)
{
GuiControl, , Gui_WeaponCheck55, 0
}
if(RegUseWC56 = 1)
{
GuiControl, , Gui_WeaponCheck56, 1
}
if(RegUseWC56 = 0)
{
GuiControl, , Gui_WeaponCheck56, 0
}
if(RegUseMC3 = 1)
{
GuiControl, , Gui_MagicCheck3, 1
}
if(RegUseMC3 = 0)
{
GuiControl, , Gui_MagicCheck3, 0
}
if(RegUseMC4 = 1)
{
GuiControl, , Gui_MagicCheck4, 1
}
if(RegUseMC4 = 0)
{
GuiControl, , Gui_MagicCheck4, 0
}
if(RegUseMC5 = 1)
{
GuiControl, , Gui_MagicCheck5, 1
}
if(RegUseMC5 = 0)
{
GuiControl, , Gui_MagicCheck5, 0
}
if(RegUseMC6 = 1)
{
GuiControl, , Gui_MagicCheck6, 1
}
if(RegUseMC6 = 0)
{
GuiControl, , Gui_MagicCheck6, 0
}
if(RegUseMC7 = 1)
{
GuiControl, , Gui_MagicCheck7, 1
}
if(RegUseMC7 = 0)
{
GuiControl, , Gui_MagicCheck7, 0
}
if(RegUseMC8 = 1)
{
GuiControl, , Gui_MagicCheck8, 1
}
if(RegUseMC8 = 0)
{
GuiControl, , Gui_MagicCheck8, 0
}
if(RegUseMC9 = 1)
{
GuiControl, , Gui_MagicCheck9, 1
}
if(RegUseMC9 = 0)
{
GuiControl, , Gui_MagicCheck9, 0
}
if(RegUseMC10 = 1)
{
GuiControl, , Gui_MagicCheck10, 1
}
if(RegUseMC10 = 0)
{
GuiControl, , Gui_MagicCheck10, 0
}
if(RegUseMC11 = 1)
{
GuiControl, , Gui_MagicCheck11, 1
}
if(RegUseMC11 = 0)
{
GuiControl, , Gui_MagicCheck11, 0
}
if(RegUseMC12 = 1)
{
GuiControl, , Gui_MagicCheck12, 1
}
if(RegUseMC12 = 0)
{
GuiControl, , Gui_MagicCheck12, 0
}
if(RegUseMC13 = 1)
{
GuiControl, , Gui_MagicCheck13, 1
}
if(RegUseMC13 = 0)
{
GuiControl, , Gui_MagicCheck13, 0
}
if(RegUseMC14 = 1)
{
GuiControl, , Gui_MagicCheck14, 1
}
if(RegUseMC14 = 0)
{
GuiControl, , Gui_MagicCheck14, 0
}
if(RegUseMC15 = 1)
{
GuiControl, , Gui_MagicCheck15, 1
}
if(RegUseMC15 = 0)
{
GuiControl, , Gui_MagicCheck15, 0
}
if(RegUseMC16 = 1)
{
GuiControl, , Gui_MagicCheck16, 1
}
if(RegUseMC16 = 0)
{
GuiControl, , Gui_MagicCheck16, 0
}
if(RegUseMC17 = 1)
{
GuiControl, , Gui_MagicCheck17, 1
}
if(RegUseMC17 = 0)
{
GuiControl, , Gui_MagicCheck17, 0
}
if(RegUseMC18 = 1)
{
GuiControl, , Gui_MagicCheck18, 1
}
if(RegUseMC18 = 0)
{
GuiControl, , Gui_MagicCheck18, 0
}
if(RegRelog = 1)
{
GuiControl, , Gui_relogerror, 1
}
if(RegRelog = 0)
{
GuiControl, , Gui_relogerror, 0
}
Gui, listview, 포프레스네소각
Loop, Read, %A_ScriptDir%\소각리스트.ini
{
LV_Add(A_Index, A_LoopReadLine)
}
Gui, Margin, 0, 0
Gui, -MinimizeBox -MaximizeBox +LastFound
Gui_ID := WinExist()
Gui, Submit, Nohide
GuiControl, choose, Gui_forceweapon, 선택
if(loady = 2)
{
Gosub, Start
}
if(VMRESET = 1 && loady != 2)
{
Gosub, START
}
return
생콩설정:
Gui, Submit, NoHide
if(포북생콩설정 = "FP 500이하")
{
포북생콩섭취 := 500
}
if(포북생콩설정 = "상시사용")
{
포북생콩섭취 := 1000
}
if(포남생콩설정 = "FP 500이하")
{
포남생콩섭취 := 500
}
if(포남생콩설정 = "상시사용")
{
포남생콩섭취 := 1000
}
return
가방수량설정:
Gui, Submit, NoHide
if(가방설정 = "줍줍끄기")
{
GuiControl,,Gui_jjOFF,1
jelan.write(0x0047B3EC, 0x4D, "Char", aOffsets*)
}
if(가방설정 = "제한없음")
{
GuiControl,,Gui_jjON,1
가방수량체크 := 50
jelan.write(0x0047B3EC, 0x02, "Char", aOffsets*)
}
if(가방설정 = "40개 초과시")
{
GuiControl,,Gui_jjON,1
가방수량체크 := 40
jelan.write(0x0047B3EC, 0x02, "Char", aOffsets*)
}
if(가방설정 = "45개 초과시")
{
GuiControl,,Gui_jjON,1
가방수량체크 := 45
jelan.write(0x0047B3EC, 0x02, "Char", aOffsets*)
}
return
SelectMuba:
Gui, Submit, Nohide
if(Gui_1Muba = 1)
{
GuiControl, Enable, Gui_Weapon1
GuiControl, Disable, Gui_Weapon2
GuiControl, Disable, Gui_Weapon3
if(Gui_HuntAuto = 1)
{
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Disable, Gui_LimitAbility0
GuiControl, Disable, Gui_LimitAbility2
GuiControl, Disable, Gui_LimitAbility3
}
GuiControl,, Gui_BasicWName0
GuiControl,, Gui_BasicWName1, %Gui_Weapon1%
GuiControl,, Gui_BasicWName2
GuiControl,, Gui_BasicWName3
}
if(Gui_2Muba = 1)
{
GuiControl, Enable, Gui_Weapon1
GuiControl, Enable, Gui_Weapon2
GuiControl, Disable, Gui_Weapon3
if(Gui_HuntAuto = 1)
{
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Disable, Gui_LimitAbility0
GuiControl, Disable, Gui_LimitAbility3
}
GuiControl,, Gui_BasicWName0
GuiControl,, Gui_BasicWName1, %Gui_Weapon1%
GuiControl,, Gui_BasicWName2, %Gui_Weapon2%
GuiControl,, Gui_BasicWName3
}
if(Gui_3Muba = 1)
{
GuiControl, Enable, Gui_Weapon1
GuiControl, Enable, Gui_Weapon2
GuiControl, Enable, Gui_Weapon3
if(Gui_HuntAuto = 1)
{
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Enable, Gui_LimitAbility3
GuiControl, Disable, Gui_LimitAbility0
}
GuiControl,, Gui_BasicWName0
GuiControl,, Gui_BasicWName1, %Gui_Weapon1%
GuiControl,, Gui_BasicWName2, %Gui_Weapon2%
GuiControl,, Gui_BasicWName3, %Gui_Weapon3%
}
if(Gui_2ButMuba = 1)
{
GuiControl, Enable, Gui_Weapon1
GuiControl, Disable, Gui_Weapon2
GuiControl, Disable, Gui_Weapon3
if(Gui_HuntAuto = 1)
{
GuiControl, Enable, Gui_LimitAbility0
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Disable, Gui_LimitAbility2
GuiControl, Disable, Gui_LimitAbility3
}
GuiControl,, Gui_BasicWName0, 격투
GuiControl,, Gui_BasicWName1, %Gui_Weapon1%
GuiControl,, Gui_BasicWName2
GuiControl,, Gui_BasicWName3
}
if(Gui_3ButMuba = 1)
{
GuiControl, Enable, Gui_Weapon1
GuiControl, Enable, Gui_Weapon2
GuiControl, Disable, Gui_Weapon3
if(Gui_HuntAuto = 1)
{
GuiControl, Enable, Gui_LimitAbility0
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Disable, Gui_LimitAbility3
}
GuiControl,, Gui_BasicWName0, 격투
GuiControl,, Gui_BasicWName1, %Gui_Weapon1%
GuiControl,, Gui_BasicWName2, %Gui_Weapon2%
GuiControl,, Gui_BasicWName3
}
if(Gui_4ButMuba = 1)
{
GuiControl, Enable, Gui_Weapon1
GuiControl, Enable, Gui_Weapon2
GuiControl, Enable, Gui_Weapon3
if(Gui_HuntAuto = 1)
{
GuiControl, Enable, Gui_LimitAbility0
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Enable, Gui_LimitAbility3
}
GuiControl,, Gui_BasicWName0, 격투
GuiControl,, Gui_BasicWName1, %Gui_Weapon1%
GuiControl,, Gui_BasicWName2, %Gui_Weapon2%
GuiControl,, Gui_BasicWName3, %Gui_Weapon3%
}
return
SelectAbility:
Gui, Submit, Nohide
if(Gui_1Muba = 1 or Gui_2ButMuba = 1)
{
if(Gui_1Muba = 1)
{
GuiControl,, Gui_WeaponName0
}
if(Gui_2ButMuba = 1)
{
GuiControl,, Gui_BasicWName0, 격투
}
GuiControl,, Gui_BasicWName1, %Gui_Weapon1%
GuiControl,, Gui_BasicWName2
GuiControl,, Gui_BasicWName3
}
if(Gui_2Muba = 1 or Gui_3ButMuba = 1)
{
if(Gui_2Muba = 1)
{
GuiControl,, Gui_WeaponName0
}
if(Gui_3ButMuba = 1)
{
GuiControl,, Gui_BasicWName0, 격투
}
GuiControl,, Gui_BasicWName1, %Gui_Weapon1%
GuiControl,, Gui_BasicWName2, %Gui_Weapon2%
GuiControl,, Gui_BasicWName3
}
if(Gui_3Muba = 1 or Gui_4ButMuba = 1)
{
if(Gui_3Muba = 1)
{
GuiControl,, Gui_BasicWName0
}
if(Gui_4ButMuba = 1)
{
GuiControl,, Gui_BasicWName0, 격투
}
GuiControl,, Gui_BasicWName1, %Gui_Weapon1%
GuiControl,, Gui_BasicWName2, %Gui_Weapon2%
GuiControl,, Gui_BasicWName3, %Gui_Weapon3%
}
return
SelectHuntPlace:
Gui, Submit, Nohide
if(Gui_HuntAuto = 1)
{
if(Gui_1Muba = 1)
{
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Disable, Gui_LimitAbility0
GuiControl, Disable, Gui_LimitAbility2
GuiControl, Disable, Gui_LimitAbility3
}
if(Gui_2Muba = 1)
{
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Disable, Gui_LimitAbility0
GuiControl, Disable, Gui_LimitAbility3
}
if(Gui_3Muba = 1)
{
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Enable, Gui_LimitAbility3
GuiControl, Disable, Gui_LimitAbility0
}
if(Gui_2ButMuba = 1)
{
GuiControl, Enable, Gui_LimitAbility0
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Disable, Gui_LimitAbility2
GuiControl, Disable, Gui_LimitAbility3
}
if(Gui_3ButMuba = 1)
{
GuiControl, Enable, Gui_LimitAbility0
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Disable, Gui_LimitAbility3
}
if(Gui_4ButMuba = 1)
{
GuiControl, Enable, Gui_LimitAbility0
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Enable, Gui_LimitAbility3
}
}
if(Gui_HuntAuto = 0)
{
GuiControl, Disable, Gui_LimitAbility0
GuiControl, Disable, Gui_LimitAbility1
GuiControl, Disable, Gui_LimitAbility2
GuiControl, Disable, Gui_LimitAbility3
}
if(Gui_HuntPobuk = 1)
{
GuiControl, Disable, Gui_Ent
GuiControl, Disable, Gui_Rockey
GuiControl, Disable, Gui_Mand
GuiControl, Disable, Gui_MobMagic
GuiControl, Disable, Gui_EntRockey
GuiControl, Disable, Gui_AllMobAND
GuiControl, Disable, Gui_AllMobOR
GuiControl, Disable, Gui_AllMobLimit
GuiControl, Disable, 포남설정
}
else
{
GuiControl, Enable, Gui_Ent
GuiControl, Enable, Gui_Rockey
GuiControl, Enable, Gui_Mand
GuiControl, Enable, Gui_MobMagic
GuiControl, Enable, Gui_EntRockey
GuiControl, Enable, Gui_AllMobAND
GuiControl, Enable, Gui_AllMobOR
GuiControl, Enable, Gui_AllMobLimit
GuiControl, Enable, 포남설정
}
return
CheckMob:
Gui, Submit, Nohide
if(Gui_Mand = 1 or Gui_AllMobAND = 1 or Gui_AllMobOR = 1 or Gui_MobMagic = 1)
{
GuiControl, , Gui_EvadeMand, 0
GuiControl, Disable, Gui_EvadeMand
}
if(Gui_AllMobAND = 1 or Gui_AllMobOR = 1 or Gui_MobMagic = 1)
{
GuiControl, Enable, Gui_AllMobLimit
}
if(Gui_AllMobAND = 0 and Gui_AllMobOR = 0 and Gui_MobMagic = 0)
{
GuiControl, Disabled, Gui_AllMobLimit
}
if(Gui_Ent = 1 or Gui_Rockey = 1 or Gui_EntRockey = 1)
{
GuiControl, Enable, Gui_EvadeMand
}
return
Check로그인:
Gui, Submit, Nohide
if( Gui_login = "인터넷" )
{
GuiControl, SHOW, Gui_NexonID
GuiControl, SHOW, Gui_NexonPassWord
GuiControl, HIDE, 넥슨x
GuiControl, HIDE, 넥슨y
GuiControl, HIDE, 좌표x
GuiControl, HIDE, 좌표y
좌표고정 := 0
}
if( Gui_login = "넥슨플러그" )
{
GuiControl, HIDE, Gui_NexonID
GuiControl, HIDE, Gui_NexonPassWord
GuiControl, SHOW, 넥슨x
GuiControl, SHOW, 넥슨y
GuiControl, SHOW, 좌표x
GuiControl, SHOW, 좌표y
좌표고정 := 1
}
return
CheckUseHPExit:
Gui, Submit, Nohide
GuiControl, % (Gui_CheckUseHPExit ? "enable":"disable"), Gui_HPExit
return
CheckUseHPPortal:
Gui, Submit, Nohide
GuiControl, % (Gui_CheckUseHPPortal ? "enable":"disable"), Gui_HPPortal
return
CheckUseHPLimited:
Gui, Submit, Nohide
GuiControl, % (Gui_CheckUseHPLimited ? "enable":"disable"), Gui_HPLimited
return
CheckUseMagic:
Gui, Submit, Nohide
GuiControl, % (Gui_CheckUseMagic ? "enable":"disable"), Gui_Magic
if(Gui_CheckUseMagic = 0)
{
GuiControl, Disabled, Gui_CHP
GuiControl, Disabled, Gui_MagicNStack
}
else if(Gui_CheckUseMagic = 1)
{
GuiControl, Enabled, Gui_CHP
GuiControl, Enabled, Gui_MagicNStack
}
return
CheckW1:
Gui, Submit, Nohide
if(Gui_WeaponCheck1 = 0)
{
GuiControl, , Gui_WeaponValue1
}
return
CheckW2:
Gui, Submit, Nohide
if(Gui_WeaponCheck2 = 0)
{
GuiControl, , Gui_WeaponValue2
}
return
CheckW3:
Gui, Submit, Nohide
if(Gui_WeaponCheck3 = 0)
{
GuiControl, , Gui_WeaponValue3
}
return
CheckW4:
Gui, Submit, Nohide
if(Gui_WeaponCheck4 = 0)
{
GuiControl, , Gui_WeaponValue4
}
return
CheckW5:
Gui, Submit, Nohide
if(Gui_WeaponCheck5 = 0)
{
GuiControl, , Gui_WeaponValue5
}
return
CheckW6:
Gui, Submit, Nohide
if(Gui_WeaponCheck6 = 0)
{
GuiControl, , Gui_WeaponValue6
}
return
CheckW7:
Gui, Submit, Nohide
if(Gui_WeaponCheck7 = 0)
{
GuiControl, , Gui_WeaponValue7
}
return
CheckW8:
Gui, Submit, Nohide
if(Gui_WeaponCheck8 = 0)
{
GuiControl, , Gui_WeaponValue8
}
return
CheckW9:
Gui, Submit, Nohide
if(Gui_WeaponCheck1 = 9)
{
GuiControl, , Gui_WeaponValue9
}
return
CheckW10:
Gui, Submit, Nohide
if(Gui_WeaponCheck10 = 0)
{
GuiControl, , Gui_WeaponValue10
}
return
CheckW11:
Gui, Submit, Nohide
if(Gui_WeaponCheck11 = 0)
{
GuiControl, , Gui_WeaponValue11
}
return
CheckW12:
Gui, Submit, Nohide
if(Gui_WeaponCheck12 = 0)
{
GuiControl, , Gui_WeaponValue12
}
return
CheckW13:
Gui, Submit, Nohide
if(Gui_WeaponCheck13 = 0)
{
GuiControl, , Gui_WeaponValue13
}
return
CheckW14:
Gui, Submit, Nohide
if(Gui_WeaponCheck14 = 0)
{
GuiControl, , Gui_WeaponValue14
}
return
CheckW15:
Gui, Submit, Nohide
if(Gui_WeaponCheck15 = 0)
{
GuiControl, , Gui_WeaponValue15
}
return
CheckW16:
Gui, Submit, Nohide
if(Gui_WeaponCheck16 = 0)
{
GuiControl, , Gui_WeaponValue16
}
return
CheckW17:
Gui, Submit, Nohide
if(Gui_WeaponCheck17 = 0)
{
GuiControl, , Gui_WeaponValue17
}
return
CheckW18:
Gui, Submit, Nohide
if(Gui_WeaponCheck18 = 0)
{
GuiControl, , Gui_WeaponValue18
}
return
CheckW19:
Gui, Submit, Nohide
if(Gui_WeaponCheck19 = 0)
{
GuiControl, , Gui_WeaponValue19
}
return
CheckW20:
Gui, Submit, Nohide
if(Gui_WeaponCheck20 = 0)
{
GuiControl, , Gui_WeaponValue20
}
return
CheckW21:
GUI, Submit, Nohide
if(Gui_WeaponCheck21 = 0)
{
GUICONTROL, , Gui_WeaponValue21
}
return
CheckW22:
GUI, Submit, Nohide
if(Gui_WeaponCheck22 = 0)
{
GUICONTROL, , Gui_WeaponValue22
}
return
CheckW23:
GUI, Submit, Nohide
if(Gui_WeaponCheck23 = 0)
{
GUICONTROL, , Gui_WeaponValue23
}
return
CheckW24:
GUI, Submit, Nohide
if(Gui_WeaponCheck24 = 0)
{
GUICONTROL, , Gui_WeaponValue24
}
return
CheckW25:
GUI, Submit, Nohide
if(Gui_WeaponCheck25 = 0)
{
GUICONTROL, , Gui_WeaponValue25
}
return
CheckW26:
GUI, Submit, Nohide
if(Gui_WeaponCheck26 = 0)
{
GUICONTROL, , Gui_WeaponValue26
}
return
CheckW27:
GUI, Submit, Nohide
if(Gui_WeaponCheck27 = 0)
{
GUICONTROL, , Gui_WeaponValue27
}
return
CheckW28:
GUI, Submit, Nohide
if(Gui_WeaponCheck28 = 0)
{
GUICONTROL, , Gui_WeaponValue28
}
return
CheckW29:
GUI, Submit, Nohide
if(Gui_WeaponCheck29 = 0)
{
GUICONTROL, , Gui_WeaponValue29
}
return
CheckW30:
GUI, Submit, Nohide
if(Gui_WeaponCheck30 = 0)
{
GUICONTROL, , Gui_WeaponValue30
}
return
CheckW31:
GUI, Submit, Nohide
if(Gui_WeaponCheck31 = 0)
{
GUICONTROL, , Gui_WeaponValue31
}
return
CheckW32:
GUI, Submit, Nohide
if(Gui_WeaponCheck32 = 0)
{
GUICONTROL, , Gui_WeaponValue32
}
return
CheckW33:
GUI, Submit, Nohide
if(Gui_WeaponCheck33 = 0)
{
GUICONTROL, , Gui_WeaponValue33
}
return
CheckW34:
GUI, Submit, Nohide
if(Gui_WeaponCheck34 = 0)
{
GUICONTROL, , Gui_WeaponValue34
}
return
CheckW35:
GUI, Submit, Nohide
if(Gui_WeaponCheck35 = 0)
{
GUICONTROL, , Gui_WeaponValue35
}
return
CheckW36:
GUI, Submit, Nohide
if(Gui_WeaponCheck36 = 0)
{
GUICONTROL, , Gui_WeaponValue36
}
return
CheckW37:
GUI, Submit, Nohide
if(Gui_WeaponCheck37 = 0)
{
GUICONTROL, , Gui_WeaponValue37
}
return
CheckW38:
GUI, Submit, Nohide
if(Gui_WeaponCheck38 = 0)
{
GUICONTROL, , Gui_WeaponValue38
}
return
CheckW39:
GUI, Submit, Nohide
if(Gui_WeaponCheck39 = 0)
{
GUICONTROL, , Gui_WeaponValue39
}
return
CheckW40:
GUI, Submit, Nohide
if(Gui_WeaponCheck40 = 0)
{
GUICONTROL, , Gui_WeaponValue40
}
return
CheckW41:
GUI, Submit, Nohide
if(Gui_WeaponCheck41 = 0)
{
GUICONTROL, , Gui_WeaponValue41
}
return
CheckW42:
GUI, Submit, Nohide
if(Gui_WeaponCheck42 = 0)
{
GUICONTROL, , Gui_WeaponValue42
}
return
CheckW43:
GUI, Submit, Nohide
if(Gui_WeaponCheck43 = 0)
{
GUICONTROL, , Gui_WeaponValue43
}
return
CheckW44:
GUI, Submit, Nohide
if(Gui_WeaponCheck44 = 0)
{
GUICONTROL, , Gui_WeaponValue44
}
return
CheckW45:
GUI, Submit, Nohide
if(Gui_WeaponCheck45 = 0)
{
GUICONTROL, , Gui_WeaponValue45
}
return
CheckW46:
GUI, Submit, Nohide
if(Gui_WeaponCheck46 = 0)
{
GUICONTROL, , Gui_WeaponValue46
}
return
CheckW47:
GUI, Submit, Nohide
if(Gui_WeaponCheck47 = 0)
{
GUICONTROL, , Gui_WeaponValue47
}
return
CheckW48:
GUI, Submit, Nohide
if(Gui_WeaponCheck48 = 0)
{
GUICONTROL, , Gui_WeaponValue48
}
return
CheckW49:
GUI, Submit, Nohide
if(Gui_WeaponCheck49 = 0)
{
GUICONTROL, , Gui_WeaponValue49
}
return
CheckW50:
GUI, Submit, Nohide
if(Gui_WeaponCheck50 = 0)
{
GUICONTROL, , Gui_WeaponValue50
}
return
CheckW51:
GUI, Submit, Nohide
if(Gui_WeaponCheck51 = 0)
{
GUICONTROL, , Gui_WeaponValue51
}
return
CheckW52:
GUI, Submit, Nohide
if(Gui_WeaponCheck52 = 0)
{
GUICONTROL, , Gui_WeaponValue52
}
return
CheckW53:
GUI, Submit, Nohide
if(Gui_WeaponCheck53 = 0)
{
GUICONTROL, , Gui_WeaponValue53
}
return
CheckW54:
GUI, Submit, Nohide
if(Gui_WeaponCheck54 = 0)
{
GUICONTROL, , Gui_WeaponValue54
}
return
CheckW55:
GUI, Submit, Nohide
if(Gui_WeaponCheck55 = 0)
{
GUICONTROL, , Gui_WeaponValue55
}
return
CheckW56:
GUI, Submit, Nohide
if(Gui_WeaponCheck56 = 0)
{
GUICONTROL, , Gui_WeaponValue56
}
return
CheckM3:
Gui, Submit, Nohide
if(Gui_MagicCheck3 = 0)
{
GuiControl, , Gui_MagicValue3
}
return
CheckM4:
Gui, Submit, Nohide
if(Gui_MagicCheck4 = 0)
{
GuiControl, , Gui_MagicValue4
}
return
CheckM5:
Gui, Submit, Nohide
if(Gui_MagicCheck5 = 0)
{
GuiControl, , Gui_MagicValue5
}
return
CheckM6:
Gui, Submit, Nohide
if(Gui_MagicCheck6 = 0)
{
GuiControl, , Gui_MagicValue6
}
return
CheckM7:
if(Gui_MagicCheck7 = 0)
{
Gui, Submit, Nohide
GuiControl, , Gui_MagicValue7
}
return
CheckM8:
Gui, Submit, Nohide
if(Gui_MagicCheck8 = 0)
{
GuiControl, , Gui_MagicValue8
}
return
CheckM9:
Gui, Submit, Nohide
if(Gui_MagicCheck9 = 0)
{
GuiControl, , Gui_MagicValue9
}
return
CheckM10:
Gui, Submit, Nohide
if(Gui_MagicCheck10 = 0)
{
GuiControl, , Gui_MagicValue10
}
return
CheckM11:
Gui, Submit, Nohide
if(Gui_MagicCheck11 = 0)
{
GuiControl, , Gui_MagicValue11
}
return
CheckM12:
Gui, Submit, Nohide
if(Gui_MagicCheck12 = 0)
{
GuiControl, , Gui_MagicValue12
}
return
CheckM13:
Gui, Submit, Nohide
if(Gui_MagicCheck13 = 0)
{
GuiControl, , Gui_MagicValue13
}
return
CheckM14:
Gui, Submit, Nohide
if(Gui_MagicCheck14 = 0)
{
GuiControl, , Gui_MagicValue14
}
return
CheckM15:
Gui, Submit, Nohide
if(Gui_MagicCheck15 = 0)
{
GuiControl, , Gui_MagicValue15
}
return
CheckM16:
Gui, Submit, Nohide
if(Gui_MagicCheck16 = 0)
{
GuiControl, , Gui_MagicValue16
}
return
CheckM17:
Gui, Submit, Nohide
if(Gui_MagicCheck17 = 0)
{
GuiControl, , Gui_MagicValue17
}
return
CheckM18:
Gui, Submit, Nohide
if(Gui_MagicCheck18 = 0)
{
GuiControl, , Gui_MagicValue18
}
return
resetting:
Gui, Submit, Nohide
Step = 0
gosub, 재실행
return
forcegrade:
Gui, Submit, Nohide
step = 700
return
Start:
Gui, Submit, Nohide
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, loady, 0
RegRead, IEVersion, HKEY_LOCAL_MACHINE, SOFTWARE\Microsoft\Internet Explorer\Version Vector, IE
RegRead, Reg실행시간, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 실행시간
if( Gui_PartyON = 1 or Gui_CheckUseParty = 1)
{
if( 랜덤차원 = 1 )
{
MsgBox,48, 차원설정,
(
차원설정이 잘못되었습니다.
<파티사용시>
랜덤차원으로 할 수 없습니다.
파티캐릭이 있는 차원으로 바꿔주세요.
)
return
}
}
if(IEVersion < 9)
{
SB_SetText("인터넷 버전이 낮습니다.")
return
}
internet := ConnectedToInternet()
if(internet = 0)
{
SB_SetText("인터넷 연결을 확인 해 주세요.")
return
}
if(Gui_Login = "")
{
SB_SetText("로그인방식을 선택 해 주세요.")
return
}
if(Gui_Server = "")
{
SB_SetText("서버를 선택 해 주세요.")
return
}
if(Gui_CharNumber = "")
{
SB_SetText("캐릭터번호를 선택 해 주세요.")
return
}
if(Gui_CheckUseHPExit = 1)
{
if(Gui_HPExit = "")
{
SB_SetText("종료 체력을 정확히 입력 해 주세요.")
return
}
if(Gui_HPExit = 0)
{
SB_SetText("종료 체력은 1 이상이여야 합니다.")
return
}
}
if(Gui_CheckUseHPPortal = 1)
{
if(Gui_HPPortal = "")
{
SB_SetText("차원이동 체력을 정확히 입력 해 주세요.")
return
}
if(Gui_HPPortal = 0)
{
SB_SetText("차원이동 체력은 1 이상이여야 합니다.")
return
}
}
if(Gui_CheckUseHPExit = 1 and Gui_CheckUseHPPortal)
{
if(Gui_HPExit > Gui_HPPortal)
{
SB_SetText("차원이동 체력 설정은 종료 체력보다 높아야 합니다.")
return
}
}
if(Gui_AllMobAND = 1 or Gui_AllMobOR = 1)
{
if(Gui_AllMobLimit = "")
{
SB_SetText("전체 몬스터 선택 어빌 제한을 올바르게 설정 해 주세요.")
return
}
if(Gui_AllMobLimit > 9500)
{
SB_SetText("전체 몬스터 선택 어빌 제한은 9500을 넘길 수 없습니다.")
return
}
}
if(Gui_1Muba = 1 or Gui_2ButMuba = 1)
{
if(Gui_Weapon1 = "")
{
SB_SetText("무기 어빌리티를 올바르게 설정 해 주세요.")
return
}
}
if(Gui_2Muba = 1 or Gui_3ButMuba = 1)
{
if(Gui_Weapon1 = "" or Gui_Weapon2 = "")
{
SB_SetText("무기 어빌리티를 올바르게 설정 해 주세요.")
return
}
if(Gui_Weapon1 = Gui_Weapon2)
{
SB_SetText("같은 어빌리티를 선택 할 수 없습니다.")
return
}
}
if(Gui_3Muba = 1 or Gui_4ButMuba = 1)
{
if(Gui_Weapon1 = "" or Gui_Weapon2 = "" or Gui_Weapon3 = "")
{
SB_SetText("무기 어빌리티를 올바르게 설정 해 주세요.")
return
}
if(Gui_Weapon1 = Gui_Weapon2 or Gui_Weapon2 = Gui_Weapon3 or Gui_Weapon3 = Gui_Weapon1)
{
SB_SetText("같은 어빌리티를 선택 할 수 없습니다.")
return
}
}
if(Gui_HuntAuto = 1)
{
if(Gui_1Muba = 1)
{
if(Gui_LimitAbility1 > 9500)
{
SB_SetText("어빌리티 제한은 9500을 넘길 수 없습니다.")
return
}
if(Gui_LimitAbility1 = "")
{
SB_SetText("어빌리티 제한을 올바르게 설정 해 주세요.")
return
}
}
if(Gui_2Muba = 1)
{
if(Gui_LimitAbility1 > 9500 or Gui_LimitAbility2 > 9500)
{
SB_SetText("어빌리티 제한은 9500을 넘길 수 없습니다.")
return
}
if(Gui_LimitAbility1 = "" or Gui_LimitAbility2 = "")
{
SB_SetText("어빌리티 제한을 올바르게 설정 해 주세요.")
return
}
}
if(Gui_3Muba = 1)
{
if(Gui_LimitAbility1 > 9500 or Gui_LimitAbility2 > 9500 or Gui_LimitAbility3 > 9500)
{
SB_SetText("어빌리티 제한은 9500을 넘길 수 없습니다.")
return
}
if(Gui_LimitAbility1 = "" or Gui_LimitAbility2 = "" or Gui_LimitAbility3 = "")
{
SB_SetText("어빌리티 제한을 올바르게 설정 해 주세요.")
return
}
}
if(Gui_2ButMuba = 1)
{
if(Gui_LimitAbility0 > 9500 or Gui_LimitAbility1 > 9500)
{
SB_SetText("어빌리티 제한은 9500을 넘길 수 없습니다.")
return
}
if(Gui_LimitAbility1 = "" or Gui_LimitAbility1 = "")
{
SB_SetText("어빌리티 제한을 올바르게 설정 해 주세요.")
return
}
}
if(Gui_3ButMuba = 1)
{
if(Gui_LimitAbility0 > 9500 or Gui_LimitAbility1 > 9500 or Gui_LimitAbility2 > 9500)
{
SB_SetText("어빌리티 제한은 9500을 넘길 수 없습니다.")
return
}
if(Gui_LimitAbility0 = "" or Gui_LimitAbility1 = "" or Gui_LimitAbility2 = "")
{
SB_SetText("어빌리티 제한을 올바르게 설정 해 주세요.")
return
}
}
if(Gui_4ButMuba = 1)
{
if(Gui_LimitAbility0 > 9500 or Gui_LimitAbility1 > 9500 or Gui_LimitAbility2 > 9500 or Gui_LimitAbility3 > 9500)
{
SB_SetText("어빌리티 제한은 9500을 넘길 수 없습니다.")
return
}
if(Gui_LimitAbility0 = "" or Gui_LimitAbility1 = "" or Gui_LimitAbility2 = "" or Gui_LimitAbility3 = "")
{
SB_SetText("어빌리티 제한을 올바르게 설정 해 주세요.")
return
}
}
}
if(monitor > 1500)
{
WinMove, ahk_id %Gui_ID%,,801,0
실행창위치 = 1
}
else if(monitor > 1100)
{
WinMove, ahk_id %Gui_ID%,,368,120
실행창위치 = 2
}
else if(monitor < 1100)
{
WinMove, ahk_id %Gui_ID%, , 670, 0
실행창위치 = 3
}
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, VMRE, 1
GuiControl,Enable,Gui_StopButton
GuiControl,Enable,Gui_Resetting
NexonID := Gui_NexonID
NexonPassword := Gui_NexonPassword
GuiControl, Disable, Gui_NexonID
GuiControl, Disable, Gui_NexonPassWord
GuiControl, Disable, Gui_Server
GuiControl, Disable, Gui_Login
GuiControl, Disable, Gui_CharNumber
GuiControl, Disable, Gui_CheckUseHPExit
GuiControl, Disable, Gui_CheckUseHPPortal
GuiControl, Disable, Gui_CheckUseHPLimited
GuiControl, Disable, Gui_HPExit
GuiControl, Disable, Gui_HPPortal
GuiControl, Disable, Gui_HPLimited
GuiControl, Disable, Gui_CHP
GuiControl, Disable, Gui_Weapon1
GuiControl, Disable, Gui_Weapon2
GuiControl, Disable, Gui_Weapon3
GuiControl, Disable, Gui_jjON
GuiControl, Disable, Gui_jjOFF
GuiControl, Disable, Gui_LimitAbility0
GuiControl, Disable, Gui_LimitAbility1
GuiControl, Disable, Gui_LimitAbility2
GuiControl, Disable, Gui_LimitAbility3
GuiControl, Disable, Gui_StartButton
GuiControl, Disable, Gui_WindowSettingButton
GuiControl, Disable, Gui_Agree
GuiControl, Disable, Gui_1Muba
GuiControl, Disable, Gui_2Muba
GuiControl, Disable, Gui_3Muba
GuiControl, Disable, Gui_2ButMuba
GuiControl, Disable, Gui_3ButMuba
GuiControl, Disable, Gui_4ButMuba
SB_SetText("로그인 중")
몸찌체크 := 0
실행초기화 := 0
Step = 0
FirstCheck = 1
MagicN = 3
Entrance = 0
ipmak = 0
callid = 1
RCC = 0
ProgramStartTime = 0
파라스타이머시작 = 0
if( Reg실행시간 = 0 )
{
지금시각 := A_Now
FormatTime, 지금시각_R, %지금시각%, yyyy 년 MM월 dd일 HH:mm
GuiControl, , 실행시간, %지금시각_R% %아이콘%
}else
{
지금시각_R := Reg실행시간
}
if(Regloady = 1)
{
ProgramStartTime := RPST
}
GuiControl, Choose, Gui_Tab, 상태창
loady = 1
SetTimer, Hunt, 50
SetTimer, AttackCheck, 50
SetTimer, 타겟팅, 100
SetTimer, RL, 18000000
시작탭사이즈 := 1
return
RL:

Gui, Submit, Nohide
Run, *RunAs %A_ScriptDir%\CashMemory.exe
Sleep,300
loady = 2
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
}
IfWinExist,ahk_exe MRMSPH.exe
{
WinKill, ahk_exe MRMSPH.exe
}
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, VMRE, 0
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 실행시간, %지금시각_R%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 서버팅김, %서버팅김%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 파라스감지,%파라스감지%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 수천감지,%수천감지%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 파라스방해감지,%파라스방해감지%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P1, %Name1%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P2, %Name2%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P3, %Name3%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P4, %Name4%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P5, %Name5%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P6, %Name6%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N1, %Gui_P1CharNumber%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N2, %Gui_P2CharNumber%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N3, %Gui_P3CharNumber%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N4, %Gui_P4CharNumber%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N5, %Gui_P5CharNumber%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N6, %Gui_P6CharNumber%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 업데이트체크, %업데이트체크%
if(Gui_CheckUseHPExit = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPExit, 1
}
if(Gui_CheckUseHPExit = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPExit, 0
}
if(Gui_CheckUseMagic = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMagic, 1
}
if(Gui_CheckUseMagic = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMagic, 0
}
if(Gui_CheckUseParty = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseParty, 1
}
if(Gui_CheckUseParty = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseParty, 0
}
if(Gui_CheckUseHPPortal = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPPortal, 1
}
if(Gui_CheckUseHPPortal = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPPortal, 0
}
if(Gui_CheckUseHPLimited = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPLimited, 1
}
if(Gui_CheckUseHPLimited = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPLimited, 0
}
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, HPExit, %Gui_HPExit%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, HPPortal, %Gui_HPPortal%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, HPLimited, %Gui_HPLimited%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, CrittHP, %Gui_CHP%
if(Gui_1Muba = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba, 1
}
if(Gui_2Muba = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba, 2
}
if(Gui_3Muba = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba, 3
}
if(Gui_2ButMuba = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba, 4
}
if(Gui_3ButMuba = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba, 5
}
if(Gui_4ButMuba = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba, 6
}
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Weapon1, %Gui_Weapon1%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Weapon2, %Gui_Weapon2%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Weapon3, %Gui_Weapon3%
if(Gui_EvadeMand = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Evade, 1
}
if(Gui_EvadeMand = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Evade, 0
}
if(Gui_KON = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, KONOFF, 1
}
if(Gui_KOFF = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, KONOFF, 2
}
if(방어구방지ON = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 방어구방지ONOFF, 1
}
if(방어구방지OFF = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 방어구방지ONOFF, 2
}
if(Gui_jjON = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, jjONOFF, 1
}
if(Gui_jjOFF = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, jjONOFF, 2
}
if(Gui_MoveLoute1 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Direct, 1
}
if(Gui_MoveLoute2 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Direct, 2
}
if(Gui_MoveLoute3 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Direct, 3
}
if(Gui_MoveLoute4 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Direct, 4
}
if(Gui_Ent = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 1
}
if(Gui_Rockey= 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 2
}
if(Gui_EntRockey= 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 3
}
if(Gui_Mand = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 4
}
if(Gui_AllMobAND = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 5
}
if(Gui_AllMobOR = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 6
}
if(Gui_MobMagic = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 7
}
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, AllMobLimit, %Gui_AllMobLimit%
if(Gui_HuntAuto = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Place, 1
}
if(Gui_HuntPonam = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Place, 2
}
if(Gui_HuntPobuk = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Place, 3
}
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Limit0, %Gui_LimitAbility0%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Limit1, %Gui_LimitAbility1%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Limit2, %Gui_LimitAbility2%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Limit3, %Gui_LimitAbility3%
if(Gui_PartyOn = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Party, 1
}
if(Gui_PartyOff = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Party, 2
}
if(Gui_Grade = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Grade, 1
}
if(Gui_Grade = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Grade, 0
}
if(알파차원 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포탈, 0
}
if(베타차원 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포탈, 1
}
if(감마차원 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포탈, 2
}
if(랜덤차원 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포탈, 3
}
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, CharNumber, %Gui_CharNumber%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MNS, %Gui_MagicNStack%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Server, %Gui_Server%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Login, %Gui_Login%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, ID, %Gui_NexonID%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Pass, %Gui_NexonPassWord%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA1, %Gui_ActiveA1%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA2, %Gui_ActiveA2%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA3, %Gui_ActiveA3%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA4, %Gui_ActiveA4%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA5, %Gui_ActiveA5%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA6, %Gui_ActiveA6%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA7, %Gui_ActiveA7%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA8, %Gui_ActiveA8%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseTAA1, %Gui_TActiveA1%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseTAA2, %Gui_TActiveA2%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseTAA3, %Gui_TActiveA3%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseTAAA1, %Gui_TAActiveA1%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포북생콩, %포북생콩설정%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포남생콩, %포남생콩설정%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 가방, %가방설정%
if(4번사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯4번, 1
}
if(4번사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯4번, 0
}
if(5번사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯5번, 1
}
if(5번사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯5번, 0
}
if(6번사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯6번, 1
}
if(6번사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯6번, 0
}
if(7번사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯7번, 1
}
if(7번사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯7번, 0
}
if(현혹사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 현혹체크, 1
}
if(현혹사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 현혹체크, 0
}
if(폭검사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 폭검체크, 1
}
if(폭검사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 폭검체크, 0
}
if(독침사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 독침체크, 1
}
if(독침사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 독침체크, 0
}
if(무기공격사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 무기공격체크, 1
}
if(무기공격사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 무기공격체크, 0
}
if(대화사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 대화체크, 1
}
if(대화사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 대화체크, 0
}
if(명상사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 명상체크, 1
}
if(명상사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 명상체크, 0
}
if(더블어택사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 더블어택체크, 1
}
if(더블어택사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 더블어택체크, 0
}
if(체력향상사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 체력향상체크, 1
}
if(체력향상사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 체력향상체크, 0
}
if(집중사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 집중체크, 1
}
if(집중사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 집중체크, 0
}
if(회피사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 회피체크, 1
}
if(회피사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 회피체크, 0
}
if(몸통지르기사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 몸통지르기체크, 1
}
if(몸통지르기사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 몸통지르기체크, 0
}
if(리무브아머사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 리무브아머체크, 1
}
if(리무브아머사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 리무브아머체크, 0
}
if(민첩향상사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 민첩향상체크, 1
}
if(민첩향상사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 민첩향상체크, 0
}
if(활방어사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 활방어체크, 1
}
if(활방어사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 활방어체크, 0
}
if(마력향상사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 마력향상체크, 1
}
if(마력향상사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 마력향상체크, 0
}
if(마력방어사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 마력방어체크, 1
}
if(마력방어사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 마력방어체크, 0
}
if(Gui_WeaponCheck1 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC1, 1
}
if(Gui_WeaponCheck1 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC1, 0
}
if(Gui_WeaponCheck2 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC2, 1
}
if(Gui_WeaponCheck2 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC2, 0
}
if(Gui_WeaponCheck3 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC3, 1
}
if(Gui_WeaponCheck3 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC3, 0
}
if(Gui_WeaponCheck4 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC4, 1
}
if(Gui_WeaponCheck4 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC4, 0
}
if(Gui_WeaponCheck5 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC5, 1
}
if(Gui_WeaponCheck5 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC5, 0
}
if(Gui_WeaponCheck6 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC6, 1
}
if(Gui_WeaponCheck6 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC6, 0
}
if(Gui_WeaponCheck7 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC7, 1
}
if(Gui_WeaponCheck7 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC7, 0
}
if(Gui_WeaponCheck8 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC8, 1
}
if(Gui_WeaponCheck8 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC8, 0
}
if(Gui_WeaponCheck9 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC9, 1
}
if(Gui_WeaponCheck9 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC9, 0
}
if(Gui_WeaponCheck10 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC10, 1
}
if(Gui_WeaponCheck10 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC10, 0
}
if(Gui_WeaponCheck11 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC11, 1
}
if(Gui_WeaponCheck11 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC11, 0
}
if(Gui_WeaponCheck12 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC12, 1
}
if(Gui_WeaponCheck12 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC12, 0
}
if(Gui_WeaponCheck13 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC13, 1
}
if(Gui_WeaponCheck13 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC13, 0
}
if(Gui_WeaponCheck14 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC14, 1
}
if(Gui_WeaponCheck14 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC14, 0
}
if(Gui_WeaponCheck15 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC15, 1
}
if(Gui_WeaponCheck15 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC15, 0
}
if(Gui_WeaponCheck16 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC16, 1
}
if(Gui_WeaponCheck16 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC16, 0
}
if(Gui_WeaponCheck17 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC17, 1
}
if(Gui_WeaponCheck17 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC17, 0
}
if(Gui_WeaponCheck18 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC18, 1
}
if(Gui_WeaponCheck18 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC18, 0
}
if(Gui_WeaponCheck19 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC19, 1
}
if(Gui_WeaponCheck19 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC19, 0
}
if(Gui_WeaponCheck20 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC20, 1
}
if(Gui_WeaponCheck20 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC20, 0
}
if(Gui_WeaponCheck21 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC21, 1
}
if(Gui_WeaponCheck21 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC21, 0
}
if(Gui_WeaponCheck22 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC22, 1
}
if(Gui_WeaponCheck22 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC22, 0
}
if(Gui_WeaponCheck23 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC23, 1
}
if(Gui_WeaponCheck23 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC23, 0
}
if(Gui_WeaponCheck24 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC24, 1
}
if(Gui_WeaponCheck24 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC24, 0
}
if(Gui_WeaponCheck25 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC25, 1
}
if(Gui_WeaponCheck25 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC25, 0
}
if(Gui_WeaponCheck26 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC26, 1
}
if(Gui_WeaponCheck26 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC26, 0
}
if(Gui_WeaponCheck27 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC27, 1
}
if(Gui_WeaponCheck27 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC27, 0
}
if(Gui_WeaponCheck28 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC28, 1
}
if(Gui_WeaponCheck28 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC28, 0
}
if(Gui_WeaponCheck29 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC29, 1
}
if(Gui_WeaponCheck29 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC29, 0
}
if(Gui_WeaponCheck30 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC30, 1
}
if(Gui_WeaponCheck30 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC30, 0
}
if(Gui_WeaponCheck31 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC31, 1
}
if(Gui_WeaponCheck31 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC31, 0
}
if(Gui_WeaponCheck32 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC32, 1
}
if(Gui_WeaponCheck32 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC32, 0
}
if(Gui_WeaponCheck33 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC33, 1
}
if(Gui_WeaponCheck33 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC33, 0
}
if(Gui_WeaponCheck34 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC34, 1
}
if(Gui_WeaponCheck34 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC34, 0
}
if(Gui_WeaponCheck35 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC35, 1
}
if(Gui_WeaponCheck35 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC35, 0
}
if(Gui_WeaponCheck36 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC36, 1
}
if(Gui_WeaponCheck36 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC36, 0
}
if(Gui_WeaponCheck37 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC37, 1
}
if(Gui_WeaponCheck37 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC37, 0
}
if(Gui_WeaponCheck38 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC38, 1
}
if(Gui_WeaponCheck38 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC38, 0
}
if(Gui_WeaponCheck39 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC39, 1
}
if(Gui_WeaponCheck39 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC39, 0
}
if(Gui_WeaponCheck40 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC40, 1
}
if(Gui_WeaponCheck40 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC40, 0
}
if(Gui_WeaponCheck41 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC41, 1
}
if(Gui_WeaponCheck41 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC41, 0
}
if(Gui_WeaponCheck42 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC42, 1
}
if(Gui_WeaponCheck42 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC42, 0
}
if(Gui_WeaponCheck43 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC43, 1
}
if(Gui_WeaponCheck43 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC43, 0
}
if(Gui_WeaponCheck44 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC44, 1
}
if(Gui_WeaponCheck44 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC44, 0
}
if(Gui_WeaponCheck45 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC45, 1
}
if(Gui_WeaponCheck45 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC45, 0
}
if(Gui_WeaponCheck46 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC46, 1
}
if(Gui_WeaponCheck46 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC46, 0
}
if(Gui_WeaponCheck47 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC47, 1
}
if(Gui_WeaponCheck47 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC47, 0
}
if(Gui_WeaponCheck48 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC48, 1
}
if(Gui_WeaponCheck48 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC48, 0
}
if(Gui_WeaponCheck49 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC49, 1
}
if(Gui_WeaponCheck49 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC49, 0
}
if(Gui_WeaponCheck50 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC50, 1
}
if(Gui_WeaponCheck50 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC50, 0
}
if(Gui_WeaponCheck51 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC51, 1
}
if(Gui_WeaponCheck51 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC51, 0
}
if(Gui_WeaponCheck52 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC52, 1
}
if(Gui_WeaponCheck52 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC52, 0
}
if(Gui_WeaponCheck53 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC53, 1
}
if(Gui_WeaponCheck53 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC53, 0
}
if(Gui_WeaponCheck54 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC54, 1
}
if(Gui_WeaponCheck54 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC54, 0
}
if(Gui_WeaponCheck55 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC55, 1
}
if(Gui_WeaponCheck55 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC55, 0
}
if(Gui_WeaponCheck56 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC56, 1
}
if(Gui_WeaponCheck56 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC56, 0
}
if(Gui_MagicCheck3 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC3, 1
}
if(Gui_MagicCheck3 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC3, 0
}
if(Gui_MagicCheck4 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC4, 1
}
if(Gui_MagicCheck4 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC4, 0
}
if(Gui_MagicCheck5 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC5, 1
}
if(Gui_MagicCheck5 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC5, 0
}
if(Gui_MagicCheck6 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC6, 1
}
if(Gui_MagicCheck6 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC6, 0
}
if(Gui_MagicCheck7 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC7, 1
}
if(Gui_MagicCheck7 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC7, 0
}
if(Gui_MagicCheck8 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC8, 1
}
if(Gui_MagicCheck8 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC8, 0
}
if(Gui_MagicCheck9 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC9, 1
}
if(Gui_MagicCheck9 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC9, 0
}
if(Gui_MagicCheck10 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC10, 1
}
if(Gui_MagicCheck10 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC10, 0
}
if(Gui_MagicCheck11 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC11, 1
}
if(Gui_MagicCheck11 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC11, 0
}
if(Gui_MagicCheck12 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC12, 1
}
if(Gui_MagicCheck12 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC12, 0
}
if(Gui_MagicCheck13 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC13, 1
}
if(Gui_MagicCheck13 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC13, 0
}
if(Gui_MagicCheck14 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC14, 1
}
if(Gui_MagicCheck14 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC14, 0
}
if(Gui_MagicCheck15 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC15, 1
}
if(Gui_MagicCheck15 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC15, 0
}
if(Gui_MagicCheck16 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC16, 1
}
if(Gui_MagicCheck16 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC16, 0
}
if(Gui_MagicCheck17 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC17, 1
}
if(Gui_MagicCheck17 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC17, 0
}
if(Gui_MagicCheck18 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC18, 1
}
if(Gui_MagicCheck18 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC18, 0
}
if(Gui_relogerror = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, relog, 1
}
if(Gui_relogerror = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, relog, 0
}
if(loady = 2)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, loady, 1
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, StartTime, %ProgramStartTime%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, CFH, %CheckFirstHP%
}
Gui, submit, nohide
Gui, listview, 포프레스네소각
FileDelete, %A_ScriptDir%\소각리스트.ini
save := LV_GetCount()
loop, %save%{
lv_gettext(savefile1,a_index)
FileAppend, %savefile1%`n, %A_ScriptDir%\소각리스트.ini
FileSetAttrib, +H, %A_ScriptDir%\소각리스트.ini
}
Sleep, 100
Reload
return
Addincinerate:
Gui, Submit, Nohide
Gui, listview, 포프레스네소각
incinerateitem := Gui_incinerateitem
LV_Add("",incinerateitem)
GuiControl, , Gui_incinerateitem
return
Delincinerate:
Gui, Submit, Nohide
Gui, listview, 포프레스네소각
RowNumber = 0
loop
{
RowNumber := LV_GetNext(RowNumber)
if not RowNumber
break
SelectRowNum := RowNumber
}
Lv_Delete(SelectRowNum)
return
incineration:
Gui, Submit, Nohide
if((Step >= 19 and Step < 90) or Step >= 1013 and Step < 1030)
{
IfInString,Location,포프레스네
{
Gui, listview, 포프레스네소각
IfInString,Location,필드
{
Loop % LV_GetCount()
{
LastRowNum := A_index
}
LV_Modify(inciNumber,"Select")
LV_Modify(inciNumber, "Vis")
LV_GetText(inciItem, inciNumber)
Sleep, 10
incinerate_item()
Sleep, 10
incinerate()
inciNumber += 1
if(inciNumber > LastRowNum)
{
inciNumber = 1
}
}
}
}
return
if(Step >= 7 and Step < 10000)
{
Get_inven()
if(NowInven = 50)
{
invenstack += 1
if(invenstack > 150)
{
SLEEP, 100
invenError += 1
step = 10000
return
}
}
}
if(invenError > 2)
{
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMSPH.exe
}
GUICONTROL, , Gui_NowState, 인벤토리에 빈공간이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("인벤칸 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
invenError = 0
return
}
Hunt:
Gui, Submit, Nohide
if(A_WDay = 5)
{
if(A_hour = 09)
{
if(A_Min = 00)
{
GuiControl, , Gui_NowState, 일랜시아 점검 15분 대기.
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
Sleep, 900000
OID리셋()
}
}
}
if(CheckPN = 1)
{
PNnowtime = %A_Now%
FormatTime, PNnowtime1, %PNnowtime%, yyyyMMddHHmm
if(PNnowtime1 > PNnewTime1)
{
MobNumber = 1
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
Step = 30
}
}
if(CheckPB = 1)
{
nowtime = %A_Now%
FormatTime, nowtime1, %nowtime%, yyyyMMddHHmm
if(nowtime1 > newTime1)
{
MobNumber = 1
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
Step = 1030
}
}
IfWinExist,Microsoft Windows
{
WinClose
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
}
IfWinExist,Microsoft Visual C++ Runtime Library
{
WinClose
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
}
IfWinExist,ahk_exe WerFault.exe
{
ControlClick, Button2, ahk_exe WerFault.exe
Process, Close, WerFault.exe
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
}
IfWinExist, ahk_pid %jPID%
{
if DllCall("IsHungAppWindow", "UInt", WinExist())
Process, Close, %jPID%
}
WinGetText, WindowErrorMsg, ahk_class #32770
IfInString,WindowErrorMsg,프로그램을 마치려면
{
ControlClick, Button1, ahk_class #32770
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
}
IfWinExist,ahk_pid %jPID%
{
SB_SetText("섭팅 : " . 서버팅김 . "  파라스방해 : " . 파라스감지 . "  수천방해 : " . 수천감지,3)
if((Step >= 11 and Step < 90) or (Step >= 1004 and Step < 1030))
{
if( 방어구방지ON = 1 )
{
AmorCheck()
if( Top = 0 or shoes = 0 or jean = 0 or Cap = 0 )
{
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMSPH.exe
}
GUICONTROL, , Gui_NowState, 캐릭터 사망 방지.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("캐릭사망방지, 자동정지")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
invenError = 0
MsgBox,48, 캐릭터사망방지, 템이 벗겨졌습니다.
return
}
}
}
if((Step >= 7 and Step < 507) or (Step >= 512 and Step < 10000))
{
Check_FormNumber()
if(FormNumber = 78)
{
Sleep,700
PostClick(132,93)
Sleep,500
Loop,
{
Check_FormNumber()
if(FormNumber = 0)
{
break
}
else
{
PostClick(132,93)
Sleep,500
}
}
수천감지++
}
}
if(Step >= 7 and Step < 10000)
{
Check_NPCMsg()
Check_FormNumber()
IfInString,NPCMsg,수호천사
{
if(FormNumber = 78)
{
SLEEP, 700
PostClick(132,93)
SLEEP, 500
수천감지++
}
}
Get_Location()
SB_SetText(Location,2)
IfInString,Location,포프레스네 마을
{
if( 방어구방지ON = 1 )
{
AmorCheck()
if( Top = 0 or shoes = 0 or jean = 0 or Cap = 0 )
{
GUICONTROL, , Gui_NowState, 착용템 확인 바랍니다.
SB_SetText("템부위확인 요망")
MsgBox,48, 캐릭터사망방지,템착용 확인 후 재시작을 눌러주세요.
gosub, 일시정지
}
}
}
Get_Gold()
GuiControl, , Gui_NowGold, %Gold1% 갈리드
Get_HP()
GuiControl,,Gui_NowHP,%NowHP% / %MaxHP% (%HPPercent%`%)
GuiControl,,Pro_NowHP,%HPPercent%
Get_MP()
GuiControl,,Gui_NowMP,%NowMP% / %MaxMP% (%MPPercent%`%)
GuiControl,,Pro_NowMP,%MPPercent%
Get_FP()
GuiControl,,Gui_NowFP,%NowFP% / %MaxFP% (%FPPercent%`%)
GuiControl,,Pro_NowFP,%FPPercent%
if(BWValue0 != "")
{
SetFormat, Float, 0.2
TempAbility := BWValue0 / 100
GuiControl,, Gui_BasicWValue0, %TempAbility%
SetFormat, Float, 0
}
if(BWValue1 != "")
{
SetFormat, Float, 0.2
TempAbility := BWValue1 / 100
GuiControl,, Gui_BasicWValue1, %TempAbility%
SetFormat, Float, 0
}
if(BWValue2 != "")
{
SetFormat, Float, 0.2
TempAbility := BWValue2 / 100
GuiControl,, Gui_BasicWValue2, %TempAbility%
SetFormat, Float, 0
}
if(BWValue3 != "")
{
SetFormat, Float, 0.2
TempAbility := BWValue3 / 100
GuiControl,, Gui_BasicWValue3, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility1 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility1 / 100
GuiControl, , Gui_WeaponValue1, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility2 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility2 / 100
GuiControl, , Gui_WeaponValue2, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility3 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility3 / 100
GuiControl, , Gui_WeaponValue3, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility4 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility4 / 100
GuiControl, , Gui_WeaponValue4, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility5 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility5 / 100
GuiControl, , Gui_WeaponValue5, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility6 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility6 / 100
GuiControl, , Gui_WeaponValue6, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility7 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility7 / 100
GuiControl, , Gui_WeaponValue7, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility8 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility8 / 100
GuiControl, , Gui_WeaponValue8, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility9 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility9 / 100
GuiControl, , Gui_WeaponValue9, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility10 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility10 / 100
GuiControl, , Gui_WeaponValue10, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility11 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility11 / 100
GuiControl, , Gui_WeaponValue11, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility12 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility12 / 100
GuiControl, , Gui_WeaponValue12, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility13 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility13 / 100
GuiControl, , Gui_WeaponValue13, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility14 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility14 / 100
GuiControl, , Gui_WeaponValue14, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility15 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility15 / 100
GuiControl, , Gui_WeaponValue15, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility16 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility16 / 100
GuiControl, , Gui_WeaponValue16, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility17 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility17 / 100
GuiControl, , Gui_WeaponValue17, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility18 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility18 / 100
GuiControl, , Gui_WeaponValue18, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility19 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility19 / 100
GuiControl, , Gui_WeaponValue19, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility20 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility20 / 100
GuiControl, , Gui_WeaponValue20, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility21 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility21 / 100
GuiControl, , Gui_WeaponValue21, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility22 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility22 / 100
GuiControl, , Gui_WeaponValue22, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility23 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility23 / 100
GuiControl, , Gui_WeaponValue23, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility24 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility24 / 100
GuiControl, , Gui_WeaponValue24, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility25 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility25 / 100
GuiControl, , Gui_WeaponValue25, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility26 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility26 / 100
GuiControl, , Gui_WeaponValue26, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility27 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility27 / 100
GuiControl, , Gui_WeaponValue27, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility28 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility28 / 100
GuiControl, , Gui_WeaponValue28, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility29 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility29 / 100
GuiControl, , Gui_WeaponValue29, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility30 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility30 / 100
GuiControl, , Gui_WeaponValue30, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility31 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility31 / 100
GuiControl, , Gui_WeaponValue31, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility32 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility32 / 100
GuiControl, , Gui_WeaponValue32, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility33 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility33 / 100
GuiControl, , Gui_WeaponValue33, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility34 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility34 / 100
GuiControl, , Gui_WeaponValue34, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility35 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility35 / 100
GuiControl, , Gui_WeaponValue35, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility36 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility36 / 100
GuiControl, , Gui_WeaponValue36, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility37 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility37 / 100
GuiControl, , Gui_WeaponValue37, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility38 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility38 / 100
GuiControl, , Gui_WeaponValue38, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility39 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility39 / 100
GuiControl, , Gui_WeaponValue39, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility40 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility40 / 100
GuiControl, , Gui_WeaponValue40, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility41 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility41 / 100
GuiControl, , Gui_WeaponValue41, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility42 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility42 / 100
GuiControl, , Gui_WeaponValue42, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility43 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility43 / 100
GuiControl, , Gui_WeaponValue43, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility44 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility44 / 100
GuiControl, , Gui_WeaponValue44, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility45 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility45 / 100
GuiControl, , Gui_WeaponValue45, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility46 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility46 / 100
GuiControl, , Gui_WeaponValue46, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility47 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility47 / 100
GuiControl, , Gui_WeaponValue47, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility48 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility48 / 100
GuiControl, , Gui_WeaponValue48, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility49 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility49 / 100
GuiControl, , Gui_WeaponValue49, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility50 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility50 / 100
GuiControl, , Gui_WeaponValue50, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility51 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility51 / 100
GuiControl, , Gui_WeaponValue51, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility52 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility52 / 100
GuiControl, , Gui_WeaponValue52, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility53 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility53 / 100
GuiControl, , Gui_WeaponValue53, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility54 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility54 / 100
GuiControl, , Gui_WeaponValue54, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility55 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility55 / 100
GuiControl, , Gui_WeaponValue55, %TempAbility%
SetFormat, Float, 0
}
if(WeaponAbility56 != "")
{
SetFormat, Float, 0.2
TempAbility := WeaponAbility56 / 100
GuiControl, , Gui_WeaponValue56, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility3 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility3
GuiControl, , Gui_MagicValue3, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility4 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility4
GuiControl, , Gui_MagicValue4, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility5 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility5
GuiControl, , Gui_MagicValue5, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility6 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility6
GuiControl, , Gui_MagicValue6, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility7 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility7
GuiControl, , Gui_MagicValue7, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility8 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility8
GuiControl, , Gui_MagicValue8, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility9 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility9
GuiControl, , Gui_MagicValue9, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility10 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility10
GuiControl, , Gui_MagicValue10, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility11 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility11
GuiControl, , Gui_MagicValue11, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility12 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility12
GuiControl, , Gui_MagicValue12, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility13 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility13
GuiControl, , Gui_MagicValue13, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility14 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility14
GuiControl, , Gui_MagicValue14, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility15 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility15
GuiControl, , Gui_MagicValue15, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility16 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility16
GuiControl, , Gui_MagicValue16, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility17 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility17
GuiControl, , Gui_MagicValue17, %TempAbility%
SetFormat, Float, 0
}
if(MagicAbility18 != "")
{
SetFormat, Float, 0.2
TempAbility := MagicAbility18
GuiControl, , Gui_MagicValue18, %TempAbility%
SetFormat, Float, 0
}
if(Regloady = 0)
{
CheckUPHP := MaxHP - CheckFirstHP
}
if(Regloady = 1)
{
CheckUPHP := MaxHP - RCFH
}
상승체력평균치 := (A_TickCount-ProgramStartTime)/1000
RunningTime := FormatSeconds((A_TickCount-ProgramStartTime)/1000)
상승체력평균값 := (CheckUPHP * 60) / (상승체력평균치/60)
GuiControl,,시작체력,%CheckFirstHP%
GuiControl,,상승체력,%CheckUPHP% (%상승체력평균값%)
GuiControl,,경과시간,%RunningTime%
if(파라스방해감지 = 1)
{
파라스타이머카운트 := FormatSeconds(파라스타이머값 - ((A_TickCount-파라스타이머시작)/1000))
GuiControl,,파라스타이머,파라스대기 = %파라스타이머카운트%
}
else
{
GuiControl,,파라스타이머,파라스대기 = 0:00:00
}
if(Gui_CheckUseHPExit = 1)
{
if(NowHP <= Gui_HPExit and NowHP != "")
{
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
GuiControl, , Gui_NowState, 체력이 %NowHP%가 되어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("헬퍼실행. 자동정지")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
}
internet := ConnectedToInternet()
if(internet = 0)
{
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
Step = 10000
return
}
ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16", aOffsets*)
IfInString,ServerMsg,서버와의 연결이
{
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
OutTime := A_TickCount
ParasTime := OutTime - JoinTime
if(ParasTime < 1200000)
{
ParasCount += 1
}
if(ParasTime >= 1200000)
{
ParasCount = 0
}
if(ParasCount > 3)
{
GuiControl, , Gui_NowState, [포남] 파라스를 감지하여 포북 이동.
ParasCount = 3
파라스방해감지 := 1
GuiControl,,Gui_huntpobuk,1
파라스감지++
}
서버팅김++
실행초기화 += 1
if(Step = 17 or step = 18)
{
Entrance += 1
}
if(Entrance > 2)
{
MsgBox, , 비정상종료감지, 포북으로 이동, 3
GuiControl, ,Gui_HuntPobuk, 1
step = 8
Sleep, 1000
return
}
}
Step = 10000
return
}
if(Gui_CheckUseHPLimited = 1)
{
if(Step >= 7 and Step < 10000)
{
if(MaxHP >= Gui_HPLimited)
{
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
GuiControl, , Gui_NowState, 설정된 체력에 도달하여 강제 종료합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("헬퍼실행. 자동정지")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
}
}
if(pbtalkcheck != 0)
{
pbtalkcheck2 := A_TickCount - pbtalkcheck1
if(pbtalkcheck2 >= 120000)
{
Sleep, 100
pbtalkcheck = 0
step = 10000
Sleep, 100
}
}
if((Step >= 19 and Step < 90) or (Step >= 1013 and Step < 1030))
{
callid := jelan.read(0x0058EDB8, "UInt", aOffsets*)
if(callid > 2500)
{
value := jelan.write(0x0058EDB8, 0, "UInt")
SLEEP, 500
Entrance = 0
}
}
if((Step >= 19 and Step < 90) or (Step >= 1013 and Step < 1030))
{
name := jelan.readString(jelan.read(0x0058F058)+0x184,,"UTF-16")
if( name != "" )
{
Loop,
{
SB_SetText("거래창 방해감지")
AltR()
Sleep,100
AltR()
Sleep,100
name := jelan.readString(jelan.read(0x0058F058)+0x184,,"UTF-16")
if( name = "" )
{
name := 0
break
}
}
}
}
if((Step >= 17 and Step < 90) or (Step >= 1006 and Step < 1032))
{
GuiControl,,호출대상이름,%호출대상%
}
if((Step >= 19 and Step < 90) or (Step >= 1013 and Step < 1030))
{
if(4번사용 = 1)
{
PostMessage, 0x100, 52, 327681, , ahk_pid %jPID%
PostMessage, 0x101, 52, 327681, , ahk_pid %jPID%
}
if(5번사용 = 1)
{
PostMessage, 0x100, 53, 393217, , ahk_pid %jPID%
PostMessage, 0x101, 53, 393217, , ahk_pid %jPID%
}
if(6번사용 = 1)
{
PostMessage, 0x100, 54, 458753, , ahk_pid %jPID%
PostMessage, 0x101, 54, 458753, , ahk_pid %jPID%
}
if(7번사용 = 1)
{
PostMessage, 0x100, 55, 524289, , ahk_pid %jPID%
PostMessage, 0x101, 55, 524289, , ahk_pid %jPID%
}
if(HuntPlace = 1 and NowFp <= 포남생콩섭취)
{
PostMessage, 0x100, 56, 589825, , ahk_pid %jPID%
PostMessage, 0x101, 56, 589825, , ahk_pid %jPID%
}
if(HuntPlace = 2 and NowFp <= 포북생콩섭취)
{
PostMessage, 0x100, 56, 589825, , ahk_pid %jPID%
PostMessage, 0x101, 56, 589825, , ahk_pid %jPID%
}
if(Gui_jjON = 1)
{
Get_Inven()
if(NowInven > 가방수량체크)
{
jelan.write(0x0047B3EC, 0x4D, "Char", aOffsets*)
}
if(NowInven <= 가방수량체크)
{
jelan.write(0x0047B3EC, 0x02, "Char", aOffsets*)
}
}
Check_SAbilityN()
Check_SAbility()
if(Slot1AN != "")
{
GuiControl, , Gui_WeaponName1, %Slot1AN%
if(Gui_WeaponCheck1 = 1)
{
WeaponAbility1 := Slot1Ability
}
if(Gui_WeaponCheck1 = 0)
{
WeaponAbility1 =
}
}
if(Slot2AN != "")
{
GuiControl, , Gui_WeaponName2, %Slot2AN%
if(Gui_WeaponCheck2 = 1)
{
WeaponAbility2 := Slot2Ability
}
if(Gui_WeaponCheck2 = 0)
{
WeaponAbility2 =
}
}
if(Slot3AN != "")
{
GuiControl, , Gui_WeaponName3, %Slot3AN%
if(Gui_WeaponCheck3 = 1)
{
WeaponAbility3 := Slot3Ability
}
if(Gui_WeaponCheck3 = 0)
{
WeaponAbility3 =
}
}
if(Slot4AN != "")
{
GuiControl, , Gui_WeaponName4, %Slot4AN%
if(Gui_WeaponCheck4 = 1)
{
WeaponAbility4 := Slot4Ability
}
if(Gui_WeaponCheck4 = 0)
{
WeaponAbility4 =
}
}
if(Slot5AN != "")
{
GuiControl, , Gui_WeaponName5, %Slot5AN%
if(Gui_WeaponCheck5 = 1)
{
WeaponAbility5 := Slot5Ability
}
if(Gui_WeaponCheck5 = 0)
{
WeaponAbility5 =
}
}
if(Slot6AN != "")
{
GuiControl, , Gui_WeaponName6, %Slot6AN%
if(Gui_WeaponCheck6 = 1)
{
WeaponAbility6 := Slot6Ability
}
if(Gui_WeaponCheck6 = 0)
{
WeaponAbility6 =
}
}
if(Slot7AN != "")
{
GuiControl, , Gui_WeaponName7, %Slot7AN%
if(Gui_WeaponCheck7 = 1)
{
WeaponAbility7 := Slot7Ability
}
if(Gui_WeaponCheck7 = 0)
{
WeaponAbility7 =
}
}
if(Slot8AN != "")
{
GuiControl, , Gui_WeaponName8, %Slot8AN%
if(Gui_WeaponCheck8 = 1)
{
WeaponAbility8 := Slot8Ability
}
if(Gui_WeaponCheck8 = 0)
{
WeaponAbility8 =
}
}
if(Slot9AN != "")
{
GuiControl, , Gui_WeaponName9, %Slot9AN%
if(Gui_WeaponCheck9 = 1)
{
WeaponAbility9 := Slot9Ability
}
if(Gui_WeaponCheck9 = 0)
{
WeaponAbility9 =
}
}
if(Slot10AN != "")
{
GuiControl, , Gui_WeaponName10, %Slot10AN%
if(Gui_WeaponCheck10 = 1)
{
WeaponAbility10 := Slot10Ability
}
if(Gui_WeaponCheck10 = 0)
{
WeaponAbility10 =
}
}
if(Slot11AN != "")
{
GuiControl, , Gui_WeaponName11, %Slot11AN%
if(Gui_WeaponCheck11 = 1)
{
WeaponAbility11 := Slot11Ability
}
if(Gui_WeaponCheck11 = 0)
{
WeaponAbility11 =
}
}
if(Slot12AN != "")
{
GuiControl, , Gui_WeaponName12, %Slot12AN%
if(Gui_WeaponCheck12 = 1)
{
WeaponAbility12 := Slot12Ability
}
if(Gui_WeaponCheck12 = 0)
{
WeaponAbility12 =
}
}
if(Slot13AN != "")
{
GuiControl, , Gui_WeaponName13, %Slot13AN%
if(Gui_WeaponCheck13 = 1)
{
WeaponAbility13 := Slot13Ability
}
if(Gui_WeaponCheck13 = 0)
{
WeaponAbility13 =
}
}
if(Slot14AN != "")
{
GuiControl, , Gui_WeaponName14, %Slot14AN%
if(Gui_WeaponCheck14 = 1)
{
WeaponAbility14 := Slot14Ability
}
if(Gui_WeaponCheck14 = 0)
{
WeaponAbility14 =
}
}
if(Slot15AN != "")
{
GuiControl, , Gui_WeaponName15, %Slot15AN%
if(Gui_WeaponCheck15 = 1)
{
WeaponAbility15 := Slot15Ability
}
if(Gui_WeaponCheck15 = 0)
{
WeaponAbility15 =
}
}
if(Slot16AN != "")
{
GuiControl, , Gui_WeaponName16, %Slot16AN%
if(Gui_WeaponCheck16 = 1)
{
WeaponAbility16 := Slot16Ability
}
if(Gui_WeaponCheck16 = 0)
{
WeaponAbility16 =
}
}
if(Slot17AN != "")
{
GuiControl, , Gui_WeaponName17, %Slot17AN%
if(Gui_WeaponCheck17 = 1)
{
WeaponAbility17 := Slot17Ability
}
if(Gui_WeaponCheck17 = 0)
{
WeaponAbility17 =
}
}
if(Slot18AN != "")
{
GuiControl, , Gui_WeaponName18, %Slot18AN%
if(Gui_WeaponCheck18 = 1)
{
WeaponAbility18 := Slot18Ability
}
if(Gui_WeaponCheck18 = 0)
{
WeaponAbility18 =
}
}
if(Slot19AN != "")
{
GuiControl, , Gui_WeaponName19, %Slot19AN%
if(Gui_WeaponCheck19 = 1)
{
WeaponAbility19 := Slot19Ability
}
if(Gui_WeaponCheck19 = 0)
{
WeaponAbility19 =
}
}
if(Slot20AN != "")
{
GuiControl, , Gui_WeaponName20, %Slot20AN%
if(Gui_WeaponCheck20 = 1)
{
WeaponAbility20 := Slot20Ability
}
if(Gui_WeaponCheck20 = 0)
{
WeaponAbility20 =
}
}
if(Slot21AN != "")
{
GuiControl, , Gui_WeaponName21, %Slot21AN%
if(Gui_WeaponCheck21 = 1)
{
WeaponAbility21 := Slot21Ability
}
if(Gui_WeaponCheck21 = 0)
{
WeaponAbility21 =
}
}
if(Slot22AN != "")
{
GuiControl, , Gui_WeaponName22, %Slot22AN%
if(Gui_WeaponCheck22 = 1)
{
WeaponAbility22 := Slot22Ability
}
if(Gui_WeaponCheck22 = 0)
{
WeaponAbility22 =
}
}
if(Slot23AN != "")
{
GuiControl, , Gui_WeaponName23, %Slot23AN%
if(Gui_WeaponCheck23 = 1)
{
WeaponAbility23 := Slot23Ability
}
if(Gui_WeaponCheck23 = 0)
{
WeaponAbility23 =
}
}
if(Slot24AN != "")
{
GuiControl, , Gui_WeaponName24, %Slot24AN%
if(Gui_WeaponCheck24 = 1)
{
WeaponAbility24 := Slot24Ability
}
if(Gui_WeaponCheck24 = 0)
{
WeaponAbility24 =
}
}
if(Slot25AN != "")
{
GuiControl, , Gui_WeaponName25, %Slot25AN%
if(Gui_WeaponCheck25 = 1)
{
WeaponAbility25 := Slot25Ability
}
if(Gui_WeaponCheck25 = 0)
{
WeaponAbility25 =
}
}
if(Slot26AN != "")
{
GuiControl, , Gui_WeaponName26, %Slot26AN%
if(Gui_WeaponCheck26 = 1)
{
WeaponAbility26 := Slot26Ability
}
if(Gui_WeaponCheck26 = 0)
{
WeaponAbility26 =
}
}
if(Slot27AN != "")
{
GuiControl, , Gui_WeaponName27, %Slot27AN%
if(Gui_WeaponCheck27 = 1)
{
WeaponAbility27 := Slot27Ability
}
if(Gui_WeaponCheck27 = 0)
{
WeaponAbility27 =
}
}
if(Slot28AN != "")
{
GuiControl, , Gui_WeaponName28, %Slot28AN%
if(Gui_WeaponCheck28 = 1)
{
WeaponAbility28 := Slot28Ability
}
if(Gui_WeaponCheck28 = 0)
{
WeaponAbility28 =
}
}
if(Slot29AN != "")
{
GuiControl, , Gui_WeaponName29, %Slot29AN%
if(Gui_WeaponCheck29 = 1)
{
WeaponAbility29 := Slot29Ability
}
if(Gui_WeaponCheck29 = 0)
{
WeaponAbility29 =
}
}
if(Slot30AN != "")
{
GuiControl, , Gui_WeaponName30, %Slot30AN%
if(Gui_WeaponCheck30 = 1)
{
WeaponAbility30 := Slot30Ability
}
if(Gui_WeaponCheck30 = 0)
{
WeaponAbility30 =
}
}
if(Slot31AN != "")
{
GuiControl, , Gui_WeaponName31, %Slot31AN%
if(Gui_WeaponCheck31 = 1)
{
WeaponAbility31 := Slot31Ability
}
if(Gui_WeaponCheck31 = 0)
{
WeaponAbility31 =
}
}
if(Slot32AN != "")
{
GuiControl, , Gui_WeaponName32, %Slot32AN%
if(Gui_WeaponCheck32 = 1)
{
WeaponAbility32 := Slot32Ability
}
if(Gui_WeaponCheck32 = 0)
{
WeaponAbility32 =
}
}
if(Slot33AN != "")
{
GuiControl, , Gui_WeaponName33, %Slot33AN%
if(Gui_WeaponCheck33 = 1)
{
WeaponAbility33 := Slot33Ability
}
if(Gui_WeaponCheck33 = 0)
{
WeaponAbility33 =
}
}
if(Slot34AN != "")
{
GuiControl, , Gui_WeaponName34, %Slot34AN%
if(Gui_WeaponCheck34 = 1)
{
WeaponAbility34 := Slot34Ability
}
if(Gui_WeaponCheck34 = 0)
{
WeaponAbility34 =
}
}
if(Slot35AN != "")
{
GuiControl, , Gui_WeaponName35, %Slot35AN%
if(Gui_WeaponCheck35 = 1)
{
WeaponAbility35 := Slot35Ability
}
if(Gui_WeaponCheck35 = 0)
{
WeaponAbility35 =
}
}
if(Slot36AN != "")
{
GuiControl, , Gui_WeaponName36, %Slot36AN%
if(Gui_WeaponCheck36 = 1)
{
WeaponAbility36 := Slot36Ability
}
if(Gui_WeaponCheck36 = 0)
{
WeaponAbility36 =
}
}
if(Slot37AN != "")
{
GuiControl, , Gui_WeaponName37, %Slot37AN%
if(Gui_WeaponCheck37 = 1)
{
WeaponAbility37 := Slot37Ability
}
if(Gui_WeaponCheck37 = 0)
{
WeaponAbility37 =
}
}
if(Slot38AN != "")
{
GuiControl, , Gui_WeaponName38, %Slot38AN%
if(Gui_WeaponCheck38 = 1)
{
WeaponAbility38 := Slot38Ability
}
if(Gui_WeaponCheck38 = 0)
{
WeaponAbility38 =
}
}
if(Slot39AN != "")
{
GuiControl, , Gui_WeaponName39, %Slot39AN%
if(Gui_WeaponCheck39 = 1)
{
WeaponAbility39 := Slot39Ability
}
if(Gui_WeaponCheck39 = 0)
{
WeaponAbility39 =
}
}
if(Slot40AN != "")
{
GuiControl, , Gui_WeaponName40, %Slot40AN%
if(Gui_WeaponCheck40 = 1)
{
WeaponAbility40 := Slot40Ability
}
if(Gui_WeaponCheck40 = 0)
{
WeaponAbility40 =
}
}
if(Slot41AN != "")
{
GuiControl, , Gui_WeaponName41, %Slot41AN%
if(Gui_WeaponCheck41 = 1)
{
WeaponAbility41 := Slot41Ability
}
if(Gui_WeaponCheck41 = 0)
{
WeaponAbility41 =
}
}
if(Slot42AN != "")
{
GuiControl, , Gui_WeaponName42, %Slot42AN%
if(Gui_WeaponCheck42 = 1)
{
WeaponAbility42 := Slot42Ability
}
if(Gui_WeaponCheck42 = 0)
{
WeaponAbility42 =
}
}
if(Slot43AN != "")
{
GuiControl, , Gui_WeaponName43, %Slot43AN%
if(Gui_WeaponCheck43 = 1)
{
WeaponAbility43 := Slot43Ability
}
if(Gui_WeaponCheck43 = 0)
{
WeaponAbility43 =
}
}
if(Slot44AN != "")
{
GuiControl, , Gui_WeaponName44, %Slot44AN%
if(Gui_WeaponCheck44 = 1)
{
WeaponAbility44 := Slot44Ability
}
if(Gui_WeaponCheck44 = 0)
{
WeaponAbility44 =
}
}
if(Slot45AN != "")
{
GuiControl, , Gui_WeaponName45, %Slot45AN%
if(Gui_WeaponCheck45 = 1)
{
WeaponAbility45 := Slot45Ability
}
if(Gui_WeaponCheck45 = 0)
{
WeaponAbility45 =
}
}
if(Slot46AN != "")
{
GuiControl, , Gui_WeaponName46, %Slot46AN%
if(Gui_WeaponCheck46 = 1)
{
WeaponAbility46 := Slot46Ability
}
if(Gui_WeaponCheck46 = 0)
{
WeaponAbility46 =
}
}
if(Slot47AN != "")
{
GuiControl, , Gui_WeaponName47, %Slot47AN%
if(Gui_WeaponCheck47 = 1)
{
WeaponAbility47 := Slot47Ability
}
if(Gui_WeaponCheck47 = 0)
{
WeaponAbility47 =
}
}
if(Slot48AN != "")
{
GuiControl, , Gui_WeaponName48, %Slot48AN%
if(Gui_WeaponCheck48 = 1)
{
WeaponAbility48 := Slot48Ability
}
if(Gui_WeaponCheck48 = 0)
{
WeaponAbility48 =
}
}
if(Slot49AN != "")
{
GuiControl, , Gui_WeaponName49, %Slot49AN%
if(Gui_WeaponCheck49 = 1)
{
WeaponAbility49 := Slot49Ability
}
if(Gui_WeaponCheck49 = 0)
{
WeaponAbility49 =
}
}
if(Slot50AN != "")
{
GuiControl, , Gui_WeaponName50, %Slot50AN%
if(Gui_WeaponCheck50 = 1)
{
WeaponAbility50 := Slot50Ability
}
if(Gui_WeaponCheck50 = 0)
{
WeaponAbility50 =
}
}
if(Slot51AN != "")
{
GuiControl, , Gui_WeaponName51, %Slot51AN%
if(Gui_WeaponCheck51 = 1)
{
WeaponAbility51 := Slot51Ability
}
if(Gui_WeaponCheck51 = 0)
{
WeaponAbility51 =
}
}
if(Slot52AN != "")
{
GuiControl, , Gui_WeaponName52, %Slot52AN%
if(Gui_WeaponCheck52 = 1)
{
WeaponAbility52 := Slot52Ability
}
if(Gui_WeaponCheck52 = 0)
{
WeaponAbility52 =
}
}
if(Slot53AN != "")
{
GuiControl, , Gui_WeaponName53, %Slot53AN%
if(Gui_WeaponCheck53 = 1)
{
WeaponAbility53 := Slot53Ability
}
if(Gui_WeaponCheck53 = 0)
{
WeaponAbility53 =
}
}
if(Slot54AN != "")
{
GuiControl, , Gui_WeaponName54, %Slot54AN%
if(Gui_WeaponCheck54 = 1)
{
WeaponAbility54 := Slot54Ability
}
if(Gui_WeaponCheck54 = 0)
{
WeaponAbility54 =
}
}
if(Slot55AN != "")
{
GuiControl, , Gui_WeaponName55, %Slot55AN%
if(Gui_WeaponCheck55 = 1)
{
WeaponAbility55 := Slot55Ability
}
if(Gui_WeaponCheck55 = 0)
{
WeaponAbility55 =
}
}
if(Slot56AN != "")
{
GuiControl, , Gui_WeaponName56, %Slot56AN%
if(Gui_WeaponCheck56 = 1)
{
WeaponAbility56 := Slot56Ability
}
if(Gui_WeaponCheck56 = 0)
{
WeaponAbility56 =
}
}
Check_SMagicN()
Check_SMagic()
if(Slot3MN != "")
{
GuiControl, , Gui_MagicName3, %Slot3MN%
if(Gui_MagicCheck3 = 1)
{
MagicAbility3 := Slot3Magic
}
if(Gui_MagicCheck3 = 0)
{
MagicAbility3 =
}
}
if(Slot4MN != "")
{
GuiControl, , Gui_MagicName4, %Slot4MN%
if(Gui_MagicCheck4 = 1)
{
MagicAbility4 := Slot4Magic
}
if(Gui_MagicCheck4 = 0)
{
MagicAbility4 =
}
}
if(Slot5MN != "")
{
GuiControl, , Gui_MagicName5, %Slot5MN%
if(Gui_MagicCheck5 = 1)
{
MagicAbility5 := Slot5Magic
}
if(Gui_MagicCheck5 = 0)
{
MagicAbility5 =
}
}
if(Slot6MN != "")
{
GuiControl, , Gui_MagicName6, %Slot6MN%
if(Gui_MagicCheck6 = 1)
{
MagicAbility6 := Slot6Magic
}
if(Gui_MagicCheck6 =0)
{
MagicAbility6 =
}
}
if(Slot7MN != "")
{
GuiControl, , Gui_MagicName7, %Slot7MN%
if(Gui_MagicCheck7 = 1)
{
MagicAbility7 := Slot7Magic
}
if(Gui_MagicCheck7 = 0)
{
MagicAbility7 =
}
}
if(Slot8MN != "")
{
GuiControl, , Gui_MagicName8, %Slot8MN%
if(Gui_MagicCheck8 = 1)
{
MagicAbility8 := Slot8Magic
}
if(Gui_MagicCheck8 = 0)
{
MagicAbility8 =
}
}
if(Slot9MN != "")
{
GuiControl, , Gui_MagicName9, %Slot9MN%
if(Gui_MagicCheck9 = 1)
{
MagicAbility9 := Slot9Magic
}
if(Gui_MagicCheck9 = 0)
{
MagicAbility9 =
}
}
if(Slot10MN != "")
{
GuiControl, , Gui_MagicName10, %Slot10MN%
if(Gui_MagicCheck10 = 1)
{
MagicAbility10 := Slot10Magic
}
if(Gui_MagicCheck10 = 0)
{
MagicAbility10 =
}
}
if(Slot11MN != "")
{
GuiControl, , Gui_MagicName11, %Slot11MN%
if(Gui_MagicCheck11 = 1)
{
MagicAbility11 := Slot11Magic
}
if(Gui_MagicCheck11 = 0)
{
MagicAbility11 =
}
}
if(Slot12MN != "")
{
GuiControl, , Gui_MagicName12, %Slot12MN%
if(Gui_MagicCheck12 = 1)
{
MagicAbility12 := Slot12Magic
}
if(Gui_MagicCheck12 =0)
{
MagicAbility12 =
}
}
if(Slot13MN != "")
{
GuiControl, , Gui_MagicName13, %Slot13MN%
if(Gui_MagicCheck13 = 1)
{
MagicAbility13 := Slot13Magic
}
if(Gui_MagicCheck13 = 0)
{
MagicAbility13 =
}
}
if(Slot14MN != "")
{
GuiControl, , Gui_MagicName14, %Slot14MN%
if(Gui_MagicCheck14 = 1)
{
MagicAbility14 := Slot14Magic
}
if(Gui_MagicCheck14 = 0)
{
MagicAbility14 =
}
}
if(Slot15MN != "")
{
GuiControl, , Gui_MagicName15, %Slot15MN%
if(Gui_MagicCheck15 = 1)
{
MagicAbility15 := Slot15Magic
}
if(Gui_MagicCheck15 = 0)
{
MagicAbility15 =
}
}
if(Slot16MN != "")
{
GuiControl, , Gui_MagicName16, %Slot16MN%
if(Gui_MagicCheck16 = 1)
{
MagicAbility16 := Slot16Magic
}
if(Gui_MagicCheck16 =0)
{
MagicAbility16 =
}
}
if(Slot17MN != "")
{
GuiControl, , Gui_MagicName17, %Slot17MN%
if(Gui_MagicCheck17 = 1)
{
MagicAbility17 := Slot17Magic
}
if(Gui_MagicCheck17 = 0)
{
MagicAbility17 =
}
}
if(Slot18MN != "")
{
GuiControl, , Gui_MagicName18, %Slot18MN%
if(Gui_MagicCheck18 = 1)
{
MagicAbility18 := Slot18Magic
}
if(Gui_MagicCheck18 = 0)
{
MagicAbility18 =
}
}
그레이드읽어오기()
if(대화사용 = 1)
{
jelan.write(0x0058D603, 대화번호, "Char")
AA1()
}
if(명상사용 = 1)
{
jelan.write(0x0058D613, 명상번호, "Char")
AA2()
}
if(더블어택사용 = 1)
{
jelan.write(0x0058D623, 더블어택번호, "Char")
AA3()
}
if(체력향상사용 = 1)
{
jelan.write(0x0058D633, 체력향상번호, "Char")
AA4()
}
if(집중사용 = 1)
{
jelan.write(0x0058D643, 집중번호, "Char")
AA5()
}
if(회피사용 = 1)
{
jelan.write(0x0058D653, 회피번호, "Char")
AA6()
}
if(몸통지르기사용 = 1)
{
jelan.write(0x0058D663, 몸통지르기번호, "Char")
AA7()
}
if(리무브아머사용 = 1)
{
jelan.write(0x0058D673, 리무브아머번호, "Char")
AA8()
}
if(민첩향상사용 = 1)
{
jelan.write(0x0058D683, 민첩향상번호, "Char")
AA9()
}
if(활방어사용 = 1)
{
jelan.write(0x0058D693, 활방어번호, "Char")
AA10()
}
if(마력향상사용 = 1)
{
jelan.write(0x0058D6A3, 마력향상번호, "Char")
AA11()
}
if(마력방어사용 = 1)
{
jelan.write(0x0058D6B3, 마력방어번호, "Char")
AA12()
}
}
if((Step >= 19 and Step < 90) or Step >= 1008)
{
if(Gui_CheckUseHPPortal = 1)
{
if(NowHP <= Gui_HPPortal and NowHP != "")
{
if(HuntPlace = 1)
{
MapNumber = 1
MobNumber = 1
MoveWaitCount = 0
SuinAStartX = 364
SuinAEndX = 430
SuinBStartY = 158
SuinBEndY = 227
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
GuiControl, , Gui_NowState, 체력이 %NowHP%가 되어 차원이동 합니다.
CheckPB = 0
CheckPN = 0
Step = 9
return
}
if(HuntPlace = 2)
{
CheckPB = 0
CheckPN = 0
MapNumber = 1
MoveWaitCount = 0
CenterStartY = 150
SuinAStartX = 364
SuinAEndX = 430
SuinBStartY = 158
SuinBEndY = 227
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
GuiControl, , Gui_NowState, 체력이 %NowHP%가 되어 차원이동 합니다.
Step = 1000
return
}
}
}
if(WeaponAbility1 = 10000 or WeaponAbility2 = 10000 or WeaponAbility3 = 10000 or WeaponAbility4 = 10000 or WeaponAbility5 = 10000 or WeaponAbility6 = 10000 or WeaponAbility7 = 10000 or WeaponAbility8 = 10000 or WeaponAbility9 = 10000 or WeaponAbility10 = 10000 or WeaponAbility11 = 10000 or WeaponAbility12 = 10000 or WeaponAbility13 = 10000 or WeaponAbility14 = 10000 or WeaponAbility15 = 10000 or WeaponAbility16 = 10000 or WeaponAbility17 = 10000 or WeaponAbility18 = 10000 or WeaponAbility19 = 10000 or WeaponAbility20 = 10000 or WeaponAbility21 = 10000 or WeaponAbility22 = 10000 or WeaponAbility23 = 10000 or WeaponAbility24 = 10000 or WeaponAbility25 = 10000 or WeaponAbility26 = 10000 or WeaponAbility27 = 10000 or WeaponAbility28 = 10000 or WeaponAbility29 = 10000 or WeaponAbility30 = 10000 or WeaponAbility31 = 10000 or WeaponAbility32 = 10000 or WeaponAbility33 = 10000 or WeaponAbility34 = 10000 or WeaponAbility35 = 10000 or WeaponAbility36 = 10000 or WeaponAbility37 = 10000  or WeaponAbility38 = 10000  or WeaponAbility39 = 10000  or WeaponAbility40 = 10000  or WeaponAbility41 = 10000  or WeaponAbility42 = 10000  or WeaponAbility43 = 10000  or WeaponAbility44 = 10000  or WeaponAbility45 = 10000  or WeaponAbility46 = 10000  or WeaponAbility47 = 10000  or WeaponAbility48 = 10000  or WeaponAbility49 = 10000  or WeaponAbility50 = 10000  or WeaponAbility51 = 10000  or WeaponAbility52 = 10000  or WeaponAbility53 = 10000 or WeaponAbility54 = 10000 or WeaponAbility55 = 10000 or WeaponAbility56 = 10000)
{
if(Gui_Grade = 1)
{
CheckPB = 0
CheckPN = 0
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 600
}
}
if(MagicAbility3 = 100 or MagicAbility4 = 100 or MagicAbility5 = 100 or MagicAbility6 = 100 or MagicAbility7 = 100 or MagicAbility8 = 100 or MagicAbility9 = 100 or MagicAbility10 = 100 or MagicAbility11 = 100 or MagicAbility12 = 100 or MagicAbility13 = 100 or MagicAbility14 = 100 or MagicAbility15 = 100 or MagicAbility16 = 100 or MagicAbility17 = 100 or MagicAbility18 = 100)
{
if(Gui_Grade = 1)
{
CheckPB = 0
CheckPN = 0
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 650
}
}
if(NowFP < 10)
{
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
CheckPB = 0
CheckPN = 0
MapNumber = 1
MobNumber = 1
MoveWaitCount = 0
CenterStartY = 150
SuinAStartX = 364
SuinAEndX = 430
SuinBStartY = 158
SuinBEndY = 227
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
GuiControl, , Gui_NowState, FP가 부족하여 채우러 갑니다.
Step = 200
return
}
}
if(Step >= 19 and Step < 90)
{
IfNotInString,Location,남쪽 필드
{
OutTime := A_TickCount
ParasTime := OutTime - JoinTime
if(ParasTime < 1200000)
{
ParasCount += 1
}
if(ParasTime >= 1200000)
{
ParasCount = 0
}
if(ParasCount > 3)
{
GuiControl, , Gui_NowState, [포남] 파라스를 감지하여 포북 이동.
ParasCount = 3
파라스방해감지 := 1
Settimer, 파라스대기, %파라스대기값%
파라스타이머시작 := A_TickCount
GuiControl,,Gui_huntpobuk,1
파라스감지++
}
MapNumber = 1
MobNumber = 1
MoveWaitCount = 0
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
Step = 9
return
}
}
if(Step >= 1013 and Step < 1030)
{
IfNotInString,Location,북쪽 필드
{
MapNumber = 1
MobNumber = 1
MoveWaitCount = 0
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
Step = 1000
return
}
}
Check_Chat()
if(Chat = 1)
{
PostMessage, 0x100, 13, 1835009, , ahk_pid %jPID%
PostMessage, 0x101, 13, 1835009, , ahk_pid %jPID%
}
if(Step = 27 or Step = 1026)
{
if(Step = 27)
{
if(Gui_EvadeMand = 1)
{
IfWinNotActive, ahk_pid %jPID%
{
WinActivate, ahk_pid %jPID%
}
PixelSearch, MandX, MandY, 0, 0, 775, 460, 0x4A044A, , *fast
if(ErrorLevel = 0)
{
AttackLoopCount = 0
AttackCount = 0
Step = 19
return
}
}
}
if(Gui_HuntAuto = 1)
{
if(HuntPlace = 1)
{
if(Gui_1Muba = 1)
{
if(BWValue1 >= Gui_LimitAbility1)
{
HuntPlace = 2
MapNumber = 1
Step = 1000
}
}
if(Gui_2Muba = 1)
{
if(BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2)
{
HuntPlace = 2
MapNumber = 1
Step = 1000
}
}
if(Gui_3Muba = 1)
{
if(BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2 or BWValue3 >= Gui_LimitAbility3)
{
HuntPlace = 2
MapNumber = 1
Step = 1000
}
}
if(Gui_2ButMuba = 1)
{
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1)
{
HuntPlace = 2
MapNumber = 1
Step = 1000
}
}
if(Gui_3ButMuba = 1)
{
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1 or  BWValue2 >= Gui_LimitAbility2)
{
HuntPlace = 2
MapNumber = 1
Step = 1000
}
}
if(Gui_4ButMuba = 1)
{
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1 or  BWValue2 >= Gui_LimitAbility2 or BWValue3 >= Gui_LimitAbility3)
{
HuntPlace = 2
MapNumber = 1
Step = 1000
}
}
}
}
}
}
}
IfWinNotExist,ahk_pid %jPID%
{
if(Step >= 5 and Step < 10000)
{
GuiControl, , 로그인상태정보, 오류로 인해 재접속 합니다.
Step = 0
}
}
재실행:
if(Step = 0)
{
GuiControl, , 로그인상태정보, 초기 세팅 중 입니다.
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
WinKill, ahk_pid %jPID%
GroupAdd, ie_gruop, ahk_exe iexplore.exe
WinKill, ahk_exe iexplore.exe
WinKill, ahk_group ie_gruop
WinKill, ahk_exe MRMSPH.exe
countsignal = 0
MapNumber = 1
MoveWaitCount = 0
MobNumber = 1
AttackLoopCount = 0
AttackCount = 0
pbtalkcheck = 0
RunDirect = 0
초기마을이동 := 0
getidc = 1
callid = 1
ipmak = 0
실행초기화 := 0
Entrance = 0
RCC = 0
inciNumber = 1
MLimit := Gui_AllMobLimit/100
MubaStep = 1
CenterStartX = 350
CenterStartY = 150
CenterEndX = 450
CenterEndY = 270
SuinAStartX = 364
SuinAStartY = 182
SuinAEndX = 430
SuinAEndY = 203
SuinBStartX = 388
SuinBStartY = 158
SuinBEndX = 406
SuinBEndY = 227
WinKill, ahk_pid %jPID%
GroupAdd, ie_gruop, ahk_exe iexplore.exe
WinKill, ahk_exe iexplore.exe
WinKill, ahk_group ie_gruop
WinKill, ahk_exe MRMSPH.exe
FileDelete, Mlog.txt
if(실행초기화 = 0)
{
SB_SetText("프로그램 구동 초기설정중")
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
Sleep, 2000
}else
{
SB_SetText("서버연결끊킴. 재접속 설정중")
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
Sleep, 15000
실행초기화 := 0
}
if( 파라스방해감지 = 1 )
{
Settimer, 파라스대기, %파라스대기값%
파라스타이머시작 := A_TickCount
}
Step = 1
}
if (Gui_Login = "인터넷")
{
if (Step = 1)
{
    GUICONTROL, , 로그인상태정보, [로그인] - 접속 중
    FileCreateDir, ChromeProfile
    ProfilePath := A_ScriptDir . "\ChromeProfile"
    ChromeInst := new Chrome(ProfilePath, , , , , False)

    Sleep, 100
    PageInst := ChromeInst.GetPage()
    Sleep, 100

    PageInst.Call("Page.navigate", {"url": "https://elancia.nexon.com/"})
    SB_SetText("홈페이지 접속중")

    while (PageInst.Evaluate("document.readyState").value != "complete")
        Sleep, 100

    PageInst.Evaluate("PS.game.startGame({ gameCode:74276 });")
    Sleep, 4000

    LoginURL := PageInst.Evaluate("window.location.href").value
    if (LoginURL != "https://nxlogin.nexon.com/common/login.aspx?redirect=https%3A%2F%2Felancia.nexon.com%2F")
    {
        reason := "접속불량"
		SB_SetText("이유 : " reason)
        Gosub, TryLoginFail
        step = 0
        return
    }

    ; ID, PW 입력
    PageInst.Evaluate("document.querySelector('#txtNexonID').value = '" Gui_NexonID "';")
    PageInst.Evaluate("document.querySelector('#txtPWD').value = '" Gui_NexonPassWord "';")
    PageInst.Evaluate("document.querySelector('.button01').click();")

    Sleep, 1000
    while (PageInst.Evaluate("document.readyState").value != "complete")
        Sleep, 100

    SB_SetText("넥슨 로그인 체크")
    LoginURL := PageInst.Evaluate("window.location.href").value

    if (LoginURL != "https://nxlogin.nexon.com/common/login.aspx?redirect=https%3A%2F%2Felancia.nexon.com%2F")
    {
        IfInString, LoginURL, errorcode=1
        {
            reason := "ID,비번 틀림"
			SB_SetText("이유 : " reason)
            Gosub, TryLoginFail
			step = 0
            return
        }
    }

    TryCount := 0
    while !PageInst.Evaluate("document.querySelector('.game_start')")
    {
        Sleep, 300
        TryCount++
        if (TryCount > 100)
        {
            reason := "게임 시작 버튼 없음"
			SB_SetText("이유 : " reason)
            Gosub, TryLoginFail
			step = 0
            return
        }
    }

    Sleep, 2000
    PageInst.Evaluate("document.querySelector('.game_start').click();")

    ; 엘랜시아 실행 대기
    CDP := ChromeInst.CDP
    ;CDP.Call("Page.enable")
    ;CDP.On("Page.javascriptDialogOpening", "HandleDialog")

    WinWait, ahk_exe jElancia.exe, , 15

    Gosub, CleanChrome

    Step = 2
    return
}
}
if(Gui_Login = "넥슨플러그")
{
if(Step = 1)
{
GuiControl, , 로그인상태정보, [로그인] - 접속 중
SetTitleMatchMode,3
IfWinNotExist ahk_exe NexonPlug.exe
{
SB_SetText("넥슨플러그를 확인해 주세요.")
GuiControl, , 로그인상태정보, [로그인] - 시작실패 ( 넥슨플러그를 실행해주세요. )
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
else
{
WinShow, ahk_exe NexonPlug.exe
sleep,1000
WinActivate, ahk_exe NexonPlug.exe
Sleep, 500
IfWinActive ahk_exe NexonPlug.exe
{
CoordMode,mouse,Screen
MouseClick, Left, %게임시작x%, %게임시작y%,1
}
WinWaitActive, ahk_exe Jelancia.exe,,5
if ErrorLevel
{
step = 0
return
}
Sleep,500
IfWinExist ahk_exe Jelancia.exe
{
WinHide, ahk_exe NexonPlug.exe
Step = 2
}
}
}
}
if(Step = 2)
{
Sleep,3000
GuiControl, , 로그인상태정보, [로그인] - 실행중
WinKill, ahk_exe MRMSPH.exe
pwb.document.querySelector("[alt='게임시작']").click()
pwb.quit
Step = 3
}
if(Step = 3)
{
WINWAIT, ahk_exe jElancia.exe, , 15
Sleep, 1000
ControlGetText, Patch, Static2, Elancia
Sleep, 1000
IfInString,Patch,일랜시아 서버에 연결할 수 없습니다.
{
WinKill, Elancia
WinKill, ahk_exe MRMSPH.exe
Step = 10000
return
}
ControlGet, GameStartButton, Visible, , Button1, Elancia
Sleep, 1000
if(GameStartButton = 1)
{
Sleep, 1000
GuiControl, , 로그인상태정보, 메모리 설정 적용중
WinGet, jPID, PID, Elancia
jelan := new _ClassMemory("ahk_pid " 0, "", hProcessCopy)
jelan := new _ClassMemory("ahk_pid " jPID, "", hProcessCopy)
jelan.write(0x00415AB6, 0x2B, "Char", aOffsets*)
jelan.write(0x00415AE7, 0x94, "Char", aOffsets*)
jelan.write(0x00460813, 0xE9, "Char", aOffsets*)
jelan.write(0x00460814, 0xE8, "Char", aOffsets*)
jelan.write(0x00460815, 0xF6, "Char", aOffsets*)
jelan.write(0x00460816, 0x12, "Char", aOffsets*)
jelan.write(0x00460817, 0x00, "Char", aOffsets*)
jelan.write(0x004CFBC5, 0xB2, "Char", aOffsets*)
jelan.write(0x004D05CD, 0xB2, "Char", aOffsets*)
jelan.write(0x0047C1A9, 0x6A, "Char", aOffsets*)
jelan.write(0x0047C1AA, 0x00, "Char", aOffsets*)
jelan.write(0x0047C1AB, 0x90, "Char", aOffsets*)
jelan.write(0x0047C1AC, 0x90, "Char", aOffsets*)
jelan.write(0x0047C1AD, 0x90, "Char", aOffsets*)
jelan.write(0x0046035B, 0x90, "Char", aOffsets*)
jelan.write(0x0046035C, 0x90, "Char", aOffsets*)
jelan.write(0x0046035D, 0x90, "Char", aOffsets*)
jelan.write(0x0046035E, 0x90, "Char", aOffsets*)
jelan.write(0x0046035F, 0x90, "Char", aOffsets*)
jelan.write(0x00460360, 0x90, "Char", aOffsets*)
jelan.write(0x0047A18D, 0xEB, "Char", aOffsets*)
jelan.write(0x0047AA20, 0xEB, "Char", aOffsets*)
jelan.write(0x004C3DC2, 0x03, "Char", aOffsets*)
jelan.write(0x0047AD18, 0xEB, "Char", aOffsets*)
jelan.write(0x0047C17B, 0x75, "Char", aOffsets*)
jelan.write(0x0047C17C, 0x19, "Char", aOffsets*)
jelan.write(0x0047C17D, 0x85, "Char", aOffsets*)
jelan.write(0x0047C17E, 0xFF, "Char", aOffsets*)
jelan.write(0x0047C17F, 0x75, "Char", aOffsets*)
jelan.write(0x00441B66, 0x90, "Char", aOffsets*)
jelan.write(0x00441B67, 0x90, "Char", aOffsets*)
jelan.write(0x00441B68, 0x90, "Char", aOffsets*)
jelan.write(0x00441B69, 0x90, "Char", aOffsets*)
jelan.write(0x00441B6A, 0x90, "Char", aOffsets*)
jelan.write(0x00441B6B, 0x90, "Char", aOffsets*)
jelan.write(0x004A1A7E, 0xE9, "Char", aOffsets*)
jelan.write(0x004A1A7F, 0x7D, "Char", aOffsets*)
jelan.write(0x004A1A80, 0xE5, "Char", aOffsets*)
jelan.write(0x004A1A81, 0x0E, "Char", aOffsets*)
jelan.write(0x004A1A82, 0x00, "Char", aOffsets*)
jelan.write(0x004766E0, 0x90, "Char", aOffsets*)
jelan.write(0x004766E1, 0x90, "Char", aOffsets*)
jelan.write(0x004766E2, 0x90, "Char", aOffsets*)
jelan.write(0x004766E3, 0x90, "Char", aOffsets*)
jelan.write(0x004766E4, 0x90, "Char", aOffsets*)
jelan.write(0x004766E5, 0x90, "Char", aOffsets*)
jelan.write(0x0042483A, 0xB0, "Char", aOffsets*)
jelan.write(0x0042483B, 0x01, "Char", aOffsets*)
jelan.write(0x0042483C, 0x90, "Char", aOffsets*)
jelan.write(0x0042483D, 0x90, "Char", aOffsets*)
jelan.write(0x0042483E, 0x90, "Char", aOffsets*)
jelan.write(0x0042483F, 0x90, "Char", aOffsets*)
jelan.write(0x00436071, 0xEB, "Char", aOffsets*)
jelan.write(0x0042304D, 0xEB, "Char", aOffsets*)
jelan.write(0x0042304B, 0x90, "Char", aOffsets*)
jelan.write(0x0042304C, 0x90, "Char", aOffsets*)
jelan.write(0x00423073, 0x90, "Char", aOffsets*)
jelan.write(0x00423074, 0x90, "Char", aOffsets*)
jelan.write(0x00590000, 0xE8, "Char", aOffsets*)
jelan.write(0x00590001, 0x73, "Char", aOffsets*)
jelan.write(0x00590002, 0xD5, "Char", aOffsets*)
jelan.write(0x00590003, 0xF7, "Char", aOffsets*)
jelan.write(0x00590004, 0xFF, "Char", aOffsets*)
jelan.write(0x00590005, 0x60, "Char", aOffsets*)
jelan.write(0x00590006, 0xA1, "Char", aOffsets*)
jelan.write(0x00590007, 0xE8, "Char", aOffsets*)
jelan.write(0x00590008, 0xEA, "Char", aOffsets*)
jelan.write(0x00590009, 0x58, "Char", aOffsets*)
jelan.write(0x0059000A, 0x00, "Char", aOffsets*)
jelan.write(0x0059000B, 0xBB, "Char", aOffsets*)
jelan.write(0x0059000C, 0x36, "Char", aOffsets*)
jelan.write(0x0059000D, 0x00, "Char", aOffsets*)
jelan.write(0x0059000E, 0x00, "Char", aOffsets*)
jelan.write(0x0059000F, 0x00, "Char", aOffsets*)
jelan.write(0x00590010, 0xB9, "Char", aOffsets*)
jelan.write(0x00590011, 0x00, "Char", aOffsets*)
jelan.write(0x00590012, 0x00, "Char", aOffsets*)
jelan.write(0x00590013, 0x00, "Char", aOffsets*)
jelan.write(0x00590014, 0x00, "Char", aOffsets*)
jelan.write(0x00590015, 0xBA, "Char", aOffsets*)
jelan.write(0x00590016, 0x88, "Char", aOffsets*)
jelan.write(0x00590017, 0x05, "Char", aOffsets*)
jelan.write(0x00590018, 0x07, "Char", aOffsets*)
jelan.write(0x00590019, 0x00, "Char", aOffsets*)
jelan.write(0x0059001A, 0xBE, "Char", aOffsets*)
jelan.write(0x0059001B, 0x01, "Char", aOffsets*)
jelan.write(0x0059001C, 0x00, "Char", aOffsets*)
jelan.write(0x0059001D, 0x07, "Char", aOffsets*)
jelan.write(0x0059001E, 0x00, "Char", aOffsets*)
jelan.write(0x0059001F, 0x8B, "Char", aOffsets*)
jelan.write(0x00590020, 0xF8, "Char", aOffsets*)
jelan.write(0x00590021, 0xFF, "Char", aOffsets*)
jelan.write(0x00590022, 0x15, "Char", aOffsets*)
jelan.write(0x00590023, 0xFC, "Char", aOffsets*)
jelan.write(0x00590024, 0x83, "Char", aOffsets*)
jelan.write(0x00590025, 0x52, "Char", aOffsets*)
jelan.write(0x00590026, 0x00, "Char", aOffsets*)
jelan.write(0x00590027, 0x50, "Char", aOffsets*)
jelan.write(0x00590028, 0x8B, "Char", aOffsets*)
jelan.write(0x00590029, 0xC6, "Char", aOffsets*)
jelan.write(0x0059002A, 0xC1, "Char", aOffsets*)
jelan.write(0x0059002B, 0xE8, "Char", aOffsets*)
jelan.write(0x0059002C, 0x1E, "Char", aOffsets*)
jelan.write(0x0059002D, 0x25, "Char", aOffsets*)
jelan.write(0x0059002E, 0x01, "Char", aOffsets*)
jelan.write(0x0059002F, 0xFF, "Char", aOffsets*)
jelan.write(0x00590030, 0xFF, "Char", aOffsets*)
jelan.write(0x00590031, 0xFF, "Char", aOffsets*)
jelan.write(0x00590032, 0x50, "Char", aOffsets*)
jelan.write(0x00590033, 0x88, "Char", aOffsets*)
jelan.write(0x00590034, 0xD9, "Char", aOffsets*)
jelan.write(0x00590035, 0x8B, "Char", aOffsets*)
jelan.write(0x00590036, 0xD7, "Char", aOffsets*)
jelan.write(0x00590037, 0xE8, "Char", aOffsets*)
jelan.write(0x00590038, 0x9F, "Char", aOffsets*)
jelan.write(0x00590039, 0xA8, "Char", aOffsets*)
jelan.write(0x0059003A, 0xEB, "Char", aOffsets*)
jelan.write(0x0059003B, 0xFF, "Char", aOffsets*)
jelan.write(0x0059003C, 0x61, "Char", aOffsets*)
jelan.write(0x0059003D, 0xC3, "Char", aOffsets*)
jelan.write(0x00590300, 0x3D, "Char", aOffsets*)
jelan.write(0x00590301, 0x62, "Char", aOffsets*)
jelan.write(0x00590302, 0x04, "Char", aOffsets*)
jelan.write(0x00590303, 0x00, "Char", aOffsets*)
jelan.write(0x00590304, 0x00, "Char", aOffsets*)
jelan.write(0x00590305, 0x74, "Char", aOffsets*)
jelan.write(0x00590306, 0x28, "Char", aOffsets*)
jelan.write(0x00590307, 0x0F, "Char", aOffsets*)
jelan.write(0x00590308, 0x1F, "Char", aOffsets*)
jelan.write(0x00590309, 0x40, "Char", aOffsets*)
jelan.write(0x0059030A, 0x00, "Char", aOffsets*)
jelan.write(0x0059030B, 0x3D, "Char", aOffsets*)
jelan.write(0x0059030C, 0x64, "Char", aOffsets*)
jelan.write(0x0059030D, 0x04, "Char", aOffsets*)
jelan.write(0x0059030E, 0x00, "Char", aOffsets*)
jelan.write(0x0059030F, 0x00, "Char", aOffsets*)
jelan.write(0x00590310, 0x74, "Char", aOffsets*)
jelan.write(0x00590311, 0x30, "Char", aOffsets*)
jelan.write(0x00590312, 0x0F, "Char", aOffsets*)
jelan.write(0x00590313, 0x1F, "Char", aOffsets*)
jelan.write(0x00590314, 0x40, "Char", aOffsets*)
jelan.write(0x00590315, 0x00, "Char", aOffsets*)
jelan.write(0x00590316, 0x3D, "Char", aOffsets*)
jelan.write(0x00590317, 0x65, "Char", aOffsets*)
jelan.write(0x00590318, 0x04, "Char", aOffsets*)
jelan.write(0x00590319, 0x00, "Char", aOffsets*)
jelan.write(0x0059031A, 0x00, "Char", aOffsets*)
jelan.write(0x0059031B, 0x74, "Char", aOffsets*)
jelan.write(0x0059031C, 0x38, "Char", aOffsets*)
jelan.write(0x0059031D, 0x0F, "Char", aOffsets*)
jelan.write(0x0059031E, 0x1F, "Char", aOffsets*)
jelan.write(0x0059031F, 0x40, "Char", aOffsets*)
jelan.write(0x00590320, 0x00, "Char", aOffsets*)
jelan.write(0x00590321, 0x0F, "Char", aOffsets*)
jelan.write(0x00590322, 0xB6, "Char", aOffsets*)
jelan.write(0x00590323, 0x4E, "Char", aOffsets*)
jelan.write(0x00590324, 0x0C, "Char", aOffsets*)
jelan.write(0x00590325, 0x50, "Char", aOffsets*)
jelan.write(0x00590326, 0xFF, "Char", aOffsets*)
jelan.write(0x00590327, 0x74, "Char", aOffsets*)
jelan.write(0x00590328, 0x24, "Char", aOffsets*)
jelan.write(0x00590329, 0x3C, "Char", aOffsets*)
jelan.write(0x0059032A, 0xE9, "Char", aOffsets*)
jelan.write(0x0059032B, 0x00, "Char", aOffsets*)
jelan.write(0x0059032C, 0xB0, "Char", aOffsets*)
jelan.write(0x0059032D, 0xEE, "Char", aOffsets*)
jelan.write(0x0059032E, 0xFF, "Char", aOffsets*)
jelan.write(0x0059032F, 0x0F, "Char", aOffsets*)
jelan.write(0x00590330, 0xB6, "Char", aOffsets*)
jelan.write(0x00590331, 0x4E, "Char", aOffsets*)
jelan.write(0x00590332, 0x0C, "Char", aOffsets*)
jelan.write(0x00590333, 0xB8, "Char", aOffsets*)
jelan.write(0x00590334, 0x63, "Char", aOffsets*)
jelan.write(0x00590335, 0x04, "Char", aOffsets*)
jelan.write(0x00590336, 0x00, "Char", aOffsets*)
jelan.write(0x00590337, 0x00, "Char", aOffsets*)
jelan.write(0x00590338, 0x50, "Char", aOffsets*)
jelan.write(0x00590339, 0xFF, "Char", aOffsets*)
jelan.write(0x0059033A, 0x74, "Char", aOffsets*)
jelan.write(0x0059033B, 0x24, "Char", aOffsets*)
jelan.write(0x0059033C, 0x3C, "Char", aOffsets*)
jelan.write(0x0059033D, 0xE9, "Char", aOffsets*)
jelan.write(0x0059033E, 0xED, "Char", aOffsets*)
jelan.write(0x0059033F, 0xAF, "Char", aOffsets*)
jelan.write(0x00590340, 0xEE, "Char", aOffsets*)
jelan.write(0x00590341, 0xFF, "Char", aOffsets*)
jelan.write(0x00590342, 0x0F, "Char", aOffsets*)
jelan.write(0x00590343, 0xB6, "Char", aOffsets*)
jelan.write(0x00590344, 0x4E, "Char", aOffsets*)
jelan.write(0x00590345, 0x0C, "Char", aOffsets*)
jelan.write(0x00590346, 0xB8, "Char", aOffsets*)
jelan.write(0x00590347, 0x63, "Char", aOffsets*)
jelan.write(0x00590348, 0x04, "Char", aOffsets*)
jelan.write(0x00590349, 0x00, "Char", aOffsets*)
jelan.write(0x0059034A, 0x00, "Char", aOffsets*)
jelan.write(0x0059034B, 0x50, "Char", aOffsets*)
jelan.write(0x0059034C, 0xFF, "Char", aOffsets*)
jelan.write(0x0059034D, 0x74, "Char", aOffsets*)
jelan.write(0x0059034E, 0x24, "Char", aOffsets*)
jelan.write(0x0059034F, 0x3C, "Char", aOffsets*)
jelan.write(0x00590350, 0xE9, "Char", aOffsets*)
jelan.write(0x00590351, 0xDA, "Char", aOffsets*)
jelan.write(0x00590352, 0xAF, "Char", aOffsets*)
jelan.write(0x00590353, 0xEE, "Char", aOffsets*)
jelan.write(0x00590354, 0xFF, "Char", aOffsets*)
jelan.write(0x00590355, 0x0F, "Char", aOffsets*)
jelan.write(0x00590356, 0xB6, "Char", aOffsets*)
jelan.write(0x00590357, 0x4E, "Char", aOffsets*)
jelan.write(0x00590358, 0x0C, "Char", aOffsets*)
jelan.write(0x00590359, 0xB8, "Char", aOffsets*)
jelan.write(0x0059035A, 0x63, "Char", aOffsets*)
jelan.write(0x0059035B, 0x04, "Char", aOffsets*)
jelan.write(0x0059035C, 0x00, "Char", aOffsets*)
jelan.write(0x0059035D, 0x00, "Char", aOffsets*)
jelan.write(0x0059035E, 0x50, "Char", aOffsets*)
jelan.write(0x0059035F, 0xFF, "Char", aOffsets*)
jelan.write(0x00590360, 0x74, "Char", aOffsets*)
jelan.write(0x00590361, 0x24, "Char", aOffsets*)
jelan.write(0x00590362, 0x3C, "Char", aOffsets*)
jelan.write(0x00590363, 0xE9, "Char", aOffsets*)
jelan.write(0x00590364, 0xC7, "Char", aOffsets*)
jelan.write(0x00590365, 0xAF, "Char", aOffsets*)
jelan.write(0x00590366, 0xEE, "Char", aOffsets*)
jelan.write(0x00590367, 0xFF, "Char", aOffsets*)
jelan.write(0x0047B326, 0xE9, "Char", aOffsets*)
jelan.write(0x0047B327, 0xD5, "Char", aOffsets*)
jelan.write(0x0047B328, 0x4F, "Char", aOffsets*)
jelan.write(0x0047B329, 0x11, "Char", aOffsets*)
jelan.write(0x0047B32A, 0x00, "Char", aOffsets*)
if(Gui_jjOFF = 1)
{
jelan.write(0x0047B3EC, 0x4D, "Char", aOffsets*)
}
MIC()
ATKM()
sleep,1
Loop,5
{
ControlSend, , {Enter}, Elancia
}
Step = 4
}
}
if(Step = 4)
{
Sleep, 2000
GuiControl, , 로그인상태정보, [로그인] - 서버 선택 중
WinGetTitle, jTitle, ahk_pid %jPID%
IfWinNotExist,ahk_pid %jPID%
{
GuiControl, , 로그인상태정보, 오류로 인해 재접속 합니다.
Step = 0
}
if(jTitle = "일랜시아")
{
WinMove, ahk_pid %jPID%, , 0,0
Server := jelan.read(0x0058DAD0, "UChar", 0xC, 0x8, 0x8, 0x6C)
if(Server = 0)
{
Sleep, 1500
if(Gui_Server = "엘")
{
PostMessage, 0x200, 0, 16187689, , ahk_pid %jPID%
PostMessage, 0x201, 1, 16187689, , ahk_pid %jPID%
PostMessage, 0x202, 0, 16187689, , ahk_pid %jPID%
Sleep, 100
PostMessage, 0x100, 13, 1835009, , ahk_pid %jPID%
PostMessage, 0x101, 13, 1835009, , ahk_pid %jPID%
}
if(Gui_Server = "테스")
{
PostMessage, 0x200, 0, 17826096, , ahk_pid %jPID%
PostMessage, 0x201, 1, 17826096, , ahk_pid %jPID%
PostMessage, 0x202, 0, 17826096, , ahk_pid %jPID%
Sleep, 100
PostMessage, 0x100, 13, 1835009, , ahk_pid %jPID%
PostMessage, 0x101, 13, 1835009, , ahk_pid %jPID%
}
Step = 5
}
}
if(jTitle = "Elancia")
{
ControlClick, Button1, ahk_pid %jPID%
Sleep, 100
}
}
if(Step = 5)
{
GuiControl, , 로그인상태정보, [로그인] - 캐릭터 선택 중
WinGetTitle, jTitle, ahk_pid %jPID%
if(jTitle = "일랜시아 - 엘" or jTitle = "일랜시아 - 테스")
{
Server := jelan.read(0x0058DAD0, "UChar", 0xC, 0x8, 0x8, 0x6C)
if(Server = 1)
{
Sleep, 1500
if(Gui_CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %jPID%
PostMessage, 0x201, 1, 13107662, , ahk_pid %jPID%
PostMessage, 0x202, 0, 13107662, , ahk_pid %jPID%
}
if(Gui_CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %jPID%
PostMessage, 0x201, 1, 14287311, , ahk_pid %jPID%
PostMessage, 0x202, 0, 14287311, , ahk_pid %jPID%
}
if(Gui_CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %jPID%
PostMessage, 0x201, 1, 15598030, , ahk_pid %jPID%
PostMessage, 0x202, 0, 15598030, , ahk_pid %jPID%
}
if(Gui_CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %jPID%
PostMessage, 0x201, 1, 16908752, , ahk_pid %jPID%
PostMessage, 0x202, 0, 16908752, , ahk_pid %jPID%
}
if(Gui_CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %jPID%
PostMessage, 0x201, 1, 18088402, , ahk_pid %jPID%
PostMessage, 0x202, 0, 18088402, , ahk_pid %jPID%
}
if(Gui_CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %jPID%
PostMessage, 0x201, 1, 19399121, , ahk_pid %jPID%
PostMessage, 0x202, 0, 19399121, , ahk_pid %jPID%
}
if(Gui_CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %jPID%
PostMessage, 0x201, 1, 20513232, , ahk_pid %jPID%
PostMessage, 0x202, 0, 20513232, , ahk_pid %jPID%
}
if(Gui_CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %jPID%
PostMessage, 0x201, 1, 21889488, , ahk_pid %jPID%
PostMessage, 0x202, 0, 21889488, , ahk_pid %jPID%
}
if(Gui_CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %jPID%
PostMessage, 0x201, 1, 23200209, , ahk_pid %jPID%
PostMessage, 0x202, 0, 23200209, , ahk_pid %jPID%
}
if(Gui_CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %jPID%
PostMessage, 0x201, 1, 24314321, , ahk_pid %jPID%
PostMessage, 0x202, 0, 24314321, , ahk_pid %jPID%
}
Sleep, 100
PostMessage, 0x200, 0, 22086223, , ahk_pid %jPID%
PostMessage, 0x201, 1, 22086223, , ahk_pid %jPID%
PostMessage, 0x202, 0, 22086223, , ahk_pid %jPID%
Sleep, 3000
Step = 6
}
}
}
if(Step = 6)
{
GuiControl, , 로그인상태정보, [로그인] - 접속 대기 중
Sleep, 7000
Server := jelan.read(0x0058DAD0, "UChar", 0xC, 0x10, 0x8, 0x36C)
if(Server = 1)
{
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
GuiControl, , 로그인상태정보, 접속 오류로 대기 후 재시작 합니다.
Sleep, 60000
Step = 10000
return
}
ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16", aOffsets*)
IfInString,ServerMsg,서버와의 연결이
{
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
Step = 10000
return
}
WinGetTitle, jTitle, ahk_pid %jPID%
if(jTitle != "일랜시아" and jTitle != "일랜시아 - 엘" and jTitle != "일랜시아 - 테스")
{
GuiControl, , 로그인상태정보, [로그인] - 접속 완료
GuiControl, , Gui_CharName, %jTitle%
GuiControl,Show,Gui_StopButton
SB_SetText("접속 완료")
Sleep, 100
초기마을이동 := 1
Sleep, 300
Step = 7
}
}
if(Step = 7 and 초기마을이동 = 1)
{
GuiControl, , Gui_NowState, 마을로 이동.
SB_SetText("마을로 라깃이동")
Gosub, 차원체크
Check_Map()
sleep,500
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 250
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 250
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
초기마을이동 := 2
Sleep,5000
return
}
if(CountPortal = 1)
{
PostClick(645,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
초기마을이동 := 2
Sleep,5000
return
}
if(CountPortal = 2)
{
PostClick(660,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
초기마을이동 := 2
Sleep,5000
return
}
}
}
if(Step = 7 and 초기마을이동 = 2)
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
WinActivate,ahk_pid %jPID%
GuiControl, , Gui_NowState, 구동을 위한 메모리 적용 중.
SB_SetText("초기 세팅 중")
빵 := 0
포북시작 := 0
초기마을이동 := 3
ActiveAscript1()
ActiveAscript2()
ActiveAscript3()
ActiveAscript4()
ActiveAscript5()
ActiveAscript6()
ActiveAscript7()
ActiveAscript8()
ActiveAscript9()
ActiveAscript10()
ActiveAscript11()
ActiveAscript12()
WriteExecutableMemory("좌표이동")
sleep,1
WriteExecutableMemory("퀵슬롯사용")
sleep,1
WriteExecutableMemory("NPC호출용1")
WriteExecutableMemory("NPC호출용2")
sleep,1
WriteExecutableMemory("타겟스킬사용")
sleep,1
WriteExecutableMemory("타겟스킬호출")
sleep,1
WriteExecutableMemory("마법모션제거")
sleep,1
WriteExecutableMemory("공속")
sleep,1
Run,*RunAs %A_ScriptDir%\MRMSPH.exe
WinWait, ahk_exe MRMSPH.exe,,15
Sleep, 1000
IfWinExist, ahk_exe MRMSPH.exe
{
WinHide, ahk_exe MRMSPH.exe
}
if(Gui_jjOn = 1)
{
JJscript()
SetTimer, incineration, 250
}
Gosub, 어빌리티탭확인
Sleep,200
아이템읽어오기()
Sleep,200
어빌리티읽어오기()
Sleep,200
마법읽어오기()
Sleep,200
기술읽어오기()
Sleep,200
포프OID()
Sleep,200
WPdisablescript()
incineratescript()
SetFormat, integer, h
AbilityADD := jelan.processPatternScan(, 0x7FFFFFFF, 0xB0, 0x62, 0x53, 0x00, 0x01, 0x03, 0x00)
AbilityNameADD := AbilityADD + 0x64
AbilityValueADD := AbilityADD + 0x264
SetFormat, integer, d
Sleep, 100
if(Gui_1Muba = 1 or Gui_2ButMuba = 1)
{
if(Gui_2ButMuba = 1)
{
BWValue0 := ReadAbility("격투")
}
BWValue1 := ReadAbility(Gui_Weapon1)
}
if(Gui_2Muba = 1 or Gui_3ButMuba = 1)
{
if(Gui_3ButMuba = 1)
{
BWValue0 := ReadAbility("격투")
}
BWValue1 := ReadAbility(Gui_Weapon1)
BWValue2 := ReadAbility(Gui_Weapon2)
}
if(Gui_3Muba = 1 or Gui_4ButMuba = 1)
{
if(Gui_4ButMuba = 1)
{
BWValue0 := ReadAbility("격투")
}
BWValue1 := ReadAbility(Gui_Weapon1)
BWValue2 := ReadAbility(Gui_Weapon2)
BWValue3 := ReadAbility(Gui_Weapon3)
}
IfWinNotActive, ahk_pid %jPID%
{
WinActivate, ahk_pid %jPID%
}
if(FirstCheck = 1)
{
if(Regloady = 0)
{
Get_HP()
CheckFirstHP := MaxHP
ProgramStartTime := A_TickCount
}
if(Regloady = 1)
{
CheckFirstHP := RCFH
ProgramStartTime := RPST
}
FirstCheck = 0
}
Send, !m
Sleep, 1000
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
WinActivate,ahk_pid %jPID%
Sleep, 100
Send, flshdk{Space}apsb{Tab}zldk{Space}apsb{Tab}znzl{Space}apsb{Tab}zmfhfltm{Space}apsb{Tab}flshtm{Space}apsb{Tab}emrhf{Space}apsb{Tab}wlrdjq{Tab}dlxm{Space}apsb{Enter}
Sleep, 300
Check_Inven()
if(Inven = 1)
{
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 73, 1507329, , ahk_pid %jPID%
PostMessage, 0x101, 73, 1507329, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 100
Move_Inven()
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 73, 1507329, , ahk_pid %jPID%
PostMessage, 0x101, 73, 1507329, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
}
if(Inven = 0)
{
Move_Inven()
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 73, 1507329, , ahk_pid %jPID%
PostMessage, 0x101, 73, 1507329, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
}
if(Mount = 0)
{
PostMessage, 0x100, 56, 589825, , ahk_pid %jPID%
PostMessage, 0x101, 56, 589825, , ahk_pid %jPID%
Sleep, 100
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
Step = 8
}
if(Step = 8)
{
GuiControl, , Gui_NowState, 구동 초기 세팅 중. ( 체잠장소 설정 )
SB_SetText("체작장소 세팅 중")
if(Gui_CheckUseMagic = 1)
{
Send, {F17 Down}
Sleep, 200
Send, {F17 UP}
Send, {F17 Down}
Sleep, 200
Send, {F17 UP}
value := jelan.write(0x00527A4C, 3, "UInt")
Stack_MN()
CritHP := Gui_CHP
Crit_HM()
jelan.write(0x004CB504, 0xE9, "Char", aOffsets*)
jelan.write(0x004CB505, 0xF7, "Char", aOffsets*)
jelan.write(0x004CB506, 0xC8, "Char", aOffsets*)
jelan.write(0x004CB507, 0x05, "Char", aOffsets*)
jelan.write(0x004CB508, 0x00, "Char", aOffsets*)
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
if(Gui_HuntAuto = 1)
{
if(Gui_1Muba = 1)
{
BWValue1 := ReadAbility(Gui_Weapon1)
if(BWValue1 >= Gui_LimitAbility1)
{
HuntPlace = 2
Step = 1000
}
if(BWValue1 < Gui_LimitAbility1)
{
HuntPlace = 1
Step = 9
}
}
if(Gui_2Muba = 1)
{
BWValue1 := ReadAbility(Gui_Weapon1)
BWValue2 := ReadAbility(Gui_Weapon2)
if(BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2)
{
HuntPlace = 2
Step = 1000
}
if(BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2)
{
HuntPlace = 1
Step = 9
}
}
if(Gui_3Muba = 1)
{
BWValue1 := ReadAbility(Gui_Weapon1)
BWValue2 := ReadAbility(Gui_Weapon2)
BWValue3 := ReadAbility(Gui_Weapon3)
if(BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2 or BWValue3 >= Gui_LimitAbility3)
{
HuntPlace = 2
Step = 1000
}
if(BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2 and BWValue3 < Gui_LimitAbility3)
{
HuntPlace = 1
Step = 9
}
}
if(Gui_2ButMuba = 1)
{
BWValue0 := ReadAbility("격투")
BWValue1 := ReadAbility(Gui_Weapon1)
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1)
{
HuntPlace = 2
Step = 1000
}
if(BWValue0 < Gui_LimitAbility0 and BWValue1 < Gui_LimitAbility1)
{
HuntPlace = 1
Step = 9
}
}
if(Gui_3ButMuba = 1)
{
BWValue0 := ReadAbility("격투")
BWValue1 := ReadAbility(Gui_Weapon1)
BWValue2 := ReadAbility(Gui_Weapon2)
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1 or  BWValue2 >= Gui_LimitAbility2)
{
HuntPlace = 2
Step = 1000
}
if(BWValue0 < Gui_LimitAbility0 and BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2)
{
HuntPlace = 1
Step = 9
}
}
if(Gui_4ButMuba = 1)
{
BWValue0 := ReadAbility("격투")
BWValue1 := ReadAbility(Gui_Weapon1)
BWValue2 := ReadAbility(Gui_Weapon2)
BWValue3 := ReadAbility(Gui_Weapon3)
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1 or  BWValue2 >= Gui_LimitAbility2 or BWValue3 >= Gui_LimitAbility3)
{
HuntPlace = 2
Step = 1000
}
if(BWValue0 < Gui_LimitAbility0 and BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2 and BWValue3 < Gui_LimitAbility3)
{
HuntPlace = 1
Step = 9
}
}
}
if(Gui_HuntPonam = 1)
{
if(Gui_1Muba = 1 or Gui_2ButMuba = 1)
{
if(Gui_2ButMuba = 1)
{
BWValue0 := ReadAbility("격투")
}
BWValue1 := ReadAbility(Gui_Weapon1)
}
if(Gui_2Muba = 1 or Gui_3ButMuba = 1)
{
if(Gui_3ButMuba = 1)
{
BWValue0 := ReadAbility("격투")
}
BWValue1 := ReadAbility(Gui_Weapon1)
BWValue2 := ReadAbility(Gui_Weapon2)
}
if(Gui_3Muba = 1 or Gui_4ButMuba = 1)
{
if(Gui_4ButMuba = 1)
{
BWValue0 := ReadAbility("격투")
}
BWValue1 := ReadAbility(Gui_Weapon1)
BWValue2 := ReadAbility(Gui_Weapon2)
BWValue3 := ReadAbility(Gui_Weapon3)
}
HuntPlace = 1
Step = 9
}
if(Gui_HuntPobuk = 1)
{
if(Gui_1Muba = 1 or Gui_2ButMuba = 1)
{
if(Gui_2ButMuba = 1)
{
BWValue0 := ReadAbility("격투")
}
BWValue1 := ReadAbility(Gui_Weapon1)
}
if(Gui_2Muba = 1 or Gui_3ButMuba = 1)
{
if(Gui_3ButMuba = 1)
{
BWValue0 := ReadAbility("격투")
}
BWValue1 := ReadAbility(Gui_Weapon1)
BWValue2 := ReadAbility(Gui_Weapon2)
}
if(Gui_3Muba = 1 or Gui_4ButMuba = 1)
{
if(Gui_4ButMuba = 1)
{
BWValue0 := ReadAbility("격투")
}
BWValue1 := ReadAbility(Gui_Weapon1)
BWValue2 := ReadAbility(Gui_Weapon2)
BWValue3 := ReadAbility(Gui_Weapon3)
}
HuntPlace = 2
Step = 1000
}
sleep,500
}
if(Step = 9 and 초기마을이동 = 3)
{
GuiControl, , Gui_NowState, [포남] 사냥터로 가기.
SB_SetText("사냥터로 이동 시작")
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Step = 10
초기마을이동 := 4
}
if(Step = 9 and 초기마을이동 = 4)
{
GuiControl, , Gui_NowState, [포남] 사냥터로 가기.
SB_SetText("사냥터로 이동 시작")
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Gosub, 차원체크
Check_Map()
sleep,500
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 250
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 250
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 10
Sleep,1000
return
}
if(CountPortal = 1)
{
PostClick(645,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 10
Sleep,1000
return
}
if(CountPortal = 2)
{
PostClick(660,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 10
Sleep,1000
return
}
}
}
if(Step = 10)
{
SB_SetText("차원 확인 중")
if(Gui_CheckUseMagic = 1)
{
Send, {F17 Down}
Sleep, 200
Send, {F17 UP}
Send, {F17 Down}
Sleep, 200
Send, {F17 UP}
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
Get_Location()
IfInString,Location,[알파차원] 포프레스네 마을
{
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
Step = 11
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
IfInString,Location,[베타차원] 포프레스네 마을
{
Step = 11
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
IfInString,Location,[감마차원] 포프레스네 마을
{
Step = 11
}
}
if(Step = 11)
{
Mapnumber = 1
GuiControl, , Gui_NowState, [포남] 사냥터로 가기.
SB_SetText("라스의깃 갯수 체크 중")
if(Gui_CheckUseMagic = 1)
{
Send, {F17 Down}
Sleep, 200
Send, {F17 UP}
Send, {F17 Down}
Sleep, 200
Send, {F17 UP}
}
Get_MsgM()
Get_Perfect()
if(라깃카운트 <= 5)
{
Step = 100
}
if(라깃카운트 > 5)
{
Step = 12
}
}
if(Step = 12)
{
SB_SetText("파티 설정 중")
Check_State()
if(State = 1)
{
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 100
}
if(Gui_PartyOff = 1)
{
Move_StateForMount()
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 100
PostDClick(190,310)
Sleep, 100
PostDClick(225,310)
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 100
}
Move_State()
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 500
Check_State()
Check_StatePos()
if(StatePosX = 549 and StatePosY = 644 and State = 1)
{
if(Gui_CheckUseParty = 1)
{
Step = 900
}
if(Gui_CheckUseParty = 0)
{
Step = 13
}
}
}
if(Step = 13)
{
아이템읽어오기()
if( Gui_Grade = 1 )
{
if( Gold <= 1000000 )
{
if(아이템갯수["골드바"] = ""  || 아이템갯수["정령의보석"] = "")
{
GuiControl, , Gui_NowState, [자동그렐]골드바 or 정보 부족
MsgBox,48,자동정지, "골드바 or 정보를 채운 후 재시작 버튼을 눌러주세요." ,
SB_SetText("자동그레이드 [ 골드바 or 정보를 채워주세요. ]")
gosub, 일시정지
return
}
Step = 500
return
}
}
GuiControl, , Gui_NowState, [포남] FP체크 중
Get_FP()
절반FP := MaxFP/2
if(NowFP < 절반FP)
{
SB_SetText("FP 확인 중")
Sleep, 200
Step = 201
return
}
if(Gui_CheckUseParty = 1)
{
party()
}
if(Gui_KON = 1)
{
GuiControl, , Gui_NowState, [포남] 사냥터로 가기.
SB_SetText("원격대화 시도 중")
Sleep, 1000
}
if(Gui_KON = 0 || 차원이동감응 = 1)
{
GuiControl, , Gui_NowState, [포남] 사냥터로 가기.
IfInString,Location,남쪽
{
    SB_SetText("현재 남쪽 위치함. 마을로 가기")
    keyclick("tab")
    Step = 9
    초기마을이동 = 4
    return
}
GuiControl, , Gui_NowState, [포남] 사냥터로 가기.
SB_SetText("포남으로 이동 중")
좌표입력(122,184,1)
RunMemory("좌표이동")
Sleep, 3000
}
Step = 14
}
if(Step = 14)
{
if(Gui_KON = 0 || 차원이동감응 = 1)
{
SB_SetText("움직임 체크 중")
Check_Moving()
if(Moving = 0)
{
Sleep, 300
Check_Moving()
if(Moving = 0)
{
AltR()
Sleep, 300
}
}
}
Step = 15
}
if(Step = 15)
{
SB_SetText("현재 위치 체크 중")
Get_Pos()
if(Gui_KON = 1)
{
if(PosY > 180)
{
Step = 9
}
if(PosX >= 32 and PosX <= 174 and PosY >= 15 and PosY <= 180)
{
Step = 16
}
else
{
Step = 13
}
}
if(Gui_KON = 0 || 차원이동감응 = 1)
{
좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
좌표Z := jelan.read(0x0058DAD4, "UInt", 0x18)
if( 좌표X = 122 && 좌표Y = 184 && 좌표Z = 1)
{
Step = 16
}
else
{
Step = 13
}
}
}
if(Step = 16)
{
SB_SetText("갈리드 체크 중")
if(Gui_Grade = 1)
{
Get_Gold()
if(Gold < 100000)
{
Step = 500
}
else
{
Step = 17
}
}
else
{
Step = 17
}
}
if(Step = 17)
{
캐릭제거()
Get_Location()
SB_SetText("원격대화 시도 중")
Move_NPCTalkForm()
callid = 1
if(Gui_KON = 1)
{
IfInString,Location,알파
{
value := jelan.write(0x00527B1C, A리노아, "UInt")
Sleep, 50
value := jelan.write(0x00527B1C, A리노아, "UInt")
Sleep, 50
value := jelan.write(0x00527B1C, A리노아, "UInt")
Send, {F14}
Sleep, 100
Send, {F14}
Sleep, 100
if( A리노아 = 0x0 )
{
    SB_SetText("해당 차원 감응조정")
    차원이동감응 := 1
    GuiControl, ,Gui_KOFF, 1
}
ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16", aOffsets*)
if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
{
A리노아 := 0x0
A동파 := 0x0
A서파 := 0x0
A길잃파 := 0x0
차원이동감응 := 1
GuiControl, ,Gui_KOFF, 1
OID저장()
return
}
호출대상 := "알파 - 리노아"
}
IfInString,Location,베타
{
value := jelan.write(0x00527B1C, B리노아, "UInt")
Sleep, 50
value := jelan.write(0x00527B1C, B리노아, "UInt")
Sleep, 50
value := jelan.write(0x00527B1C, B리노아, "UInt")
Send, {F14}
Sleep, 100
Send, {F14}
Sleep, 100
호출대상 := "베타 - 리노아"
if( B리노아 = 0x0 )
{
    SB_SetText("해당 차원 감응조정")
    차원이동감응 := 1
    GuiControl, ,Gui_KOFF, 1
}
ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16", aOffsets*)
if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
{
B리노아 := 0x0
B동파 := 0x0
B서파 := 0x0
B길잃파 := 0x0
차원이동감응 := 1
GuiControl, ,Gui_KOFF, 1
OID저장()
return
}
}
IfInString,Location,감마
{
value := jelan.write(0x00527B1C, G리노아, "UInt")
Sleep, 50
value := jelan.write(0x00527B1C, G리노아, "UInt")
Sleep, 50
value := jelan.write(0x00527B1C, G리노아, "UInt")
Send, {F14}
Sleep, 100
Send, {F14}
Sleep, 100
호출대상 := "감마 - 리노아"
if(G리노아 = 0x0)
{
    SB_SetText("해당 차원 감응조정")
    차원이동감응 := 1
    GuiControl, ,Gui_KOFF, 1
}
ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16", aOffsets*)
if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
{
G리노아 := 0x0
G동파 := 0x0
G서파 := 0x0
G길잃파 := 0x0
차원이동감응 := 1
GuiControl, ,Gui_KOFF, 1
OID저장()
return
}
}
ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16", aOffsets*)
IfInString,ServerMsg,서버와의 연결이
{
OID리셋()
GuiControl, ,Gui_KOFF, 1
}
}
if(Gui_KON = 0 ||  차원이동감응 = 1)
{
SB_SetText("포남 리노아 감응.")
IfWinNotActive, ahk_pid %jPID%
{
WinActivate, ahk_pid %jPID%
}
PixelSearch, MobX, MobY,  258, 184, 654, 439, 0x7351AD, 2, *Fast  *RGB
if(ErrorLevel = 1)
{
WinMinimize, ahk_exe NexonPlug.exe
Sleep, 200
AltR()
PixelSearch, MobX, MobY,  258, 184, 654, 439, 0x7351AD, 2, *Fast  *RGB
}
if(ErrorLevel = 0)
{
PostClick(MobX,MobY)
}
Sleep, 200
Loop,10
{
    SetFormat, integer, H
    NPCOID := jelan.read(0x00584C2C, "UInt", aOffsets*)
    SetFormat, integer, D
    WriteExecutableMemory("NPC호출용1")
    WriteExecutableMemory("NPC호출용2")
    jelan.write(0x00527b54, NPCOID, "UInt", aOffset*)
    Sleep, 500
    RunMemory("NPC호출")
    Sleep, 500
    SB_SETTEXT(NPCOID "호출", 2)
    Check_FormNumber()
    if (FormNumber = 85)
    {
        break  ; 루프를 멈추고 정상적으로 진행
    }
    if (NPCOID = "0x0000" || NPCOID = "0x0")
    {
        SB_SETTEXT(NPCOID "호출신호없음, 재감응", 1)
        sleep,100
        차원이동감응 := 1
        GuiControl, ,Gui_KOFF, 1
        break  ; 루프를 멈추고 정상적으로 진행
    }
}
if (FormNumber = 85)
{
    Dimension := jelan.read(0x0058EB1C, "UInt", 0x10A)
    if (Dimension > 20000)
    {
        차원 := "감마"
        G리노아 := NPCOID
        GuiControl,, G리노아, %G리노아%
        SB_SETTEXT(차원 . G리노아 "입력완료", 2)
        NPCOID := 0x0
        sleep,100
        Step := 18
    }
    else if (Dimension > 10000)
    {
        차원 := "베타"
        B리노아 := NPCOID
        GuiControl,, B리노아, %B리노아%
        SB_SETTEXT(차원 . B리노아 "입력완료", 2)
        NPCOID := 0x0
        sleep,100
        Step := 18
    }
    else if (Dimension < 10000)
    {
        차원 := "알파"
        A리노아 := NPCOID
        GuiControl,, A리노아, %A리노아%
        SB_SETTEXT(차원 . A리노아 "입력완료", 2)
        NPCOID := 0x0
        sleep,100
        Step := 18
    }
}
Sleep, 500
}
if(Gui_KON = 1)
{
if(ipmak >= 5)
{
GUICONTROL, , Gui_NowState, 리노아 호출오류 감응 OFF
SLEEP, 500
OID리셋()
GUICONTROL, , Gui_KOFF, 1
}
}
NPCTalkedTime := A_TickCount
if(RCC >= 1)
{
Step = 90
}
if(RCC = 0)
{
Step = 18
}
}
if(Step = 18)
{
SB_SetText("포남 입장 시도 중")
Get_Location()
if(Gui_KON = 1)
{
IfInString,Location,[알파차원]
{
value := jelan.write(0x00527B1C, A동파, "UInt")
Sleep, 50
value := jelan.write(0x00527B1C, A동파, "UInt")
Sleep, 50
}
IfInString,Location,[베타차원]
{
value := jelan.write(0x00527B1C, B동파, "UInt")
Sleep, 50
value := jelan.write(0x00527B1C, B동파, "UInt")
Sleep, 50
}
IfInString,Location,[감마차원]
{
value := jelan.write(0x00527B1C, G동파, "UInt")
Sleep, 50
value := jelan.write(0x00527B1C, G동파, "UInt")
Sleep, 50
}
}
NPCTalkTime := A_TickCount - NPCTalkedTime
if(NPCTalkTime >= 5000)
{
AltR()
Sleep, 1000
ipmak += 1
Step = 13
return
}
Check_FormNumber()
Check_NPCMsg()
Sleep, 400
PostClick(395,325)
Sleep, 400
if(FormNumber = 97)
{
IfInString,NPCMsg,100
{if (Gui_KON = 0 || 차원이동감응 = 1)
{
    Sleep, 100
    Get_Location()
    IfInString, Location, 남쪽
    {
        XX := 126
        YY := 36
        Z := 1
        ; XX값을 -1에서 1 사이의 무작위 값으로 변동
        Random, randomY, -1, 1
        YY := YY + randomY
        좌표입력(XX, YY, Z)
        RunMemory("좌표이동")
        Sleep, 2000
        GuiControl, , Gui_NowState, [포남] 파수꾼 감응 위치 확인 중입니다.
        Sleep, 2000
        step = 1061
        SB_SetText("포남 동파 감응 이동.") ;수정 필요
        if (step = 1061)
        {
            ;Get_Location()
            SB_SetText("포남 동파 감응 장소로 이동.")
            if InStr(Location, "남쪽")
            {
            XX := 188
            YY := 35
            Z := 1
            좌표입력(XX, YY, Z)
            RunMemory("좌표이동")
            Sleep, 10000
            step = 1062
            포남대화시도 := A_TickCount
            }
            else
            {
            ParasCount:=3
            step = 8
            return
            }
        }
        while (step = 1062)
        {
            Get_Location()
            if InStr(Location, "남쪽")
            {
            GuiControl, , Gui_NowState, [포남] 동파 감응 위치확인
            SB_SetText("포남 동파 감응 위치 확인.")
            Sleep, 1000
            WinActivate, ahk_pid %jPID%
            WinWaitActive, ahk_pid %jPID%, , 3
            PixelSearch, MobX, MobY, 390, 99, 750, 420, 0xB56900, 10, Fast RGB
            if(ErrorLevel = 1)
            {
            WinMinimize, ahk_exe NexonPlug.exe
            keyclick("tab")
            sleep,400
            AltR()
            sleep,300
            PixelSearch, MobX, MobY, 200, 100, 750, 420, 0xB56900, 10, Fast RGB
            }
            if(ErrorLevel = 0)
            {
                Sleep, 200
                keyclick("tab")
                Sleep, 500
                PostClick(MobX,MobY)
                sleep, 200
                step := 1063
                break
            }
            XX := 188
            YY := 35
            Z := 1
            좌표입력(XX, YY, Z)
            RunMemory("좌표이동")
            Sleep, 1500
            ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",         aOffsets*)
            if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
            {
                이유 := "step18서버연결종료3"
                step := 10000
                return
            }
            }
            else
            {
            ParasCount:=3
            step = 8
            return
            }
        }
        if (step = 1063)
        {
            Get_Location()
            if InStr(Location, "남쪽")
            {
            GuiControl, , Gui_NowState, [포남] 동파 감응 시도
            SB_SetText("포남 동쪽파수꾼 감응.")
            Sleep, 2000
            Loop, 10
            {
                Sleep, 200
                SetFormat, integer, H
                NPCOID2 := jelan.read(0x00584C2C, "UInt", aOffsets*)
                SetFormat, integer, D
                WriteExecutableMemory("NPC호출용1")
                WriteExecutableMemory("NPC호출용2")
                Sleep, 10
                jelan.write(0x00527b54, NPCOID2, "UInt", aOffset*)
                Sleep, 200
                RunMemory("NPC호출")
                Sleep, 200
                SB_SETTEXT(NPCOID2 "호출", 2)
                if (NPCOID2 != "0x0")
                {
                    sleep, 300
                    break  ; 루프를 멈추고 정상적으로 진행
                }
            }
            if (NPCOID2 != "0x0")
            {
                Dimension := jelan.read(0x0058EB1C, "UInt", 0x10A)
                if (Dimension > 20000)
                {
                    차원 := "감마"
                    G동파 := NPCOID2
                    GuiControl,, G동파, %G동파%
                    SB_SETTEXT(차원 . G동파 "입력완료", 2)
                    호출대상 := "감마 - 동파"
                    Sleep, 1000
                    NPCOID2 := 0
                    step := 1064
                    서파실패 := 1
                }
                else if (Dimension > 10000)
                {
                    차원 := "베타"
                    B동파 := NPCOID2
                    GuiControl,, B동파, %B동파%
                    SB_SETTEXT(차원 . B동파 "입력완료", 2)
                    호출대상 := "베타 - 동파"
                    Sleep, 1000
                    NPCOID2 := 0
                    step := 1064
                    서파실패 := 1
                }
                else if (Dimension < 10000)
                {
                    차원 := "알파"
                    A동파 := NPCOID2
                    GuiControl,, A동파, %A동파%
                    SB_SETTEXT(차원 . A동파 "입력완료", 2)
                    호출대상 := "알파 - 동파"
                    Sleep, 1000
                    NPCOID2 := 0
                    step := 1064
                    서파실패 := 1
                }
            }
            }
            else
            {
            Keyclick("tab")
            ParasCount:=3
            step = 8
            return
            }
            ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",         aOffsets*)
            if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
            {
                이유 := "step18서버연결종료3"
                step := 10000
                return
            }
        }
        if (step = 1064)
        {
            Get_Location()
            if InStr(Location, "남쪽")
            {
            GuiControl, , Gui_NowState, [포남] 서파 감응 이동
            Sleep, 500
            SB_SetText("포남 서파 감응 이동.")
            좌표입력(45, 174, 1)
            RunMemory("좌표이동")
            Sleep, 10000
            step = 1065
            서파실패 := 1
            }
            else
            {
            Keyclick("tab")
            ParasCount:=3
            step = 8
            return
            }
                    ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",         aOffsets*)
            if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
            {
                이유 := "step18서버연결종료2"
                step := 10000
                return
            }
        }
        while (step = 1065)
        {
            Get_Location()
            ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",         aOffsets*)
        if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
        {
            break
        }
        if InStr(Location, "남쪽")
        {
            GuiControl, , Gui_NowState, [포남] 서파 감응 위치확인
            SB_SetText("포남 서파 감응 위치 확인.")
            Sleep, 5000
            keyclick("AltR")
            좌표X := jelan.read(s0x0058DAD4, "UInt", 0x10)
            좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
            좌표Z := jelan.read(0x0058DAD4, "UInt", 0x18)
            if (Abs(좌표X - 45) <= 1 && Abs(좌표Y - 174) <= 1 && 좌표Z = 1)
            {
            SB_SetText("포남 서파 감응 장소 도착.")
            step := 1066
            keyclick("tab")
            sleep, 200
            }
            else
            {
            GuiControl, , Gui_NowState, [포남] 서파 감응이동
            SB_SetText("포남 서파 감응 이동.")
            좌표입력(45, 174, 1)
            RunMemory("좌표이동")
            Sleep, 5000
            }
        }
        else
        {
        Keyclick("tab")
        ParasCount:=3
        step = 8
        }
        }
        if (step = 1066)
        {
            Get_Location()
            if InStr(Location, "남쪽")
            {
            GuiControl, , Gui_NowState, [포남] 서파 감응 시도
            SB_SetText("포남 서쪽파수꾼 감응.")
                IfWinNotActive, ahk_pid %jPID%
                {
                    WinActivate, ahk_pid %jPID%
                }
                    PixelSearch, MobX, MobY,  200, 100, 370, 450, 0xEF8AFF, 1, Fast
                    if(ErrorLevel = 1)
                    {
                    WinMinimize, ahk_exe NexonPlug.exe
                    AltR()
                    Sleep,200
                    PixelSearch, MobX, MobY,  200, 100, 370, 450, 0xEF8AFF, 1, Fast
                    }
                    if(ErrorLevel = 0)
                    {
                    PostClick(MobX,MobY)
                    Sleep, 200
                    }
            Loop, 10
            {
                Sleep, 100
                SetFormat, integer, H
                NPCOID3 := jelan.read(0x00584C2C, "UInt", aOffsets*)
                SetFormat, integer, D
                WriteExecutableMemory("NPC호출용1")
                WriteExecutableMemory("NPC호출용2")
                Sleep, 10
                jelan.write(0x00527b54, NPCOID3, "UInt", aOffset*)
                Sleep, 100
                RunMemory("NPC호출")
                Sleep, 100
                SB_SETTEXT(NPCOID3 "호출", 2)
                if (NPCOID3 != "0x0000")
                {
                    sleep,500
                    break  ; 루프를 멈추고 정상적으로 진행
                }
                else if ( NPCOID3 = NPCOID2 )
                {
                    step = 1065
                    NPCOID3 := 0
                    break
                }
            }
            if (NPCOID3 != "0x0000")
            {
                Dimension := jelan.read(0x0058EB1C, "UInt", 0x10A)
                if (Dimension > 20000)
                {
                    차원 := "감마"
                    G서파 := NPCOID3
                    GuiControl,, G서파, %G서파%
                    SB_SETTEXT(차원 . G서파 "입력완료", 2)
                    GuiControl, , Gui_KON, 1
                    SB_SETTEXT("해당" 차원 "감응 입력완료", 1)
                    NPCOID3 := 0
                    sleep,1000
                    step := 1067
                }
                else if (Dimension > 10000)
                {
                    차원 := "베타"
                    B서파 := NPCOID3
                    GuiControl,, B서파, %B서파%
                    SB_SETTEXT(차원 . B서파 "입력완료", 2)
                    GuiControl, , Gui_KON, 1
                    SB_SETTEXT("해당" 차원 "감응 입력완료", 1)
                    NPCOID3 := 0
                    sleep,1000
                    step := 1067
                }
                else if (Dimension < 10000)
                {
                    차원 := "알파"
                    A서파 := NPCOID3
                    GuiControl,, A서파, %A서파%
                    GuiControl, , Gui_KON, 1
                    SB_SETTEXT("해당" 차원 "감응 입력완료", 1)
                    NPCOID3 := 0
                    sleep,1000
                    step := 1067
                }
            }
        }
        else
            {
            Keyclick("tab")
            ParasCount:=3
            step = 8
            return
            }
            ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",         aOffsets*)
            if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
            {
                이유 := "step18서버연결종료"
                step := 10000
                ipmak += 1
                return
            }
        }
         if (step = 1067)
        {
            Get_Location()
            if InStr(Location, "남쪽")
            {
            GuiControl, , Gui_NowState, [포남] 감응 저장 중
            업데이트체크 := 1
            차원이동감응 = 0
            서파실패 := 0
            동파실패 := 0
            포북가자 := 0
            sleep,300
            if(Gui_MoveLoute1 = 1)
            {
            if(Aloute = 1)
            {
            MapNumber := 125
            }
            if(Bloute = 1)
            {
            MapNumber := 5
            }
            if(Cloute = 1)
            {
            MapNumber := 266
            }
            }
            else
            {
            MapNumber := 5
            }
            step = 20
            CheckPN := 1
            OID저장()
            }
            else
            {
            Keyclick("tab")
            저장완료 := 1
            업데이트체크 := 1
            차원이동감응 = 0
            포북가자 := 1
            step = 11
            if(Gui_CheckWPDMagic = 1)
            {
            WPD()
            }
            OID저장()
            return
            }
            ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",aOffsets*)
            if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
            {
                이유 := "step18감응실패"
                step := 10000
                ipmak += 1
                return
            }
        }
    }
}
}
Entrance = 0
Sleep, 400
PostClick(90,80)
Sleep, 400
GuiControl, , Gui_NowState, [포남] 사냥터에 도착하였습니다.
SB_SetText("포남 사냥터에 입장 하였습니다.")
Entrance = 0
PostMessage, 0x100, 54, 458753, , ahk_pid %jPID%
PostMessage, 0x101, 54, 458753, , ahk_pid %jPID%
JoinTime := A_TickCount
Sleep, 400
if(Gui_jjOn = 1)
{
Send, {F15 Down}
Sleep, 40
Send, {F15 Up}
Sleep, 10
Send, {F15 Down}
Sleep, 40
Send, {F15 Up}
PickUp_itemsetPS()
}
Step = 19
if(Gui_KON = 1)
{
Sleep, 500
Get_Location()
IfInString,Location,남쪽
{
Send, {F14}
Sleep, 100
Send, {F14}
Sleep, 100
}
GuiControl,,시작체력,%CheckFirstHP%
GuiControl,,상승체력,%CheckUPHP% (%상승체력평균값%)
GuiControl,,경과시간,%RunningTime%
CheckPN = 1
}
if (Gui_KON = 0 || 차원이동감응 = 1)
{
    Sleep, 100
    Get_Location()
    IfInString, Location, 남쪽
    {
        XX := 126
        YY := 36
        Z := 1
        ; XX값을 -1에서 1 사이의 무작위 값으로 변동
        Random, randomY, -1, 1
        YY := YY + randomY
        좌표입력(XX, YY, Z)
        RunMemory("좌표이동")
        Sleep, 2000
        GuiControl, , Gui_NowState, [포남] 파수꾼 감응 위치 확인 중입니다.
        Sleep, 2000
        step = 1061
        SB_SetText("포남 동파 감응 이동.") ;수정 필요
        if (step = 1061)
        {
            ;Get_Location()
            SB_SetText("포남 동파 감응 장소로 이동.")
            if InStr(Location, "남쪽")
            {
            XX := 188
            YY := 35
            Z := 1
            좌표입력(XX, YY, Z)
            RunMemory("좌표이동")
            Sleep, 10000
            step = 1062
            포남대화시도 := A_TickCount
            }
            else
            {
            ParasCount:=3
            step = 8
            return
            }
        }
        while (step = 1062)
        {
            Get_Location()
            if InStr(Location, "남쪽")
            {
            GuiControl, , Gui_NowState, [포남] 동파 감응 위치확인
            SB_SetText("포남 동파 감응 위치 확인.")
            Sleep, 1000
            WinActivate, ahk_pid %jPID%
            WinWaitActive, ahk_pid %jPID%, , 3
            PixelSearch, MobX, MobY, 390, 99, 750, 420, 0xB56900, 10, Fast RGB
            if(ErrorLevel = 1)
            {
            WinMinimize, ahk_exe NexonPlug.exe
            keyclick("tab")
            sleep,400
            AltR()
            sleep,300
            PixelSearch, MobX, MobY, 200, 100, 750, 420, 0xB56900, 10, Fast RGB
            }
            if(ErrorLevel = 0)
            {
                Sleep, 200
                keyclick("tab")
                Sleep, 500
                PostClick(MobX,MobY)
                sleep, 200
                step := 1063
                break
            }
            XX := 188
            YY := 35
            Z := 1
            좌표입력(XX, YY, Z)
            RunMemory("좌표이동")
            Sleep, 1500
            ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",         aOffsets*)
            if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
            {
                이유 := "step18서버연결종료3"
                step := 10000
                return
            }
            }
            else
            {
            ParasCount:=3
            step = 8
            return
            }
        }
        if (step = 1063)
        {
            Get_Location()
            if InStr(Location, "남쪽")
            {
            GuiControl, , Gui_NowState, [포남] 동파 감응 시도
            SB_SetText("포남 동쪽파수꾼 감응.")
            Sleep, 2000
            Loop, 10
            {
                Sleep, 200
                SetFormat, integer, H
                NPCOID2 := jelan.read(0x00584C2C, "UInt", aOffsets*)
                SetFormat, integer, D
                WriteExecutableMemory("NPC호출용1")
                WriteExecutableMemory("NPC호출용2")
                Sleep, 10
                jelan.write(0x00527b54, NPCOID2, "UInt", aOffset*)
                Sleep, 200
                RunMemory("NPC호출")
                Sleep, 200
                SB_SETTEXT(NPCOID2 "호출", 2)
                if (NPCOID2 != "0x0")
                {
                    sleep, 300
                    break  ; 루프를 멈추고 정상적으로 진행
                }
            }
            if (NPCOID2 != "0x0")
            {
                Dimension := jelan.read(0x0058EB1C, "UInt", 0x10A)
                if (Dimension > 20000)
                {
                    차원 := "감마"
                    G동파 := NPCOID2
                    GuiControl,, G동파, %G동파%
                    SB_SETTEXT(차원 . G동파 "입력완료", 2)
                    호출대상 := "감마 - 동파"
                    Sleep, 1000
                    NPCOID2 := 0
                    step := 1064
                    서파실패 := 1
                }
                else if (Dimension > 10000)
                {
                    차원 := "베타"
                    B동파 := NPCOID2
                    GuiControl,, B동파, %B동파%
                    SB_SETTEXT(차원 . B동파 "입력완료", 2)
                    호출대상 := "베타 - 동파"
                    Sleep, 1000
                    NPCOID2 := 0
                    step := 1064
                    서파실패 := 1
                }
                else if (Dimension < 10000)
                {
                    차원 := "알파"
                    A동파 := NPCOID2
                    GuiControl,, A동파, %A동파%
                    SB_SETTEXT(차원 . A동파 "입력완료", 2)
                    호출대상 := "알파 - 동파"
                    Sleep, 1000
                    NPCOID2 := 0
                    step := 1064
                    서파실패 := 1
                }
            }
            }
            else
            {
            Keyclick("tab")
            ParasCount:=3
            step = 8
            return
            }
            ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",         aOffsets*)
            if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
            {
                이유 := "step18서버연결종료3"
                step := 10000
                return
            }
        }
        if (step = 1064)
        {
            Get_Location()
            if InStr(Location, "남쪽")
            {
            GuiControl, , Gui_NowState, [포남] 서파 감응 이동
            Sleep, 500
            SB_SetText("포남 서파 감응 이동.")
            좌표입력(45, 174, 1)
            RunMemory("좌표이동")
            Sleep, 10000
            step = 1065
            서파실패 := 1
            }
            else
            {
            Keyclick("tab")
            ParasCount:=3
            step = 8
            return
            }
                    ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",         aOffsets*)
            if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
            {
                이유 := "step18서버연결종료2"
                step := 10000
                return
            }
        }
        while (step = 1065)
        {
            Get_Location()
            ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",         aOffsets*)
        if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
        {
            break
        }
        if InStr(Location, "남쪽")
        {
            GuiControl, , Gui_NowState, [포남] 서파 감응 위치확인
            SB_SetText("포남 서파 감응 위치 확인.")
            Sleep, 5000
            keyclick("AltR")
            좌표X := jelan.read(s0x0058DAD4, "UInt", 0x10)
            좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
            좌표Z := jelan.read(0x0058DAD4, "UInt", 0x18)
            if (Abs(좌표X - 45) <= 1 && Abs(좌표Y - 174) <= 1 && 좌표Z = 1)
            {
            SB_SetText("포남 서파 감응 장소 도착.")
            step := 1066
            keyclick("tab")
            sleep, 200
            }
            else
            {
            GuiControl, , Gui_NowState, [포남] 서파 감응이동
            SB_SetText("포남 서파 감응 이동.")
            좌표입력(45, 174, 1)
            RunMemory("좌표이동")
            Sleep, 5000
            }
        }
        else
        {
        Keyclick("tab")
        ParasCount:=3
        step = 8
        }
        }
        if (step = 1066)
        {
            Get_Location()
            if InStr(Location, "남쪽")
            {
            GuiControl, , Gui_NowState, [포남] 서파 감응 시도
            SB_SetText("포남 서쪽파수꾼 감응.")
                IfWinNotActive, ahk_pid %jPID%
                {
                    WinActivate, ahk_pid %jPID%
                }
                    PixelSearch, MobX, MobY,  200, 100, 370, 450, 0xEF8AFF, 1, Fast
                    if(ErrorLevel = 1)
                    {
                    WinMinimize, ahk_exe NexonPlug.exe
                    AltR()
                    Sleep,200
                    PixelSearch, MobX, MobY,  200, 100, 370, 450, 0xEF8AFF, 1, Fast
                    }
                    if(ErrorLevel = 0)
                    {
                    PostClick(MobX,MobY)
                    Sleep, 200
                    }
            Loop, 10
            {
                Sleep, 100
                SetFormat, integer, H
                NPCOID3 := jelan.read(0x00584C2C, "UInt", aOffsets*)
                SetFormat, integer, D
                WriteExecutableMemory("NPC호출용1")
                WriteExecutableMemory("NPC호출용2")
                Sleep, 10
                jelan.write(0x00527b54, NPCOID3, "UInt", aOffset*)
                Sleep, 100
                RunMemory("NPC호출")
                Sleep, 100
                SB_SETTEXT(NPCOID3 "호출", 2)
                if (NPCOID3 != "0x0000")
                {
                    sleep,500
                    break  ; 루프를 멈추고 정상적으로 진행
                }
                else if ( NPCOID3 = NPCOID2 )
                {
                    step = 1065
                    NPCOID3 := 0
                    break
                }
            }
            if (NPCOID3 != "0x0000")
            {
                Dimension := jelan.read(0x0058EB1C, "UInt", 0x10A)
                if (Dimension > 20000)
                {
                    차원 := "감마"
                    G서파 := NPCOID3
                    GuiControl,, G서파, %G서파%
                    SB_SETTEXT(차원 . G서파 "입력완료", 2)
                    GuiControl, , Gui_KON, 1
                    SB_SETTEXT("해당" 차원 "감응 입력완료", 1)
                    NPCOID3 := 0
                    sleep,100
                    step := 1067
                }
                else if (Dimension > 10000)
                {
                    차원 := "베타"
                    B서파 := NPCOID3
                    GuiControl,, B서파, %B서파%
                    SB_SETTEXT(차원 . B서파 "입력완료", 2)
                    GuiControl, , Gui_KON, 1
                    SB_SETTEXT("해당" 차원 "감응 입력완료", 1)
                    NPCOID3 := 0
                    sleep,100
                    step := 1067
                }
                else if (Dimension < 10000)
                {
                    차원 := "알파"
                    A서파 := NPCOID3
                    GuiControl,, A서파, %A서파%
                    GuiControl, , Gui_KON, 1
                    SB_SETTEXT("해당" 차원 "감응 입력완료", 1)
                    NPCOID3 := 0
                    sleep,100
                    step := 1067
                }
            }
        }
        else
            {
            Keyclick("tab")
            ParasCount:=3
            step = 8
            return
            }
            ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",         aOffsets*)
            if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
            {
                이유 := "step18서버연결종료"
                step := 10000
                ipmak += 1
                return
            }
        }
         if (step = 1067)
        {
            Get_Location()
            if InStr(Location, "남쪽")
            {
            GuiControl, , Gui_NowState, [포남] 감응 저장 중
            업데이트체크 := 1
            차원이동감응 = 0
            서파실패 := 0
            동파실패 := 0
            포북가자 := 0
            sleep,300
            if(Gui_MoveLoute1 = 1)
            {
            if(Aloute = 1)
            {
            MapNumber := 125
            }
            if(Bloute = 1)
            {
            MapNumber := 5
            }
            if(Cloute = 1)
            {
            MapNumber := 266
            }
            }
            else
            {
            MapNumber := 5
            }
            step = 20
            CheckPN := 1
            OID저장()
            }
            else
            {
            Keyclick("tab")
            저장완료 := 1
            업데이트체크 := 1
            차원이동감응 = 0
            step = 11
            if(Gui_CheckWPDMagic = 1)
            {
            WPD()
            }
            OID저장()
            return
            }
            ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",aOffsets*)
            if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
            {
                이유 := "step18감응실패"
                step := 10000
                ipmak += 1
                return
            }
        }
    }
}
}
}
if(Step = 19)
{
GuiControl, , Gui_NowState, [포남] 체잠 시작.
SB_SetText("맵 이동 중")
if(Gui_CheckWPDMagic = 1)
{
WPD()
}
ipmak = 0
if(MapNumber >= 143)
{
MapNumber = 1
Step = 9
return
}
CharMovePonam()
Step = 20
}
if(Step = 20)
{
SB_SetText("움직임 체크 중")
Check_Moving()
Get_Pos()
Get_MovePos()
if(Moving = 0)
{
Sleep, 200
Check_Moving()
if(Moving = 0)
{
Step = 21
}
}
if((PosX >= MovePosX and PosX <= MovePosX) and (PosY >= MovePosY and PosY <= MovePosY))
{
MoveWaitCount = 0
Step = 24
}
}
if(Step = 21)
{
Get_Pos()
Get_MovePos()
if((PosX >= MovePosX and PosX <= MovePosX) and (PosY >= MovePosY and PosY <= MovePosY))
{
MoveWaitCount = 0
Step = 24
}
if(!((PosX >= MovePosX and PosX <= MovePosX) and (PosY >= MovePosY and PosY <= MovePosY)))
{
if(MoveWaitCount >= 2)
{
MoveWaitCount = 0
Step = 9
}
else
{
Step = 24
}
}
}
if(Step = 24)
{
SB_SetText("몬스터 찾는 중")
IfWinNotActive, ahk_pid %jPID%
{
WinActivate, ahk_pid %jPID%
}
if(Gui_Ent = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
}
if(Gui_Rockey = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
}
if(Gui_EntRockey = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
}
}
if(Gui_Mand = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
if(Gui_AllMobAND = 1)
{
if(Gui_1Muba = 1)
{
if(BWValue1 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue1 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_2Muba = 1)
{
if(BWValue1 < Gui_AllMobLimit or BWValue2 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue1 >= Gui_AllMobLimit and BWValue2 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_3Muba = 1)
{
if(BWValue1 < Gui_AllMobLimit or BWValue2 < Gui_AllMobLimit or BWValue3 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue1 >= Gui_AllMobLimit and BWValue2 >= Gui_AllMobLimit and BWValue3 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_2ButMuba = 1)
{
if(BWValue0 < Gui_AllMobLimit or BWValue1 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue0 >= Gui_AllMobLimit and BWValue1 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_3ButMuba = 1)
{
if(BWValue0 < Gui_AllMobLimit or BWValue1 < Gui_AllMobLimit or BWValue2 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue0 >= Gui_AllMobLimit and BWValue1 >= Gui_AllMobLimit and BWValue2 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_4ButMuba = 1)
{
if(BWValue0 < Gui_AllMobLimit or BWValue1 < Gui_AllMobLimit or BWValue2 < Gui_AllMobLimit or BWValue3 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue0 >= Gui_AllMobLimit and BWValue1 >= Gui_AllMobLimit and BWValue2 >= Gui_AllMobLimit and BWValue3 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(Gui_AllMobOR = 1)
{
if(Gui_1Muba = 1)
{
if(BWValue1 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue1 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_2Muba = 1)
{
if(BWValue1 < Gui_AllMobLimit and BWValue2 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue1 >= Gui_AllMobLimit or BWValue2 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_3Muba = 1)
{
if(BWValue1 < Gui_AllMobLimit and BWValue2 < Gui_AllMobLimit and BWValue3 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue1 >= Gui_AllMobLimit or BWValue2 >= Gui_AllMobLimit or BWValue3 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_2ButMuba = 1)
{
if(BWValue0 < Gui_AllMobLimit and BWValue1 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue0 >= Gui_AllMobLimit or BWValue1 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_3ButMuba = 1)
{
if(BWValue0 < Gui_AllMobLimit and BWValue1 < Gui_AllMobLimit and BWValue2 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue0 >= Gui_AllMobLimit or BWValue1 >= Gui_AllMobLimit or BWValue2 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_4ButMuba = 1)
{
if(BWValue0 < Gui_AllMobLimit and BWValue1 < Gui_AllMobLimit and BWValue2 < Gui_AllMobLimit and BWValue3 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue0 >= Gui_AllMobLimit or BWValue1 >= Gui_AllMobLimit or BWValue2 >= Gui_AllMobLimit or BWValue3 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(Gui_MobMagic = 1)
{
if(Gui_1Muba = 1)
{
if(MagicAbility3 < MLimit and MagicAbility4 < MLimit and MagicAbility5 < MLimit and MagicAbility6 < MLimit and MagicAbility7 < MLimit and MagicAbility8 < MLimit and MagicAbility9 < MLimit and MagicAbility10 < MLimit and MagicAbility11 < MLimit and MagicAbility12 < MLimit and MagicAbility13 < MLimit and MagicAbility14 < MLimit and MagicAbility15 < MLimit and MagicAbility16 < MLimit and MagicAbility17 < MLimit and MagicAbility18 < MLimit)
{
if(BWValue1 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue1 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(MagicAbility3 >= MLimit or MagicAbility4 >= MLimit or MagicAbility5 >= MLimit or MagicAbility6 >= MLimit or MagicAbility7 >= MLimit or MagicAbility8 >= MLimit or MagicAbility9 >= MLimit or MagicAbility10 >= MLimit or MagicAbility11 >= MLimit or MagicAbility12 >= MLimit or MagicAbility13 >= MLimit or MagicAbility14 >= MLimit or MagicAbility15 >= MLimit or MagicAbility16 >= MLimit or MagicAbility17 >= MLimit or MagicAbility18 >= MLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_2Muba = 1)
{
if(MagicAbility3 < MLimit and MagicAbility4 < MLimit and MagicAbility5 < MLimit and MagicAbility6 < MLimit and MagicAbility7 < MLimit and MagicAbility8 < MLimit and MagicAbility9 < MLimit and MagicAbility10 < MLimit and MagicAbility11 < MLimit and MagicAbility12 < MLimit and MagicAbility13 < MLimit and MagicAbility14 < MLimit and MagicAbility15 < MLimit and MagicAbility16 < MLimit and MagicAbility17 < MLimit and MagicAbility18 < MLimit)
{
if(BWValue1 < Gui_AllMobLimit and BWValue2 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue1 >= Gui_AllMobLimit or BWValue2 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(MagicAbility3 >= MLimit or MagicAbility4 >= MLimit or MagicAbility5 >= MLimit or MagicAbility6 >= MLimit or MagicAbility7 >= MLimit or MagicAbility8 >= MLimit or MagicAbility9 >= MLimit or MagicAbility10 >= MLimit or MagicAbility11 >= MLimit or MagicAbility12 >= MLimit or MagicAbility13 >= MLimit or MagicAbility14 >= MLimit or MagicAbility15 >= MLimit or MagicAbility16 >= MLimit or MagicAbility17 >= MLimit or MagicAbility18 >= MLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_3Muba = 1)
{
if(MagicAbility3 < MLimit and MagicAbility4 < MLimit and MagicAbility5 < MLimit and MagicAbility6 < MLimit and MagicAbility7 < MLimit and MagicAbility8 < MLimit and MagicAbility9 < MLimit and MagicAbility10 < MLimit and MagicAbility11 < MLimit and MagicAbility12 < MLimit and MagicAbility13 < MLimit and MagicAbility14 < MLimit and MagicAbility15 < MLimit and MagicAbility16 < MLimit and MagicAbility17 < MLimit and MagicAbility18 < MLimit)
{
if(BWValue1 < Gui_AllMobLimit and BWValue2 < Gui_AllMobLimit and BWValue3 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue1 >= Gui_AllMobLimit or BWValue2 >= Gui_AllMobLimit or BWValue3 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(MagicAbility3 >= MLimit or MagicAbility4 >= MLimit or MagicAbility5 >= MLimit or MagicAbility6 >= MLimit or MagicAbility7 >= MLimit or MagicAbility8 >= MLimit or MagicAbility9 >= MLimit or MagicAbility10 >= MLimit or MagicAbility11 >= MLimit or MagicAbility12 >= MLimit or MagicAbility13 >= MLimit or MagicAbility14 >= MLimit or MagicAbility15 >= MLimit or MagicAbility16 >= MLimit or MagicAbility17 >= MLimit or MagicAbility18 >= MLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_2ButMuba = 1)
{
if(MagicAbility3 < MLimit and MagicAbility4 < MLimit and MagicAbility5 < MLimit and MagicAbility6 < MLimit and MagicAbility7 < MLimit and MagicAbility8 < MLimit and MagicAbility9 < MLimit and MagicAbility10 < MLimit and MagicAbility11 < MLimit and MagicAbility12 < MLimit and MagicAbility13 < MLimit and MagicAbility14 < MLimit and MagicAbility15 < MLimit and MagicAbility16 < MLimit and MagicAbility17 < MLimit and MagicAbility18 < MLimit)
{
if(BWValue0 < Gui_AllMobLimit and BWValue1 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue0 >= Gui_AllMobLimit or BWValue1 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(MagicAbility3 >= MLimit or MagicAbility4 >= MLimit or MagicAbility5 >= MLimit or MagicAbility6 >= MLimit or MagicAbility7 >= MLimit or MagicAbility8 >= MLimit or MagicAbility9 >= MLimit or MagicAbility10 >= MLimit or MagicAbility11 >= MLimit or MagicAbility12 >= MLimit or MagicAbility13 >= MLimit or MagicAbility14 >= MLimit or MagicAbility15 >= MLimit or MagicAbility16 >= MLimit or MagicAbility17 >= MLimit or MagicAbility18 >= MLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_3ButMuba = 1)
{
if(MagicAbility3 < MLimit and MagicAbility4 < MLimit and MagicAbility5 < MLimit and MagicAbility6 < MLimit and MagicAbility7 < MLimit and MagicAbility8 < MLimit and MagicAbility9 < MLimit and MagicAbility10 < MLimit and MagicAbility11 < MLimit and MagicAbility12 < MLimit and MagicAbility13 < MLimit and MagicAbility14 < MLimit and MagicAbility15 < MLimit and MagicAbility16 < MLimit and MagicAbility17 < MLimit and MagicAbility18 < MLimit)
{
if(BWValue0 < Gui_AllMobLimit and BWValue1 < Gui_AllMobLimit and BWValue2 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue0 >= Gui_AllMobLimit or BWValue1 >= Gui_AllMobLimit or BWValue2 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(MagicAbility3 >= MLimit or MagicAbility4 >= MLimit or MagicAbility5 >= MLimit or MagicAbility6 >= MLimit or MagicAbility7 >= MLimit or MagicAbility8 >= MLimit or MagicAbility9 >= MLimit or MagicAbility10 >= MLimit or MagicAbility11 >= MLimit or MagicAbility12 >= MLimit or MagicAbility13 >= MLimit or MagicAbility14 >= MLimit or MagicAbility15 >= MLimit or MagicAbility16 >= MLimit or MagicAbility17 >= MLimit or MagicAbility18 >= MLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_4ButMuba = 1)
{
if(MagicAbility3 < MLimit and MagicAbility4 < MLimit and MagicAbility5 < MLimit and MagicAbility6 < MLimit and MagicAbility7 < MLimit and MagicAbility8 < MLimit and MagicAbility9 < MLimit and MagicAbility10 < MLimit and MagicAbility11 < MLimit and MagicAbility12 < MLimit and MagicAbility13 < MLimit and MagicAbility14 < MLimit and MagicAbility15 < MLimit and MagicAbility16 < MLimit and MagicAbility17 < MLimit and MagicAbility18 < MLimit)
{
if(BWValue0 < Gui_AllMobLimit and BWValue1 < Gui_AllMobLimit and BWValue2 < Gui_AllMobLimit and BWValue3 < Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xFFB68C, 10, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 760, 450, 0xE7E7E7, 5, *fast
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(BWValue0 >= Gui_AllMobLimit or BWValue1 >= Gui_AllMobLimit or BWValue2 >= Gui_AllMobLimit or BWValue3 >= Gui_AllMobLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(MagicAbility3 >= MLimit or MagicAbility4 >= MLimit or MagicAbility5 >= MLimit or MagicAbility6 >= MLimit or MagicAbility7 >= MLimit or MagicAbility8 >= MLimit or MagicAbility9 >= MLimit or MagicAbility10 >= MLimit or MagicAbility11 >= MLimit or MagicAbility12 >= MLimit or MagicAbility13 >= MLimit or MagicAbility14 >= MLimit or MagicAbility15 >= MLimit or MagicAbility16 >= MLimit or MagicAbility17 >= MLimit or MagicAbility18 >= MLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
}
if(ErrorLevel = 0)
{
PostClick(MobX,MobY)
Monster_OID()
WinGetPos, ElanciaClientX, ElanciaClientY, Width, Height, ahk_pid %jPID%
SplashX := MobX + ElanciaClientX - 30
SplashY := MobY + ElanciaClientY - 20
SplashImage, %MobNumber%:, b X%SplashX% Y%SplashY% W80 H80 CW000000
MobNumber += 1
if(MobNumber >= 11)
{
MobNumber = 1
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
Step = 19
return
}
AttackLoopCount = 0
AttackCount = 0
Sleep, 500
Step = 25
return
}
if(ErrorLevel = 1)
{
MobNumber = 1
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
Step = 19
return
}
}
if(Step = 25)
{
SB_SetText("몹 공격 체크 중")
AttackLoopCount += 1
Check_Attack()
if(Attack = 0)
{
AttackCount += 1
}
if(Attack = 1 or Attack = 2)
{
AttackCount = 0
}
if(AttackLoopCount >= 10)
{
if(AttackCount > 5)
{
AttackLoopCount = 0
AttackCount = 0
Step = 24
}
else
{
MobNumber = 1
AttackLoopCount = 0
AttackCount = 0
movmob := A_TickCount
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
Step = 26
}
}
}
if(Step = 26)
{
SB_SetText("몹 근접 체크 중")
Check_Moving()
if(Moving = 0)
{
Sleep, 200
Check_Moving()
if(Moving = 0)
{
AltR()
Step = 27
}
}
movmob2 := A_TickCount - movmob
if(movmob2 >= 2300)
{
Sleep, 100
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 24
}
}
if(Step = 27)
{
SB_SetText("무바를 시작하였습니다")
if(Gui_1Muba = 1)
{
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(RepairWeaponCount1 >= 5)
{
RepairWeaponCount1 = 0
MapNumber = 1
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 300
return
}
keyclick(1)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount1 += 1
}
if(Weapon != 0)
{
RepairWeaponCount1 = 0
}
}
if(Gui_2Muba = 1)
{
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(RepairWeaponCount1 >= 5 or RepairWeaponCount2 >= 5)
{
RepairWeaponCount1 = 0
RepairWeaponCount2 = 0
MapNumber = 1
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 300
return
}
if(MubaStep = 1)
{
keyclick(1)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(TempWeapon = Weapon)
{
TempWeapon := Weapon
RepairWeaponCount1 += 1
}
if(TempWeapon != Weapon)
{
TempWeapon := Weapon
RepairWeaponCount1 = 0
}
MubaStep = 2
return
}
if(MubaStep = 2)
{
keyclick(2)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(TempWeapon = Weapon)
{
TempWeapon := Weapon
RepairWeaponCount2 += 1
}
if(TempWeapon != Weapon)
{
TempWeapon := Weapon
RepairWeaponCount2 = 0
}
MubaStep = 1
return
}
}
if(Gui_3Muba = 1)
{
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(AbilityName = Gui_Weapon3)
{
BWValue3 := AbilityValue
}
if(RepairWeaponCount1 >= 5 or RepairWeaponCount2 >= 5 or RepairWeaponCount3 >= 5)
{
RepairWeaponCount1 = 0
RepairWeaponCount2 = 0
RepairWeaponCount3 = 0
MapNumber = 1
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 300
return
}
if(MubaStep = 1)
{
keyclick(1)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(TempWeapon = Weapon)
{
TempWeapon := Weapon
RepairWeaponCount1 += 1
}
if(TempWeapon != Weapon)
{
TempWeapon := Weapon
RepairWeaponCount1 = 0
}
MubaStep = 2
return
}
if(MubaStep = 2)
{
keyclick(2)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(TempWeapon = Weapon)
{
TempWeapon := Weapon
RepairWeaponCount2 += 1
}
if(TempWeapon != Weapon)
{
TempWeapon := Weapon
RepairWeaponCount2 = 0
}
MubaStep = 3
return
}
if(MubaStep = 3)
{
keyclick(3)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon3 = "현금" or Gui_Weapon3 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon3)
{
BWValue3 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon3 = "현금" or Gui_Weapon3 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(TempWeapon = Weapon)
{
TempWeapon := Weapon
RepairWeaponCount3 += 1
}
if(TempWeapon != Weapon)
{
TempWeapon := Weapon
RepairWeaponCount3 = 0
}
MubaStep = 1
return
}
}
if(Gui_2ButMuba = 1)
{
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(RepairWeaponCount1 >= 5)
{
RepairWeaponCount1 = 0
MapNumber = 1
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 300
return
}
if(MubaStep = 1)
{
keyclick(1)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount1 += 1
}
if(Weapon != 0)
{
RepairWeaponCount1 = 0
}
MubaStep = 2
return
}
if(MubaStep = 2)
{
WPD()
Sleep, 100
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
RemoteM()
}
MubaStep = 1
return
}
}
if(Gui_3ButMuba = 1)
{
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(RepairWeaponCount1 >= 5 or RepairWeaponCount2 >= 5)
{
RepairWeaponCount1 = 0
RepairWeaponCount2 = 0
MapNumber = 1
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 300
return
}
if(MubaStep = 1)
{
keyclick(1)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount1 += 1
}
if(Weapon != 0)
{
RepairWeaponCount1 = 0
}
MubaStep = 2
return
}
if(MubaStep = 2)
{
WPD()
Sleep, 100
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
RemoteM()
}
MubaStep = 3
return
}
if(MubaStep = 3)
{
keyclick(2)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount2 += 1
}
if(Weapon != 0)
{
RepairWeaponCount2 = 0
}
MubaStep = 4
return
}
if(MubaStep = 4)
{
WPD()
Sleep, 100
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
RemoteM()
}
MubaStep = 1
return
}
}
if(Gui_4ButMuba = 1)
{
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(AbilityName = Gui_Weapon3)
{
BWValue3 := AbilityValue
}
if(RepairWeaponCount1 >= 5 or RepairWeaponCount2 >= 5 or RepairWeaponCount3 >= 5)
{
RepairWeaponCount1 = 0
RepairWeaponCount2 = 0
RepairWeaponCount3 = 0
MapNumber = 1
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 300
return
}
if(MubaStep = 1)
{
keyclick(1)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount1 += 1
}
if(Weapon != 0)
{
RepairWeaponCount1 = 0
}
MubaStep = 2
return
}
if(MubaStep = 2)
{
WPD()
Sleep, 100
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
RemoteM()
}
MubaStep = 3
return
}
if(MubaStep = 3)
{
keyclick(2)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount2 += 1
}
if(Weapon != 0)
{
RepairWeaponCount2 = 0
}
MubaStep = 4
return
}
if(MubaStep = 4)
{
WPD()
Sleep, 100
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
RemoteM()
}
MubaStep = 5
return
}
if(MubaStep = 5)
{
keyclick(3)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon3 = "현금" or Gui_Weapon3 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon3)
{
BWValue3 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon3 = "현금" or Gui_Weapon3 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount3 += 1
}
if(Weapon != 0)
{
RepairWeaponCount3 = 0
}
MubaStep = 6
return
}
if(MubaStep = 6)
{
WPD()
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
Sleep, 100
if(Gui_CheckUseMagic = 1)
{
RemoteM()
}
MubaStep = 1
return
}
}
}
if(Step = 30)
{
SB_SetText("감응 버프 받는 중")
CheckPN := 0
RepairWeaponCount := 0
countsignal := 0
Step = 31
}
if(step = 31)
{
GuiControl, , Gui_NowState, [포남] 감응 전 메모리 점유율 확인
SB_SetText("메모리 점유율 체크 중")
GetPrivateWorkingSet(jPID)
if(TotalPhy > 2000000)
{
if(byte > 1000000)
{
이전스텝 := step
step = 10000
return
}
if(byte <= 1000000)
{
step = 32
}
}
if(TotalPhy <= 2000000)
{
if(byte > 620000)
{
이전스텝 := step
step = 10000
return
}
if(byte <= 620000)
{
step = 32
}
}
step = 32
}
if(Step = 32) ;포남 무바 중 감응 파트
{
    SB_SetText("감응 버프 받는 중")
    IfWinNotActive, ahk_pid %jPID%
    {
    WINACTIVATE, ahk_pid %jPID%
    }
	Keyclick("tab")
	sleep,100
    감응()
    sleep,500
    감응 += 1
    PNnewTime = %A_Now%
    EnvAdd, PNnewTime, 18, Minutes
    FormatTime, PNnewTime1, %PNnewTime%, yyyyMMddHHmm
    CheckPN := 1
    step = 24
}
if(Step = 90)
{
GuiControl, , Gui_NowState, [포남] 포남링/생결 교환 중
SB_SetText("포남링과 생결 도전 중")
NPCTalkTime := A_TickCount - NPCTalkedTime
if(NPCTalkTime >= 5000)
{
AltR()
Sleep, 1000
ipmak += 1
Step = 13
return
}
Check_FormNumber()
Check_NPCMsg()
if(FormNumber = 85)
{
Sleep, 400
PostClick(375,340)
Sleep, 600
Step = 91
}
}
if(Step = 91)
{
Check_FormNumber()
Check_NPCMsg()
if(FormNumber = 121)
{
Sleep, 400
PostClick(115,65)
Sleep, 800
Step = 93
}
}
if(Step = 93)
{
Check_FormNumber()
Check_NPCMsg()
if(FormNumber = 85)
{
IfInString,NPCMsg,도전
{
Sleep, 400
PostClick(123,85)
Sleep, 800
step = 94
}
}
}
if(Step = 94)
{
Check_FormNumber()
Check_NPCMsg()
if(FormNumber = 121)
{
Sleep, 400
PostClick(120,80)
Sleep, 800
Step = 95
}
}
if(Step = 95)
{
Check_FormNumber()
Check_NPCMsg()
if(FormNumber = 85)
{
IfInString,NPCMsg,확률
{
Sleep, 400
PostClick(123,85)
Sleep, 800
step = 96
}
}
}
if(Step = 96)
{
Check_FormNumber()
Check_NPCMsg()
if(FormNumber = 121)
{
Sleep, 400
PostClick(80,115)
Sleep, 800
RCC = 0
Step = 13
}
}
if(Step = 100)
{
GuiControl, , Gui_NowState, [잡화점] 상점으로 이동 중.
SB_SetText("라깃구매 - 잡화점으로 이동 중")
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(345,297)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 101
}
if(Step = 101)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 102
}
}
}
if(Step = 102)
{
SB_SetText("현재위치가 잡화점인지 확인 중")
Get_Location()
IfInString,Location,잡화점
{
Step = 103
}
IfNotInString,Location,잡화점
{
AltR()
Step = 100
}
}
if(Step = 103)
{
GuiControl, , Gui_NowState, [잡화점] 상점 도착.
SB_SetText("현재소지 갈리드 체크 중")
Get_Gold()
if(Gold <= 10000)
{
Step = 500
}
else
{
Step = 1115
}
if(Step = 1115)
{
SB_SetText("몸찌방지 자리이동 중")
좌표입력(41,30,0)
RunMemory("좌표이동")
Sleep, 3500
Step = 1116
}
if(Step = 1116)
{
AltR()
좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
좌표Z := jelan.read(0x0058DAD4, "UInt", 0x18)
SB_SetText("몸찌방지 자리확인 중")
if( 좌표X = 41 && 좌표Y = 30 && 좌표Z = 0)
{
Step = 104
}
else
{
Step = 1115
}
}
}
if(Step = 104)
{
SB_SetText("라깃구매 중")
Move_Buy()
Sleep, 100
PostMessage, 0x100, 17, 1900545, , ahk_pid %jPID%
PostMessage, 0x100, 52, 327681, , ahk_pid %jPID%
PostMessage, 0x101, 52, 327681, , ahk_pid %jPID%
PostMessage, 0x101, 17, 1900545, , ahk_pid %jPID%
ShopOpendTime := A_TickCount
Sleep, 500
Step = 105
}
if(Step = 105)
{
Check_NPCMenu()
if(NPCMenu = 1)
{
Check_NPCMenuPos()
if(NPCMenuBuyPosX != "" and NPCMenuBuyPosY != "")
{
Sleep, 500
PostClick(NPCMenuBuyPosX,NPCMenuBuyPosY)
BuyCheckedTime := A_TickCount
Step = 106
}
}
if(NPCMenu = 0)
{
ShopOpenTime := A_TickCount - ShopOpendTime
if(ShopOpenTime >= 10000)
{
Step = 104
}
}
}
if(Step = 106)
{
Check_Shop()
if(Buy = 1)
{
Sleep, 500
Step = 107
}
if(Buy = 0)
{
BuyCheckTime := A_TickCount - BuyCheckedTime
if(BuyCheckTime >= 10000)
{
Step = 104
}
}
}
if(Step = 107)
{
PostClick(180,60)
Sleep, 100
Loop,11
{
PostMessage, 0x100, 40, 22020097, , ahk_pid %jPID%
PostMessage, 0x101, 40, 22020097, , ahk_pid %jPID%
}
PostMessage, 0x100, 53, 393217, , ahk_pid %jPID%
PostMessage, 0x101, 53, 393217, , ahk_pid %jPID%
PostMessage, 0x100, 53, 393217, , ahk_pid %jPID%
PostMessage, 0x101, 53, 393217, , ahk_pid %jPID%
PostMessage, 0x100, 13, 1835009, , ahk_pid %jPID%
PostMessage, 0x101, 13, 1835009, , ahk_pid %jPID%
Sleep, 2000
Step = 108
}
if(Step = 108)
{
SB_SetText("라깃구매 완료")
Check_Shop()
if(Buy = 1)
{
Sleep, 1000
PostMessage, 0x100, 27, 65537, , ahk_pid %jPID%
PostMessage, 0x101, 27, 65537, , ahk_pid %jPID%
Sleep, 1000
Step = 109
}
}
if(Step = 109)
{
라깃카운트 := 라깃카운트+55
GuiControl, , Gui_RasCount, %라깃카운트%
Check_Shop()
if(Buy = 0)
{
Step = 110
}
if(Buy = 1)
{
Step = 108
}
}
if(Step = 110)
{
SB_SetText("상점 밖으로 이동 중")
좌표입력(33,31,0)
RunMemory("좌표이동")
Sleep, 3500
Step = 111
}
if(Step = 111)
{
SB_SetText("잡화점을 나왔는지 체크 중")
Get_Location()
IfInString,Location,잡화점
{
AltR()
Step = 109
}
IfNotInString,Location,잡화점
{
Step = 112
}
}
if(Step = 112)
{
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(370,310)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Sleep, 500
Step = 113
}
if(Step = 113)
{
if(HuntPlace = 1)
{
Step = 11
}
if(HuntPlace = 2)
{
Step = 1002
}
}
if(STEP = 200)
{
GuiControl, , Gui_NowState, [베이커리] 상점으로 이동 중.
SB_SetText("FP채우기 - 베이커리로 이동 중")
value := jelan.write(0x0042483A, 0xB0, "Char", aOffsets*)
value := jelan.write(0x0042483B, 0x01, "Char", aOffsets*)
value := jelan.write(0x0042483C, 0x90, "Char", aOffsets*)
value := jelan.write(0x0042483D, 0x90, "Char", aOffsets*)
value := jelan.write(0x0042483E, 0x90, "Char", aOffsets*)
value := jelan.write(0x0042483F, 0x90, "Char", aOffsets*)
value := jelan.write(0x00436071, 0xEB, "Char", aOffsets*)
value := jelan.write(0x0042304D, 0xEB, "Char", aOffsets*)
value := jelan.write(0x0042304B, 0x90, "Char", aOffsets*)
value := jelan.write(0x0042304C, 0x90, "Char", aOffsets*)
value := jelan.write(0x00423073, 0x90, "Char", aOffsets*)
value := jelan.write(0x00423074, 0x90, "Char", aOffsets*)
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 500
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 201
return
}
if(CountPortal = 1)
{
PostClick(645,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 201
return
}
if(CountPortal = 2)
{
PostClick(660,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 201
return
}
}
}
if(Step = 201)
{
Check_NPCMsg()
IfInString,NPCMsg,라스의깃
{
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMSPH.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
Get_Location()
IfInString,Location,[알파차원] 포프레스네 마을
{
Step = 202
}
IfInString,Location,[베타차원] 포프레스네 마을
{
Step = 202
}
IfInString,Location,[감마차원] 포프레스네 마을
{
Step = 202
}
}
if(Step = 202)
{
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(295,262)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 203
}
if(Step = 203)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 204
}
}
}
if(Step = 204)
{
SB_SetText("현재위치가 베이커리인지 확인 중")
Get_Location()
IfInString,Location,포프레스네 베이커리
{
Step = 205
}
IfNotInString,Location,포프레스네 베이커리
{
AltR()
Step = 202
}
}
if(Step = 205)
{
GuiControl, , Gui_NowState, [베이커리] 상점 도착.
SB_SetText("FP채우기 - 현재소지 갈리드 체크 중")
Get_Gold()
if(Gold <= 100000)
{
Step = 500
}
else
{
Step = 206
}
}
if(Step = 206)
{
SB_SetText("몸찌방지 자리이동 중")
if(몸찌체크 < 3)
{
좌표입력(19,28,0)
RunMemory("좌표이동")
Step = 207
}
else if(3 <= 몸찌체크 and 몸찌체크 < 6)
{
좌표입력(29,15,0)
RunMemory("좌표이동")
Step = 207
}
else
{
AltR()
Step = 250
몸찌체크 := 0
}
Sleep, 3500
}
if(Step = 207)
{
좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
SB_SetText("몸찌방지 자리확인 중")
Sleep,500
if((좌표X = 19 and 좌표Y = 28) or (좌표X = 29 and 좌표Y = 15))
{
Step = 208
Sleep, 100
}
else
{
Step = 206
몸찌체크 += 1
Sleep, 100
}
}
if(Step = 208)
{
SB_SetText("식빵 구매 중")
Move_Buy()
Sleep, 100
PostMessage, 0x100, 17, 1900545, , ahk_pid %jPID%
PostMessage, 0x100, 51, 262145, , ahk_pid %jPID%
PostMessage, 0x101, 51, 262145, , ahk_pid %jPID%
PostMessage, 0x101, 17, 1900545, , ahk_pid %jPID%
Sleep, 500
ShopOpendTime := A_TickCount
Step = 209
}
if(Step = 209)
{
Check_NPCMenu()
if(NPCMenu = 1)
{
Check_NPCMenuPos()
if(NPCMenuBuyPosX != "" and NPCMenuBuyPosY != "")
{
Sleep, 700
PostClick(NPCMenuBuyPosX,NPCMenuBuyPosY)
BuyCheckedTime := A_TickCount
Step = 210
}
}
if(NPCMenu = 0)
{
ShopOpenTime := A_TickCount - ShopOpendTime
if(ShopOpenTime >= 10000)
{
Step = 208
}
}
}
if(Step = 210)
{
Check_Shop()
if(Buy = 1)
{
Sleep, 500
빵++
PostClick(115,60)
Step = 211
Sleep,500
}
if(Buy = 0)
{
BuyCheckTime := A_TickCount - BuyCheckedTime
if(BuyCheckTime >= 10000)
{
Step = 208
}
}
}
if(Step = 211)
{
Get_inven()
if(빵 = 1)
{
if(Nowinven < 48)
{
Loop,27
{
PostMessage, 0x100, 40, 22020097, , ahk_pid %jPID%
PostMessage, 0x101, 40, 22020097, , ahk_pid %jPID%
}
Loop,100
{
POSTMESSAGE,0x100,39,21823489,,ahk_pid %jPID%
POSTMESSAGE,0x101,39,21823489,,ahk_pid %jPID%
}
KeyClick("Enter")
Sleep, 300
PostMessage, 0x100, 27, 65537, , ahk_pid %jPID%
PostMessage, 0x101, 27, 65537, , ahk_pid %jPID%
Sleep, 300
Step = 208
빵 = 1
}
else
{
Loop,27
{
PostMessage, 0x100, 40, 22020097, , ahk_pid %jPID%
PostMessage, 0x101, 40, 22020097, , ahk_pid %jPID%
}
Loop,100
{
POSTMESSAGE,0x100,39,21823489,,ahk_pid %jPID%
POSTMESSAGE,0x101,39,21823489,,ahk_pid %jPID%
}
KeyClick("Enter")
Sleep,100
Step = 212
빵 = 1
}
}
if(빵 > 1)
{
Get_inven()
Get_FP()
Loop,27
{
PostMessage, 0x100, 40, 22020097, , ahk_pid %jPID%
PostMessage, 0x101, 40, 22020097, , ahk_pid %jPID%
}
Loop,100
{
POSTMESSAGE,0x100,39,21823489,,ahk_pid %jPID%
POSTMESSAGE,0x101,39,21823489,,ahk_pid %jPID%
}
Loop,
{
Sleep,200
POSTMESSAGE,0x100,13,1835009 ,,ahk_pid %jPID%
POSTMESSAGE,0x101,13,1835009 ,,ahk_pid %jPID%
Sleep,200
POSTMESSAGE,0x100,13,1835009 ,,ahk_pid %jPID%
POSTMESSAGE,0x101,13,1835009 ,,ahk_pid %jPID%
Sleep,200
Get_inven()
if(Nowinven = 50)
{
SLEEP,100
Step = 212
빵 = 1
break
}
}
}
}
if(Step = 212)
{
Check_Shop()
if(Buy = 1)
{
Sleep, 300
PostMessage, 0x100, 27, 65537, , ahk_pid %jPID%
PostMessage, 0x101, 27, 65537, , ahk_pid %jPID%
Sleep, 300
Step = 213
}
}
if(Step = 213)
{
Check_Shop()
if(Buy = 0)
{
Step = 214
}
if(Buy = 1)
{
Step = 212
}
}
if(Step = 214)
{
SB_SetText("FP 채우는 중")
Check_Shop()
if(Buy = 0)
{
Loop,120
{
Loop,50
{
PostMessage, 0x100, 57, 655361, , ahk_pid %jPID%
PostMessage, 0x101, 57, 655361, , ahk_pid %jPID%
}
Sleep,30
}
Step = 215
}
}
if(Step = 215)
{
Get_FP()
if(NowFP != MaxFP)
{
Sleep, 200
Step = 205
}
if(NowFP = MaxFP)
{
SB_SetText("FP채우기 완료")
Sleep, 200
Step = 216
몸찌체크 := 0
}
}
if(Step = 216)
{
SB_SetText("상점 밖으로 이동 중")
좌표입력(32,31,0)
RunMemory("좌표이동")
Sleep,3500
if(상점밖이동 >= 3)
{
AltR()
상점밖이동 := 0
Step = 8
return
}
Step = 217
}
if(Step = 217)
{
SB_SetText("베이커리 상점을 나왔는지 체크 중")
Get_Location()
IfInString,Location,베이커리
{
AltR()
Step = 216
상점밖이동++
}
IfNotInString,Location,베이커리
{
if(HuntPlace = 1)
{
Step = 11
상점밖이동 := 0
}
if(HuntPlace = 2)
{
Step = 1002
상점밖이동 := 0
}
}
}
if(Step = 250)
{
GuiControl, , Gui_NowState, 몸찌방해감지 - 세르니카로이동.
SB_SetText("베이커리 - 몸찌방해감지")
value := jelan.write(0x0042483A, 0xB0, "Char", aOffsets*)
value := jelan.write(0x0042483B, 0x01, "Char", aOffsets*)
value := jelan.write(0x0042483C, 0x90, "Char", aOffsets*)
value := jelan.write(0x0042483D, 0x90, "Char", aOffsets*)
value := jelan.write(0x0042483E, 0x90, "Char", aOffsets*)
value := jelan.write(0x0042483F, 0x90, "Char", aOffsets*)
value := jelan.write(0x00436071, 0xEB, "Char", aOffsets*)
value := jelan.write(0x0042304D, 0xEB, "Char", aOffsets*)
value := jelan.write(0x0042304B, 0x90, "Char", aOffsets*)
value := jelan.write(0x0042304C, 0x90, "Char", aOffsets*)
value := jelan.write(0x00423073, 0x90, "Char", aOffsets*)
value := jelan.write(0x00423074, 0x90, "Char", aOffsets*)
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 500
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(542,142)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(546,120)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 251
return
}
if(CountPortal = 1)
{
PostClick(560,120)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 251
return
}
if(CountPortal = 2)
{
PostClick(574,120)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 251
return
}
}
}
if(Step = 251)
{
Check_NPCMsg()
IfInString,NPCMsg,라스의깃
{
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMSPH.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
Get_Location()
IfInString,Location,[알파차원] 세르니카 마을
{
Step = 252
}
IfInString,Location,[베타차원] 세르니카 마을
{
Step = 252
}
IfInString,Location,[감마차원] 세르니카 마을
{
Step = 252
}
}
if(Step = 252)
{
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(380,205)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 253
}
if(Step = 253)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 254
}
}
}
if(Step = 254)
{
SB_SetText("현재위치가 베이커리인지 확인 중")
Get_Location()
IfInString,Location,세르니카 빵가게
{
Step = 255
sleep,100
}
IfNotInString,Location,세르니카 빵가게
{
AltR()
Step = 252
}
}
if(Step = 255)
{
GuiControl, , Gui_NowState, [베이커리] 상점 도착.
SB_SetText("FP채우기 - 현재소지 갈리드 체크 중")
Get_Gold()
if(Gold <= 100000)
{
Step = 500
}
else
{
Step = 256
}
}
if(Step = 256)
{
SB_SetText("몸찌방지 자리이동 중")
if(몸찌체크 < 5)
{
좌표입력(20,36,0)
RunMemory("좌표이동")
Step = 257
}
else
{
AltR()
Step = 8
몸찌체크 := 0
}
Sleep, 3000
}
if(Step = 257)
{
좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
SB_SetText("몸찌방지 자리확인 중")
Sleep,500
if(좌표X = 20 and 좌표Y = 36)
{
Step = 258
}
else
{
Step = 256
몸찌체크 += 1
Sleep, 100
}
}
if(Step = 258)
{
SB_SetText("식빵 구매 중")
Move_Buy()
Sleep, 100
PostMessage, 0x100, 17, 1900545, , ahk_pid %jPID%
PostMessage, 0x100, 56, 589825, , ahk_pid %jPID%
PostMessage, 0x101, 56, 589825, , ahk_pid %jPID%
PostMessage, 0x101, 17, 1900545, , ahk_pid %jPID%
Sleep, 500
ShopOpendTime := A_TickCount
Step = 259
}
if(Step = 259)
{
Check_NPCMenu()
if(NPCMenu = 1)
{
Check_NPCMenuPos()
if(NPCMenuBuyPosX != "" and NPCMenuBuyPosY != "")
{
Sleep, 700
PostClick(NPCMenuBuyPosX,NPCMenuBuyPosY)
BuyCheckedTime := A_TickCount
Step = 260
}
}
if(NPCMenu = 0)
{
ShopOpenTime := A_TickCount - ShopOpendTime
if(ShopOpenTime >= 10000)
{
Step = 258
}
}
}
if(Step = 260)
{
Check_Shop()
if(Buy = 1)
{
Sleep, 500
빵++
PostClick(115,60)
Step = 261
Sleep,500
}
if(Buy = 0)
{
BuyCheckTime := A_TickCount - BuyCheckedTime
if(BuyCheckTime >= 10000)
{
Step = 258
}
}
}
if(Step = 261)
{
Get_inven()
if(빵 = 1)
{
if(Nowinven != 50)
{
Loop,41
{
PostMessage, 0x100, 40, 22020097, , ahk_pid %jPID%
PostMessage, 0x101, 40, 22020097, , ahk_pid %jPID%
}
Loop,100
{
POSTMESSAGE,0x100,39,21823489,,ahk_pid %jPID%
POSTMESSAGE,0x101,39,21823489,,ahk_pid %jPID%
}
KeyClick("Enter")
Sleep, 300
PostMessage, 0x100, 27, 65537, , ahk_pid %jPID%
PostMessage, 0x101, 27, 65537, , ahk_pid %jPID%
Sleep, 300
Step = 258
}
if(Nowinven = 50)
{
Sleep,100
Step = 262
빵 = 1
}
}
if(빵 > 1)
{
Get_inven()
Get_FP()
Loop,41
{
PostMessage, 0x100, 40, 22020097, , ahk_pid %jPID%
PostMessage, 0x101, 40, 22020097, , ahk_pid %jPID%
}
Loop,100
{
POSTMESSAGE,0x100,39,21823489,,ahk_pid %jPID%
POSTMESSAGE,0x101,39,21823489,,ahk_pid %jPID%
}
Loop,
{
Sleep,200
POSTMESSAGE,0x100,13,1835009 ,,ahk_pid %jPID%
POSTMESSAGE,0x101,13,1835009 ,,ahk_pid %jPID%
Sleep,200
POSTMESSAGE,0x100,13,1835009 ,,ahk_pid %jPID%
POSTMESSAGE,0x101,13,1835009 ,,ahk_pid %jPID%
Sleep,200
Get_inven()
if(Nowinven = 50)
{
SLEEP,100
Step = 262
빵 = 1
break
}
}
}
}
if(Step = 262)
{
Check_Shop()
if(Buy = 1)
{
Sleep, 300
PostMessage, 0x100, 27, 65537, , ahk_pid %jPID%
PostMessage, 0x101, 27, 65537, , ahk_pid %jPID%
Sleep, 300
Step = 263
}
}
if(Step = 263)
{
Check_Shop()
if(Buy = 0)
{
Step = 264
}
if(Buy = 1)
{
Step = 262
}
}
if(Step = 264)
{
SB_SetText("FP 채우는 중")
Check_Shop()
if(Buy = 0)
{
Loop,120
{
Loop,50
{
PostMessage, 0x100, 57, 655361, , ahk_pid %jPID%
PostMessage, 0x101, 57, 655361, , ahk_pid %jPID%
}
Sleep,30
}
Step = 265
}
}
if(Step = 265)
{
Get_FP()
if(NowFP != MaxFP)
{
Sleep, 200
Step = 255
}
if(NowFP = MaxFP)
{
SB_SetText("FP채우기 완료")
Sleep, 200
Step = 266
몸찌체크 := 0
}
}
if(Step = 266)
{
SB_SetText("포프레스네 마을로 이동")
GuiControl, , Gui_NowState, 포프레스네 마을로 이동.
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 500
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 267
return
}
if(CountPortal = 1)
{
PostClick(645,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 267
return
}
if(CountPortal = 2)
{
PostClick(660,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 267
return
}
}
}
if(Step = 267)
{
SB_SetText("포프레스네 마을 체크중")
Get_Location()
IfInString,Location,세르니카 마을
{
AltR()
Step = 266
}
IfNotInString,Location,세르니카 마을
{
if(HuntPlace = 1)
{
Step = 11
}
if(HuntPlace = 2)
{
Step = 1002
}
}
}
if(Step = 300)
{
GuiControl, , Gui_NowState, [수리점] 상점으로 이동 중.
SB_SetText("무기수리 - 수리점으로 이동 중")
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 500
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 301
return
}
if(CountPortal = 1)
{
PostClick(645,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 301
return
}
if(CountPortal = 2)
{
PostClick(660,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 301
return
}
}
}
if(Step = 301)
{
Check_NPCMsg()
IfInString,NPCMsg,라스의깃
{
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMSPH.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
Get_Location()
IfInString,Location,[알파차원] 포프레스네 마을
{
Step = 302
}
IfInString,Location,[베타차원] 포프레스네 마을
{
Step = 302
}
IfInString,Location,[감마차원] 포프레스네 마을
{
Step = 302
}
}
if(Step = 302)
{
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(348,206)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 303
}
if(Step = 303)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 304
}
}
}
if(Step = 304)
{
SB_SetText("현재위치가 수리점인지 확인 중")
Get_Location()
IfInString,Location,석공소
{
Step = 305
}
IfNotInString,Location,석공소
{
AltR()
Step = 302
}
}
if(Step = 305)
{
SB_SetText("현재소지 갈리드 체크 중")
Get_Gold()
if(Gold <= 100000)
{
Step = 500
}
else
{
Step = 1113
}
}
if(Step = 1113)
{
GuiControl, , Gui_NowState, [수리점] 상점 도착.
SB_SetText("몸찌방지 자리이동 중")
좌표입력(20,29,0)
RunMemory("좌표이동")
Sleep, 3500
Step = 1114
}
if(Step = 1114)
{
좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
좌표Z := jelan.read(0x0058DAD4, "UInt", 0x18)
SB_SetText("몸찌방지 자리확인 중")
if( 좌표X = 20 && 좌표Y = 29 && 좌표Z = 0)
{
Step = 306
}
else
{
Step = 1113
}
}
if(Step = 306)
{
SB_SetText("무기 수리 중")
Move_Repair()
Sleep, 100
PostMessage, 0x100, 17, 1900545, , ahk_pid %jPID%
PostMessage, 0x100, 50, 196609, , ahk_pid %jPID%
PostMessage, 0x101, 50, 196609, , ahk_pid %jPID%
PostMessage, 0x101, 17, 1900545, , ahk_pid %jPID%
Sleep, 500
ShopOpendTime := A_TickCount
Step = 307
}
if(Step = 307)
{
Check_NPCMenu()
if(NPCMenu = 1)
{
Check_NPCMenuPos()
if(NPCMenuRepairPosX != "" and NPCMenuRepairPosY != "")
{
Sleep, 500
PostClick(NPCMenuRepairPosX,NPCMenuRepairPosY)
RepairClickedTime := A_TickCount
Step = 308
}
}
if(NPCMenu = 0)
{
ShopOpenTime := A_TickCount - ShopOpendTime
if(ShopOpenTime >= 10000)
{
Step = 306
}
}
}
if(Step = 308)
{
Check_Shop()
if(Repair = 1)
{
Sleep, 500
Step = 309
}
if(Repair = 0)
{
RepairClickTime := A_TickCount - RepairClickedTime
if(RepairClickTime >= 10000)
{
Step = 312
}
}
}
if(Step = 309)
{
PostClick(355,320)
Sleep, 2000
Step = 310
}
if(Step = 310)
{
Check_Shop()
if(Repair = 1)
{
PostMessage, 0x100, 27, 65537, , ahk_pid %jPID%
PostMessage, 0x101, 27, 65537, , ahk_pid %jPID%
Sleep, 1000
Step = 311
}
}
if(Step = 311)
{
SB_SetText("무기 수리 완료")
Check_Shop()
if(Repair = 0)
{
Step = 312
}
if(Repair = 1)
{
Step = 310
}
}
if(Step = 312)
{
SB_SetText("수리점 밖으로 이동 중")
좌표입력(31,31,0)
RunMemory("좌표이동")
Sleep, 3500
Step = 313
}
if(Step = 313)
{
SB_SetText("현재위치가 수리점 밖인지 체크 중")
Get_Location()
IfInString,Location,석공소
{
AltR()
Step = 311
}
IfNotInString,Location,석공소
{
Step = 314
}
}
if(Step = 314)
{
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(349,233)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Sleep, 500
Step = 315
}
if(Step = 315)
{
if(HuntPlace = 1)
{
RCC += 1
Step = 11
}
if(HuntPlace = 2)
{
Step = 1002
}
}
if(Step = 400)
{
GuiControl, , Gui_NowState, [마법상점] 상점으로 이동 중.
SB_SetText("물약구매 - 상점으로 이동 중")
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(470,335)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 401
}
if(Step = 401)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 402
}
}
}
if(Step = 402)
{
SB_SetText("현재위치가 마법상점인지 체크 중")
Get_Location()
IfInString,Location,마법상점
{
Step = 403
}
IfNotInString,Location,마법상점
{
AltR()
Step = 400
}
}
if(Step = 403)
{
GuiControl, , Gui_NowState, [마법상점] 상점 도착.
SB_SetText("마법상점 NPC에게 가는 중")
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(402,293)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 404
}
if(Step = 404)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 405
}
}
}
if(Step = 405)
{
SB_SetText("마법상점 NPC가 있는지 체크 중")
Get_Pos()
if(PosX >= 20 and PosX <= 51 and PosY >= 10 and PosY <= 24)
{
Step = 406
}
if(!(PosX >= 20 and PosX <= 51 and PosY >= 10 and PosY <= 24))
{
Step = 403
}
}
if(Step = 406)
{
SB_SetText("현재소지 갈리드 체크 중")
Get_Gold()
if(Gold <= 100000)
{
Step = 500
}
else
{
Step = 407
}
}
if(Step = 407)
{
SB_SetText("물약 구매 중")
Move_Buy()
Sleep, 100
PostMessage, 0x100, 17, 1900545, , ahk_pid %jPID%
PostMessage, 0x100, 53, 393217, , ahk_pid %jPID%
PostMessage, 0x101, 53, 393217, , ahk_pid %jPID%
PostMessage, 0x101, 17, 1900545, , ahk_pid %jPID%
Sleep, 500
ShopOpendTime := A_TickCount
Step = 408
}
if(Step = 408)
{
Check_NPCMenu()
if(NPCMenu = 1)
{
Check_NPCMenuPos()
if(NPCMenuBuyPosX != "" and NPCMenuBuyPosY != "")
{
Sleep, 500
PostClick(NPCMenuBuyPosX,NPCMenuBuyPosY)
Step = 409
}
}
if(NPCMenu = 0)
{
ShopOpenTime := A_TickCount - ShopOpendTime
if(ShopOpenTime >= 10000)
{
AltR()
Sleep, 2000
Step = 403
}
}
}
if(Step = 409)
{
Check_Shop()
if(Buy = 1)
{
Sleep, 500
Step = 410
}
}
if(Step = 410)
{
PostClick(180,60)
Sleep, 100
PostMessage, 0x100, 50, 196609, , ahk_pid %jPID%
PostMessage, 0x101, 50, 196609, , ahk_pid %jPID%
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x100, 13, 1835009, , ahk_pid %jPID%
PostMessage, 0x101, 13, 1835009, , ahk_pid %jPID%
MedCount := Gui_MedCount + 20
GuiControl, , Gui_MedCount, %MedCount%
Sleep, 2000
Step = 411
}
if(Step = 411)
{
Check_Shop()
if(Buy = 1)
{
Sleep, 1000
PostMessage, 0x100, 27, 65537, , ahk_pid %jPID%
PostMessage, 0x101, 27, 65537, , ahk_pid %jPID%
Sleep, 1000
Step = 412
}
}
if(Step = 412)
{
SB_SetText("물약 구매 완료")
Check_Shop()
if(Buy = 0)
{
Step = 413
}
if(Buy = 1)
{
Step = 411
}
}
if(Step = 413)
{
SB_SetText("마법상점 밖으로 이동 중")
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(400,324)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 414
}
if(Step = 414)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 415
}
}
}
if(Step = 415)
{
SB_SetText("마법상점을 나왔는지 체크 중")
Get_Location()
IfInString,Location,마법상점
{
AltR()
Step = 412
}
IfNotInString,Location,마법상점
{
Step = 1002
}
}
if(Step = 500)
{
GuiControl, , Gui_NowState, [은행] 상점으로 이동 중.
SB_SetText("골드바 > 갈리드로 변경하러 가는 중")
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 500
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
CountPortal = 1
Step = 501
}
}
if(Step = 501)
{
Check_NPCMsg()
IfInString,NPCMsg,라스의깃
{
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMSPH.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
Get_Location()
IfInString,Location,[알파차원] 포프레스네 마을
{
Step = 502
}
}
if(Step = 502)
{
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(358,372)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 503
}
if(Step = 503)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 504
}
}
}
if(Step = 504)
{
SB_SetText("현재위치가 은행인지 확인 중")
Get_Location()
IfInString,Location,은행
{
Step = 505
}
IfNotInString,Location,은행
{
AltR()
Step = 502
}
}
if(Step = 505)
{
GuiControl, , Gui_NowState, [은행] 상점 도착.
SB_SetText("NPC 위치로 이동 중")
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(380,302)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 506
}
if(Step = 506)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 507
}
}
}
if(Step = 507)
{
SB_SetText("NPC가 있는지 확인 중")
Get_Pos()
if(PosX >= 16 and PosX <= 40 and PosY >= 9 and PosY <= 29)
{
Step = 508
}
if(!(PosX >= 16 and PosX <= 40 and PosY >= 9 and PosY <= 29))
{
Step = 505
}
}
if(Step = 508)
{
SB_SetText("골드바 > 갈리드 교환 중")
Move_NPCTalkForm()
Sleep, 100
PostMessage, 0x100, 17, 1900545, , ahk_pid %jPID%
PostMessage, 0x100, 54, 458753, , ahk_pid %jPID%
PostMessage, 0x101, 54, 458753, , ahk_pid %jPID%
PostMessage, 0x101, 17, 1900545, , ahk_pid %jPID%
Step = 509
}
if(Step = 509)
{
Check_NPCMsg()
IfInString,NPCMsg,사고 팔아요
{
Sleep, 500
PostClick(135,87)
Sleep, 500
}
IfInString,NPCMsg,팔건가?
{
Sleep, 500
PostClick(121,80)
Sleep, 500
Step = 510
}
IfInString,NPCMsg,속이려는건가
{
GuiControl, , Gui_NowState, 골드바가 없어 종료합니다.
SB_SetText("골드바가 부족합니다.")
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
}
if(Step = 510)
{
SB_SetText("골드바 거래 완료")
Check_NPCMsg()
IfInString,NPCMsg,사고 팔아요
{
Sleep, 500
PostClick(135,100)
Sleep, 500
Step = 511
}
}
if(Step = 511)
{
SB_SetText("은행 밖으로 이동 중")
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(400,324)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 512
}
if(Step = 512)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 513
}
}
}
if(Step = 513)
{
SB_SetText("현재위치가 은행 밖인지 체크 중")
Get_Location()
IfInString,Location,은행
{
AltR()
Step = 511
}
IfNotInString,Location,은행
{
if(Grade = 1)
{
Grade = 0
Step = 602
return
}
if(HuntPlace = 1)
{
Step = 11
}
if(HuntPlace = 2)
{
Step = 1002
}
}
}
if(Step = 550)
{
GuiControl, , Gui_NowState, [은행] 상점으로 이동 중.
SB_SetText("골드바 > 갈리드로 변경하러 가는 중")
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 500
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
CountPortal = 1
Step = 551
}
}
if(Step = 551)
{
Check_NPCMsg()
IfInString,NPCMsg,라스의깃
{
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMSPH.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
Get_Location()
IfInString,Location,[알파차원] 포프레스네 마을
{
Step = 552
}
}
if(Step = 552)
{
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(358,372)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 553
}
if(Step = 553)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 554
}
}
}
if(Step = 554)
{
SB_SetText("현재위치가 은행인지 체크 중")
Get_Location()
IfInString,Location,은행
{
Step = 555
}
IfNotInString,Location,은행
{
AltR()
Step = 552
}
}
if(Step = 555)
{
GuiControl, , Gui_NowState, [은행] 상점 도착.
SB_SetText("NPC 위치로 이동 중")
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(380,302)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 556
}
if(Step = 556)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 557
}
}
}
if(Step = 557)
{
SB_SetText("NPC가 있는지 체크 중")
Get_Pos()
if(PosX >= 16 and PosX <= 40 and PosY >= 9 and PosY <= 29)
{
Step = 558
}
if(!(PosX >= 16 and PosX <= 40 and PosY >= 9 and PosY <= 29))
{
Step = 555
}
}
if(Step = 558)
{
SB_SetText("골드바 > 갈리드 교환 중")
Move_NPCTalkForm()
Sleep, 100
PostMessage, 0x100, 17, 1900545, , ahk_pid %jPID%
PostMessage, 0x100, 54, 458753, , ahk_pid %jPID%
PostMessage, 0x101, 54, 458753, , ahk_pid %jPID%
PostMessage, 0x101, 17, 1900545, , ahk_pid %jPID%
Step = 559
}
if(Step = 559)
{
Check_NPCMsg()
IfInString,NPCMsg,사고 팔아요
{
Sleep, 500
PostClick(135,87)
Sleep, 500
}
IfInString,NPCMsg,팔건가?
{
Sleep, 500
PostClick(121,80)
Sleep, 500
Step = 560
}
IfInString,NPCMsg,속이려는건가
{
GuiControl, , Gui_NowState, 골드바가 없어 종료합니다.
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
}
if(Step = 560)
{
SB_SetText("골드바 거래 완료")
Check_NPCMsg()
IfInString,NPCMsg,사고 팔아요
{
Sleep, 500
PostClick(135,100)
Sleep, 500
Step = 561
}
}
if(Step = 561)
{
SB_SetText("은행 밖으로 이동 중")
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(400,324)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 562
}
if(Step = 562)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 563
}
}
}
if(Step = 563)
{
SB_SetText("현재위치가 은행 밖인지 체크 중")
Get_Location()
IfInString,Location,은행
{
AltR()
Step = 561
}
IfNotInString,Location,은행
{
if(Grade = 1)
{
Grade = 0
Step = 652
return
}
if(HuntPlace = 1)
{
Step = 11
}
if(HuntPlace = 2)
{
Step = 1002
}
}
}
if(Step = 800)
{
GuiControl, , Gui_NowState, [은행] 상점으로 이동 중.
SB_SetText("강제그렐 골드바 > 갈리드 이동중")
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 500
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
CountPortal = 1
Step = 801
}
}
if(Step = 801)
{
Check_NPCMsg()
IfInString,NPCMsg,라스의깃
{
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMSPH.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
Get_Location()
IfInString,Location,[알파차원] 포프레스네 마을
{
Step = 802
}
}
if(Step = 802)
{
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(358,372)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 803
}
if(Step = 803)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 804
}
}
}
if(Step = 804)
{
SB_SetText("현재위치가 은행인지 체크 중")
Get_Location()
IfInString,Location,은행
{
Step = 805
}
IfNotInString,Location,은행
{
AltR()
Step = 802
}
}
if(Step = 805)
{
GuiControl, , Gui_NowState, [은행] 상점 도착.
SB_SetText("NPC 위치로 이동 중")
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(380,302)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 806
}
if(Step = 806)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 807
}
}
}
if(Step = 807)
{
SB_SetText("NPC가 있는지 체크 중")
Get_Pos()
if(PosX >= 16 and PosX <= 40 and PosY >= 9 and PosY <= 29)
{
Step = 808
}
if(!(PosX >= 16 and PosX <= 40 and PosY >= 9 and PosY <= 29))
{
Step = 805
}
}
if(Step = 808)
{
SB_SetText("골드바 > 갈리드 거래중")
Move_NPCTalkForm()
Sleep, 100
PostMessage, 0x100, 17, 1900545, , ahk_pid %jPID%
PostMessage, 0x100, 54, 458753, , ahk_pid %jPID%
PostMessage, 0x101, 54, 458753, , ahk_pid %jPID%
PostMessage, 0x101, 17, 1900545, , ahk_pid %jPID%
Step = 809
}
if(Step = 809)
{
Check_NPCMsg()
IfInString,NPCMsg,사고 팔아요
{
Sleep, 500
PostClick(135,87)
Sleep, 500
}
IfInString,NPCMsg,팔건가?
{
Sleep, 500
PostClick(121,80)
Sleep, 500
Step = 810
}
IfInString,NPCMsg,속이려는건가
{
GuiControl, , Gui_NowState, 골드바가 없어 종료합니다.
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
}
if(Step = 810)
{
SB_SetText("골드바 거래 완료")
Check_NPCMsg()
IfInString,NPCMsg,사고 팔아요
{
Sleep, 500
PostClick(135,100)
Sleep, 500
Step = 811
}
}
if(Step = 811)
{
SB_SetText("은행 밖으로 이동 중")
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(400,324)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 812
}
if(Step = 812)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 813
}
}
}
if(Step = 813)
{
SB_SetText("현재위치가 은행 밖인지 확인 중")
Get_Location()
IfInString,Location,은행
{
AltR()
Step = 811
}
IfNotInString,Location,은행
{
Step = 702
return
}
}
if(Step = 600)
{
GuiControl, , Gui_NowState, [신전/성당] 기도하러 가는 중
SB_SetText("그레이드 하러 가는 중")
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 500
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 601
return
}
if(CountPortal = 1)
{
PostClick(645,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 601
return
}
if(CountPortal = 2)
{
PostClick(660,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 601
return
}
}
}
if(Step = 601)
{
Check_NPCMsg()
IfInString,NPCMsg,라스의깃
{
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMSPH.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
Get_Location()
IfInString,Location,[알파차원] 포프레스네 마을
{
Step = 602
}
IfInString,Location,[베타차원] 포프레스네 마을
{
Step = 602
}
IfInString,Location,[감마차원] 포프레스네 마을
{
Step = 602
}
}
if(Step = 602)
{
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(406,180)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 603
}
if(Step = 603)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 604
}
}
}
if(Step = 604)
{
SB_SetText("현재위치가 신전/성당인지 체크 중")
Get_Location()
IfInString,Location,신전
{
Step = 605
}
IfNotInString,Location,신전
{
AltR()
Step = 602
}
}
if(Step = 605)
{
GuiControl, , Gui_NowState, [신전/성당] 도착.
SB_SetText("현재소지 갈리드 체크 중")
Get_Gold()
if(Gold < 1000000)
{
if(Gui_Grade = 1)
{
Grade = 1
Step = 500
}
if(Gui_Grade = 0)
{
GuiControl, , Gui_NowState, 갈리드가 부족하여 종료합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("갈리드 부족")
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
}
if(Gold >= 1000000)
{
Step = 606
}
}
if(Step = 606)
{
SB_SetText("그레이드 중")
Move_NPCTalkForm()
Sleep, 100
PostMessage, 0x100, 17, 1900545, , ahk_pid %jPID%
PostMessage, 0x100, 55, 524289, , ahk_pid %jPID%
PostMessage, 0x101, 55, 524289, , ahk_pid %jPID%
PostMessage, 0x101, 17, 1900545, , ahk_pid %jPID%
Sleep, 500
Step = 607
}
if(Step = 607)
{
Check_FormNumber()
Check_NPCMsg()
if(FormNumber = 92)
{
IfInString,NPCMsg,무엇을 도와드릴까요
{
Sleep, 500
PostClick(129,77)
Sleep, 500
}
}
if(FormNumber = 68)
{
IfInString,NPCMsg,맞습니까
{
Sleep, 500
PostClick(120,73)
Sleep, 500
Step = 608
}
}
if(FormNumber = 56)
{
IfInString,NPCMsg,어떤 것을 도와 드릴까요
{
Sleep, 500
PostClick(135,70)
Sleep, 500
}
IfInString,NPCMsg,선택하세요
{
Sleep, 500
PostClick(134,57)
Sleep, 500
}
IfInString,NPCMsg,맞습니까
{
Sleep, 500
PostClick(123,69)
Sleep, 500
Step = 608
}
}
if(FormNumber = 44)
{
IfInString,NPCMsg,올리시겠습니까
{
Sleep, 500
PostClick(122,63)
Sleep, 500
}
}
if(FormNumber = 38)
{
IfWinNotActive,ahk_pid %jPID%
{
WinActivate, ahk_pid %jPID%
}
if(Gui_1Muba = 1 or Gui_2ButMuba = 1)
{
정보카운트 := 정보카운트 - 10
GuiControl, ,Gui_정보count, %정보카운트%
if(WeaponAbility1 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName1)
WeaponAbility1 = 0
Sleep, 1000
return
}
if(WeaponAbility2 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName2)
WeaponAbility2 = 0
Sleep, 1000
return
}
if(WeaponAbility3 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName3)
WeaponAbility3 = 0
Sleep, 1000
return
}
if(WeaponAbility4 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName4)
WeaponAbility4 = 0
Sleep, 1000
return
}
if(WeaponAbility5 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName5)
WeaponAbility5 = 0
Sleep, 1000
return
}
if(WeaponAbility6 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName6)
WeaponAbility6 = 0
Sleep, 1000
return
}
if(WeaponAbility7 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName7)
WeaponAbility7 = 0
Sleep, 1000
return
}
if(WeaponAbility8 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName8)
WeaponAbility8 = 0
Sleep, 1000
return
}
if(WeaponAbility9 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName9)
WeaponAbility9 = 0
Sleep, 1000
return
}
if(WeaponAbility10 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName10)
WeaponAbility10 = 0
Sleep, 1000
return
}
if(WeaponAbility11 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName11)
WeaponAbility11 = 0
Sleep, 1000
return
}
if(WeaponAbility12 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName12)
WeaponAbility12 = 0
Sleep, 1000
return
}
if(WeaponAbility13 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName13)
WeaponAbility13 = 0
Sleep, 1000
return
}
if(WeaponAbility14 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName14)
WeaponAbility14 = 0
Sleep, 1000
return
}
if(WeaponAbility15 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName15)
WeaponAbility15 = 0
Sleep, 1000
return
}
if(WeaponAbility16 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName16)
WeaponAbility16 = 0
Sleep, 1000
return
}
if(WeaponAbility17 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName17)
WeaponAbility17 = 0
Sleep, 1000
return
}
if(WeaponAbility18 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName18)
WeaponAbility18 = 0
Sleep, 1000
return
}
if(WeaponAbility19 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName19)
WeaponAbility19 = 0
Sleep, 1000
return
}
if(WeaponAbility20 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName20)
WeaponAbility20 = 0
Sleep, 1000
return
}
if(WeaponAbility21 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName21)
WeaponAbility21 = 0
Sleep, 1000
return
}
if(WeaponAbility22 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName22)
WeaponAbility22 = 0
Sleep, 1000
return
}
if(WeaponAbility23 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName23)
WeaponAbility23 = 0
Sleep, 1000
return
}
if(WeaponAbility24 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName24)
WeaponAbility24 = 0
Sleep, 1000
return
}
if(WeaponAbility25 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName25)
WeaponAbility25 = 0
Sleep, 1000
return
}
if(WeaponAbility26 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName26)
WeaponAbility26 = 0
Sleep, 1000
return
}
if(WeaponAbility27 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName27)
WeaponAbility27 = 0
Sleep, 1000
return
}
if(WeaponAbility28 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName28)
WeaponAbility28 = 0
Sleep, 1000
return
}
if(WeaponAbility29 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName29)
WeaponAbility29 = 0
Sleep, 1000
return
}
if(WeaponAbility30 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName30)
WeaponAbility30 = 0
Sleep, 1000
return
}
if(WeaponAbility31 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName31)
WeaponAbility31 = 0
Sleep, 1000
return
}
if(WeaponAbility32 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName32)
WeaponAbility32 = 0
Sleep, 1000
return
}
if(WeaponAbility33 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName33)
WeaponAbility33 = 0
Sleep, 1000
return
}
if(WeaponAbility34 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName34)
WeaponAbility34 = 0
Sleep, 1000
return
}
if(WeaponAbility35 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName35)
WeaponAbility35 = 0
Sleep, 1000
return
}
if(WeaponAbility36 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName36)
WeaponAbility36 = 0
Sleep, 1000
return
}
if(WeaponAbility37 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName37)
WeaponAbility37 = 0
Sleep, 1000
return
}
if(WeaponAbility38 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName38)
WeaponAbility38 = 0
Sleep, 1000
return
}
if(WeaponAbility39 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName39)
WeaponAbility39 = 0
Sleep, 1000
return
}
if(WeaponAbility40 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName40)
WeaponAbility40 = 0
Sleep, 1000
return
}
if(WeaponAbility41 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName41)
WeaponAbility41 = 0
Sleep, 1000
return
}
if(WeaponAbility42 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName42)
WeaponAbility42 = 0
Sleep, 1000
return
}
if(WeaponAbility43 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName43)
WeaponAbility43 = 0
Sleep, 1000
return
}
if(WeaponAbility44 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName44)
WeaponAbility44 = 0
Sleep, 1000
return
}
if(WeaponAbility45 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName45)
WeaponAbility45 = 0
Sleep, 1000
return
}
if(WeaponAbility46 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName46)
WeaponAbility46 = 0
Sleep, 1000
return
}
if(WeaponAbility47 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName47)
WeaponAbility47 = 0
Sleep, 1000
return
}
if(WeaponAbility48 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName48)
WeaponAbility48 = 0
Sleep, 1000
return
}
if(WeaponAbility49 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName49)
WeaponAbility49 = 0
Sleep, 1000
return
}
if(WeaponAbility50 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName50)
WeaponAbility50 = 0
Sleep, 1000
return
}
if(WeaponAbility51 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName51)
WeaponAbility51 = 0
Sleep, 1000
return
}
if(WeaponAbility52 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName52)
WeaponAbility52 = 0
Sleep, 1000
return
}
if(WeaponAbility53 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName53)
WeaponAbility53 = 0
Sleep, 1000
return
}
if(WeaponAbility54 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName54)
WeaponAbility54 = 0
Sleep, 1000
return
}
if(WeaponAbility55 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName55)
WeaponAbility55 = 0
Sleep, 1000
return
}
if(WeaponAbility56 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName56)
WeaponAbility56 = 0
Sleep, 1000
return
}
}
if(Gui_2Muba = 1 or Gui_3ButMuba = 1)
{
정보카운트 := 정보카운트 - 10
GuiControl, ,Gui_정보count, %정보카운트%
if(WeaponAbility1 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName1)
WeaponAbility1 = 0
Sleep, 1000
return
}
if(WeaponAbility2 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName2)
WeaponAbility2 = 0
Sleep, 1000
return
}
if(WeaponAbility3 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName3)
WeaponAbility3 = 0
Sleep, 1000
return
}
if(WeaponAbility4 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName4)
WeaponAbility4 = 0
Sleep, 1000
return
}
if(WeaponAbility5 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName5)
WeaponAbility5 = 0
Sleep, 1000
return
}
if(WeaponAbility6 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName6)
WeaponAbility6 = 0
Sleep, 1000
return
}
if(WeaponAbility7 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName7)
WeaponAbility7 = 0
Sleep, 1000
return
}
if(WeaponAbility8 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName8)
WeaponAbility8 = 0
Sleep, 1000
return
}
if(WeaponAbility9 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName9)
WeaponAbility9 = 0
Sleep, 1000
return
}
if(WeaponAbility10 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName10)
WeaponAbility10 = 0
Sleep, 1000
return
}
if(WeaponAbility11 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName11)
WeaponAbility11 = 0
Sleep, 1000
return
}
if(WeaponAbility12 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName12)
WeaponAbility12 = 0
Sleep, 1000
return
}
if(WeaponAbility13 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName13)
WeaponAbility13 = 0
Sleep, 1000
return
}
if(WeaponAbility14 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName14)
WeaponAbility14 = 0
Sleep, 1000
return
}
if(WeaponAbility15 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName15)
WeaponAbility15 = 0
Sleep, 1000
return
}
if(WeaponAbility16 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName16)
WeaponAbility16 = 0
Sleep, 1000
return
}
if(WeaponAbility17 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName17)
WeaponAbility17 = 0
Sleep, 1000
return
}
if(WeaponAbility18 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName18)
WeaponAbility18 = 0
Sleep, 1000
return
}
if(WeaponAbility19 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName19)
WeaponAbility19 = 0
Sleep, 1000
return
}
if(WeaponAbility20 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName20)
WeaponAbility20 = 0
Sleep, 1000
return
}
if(WeaponAbility21 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName21)
WeaponAbility21 = 0
Sleep, 1000
return
}
if(WeaponAbility22 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName22)
WeaponAbility22 = 0
Sleep, 1000
return
}
if(WeaponAbility23 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName23)
WeaponAbility23 = 0
Sleep, 1000
return
}
if(WeaponAbility24 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName24)
WeaponAbility24 = 0
Sleep, 1000
return
}
if(WeaponAbility25 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName25)
WeaponAbility25 = 0
Sleep, 1000
return
}
if(WeaponAbility26 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName26)
WeaponAbility26 = 0
Sleep, 1000
return
}
if(WeaponAbility27 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName27)
WeaponAbility27 = 0
Sleep, 1000
return
}
if(WeaponAbility28 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName28)
WeaponAbility28 = 0
Sleep, 1000
return
}
if(WeaponAbility29 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName29)
WeaponAbility29 = 0
Sleep, 1000
return
}
if(WeaponAbility30 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName30)
WeaponAbility30 = 0
Sleep, 1000
return
}
if(WeaponAbility31 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName31)
WeaponAbility31 = 0
Sleep, 1000
return
}
if(WeaponAbility32 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName32)
WeaponAbility32 = 0
Sleep, 1000
return
}
if(WeaponAbility33 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName33)
WeaponAbility33 = 0
Sleep, 1000
return
}
if(WeaponAbility34 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName34)
WeaponAbility34 = 0
Sleep, 1000
return
}
if(WeaponAbility35 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName35)
WeaponAbility35 = 0
Sleep, 1000
return
}
if(WeaponAbility36 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName36)
WeaponAbility36 = 0
Sleep, 1000
return
}
if(WeaponAbility37 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName37)
WeaponAbility37 = 0
Sleep, 1000
return
}
if(WeaponAbility38 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName38)
WeaponAbility38 = 0
Sleep, 1000
return
}
if(WeaponAbility39 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName39)
WeaponAbility39 = 0
Sleep, 1000
return
}
if(WeaponAbility40 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName40)
WeaponAbility40 = 0
Sleep, 1000
return
}
if(WeaponAbility41 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName41)
WeaponAbility41 = 0
Sleep, 1000
return
}
if(WeaponAbility42 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName42)
WeaponAbility42 = 0
Sleep, 1000
return
}
if(WeaponAbility43 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName43)
WeaponAbility43 = 0
Sleep, 1000
return
}
if(WeaponAbility44 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName44)
WeaponAbility44 = 0
Sleep, 1000
return
}
if(WeaponAbility45 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName45)
WeaponAbility45 = 0
Sleep, 1000
return
}
if(WeaponAbility46 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName46)
WeaponAbility46 = 0
Sleep, 1000
return
}
if(WeaponAbility47 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName47)
WeaponAbility47 = 0
Sleep, 1000
return
}
if(WeaponAbility48 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName48)
WeaponAbility48 = 0
Sleep, 1000
return
}
if(WeaponAbility49 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName49)
WeaponAbility49 = 0
Sleep, 1000
return
}
if(WeaponAbility50 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName50)
WeaponAbility50 = 0
Sleep, 1000
return
}
if(WeaponAbility51 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName51)
WeaponAbility51 = 0
Sleep, 1000
return
}
if(WeaponAbility52 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName52)
WeaponAbility52 = 0
Sleep, 1000
return
}
if(WeaponAbility53 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName53)
WeaponAbility53 = 0
Sleep, 1000
return
}
if(WeaponAbility54 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName54)
WeaponAbility54 = 0
Sleep, 1000
return
}
if(WeaponAbility55 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName55)
WeaponAbility55 = 0
Sleep, 1000
return
}
if(WeaponAbility56 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName56)
WeaponAbility56 = 0
Sleep, 1000
return
}
}
if(Gui_3Muba = 1 or Gui_4ButMuba = 1)
{
정보카운트 := 정보카운트 - 10
GuiControl, ,Gui_정보count, %정보카운트%
if(WeaponAbility1 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName1)
WeaponAbility1 = 0
Sleep, 1000
return
}
if(WeaponAbility2 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName2)
WeaponAbility2 = 0
Sleep, 1000
return
}
if(WeaponAbility3 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName3)
WeaponAbility3 = 0
Sleep, 1000
return
}
if(WeaponAbility4 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName4)
WeaponAbility4 = 0
Sleep, 1000
return
}
if(WeaponAbility5 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName5)
WeaponAbility5 = 0
Sleep, 1000
return
}
if(WeaponAbility6 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName6)
WeaponAbility6 = 0
Sleep, 1000
return
}
if(WeaponAbility7 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName7)
WeaponAbility7 = 0
Sleep, 1000
return
}
if(WeaponAbility8 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName8)
WeaponAbility8 = 0
Sleep, 1000
return
}
if(WeaponAbility9 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName9)
WeaponAbility9 = 0
Sleep, 1000
return
}
if(WeaponAbility10 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName10)
WeaponAbility10 = 0
Sleep, 1000
return
}
if(WeaponAbility11 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName11)
WeaponAbility11 = 0
Sleep, 1000
return
}
if(WeaponAbility12 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName12)
WeaponAbility12 = 0
Sleep, 1000
return
}
if(WeaponAbility13 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName13)
WeaponAbility13 = 0
Sleep, 1000
return
}
if(WeaponAbility14 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName14)
WeaponAbility14 = 0
Sleep, 1000
return
}
if(WeaponAbility15 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName15)
WeaponAbility15 = 0
Sleep, 1000
return
}
if(WeaponAbility16 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName16)
WeaponAbility16 = 0
Sleep, 1000
return
}
if(WeaponAbility17 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName17)
WeaponAbility17 = 0
Sleep, 1000
return
}
if(WeaponAbility18 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName18)
WeaponAbility18 = 0
Sleep, 1000
return
}
if(WeaponAbility19 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName19)
WeaponAbility19 = 0
Sleep, 1000
return
}
if(WeaponAbility20 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName20)
WeaponAbility20 = 0
Sleep, 1000
return
}
if(WeaponAbility21 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName21)
WeaponAbility21 = 0
Sleep, 1000
return
}
if(WeaponAbility22 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName22)
WeaponAbility22 = 0
Sleep, 1000
return
}
if(WeaponAbility23 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName23)
WeaponAbility23 = 0
Sleep, 1000
return
}
if(WeaponAbility24 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName24)
WeaponAbility24 = 0
Sleep, 1000
return
}
if(WeaponAbility25 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName25)
WeaponAbility25 = 0
Sleep, 1000
return
}
if(WeaponAbility26 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName26)
WeaponAbility26 = 0
Sleep, 1000
return
}
if(WeaponAbility27 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName27)
WeaponAbility27 = 0
Sleep, 1000
return
}
if(WeaponAbility28 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName28)
WeaponAbility28 = 0
Sleep, 1000
return
}
if(WeaponAbility29 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName29)
WeaponAbility29 = 0
Sleep, 1000
return
}
if(WeaponAbility30 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName30)
WeaponAbility30 = 0
Sleep, 1000
return
}
if(WeaponAbility31 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName31)
WeaponAbility31 = 0
Sleep, 1000
return
}
if(WeaponAbility32 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName32)
WeaponAbility32 = 0
Sleep, 1000
return
}
if(WeaponAbility33 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName33)
WeaponAbility33 = 0
Sleep, 1000
return
}
if(WeaponAbility34 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName34)
WeaponAbility34 = 0
Sleep, 1000
return
}
if(WeaponAbility35 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName35)
WeaponAbility35 = 0
Sleep, 1000
return
}
if(WeaponAbility36 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName36)
WeaponAbility36 = 0
Sleep, 1000
return
}
if(WeaponAbility37 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName37)
WeaponAbility37 = 0
Sleep, 1000
return
}
if(WeaponAbility38 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName38)
WeaponAbility38 = 0
Sleep, 1000
return
}
if(WeaponAbility39 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName39)
WeaponAbility39 = 0
Sleep, 1000
return
}
if(WeaponAbility40 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName40)
WeaponAbility40 = 0
Sleep, 1000
return
}
if(WeaponAbility41 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName41)
WeaponAbility41 = 0
Sleep, 1000
return
}
if(WeaponAbility42 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName42)
WeaponAbility42 = 0
Sleep, 1000
return
}
if(WeaponAbility43 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName43)
WeaponAbility43 = 0
Sleep, 1000
return
}
if(WeaponAbility44 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName44)
WeaponAbility44 = 0
Sleep, 1000
return
}
if(WeaponAbility45 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName45)
WeaponAbility45 = 0
Sleep, 1000
return
}
if(WeaponAbility46 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName46)
WeaponAbility46 = 0
Sleep, 1000
return
}
if(WeaponAbility47 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName47)
WeaponAbility47 = 0
Sleep, 1000
return
}
if(WeaponAbility48 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName48)
WeaponAbility48 = 0
Sleep, 1000
return
}
if(WeaponAbility49 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName49)
WeaponAbility49 = 0
Sleep, 1000
return
}
if(WeaponAbility50 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName50)
WeaponAbility50 = 0
Sleep, 1000
return
}
if(WeaponAbility51 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName51)
WeaponAbility51 = 0
Sleep, 1000
return
}
if(WeaponAbility52 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName52)
WeaponAbility52 = 0
Sleep, 1000
return
}
if(WeaponAbility53 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName53)
WeaponAbility53 = 0
Sleep, 1000
return
}
if(WeaponAbility54 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName54)
WeaponAbility54 = 0
Sleep, 1000
return
}
if(WeaponAbility55 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName55)
WeaponAbility55 = 0
Sleep, 1000
return
}
if(WeaponAbility56 = 10000)
{
Sleep, 1000
SendWeaponName(Gui_WeaponName56)
WeaponAbility56 = 0
Sleep, 1000
return
}
}
}
}
if(Step = 608)
{
SB_SetText("그레이드 할 어빌리티 체크 중")
if(Gui_1Muba = 1)
{
if(WeaponAbility1 = 10000 or WeaponAbility2 = 10000 or WeaponAbility3 = 10000 or WeaponAbility4 = 10000 or WeaponAbility5 = 10000 or WeaponAbility6 = 10000 or WeaponAbility7 = 10000 or WeaponAbility8 = 10000 or WeaponAbility9 = 10000  or WeaponAbility10 = 10000  or WeaponAbility11 = 10000  or WeaponAbility12 = 10000  or WeaponAbility13 = 10000  or WeaponAbility14 = 10000  or WeaponAbility15 = 10000  or WeaponAbility16 = 10000  or WeaponAbility17 = 10000  or WeaponAbility18 = 10000  or WeaponAbility19 = 10000  or WeaponAbility20 = 10000  or WeaponAbility21 = 10000  or WeaponAbility22 = 10000  or WeaponAbility23 = 10000  or WeaponAbility24 = 10000  or WeaponAbility25 = 10000 or WeaponAbility26 = 10000 or WeaponAbility27 = 10000 or WeaponAbility28 = 10000 or WeaponAbility29 = 10000 or WeaponAbility30 = 10000 or WeaponAbility31 = 10000 or WeaponAbility32 = 10000 or WeaponAbility33 = 10000 or WeaponAbility34 = 10000 or WeaponAbility35 = 10000 or WeaponAbility36 = 10000 or WeaponAbility37 = 10000  or WeaponAbility38 = 10000  or WeaponAbility39 = 10000  or WeaponAbility40 = 10000  or WeaponAbility41 = 10000  or WeaponAbility42 = 10000  or WeaponAbility43 = 10000  or WeaponAbility44 = 10000  or WeaponAbility45 = 10000  or WeaponAbility46 = 10000  or WeaponAbility47 = 10000  or WeaponAbility48 = 10000  or WeaponAbility49 = 10000  or WeaponAbility50 = 10000  or WeaponAbility51 = 10000  or WeaponAbility52 = 10000  or WeaponAbility53 = 10000 or WeaponAbility54 = 10000 or WeaponAbility55 = 10000 or WeaponAbility56 = 10000)
{
Step = 605
}
if(WeaponAbility1 != 10000 and WeaponAbility2 != 10000 and WeaponAbility3 != 10000 and WeaponAbility4 != 10000 and WeaponAbility5 != 10000 and WeaponAbility6 != 10000 and WeaponAbility7 != 10000 and WeaponAbility8 != 10000 and WeaponAbility9 != 10000 and WeaponAbility10 != 10000 and WeaponAbility11 != 10000 and WeaponAbility12 != 10000 and WeaponAbility13 != 10000 and WeaponAbility14 != 10000 and WeaponAbility15 != 10000 and WeaponAbility16 != 10000 and WeaponAbility17 != 10000 and WeaponAbility18 != 10000 and WeaponAbility19 != 10000 and WeaponAbility20 != 10000 and WeaponAbility21 != 10000 and WeaponAbility22 != 10000 and WeaponAbility23 != 10000 and WeaponAbility24 != 10000 and WeaponAbility25 != 10000 and WeaponAbility26 != 10000 and WeaponAbility27 != 10000 and WeaponAbility28 != 10000 and WeaponAbility29 != 10000 and WeaponAbility30 != 10000 and WeaponAbility31 != 10000 and WeaponAbility32 != 10000 and WeaponAbility33 != 10000 and WeaponAbility34 != 10000 and WeaponAbility35 != 10000 and WeaponAbility36 != 10000 and WeaponAbility37 != 10000 and WeaponAbility38 != 10000 and WeaponAbility39 != 10000 and WeaponAbility40 != 10000 and WeaponAbility41 != 10000 and WeaponAbility42 != 10000 and WeaponAbility43 != 10000 and WeaponAbility44 != 10000 and WeaponAbility45 != 10000 and WeaponAbility46 != 10000 and WeaponAbility47 != 10000 and WeaponAbility48 != 10000 and WeaponAbility49 != 10000 and WeaponAbility50 != 10000 and WeaponAbility51 != 10000 and WeaponAbility52 != 10000 and WeaponAbility53 != 10000 and WeaponAbility54 != 10000 and WeaponAbility55 != 10000 and WeaponAbility56 != 10000)
{
Step = 609
}
}
if(Gui_2Muba = 1)
{
if(WeaponAbility1 = 10000 or WeaponAbility2 = 10000 or WeaponAbility3 = 10000 or WeaponAbility4 = 10000 or WeaponAbility5 = 10000 or WeaponAbility6 = 10000 or WeaponAbility7 = 10000 or WeaponAbility8 = 10000 or WeaponAbility9 = 10000  or WeaponAbility10 = 10000  or WeaponAbility11 = 10000  or WeaponAbility12 = 10000  or WeaponAbility13 = 10000  or WeaponAbility14 = 10000  or WeaponAbility15 = 10000  or WeaponAbility16 = 10000  or WeaponAbility17 = 10000  or WeaponAbility18 = 10000  or WeaponAbility19 = 10000  or WeaponAbility20 = 10000  or WeaponAbility21 = 10000  or WeaponAbility22 = 10000  or WeaponAbility23 = 10000  or WeaponAbility24 = 10000  or WeaponAbility25 = 10000 or WeaponAbility26 = 10000 or WeaponAbility27 = 10000 or WeaponAbility28 = 10000 or WeaponAbility29 = 10000 or WeaponAbility30 = 10000 or WeaponAbility31 = 10000 or WeaponAbility32 = 10000 or WeaponAbility33 = 10000 or WeaponAbility34 = 10000 or WeaponAbility35 = 10000 or WeaponAbility36 = 10000 or WeaponAbility37 = 10000  or WeaponAbility38 = 10000  or WeaponAbility39 = 10000  or WeaponAbility40 = 10000  or WeaponAbility41 = 10000  or WeaponAbility42 = 10000  or WeaponAbility43 = 10000  or WeaponAbility44 = 10000  or WeaponAbility45 = 10000  or WeaponAbility46 = 10000  or WeaponAbility47 = 10000  or WeaponAbility48 = 10000  or WeaponAbility49 = 10000  or WeaponAbility50 = 10000  or WeaponAbility51 = 10000  or WeaponAbility52 = 10000  or WeaponAbility53 = 10000 or WeaponAbility54 = 10000 or WeaponAbility55 = 10000 or WeaponAbility56 = 10000)
{
Step = 605
}
if(WeaponAbility1 != 10000 and WeaponAbility2 != 10000 and WeaponAbility3 != 10000 and WeaponAbility4 != 10000 and WeaponAbility5 != 10000 and WeaponAbility6 != 10000 and WeaponAbility7 != 10000 and WeaponAbility8 != 10000 and WeaponAbility9 != 10000 and WeaponAbility10 != 10000 and WeaponAbility11 != 10000 and WeaponAbility12 != 10000 and WeaponAbility13 != 10000 and WeaponAbility14 != 10000 and WeaponAbility15 != 10000 and WeaponAbility16 != 10000 and WeaponAbility17 != 10000 and WeaponAbility18 != 10000 and WeaponAbility19 != 10000 and WeaponAbility20 != 10000 and WeaponAbility21 != 10000 and WeaponAbility22 != 10000 and WeaponAbility23 != 10000 and WeaponAbility24 != 10000 and WeaponAbility25 != 10000 and WeaponAbility26 != 10000 and WeaponAbility27 != 10000 and WeaponAbility28 != 10000 and WeaponAbility29 != 10000 and WeaponAbility30 != 10000 and WeaponAbility31 != 10000 and WeaponAbility32 != 10000 and WeaponAbility33 != 10000 and WeaponAbility34 != 10000 and WeaponAbility35 != 10000 and WeaponAbility36 != 10000 and WeaponAbility37 != 10000 and WeaponAbility38 != 10000 and WeaponAbility39 != 10000 and WeaponAbility40 != 10000 and WeaponAbility41 != 10000 and WeaponAbility42 != 10000 and WeaponAbility43 != 10000 and WeaponAbility44 != 10000 and WeaponAbility45 != 10000 and WeaponAbility46 != 10000 and WeaponAbility47 != 10000 and WeaponAbility48 != 10000 and WeaponAbility49 != 10000 and WeaponAbility50 != 10000 and WeaponAbility51 != 10000 and WeaponAbility52 != 10000 and WeaponAbility53 != 10000 and WeaponAbility54 != 10000 and WeaponAbility55 != 10000 and WeaponAbility56 != 10000)
{
Step = 609
}
}
if(Gui_3Muba = 1)
{
if(WeaponAbility1 = 10000 or WeaponAbility2 = 10000 or WeaponAbility3 = 10000 or WeaponAbility4 = 10000 or WeaponAbility5 = 10000 or WeaponAbility6 = 10000 or WeaponAbility7 = 10000 or WeaponAbility8 = 10000 or WeaponAbility9 = 10000  or WeaponAbility10 = 10000  or WeaponAbility11 = 10000  or WeaponAbility12 = 10000  or WeaponAbility13 = 10000  or WeaponAbility14 = 10000  or WeaponAbility15 = 10000  or WeaponAbility16 = 10000  or WeaponAbility17 = 10000  or WeaponAbility18 = 10000  or WeaponAbility19 = 10000  or WeaponAbility20 = 10000  or WeaponAbility21 = 10000  or WeaponAbility22 = 10000  or WeaponAbility23 = 10000  or WeaponAbility24 = 10000  or WeaponAbility25 = 10000 or WeaponAbility26 = 10000 or WeaponAbility27 = 10000 or WeaponAbility28 = 10000 or WeaponAbility29 = 10000 or WeaponAbility30 = 10000 or WeaponAbility31 = 10000 or WeaponAbility32 = 10000 or WeaponAbility33 = 10000 or WeaponAbility34 = 10000 or WeaponAbility35 = 10000 or WeaponAbility36 = 10000 or WeaponAbility37 = 10000  or WeaponAbility38 = 10000  or WeaponAbility39 = 10000  or WeaponAbility40 = 10000  or WeaponAbility41 = 10000  or WeaponAbility42 = 10000  or WeaponAbility43 = 10000  or WeaponAbility44 = 10000  or WeaponAbility45 = 10000  or WeaponAbility46 = 10000  or WeaponAbility47 = 10000  or WeaponAbility48 = 10000  or WeaponAbility49 = 10000  or WeaponAbility50 = 10000  or WeaponAbility51 = 10000  or WeaponAbility52 = 10000  or WeaponAbility53 = 10000 or WeaponAbility54 = 10000 or WeaponAbility55 = 10000 or WeaponAbility56 = 10000)
{
Step = 605
}
if(WeaponAbility1 != 10000 and WeaponAbility2 != 10000 and WeaponAbility3 != 10000 and WeaponAbility4 != 10000 and WeaponAbility5 != 10000 and WeaponAbility6 != 10000 and WeaponAbility7 != 10000 and WeaponAbility8 != 10000 and WeaponAbility9 != 10000 and WeaponAbility10 != 10000 and WeaponAbility11 != 10000 and WeaponAbility12 != 10000 and WeaponAbility13 != 10000 and WeaponAbility14 != 10000 and WeaponAbility15 != 10000 and WeaponAbility16 != 10000 and WeaponAbility17 != 10000 and WeaponAbility18 != 10000 and WeaponAbility19 != 10000 and WeaponAbility20 != 10000 and WeaponAbility21 != 10000 and WeaponAbility22 != 10000 and WeaponAbility23 != 10000 and WeaponAbility24 != 10000 and WeaponAbility25 != 10000 and WeaponAbility26 != 10000 and WeaponAbility27 != 10000 and WeaponAbility28 != 10000 and WeaponAbility29 != 10000 and WeaponAbility30 != 10000 and WeaponAbility31 != 10000 and WeaponAbility32 != 10000 and WeaponAbility33 != 10000 and WeaponAbility34 != 10000 and WeaponAbility35 != 10000 and WeaponAbility36 != 10000 and WeaponAbility37 != 10000 and WeaponAbility38 != 10000 and WeaponAbility39 != 10000 and WeaponAbility40 != 10000 and WeaponAbility41 != 10000 and WeaponAbility42 != 10000 and WeaponAbility43 != 10000 and WeaponAbility44 != 10000 and WeaponAbility45 != 10000 and WeaponAbility46 != 10000 and WeaponAbility47 != 10000 and WeaponAbility48 != 10000 and WeaponAbility49 != 10000 and WeaponAbility50 != 10000 and WeaponAbility51 != 10000 and WeaponAbility52 != 10000 and WeaponAbility53 != 10000 and WeaponAbility54 != 10000 and WeaponAbility55 != 10000 and WeaponAbility56 != 10000)
{
Step = 609
}
}
if(Gui_2ButMuba = 1)
{
if(WeaponAbility1 = 10000 or WeaponAbility2 = 10000 or WeaponAbility3 = 10000 or WeaponAbility4 = 10000 or WeaponAbility5 = 10000 or WeaponAbility6 = 10000 or WeaponAbility7 = 10000 or WeaponAbility8 = 10000 or WeaponAbility9 = 10000  or WeaponAbility10 = 10000  or WeaponAbility11 = 10000  or WeaponAbility12 = 10000  or WeaponAbility13 = 10000  or WeaponAbility14 = 10000  or WeaponAbility15 = 10000  or WeaponAbility16 = 10000  or WeaponAbility17 = 10000  or WeaponAbility18 = 10000  or WeaponAbility19 = 10000  or WeaponAbility20 = 10000  or WeaponAbility21 = 10000  or WeaponAbility22 = 10000  or WeaponAbility23 = 10000  or WeaponAbility24 = 10000  or WeaponAbility25 = 10000 or WeaponAbility26 = 10000 or WeaponAbility27 = 10000 or WeaponAbility28 = 10000 or WeaponAbility29 = 10000 or WeaponAbility30 = 10000 or WeaponAbility31 = 10000 or WeaponAbility32 = 10000 or WeaponAbility33 = 10000 or WeaponAbility34 = 10000 or WeaponAbility35 = 10000 or WeaponAbility36 = 10000 or WeaponAbility37 = 10000  or WeaponAbility38 = 10000  or WeaponAbility39 = 10000  or WeaponAbility40 = 10000  or WeaponAbility41 = 10000  or WeaponAbility42 = 10000  or WeaponAbility43 = 10000  or WeaponAbility44 = 10000  or WeaponAbility45 = 10000  or WeaponAbility46 = 10000  or WeaponAbility47 = 10000  or WeaponAbility48 = 10000  or WeaponAbility49 = 10000  or WeaponAbility50 = 10000  or WeaponAbility51 = 10000  or WeaponAbility52 = 10000  or WeaponAbility53 = 10000 or WeaponAbility54 = 10000 or WeaponAbility55 = 10000 or WeaponAbility56 = 10000)
{
Step = 605
}
if(WeaponAbility1 != 10000 and WeaponAbility2 != 10000 and WeaponAbility3 != 10000 and WeaponAbility4 != 10000 and WeaponAbility5 != 10000 and WeaponAbility6 != 10000 and WeaponAbility7 != 10000 and WeaponAbility8 != 10000 and WeaponAbility9 != 10000 and WeaponAbility10 != 10000 and WeaponAbility11 != 10000 and WeaponAbility12 != 10000 and WeaponAbility13 != 10000 and WeaponAbility14 != 10000 and WeaponAbility15 != 10000 and WeaponAbility16 != 10000 and WeaponAbility17 != 10000 and WeaponAbility18 != 10000 and WeaponAbility19 != 10000 and WeaponAbility20 != 10000 and WeaponAbility21 != 10000 and WeaponAbility22 != 10000 and WeaponAbility23 != 10000 and WeaponAbility24 != 10000 and WeaponAbility25 != 10000 and WeaponAbility26 != 10000 and WeaponAbility27 != 10000 and WeaponAbility28 != 10000 and WeaponAbility29 != 10000 and WeaponAbility30 != 10000 and WeaponAbility31 != 10000 and WeaponAbility32 != 10000 and WeaponAbility33 != 10000 and WeaponAbility34 != 10000 and WeaponAbility35 != 10000 and WeaponAbility36 != 10000 and WeaponAbility37 != 10000 and WeaponAbility38 != 10000 and WeaponAbility39 != 10000 and WeaponAbility40 != 10000 and WeaponAbility41 != 10000 and WeaponAbility42 != 10000 and WeaponAbility43 != 10000 and WeaponAbility44 != 10000 and WeaponAbility45 != 10000 and WeaponAbility46 != 10000 and WeaponAbility47 != 10000 and WeaponAbility48 != 10000 and WeaponAbility49 != 10000 and WeaponAbility50 != 10000 and WeaponAbility51 != 10000 and WeaponAbility52 != 10000 and WeaponAbility53 != 10000 and WeaponAbility54 != 10000 and WeaponAbility55 != 10000 and WeaponAbility56 != 10000)
{
Step = 609
}
}
if(Gui_3ButMuba = 1)
{
if(WeaponAbility1 = 10000 or WeaponAbility2 = 10000 or WeaponAbility3 = 10000 or WeaponAbility4 = 10000 or WeaponAbility5 = 10000 or WeaponAbility6 = 10000 or WeaponAbility7 = 10000 or WeaponAbility8 = 10000 or WeaponAbility9 = 10000  or WeaponAbility10 = 10000  or WeaponAbility11 = 10000  or WeaponAbility12 = 10000  or WeaponAbility13 = 10000  or WeaponAbility14 = 10000  or WeaponAbility15 = 10000  or WeaponAbility16 = 10000  or WeaponAbility17 = 10000  or WeaponAbility18 = 10000  or WeaponAbility19 = 10000  or WeaponAbility20 = 10000  or WeaponAbility21 = 10000  or WeaponAbility22 = 10000  or WeaponAbility23 = 10000  or WeaponAbility24 = 10000  or WeaponAbility25 = 10000 or WeaponAbility26 = 10000 or WeaponAbility27 = 10000 or WeaponAbility28 = 10000 or WeaponAbility29 = 10000 or WeaponAbility30 = 10000 or WeaponAbility31 = 10000 or WeaponAbility32 = 10000 or WeaponAbility33 = 10000 or WeaponAbility34 = 10000 or WeaponAbility35 = 10000 or WeaponAbility36 = 10000 or WeaponAbility37 = 10000  or WeaponAbility38 = 10000  or WeaponAbility39 = 10000  or WeaponAbility40 = 10000  or WeaponAbility41 = 10000  or WeaponAbility42 = 10000  or WeaponAbility43 = 10000  or WeaponAbility44 = 10000  or WeaponAbility45 = 10000  or WeaponAbility46 = 10000  or WeaponAbility47 = 10000  or WeaponAbility48 = 10000  or WeaponAbility49 = 10000  or WeaponAbility50 = 10000  or WeaponAbility51 = 10000  or WeaponAbility52 = 10000  or WeaponAbility53 = 10000 or WeaponAbility54 = 10000 or WeaponAbility55 = 10000 or WeaponAbility56 = 10000)
{
Step = 605
}
if(WeaponAbility1 != 10000 and WeaponAbility2 != 10000 and WeaponAbility3 != 10000 and WeaponAbility4 != 10000 and WeaponAbility5 != 10000 and WeaponAbility6 != 10000 and WeaponAbility7 != 10000 and WeaponAbility8 != 10000 and WeaponAbility9 != 10000 and WeaponAbility10 != 10000 and WeaponAbility11 != 10000 and WeaponAbility12 != 10000 and WeaponAbility13 != 10000 and WeaponAbility14 != 10000 and WeaponAbility15 != 10000 and WeaponAbility16 != 10000 and WeaponAbility17 != 10000 and WeaponAbility18 != 10000 and WeaponAbility19 != 10000 and WeaponAbility20 != 10000 and WeaponAbility21 != 10000 and WeaponAbility22 != 10000 and WeaponAbility23 != 10000 and WeaponAbility24 != 10000 and WeaponAbility25 != 10000 and WeaponAbility26 != 10000 and WeaponAbility27 != 10000 and WeaponAbility28 != 10000 and WeaponAbility29 != 10000 and WeaponAbility30 != 10000 and WeaponAbility31 != 10000 and WeaponAbility32 != 10000 and WeaponAbility33 != 10000 and WeaponAbility34 != 10000 and WeaponAbility35 != 10000 and WeaponAbility36 != 10000 and WeaponAbility37 != 10000 and WeaponAbility38 != 10000 and WeaponAbility39 != 10000 and WeaponAbility40 != 10000 and WeaponAbility41 != 10000 and WeaponAbility42 != 10000 and WeaponAbility43 != 10000 and WeaponAbility44 != 10000 and WeaponAbility45 != 10000 and WeaponAbility46 != 10000 and WeaponAbility47 != 10000 and WeaponAbility48 != 10000 and WeaponAbility49 != 10000 and WeaponAbility50 != 10000 and WeaponAbility51 != 10000 and WeaponAbility52 != 10000 and WeaponAbility53 != 10000 and WeaponAbility54 != 10000 and WeaponAbility55 != 10000 and WeaponAbility56 != 10000)
{
Step = 609
}
}
if(Gui_4ButMuba = 1)
{
if(WeaponAbility1 = 10000 or WeaponAbility2 = 10000 or WeaponAbility3 = 10000 or WeaponAbility4 = 10000 or WeaponAbility5 = 10000 or WeaponAbility6 = 10000 or WeaponAbility7 = 10000 or WeaponAbility8 = 10000 or WeaponAbility9 = 10000  or WeaponAbility10 = 10000  or WeaponAbility11 = 10000  or WeaponAbility12 = 10000  or WeaponAbility13 = 10000  or WeaponAbility14 = 10000  or WeaponAbility15 = 10000  or WeaponAbility16 = 10000  or WeaponAbility17 = 10000  or WeaponAbility18 = 10000  or WeaponAbility19 = 10000  or WeaponAbility20 = 10000  or WeaponAbility21 = 10000  or WeaponAbility22 = 10000  or WeaponAbility23 = 10000  or WeaponAbility24 = 10000  or WeaponAbility25 = 10000 or WeaponAbility26 = 10000 or WeaponAbility27 = 10000 or WeaponAbility28 = 10000 or WeaponAbility29 = 10000 or WeaponAbility30 = 10000 or WeaponAbility31 = 10000 or WeaponAbility32 = 10000 or WeaponAbility33 = 10000 or WeaponAbility34 = 10000 or WeaponAbility35 = 10000 or WeaponAbility36 = 10000 or WeaponAbility37 = 10000  or WeaponAbility38 = 10000  or WeaponAbility39 = 10000  or WeaponAbility40 = 10000  or WeaponAbility41 = 10000  or WeaponAbility42 = 10000  or WeaponAbility43 = 10000  or WeaponAbility44 = 10000  or WeaponAbility45 = 10000  or WeaponAbility46 = 10000  or WeaponAbility47 = 10000  or WeaponAbility48 = 10000  or WeaponAbility49 = 10000  or WeaponAbility50 = 10000  or WeaponAbility51 = 10000  or WeaponAbility52 = 10000  or WeaponAbility53 = 10000 or WeaponAbility54 = 10000 or WeaponAbility55 = 10000 or WeaponAbility56 = 10000)
{
Step = 605
}
if(WeaponAbility1 != 10000 and WeaponAbility2 != 10000 and WeaponAbility3 != 10000 and WeaponAbility4 != 10000 and WeaponAbility5 != 10000 and WeaponAbility6 != 10000 and WeaponAbility7 != 10000 and WeaponAbility8 != 10000 and WeaponAbility9 != 10000 and WeaponAbility10 != 10000 and WeaponAbility11 != 10000 and WeaponAbility12 != 10000 and WeaponAbility13 != 10000 and WeaponAbility14 != 10000 and WeaponAbility15 != 10000 and WeaponAbility16 != 10000 and WeaponAbility17 != 10000 and WeaponAbility18 != 10000 and WeaponAbility19 != 10000 and WeaponAbility20 != 10000 and WeaponAbility21 != 10000 and WeaponAbility22 != 10000 and WeaponAbility23 != 10000 and WeaponAbility24 != 10000 and WeaponAbility25 != 10000 and WeaponAbility26 != 10000 and WeaponAbility27 != 10000 and WeaponAbility28 != 10000 and WeaponAbility29 != 10000 and WeaponAbility30 != 10000 and WeaponAbility31 != 10000 and WeaponAbility32 != 10000 and WeaponAbility33 != 10000 and WeaponAbility34 != 10000 and WeaponAbility35 != 10000 and WeaponAbility36 != 10000 and WeaponAbility37 != 10000 and WeaponAbility38 != 10000 and WeaponAbility39 != 10000 and WeaponAbility40 != 10000 and WeaponAbility41 != 10000 and WeaponAbility42 != 10000 and WeaponAbility43 != 10000 and WeaponAbility44 != 10000 and WeaponAbility45 != 10000 and WeaponAbility46 != 10000 and WeaponAbility47 != 10000 and WeaponAbility48 != 10000 and WeaponAbility49 != 10000 and WeaponAbility50 != 10000 and WeaponAbility51 != 10000 and WeaponAbility52 != 10000 and WeaponAbility53 != 10000 and WeaponAbility54 != 10000 and WeaponAbility55 != 10000 and WeaponAbility56 != 10000)
{
Step = 609
}
}
}
if(Step = 609)
{
SB_SetText("신전 밖으로 이동 중")
Set_MoveSpeed()
Loop,3
{
PostMessage, 0x100, 40, 22020097, , ahk_pid %jPID%
PostMessage, 0x101, 40, 22020097, , ahk_pid %jPID%
Sleep, 500
}
Step = 610
}
if(Step = 610)
{
SB_SetText("현재위치가 신전 밖인지 체크 중")
Get_Location()
IfInString,Location,신전
{
AltR()
Step = 609
}
IfNotInString,Location,신전
{
if(Gui_HuntAuto = 1)
{
if(Gui_1Muba = 1)
{
if(BWValue1 < Gui_LimitAbility1)
{
HuntPlace = 1
Step = 8
}
if(BWValue1 >= Gui_LimitAbility1)
{
HuntPlace = 2
Step = 8
}
}
if(Gui_2Muba = 1)
{
if(BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2)
{
HuntPlace = 1
Step = 8
}
if(BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2)
{
HuntPlace = 2
Step = 8
}
}
if(Gui_3Muba = 1)
{
if(BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2 and BWValue3 < Gui_LimitAbility3)
{
HuntPlace = 1
Step = 8
}
if(BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2 or BWValue3 >= Gui_LimitAbility3)
{
HuntPlace = 2
Step = 8
}
}
if(Gui_2ButMuba = 1)
{
if(BWValue0 < Gui_LimitAbility0 and BWValue1 < Gui_LimitAbility1)
{
HuntPlace = 1
Step = 8
}
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1)
{
HuntPlace = 2
Step = 8
}
}
if(Gui_3ButMuba = 1)
{
if(BWValue0 < Gui_LimitAbility0 and BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2)
{
HuntPlace = 1
Step = 8
}
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2)
{
HuntPlace = 2
Step = 8
}
}
if(Gui_4ButMuba = 1)
{
if(BWValue0 < Gui_LimitAbility0 and BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2 and BWValue3 < Gui_LimitAbility3)
{
HuntPlace = 1
Step = 8
}
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2 or BWValue3 >= Gui_LimitAbility3)
{
HuntPlace = 2
Step = 8
}
}
}
if(Gui_HuntPonam = 1)
{
HuntPlace = 1
Step = 11
}
if(Gui_HuntPobuk = 1)
{
HuntPlace = 2
Step = 1002
}
}
}
if(Step = 650)
{
GuiControl, , Gui_NowState, [신전/성당] 기도하러 가는 중.
SB_SetText("스펠 그레이드 하러 가는 중")
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 500
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 651
return
}
if(CountPortal = 1)
{
PostClick(645,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 651
return
}
if(CountPortal = 2)
{
PostClick(660,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 651
return
}
}
}
if(Step = 651)
{
Check_NPCMsg()
IfInString,NPCMsg,라스의깃
{
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMSPH.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
Get_Location()
IfInString,Location,[알파차원] 포프레스네 마을
{
Step = 652
}
IfInString,Location,[베타차원] 포프레스네 마을
{
Step = 652
}
IfInString,Location,[감마차원] 포프레스네 마을
{
Step = 652
}
}
if(Step = 652)
{
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(406,180)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 653
}
if(Step = 653)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 654
}
}
}
if(Step = 654)
{
SB_SetText("현재위치가 신전인지 체크 중")
Get_Location()
IfInString,Location,신전
{
Step = 655
}
IfNotInString,Location,신전
{
AltR()
Step = 652
}
}
if(Step = 655)
{
GuiControl, , Gui_NowState, [신전/성당] 도착.
SB_SetText("현재소지 갈리드 체크 중")
Get_Gold()
if(Gold < 1000000)
{
if(Gui_Grade = 1)
{
Grade = 1
Step = 550
}
if(Gui_Grade = 0)
{
GuiControl, , Gui_NowState, 갈리드가 부족하여 종료합니다.
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMSPH.exe
}
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
}
if(Gold >= 1000000)
{
Step = 656
}
}
if(Step = 656)
{
Move_NPCTalkForm()
Sleep, 100
PostMessage, 0x100, 17, 1900545, , ahk_pid %jPID%
PostMessage, 0x100, 55, 524289, , ahk_pid %jPID%
PostMessage, 0x101, 55, 524289, , ahk_pid %jPID%
PostMessage, 0x101, 17, 1900545, , ahk_pid %jPID%
Sleep, 500
Step = 657
}
if(Step = 657)
{
SB_SetText("스펠 그레이드 중")
정보카운트 := 정보카운트 - 10
GuiControl, ,Gui_정보count, %정보카운트%
Check_FormNumber()
Check_NPCMsg()
if(FormNumber = 92)
{
IfInString,NPCMsg,무엇을 도와드릴까요
{
Sleep, 500
PostClick(129,77)
Sleep, 500
}
}
if(FormNumber = 56)
{
IfInString,NPCMsg,어떤 것을 도와 드릴까요
{
Sleep, 500
PostClick(135,70)
Sleep, 500
}
IfInString,NPCMsg,선택하세요
{
Sleep, 500
PostClick(120,70)
Sleep, 500
}
IfInString,NPCMsg,맞습니까
{
Sleep, 500
PostClick(123,69)
Sleep, 500
Step = 658
}
}
if(FormNumber = 44)
{
IfInString,NPCMsg,올리시겠습니까
{
Sleep, 500
PostClick(122,63)
Sleep, 500
}
}
if(FormNumber = 38)
{
IfWinNotActive,ahk_pid %jPID%
{
WinActivate, ahk_pid %jPID%
}
if(MagicAbility3 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName3)
MagicAbility3 = 0
Sleep, 1000
return
}
if(MagicAbility4 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName4)
MagicAbility4 = 0
Sleep, 1000
return
}
if(MagicAbility5 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName5)
MagicAbility5 = 0
Sleep, 1000
return
}
if(MagicAbility6 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName6)
MagicAbility6 = 0
Sleep, 1000
return
}
if(MagicAbility7 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName7)
MagicAbility7 = 0
Sleep, 1000
return
}
if(MagicAbility8 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName8)
MagicAbility8 = 0
Sleep, 1000
return
}
if(MagicAbility9 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName9)
MagicAbility9 = 0
Sleep, 1000
return
}
if(MagicAbility10 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName10)
MagicAbility10 = 0
Sleep, 1000
return
}
if(MagicAbility11 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName11)
MagicAbility11 = 0
Sleep, 1000
return
}
if(MagicAbility12 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName12)
MagicAbility12 = 0
Sleep, 1000
return
}
if(MagicAbility13 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName13)
MagicAbility13 = 0
Sleep, 1000
return
}
if(MagicAbility14 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName14)
MagicAbility14 = 0
Sleep, 1000
return
}
if(MagicAbility15 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName15)
MagicAbility15 = 0
Sleep, 1000
return
}
if(MagicAbility16 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName16)
MagicAbility16 = 0
Sleep, 1000
return
}
if(MagicAbility17 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName17)
MagicAbility17 = 0
Sleep, 1000
return
}
if(MagicAbility18 = 100)
{
Sleep, 1000
SendMagicName(Gui_MagicName18)
MagicAbility18 = 0
Sleep, 1000
return
}
}
}
if(Step = 658)
{
SB_SetText("스펠 그레이드할 어빌리티 체크 중")
if(MagicAbility3 = 100 or MagicAbility4 = 100 or MagicAbility5 = 100 or MagicAbility6 = 100 or MagicAbility7 = 100 or MagicAbility8 = 100 or MagicAbility9 = 100 or MagicAbility10 = 100 or MagicAbility11 = 100 or MagicAbility12 = 100 or MagicAbility13 = 100 or MagicAbility14 = 100 or MagicAbility15 = 100 or MagicAbility16 = 100 or MagicAbility17 = 100 or MagicAbility18 = 100)
{
Step = 655
}
if(MagicAbility3 != 100 and MagicAbility4 != 100 and MagicAbility5 != 100 and MagicAbility6 != 100 and MagicAbility7 != 100 and MagicAbility8 != 100 and MagicAbility9 != 100 and MagicAbility10 != 100 and MagicAbility11 != 100 and MagicAbility12 != 100 and MagicAbility13 != 100 and MagicAbility14 != 100 and MagicAbility15 != 100 and MagicAbility16 != 100 and MagicAbility17 != 100 and MagicAbility18 != 100)
{
Step = 659
}
}
if(Step = 659)
{
SB_SetText("신전 밖으로 이동 중")
Set_MoveSpeed()
Loop,3
{
PostMessage, 0x100, 40, 22020097, , ahk_pid %jPID%
PostMessage, 0x101, 40, 22020097, , ahk_pid %jPID%
Sleep, 500
}
Step = 660
}
if(Step = 660)
{
SB_SetText("현재위치가 신전 밖인지 체크 중")
Get_Location()
IfInString,Location,신전
{
AltR()
Step = 659
}
IfNotInString,Location,신전
{
if(Gui_HuntAuto = 1)
{
if(Gui_1Muba = 1)
{
if(BWValue1 < Gui_LimitAbility1)
{
HuntPlace = 1
Step = 8
}
if(BWValue1 >= Gui_LimitAbility1)
{
HuntPlace = 2
Step = 8
}
}
if(Gui_2Muba = 1)
{
if(BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2)
{
HuntPlace = 1
Step = 8
}
if(BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2)
{
HuntPlace = 2
Step = 8
}
}
if(Gui_3Muba = 1)
{
if(BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2 and BWValue3 < Gui_LimitAbility3)
{
HuntPlace = 1
Step = 8
}
if(BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2 or BWValue3 >= Gui_LimitAbility3)
{
HuntPlace = 2
Step = 8
}
}
if(Gui_2ButMuba = 1)
{
if(BWValue0 < Gui_LimitAbility0 and BWValue1 < Gui_LimitAbility1)
{
HuntPlace = 1
Step = 8
}
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1)
{
HuntPlace = 2
Step = 8
}
}
if(Gui_3ButMuba = 1)
{
if(BWValue0 < Gui_LimitAbility0 and BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2)
{
HuntPlace = 1
Step = 8
}
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2)
{
HuntPlace = 2
Step = 8
}
}
if(Gui_4ButMuba = 1)
{
if(BWValue0 < Gui_LimitAbility0 and BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2 and BWValue3 < Gui_LimitAbility3)
{
HuntPlace = 1
Step = 8
}
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2 or BWValue3 >= Gui_LimitAbility3)
{
HuntPlace = 2
Step = 8
}
}
}
if(Gui_HuntPonam = 1)
{
HuntPlace = 1
Step = 11
}
if(Gui_HuntPobuk = 1)
{
HuntPlace = 2
Step = 1002
}
}
}
if(Step = 700)
{
GuiControl, , Gui_NowState, [신전/성당] 기도하러 가는 중.
SB_SetText("강제그레이드 - 신전으로 이동 중")
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 500
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 701
return
}
if(CountPortal = 1)
{
PostClick(645,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 701
return
}
if(CountPortal = 2)
{
PostClick(660,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 701
return
}
}
}
if(Step = 701)
{
Check_NPCMsg()
IfInString,NPCMsg,라스의깃
{
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMSPH.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
CheckPN = 0
return
}
Get_Location()
IfInString,Location,[알파차원] 포프레스네 마을
{
Step = 702
}
IfInString,Location,[베타차원] 포프레스네 마을
{
Step = 702
}
IfInString,Location,[감마차원] 포프레스네 마을
{
Step = 702
}
}
if(Step = 702)
{
if(Gui_forceweapon = "선택")
{
GuiControl, , Gui_NowState, [강제 그레이드] 그레이드 어빌 선택 해주세요.
Sleep, 1000
step = 9
return
}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(406,180)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 703
}
if(Step = 703)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 704
}
}
}
if(Step = 704)
{
SB_SetText("현재위치가 신전인지 체크 중")
Get_Location()
IfInString,Location,신전
{
Step = 705
}
IfNotInString,Location,신전
{
AltR()
Step = 702
}
}
if(Step = 705)
{
GuiControl, , Gui_NowState, [신전/성당] 도착.
SB_SetText("현재소지 갈리드 체크 중")
Get_Gold()
if(Gold < 1000000)
{
Step = 800
}
if(Gold >= 1000000)
{
Step = 706
}
}
if(Step = 706)
{
SB_SetText("강제 그레이드 중")
Move_NPCTalkForm()
Sleep, 100
PostMessage, 0x100, 17, 1900545, , ahk_pid %jPID%
PostMessage, 0x100, 55, 524289, , ahk_pid %jPID%
PostMessage, 0x101, 55, 524289, , ahk_pid %jPID%
PostMessage, 0x101, 17, 1900545, , ahk_pid %jPID%
Sleep, 500
Step = 707
}
if(Step = 707)
{
정보카운트 := 정보카운트 - 10
GuiControl, ,Gui_정보count, %정보카운트%
Check_FormNumber()
Check_NPCMsg()
if(FormNumber = 92)
{
IfInString,NPCMsg,무엇을 도와드릴까요
{
Sleep, 500
PostClick(129,77)
Sleep, 500
}
}
if(FormNumber = 68)
{
IfInString,NPCMsg,맞습니까
{
Sleep, 500
PostClick(120,73)
Sleep, 500
Step = 608
}
}
if(FormNumber = 56)
{
IfInString,NPCMsg,어떤 것을 도와 드릴까요
{
Sleep, 500
PostClick(135,70)
Sleep, 500
}
IfInString,NPCMsg,선택하세요
{
Sleep, 500
PostClick(134,57)
Sleep, 500
}
IfInString,NPCMsg,맞습니까
{
Sleep, 500
PostClick(123,69)
Sleep, 500
Step = 708
}
}
if(FormNumber = 44)
{
IfInString,NPCMsg,올리시겠습니까
{
Sleep, 500
PostClick(122,63)
Sleep, 500
}
}
if(FormNumber = 38)
{
IfWinNotActive,ahk_pid %jPID%
{
WinActivate, ahk_pid %jPID%
}
if(Gui_forceweapon != "선택")
{
if(Gui_forceweapon = "격투")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Sleep, 1000
Send, rurxn{Enter}
WeaponAbility0 = 0
Sleep, 1000
return
}
if(Gui_forceweapon != "격투")
{
Sleep, 1000
SendWeaponName(Gui_forceweapon)
Sleep, 1000
return
}
}
}
}
if(Step = 708)
{
GuiControl, choose, Gui_forceweapon, 선택
Step = 609
}
if(Step = 1000 and 초기마을이동 = 3)
{
GuiControl, , Gui_NowState, [포북] 사냥터로 가기.
SB_SetText("사냥터로 이동 시작")
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Step = 1001
초기마을이동 := 4
포북시작 := 0
MapNumber := 1
}
if(Step = 1000 and 초기마을이동 = 4)
{
GuiControl, , Gui_NowState, [포북] 사냥터로 가기.
SB_SetText("사냥터로 이동 시작")
CheckPB = 0
CheckPN = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
if( 랜덤차원 = 1 )
{
if(Gui_CheckUseParty = 0)
{
Random, CountPortal, 0, 2
}
}
if ( 알파차원 = 1 )
{
CountPortal = 0
}
if ( 베타차원 = 1 )
{
CountPortal = 1
}
if ( 감마차원 = 1 )
{
CountPortal = 2
}
if( 현재차원 = %CountPortal% )
{
포북시작 := 1
MapNumber := 5
현재차원 := CountPortal
}else
{
포북시작 := 0
MapNumber := 1
현재차원 := CountPortal
}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
Sleep, 250
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 250
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 500
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 800
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 500
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 1001
Sleep,5000
return
}
if(CountPortal = 1)
{
PostClick(645,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 1001
Sleep,5000
return
}
if(CountPortal = 2)
{
PostClick(660,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 1001
Sleep,5000
return
}
}
}
if(Step = 1001)
{
SB_SetText("차원 확인 중")
if(Gui_CheckUseMagic = 1)
{
Send, {F17 Down}
Sleep, 200
Send, {F17 UP}
Send, {F17 Down}
Sleep, 200
Send, {F17 UP}
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
Get_Location()
IfInString,Location,[알파차원] 포프레스네 마을
{
Step = 1002
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
IfInString,Location,[베타차원] 포프레스네 마을
{
Step = 1002
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
IfInString,Location,[감마차원] 포프레스네 마을
{
Step = 1002
}
}
if(Step = 1002)
{
SB_SetText("라깃 갯수 체크 중")
if(Gui_CheckUseMagic = 1)
{
Send, {F17 Down}
Sleep, 200
Send, {F17 UP}
Send, {F17 Down}
Sleep, 200
Send, {F17 UP}
}
Get_MsgM()
Get_Perfect()
if(라깃카운트 <= 5)
{
Step = 100
}
if(라깃카운트 > 5)
{
Step = 1055
}
}
if(Step = 1055)
{
SB_SetText("메모리 점유율 체크 중")
Step = 1003
GetPrivateWorkingSet(jPID)
if(TotalPhy > 2000000)
{
if(byte > 1000000)
{
step = 10000
}
if(byte <= 1000000)
{
step = 1003
}
}
if(TotalPhy <= 2000000)
{
if(byte > 620000)
{
step = 10000
}
if(byte <= 620000)
{
step = 1003
}
}
}
if(Step = 1003)
{
SB_SetText("파티 체크 중")
Check_State()
if(State = 1)
{
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 100
}
if(Gui_PartyOff = 1)
{
Move_StateForMount()
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 100
PostDClick(190,310)
Sleep, 100
PostDClick(225,310)
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 100
}
Move_State()
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 500
Check_State()
Check_StatePos()
if(StatePosX = 549 and StatePosY = 644 and State = 1)
{
if(Gui_CheckUseParty = 1)
{
Step = 910
}
if(Gui_CheckUseParty = 0)
{
Step = 1004
}
}
}
if(Step = 1004)
{
아이템읽어오기()
if( Gui_Grade = 1 )
{
if( Gold <= 1000000 )
{
if(아이템갯수["골드바"] = ""  || 아이템갯수["정령의보석"] = "")
{
GuiControl, , Gui_NowState, [자동그렐]골드바 or 정보 부족
MsgBox,48,자동정지, "골드바 or 정보를 채운 후 재시작 버튼을 눌러주세요." ,
SB_SetText("자동그레이드 [ 골드바 or 정보를 채워주세요. ]")
gosub, 일시정지
return
}
Step = 500
return
}
}
Get_FP()
절반FP := MaxFP/2
if(NowFP < 절반FP)
{
GuiControl, , Gui_NowState, FP를 채우고 체잠 시작합니다.
SB_SetText("FP 채우러 가기")
Sleep, 200
Step = 201
return
}
if(Gui_CheckUseParty = 1)
{
party()
}
GuiControl, , Gui_NowState, [포북] 사냥터로 가기.
SB_SetText("포북 사냥터로 이동 중")
포북캐릭()
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(550,130)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
if(BWValue0 = 9999)
{
BWValue0 := ReadAbility("격투")
SetFormat, Float, 0.2
TempAbility := BWValue0 / 100
GuiControl, , Gui_BasicWValue0, %TempAbility%
SetFormat, Float, 0
}
if(BWValue1 = 9999)
{
BWValue1 := ReadAbility(Gui_Weapon1)
SetFormat, Float, 0.2
TempAbility := BWValue1 / 100
GuiControl, , Gui_BasicWValue1, %TempAbility%
SetFormat, Float, 0
}
if(BWValue2 = 9999)
{
BWValue2 := ReadAbility(Gui_Weapon2)
SetFormat, Float, 0.2
TempAbility := BWValue2 / 100
GuiControl, , Gui_BasicWValue2, %TempAbility%
SetFormat, Float, 0
}
if(BWValue3 = 9999)
{
BWValue3 := ReadAbility(Gui_Weapon3)
SetFormat, Float, 0.2
TempAbility := BWValue3 / 100
GuiControl, , Gui_WeaponValue3, %TempAbility%
SetFormat, Float, 0
}
Step = 1005
}
if(Step = 1005)
{
SB_SetText("움직임 체크 중")
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 1006
}
}
}
if(Step = 1006)
{
SB_SetText("북쪽 필드인지 확인 중")
Get_Location()
IfInString,Location,북쪽 필드
{
RCC = 0
Step = 1007
}
IfNotInString,Location,북쪽 필드
{
AltR()
Step = 1004
}
}
if( 포북시작 = 0 )
{
if(Step = 1007)
{
GuiControl, , Gui_NowState, [포북] 사냥터 도착.
SB_SetText("파수꾼으로 이동 중")
if(Gui_jjOn = 1)
{
Send, {F18 Down}
Sleep, 40
Send, {F18 Up}
Sleep, 10
Send, {F18 Down}
Sleep, 40
Send, {F18 Up}
PickUp_itemsetPN()
}
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(480,205)
PostClick(520,330)
OpenMap()
Sleep, 500
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Step = 1008
}
if(Step = 1008)
{
Get_Location()
IfNotInString,Location,북쪽 필드
{
AltR()
Step = 1000
}
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
Step = 1009
}
}
}
if(Step = 1009)
{
SB_SetText("현재 위치 체크 중")
Get_Location()
IfNotInString,Location,북쪽 필드
{
AltR()
Step = 1000
}
Check_Moving()
Get_Pos()
Get_MovePos()
if((PosX >= MovePosX-2 and PosX <= MovePosX+2) and (PosY >= MovePosY-2 and PosY <= MovePosY+2))
{
MoveWaitCount = 0
pbtalkcheck += 1
pbtalkcheck1 := A_TickCount
Step = 1010
}
if(!((PosX >= MovePosX-2 and PosX <= MovePosX+2) and (PosY >= MovePosY-2 and PosY <= MovePosY+2)))
{
if(MoveWaitCount >= 3)
{
MoveWaitCount = 0
AltR()
Step = 1000
}
else
{
AltR()
Step = 1007
MoveWaitCount += 1
}
}
}
if(Step = 1010)
{
정수체크()
Get_Location()
IfInString,Location,[알파차원]
{
호출대상 := "알파 - 길잃은수색대"
}
IfInString,Location,[베타차원]
{
호출대상 := "베타 - 길잃은수색대"
}
IfInString,Location,[감마차원]
{
호출대상 := "감마 - 길잃은수색대"
}
SB_SetText("파수꾼과 대화 중")
WinActivate, ahk_pid %jPID%
Move_NPCTalkForm()
callid = 1
Sleep, 1000
PixelSearch, MobX, MobY, 410, 100, 580, 235, 0xEF8AFF, 1, Fast
if(ErrorLevel = 1)
{
AltR()
Sleep,500
PixelSearch, MobX, MobY, 410, 100, 580, 235, 0xEF8AFF, 1, Fast
}
if(ErrorLevel = 0)
{
PostClick(MobX,MobY)
Sleep, 800
Check_FormNumber()
if(FormNumber = 117)
{
Check_OID()
Step = 1011
}
}
}
if(Step = 1011)
{
SB_SetText("피부 버프 받는 중")
Sleep, 400
Check_FormNumber()
Sleep, 100
if(FormNumber = 117)
{
Sleep, 300
PostClick(110,85)
Sleep, 300
}
if(FormNumber = 93)
{
Sleep, 300
PostClick(130,90)
Sleep, 300
}
if(FormNumber = 81)
{
Sleep, 300
PostClick(120,80)
Sleep, 300
step = 10334
}
}
if(Step = 10334)
{
if(결정갯수 >= 2)
{
결정갯수 := 결정갯수-1
Loop, %결정갯수%
{
SB_SetText("결정 > 정수로 교환 중")
Sleep, 300
PostClick(121,104)
Sleep, 300
PostClick(90,90)
Sleep, 300
}
}
if(나무갯수 >= 2)
{
나무갯수 := 나무갯수-1
Loop, %나무갯수%
{
SB_SetText("나무 > 결정으로 교환 중")
Sleep, 300
PostClick(121,104)
Sleep, 300
PostClick(90,78)
Sleep, 300
}
}
if(가루갯수 >= 3)
{
가루갯수 := 가루갯수-2
Loop, %가루갯수%
{
SB_SetText("가루 > 결정으로 교환 중")
Sleep, 300
PostClick(121,104)
Sleep, 300
PostClick(90,65)
Sleep, 300
}
}
Step = 1012
}
if(Step = 1012)
{
Check_FormNumber()
Sleep, 300
if(FormNumber = 117)
{
Sleep, 300
PostClick(85,113)
Sleep, 300
newTime = %A_Now%
EnvAdd, newTime, 27, Minutes
FormatTime, newTime1, %newTime%, yyyyMMddHHmm
CheckPB = 1
pbtalkcheck = 0
Sleep, 50
step = 1013
Sleep,500
포북시작 := 1
MapNumber = 1
}
}
}
if( 포북시작 = 1 )
{
if(Step = 1007)
{
정수체크()
GuiControl, , Gui_NowState, [포북] 사냥터 도착.
SB_SetText("원격대화 시도 중")
포북대화시도 := A_TickCount
IfInString,Location,[알파차원]
{
호출대상 := "알파 - 길잃은수색대"
}
IfInString,Location,[베타차원]
{
호출대상 := "베타 - 길잃은수색대"
}
IfInString,Location,[감마차원]
{
호출대상 := "감마 - 길잃은수색대"
}
if(Gui_jjOn = 1)
{
Send, {F18 Down}
Sleep, 40
Send, {F18 Up}
Sleep, 10
Send, {F18 Down}
Sleep, 40
Send, {F18 Up}
PickUp_itemsetPN()
}
Loop,
{
WriteExecutableMemory("NPC호출용1")
WriteExecutableMemory("NPC호출용2")
jelan.write(0x00527b54, CCD, "UInt", aOffset*)
sleep, 100
RunMemory("NPC호출")
Check_FormNumber()
if( FormNumber != 0 )
{
Step = 1011
break
}
포북대화경과 := A_TickCount - 포북대화시도
if(포북대화경과 >= 5000)
{
AltR()
Sleep, 1000
Step = 1006
포북시작 := 0
break
}
}
}
if(Step = 1011)
{
SB_SetText("피부 버프 받는 중")
Sleep, 400
Check_FormNumber()
Sleep, 100
if(FormNumber = 117)
{
Sleep, 300
PostClick(110,85)
Sleep, 300
}
if(FormNumber = 93)
{
Sleep, 300
PostClick(130,90)
Sleep, 300
}
if(FormNumber = 81)
{
Sleep, 300
PostClick(120,80)
Sleep, 300
step = 10334
}
}
if(Step = 10334)
{
if(결정갯수 >= 2)
{
결정갯수 := 결정갯수-1
Loop, %결정갯수%
{
SB_SetText("결정 > 정수로 교환 중")
Sleep, 300
PostClick(121,104)
Sleep, 300
PostClick(90,90)
Sleep, 300
}
}
if(나무갯수 >= 2)
{
나무갯수 := 나무갯수-1
Loop, %나무갯수%
{
SB_SetText("나무 > 결정으로 교환 중")
Sleep, 300
PostClick(121,104)
Sleep, 300
PostClick(90,78)
Sleep, 300
}
}
if(가루갯수 >= 3)
{
가루갯수 := 가루갯수-2
Loop, %가루갯수%
{
SB_SetText("가루 > 결정으로 교환 중")
Sleep, 300
PostClick(121,104)
Sleep, 300
PostClick(90,65)
Sleep, 300
}
}
Step = 1012
}
if(Step = 1012)
{
Check_FormNumber()
Sleep, 300
if(FormNumber = 117)
{
Sleep, 300
PostClick(85,113)
Sleep, 300
newTime = %A_Now%
EnvAdd, newTime, 27, Minutes
FormatTime, newTime1, %newTime%, yyyyMMddHHmm
CheckPB = 1
pbtalkcheck = 0
Sleep, 50
step = 1013
Sleep,500
MapNumber := 5
}
}
}
if(Step = 1013)
{
GuiControl, , Gui_NowState, [포북] 체잠 시작.
SB_SetText("맵 이동 중")
if(Gui_CheckWPDMagic = 1)
{
WPD()
}
if(MapNumber >= 153)
{
MapNumber = 1
Step = 1000
return
}
CharMovePobuk()
Step = 1014
}
if(Step = 1014)
{
SB_SetText("움직임 체크 중")
Check_Moving()
Get_Pos()
Get_MovePos()
if(Moving = 0)
{
Sleep, 200
Check_Moving()
if(Moving = 0)
{
Step = 1015
}
}
if((PosX >= MovePosX and PosX <= MovePosX) and (PosY >= MovePosY and PosY <= MovePosY))
{
MoveWaitCount = 0
Step = 1016
}
}
if(Step = 1015)
{
Get_Pos()
Get_MovePos()
if((PosX >= MovePosX and PosX <= MovePosX) and (PosY >= MovePosY and PosY <= MovePosY))
{
MoveWaitCount = 0
Step = 1016
}
if(!((PosX >= MovePosX and PosX <= MovePosX) and (PosY >= MovePosY and PosY <= MovePosY)))
{
if(MoveWaitCount >= 2)
{
MoveWaitCount = 0
Step = 1000
}
else
{
Step = 1016
}
}
}
if(Step = 1016)
{
SB_SetText("몬스터 찾는 중")
IfWinNotActive, ahk_pid %jPID%
{
WinActivate, ahk_pid %jPID%
}
포북몹 := 0xB5F5F7
PixelSearch, MobX, MobY, 350, 160, 410, 260, 포북몹, 5, *ScanBR *Fast  *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 350, 160, 410, 260, 포북몹, 5, *ScanBR *Fast  *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 360, 209, 437, 260, 포북몹, 5, *ScanLB *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 360, 209, 437, 260, 포북몹, 5, *ScanLB *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 362, 186, 432, 255, 포북몹, 5, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 362, 186, 432, 255, 포북몹, 5, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 333, 161, 460, 281, 포북몹, 5, *ScanRT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 333, 161, 460, 281, 포북몹, 5, *ScanRT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 315, 138, 483, 305, 포북몹, 5, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 315, 138, 483, 305, 포북몹, 5, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 260, 92, 533, 352, 포북몹, 5, *ScanRT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 260, 92, 533, 352, 포북몹, 5, *ScanRT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 214, 44, 580, 400, 포북몹, 5, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 214, 44, 580, 400, 포북몹, 5, *ScanLT *Fast *RGB
}
}
}
}
}
}
}
}
}
}
}
}
}
if(ErrorLevel = 0)
{
PostClick(MobX,MobY)
Monster_OID()
WinGetPos, ElanciaClientX, ElanciaClientY, Width, Height, ahk_pid %jPID%
SplashX := MobX + ElanciaClientX - 13
SplashY := MobY + ElanciaClientY + 15
SplashImage, %MobNumber%:, b X%SplashX% Y%SplashY% W30 H60 CW000000
MobNumber += 1
if(MobNumber >= 11)
{
MobNumber = 1
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
Step = 1013
return
}
AttackLoopCount = 0
AttackCount = 0
Sleep, 500
movmob := A_TickCount
Step = 1019
return
}
if(ErrorLevel = 1)
{
MobNumber = 1
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
Step = 1013
return
}
}
if(Step = 1018)
{
SB_SetText("몹 공격 체크 중")
AttackLoopCount += 1
Check_Attack()
if(Attack = 0)
{
AttackCount += 1
}
if(Attack = 1 or Attack = 2)
{
AttackCount = 0
}
if(AttackLoopCount >= 10)
{
if(AttackCount > 5)
{
AttackLoopCount = 0
AttackCount = 0
Step = 1016
}
else
{
MobNumber = 1
AttackLoopCount = 0
AttackCount = 0
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
Step = 1026
AttackingCount := A_TickCount
}
}
}
if(Step = 1019)
{
SB_SetText("몬스터가 가까이 있는지 확인 중")
Check_Moving()
if(Moving = 0)
{
Sleep, 200
Check_Moving()
if(Moving = 0)
{
AltR()
Step = 1018
}
}
movmob2 := A_TickCount - movmob
if(movmob2 >= 4000)
{
SB_SetText("거리가 멉니다.")
Sleep, 100
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 1016
}
}
if(Step = 1026)
{
SB_SetText("무바를 시작하였습니다")
if(Gui_1Muba = 1)
{
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(RepairWeaponCount1 >= 5)
{
RepairWeaponCount1 = 0
MapNumber = 1
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 300
return
}
keyclick(1)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount1 += 1
}
if(Weapon != 0)
{
RepairWeaponCount1 = 0
}
}
if(Gui_2Muba = 1)
{
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(RepairWeaponCount1 >= 5 or RepairWeaponCount2 >= 5)
{
RepairWeaponCount1 = 0
RepairWeaponCount2 = 0
MapNumber = 1
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 300
return
}
if(MubaStep = 1)
{
keyclick(1)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(TempWeapon = Weapon)
{
TempWeapon := Weapon
RepairWeaponCount1 += 1
}
if(TempWeapon != Weapon)
{
TempWeapon := Weapon
RepairWeaponCount1 = 0
}
MubaStep = 2
return
}
if(MubaStep = 2)
{
keyclick(2)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(TempWeapon = Weapon)
{
TempWeapon := Weapon
RepairWeaponCount2 += 1
}
if(TempWeapon != Weapon)
{
TempWeapon := Weapon
RepairWeaponCount2 = 0
}
MubaStep = 1
return
}
}
if(Gui_3Muba = 1)
{
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(AbilityName = Gui_Weapon3)
{
BWValue3 := AbilityValue
}
if(RepairWeaponCount1 >= 5 or RepairWeaponCount2 >= 5 or RepairWeaponCount3 >= 5)
{
RepairWeaponCount1 = 0
RepairWeaponCount2 = 0
RepairWeaponCount3 = 0
MapNumber = 1
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 300
return
}
if(MubaStep = 1)
{
keyclick(1)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(TempWeapon = Weapon)
{
TempWeapon := Weapon
RepairWeaponCount1 += 1
}
if(TempWeapon != Weapon)
{
TempWeapon := Weapon
RepairWeaponCount1 = 0
}
MubaStep = 2
return
}
if(MubaStep = 2)
{
keyclick(2)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(TempWeapon = Weapon)
{
TempWeapon := Weapon
RepairWeaponCount2 += 1
}
if(TempWeapon != Weapon)
{
TempWeapon := Weapon
RepairWeaponCount2 = 0
}
MubaStep = 3
return
}
if(MubaStep = 3)
{
keyclick(3)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon3 = "현금" or Gui_Weapon3 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon3)
{
BWValue3 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon3 = "현금" or Gui_Weapon3 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(TempWeapon = Weapon)
{
TempWeapon := Weapon
RepairWeaponCount3 += 1
}
if(TempWeapon != Weapon)
{
TempWeapon := Weapon
RepairWeaponCount3 = 0
}
MubaStep = 1
return
}
}
if(Gui_2ButMuba = 1)
{
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(RepairWeaponCount1 >= 5)
{
RepairWeaponCount1 = 0
MapNumber = 1
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 300
return
}
if(MubaStep = 1)
{
keyclick(1)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount1 += 1
}
if(Weapon != 0)
{
RepairWeaponCount1 = 0
}
MubaStep = 2
return
}
if(MubaStep = 2)
{
WPD()
Sleep, 100
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
RemoteM()
}
MubaStep = 1
return
}
}
if(Gui_3ButMuba = 1)
{
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(RepairWeaponCount1 >= 5 or RepairWeaponCount2 >= 5)
{
RepairWeaponCount1 = 0
RepairWeaponCount2 = 0
MapNumber = 1
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 300
return
}
if(MubaStep = 1)
{
keyclick(1)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount1 += 1
}
if(Weapon != 0)
{
RepairWeaponCount1 = 0
}
MubaStep = 2
return
}
if(MubaStep = 2)
{
WPD()
Sleep, 100
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
RemoteM()
}
MubaStep = 3
return
}
if(MubaStep = 3)
{
keyclick(2)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount2 += 1
}
if(Weapon != 0)
{
RepairWeaponCount2 = 0
}
MubaStep = 4
return
}
if(MubaStep = 4)
{
WPD()
Sleep, 100
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
RemoteM()
}
MubaStep = 1
return
}
}
if(Gui_4ButMuba = 1)
{
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(AbilityName = Gui_Weapon3)
{
BWValue3 := AbilityValue
}
if(RepairWeaponCount1 >= 5 or RepairWeaponCount2 >= 5 or RepairWeaponCount3 >= 5)
{
RepairWeaponCount1 = 0
RepairWeaponCount2 = 0
RepairWeaponCount3 = 0
MapNumber = 1
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 300
return
}
if(MubaStep = 1)
{
keyclick(1)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount1 += 1
}
if(Weapon != 0)
{
RepairWeaponCount1 = 0
}
MubaStep = 2
return
}
if(MubaStep = 2)
{
WPD()
Sleep, 100
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
RemoteM()
}
MubaStep = 3
return
}
if(MubaStep = 3)
{
keyclick(2)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon2)
{
BWValue2 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount2 += 1
}
if(Weapon != 0)
{
RepairWeaponCount2 = 0
}
MubaStep = 4
return
}
if(MubaStep = 4)
{
WPD()
Sleep, 100
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
RemoteM()
}
MubaStep = 5
return
}
if(MubaStep = 5)
{
keyclick(3)
Sleep, %무바딜레이%
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon3 = "현금" or Gui_Weapon3 =  "스태프")
{
RemoteM()
}
}
Sleep, %무바딜레이%
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon3)
{
BWValue3 := AbilityValue
}
if(Gui_CheckUseMagic = 1)
{
if(Gui_Weapon3 = "현금" or Gui_Weapon3 =  "스태프")
{
RemoteM()
}
}
Check_Weapon()
if(Weapon = 0)
{
RepairWeaponCount3 += 1
}
if(Weapon != 0)
{
RepairWeaponCount3 = 0
}
MubaStep = 6
return
}
if(MubaStep = 6)
{
WPD()
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
Sleep, 100
if(Gui_CheckUseMagic = 1)
{
RemoteM()
}
MubaStep = 1
return
}
}
}
if(Step = 1030)
{
SB_SetText("피부 버프 받는 중")
CheckPB = 0
CheckPN = 0
Sleep, 100
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 300
CheckPB = 0
CheckPN = 0
Step = 1056
}
if(Step = 1056)
{
SB_SetText("메모리 점유율 체크 중")
GetPrivateWorkingSet(jPID)
if(TotalPhy > 2000000)
{
if(byte > 1000000)
{
step = 10000
}
if(byte <= 1000000)
{
pbtalkcheck1 := A_TickCount
pbtalkcheck += 1
step = 1031
}
}
if(TotalPhy <= 2000000)
{
if(byte > 620000)
{
step = 10000
}
if(byte <= 620000)
{
pbtalkcheck1 := A_TickCount
pbtalkcheck += 1
step = 1031
}
}
}
if(Step = 1031)
{
정수체크()
SB_SetText("원격대화 시도 중")
포북대화시도 := A_TickCount
IfInString,Location,[알파차원]
{
호출대상 := "알파 - 길잃은수색대"
}
IfInString,Location,[베타차원]
{
호출대상 := "베타 - 길잃은수색대"
}
IfInString,Location,[감마차원]
{
호출대상 := "감마 - 길잃은수색대"
}
Loop,
{
WriteExecutableMemory("NPC호출용1")
WriteExecutableMemory("NPC호출용2")
jelan.write(0x00527b54, CCD, "UInt", aOffset*)
sleep, 100
RunMemory("NPC호출")
Check_FormNumber()
if( FormNumber != 0 )
{
Step = 1032
break
}
포북대화경과 := A_TickCount - 포북대화시도
if(포북대화경과 >= 5000)
{
AltR()
Sleep,1000
Step = 1006
포북시작 := 0
break
}
}
}
if(Step = 1032)
{
SB_SetText("피부 버프 갱신 중")
Sleep, 400
Check_FormNumber()
Sleep, 100
if(FormNumber = 117)
{
Sleep, 300
PostClick(110,85)
Sleep, 300
}
if(FormNumber = 93)
{
Sleep, 300
PostClick(130,90)
Sleep, 300
}
if(FormNumber = 81)
{
Sleep, 300
PostClick(120,80)
Sleep, 300
step = 10333
}
}
if(Step = 10333)
{
if(결정갯수 >= 2)
{
결정갯수 := 결정갯수-1
Loop, %결정갯수%
{
SB_SetText("결정 > 정수로 교환 중")
Sleep, 300
PostClick(121,104)
Sleep, 300
PostClick(90,90)
Sleep, 300
}
}
if(나무갯수 >= 2)
{
나무갯수 := 나무갯수-1
Loop, %나무갯수%
{
SB_SetText("나무 > 결정으로 교환 중")
Sleep, 300
PostClick(121,104)
Sleep, 300
PostClick(90,78)
Sleep, 300
}
}
if(가루갯수 >= 3)
{
가루갯수 := 가루갯수-2
Loop, %가루갯수%
{
SB_SetText("가루 > 결정으로 교환 중")
Sleep, 300
PostClick(121,104)
Sleep, 300
PostClick(90,65)
Sleep, 300
}
}
Step = 1033
}
if(Step = 1033)
{
Check_FormNumber()
Sleep, 600
if(FormNumber = 117)
{
Sleep, 300
PostClick(85,113)
Sleep, 300
newTime = %A_Now%
EnvAdd, newTime, 27, Minutes
FormatTime, newTime1, %newTime%, yyyyMMddHHmm
CheckPB = 1
pbtalkcheck = 0
Sleep, 50
step = 1016
}
}
if(Step = 10000)
{
internet := ConnectedToInternet()
if(internet = 0)
{
GuiControl, , 로그인상태정보, 인터넷 끊김. 대기.
SB_SetText("인터넷이 끊어졌습니다.")
Sleep, 1000
}
if(internet = 1)
{
try {
    ; ======================
    ; MSXML2.XMLHTTP 방식
    ; ======================
    winhttp := ComObjCreate("MSXML2.XMLHTTP.6.0")
    winhttp.Open("GET", "http://elancia.nexon.com/main/page/nx.aspx?url=home/index", false)
    winhttp.Send("")

    ; HTTP 상태 코드 확인
    if (winhttp.Status != 200) {
        ; 상태 코드가 200이 아닐 경우 처리
    } else {
        Content := winhttp.ResponseText
        ; 정규 표현식으로 데이터 추출
        if RegExMatch(Content, "style=""color:#60c722;"" >(.*?)</span>", Server) {
            ; 성공적인 데이터 추출
        } else {
            ; 데이터 추출 실패
        }
    }

    ; ======================
    ; Internet Explorer 방식
    ; ======================
    ie := ComObjCreate("InternetExplorer.Application")
    ie.Visible := false
    url := "https://elancia.nexon.com/"
    ie.Navigate(url)

    ; 로딩 완료 대기
    while ie.Busy or ie.ReadyState != 4
        Sleep, 100

    ; 페이지 HTML 가져오기
    html := ie.document.body.innerHTML

    ; "서버 현황" 단어 위치 찾기 및 300자 추출
    target1 := "서버 현황"
    pos1 := InStr(html, target1, false) ; 대소문자 구분 없이 검색

    if (pos1 > 0) {
        extractedText := SubStr(html, pos1, 300)

        ; "엘" 단어 찾기
        target2 := "엘"
        pos2 := InStr(extractedText, target2, false)

        if (pos2 > 0) {
            ; <dd>와 </dd> 사이 텍스트 추출
            ddStart := InStr(extractedText, "<dd>", false, pos2)
            ddEnd := InStr(extractedText, "</dd>", false, ddStart)

            if (ddStart > 0 and ddEnd > ddStart) {
                result := SubStr(extractedText, ddStart + 4, ddEnd - ddStart - 4)
                ; 성공적인 데이터 추출
            } else {
                ; <dd> 데이터 추출 실패
            }
        } else {
            ; '엘' 단어를 찾을 수 없음
        }
    } else {
        ; '서버 현황' 단어를 찾을 수 없음
    }

    ; IE 객체 종료
    ie.Quit()
} catch e {
    ; 예외 처리
}
{
GuiControl, , 로그인상태정보, 일랜시아 서버 정상. 재접속 중
SB_SetText("재접속 시도 중")
if(ParasCount > 3)
{
GuiControl, , Gui_NowState, [포남] 파라스 감지 포남 > 포북변경
MsgBox,48,파라스감지, 파라스방해로 포남 > 포북 사냥터로 변경,2
SB_SetText("파라스를 방해감지")
GuiControl, , Gui_HuntPobuk, 1
ParasCount = 3
파라스방해감지 := 1
실행초기화 := 1
}
Step = 0
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
return
}
if(Server1 != "정상")
{
GuiControl, , 로그인상태정보, 일랜시아 홈페이지 서버 점검 중. 대기
SB_SetText("일랜시아 서버 점검 대기 중")
ParasCount = 0
실행초기화 := 0
Sleep, 1000
return
}
}
}
if(Step = 900)
{
Gui, Submit, Nohide
pName1 := Name1
pName2 := Name2
pName3 := Name3
pName4 := Name4
pName5 := Name5
pName6 := Name6
WinGet, pP1, PID, %pName1%
WinGet, pP2, PID, %pName2%
WinGet, pP3, PID, %pName3%
WinGet, pP4, PID, %pName4%
WinGet, pP5, PID, %pName5%
WinGet, pP6, PID, %pName6%
wtf := new _ClassMemory("ahk_pid " pP1, "", hProcessCopy)
ServerNsger := wtf.readString(0x0017E574, 40, "UTF-16", aOffsets*)
IfInString,ServerNsger,서버와의 연결이
{
GuiControl, , Gui_NowState, [포남] 파티 캐릭 재접속
SB_SetText("파티 캐릭 재접속 중")
Sleep, 1000
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP1%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP1%
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP2%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP2%
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP3%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP3%
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP4%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP4%
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP5%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP5%
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP6%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP6%
Sleep, 500
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP1%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP1%
Sleep, 1500
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP2%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP2%
Sleep, 1500
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP3%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP3%
Sleep, 1500
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP4%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP4%
Sleep, 1500
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP5%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP5%
Sleep, 1500
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP6%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP6%
step = 901
}
IfNotInString,ServerNsger,서버와의 연결이
{
step = 13
}
}
if(step = 901)
{
Gui, Submit, Nohide
WinGet, P1, PID, ahk_pid %pP1%
WinGet, P2, PID, ahk_pid %pP2%
WinGet, P3, PID, ahk_pid %pP3%
WinGet, P4, PID, ahk_pid %pP4%
WinGet, P5, PID, ahk_pid %pP5%
WinGet, P6, PID, ahk_pid %pP6%
Sleep, 2000
if(Gui_P1CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %P1%
PostMessage, 0x201, 1, 13107662, , ahk_pid %P1%
PostMessage, 0x202, 0, 13107662, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %P1%
PostMessage, 0x201, 1, 14287311, , ahk_pid %P1%
PostMessage, 0x202, 0, 14287311, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %P1%
PostMessage, 0x201, 1, 15598030, , ahk_pid %P1%
PostMessage, 0x202, 0, 15598030, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %P1%
PostMessage, 0x201, 1, 16908752, , ahk_pid %P1%
PostMessage, 0x202, 0, 16908752, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %P1%
PostMessage, 0x201, 1, 18088402, , ahk_pid %P1%
PostMessage, 0x202, 0, 18088402, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %P1%
PostMessage, 0x201, 1, 19399121, , ahk_pid %P1%
PostMessage, 0x202, 0, 19399121, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %P1%
PostMessage, 0x201, 1, 20513232, , ahk_pid %P1%
PostMessage, 0x202, 0, 20513232, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %P1%
PostMessage, 0x201, 1, 21889488, , ahk_pid %P1%
PostMessage, 0x202, 0, 21889488, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %P1%
PostMessage, 0x201, 1, 23200209, , ahk_pid %P1%
PostMessage, 0x202, 0, 23200209, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %P1%
PostMessage, 0x201, 1, 24314321, , ahk_pid %P1%
PostMessage, 0x202, 0, 24314321, , ahk_pid %P1%
}
Sleep, 500
PostMessage, 0x200, 0, 22086223, , ahk_pid %P1%
PostMessage, 0x201, 1, 22086223, , ahk_pid %P1%
PostMessage, 0x202, 0, 22086223, , ahk_pid %P1%
Sleep, 1500
if(Gui_P2CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %P2%
PostMessage, 0x201, 1, 13107662, , ahk_pid %P2%
PostMessage, 0x202, 0, 13107662, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %P2%
PostMessage, 0x201, 1, 14287311, , ahk_pid %P2%
PostMessage, 0x202, 0, 14287311, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %P2%
PostMessage, 0x201, 1, 15598030, , ahk_pid %P2%
PostMessage, 0x202, 0, 15598030, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %P2%
PostMessage, 0x201, 1, 16908752, , ahk_pid %P2%
PostMessage, 0x202, 0, 16908752, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %P2%
PostMessage, 0x201, 1, 18088402, , ahk_pid %P2%
PostMessage, 0x202, 0, 18088402, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %P2%
PostMessage, 0x201, 1, 19399121, , ahk_pid %P2%
PostMessage, 0x202, 0, 19399121, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %P2%
PostMessage, 0x201, 1, 20513232, , ahk_pid %P2%
PostMessage, 0x202, 0, 20513232, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %P2%
PostMessage, 0x201, 1, 21889488, , ahk_pid %P2%
PostMessage, 0x202, 0, 21889488, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %P2%
PostMessage, 0x201, 1, 23200209, , ahk_pid %P2%
PostMessage, 0x202, 0, 23200209, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %P2%
PostMessage, 0x201, 1, 24314321, , ahk_pid %P2%
PostMessage, 0x202, 0, 24314321, , ahk_pid %P2%
}
Sleep, 500
PostMessage, 0x200, 0, 22086223, , ahk_pid %P2%
PostMessage, 0x201, 1, 22086223, , ahk_pid %P2%
PostMessage, 0x202, 0, 22086223, , ahk_pid %P2%
Sleep, 1500
if(Gui_P3CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %P3%
PostMessage, 0x201, 1, 13107662, , ahk_pid %P3%
PostMessage, 0x202, 0, 13107662, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %P3%
PostMessage, 0x201, 1, 14287311, , ahk_pid %P3%
PostMessage, 0x202, 0, 14287311, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %P3%
PostMessage, 0x201, 1, 15598030, , ahk_pid %P3%
PostMessage, 0x202, 0, 15598030, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %P3%
PostMessage, 0x201, 1, 16908752, , ahk_pid %P3%
PostMessage, 0x202, 0, 16908752, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %P3%
PostMessage, 0x201, 1, 18088402, , ahk_pid %P3%
PostMessage, 0x202, 0, 18088402, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %P3%
PostMessage, 0x201, 1, 19399121, , ahk_pid %P3%
PostMessage, 0x202, 0, 19399121, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %P3%
PostMessage, 0x201, 1, 20513232, , ahk_pid %P3%
PostMessage, 0x202, 0, 20513232, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %P3%
PostMessage, 0x201, 1, 21889488, , ahk_pid %P3%
PostMessage, 0x202, 0, 21889488, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %P3%
PostMessage, 0x201, 1, 23200209, , ahk_pid %P3%
PostMessage, 0x202, 0, 23200209, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %P3%
PostMessage, 0x201, 1, 24314321, , ahk_pid %P3%
PostMessage, 0x202, 0, 24314321, , ahk_pid %P3%
}
Sleep, 500
PostMessage, 0x200, 0, 22086223, , ahk_pid %P3%
PostMessage, 0x201, 1, 22086223, , ahk_pid %P3%
PostMessage, 0x202, 0, 22086223, , ahk_pid %P3%
Sleep, 1500
if(Gui_P4CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %P4%
PostMessage, 0x201, 1, 13107662, , ahk_pid %P4%
PostMessage, 0x202, 0, 13107662, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %P4%
PostMessage, 0x201, 1, 14287311, , ahk_pid %P4%
PostMessage, 0x202, 0, 14287311, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %P4%
PostMessage, 0x201, 1, 15598030, , ahk_pid %P4%
PostMessage, 0x202, 0, 15598030, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %P4%
PostMessage, 0x201, 1, 16908752, , ahk_pid %P4%
PostMessage, 0x202, 0, 16908752, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %P4%
PostMessage, 0x201, 1, 18088402, , ahk_pid %P4%
PostMessage, 0x202, 0, 18088402, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %P4%
PostMessage, 0x201, 1, 19399121, , ahk_pid %P4%
PostMessage, 0x202, 0, 19399121, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %P4%
PostMessage, 0x201, 1, 20513232, , ahk_pid %P4%
PostMessage, 0x202, 0, 20513232, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %P4%
PostMessage, 0x201, 1, 21889488, , ahk_pid %P4%
PostMessage, 0x202, 0, 21889488, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %P4%
PostMessage, 0x201, 1, 23200209, , ahk_pid %P4%
PostMessage, 0x202, 0, 23200209, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %P4%
PostMessage, 0x201, 1, 24314321, , ahk_pid %P4%
PostMessage, 0x202, 0, 24314321, , ahk_pid %P4%
}
Sleep, 500
PostMessage, 0x200, 0, 22086223, , ahk_pid %P4%
PostMessage, 0x201, 1, 22086223, , ahk_pid %P4%
PostMessage, 0x202, 0, 22086223, , ahk_pid %P4%
Sleep, 1500
if(Gui_P5CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %P5%
PostMessage, 0x201, 1, 13107662, , ahk_pid %P5%
PostMessage, 0x202, 0, 13107662, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %P5%
PostMessage, 0x201, 1, 14287311, , ahk_pid %P5%
PostMessage, 0x202, 0, 14287311, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %P5%
PostMessage, 0x201, 1, 15598030, , ahk_pid %P5%
PostMessage, 0x202, 0, 15598030, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %P5%
PostMessage, 0x201, 1, 16908752, , ahk_pid %P5%
PostMessage, 0x202, 0, 16908752, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %P5%
PostMessage, 0x201, 1, 18088402, , ahk_pid %P5%
PostMessage, 0x202, 0, 18088402, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %P5%
PostMessage, 0x201, 1, 19399121, , ahk_pid %P5%
PostMessage, 0x202, 0, 19399121, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %P5%
PostMessage, 0x201, 1, 20513232, , ahk_pid %P5%
PostMessage, 0x202, 0, 20513232, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %P5%
PostMessage, 0x201, 1, 21889488, , ahk_pid %P5%
PostMessage, 0x202, 0, 21889488, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %P5%
PostMessage, 0x201, 1, 23200209, , ahk_pid %P5%
PostMessage, 0x202, 0, 23200209, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %P5%
PostMessage, 0x201, 1, 24314321, , ahk_pid %P5%
PostMessage, 0x202, 0, 24314321, , ahk_pid %P5%
}
Sleep, 500
PostMessage, 0x200, 0, 22086223, , ahk_pid %P5%
PostMessage, 0x201, 1, 22086223, , ahk_pid %P5%
PostMessage, 0x202, 0, 22086223, , ahk_pid %P5%
Sleep, 1500
if(Gui_P6CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %P6%
PostMessage, 0x201, 1, 13107662, , ahk_pid %P6%
PostMessage, 0x202, 0, 13107662, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %P6%
PostMessage, 0x201, 1, 14287311, , ahk_pid %P6%
PostMessage, 0x202, 0, 14287311, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %P6%
PostMessage, 0x201, 1, 15598030, , ahk_pid %P6%
PostMessage, 0x202, 0, 15598030, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %P6%
PostMessage, 0x201, 1, 16908752, , ahk_pid %P6%
PostMessage, 0x202, 0, 16908752, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %P6%
PostMessage, 0x201, 1, 18088402, , ahk_pid %P6%
PostMessage, 0x202, 0, 18088402, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %P6%
PostMessage, 0x201, 1, 19399121, , ahk_pid %P6%
PostMessage, 0x202, 0, 19399121, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %P6%
PostMessage, 0x201, 1, 20513232, , ahk_pid %P6%
PostMessage, 0x202, 0, 20513232, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %P6%
PostMessage, 0x201, 1, 21889488, , ahk_pid %P6%
PostMessage, 0x202, 0, 21889488, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %P6%
PostMessage, 0x201, 1, 23200209, , ahk_pid %P6%
PostMessage, 0x202, 0, 23200209, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %P6%
PostMessage, 0x201, 1, 24314321, , ahk_pid %P6%
PostMessage, 0x202, 0, 24314321, , ahk_pid %P6%
}
Sleep, 500
PostMessage, 0x200, 0, 22086223, , ahk_pid %P6%
PostMessage, 0x201, 1, 22086223, , ahk_pid %P6%
PostMessage, 0x202, 0, 22086223, , ahk_pid %P6%
Sleep, 10000
step = 13
}
if(Step = 910)
{
Gui, Submit, Nohide
pName1 := Name1
pName2 := Name2
pName3 := Name3
pName4 := Name4
pName5 := Name5
pName6 := Name6
WinGet, pP1, PID, %pName1%
WinGet, pP2, PID, %pName2%
WinGet, pP3, PID, %pName3%
WinGet, pP4, PID, %pName4%
WinGet, pP5, PID, %pName5%
WinGet, pP6, PID, %pName6%
wtf := new _ClassMemory("ahk_pid " pP1, "", hProcessCopy)
ServerNsger := wtf.readString(0x0017E574, 40, "UTF-16", aOffsets*)
IfInString,ServerNsger,서버와의 연결이
{
GuiControl, , Gui_NowState, [포남] 파티 캐릭 재접속
SB_SetText("파티 캐릭 재접속 중")
Sleep, 1000
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP1%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP1%
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP2%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP2%
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP3%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP3%
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP4%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP4%
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP5%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP5%
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP6%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP6%
Sleep, 500
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP1%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP1%
Sleep, 1500
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP2%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP2%
Sleep, 1500
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP3%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP3%
Sleep, 1500
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP4%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP4%
Sleep, 1500
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP5%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP5%
Sleep, 1500
PostMessage, 0x100, 13, 1835009, , ahk_pid %pP6%
PostMessage, 0x101, 13, 1835009, , ahk_pid %pP6%
step = 911
}
IfNotInString,ServerNsger,서버와의 연결이
{
step = 1004
}
}
if(step = 911)
{
Gui, Submit, Nohide
WinGet, P1, PID, ahk_pid %pP1%
WinGet, P2, PID, ahk_pid %pP2%
WinGet, P3, PID, ahk_pid %pP3%
WinGet, P4, PID, ahk_pid %pP4%
WinGet, P5, PID, ahk_pid %pP5%
WinGet, P6, PID, ahk_pid %pP6%
Sleep, 2000
if(Gui_P1CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %P1%
PostMessage, 0x201, 1, 13107662, , ahk_pid %P1%
PostMessage, 0x202, 0, 13107662, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %P1%
PostMessage, 0x201, 1, 14287311, , ahk_pid %P1%
PostMessage, 0x202, 0, 14287311, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %P1%
PostMessage, 0x201, 1, 15598030, , ahk_pid %P1%
PostMessage, 0x202, 0, 15598030, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %P1%
PostMessage, 0x201, 1, 16908752, , ahk_pid %P1%
PostMessage, 0x202, 0, 16908752, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %P1%
PostMessage, 0x201, 1, 18088402, , ahk_pid %P1%
PostMessage, 0x202, 0, 18088402, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %P1%
PostMessage, 0x201, 1, 19399121, , ahk_pid %P1%
PostMessage, 0x202, 0, 19399121, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %P1%
PostMessage, 0x201, 1, 20513232, , ahk_pid %P1%
PostMessage, 0x202, 0, 20513232, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %P1%
PostMessage, 0x201, 1, 21889488, , ahk_pid %P1%
PostMessage, 0x202, 0, 21889488, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %P1%
PostMessage, 0x201, 1, 23200209, , ahk_pid %P1%
PostMessage, 0x202, 0, 23200209, , ahk_pid %P1%
}
if(Gui_P1CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %P1%
PostMessage, 0x201, 1, 24314321, , ahk_pid %P1%
PostMessage, 0x202, 0, 24314321, , ahk_pid %P1%
}
Sleep, 500
PostMessage, 0x200, 0, 22086223, , ahk_pid %P1%
PostMessage, 0x201, 1, 22086223, , ahk_pid %P1%
PostMessage, 0x202, 0, 22086223, , ahk_pid %P1%
Sleep, 1500
if(Gui_P2CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %P2%
PostMessage, 0x201, 1, 13107662, , ahk_pid %P2%
PostMessage, 0x202, 0, 13107662, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %P2%
PostMessage, 0x201, 1, 14287311, , ahk_pid %P2%
PostMessage, 0x202, 0, 14287311, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %P2%
PostMessage, 0x201, 1, 15598030, , ahk_pid %P2%
PostMessage, 0x202, 0, 15598030, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %P2%
PostMessage, 0x201, 1, 16908752, , ahk_pid %P2%
PostMessage, 0x202, 0, 16908752, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %P2%
PostMessage, 0x201, 1, 18088402, , ahk_pid %P2%
PostMessage, 0x202, 0, 18088402, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %P2%
PostMessage, 0x201, 1, 19399121, , ahk_pid %P2%
PostMessage, 0x202, 0, 19399121, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %P2%
PostMessage, 0x201, 1, 20513232, , ahk_pid %P2%
PostMessage, 0x202, 0, 20513232, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %P2%
PostMessage, 0x201, 1, 21889488, , ahk_pid %P2%
PostMessage, 0x202, 0, 21889488, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %P2%
PostMessage, 0x201, 1, 23200209, , ahk_pid %P2%
PostMessage, 0x202, 0, 23200209, , ahk_pid %P2%
}
if(Gui_P2CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %P2%
PostMessage, 0x201, 1, 24314321, , ahk_pid %P2%
PostMessage, 0x202, 0, 24314321, , ahk_pid %P2%
}
Sleep, 500
PostMessage, 0x200, 0, 22086223, , ahk_pid %P2%
PostMessage, 0x201, 1, 22086223, , ahk_pid %P2%
PostMessage, 0x202, 0, 22086223, , ahk_pid %P2%
Sleep, 1500
if(Gui_P3CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %P3%
PostMessage, 0x201, 1, 13107662, , ahk_pid %P3%
PostMessage, 0x202, 0, 13107662, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %P3%
PostMessage, 0x201, 1, 14287311, , ahk_pid %P3%
PostMessage, 0x202, 0, 14287311, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %P3%
PostMessage, 0x201, 1, 15598030, , ahk_pid %P3%
PostMessage, 0x202, 0, 15598030, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %P3%
PostMessage, 0x201, 1, 16908752, , ahk_pid %P3%
PostMessage, 0x202, 0, 16908752, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %P3%
PostMessage, 0x201, 1, 18088402, , ahk_pid %P3%
PostMessage, 0x202, 0, 18088402, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %P3%
PostMessage, 0x201, 1, 19399121, , ahk_pid %P3%
PostMessage, 0x202, 0, 19399121, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %P3%
PostMessage, 0x201, 1, 20513232, , ahk_pid %P3%
PostMessage, 0x202, 0, 20513232, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %P3%
PostMessage, 0x201, 1, 21889488, , ahk_pid %P3%
PostMessage, 0x202, 0, 21889488, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %P3%
PostMessage, 0x201, 1, 23200209, , ahk_pid %P3%
PostMessage, 0x202, 0, 23200209, , ahk_pid %P3%
}
if(Gui_P3CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %P3%
PostMessage, 0x201, 1, 24314321, , ahk_pid %P3%
PostMessage, 0x202, 0, 24314321, , ahk_pid %P3%
}
Sleep, 500
PostMessage, 0x200, 0, 22086223, , ahk_pid %P3%
PostMessage, 0x201, 1, 22086223, , ahk_pid %P3%
PostMessage, 0x202, 0, 22086223, , ahk_pid %P3%
Sleep, 1500
if(Gui_P4CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %P4%
PostMessage, 0x201, 1, 13107662, , ahk_pid %P4%
PostMessage, 0x202, 0, 13107662, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %P4%
PostMessage, 0x201, 1, 14287311, , ahk_pid %P4%
PostMessage, 0x202, 0, 14287311, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %P4%
PostMessage, 0x201, 1, 15598030, , ahk_pid %P4%
PostMessage, 0x202, 0, 15598030, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %P4%
PostMessage, 0x201, 1, 16908752, , ahk_pid %P4%
PostMessage, 0x202, 0, 16908752, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %P4%
PostMessage, 0x201, 1, 18088402, , ahk_pid %P4%
PostMessage, 0x202, 0, 18088402, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %P4%
PostMessage, 0x201, 1, 19399121, , ahk_pid %P4%
PostMessage, 0x202, 0, 19399121, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %P4%
PostMessage, 0x201, 1, 20513232, , ahk_pid %P4%
PostMessage, 0x202, 0, 20513232, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %P4%
PostMessage, 0x201, 1, 21889488, , ahk_pid %P4%
PostMessage, 0x202, 0, 21889488, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %P4%
PostMessage, 0x201, 1, 23200209, , ahk_pid %P4%
PostMessage, 0x202, 0, 23200209, , ahk_pid %P4%
}
if(Gui_P4CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %P4%
PostMessage, 0x201, 1, 24314321, , ahk_pid %P4%
PostMessage, 0x202, 0, 24314321, , ahk_pid %P4%
}
Sleep, 500
PostMessage, 0x200, 0, 22086223, , ahk_pid %P4%
PostMessage, 0x201, 1, 22086223, , ahk_pid %P4%
PostMessage, 0x202, 0, 22086223, , ahk_pid %P4%
Sleep, 1500
if(Gui_P5CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %P5%
PostMessage, 0x201, 1, 13107662, , ahk_pid %P5%
PostMessage, 0x202, 0, 13107662, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %P5%
PostMessage, 0x201, 1, 14287311, , ahk_pid %P5%
PostMessage, 0x202, 0, 14287311, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %P5%
PostMessage, 0x201, 1, 15598030, , ahk_pid %P5%
PostMessage, 0x202, 0, 15598030, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %P5%
PostMessage, 0x201, 1, 16908752, , ahk_pid %P5%
PostMessage, 0x202, 0, 16908752, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %P5%
PostMessage, 0x201, 1, 18088402, , ahk_pid %P5%
PostMessage, 0x202, 0, 18088402, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %P5%
PostMessage, 0x201, 1, 19399121, , ahk_pid %P5%
PostMessage, 0x202, 0, 19399121, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %P5%
PostMessage, 0x201, 1, 20513232, , ahk_pid %P5%
PostMessage, 0x202, 0, 20513232, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %P5%
PostMessage, 0x201, 1, 21889488, , ahk_pid %P5%
PostMessage, 0x202, 0, 21889488, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %P5%
PostMessage, 0x201, 1, 23200209, , ahk_pid %P5%
PostMessage, 0x202, 0, 23200209, , ahk_pid %P5%
}
if(Gui_P5CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %P5%
PostMessage, 0x201, 1, 24314321, , ahk_pid %P5%
PostMessage, 0x202, 0, 24314321, , ahk_pid %P5%
}
Sleep, 500
PostMessage, 0x200, 0, 22086223, , ahk_pid %P5%
PostMessage, 0x201, 1, 22086223, , ahk_pid %P5%
PostMessage, 0x202, 0, 22086223, , ahk_pid %P5%
Sleep, 1500
if(Gui_P6CharNumber = 1)
{
PostMessage, 0x200, 0, 13107662, , ahk_pid %P6%
PostMessage, 0x201, 1, 13107662, , ahk_pid %P6%
PostMessage, 0x202, 0, 13107662, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 2)
{
PostMessage, 0x200, 0, 14287311, , ahk_pid %P6%
PostMessage, 0x201, 1, 14287311, , ahk_pid %P6%
PostMessage, 0x202, 0, 14287311, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 3)
{
PostMessage, 0x200, 0, 15598030, , ahk_pid %P6%
PostMessage, 0x201, 1, 15598030, , ahk_pid %P6%
PostMessage, 0x202, 0, 15598030, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 4)
{
PostMessage, 0x200, 0, 16908752, , ahk_pid %P6%
PostMessage, 0x201, 1, 16908752, , ahk_pid %P6%
PostMessage, 0x202, 0, 16908752, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 5)
{
PostMessage, 0x200, 0, 18088402, , ahk_pid %P6%
PostMessage, 0x201, 1, 18088402, , ahk_pid %P6%
PostMessage, 0x202, 0, 18088402, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 6)
{
PostMessage, 0x200, 0, 19399121, , ahk_pid %P6%
PostMessage, 0x201, 1, 19399121, , ahk_pid %P6%
PostMessage, 0x202, 0, 19399121, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 7)
{
PostMessage, 0x200, 0, 20513232, , ahk_pid %P6%
PostMessage, 0x201, 1, 20513232, , ahk_pid %P6%
PostMessage, 0x202, 0, 20513232, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 8)
{
PostMessage, 0x200, 0, 21889488, , ahk_pid %P6%
PostMessage, 0x201, 1, 21889488, , ahk_pid %P6%
PostMessage, 0x202, 0, 21889488, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 9)
{
PostMessage, 0x200, 0, 23200209, , ahk_pid %P6%
PostMessage, 0x201, 1, 23200209, , ahk_pid %P6%
PostMessage, 0x202, 0, 23200209, , ahk_pid %P6%
}
if(Gui_P6CharNumber = 10)
{
PostMessage, 0x200, 0, 24314321, , ahk_pid %P6%
PostMessage, 0x201, 1, 24314321, , ahk_pid %P6%
PostMessage, 0x202, 0, 24314321, , ahk_pid %P6%
}
Sleep, 500
PostMessage, 0x200, 0, 22086223, , ahk_pid %P6%
PostMessage, 0x201, 1, 22086223, , ahk_pid %P6%
PostMessage, 0x202, 0, 22086223, , ahk_pid %P6%
Sleep, 10000
step = 1004
}
return
AttackCheck:
Gui, Submit, Nohide
if(Step >= 7 and Step < 10000)
{
Set_MoveSpeed()
}
if(Step = 27 or Step = 1026)
{
AttackLoopCount += 1
Check_Attack()
if(Attack = 0)
{
AttackCount += 1
}
if(Attack = 1 or Attack = 2)
{
AttackCount = 0
}
if(AttackLoopCount >= 10)
{
if(AttackCount > 5)
{
AttackLoopCount = 0
AttackCount = 0
if(HuntPlace = 1)
{
Step = 24
AltR()
}
if(HuntPlace = 2)
{
Step = 1016
AltR()
}
}
else
{
AttackLoopCount = 0
AttackCount = 0
}
}
if(Step = 1026)
{
Attacking := A_TickCount - AttackingCount
if(Attacking >= 300000)
{
AttackingCount := A_TickCount
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep,500
AltR()
}
}
}
return
GuiClose:
Gui, Submit, NoHide
SkinForm(0)
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
}
IfWinExist,ahk_exe MRMSPH.exe
{
WinKill, ahk_exe MRMSPH.exe
}
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, VMRE, 0
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 실행시간, 0
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 게임시작x, %게임시작x%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 게임시작y, %게임시작y%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 서버팅김,0
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 파라스감지,0
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 수천감지,0
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 파라스방해감지,0
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P1, %Name1%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P2, %Name2%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P3, %Name3%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P4, %Name4%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P5, %Name5%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, P6, %Name6%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N1, %Gui_P1CharNumber%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N2, %Gui_P2CharNumber%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N3, %Gui_P3CharNumber%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N4, %Gui_P4CharNumber%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N5, %Gui_P5CharNumber%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, N6, %Gui_P6CharNumber%
if(Gui_CheckUseHPExit = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPExit, 1
}
if(Gui_CheckUseHPExit = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPExit, 0
}
if(Gui_CheckUseMagic = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMagic, 1
}
if(Gui_CheckUseMagic = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMagic, 0
}
if(Gui_CheckUseParty = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseParty, 1
}
if(Gui_CheckUseParty = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseParty, 0
}
if(Gui_CheckUseHPPortal = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPPortal, 1
}
if(Gui_CheckUseHPPortal = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPPortal, 0
}
if(Gui_CheckUseHPLimited = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPLimited, 1
}
if(Gui_CheckUseHPLimited = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPLimited, 0
}
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, HPExit, %Gui_HPExit%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, HPPortal, %Gui_HPPortal%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, HPLimited, %Gui_HPLimited%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, CrittHP, %Gui_CHP%
if(Gui_1Muba = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba, 1
}
if(Gui_2Muba = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba, 2
}
if(Gui_3Muba = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba, 3
}
if(Gui_2ButMuba = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba, 4
}
if(Gui_3ButMuba = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba, 5
}
if(Gui_4ButMuba = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Muba, 6
}
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Weapon1, %Gui_Weapon1%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Weapon2, %Gui_Weapon2%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Weapon3, %Gui_Weapon3%
if(Gui_EvadeMand = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Evade, 1
}
if(Gui_EvadeMand = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Evade, 0
}
if(Gui_KON = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, KONOFF, 1
}
if(Gui_KOFF = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, KONOFF, 2
}
if(방어구방지ON = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 방어구방지ONOFF, 1
}
if(방어구방지OFF = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 방어구방지ONOFF, 2
}
if(Gui_jjON = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, jjONOFF, 1
}
if(Gui_jjOFF = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, jjONOFF, 2
}
if(Gui_MoveLoute1 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Direct, 1
}
if(Gui_MoveLoute2 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Direct, 2
}
if(Gui_MoveLoute3 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Direct, 3
}
if(Gui_MoveLoute4 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Direct, 4
}
if(Gui_Ent = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 1
}
if(Gui_Rockey= 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 2
}
if(Gui_EntRockey= 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 3
}
if(Gui_Mand = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 4
}
if(Gui_AllMobAND = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 5
}
if(Gui_AllMobOR = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 6
}
if(Gui_MobMagic = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Monster, 7
}
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, AllMobLimit, %Gui_AllMobLimit%
if(Gui_HuntAuto = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Place, 1
}
if(Gui_HuntPonam = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Place, 2
}
if(Gui_HuntPobuk = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Place, 3
}
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Limit0, %Gui_LimitAbility0%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Limit1, %Gui_LimitAbility1%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Limit2, %Gui_LimitAbility2%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Limit3, %Gui_LimitAbility3%
if(Gui_PartyOn = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Party, 1
}
if(Gui_PartyOff = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Party, 2
}
if(Gui_Grade = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Grade, 1
}
if(Gui_Grade = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Grade, 0
}
if(알파차원 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포탈, 0
}
if(베타차원 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포탈, 1
}
if(감마차원 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포탈, 2
}
if(랜덤차원 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포탈, 3
}
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, loady, 0
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 업데이트체크, %업데이트체크%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, CharNumber, %Gui_CharNumber%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MNS, %Gui_MagicNStack%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Server, %Gui_Server%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Login, %Gui_Login%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA1, %Gui_ActiveA1%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA2, %Gui_ActiveA2%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA3, %Gui_ActiveA3%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA4, %Gui_ActiveA4%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA5, %Gui_ActiveA5%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA6, %Gui_ActiveA6%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA7, %Gui_ActiveA7%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseAA8, %Gui_ActiveA8%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseTAA1, %Gui_TActiveA1%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseTAA2, %Gui_TActiveA2%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseTAA3, %Gui_TActiveA3%
REGWRITE, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseTAAA1, %Gui_TAActiveA1%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, ID, %Gui_NexonID%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Pass, %Gui_NexonPassWord%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, StartTime
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, CFH
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포북생콩, %포북생콩설정%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포남생콩, %포남생콩설정%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 가방, %가방설정%
if(4번사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯4번, 1
}
if(4번사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯4번, 0
}
if(5번사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯5번, 1
}
if(5번사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯5번, 0
}
if(6번사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯6번, 1
}
if(6번사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯6번, 0
}
if(7번사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯7번, 1
}
if(7번사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯7번, 0
}
if(현혹사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 현혹체크, 1
}
if(현혹사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 현혹체크, 0
}
if(폭검사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 폭검체크, 1
}
if(폭검사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 폭검체크, 0
}
if(독침사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 독침체크, 1
}
if(독침사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 독침체크, 0
}
if(무기공격사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 무기공격체크, 1
}
if(무기공격사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 무기공격체크, 0
}
if(대화사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 대화체크, 1
}
if(대화사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 대화체크, 0
}
if(명상사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 명상체크, 1
}
if(명상사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 명상체크, 0
}
if(더블어택사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 더블어택체크, 1
}
if(더블어택사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 더블어택체크, 0
}
if(체력향상사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 체력향상체크, 1
}
if(체력향상사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 체력향상체크, 0
}
if(집중사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 집중체크, 1
}
if(집중사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 집중체크, 0
}
if(회피사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 회피체크, 1
}
if(회피사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 회피체크, 0
}
if(몸통지르기사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 몸통지르기체크, 1
}
if(몸통지르기사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 몸통지르기체크, 0
}
if(리무브아머사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 리무브아머체크, 1
}
if(리무브아머사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 리무브아머체크, 0
}
if(민첩향상사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 민첩향상체크, 1
}
if(민첩향상사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 민첩향상체크, 0
}
if(활방어사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 활방어체크, 1
}
if(활방어사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 활방어체크, 0
}
if(마력향상사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 마력향상체크, 1
}
if(마력향상사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 마력향상체크, 0
}
if(마력방어사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 마력방어체크, 1
}
if(마력방어사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 마력방어체크, 0
}
if(Gui_WeaponCheck1 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC1, 1
}
if(Gui_WeaponCheck1 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC1, 0
}
if(Gui_WeaponCheck2 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC2, 1
}
if(Gui_WeaponCheck2 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC2, 0
}
if(Gui_WeaponCheck3 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC3, 1
}
if(Gui_WeaponCheck3 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC3, 0
}
if(Gui_WeaponCheck4 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC4, 1
}
if(Gui_WeaponCheck4 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC4, 0
}
if(Gui_WeaponCheck5 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC5, 1
}
if(Gui_WeaponCheck5 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC5, 0
}
if(Gui_WeaponCheck6 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC6, 1
}
if(Gui_WeaponCheck6 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC6, 0
}
if(Gui_WeaponCheck7 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC7, 1
}
if(Gui_WeaponCheck7 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC7, 0
}
if(Gui_WeaponCheck8 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC8, 1
}
if(Gui_WeaponCheck8 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC8, 0
}
if(Gui_WeaponCheck9 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC9, 1
}
if(Gui_WeaponCheck9 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC9, 0
}
if(Gui_WeaponCheck10 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC10, 1
}
if(Gui_WeaponCheck10 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC10, 0
}
if(Gui_WeaponCheck11 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC11, 1
}
if(Gui_WeaponCheck11 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC11, 0
}
if(Gui_WeaponCheck12 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC12, 1
}
if(Gui_WeaponCheck12 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC12, 0
}
if(Gui_WeaponCheck13 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC13, 1
}
if(Gui_WeaponCheck13 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC13, 0
}
if(Gui_WeaponCheck14 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC14, 1
}
if(Gui_WeaponCheck14 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC14, 0
}
if(Gui_WeaponCheck15 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC15, 1
}
if(Gui_WeaponCheck15 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC15, 0
}
if(Gui_WeaponCheck16 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC16, 1
}
if(Gui_WeaponCheck16 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC16, 0
}
if(Gui_WeaponCheck17 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC17, 1
}
if(Gui_WeaponCheck17 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC17, 0
}
if(Gui_WeaponCheck18 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC18, 1
}
if(Gui_WeaponCheck18 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC18, 0
}
if(Gui_WeaponCheck19 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC19, 1
}
if(Gui_WeaponCheck19 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC19, 0
}
if(Gui_WeaponCheck20 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC20, 1
}
if(Gui_WeaponCheck20 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC20, 0
}
if(Gui_WeaponCheck21 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC21, 1
}
if(Gui_WeaponCheck21 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC21, 0
}
if(Gui_WeaponCheck22 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC22, 1
}
if(Gui_WeaponCheck22 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC22, 0
}
if(Gui_WeaponCheck23 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC23, 1
}
if(Gui_WeaponCheck23 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC23, 0
}
if(Gui_WeaponCheck24 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC24, 1
}
if(Gui_WeaponCheck24 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC24, 0
}
if(Gui_WeaponCheck25 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC25, 1
}
if(Gui_WeaponCheck25 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC25, 0
}
if(Gui_WeaponCheck26 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC26, 1
}
if(Gui_WeaponCheck26 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC26, 0
}
if(Gui_WeaponCheck27 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC27, 1
}
if(Gui_WeaponCheck27 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC27, 0
}
if(Gui_WeaponCheck28 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC28, 1
}
if(Gui_WeaponCheck28 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC28, 0
}
if(Gui_WeaponCheck29 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC29, 1
}
if(Gui_WeaponCheck29 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC29, 0
}
if(Gui_WeaponCheck30 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC30, 1
}
if(Gui_WeaponCheck30 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC30, 0
}
if(Gui_WeaponCheck31 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC31, 1
}
if(Gui_WeaponCheck31 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC31, 0
}
if(Gui_WeaponCheck32 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC32, 1
}
if(Gui_WeaponCheck32 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC32, 0
}
if(Gui_WeaponCheck33 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC33, 1
}
if(Gui_WeaponCheck33 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC33, 0
}
if(Gui_WeaponCheck34 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC34, 1
}
if(Gui_WeaponCheck34 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC34, 0
}
if(Gui_WeaponCheck35 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC35, 1
}
if(Gui_WeaponCheck35 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC35, 0
}
if(Gui_WeaponCheck36 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC36, 1
}
if(Gui_WeaponCheck36 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC36, 0
}
if(Gui_WeaponCheck37 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC37, 1
}
if(Gui_WeaponCheck37 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC37, 0
}
if(Gui_WeaponCheck38 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC38, 1
}
if(Gui_WeaponCheck38 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC38, 0
}
if(Gui_WeaponCheck39 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC39, 1
}
if(Gui_WeaponCheck39 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC39, 0
}
if(Gui_WeaponCheck40 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC40, 1
}
if(Gui_WeaponCheck40 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC40, 0
}
if(Gui_WeaponCheck41 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC41, 1
}
if(Gui_WeaponCheck41 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC41, 0
}
if(Gui_WeaponCheck42 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC42, 1
}
if(Gui_WeaponCheck42 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC42, 0
}
if(Gui_WeaponCheck43 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC43, 1
}
if(Gui_WeaponCheck43 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC43, 0
}
if(Gui_WeaponCheck44 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC44, 1
}
if(Gui_WeaponCheck44 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC44, 0
}
if(Gui_WeaponCheck45 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC45, 1
}
if(Gui_WeaponCheck45 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC45, 0
}
if(Gui_WeaponCheck46 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC46, 1
}
if(Gui_WeaponCheck46 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC46, 0
}
if(Gui_WeaponCheck47 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC47, 1
}
if(Gui_WeaponCheck47 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC47, 0
}
if(Gui_WeaponCheck48 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC48, 1
}
if(Gui_WeaponCheck48 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC48, 0
}
if(Gui_WeaponCheck49 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC49, 1
}
if(Gui_WeaponCheck49 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC49, 0
}
if(Gui_WeaponCheck50 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC50, 1
}
if(Gui_WeaponCheck50 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC50, 0
}
if(Gui_WeaponCheck51 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC51, 1
}
if(Gui_WeaponCheck51 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC51, 0
}
if(Gui_WeaponCheck52 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC52, 1
}
if(Gui_WeaponCheck52 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC52, 0
}
if(Gui_WeaponCheck53 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC53, 1
}
if(Gui_WeaponCheck53 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC53, 0
}
if(Gui_WeaponCheck54 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC54, 1
}
if(Gui_WeaponCheck54 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC54, 0
}
if(Gui_WeaponCheck55 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC55, 1
}
if(Gui_WeaponCheck55 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC55, 0
}
if(Gui_WeaponCheck56 = 1)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC56, 1
}
if(Gui_WeaponCheck56 = 0)
{
REGWRITE, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseWC56, 0
}
if(Gui_MagicCheck3 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC3, 1
}
if(Gui_MagicCheck3 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC3, 0
}
if(Gui_MagicCheck4 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC4, 1
}
if(Gui_MagicCheck4 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC4, 0
}
if(Gui_MagicCheck5 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC5, 1
}
if(Gui_MagicCheck5 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC5, 0
}
if(Gui_MagicCheck6 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC6, 1
}
if(Gui_MagicCheck6 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC6, 0
}
if(Gui_MagicCheck7 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC7, 1
}
if(Gui_MagicCheck7 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC7, 0
}
if(Gui_MagicCheck8 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC8, 1
}
if(Gui_MagicCheck8 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC8, 0
}
if(Gui_MagicCheck9 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC9, 1
}
if(Gui_MagicCheck9 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC9, 0
}
if(Gui_MagicCheck10 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC10, 1
}
if(Gui_MagicCheck10 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC10, 0
}
if(Gui_MagicCheck11 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC11, 1
}
if(Gui_MagicCheck11 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC11, 0
}
if(Gui_MagicCheck12 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC12, 1
}
if(Gui_MagicCheck12 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC12, 0
}
if(Gui_MagicCheck13 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC13, 1
}
if(Gui_MagicCheck13 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC13, 0
}
if(Gui_MagicCheck14 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC14, 1
}
if(Gui_MagicCheck14 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC14, 0
}
if(Gui_MagicCheck15 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC15, 1
}
if(Gui_MagicCheck15 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC15, 0
}
if(Gui_MagicCheck16 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC16, 1
}
if(Gui_MagicCheck16 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC16, 0
}
if(Gui_MagicCheck17 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC17, 1
}
if(Gui_MagicCheck17 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC17, 0
}
if(Gui_MagicCheck18 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC18, 1
}
if(Gui_MagicCheck18 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseMC18, 0
}
if(Gui_relogerror = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, relog, 1
}
if(Gui_relogerror = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, relog, 0
}
Gui, submit, nohide
Gui, listview, 포프레스네소각
FileDelete, %A_ScriptDir%\소각리스트.ini
save := LV_GetCount()
loop, %save%{
lv_gettext(savefile1,a_index)
FileAppend, %savefile1%`n, %A_ScriptDir%\소각리스트.ini
FileSetAttrib, +H, %A_ScriptDir%\소각리스트.ini
}
WinKill, ahk_exe MRMSPH.exe
ExitApp
return
IME_CHECK(WinTitle)
{
WinGet, hWnd, ID, %WinTitle%
Return, Send_ImeControl(ImmGetDefaultIMEWnd(hWnd),0x005,"")
}
Send_ImeControl(DefaultIMEWnd, wParam, lParam)
{
DetectSave := A_DetectHiddenWindows
DetectHiddenWindows, ON
SendMessage, 0x283, wParam, lParam, , ahk_id %DefaultIMEWnd%
if (DetectSave <> A_DetectHiddenWindows)
DetectHiddenWindows, %DetectSave%
Return, ErrorLevel
}
ImmGetDefaultIMEWnd(hWnd)
{
Return, DllCall("imm32\ImmGetDefaultIMEWnd", Uint,hWnd, Uint)
}
ChangeDisplaySettings( cD, sW, sH, rR )
{
VarSetCapacity(dM,156,0), NumPut(156,dM,36)
DllCall( "EnumDisplaySettingsA", UInt,0, UInt,-1, UInt,&dM ), NumPut(0x5c0000,dM,40)
NumPut(cD,dM,104),  NumPut(sW,dM,108),  NumPut(sH,dM,112),  NumPut(rR,dM,120)
Return, DllCall( "ChangeDisplaySettingsA", UInt,&dM, UInt,0 )
}
FormatSeconds(NumberOfSeconds)
{
time = 19990101
time += %NumberOfSeconds%, seconds
FormatTime, mmss, %time%, mm:ss
SetFormat, float, 2.0
Return, NumberOfSeconds//3600 ":" mmss
}
ConnectedToInternet(flag=0x40)
{
Return, DllCall("Wininet.dll\InternetGetConnectedState", "Str", flag,"Int",0)
}
Loading()
{
Sleep, 500
Loop,
{
if(pwb.readyState() = 4)
{
Sleep, 500
break
}
}
}
Gui_Enable()
{
GuiControlGet, Gui_CheckUseHPExit
GuiControlGet, Gui_CheckUseHPPortal
GuiControlGet, Gui_1Muba
GuiControlGet, Gui_2Muba
GuiControlGet, Gui_3Muba
GuiControlGet, Gui_2ButMuba
GuiControlGet, Gui_3ButMuba
GuiControlGet, Gui_4ButMuba
GuiControlGet, Gui_Mand
GuiControlGet, Gui_AllMobAND
GuiControlGet, Gui_AllMobOR
GuiControlGet, Gui_MobMagic
GuiControlGet, Gui_Login
if( Gui_Login = "인터넷" )
{
GuiControl, Enable, Gui_NexonID
GuiControl, Enable, Gui_NexonPassWord
}
GuiControl, Enable, Gui_Server
GuiControl, Enable, Gui_Login
GuiControl, Enable, Gui_CharNumber
GuiControl, Enable, Gui_CheckUseHPExit
GuiControl, Enable, Gui_CheckUseHPPortal
GuiControl, Enable, Gui_CheckUseHPLimited
GuiControl, Enable, Gui_jjON
GuiControl, Enable, Gui_jjOFF
if(Gui_CheckUseHPExit = 1)
{
GuiControl, Enable, Gui_HPExit
}
if(Gui_CheckUseHPPortal = 1)
{
GuiControl, Enable, Gui_HPPortal
}
if(Gui_CheckUseHPLimited = 1)
{
GuiControl, Enable, Gui_HPLimited
}
GuiControl, Enable, Gui_1Muba
GuiControl, Enable, Gui_2Muba
GuiControl, Enable, Gui_3Muba
GuiControl, Enable, Gui_2ButMuba
GuiControl, Enable, Gui_3ButMuba
GuiControl, Enable, Gui_4ButMuba
if(Gui_1Muba = 1 or Gui_2ButMuba = 1)
{
GuiControl, Enable, Gui_Weapon1
if(Gui_2ButMuba = 1)
{
GuiControl, Enable, Gui_LimitAbility0
}
GuiControl, Enable, Gui_LimitAbility1
}
if(Gui_2Muba = 1 or Gui_3ButMuba = 1)
{
GuiControl, Enable, Gui_Weapon1
GuiControl, Enable, Gui_Weapon2
if(Gui_3ButMuba = 1)
{
GuiControl, Enable, Gui_LimitAbility0
}
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility2
}
if(Gui_3Muba = 1 or Gui_4ButMuba = 1)
{
GuiControl, Enable, Gui_Weapon1
GuiControl, Enable, Gui_Weapon2
GuiControl, Enable, Gui_Weapon3
if(Gui_4ButMuba = 1)
{
GuiControl, Enable, Gui_LimitAbility0
}
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Enable, Gui_LimitAbility3
}
if(Gui_Mand = 0 and Gui_AllMobAND = 0 and Gui_AllMobOR = 0 and Gui_MobMagic =0)
{
GuiControl, Enable, Gui_EvadeMand
}
if(Gui_Mand = 1 or Gui_AllMobAND = 1 or Gui_AllMobOR = 1 or Gui_MobMaigc =1)
{
GuiControl, Enable, Gui_AllMobLimit
}
GuiControl, Enable, Gui_MoveLoute1
GuiControl, Enable, Gui_MoveLoute2
GuiControl, Enable, Gui_MoveLoute3
GuiControl, Enable, Gui_MoveLoute4
GuiControl, Enable, Gui_Ent
GuiControl, Enable, Gui_Rockey
GuiControl, Enable, Gui_EntRockey
GuiControl, Enable, Gui_Mand
GuiControl, Enable, Gui_AllMobAND
GuiControl, Enable, Gui_AllMobOR
GuiControl, Enable, Gui_MobMagic
GuiControl, Enable, Gui_PartyOn
GuiControl, Enable, Gui_PartyOff
GuiControl, Enable, Gui_Grade
GuiControl, Enable, Gui_StartButton
GuiControl, Enable, Gui_WindowSettingButton
GuiControl, Enable, Gui_Agree
}
PostMove(MouseX,MouseY)
{
MousePos := MouseX | MouseY<< 16
PostMessage, 0x200, 0, %MousePos%, , ahk_pid %jPID%
}
PostClick(MouseX,MouseY)
{
MousePos := MouseX | MouseY<< 16
PostMessage, 0x200, 0, %MousePos%, , ahk_pid %jPID%
PostMessage, 0x201, 1, %MousePos%, , ahk_pid %jPID%
PostMessage, 0x202, 0, %MousePos%, , ahk_pid %jPID%
}
PostDClick(MouseX,MouseY)
{
MousePos := MouseX | MouseY<< 16
PostMessage, 0x200, 0, %MousePos%, , ahk_pid %jPID%
PostMessage, 0x203, 0, %MousePos%, , ahk_pid %jPID%
PostMessage, 0x202, 0, %MousePos%, , ahk_pid %jPID%
}
PostRClick(MouseX,MouseY)
{
MousePos := MouseX | MouseY<< 16
PostMessage, 0x200, 0, %MousePos%, , ahk_pid %jPID%
PostMessage, 0x204, 1, %MousePos%, , ahk_pid %jPID%
PostMessage, 0x205, 0, %MousePos%, , ahk_pid %jPID%
}
OpenMap()
{
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 86, 3014657, , ahk_pid %jPID%
PostMessage, 0x101, 86, 3014657, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
}
AltR()
{
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 82, 1245185, , ahk_pid %jPID%
PostMessage, 0x101, 82, 1245185, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
}
Set_MoveSpeed()
{
value := jelan.write(0x0058DAD4, 730, "UInt", 0x178, 0x9C)
value := jelan.write(0x0058DAD4, 730, "UInt", 0x178, 0x98)
}
Check_Moving()
{
Moving := jelan.read(0x0058EB1C, "UInt", 0x174)
}
Check_OID()
{
Get_Location()
CCD := jelan.read(0x00584C2C, "UInt", aOffsets*)
}
Monster_OID()
{
몬스터ID :=	jelan.read(0x00584C2C, "UInt", aOffsets*)
GuiControl,,현재타겟OID값,0x%몬스터ID%
}
Check_State()
{
State := jelan.read(0x0058EB98, "UInt", aOffsets*)
if(State != 0)
{
State = 1
}
}
Check_StatePos()
{
StatePosX := jelan.read(0x0058EB48, "UInt", 0x44)
StatePosY := jelan.read(0x0058EB48, "UInt", 0x48)
}
Check_Mount()
{
Mount := jelan.read(0x0058DAD4, "UInt", 0x22C)
}
Check_Shield()
{
Shield := jelan.read(0x0058DAD4, "UInt", 0x1FC)
}
Check_Inven()
{
Inven := jelan.read(0x0058EB2C, "UInt", aOffsets*)
if(Inven != 0)
{
Inven = 1
}
}
Check_Shop()
{
Buy := jelan.read(0x0058EBB9, "UInt", aOffsets*)
if(Buy != 0)
{
Buy = 1
}
Repair := jelan.read(0x0058F0C0, "UInt", aOffsets*)
if(Repair != 0)
{
Repair = 1
}
}
Check_Ras()
{
Ras := jelan.read(0x0058F0CC, "UInt", aOffsets*)
if(Ras != 0)
{
Ras = 1
}
SelectRas := jelan.read(0x0058F100, "UInt", aOffsets*)
if(SelectRas != 0)
{
SelectRas = 1
}
}
Check_Map()
{
Map := jelan.read(0x0058EB6C, "UInt", aOffsets*)
MapSize := jelan.read(0x0058DAD0, "UInt", 0xC, 0x10, 0x8, 0x264)
if(Map != 0)
{
Map = 1
}
}
Amorcheck()
{
Shoes := jelan.read(0x0058DAD4,"UInt",0x218)
Top := jelan.read(0x0058DAD4,"UInt",0x208)
jean := jelan.read(0x0058DAD4,"UInt",0x210)
Cap := jelan.read(0x0058DAD4,"UInt",0x224)
}
Check_SAbilityN()
{
Slot1AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x4, 0x8, 0x4)
Slot2AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x8, 0x8, 0x4)
Slot3AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xC, 0x8, 0x4)
Slot4AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x10, 0x8, 0x4)
Slot5AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x14, 0x8, 0x4)
Slot6AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x18, 0x8, 0x4)
Slot7AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x1C, 0x8, 0x4)
Slot8AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x20, 0x8, 0x4)
Slot9AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x24, 0x8, 0x4)
Slot10AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x28, 0x8, 0x4)
Slot11AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x2C, 0x8, 0x4)
Slot12AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x30, 0x8, 0x4)
Slot13AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x34, 0x8, 0x4)
Slot14AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x38, 0x8, 0x4)
Slot15AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x3C, 0x8, 0x4)
Slot16AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x40, 0x8, 0x4)
Slot17AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x44, 0x8, 0x4)
Slot18AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x48, 0x8, 0x4)
Slot19AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x4C, 0x8, 0x4)
Slot20AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x50, 0x8, 0x4)
Slot21AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x54, 0x8, 0x4)
Slot22AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x58, 0x8, 0x4)
Slot23AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x5C, 0x8, 0x4)
Slot24AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x60, 0x8, 0x4)
Slot25AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x64, 0x8, 0x4)
Slot26AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x68, 0x8, 0x4)
Slot27AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x6C, 0x8, 0x4)
Slot28AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x70, 0x8, 0x4)
Slot29AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x74, 0x8, 0x4)
Slot30AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x78, 0x8, 0x4)
Slot31AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x7C, 0x8, 0x4)
Slot32AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x80, 0x8, 0x4)
Slot33AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x84, 0x8, 0x4)
Slot34AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x88, 0x8, 0x4)
Slot35AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x8C, 0x8, 0x4)
Slot36AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x90, 0x8, 0x4)
Slot37AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x94, 0x8, 0x4)
Slot38AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x98, 0x8, 0x4)
Slot39AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0x9C, 0x8, 0x4)
Slot40AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xa0, 0x8, 0x4)
Slot41AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xa4, 0x8, 0x4)
Slot42AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xa8, 0x8, 0x4)
Slot43AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xaC, 0x8, 0x4)
Slot44AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xb0, 0x8, 0x4)
Slot45AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xb4, 0x8, 0x4)
Slot46AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xb8, 0x8, 0x4)
Slot47AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xbC, 0x8, 0x4)
Slot48AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xc0, 0x8, 0x4)
Slot49AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xc4, 0x8, 0x4)
Slot50AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xc8, 0x8, 0x4)
Slot51AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xcC, 0x8, 0x4)
Slot52AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xd0, 0x8, 0x4)
Slot53AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xd4, 0x8, 0x4)
Slot54AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xd8, 0x8, 0x4)
Slot55AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xdC, 0x8, 0x4)
Slot56AN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC6, 0x8, 0xe0, 0x8, 0x4)
}
Check_SMagicN()
{
Slot3MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0xC, 0x8, 0xC)
Slot4MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x10, 0x8, 0xC)
Slot5MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x14, 0x8, 0xC)
Slot6MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x18, 0x8, 0xC)
Slot7MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x1C, 0x8, 0xC)
Slot8MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x20, 0x8, 0xC)
Slot9MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x24, 0x8, 0xC)
Slot10MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x28, 0x8, 0xC)
Slot11MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x2C, 0x8, 0xC)
Slot12MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x30, 0x8, 0xC)
Slot13MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x34, 0x8, 0xC)
Slot14MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x38, 0x8, 0xC)
Slot15MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x3C, 0x8, 0xC)
Slot16MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x40, 0x8, 0xC)
Slot17MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x44, 0x8, 0xC)
Slot18MN := jelan.readString(0x0058DAD4, 22, "UTF-16", 0x178, 0xC2, 0x8, 0x48, 0x8, 0xC)
}
Check_SMagic()
{
Slot3Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0xC, 0x8, 0x42C)
Slot4Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x10, 0x8, 0x42C)
Slot5Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x14, 0x8, 0x42C)
Slot6Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x18, 0x8, 0x42C)
Slot7Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x1C, 0x8, 0x42C)
Slot8Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x20, 0x8, 0x42C)
Slot9Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x24, 0x8, 0x42C)
Slot10Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x28, 0x8, 0x42C)
Slot11Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x2C, 0x8, 0x42C)
Slot12Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x30, 0x8, 0x42C)
Slot13Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x34, 0x8, 0x42C)
Slot14Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x38, 0x8, 0x42C)
Slot15Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x3C, 0x8, 0x42C)
Slot16Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x40, 0x8, 0x42C)
Slot17Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x44, 0x8, 0x42C)
Slot18Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x48, 0x8, 0x42C)
}
Check_SAbility()
{
Slot1Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x4, 0x8, 0x208)
Slot2Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x8, 0x8, 0x208)
Slot3Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xC, 0x8, 0x208)
Slot4Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x10, 0x8, 0x208)
Slot5Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x14, 0x8, 0x208)
Slot6Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x18, 0x8, 0x208)
Slot7Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x1C, 0x8, 0x208)
Slot8Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x20, 0x8, 0x208)
Slot9Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x24, 0x8, 0x208)
Slot10Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x28, 0x8, 0x208)
Slot11Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x2C, 0x8, 0x208)
Slot12Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x30, 0x8, 0x208)
Slot13Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x34, 0x8, 0x208)
Slot14Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x38, 0x8, 0x208)
Slot15Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x3C, 0x8, 0x208)
Slot16Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x40, 0x8, 0x208)
Slot17Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x44, 0x8, 0x208)
Slot18Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x48, 0x8, 0x208)
Slot19Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x4C, 0x8, 0x208)
Slot20Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x50, 0x8, 0x208)
Slot21Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x54, 0x8, 0x208)
Slot22Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x58, 0x8, 0x208)
Slot23Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x5C, 0x8, 0x208)
Slot24Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x60, 0x8, 0x208)
Slot25Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x64, 0x8, 0x208)
Slot26Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x68, 0x8, 0x208)
Slot27Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x6C, 0x8, 0x208)
Slot28Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x70, 0x8, 0x208)
Slot29Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x74, 0x8, 0x208)
Slot30Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x78, 0x8, 0x208)
Slot31Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x7C, 0x8, 0x208)
Slot32Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x80, 0x8, 0x208)
Slot33Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x84, 0x8, 0x208)
Slot34Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x88, 0x8, 0x208)
Slot35Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x8C, 0x8, 0x208)
Slot36Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x90, 0x8, 0x208)
Slot37Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x94, 0x8, 0x208)
Slot38Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x98, 0x8, 0x208)
Slot39Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0x9C, 0x8, 0x208)
Slot40Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xa0, 0x8, 0x208)
Slot41Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xa4, 0x8, 0x208)
Slot42Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xa8, 0x8, 0x208)
Slot43Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xaC, 0x8, 0x208)
Slot44Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xb0, 0x8, 0x208)
Slot45Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xb4, 0x8, 0x208)
Slot46Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xb8, 0x8, 0x208)
Slot47Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xbC, 0x8, 0x208)
Slot48Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xc0, 0x8, 0x208)
Slot49Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xc4, 0x8, 0x208)
Slot50Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xc8, 0x8, 0x208)
Slot51Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xcC, 0x8, 0x208)
Slot52Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xd0, 0x8, 0x208)
Slot53Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xd4, 0x8, 0x208)
Slot54Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xd8, 0x8, 0x208)
Slot55Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xdC, 0x8, 0x208)
Slot56Ability := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC6, 0x8, 0xe0, 0x8, 0x208)
}
Check_Weapon()
{
Weapon := jelan.read(0x0058DAD4, "UInt", 0x121)
}
Check_Attack()
{
Attack := jelan.read(0x0058DAD4, "UInt", 0x178, 0xEB)
}
Check_Chat()
{
Chat := jelan.read(0x0058DAD4, "UInt", 0x1AC)
}
Check_NPCMenu()
{
NPCMenu := jelan.read(0x0058F0A4, "UInt", aOffsets*)
if(NPCMenu != 0)
{
NPCMenu = 1
}
}
Check_NPCMenuPos()
{
NPCMenuPosX := jelan.read(0x0058F0A4, "UShort", 0x9A)
NPCMenuPosY := jelan.read(0x0058F0A4, "UShort", 0x9E)
NPCMenuBuyPosX := NPCMenuPosX + 10
NPCMenuBuyPosY := NPCMenuPosY + 15
NPCMenuRepairPosX := NPCMenuPosX + 60
NPCMenuRepairPosY := NPCMenuPosY + 15
}
Crit_HM()
{
value := jelan.write(0x00527A40, CritHP, "UInt")
}
Stack_MN()
{
GuiControlGet, RMNN, , Gui_MagicNStack
value := jelan.write(0x00527ABB, RMNN, "UInt")
}
Check_NPCMsg()
{
NPCMsg := jelan.readString(0x0017E4EC, 100, "UTF-16")
}
Write_NPCMsg()
{
WriteNPCMsg := jelan.writeString(0x0017E4EC, "", "UTF-16")
}
PickUp_itemsetPS()
{
value := jelan.writeString(0x00590A00, "생명의콩", "UTF-16")
}
PickUp_itemsetPN()
{
value := jelan.writeString(0x00590A00, "빛나는가루", "UTF-16")
}
incinerate_item()
{
value := jelan.writeString(0x005909C0, inciItem , "UTF-16")
}
Check_FormNumber()
{
FormNumber := jelan.read(0x0058DAD0, "UInt", 0xC, 0x10, 0x8, 0xA0)
}
Move_Inven()
{
value := jelan.write(0x0058EB48, 302, "UInt", 0x5C)
value := jelan.write(0x0058EB48, 534, "UInt", 0x60)
}
Move_State()
{
value := jelan.write(0x0058EB48, 549, "UInt", 0x44)
value := jelan.write(0x0058EB48, 644, "UInt", 0x48)
}
Move_StateForMount()
{
value := jelan.write(0x0058EB48, 130, "UInt", 0x44)
value := jelan.write(0x0058EB48, 174, "UInt", 0x48)
}
Move_Map()
{
value := jelan.write(0x0058EB48, 400, "UInt", 0x80)
value := jelan.write(0x0058EB48, 300, "UInt", 0x84)
}
Move_Buy()
{
value := jelan.write(0x0058EB48, 233, "UInt", 0x8C)
value := jelan.write(0x0058EB48, 173, "UInt", 0x90)
}
Move_Repair()
{
value := jelan.write(0x0058EB48, 230, "UInt", 0xA4)
value := jelan.write(0x0058EB48, 170, "UInt", 0xA8)
}
Move_NPCTalkForm()
{
value := jelan.write(0x0058EB48, 135, "UInt", 0xC8)
value := jelan.write(0x0058EB48, 69, "UInt", 0xCC)
}
Get_Pos()
{
PosX := jelan.read(0x0058DAD4, "UInt", 0x10)
PosY := jelan.read(0x0058DAD4, "UInt", 0x14)
}
Get_MovePos()
{
MovePosX := jelan.read(0x0058EA10, "UInt", aOffsets*)
MovePosY := jelan.read(0x0058EA14, "UInt", aOffsets*)
}
Get_HP()
{
NowHP := jelan.read(0x0058DAD4, "UInt", 0x178, 0x5B)
MaxHP := jelan.read(0x0058DAD4, "UInt", 0x178, 0x1F)
HPPercent := Floor((NowHP / MaxHP) * 100)
}
Get_MP()
{
NowMP := jelan.read(0x0058DAD4, "UInt", 0x178, 0x5F)
MaxMP := jelan.read(0x0058DAD4, "UInt", 0x178, 0x23)
MPPercent := Floor((NowMP / MaxMP) * 100)
}
Get_FP()
{
NowFP := jelan.read(0x0058DAD4, "UInt", 0x178, 0x63)
MaxFP := jelan.read(0x0058DAD4, "UInt", 0x178, 0x27)
FPPercent := Floor((NowFP / MaxFP) * 100)
}
Get_Inven()
{
NowInven := jelan.read(0x0058DAD4, "UInt", 0x178, 0xBE, 0x14)
}
Get_Gold()
{
Gold := jelan.read(0x0058DAD4, "UInt", 0x178, 0x6F)
Gold1 := RegExReplace(Gold,  "(?<=\d)(?=(\d{3})+$)", "," )
}
Get_AGI()
{
AGI := jelan.read(0x0058DAD4, "UInt", 0x178, 0x3F)
}
Get_MsgM()
{
SetFormat, integer, H
jelanCoreMM := jelan.getModuleBaseAddress("jelancia_core.dll")
MsgMacro := jelanCoreMM + 0x000764EC
SetFormat, integer, D
MsgM := jelan.read(MsgMacro, "UChar", aOffsets*)
if(MsgM != 0)
{
Send, !2
}
}
Get_Perfect()
{
SetFormat, integer, H
jelanCorePF := jelan.getModuleBaseAddress("jelancia_core.dll")
PFOF := jelanCorePF + 0x000764EE
SetFormat, integer, D
PFOFS := jelan.read(PFOF, "UChar", aOffsets*)
if(PFOFS != 0)
{
Send, !1
}
}
Get_Location()
{
SetFormat, integer, H
jelanCoreAdd := jelan.getModuleBaseAddress("jelancia_core.dll")
LocationPointerAdd := jelanCoreAdd + 0x00076508
SetFormat, integer, D
Location := jelan.readString(LocationPointerAdd, 50, "UTF-16",0)
}
CharMovePonam()
{
if(MapNumber = 1)
{
RunDirect = 0
}
if(MapNumber = 2)
{
좌표입력(128,23,1)
RunMemory("좌표이동")
}
if(MapNumber = 3)
{
좌표입력(127,33,1)
RunMemory("좌표이동")
}
if(MapNumber = 4)
{
좌표입력(127,45,1)
RunMemory("좌표이동")
}
if(MapNumber = 5)
{
좌표입력(136,47,1)
RunMemory("좌표이동")
}
if(MapNumber = 6)
{
좌표입력(145,53,1)
RunMemory("좌표이동")
}
if(MapNumber = 7)
{
좌표입력(154,42,1)
RunMemory("좌표이동")
}
if(MapNumber = 8)
{
좌표입력(163,35,1)
RunMemory("좌표이동")
}
if(MapNumber = 9)
{
좌표입력(170,28,1)
RunMemory("좌표이동")
}
if(MapNumber = 10)
{
좌표입력(175,21,1)
RunMemory("좌표이동")
}
if(MapNumber = 11)
{
좌표입력(187,28,1)
RunMemory("좌표이동")
}
if(MapNumber = 12)
{
좌표입력(199,40,1)
RunMemory("좌표이동")
}
if(MapNumber = 13)
{
좌표입력(217,35,1)
RunMemory("좌표이동")
}
if(MapNumber = 14)
{
좌표입력(226,34,1)
RunMemory("좌표이동")
}
if(MapNumber = 15)
{
좌표입력(218,45,1)
RunMemory("좌표이동")
}
if(MapNumber = 16)
{
좌표입력(225,54,1)
RunMemory("좌표이동")
}
if(MapNumber = 17)
{
좌표입력(212,62,1)
RunMemory("좌표이동")
}
if(MapNumber = 18)
{
좌표입력(198,62,1)
RunMemory("좌표이동")
}
if(MapNumber = 19)
{
좌표입력(187,63,1)
RunMemory("좌표이동")
}
if(MapNumber = 20)
{
좌표입력(176,61,1)
RunMemory("좌표이동")
}
if(MapNumber = 21)
{
좌표입력(162,63,1)
RunMemory("좌표이동")
}
if(MapNumber = 22)
{
좌표입력(149,66,1)
RunMemory("좌표이동")
}
if(MapNumber = 23)
{
좌표입력(143,68,1)
RunMemory("좌표이동")
}
if(MapNumber = 24)
{
좌표입력(132,73,1)
RunMemory("좌표이동")
}
if(MapNumber = 25)
{
좌표입력(125,78,1)
RunMemory("좌표이동")
}
if(MapNumber = 26)
{
좌표입력(125,87,1)
RunMemory("좌표이동")
}
if(MapNumber = 27)
{
좌표입력(138,89,1)
RunMemory("좌표이동")
}
if(MapNumber = 28)
{
좌표입력(152,89,1)
RunMemory("좌표이동")
}
if(MapNumber = 29)
{
좌표입력(168,88,1)
RunMemory("좌표이동")
}
if(MapNumber = 30)
{
좌표입력(179,85,1)
RunMemory("좌표이동")
}
if(MapNumber = 31)
{
좌표입력(190,86,1)
RunMemory("좌표이동")
}
if(MapNumber = 32)
{
좌표입력(206,86,1)
RunMemory("좌표이동")
}
if(MapNumber = 33)
{
좌표입력(218,87,1)
RunMemory("좌표이동")
}
if(MapNumber = 34)
{
좌표입력(226,96,1)
RunMemory("좌표이동")
}
if(MapNumber = 35)
{
좌표입력(218,103,1)
RunMemory("좌표이동")
}
if(MapNumber = 36)
{
좌표입력(205,108,1)
RunMemory("좌표이동")
}
if(MapNumber = 37)
{
좌표입력(194,109,1)
RunMemory("좌표이동")
}
if(MapNumber = 38)
{
좌표입력(180,111,1)
RunMemory("좌표이동")
}
if(MapNumber = 39)
{
좌표입력(169,110,1)
RunMemory("좌표이동")
}
if(MapNumber = 40)
{
좌표입력(155,108,1)
RunMemory("좌표이동")
}
if(MapNumber = 41)
{
좌표입력(146,108,1)
RunMemory("좌표이동")
}
if(MapNumber = 42)
{
좌표입력(135,112,1)
RunMemory("좌표이동")
}
if(MapNumber = 43)
{
좌표입력(129,116,1)
RunMemory("좌표이동")
}
if(MapNumber = 44)
{
좌표입력(126,127,1)
RunMemory("좌표이동")
}
if(MapNumber = 45)
{
좌표입력(136,136,1)
RunMemory("좌표이동")
}
if(MapNumber = 46)
{
좌표입력(153,138,1)
RunMemory("좌표이동")
}
if(MapNumber = 47)
{
좌표입력(175,143,1)
RunMemory("좌표이동")
}
if(MapNumber = 48)
{
좌표입력(188,143,1)
RunMemory("좌표이동")
}
if(MapNumber = 49)
{
좌표입력(200,145,1)
RunMemory("좌표이동")
}
if(MapNumber = 50)
{
좌표입력(213,148,1)
RunMemory("좌표이동")
}
if(MapNumber = 51)
{
좌표입력(220,163,1)
RunMemory("좌표이동")
}
if(MapNumber = 52)
{
좌표입력(209,164,1)
RunMemory("좌표이동")
}
if(MapNumber = 53)
{
좌표입력(197,171,1)
RunMemory("좌표이동")
}
if(MapNumber = 54)
{
좌표입력(206,178,1)
RunMemory("좌표이동")
}
if(MapNumber = 55)
{
좌표입력(215,181,1)
RunMemory("좌표이동")
}
if(MapNumber = 56)
{
좌표입력(224,181,1)
RunMemory("좌표이동")
}
if(MapNumber = 57)
{
좌표입력(214,177,1)
RunMemory("좌표이동")
}
if(MapNumber = 58)
{
좌표입력(204,169,1)
RunMemory("좌표이동")
}
if(MapNumber = 59)
{
좌표입력(190,161,1)
RunMemory("좌표이동")
}
if(MapNumber = 60)
{
좌표입력(176,154,1)
RunMemory("좌표이동")
}
if(MapNumber = 61)
{
좌표입력(162,144,1)
RunMemory("좌표이동")
}
if(MapNumber = 62)
{
좌표입력(140,139,1)
RunMemory("좌표이동")
}
if(MapNumber = 63)
{
좌표입력(128,138,1)
RunMemory("좌표이동")
}
if(MapNumber = 64)
{
좌표입력(116,140,1)
RunMemory("좌표이동")
}
if(MapNumber = 65)
{
좌표입력(103,146,1)
RunMemory("좌표이동")
}
if(MapNumber = 66)
{
좌표입력(95,159,1)
RunMemory("좌표이동")
}
if(MapNumber = 67)
{
좌표입력(95,169,1)
RunMemory("좌표이동")
}
if(MapNumber = 68)
{
좌표입력(94,181,1)
RunMemory("좌표이동")
}
if(MapNumber = 69)
{
좌표입력(92,169,1)
RunMemory("좌표이동")
}
if(MapNumber = 70)
{
좌표입력(87,156,1)
RunMemory("좌표이동")
}
if(MapNumber = 71)
{
좌표입력(72,147,1)
RunMemory("좌표이동")
}
if(MapNumber = 72)
{
좌표입력(65,142,1)
RunMemory("좌표이동")
}
if(MapNumber = 73)
{
좌표입력(56,140,1)
RunMemory("좌표이동")
}
if(MapNumber = 74)
{
좌표입력(60,133,1)
RunMemory("좌표이동")
}
if(MapNumber = 75)
{
좌표입력(72,132,1)
RunMemory("좌표이동")
}
if(MapNumber = 76)
{
좌표입력(88,133,1)
RunMemory("좌표이동")
}
if(MapNumber = 77)
{
좌표입력(102,134,1)
RunMemory("좌표이동")
}
if(MapNumber = 78)
{
좌표입력(109,123,1)
RunMemory("좌표이동")
}
if(MapNumber = 79)
{
좌표입력(94,121,1)
RunMemory("좌표이동")
}
if(MapNumber = 80)
{
좌표입력(80,120,1)
RunMemory("좌표이동")
}
if(MapNumber = 81)
{
좌표입력(68,119,1)
RunMemory("좌표이동")
}
if(MapNumber = 82)
{
좌표입력(63,109,1)
RunMemory("좌표이동")
}
if(MapNumber = 83)
{
좌표입력(71,107,1)
RunMemory("좌표이동")
}
if(MapNumber = 84)
{
좌표입력(86,105,1)
RunMemory("좌표이동")
}
if(MapNumber = 85)
{
좌표입력(97,103,1)
RunMemory("좌표이동")
}
if(MapNumber = 86)
{
좌표입력(104,94,1)
RunMemory("좌표이동")
}
if(MapNumber = 87)
{
좌표입력(93,90,1)
RunMemory("좌표이동")
}
if(MapNumber = 88)
{
좌표입력(81,90,1)
RunMemory("좌표이동")
}
if(MapNumber = 89)
{
좌표입력(69,91,1)
RunMemory("좌표이동")
}
if(MapNumber = 90)
{
좌표입력(67,82,1)
RunMemory("좌표이동")
}
if(MapNumber = 91)
{
좌표입력(77,80,1)
RunMemory("좌표이동")
}
if(MapNumber = 92)
{
좌표입력(88,79,1)
RunMemory("좌표이동")
}
if(MapNumber = 93)
{
좌표입력(102,77,1)
RunMemory("좌표이동")
}
if(MapNumber = 94)
{
좌표입력(101,69,1)
RunMemory("좌표이동")
}
if(MapNumber = 95)
{
좌표입력(89,69,1)
RunMemory("좌표이동")
}
if(MapNumber = 96)
{
좌표입력(78,70,1)
RunMemory("좌표이동")
}
if(MapNumber = 97)
{
좌표입력(64,66,1)
RunMemory("좌표이동")
}
if(MapNumber = 98)
{
좌표입력(61,59,1)
RunMemory("좌표이동")
}
if(MapNumber = 99)
{
좌표입력(72,54,1)
RunMemory("좌표이동")
}
if(MapNumber = 100)
{
좌표입력(85,47,1)
RunMemory("좌표이동")
}
if(MapNumber = 101)
{
좌표입력(85,39,1)
RunMemory("좌표이동")
}
if(MapNumber = 102)
{
좌표입력(92,31,1)
RunMemory("좌표이동")
}
if(MapNumber = 103)
{
좌표입력(91,22,1)
RunMemory("좌표이동")
}
if(MapNumber = 104)
{
좌표입력(78,24,1)
RunMemory("좌표이동")
}
if(MapNumber = 105)
{
좌표입력(66,36,1)
RunMemory("좌표이동")
}
if(MapNumber = 106)
{
좌표입력(57,47,1)
RunMemory("좌표이동")
}
if(MapNumber = 107)
{
좌표입력(46,47,1)
RunMemory("좌표이동")
}
if(MapNumber = 108)
{
좌표입력(43,40,1)
RunMemory("좌표이동")
}
if(MapNumber = 109)
{
좌표입력(40,35,1)
RunMemory("좌표이동")
}
if(MapNumber = 110)
{
좌표입력(39,27,1)
RunMemory("좌표이동")
}
if(MapNumber = 111)
{
좌표입력(38,19,1)
RunMemory("좌표이동")
}
if(MapNumber = 112)
{
좌표입력(24,20,1)
RunMemory("좌표이동")
}
if(MapNumber = 113)
{
좌표입력(17,30,1)
RunMemory("좌표이동")
}
if(MapNumber = 114)
{
좌표입력(16,40,1)
RunMemory("좌표이동")
}
if(MapNumber = 115)
{
좌표입력(22,50,1)
RunMemory("좌표이동")
}
if(MapNumber = 116)
{
좌표입력(32,58,1)
RunMemory("좌표이동")
}
if(MapNumber = 117)
{
좌표입력(33,67,1)
RunMemory("좌표이동")
}
if(MapNumber = 118)
{
좌표입력(22,68,1)
RunMemory("좌표이동")
}
if(MapNumber = 119)
{
좌표입력(16,73,1)
RunMemory("좌표이동")
}
if(MapNumber = 120)
{
좌표입력(18,84,1)
RunMemory("좌표이동")
}
if(MapNumber = 121)
{
좌표입력(27,85,1)
RunMemory("좌표이동")
}
if(MapNumber = 122)
{
좌표입력(38,86,1)
RunMemory("좌표이동")
}
if(MapNumber = 123)
{
좌표입력(33,101,1)
RunMemory("좌표이동")
}
if(MapNumber = 124)
{
좌표입력(24,106,1)
RunMemory("좌표이동")
}
if(MapNumber = 125)
{
좌표입력(17,111,1)
RunMemory("좌표이동")
}
if(MapNumber = 126)
{
좌표입력(18,122,1)
RunMemory("좌표이동")
}
if(MapNumber = 127)
{
좌표입력(27,125,1)
RunMemory("좌표이동")
}
if(MapNumber = 128)
{
좌표입력(33,127,1)
RunMemory("좌표이동")
}
if(MapNumber = 129)
{
좌표입력(27,138,1)
RunMemory("좌표이동")
}
if(MapNumber = 130)
{
좌표입력(17,140,1)
RunMemory("좌표이동")
}
if(MapNumber = 131)
{
좌표입력(17,150,1)
RunMemory("좌표이동")
}
if(MapNumber = 132)
{
좌표입력(24,153,1)
RunMemory("좌표이동")
}
if(MapNumber = 133)
{
좌표입력(32,154,1)
RunMemory("좌표이동")
}
if(MapNumber = 134)
{
좌표입력(38,164,1)
RunMemory("좌표이동")
}
if(MapNumber = 135)
{
좌표입력(46,171,1)
RunMemory("좌표이동")
}
if(MapNumber = 136)
{
좌표입력(46,180,1)
RunMemory("좌표이동")
}
if(MapNumber = 137)
{
좌표입력(60,173,1)
RunMemory("좌표이동")
}
if(MapNumber = 138)
{
좌표입력(43,174,1)
RunMemory("좌표이동")
}
if(MapNumber = 139)
{
좌표입력(35,174,1)
RunMemory("좌표이동")
}
if(MapNumber = 140)
{
좌표입력(25,175,1)
RunMemory("좌표이동")
}
if(MapNumber = 141)
{
좌표입력(23,169,1)
RunMemory("좌표이동")
}
if(MapNumber = 142)
{
좌표입력(26,163,1)
RunMemory("좌표이동")
RunDirect = 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,%맵이동속도%
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,%맵이동속도%
}
}
CharMovePobuk()
{
if(MapNumber = 1)
{
좌표입력(185,124,1)
RunMemory("좌표이동")
RunDirect = 0
}
if(MapNumber = 2)
{
좌표입력(188,134,1)
RunMemory("좌표이동")
}
if(MapNumber = 3)
{
좌표입력(189,152,1)
RunMemory("좌표이동")
}
if(MapNumber = 4)
{
좌표입력(189,165,1)
RunMemory("좌표이동")
}
if(MapNumber = 5)
{
좌표입력(189,182,1)
RunMemory("좌표이동")
}
if(MapNumber = 6)
{
좌표입력(198,171,1)
RunMemory("좌표이동")
}
if(MapNumber = 7)
{
좌표입력(210,164,1)
RunMemory("좌표이동")
}
if(MapNumber = 8)
{
좌표입력(221,172,1)
RunMemory("좌표이동")
}
if(MapNumber = 9)
{
좌표입력(225,157,1)
RunMemory("좌표이동")
}
if(MapNumber = 10)
{
좌표입력(228,147,1)
RunMemory("좌표이동")
}
if(MapNumber = 11)
{
좌표입력(225,139,1)
RunMemory("좌표이동")
}
if(MapNumber = 12)
{
좌표입력(226,124,1)
RunMemory("좌표이동")
}
if(MapNumber = 13)
{
좌표입력(228,111,1)
RunMemory("좌표이동")
}
if(MapNumber = 14)
{
좌표입력(215,104,1)
RunMemory("좌표이동")
}
if(MapNumber = 15)
{
좌표입력(198,101,1)
RunMemory("좌표이동")
}
if(MapNumber = 16)
{
좌표입력(183,88,1)
RunMemory("좌표이동")
}
if(MapNumber = 17)
{
좌표입력(198,74,1)
RunMemory("좌표이동")
}
if(MapNumber = 18)
{
좌표입력(213,83,1)
RunMemory("좌표이동")
}
if(MapNumber = 19)
{
좌표입력(229,84,1)
RunMemory("좌표이동")
}
if(MapNumber = 20)
{
좌표입력(230,68,1)
RunMemory("좌표이동")
}
if(MapNumber = 21)
{
좌표입력(218,64,1)
RunMemory("좌표이동")
}
if(MapNumber = 22)
{
좌표입력(204,51,1)
RunMemory("좌표이동")
}
if(MapNumber = 23)
{
좌표입력(196,45,1)
RunMemory("좌표이동")
}
if(MapNumber = 24)
{
좌표입력(186,39,1)
RunMemory("좌표이동")
}
if(MapNumber = 25)
{
좌표입력(171,36,1)
RunMemory("좌표이동")
}
if(MapNumber = 26)
{
좌표입력(156,37,1)
RunMemory("좌표이동")
}
if(MapNumber = 27)
{
좌표입력(139,37,1)
RunMemory("좌표이동")
}
if(MapNumber = 28)
{
좌표입력(145,29,1)
RunMemory("좌표이동")
}
if(MapNumber = 29)
{
좌표입력(141,15,1)
RunMemory("좌표이동")
}
if(MapNumber = 30)
{
좌표입력(168,17,1)
RunMemory("좌표이동")
}
if(MapNumber = 31)
{
좌표입력(188,19,1)
RunMemory("좌표이동")
}
if(MapNumber = 32)
{
좌표입력(202,22,1)
RunMemory("좌표이동")
}
if(MapNumber = 33)
{
좌표입력(224,31,1)
RunMemory("좌표이동")
}
if(MapNumber = 34)
{
좌표입력(225,49,1)
RunMemory("좌표이동")
}
if(MapNumber = 35)
{
좌표입력(219,34,1)
RunMemory("좌표이동")
}
if(MapNumber = 36)
{
좌표입력(205,25,1)
RunMemory("좌표이동")
}
if(MapNumber = 37)
{
좌표입력(185,15,1)
RunMemory("좌표이동")
}
if(MapNumber = 38)
{
좌표입력(169,15,1)
RunMemory("좌표이동")
}
if(MapNumber = 39)
{
좌표입력(149,18,1)
RunMemory("좌표이동")
}
if(MapNumber = 40)
{
좌표입력(143,36,1)
RunMemory("좌표이동")
}
if(MapNumber = 41)
{
좌표입력(133,39,1)
RunMemory("좌표이동")
}
if(MapNumber = 42)
{
좌표입력(114,36,1)
RunMemory("좌표이동")
}
if(MapNumber = 43)
{
좌표입력(106,24,1)
RunMemory("좌표이동")
}
if(MapNumber = 44)
{
좌표입력(89,27,1)
RunMemory("좌표이동")
}
if(MapNumber = 45)
{
좌표입력(89,40,1)
RunMemory("좌표이동")
}
if(MapNumber = 46)
{
좌표입력(77,41,1)
RunMemory("좌표이동")
}
if(MapNumber = 47)
{
좌표입력(68,38,1)
RunMemory("좌표이동")
}
if(MapNumber = 48)
{
좌표입력(53,31,1)
RunMemory("좌표이동")
}
if(MapNumber = 49)
{
좌표입력(61,15,1)
RunMemory("좌표이동")
}
if(MapNumber = 50)
{
좌표입력(60,32,1)
RunMemory("좌표이동")
}
if(MapNumber = 51)
{
좌표입력(60,49,1)
RunMemory("좌표이동")
}
if(MapNumber = 52)
{
좌표입력(51,55,1)
RunMemory("좌표이동")
}
if(MapNumber = 53)
{
좌표입력(41,61,1)
RunMemory("좌표이동")
}
if(MapNumber = 54)
{
좌표입력(49,67,1)
RunMemory("좌표이동")
}
if(MapNumber = 55)
{
좌표입력(35,70,1)
RunMemory("좌표이동")
}
if(MapNumber = 56)
{
좌표입력(21,63,1)
RunMemory("좌표이동")
}
if(MapNumber = 57)
{
좌표입력(23,50,1)
RunMemory("좌표이동")
}
if(MapNumber = 58)
{
좌표입력(27,36,1)
RunMemory("좌표이동")
}
if(MapNumber = 59)
{
좌표입력(33,25,1)
RunMemory("좌표이동")
}
if(MapNumber = 60)
{
좌표입력(30,15,1)
RunMemory("좌표이동")
}
if(MapNumber = 61)
{
좌표입력(22,25,1)
RunMemory("좌표이동")
}
if(MapNumber = 62)
{
좌표입력(22,36,1)
RunMemory("좌표이동")
}
if(MapNumber = 63)
{
좌표입력(23,51,1)
RunMemory("좌표이동")
}
if(MapNumber = 64)
{
좌표입력(22,64,1)
RunMemory("좌표이동")
}
if(MapNumber = 65)
{
좌표입력(25,80,1)
RunMemory("좌표이동")
}
if(MapNumber = 66)
{
좌표입력(32,84,1)
RunMemory("좌표이동")
}
if(MapNumber = 67)
{
좌표입력(44,87,1)
RunMemory("좌표이동")
}
if(MapNumber = 68)
{
좌표입력(50,94,1)
RunMemory("좌표이동")
}
if(MapNumber = 69)
{
좌표입력(46,104,1)
RunMemory("좌표이동")
}
if(MapNumber = 70)
{
좌표입력(30,106,1)
RunMemory("좌표이동")
}
if(MapNumber = 71)
{
좌표입력(19,110,1)
RunMemory("좌표이동")
}
if(MapNumber = 72)
{
좌표입력(25,125,1)
RunMemory("좌표이동")
}
if(MapNumber = 73)
{
좌표입력(25,137,1)
RunMemory("좌표이동")
}
if(MapNumber = 74)
{
좌표입력(31,143,1)
RunMemory("좌표이동")
}
if(MapNumber = 75)
{
좌표입력(26,151,1)
RunMemory("좌표이동")
}
if(MapNumber = 76)
{
좌표입력(35,156,1)
RunMemory("좌표이동")
}
if(MapNumber = 77)
{
좌표입력(47,154,1)
RunMemory("좌표이동")
}
if(MapNumber = 78)
{
좌표입력(49,168,1)
RunMemory("좌표이동")
}
if(MapNumber = 79)
{
좌표입력(37,178,1)
RunMemory("좌표이동")
}
if(MapNumber = 80)
{
좌표입력(24,183,1)
RunMemory("좌표이동")
}
if(MapNumber = 81)
{
좌표입력(50,184,1)
RunMemory("좌표이동")
}
if(MapNumber = 82)
{
좌표입력(52,169,1)
RunMemory("좌표이동")
}
if(MapNumber = 83)
{
좌표입력(43,161,1)
RunMemory("좌표이동")
}
if(MapNumber = 84)
{
좌표입력(41,149,1)
RunMemory("좌표이동")
}
if(MapNumber = 85)
{
좌표입력(29,143,1)
RunMemory("좌표이동")
}
if(MapNumber = 86)
{
좌표입력(24,134,1)
RunMemory("좌표이동")
}
if(MapNumber = 87)
{
좌표입력(25,122,1)
RunMemory("좌표이동")
}
if(MapNumber = 88)
{
좌표입력(24,114,1)
RunMemory("좌표이동")
}
if(MapNumber = 89)
{
좌표입력(34,105,1)
RunMemory("좌표이동")
}
if(MapNumber = 90)
{
좌표입력(40,103,1)
RunMemory("좌표이동")
}
if(MapNumber = 91)
{
좌표입력(49,96,1)
RunMemory("좌표이동")
}
if(MapNumber = 92)
{
좌표입력(48,84,1)
RunMemory("좌표이동")
}
if(MapNumber = 93)
{
좌표입력(42,77,1)
RunMemory("좌표이동")
}
if(MapNumber = 94)
{
좌표입력(57,65,1)
RunMemory("좌표이동")
}
if(MapNumber = 95)
{
좌표입력(52,54,1)
RunMemory("좌표이동")
}
if(MapNumber = 96)
{
좌표입력(64,50,1)
RunMemory("좌표이동")
}
if(MapNumber = 97)
{
좌표입력(72,41,1)
RunMemory("좌표이동")
}
if(MapNumber = 98)
{
좌표입력(87,50,1)
RunMemory("좌표이동")
}
if(MapNumber = 99)
{
좌표입력(99,59,1)
RunMemory("좌표이동")
}
if(MapNumber = 100)
{
좌표입력(111,46,1)
RunMemory("좌표이동")
}
if(MapNumber = 101)
{
좌표입력(116,35,1)
RunMemory("좌표이동")
}
if(MapNumber = 102)
{
좌표입력(132,40,1)
RunMemory("좌표이동")
}
if(MapNumber = 103)
{
좌표입력(145,38,1)
RunMemory("좌표이동")
}
if(MapNumber = 104)
{
좌표입력(161,40,1)
RunMemory("좌표이동")
}
if(MapNumber =105 )
{
좌표입력(180,37,1)
RunMemory("좌표이동")
}
if(MapNumber = 106)
{
좌표입력(197,45,1)
RunMemory("좌표이동")
}
if(MapNumber = 107)
{
좌표입력(215,60,1)
RunMemory("좌표이동")
}
if(MapNumber = 108)
{
좌표입력(215,64,1)
RunMemory("좌표이동")
}
if(MapNumber = 109)
{
좌표입력(224,77,1)
RunMemory("좌표이동")
}
if(MapNumber = 110)
{
좌표입력(231,87,1)
RunMemory("좌표이동")
}
if(MapNumber = 111)
{
좌표입력(219,83,1)
RunMemory("좌표이동")
}
if(MapNumber = 112)
{
좌표입력(204,76,1)
RunMemory("좌표이동")
}
if(MapNumber = 113)
{
좌표입력(189,81,1)
RunMemory("좌표이동")
}
if(MapNumber = 114)
{
좌표입력(180,87,1)
RunMemory("좌표이동")
}
if(MapNumber = 115)
{
좌표입력(167,95,1)
RunMemory("좌표이동")
}
if(MapNumber = 116)
{
좌표입력(158,100,1)
RunMemory("좌표이동")
}
if(MapNumber = 117)
{
좌표입력(142,100,1)
RunMemory("좌표이동")
}
if(MapNumber = 118)
{
좌표입력(128,91,1)
RunMemory("좌표이동")
}
if(MapNumber = 119)
{
좌표입력(118,76,1)
RunMemory("좌표이동")
}
if(MapNumber = 120)
{
좌표입력(99,81,1)
RunMemory("좌표이동")
}
if(MapNumber = 121)
{
좌표입력(84,77,1)
RunMemory("좌표이동")
}
if(MapNumber = 122)
{
좌표입력(81,93,1)
RunMemory("좌표이동")
}
if(MapNumber = 123)
{
좌표입력(83,103,1)
RunMemory("좌표이동")
}
if(MapNumber = 124)
{
좌표입력(84,119,1)
RunMemory("좌표이동")
}
if(MapNumber = 125)
{
좌표입력(104,119,1)
RunMemory("좌표이동")
}
if(MapNumber = 126)
{
좌표입력(126,119,1)
RunMemory("좌표이동")
}
if(MapNumber = 127)
{
좌표입력(143,123,1)
RunMemory("좌표이동")
}
if(MapNumber = 128)
{
좌표입력(151,132,1)
RunMemory("좌표이동")
}
if(MapNumber = 129)
{
좌표입력(170,141,1)
RunMemory("좌표이동")
}
if(MapNumber = 130)
{
좌표입력(177,155,1)
RunMemory("좌표이동")
}
if(MapNumber = 131)
{
좌표입력(165,165,1)
RunMemory("좌표이동")
}
if(MapNumber = 132)
{
좌표입력(153,163,1)
RunMemory("좌표이동")
}
if(MapNumber = 133)
{
좌표입력(148,176,1)
RunMemory("좌표이동")
}
if(MapNumber = 134)
{
좌표입력(127,173,1)
RunMemory("좌표이동")
}
if(MapNumber = 135)
{
좌표입력(101,172,1)
RunMemory("좌표이동")
}
if(MapNumber = 136)
{
좌표입력(77,171,1)
RunMemory("좌표이동")
}
if(MapNumber = 137)
{
좌표입력(77,155,1)
RunMemory("좌표이동")
}
if(MapNumber = 138)
{
좌표입력(76,138,1)
RunMemory("좌표이동")
}
if(MapNumber = 139)
{
좌표입력(64,128,1)
RunMemory("좌표이동")
}
if(MapNumber = 140)
{
좌표입력(70,114,1)
RunMemory("좌표이동")
}
if(MapNumber = 141)
{
좌표입력(83,105,1)
RunMemory("좌표이동")
}
if(MapNumber = 142)
{
좌표입력(89,93,1)
RunMemory("좌표이동")
}
if(MapNumber = 143)
{
좌표입력(77,82,1)
RunMemory("좌표이동")
}
if(MapNumber = 144)
{
좌표입력(86,75,1)
RunMemory("좌표이동")
}
if(MapNumber = 145)
{
좌표입력(101,85,1)
RunMemory("좌표이동")
}
if(MapNumber = 146)
{
좌표입력(120,99,1)
RunMemory("좌표이동")
}
if(MapNumber = 147)
{
좌표입력(128,113,1)
RunMemory("좌표이동")
}
if(MapNumber = 148)
{
좌표입력(132,124,1)
RunMemory("좌표이동")
}
if(MapNumber = 149)
{
좌표입력(128,137,1)
RunMemory("좌표이동")
}
if(MapNumber = 150)
{
좌표입력(124,150,1)
RunMemory("좌표이동")
}
if(MapNumber = 151)
{
좌표입력(134,150,1)
RunMemory("좌표이동")
}
if(MapNumber = 152)
{
좌표입력(144,143,1)
RunMemory("좌표이동")
RunDirect = 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,%맵이동속도%
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,%맵이동속도%
}
}
ReadAbility(WeaponName)
{
SetFormat, integer, h
if(WeaponName = "격투")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0xA9, 0xAC, 0x2C, 0xD2)
}
if(WeaponName = "검")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0x80, 0xAC)
}
if(WeaponName = "단검")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0xE8, 0xB2, 0x80, 0xAC)
}
if(WeaponName = "도")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0xC4, 0xB3, 0x00)
}
if(WeaponName = "도끼")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0xC4, 0xB3, 0x7C, 0xB0)
}
if(WeaponName = "거대도끼")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0x70, 0xAC, 0x00, 0xB3, 0xC4, 0xB3, 0x7C, 0xB0)
}
if(WeaponName = "대검")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0x00, 0xB3, 0x80, 0xAC)
}
if(WeaponName = "대도")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0x00, 0xB3, 0xC4, 0xB3)
}
if(WeaponName = "창, 특수창")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0x3D, 0xCC, 0x2C, 0x00, 0x20, 0x00, 0xB9, 0xD2, 0x18, 0xC2, 0x3D, 0xCC)
}
if(WeaponName = "봉, 해머")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0x09, 0xBD, 0x2C, 0x00, 0x20, 0x00, 0x74, 0xD5, 0x38, 0xBA)
}
if(WeaponName = "현금")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0x04, 0xD6, 0x08, 0xAE)
}
if(WeaponName = "활")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0x5C, 0xD6)
}
if(WeaponName = "거대검")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0x70, 0xAC, 0x00, 0xB3, 0x80, 0xAC)
}
if(WeaponName = "거대도")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0x70, 0xAC, 0x00, 0xB3, 0xC4, 0xB3)
}
if(WeaponName = "양손단검")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0x91, 0xC5, 0x90, 0xC1, 0xE8, 0xB2, 0x80, 0xAC)
}
if(WeaponName = "양손도끼")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0x91, 0xC5, 0x90, 0xC1, 0xC4, 0xB3, 0x7C, 0xB0)
}
if(WeaponName = "스태프")
{
ReadAbilityADD := jelan.processPatternScan( 0x00000000, 0x7FFFFFFF, 0x18, 0x20, 0x53, 0x00, 0xA4, 0xC2, 0xDC, 0xD0, 0x04, 0xD5)
}
ReadAbilityADD := ReadAbilityADD + 0x208
SetFormat, integer, d
ReadAbility := jelan.read(ReadAbilityADD, "UInt", aOffsets*)
Return, ReadAbility
}
ReadAbilityNameValue()
{
AbilityName := jelan.readString(AbilityNameADD, 20, "UTF-16", aOffsets*)
AbilityValue := jelan.read(AbilityValueADD, "UShort", aOffsets*)
}
SendWeaponName(WeaponName)
{
if(WeaponName = "격투")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, rurxn{Enter}
}
if(WeaponName = "검")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, rja{Enter}
}
if(WeaponName = "단검")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, eksrja{Enter}
}
if(WeaponName = "도")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, eh{Enter}
}
if(WeaponName = "도끼")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, ehRl{Enter}
}
if(WeaponName = "거대도끼")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, rjeoehRl{Enter}
}
if(WeaponName = "대검")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, eorja{Enter}
}
if(WeaponName = "대도")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, eoeh{Enter}
}
if(WeaponName = "창, 특수창")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, ckd,{Space}xmrtnckd{Enter}
}
if(WeaponName = "봉, 해머")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, qhd,{Space}goaj{Enter}
}
if(WeaponName = "현금")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, gusrma{Enter}
}
if(WeaponName = "활")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, ghkf{Enter}
}
if(WeaponName = "거대검")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, rjeorja{Enter}
}
if(WeaponName = "거대도")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, rjeoeh{Enter}
}
if(WeaponName = "양손단검")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, didthseksrja{Enter}
}
if(WeaponName = "양손도끼")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, didthsehRl{Enter}
}
if(WeaponName = "스태프")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, tmxovm{Enter}
}
if(WeaponName = "대화")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, eoghk{Enter}
}
if(WeaponName = "명상")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, audtkd{Enter}
}
if(WeaponName = "집중")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, wlqwnd{Enter}
}
if(WeaponName = "회피")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, ghlvl{Enter}
}
if(WeaponName = "몸통지르기")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, ahaxhdwlfmrl{Enter}
}
if(WeaponName = "민첩향상")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, alscjqgidtkd{Enter}
}
if(WeaponName = "체력향상")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, cpfurgidtkd{Enter}
}
if(WeaponName = "활방어")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, ghkfqkddj{Enter}
}
if(WeaponName = "RemoveArmor")
{
ime_status := % IME_CHECK("A")
if (ime_status = "1")
{
Send, {vk15sc138}
Sleep, 100
}
Send, RemoveArmor{Enter}
}
if(WeaponName = "더블어택")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, ejqmfdjxor{Enter}
}
if(WeaponName = "엘")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, dpf{Enter}
}
if(WeaponName = "테스")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, xptm{Enter}
}
if(WeaponName = "마하")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, akgk{Enter}
}
if(WeaponName = "브리깃드")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, qmflrltem{Enter}
}
if(WeaponName = "다뉴")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, eksb{Enter}
}
if(WeaponName = "브라키")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, qmfkzl{Enter}
}
}
SendMagicName(MagicName)
{
if(MagicName = "쿠로")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, znfh{Enter}
}
if(MagicName = "나프")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, skvm{Enter}
}
if(MagicName = "베네피쿠스")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, qpspvlzntm{Enter}
}
if(MagicName = "브리스")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, qmfltm{Enter}
}
if(MagicName = "파라스")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, vkfktm{Enter}
}
if(MagicName = "파스티")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, vktmxl{Enter}
}
if(MagicName = "다라")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, ekfk{Enter}
}
if(MagicName = "마스")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, aktm{Enter}
}
if(MagicName = "라크")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, fkzm{Enter}
}
if(MagicName = "슈키")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, tbzl{Enter}
}
if(MagicName = "클리드")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, zmfflem{Enter}
}
if(MagicName = "저주")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, wjwn{Enter}
}
if(MagicName = "번개")
{
ime_status := % IME_CHECK("A")
if (ime_status = "0")
{
Send, {vk15sc138}
Sleep, 100
}
Send, qjsro{Enter}
}
}
UriEncode(Uri, Enc = "UTF-8")
{
StrPutVar(Uri, Var, Enc)
f := A_FormatInteger
SetFormat, IntegerFast, H
Loop
{
Code := NumGet(Var, A_Index - 1, "UChar")
If (!Code)
Break
If (Code >= 0x30 && Code <= 0x39
|| Code >= 0x41 && Code <= 0x5A
|| Code >= 0x61 && Code <= 0x7A)
Res .= Chr(Code)
Else
Res .= "%" . SubStr(Code + 0x100, -1)
}
SetFormat, IntegerFast, %f%
Return, Res
}
UriDecode(Uri, Enc = "UTF-8")
{
Pos := 1
Loop
{
Pos := RegExMatch(Uri, "i)(?:%[\da-f]{2})+", Code, Pos++)
If (Pos = 0)
Break
VarSetCapacity(Var, StrLen(Code) // 3, 0)
StringTrimLeft, Code, Code, 1
Loop, Parse, Code, `%
NumPut("0x" . A_LoopField, Var, A_Index - 1, "UChar")
StringReplace, Uri, Uri, `%%Code%, % StrGet(&Var, Enc), All
}
Return, Uri
}
StrPutVar(Str, ByRef Var, Enc = "")
{
Len := StrPut(Str, Enc) * (Enc = "UTF-16" || Enc = "CP1200" ? 2 : 1)
VarSetCapacity(Var, Len, 0)
Return, StrPut(Str, &Var, Enc)
}
return

checktxt()
{
Try
{
RETURN pwb.document.querySelectorAll("[class='login_wrap']")[0].InnerText
}
Catch e
{
GROUPADD, ie_gruop, ahk_exe iexplore.exe
WINKILL, ahk_exe iexplore.exe
WINKILL, ahk_group ie_gruop
GOSUB, RL
}
}
iereturn()
{
Try
{
RETURN ComObjCreate("InternetExplorer.Application")
}
Catch e
{
GROUPADD, ie_gruop, ahk_exe iexplore.exe
WINKILL, ahk_exe iexplore.exe
WINKILL, ahk_group ie_gruop
GOSUB, RL
}
}
iereturn1()
{
Try
{
RETURN ComObjCreate("InternetExplorer.Application")
}
Catch e
{
GROUPADD, ie_gruop, ahk_exe iexplore.exe
WINKILL, ahk_exe iexplore.exe
WINKILL, ahk_group ie_gruop
GOSUB, RL
}
}
SkinForm(Param1 = "Apply", DLL = "", SkinName = "")
{
if(Param1 = Apply)
{
DllCall("LoadLibrary", str, DLL)
DllCall(DLL . "\USkinInit", Int,0, Int,0, AStr, SkinName)
}
else if(Param1 = 0)
{
DllCall(DLL . "\USkinExit")
}
}
RunThread(Addrs)
{
Gui,Submit,Nohide
SetTitleMatchMode, 3
WinGet, pid, PID, ahk_pid %jPID%
RT_Delay := A_TickCount - RunThreadCounter
if (RT_Delay < 30)
{
sleep, %RT_Delay%
}
RunThreadCounter := A_TickCount
ProcHwnd := DllCall("OpenProcess", "UInt", 2035711, "Int", 0, "UInt", pid)
ThreadHandle := DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "UInt", 0, "Ptr", Addrs, "Ptr", 0, "UInt", 0, "PtrP", 0)
DllCall("CloseHandle", "Ptr", ThreadHandle)
return
}
RunMemory(코드)
{
SetFormat, Integer, H
Run_Thread := 0
if (코드 = "은행넣기")  {
Run_Thread := 1
Addrs := 0x00590005
}
else if (코드 = "은행빼기")  {
Run_Thread := 1
Addrs := 0x00590251
}
else if (코드 = "하나씩소각")  {
Run_Thread := 1
Addrs := 0x00590097
}
else if (코드 = "무기탈거")  {
Run_Thread := 1
Addrs := 0x0058D250
}
else if (코드 = "스킬사용")  {
Run_Thread := 1
Addrs := 0x0058D600
}
else if (코드 = "마법사용")  {
Run_Thread := 1
Addrs := 0x00590400
}
else if (코드 = "타겟스킬사용")  {
Run_Thread := 1
Addrs := 0x00590200
}
else if (코드 = "파티걸기") {
Run_Thread := 1
Addrs := 0x0058FE00
}
else if (코드 = "NPC호출") {
Run_Thread := 1
Addrs := 0x00527B4C
}
else if (코드 = "따라가기") {
gui,submit,nohide
Run_Thread := 1
Addrs := 0x00590740
}
else if (코드 = "공격하기") {
gui,submit,nohide
Run_Thread := 1
Addrs := 0x00590700
}
else if (코드 = "좌표이동") {
gui,submit,nohide
Run_Thread := 1
Addrs := 0x00590620
}
else if (코드 = "퀵슬롯사용") {
Run_Thread := 1
Addrs := 0x0058D300
}
else if (코드 = "섭팅하기") {
Run_Thread :=
Addrs :=
}
if (Run_Thread = 1) {
useskill := RunThread(Addrs)
}
SetFormat, Integer, D
return
}
RemoteM()
{
SetTitleMatchMode, 3
WinGet, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0052794C, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
WriteMemory2(WVALUE,MADDRESS,PROGRAM)
{
Process, wait, %PROGRAM%, 0.5
PID = %ErrorLevel%
if PID = 0
{
Return
}
ProcessHandle := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
DllCall("WriteProcessMemory", "UInt", ProcessHandle, "UInt", MADDRESS, "Uint*", WVALUE, "Uint", 07, "Uint *", 0)
DllCall("CloseHandle", "int", ProcessHandle)
Return
}
WriteExecutableMemory(코드)
{
if (코드 = "타겟스킬사용") {
Addrs := 0x00590200
RegionSize := 0x100
target = 00B900590220B8F424DCE80000000000000000C3FF
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "타겟스킬호출") {
Addrs := 0x00590220
RegionSize := 0x50
target = 00003E0054803800000054804400000000000000000201000000
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "마법사용") {
Addrs := 0x00590400
RegionSize := 0x100
target = 00B900590420B8F424DCE80000000000000000C3FF
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "마법호출") {
Addrs := 0x00590420
RegionSize := 0x50
target = 00000F00548038000000548044000000000000000000010201000000
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "스킬사용") {
Addrs := 0x0058D600
RegionSize := 0x20
target = DF95E8016A006AC35959FFF2
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "무기탈거") {
Addrs := 0x0058D250
RegionSize := 0x100
target = 01BB000000F8B8000000BE000000F6C45EE81E6A00FFF4487BE859FFD5E8FF438DF88B8D194788FFEF7224448D50242444FFED5D55E8501C89661824448B662424448B661A47E8C78B1C478966C3FFF4544A
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "좌표이동") {
Addrs := 0x00590600
RegionSize := 0x200
target = 000000000000000000000000010000000000000000000000000000003D836000000000840FFF00590600DAD4A100000084F8830C408B005800000073840F0200005906B93D838D0000000B850FB9A300590600055906B9A10059060F04488B188B0058DAD4A153D9AF14488B10588B000FD83958D9AF0F05830000002085B9A110005906B90F003883005906058D0000000B8506B9A3005906005906B9058D0059DAD41D8B008B00EFE850016A00580C4D8B61FFF3A200590600C3C129590600
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "게임내시간제어")  {
Addrs := 0x0058FF80
RegionSize := 0x200
target = 010058FFE03D8B05030058FF9A3D6C46890058FF9AFFE7FBAFE9
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "NPC호출용1") {
Addrs := 0x00527ACC
RegionSize := 0x200
target = 00004300547E10000000547E1C00000000000000000000010100000000000000
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "NPC호출용2") {
Addrs := 0x00527B4C
target = 40C700527ACCB88EE8000000001A000000C3FFFAAB0000000000000000000000
}
else if (코드 = "몬스터주소기록켜기") {
Addrs := 0x0048E1EB
target = 1024001025A0E9
}
else if (코드 = "몬스터주소기록끄기") {
Addrs := 0x0048E1EB
target = 10245489CB3966
}
else if (코드 = "몬스터주소기록함수") {
Addrs := 0x00590790
RegionSize := 0x200
target = 89005907D0A160005907D0058130D03D810000000400590800005907C700401F0F0E7507D4005907D00500000001B80059245489CB39666100FFEFDA24E910
executable := jelan.executable(Addrs, RegionSize)
jelan.write(0x005907D0,0x005907D4,"uint",aOffsets*)
}
else if (코드 = "공격하기") {
Addrs := 0x00590700
RegionSize := 0x100
target = 0D8B00000001B80002BA0059073058DAD4358B0000F3A3B0E8515000C3FF
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "퀵슬롯사용") {
Addrs := 0x0058D300
RegionSize := 0x20
target = 0058DAD4BF016AC3FFF26A64E8
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "마법모션제거") {
Addrs := 0x00527E00
RegionSize := 0x40
target = 000000042444C7E9F98B57565300000000FFFA36F7
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "공속"){
Addrs := 0x00527B35
RegionSize := 0x20
target = 0D5ED90004668300003E000F46C7FFF38CD2E9
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "1무바"){
Addrs := 0x005FB740
RegionSize := 0x600
target = 1D89005FB876A3960D89005FB886B8A61589005FB85FB8B63589005F005FB8C63D89000058DAD4058B6039000001A5808B840F005FB85E05B85EA3000000B25FB86E3D83005F00000D840F0000005FB86E3D830000000078840F0183005FB87205FF0F03005FB8723D05C7000000808C000001005FB872BB000000F8B8000000BE00000001DEF6E81E6A0000ED6313E859FFEFE8FF438DF88BFF194788FFE88D6D448D502424448DE677EDE8501C24661824448B66FF24448B661A4789C78B1C4789662405C7FFED6EE2E8000001005FB86E58DAD4BF006A00C7FFEB8551E8000000005FB86E0500000000E90000005FB876058B618B005FB8861D8B158B005FB8960DB6358B005FB8A6B8C63D8B005FB80001A48589005F00FFED0635E900000000000000000000000000000000010000000000
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "2무바"){
Addrs := 0x005FB740
RegionSize := 0x600
target = 1D89005FB923A3430D89005FB933B9531589005FB95FB9633589005F005FB9733D89000058DAD4058B6039000001A5808B840F005FB90B05B90BA30000015F5FB91B3D83005F000027840F0000005FB91B3D830000000092840F0102005FB91B3D8383000000A0840F0F03005FB91B3D05FF0000010B841F3D83005FB91F138C0F03005FB9B91F05C7000001B800000001005F0001BB000000F800000000BE0000FFEFDEDCE81E6A8BFFED62F9E8598D53E8FF438DF8448D194788FFE81C24448D50242466FFE677D3E8504789661824448B662424448B661AC8E8C78B1C4789B91B05C7FFED6E6A00000001005FE80058DAD4BF001B05C7FFEB853700000002005FB905FF00000093E91F3D83005FB91F808C0F03005FB9B91F05C7000000B800000001005F0001BB000000F800000000BE0000FFEFDE49E81E6A8BFFED6266E8598CC0E8FF438DF8448D194788FFE81C24448D50242466FFE67740E8504789661824448B662424448B661A35E8C78B1C4789B91B05C7FFED6E6A00000003005FE80058DAD4BF011B05C7FFEB84A400000000005FB98B6100000000E91D8B005FB92305430D8B005FB933B953158B005FB95FB963358B005F005FB9733D8B00E9000001A48589000000FFED058800000000000000000000000000000000000100000000000000000000
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "3무바"){
Addrs := 0x005FB740
RegionSize := 0x600
target = 1D89005FB9D0A3F00D89005FB9E0BA001589005FB95FBA103589005F005FBA203D89000058DAD4058B6039000001A5808B840F005FB9B805B9B8A30000020C5FB9C83D83005F000041840F0000005FB9C83D8300000000AC840F0102005FB9C83D8383000000BA840F0F03005FB9C83D3D830000012584840F04005FB9C8C83D83000001339E840F05005FB9B9CC05FF0000015FB9CC3D83005F0001A68C0F0300005FB9CC05C70000F8B80000000100000001BB00001E6A00000000BEE859FFEFDEC2E88DF88BFFED62DFFFE88D39E8FF432424448D194788E8501C24448D50448B66FFE677B9661A47896618244789662424448BED6EAEE8C78B1C005FB9C805C7FFBF006A00000001851DE80058DAD45FB9C805C7FFEB26E90000000200B9CC05FF0000015FB9CC3D83005F0001138C0F0300005FB9CC05C70000F8B80000000100000001BB00001E6A00000000BEE859FFEFDE2FE88DF88BFFED624CFFE88CA6E8FF432424448D194788E8501C24448D50448B66FFE67726661A47896618244789662424448BED6E1BE8C78B1C005FB9C805C7FFBF016A00000003848AE80058DAD45FB9C805C7FFEB93E90000000400B9CC05FF0000005FB9CC3D83005F0000808C0F0300005FB9CC05C70000F8B80000000100000001BB00001E6A00000000BEE859FFEFDD9CE88DF88BFFED61B9FFE88C13E8FF432424448D194788E8501C24448D50448B66FFE67693661A47896618244789662424448BED6D88E8C78B1C005FB9C805C7FFBF026A0000000583F7E80058DAD45FB9C805C7FFEB00E90000000000D0058B61000000B9E01D8B005FB95FB9F00D8B005F005FBA00158B008B005FBA10358B8589005FBA203D04DBE9000001A40000000000FFED0000000000000000000000000000000000000001000000000000000000000000000000
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "2벗무바"){
Addrs := 0x005FB740
RegionSize := 0x600
target = 1D89005FB87BA39B0D89005FB88BB8AB1589005FB85FB8BB3589005F005FB8CB3D89000058DAD4058B6039000001A5808B840F005FB86305B863A3000000B75FB8733D83005F00000D840F0000005FB8733D83000000007D840F0183005FB87705FF0F03005FB8773D05C7000000658C000001005FB873005FB87705C70000F8B80000000100000001BB00001E6A00000000BEE859FFEFDEECE88DF88BFFED6309FFE88D63E8FF432424448D194788E8501C24448D50448B66FFE677E3661A47896618244789662424448BED6ED8E8C78B1C6A0000001BE9FFE80058DAD4BF007305C7FFEB854C00000000005FB88B6100000000E91D8B005FB87B059B0D8B005FB88BB8AB158B005FB85FB8BB358B005F005FB8CB3D8B00E9000001A48589000000FFED0630000000000000000000000000000000000001000000
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "3벗무바"){
Addrs := 0x005FB740
RegionSize := 0x600
target = 1D89005FB92BA34B0D89005FB93BB95B1589005FB95FB96B3589005F005FB97B3D89008B0058DAD4A1600539000001A58069840F005FB9135FB913A3000001005FB9233D830000000027840F0001005FB9233D838300000097840F0F02005FB9233D3D83000000A584840F03005FB9232705FF00000115B9273D83005FB900658C0F03005F5FB92305C7000005C70000000100000001005FB927BB000000F8B8000000BE00000001DED3E81E6A0000ED62F0E859FFEFE8FF438DF88BFF194788FFE88D4A448D502424448DE677CAE8501C24661824448B66FF24448B661A4789C78B1C47896624B3E9FFED6EBFE8D4BF006A000000EB8533E80058DA005FB92305C7FF0098E9000000025FB92705FF0000005FB9273D8300000000858C0F0301005FB92705C70000F8B8000000BE00000001BB00E81E6A0000000062E859FFEFDE45438DF88BFFED6288FFE88CBCE8FF502424448D19473CE8501C24448D24448B66FFE6778B661A478966181C478966242444FFED6E31E8C78B03005FB92305C700001BE900000058DAD4BF016A00C7FFEB849BE8000000005FB9230500000000E900008B005FB92BA1610D8B005FB93B1D5B158B005FB94BB96B358B005FB95FB97B3D8B005F000001A48589000000FFED0580E90000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "3벗무바1"){
Addrs := 0x005FB740
RegionSize := 0x600
target = 8B0000011DE960850F5E4739624081909000000068B0010000011DBF000056840F0000EB5805FF90900058EB583D83005800005A840F00000058EB583D8300000000A0840F01030058EB583D838300000040840F0F040058EB583D3D830000009584840F060058EB58583D83000000268D840F070058EB640D8B610000000086968B0058F0FFE62568E80000FFE64E6BE9F88B01BB000000F8B8000000BE000000EFDEDBE81E6A00FFED62F8E859FF52E8FF438DF88B8D194788FFE88D24448D50242444FFE677D2E8501C89661824448B662424448B661A47E8C78B1C4789666A94EBFFED6EC70058DAD43D8B0085EBFFEB853DE858DAD43D8B016AE9FFEB852EE8008B006AFFFFFF731CE80058DAD43DEB5805C7FFEB85E9000000010058D4058BFFFFFF578504CF390058DAFED0E9FFFFFF490000000000FFFF
executable := jelan.executable(Addrs, RegionSize)
}
else if (코드 = "4벗무바"){
Addrs := 0x005FB740
RegionSize := 0x600
target = 1D89005FB9D5A3F50D89005FB9E5BA051589005FB95FBA153589005F005FBA253D89000058DAD4058B6039000001A5808B840F005FB9BD05B9BDA3000002115FB9CD3D83005F000041840F0000005FB9CD3D8300000000B1840F0102005FB9CD3D8383000000BF840F0F03005FB9CD3D3D830000012A84840F04005FB9CDCD3D8300000138A3840F05005FB9B9D105FF0000015FB9D13D83005F0000658C0F0300005FB9CD05C700D105C70000000100000001005FB901BB000000F8B8000000BE000000EFDEB8E81E6A00FFED62D5E859FF2FE8FF438DF88B8D194788FFE88D24448D50242444FFE677AFE8501C89661824448B662424448B661A47E8C78B1C4789660141E9FFED6EA4DAD4BF006A0000FFEB8518E8005802005FB9CD05C7000126E9000000005FB9D105FF0003005FB9D13D83C7000001138C0F0001005FB9D105000000F8B8000000BE00000001BB2AE81E6A0000006247E859FFEFDEFF438DF88BFFED4788FFE88CA1E88D502424448D197721E8501C24441824448B66FFE6448B661A4789668B1C4789662424C7FFED6E16E8C70003005FB9CD05DAD4BF016A0000FFEB8485E8005804005FB9CD05C7000093E9000000005FB9D105FF0003005FB9D13D83C7000000808C0F0001005FB9D105000000F8B8000000BE00000001BB97E81E6A00000061B4E859FFEFDDFF438DF88BFFED4788FFE88C0EE88D502424448D19768EE8501C24441824448B66FFE6448B661A4789668B1C4789662424C7FFED6D83E8C70005005FB9CD05DAD4BF026A0000FFEB83F2E8005800005FB9CD05C7000000E90000005FB9D5058B6100005FB9E51D8B008B005FB9F50D8B358B005FBA0515253D8B005FBA1501A48589005FBAFFED04D6E9000000000000000000000000000000000100000000000000000000000000
executable := jelan.executable(Addrs, RegionSize)
}
ContentLength := StrLen(target)
LoopCount := ContentLength // 14
LastLoopLength := ContentLength - (LoopCount * 14)
if (LastLoopLength > 0)
{
LoopCount := LoopCount + 1
}
Loop, % LoopCount
{
A := SubStr(target, 1, 14)
C := Substr(target, 15)
target := C
jelan.write7bytes(Addrs, "0x"A)
Addrs := Addrs+7
}
return
}
WPdisablescript()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
WPDValue = 01BB000000F8B8
WPDValue2 = 000000BE000000
WPDValue3 = F6C45EE81E6A00
WPDValue4 = FFF4487BE859FF
WPDValue5 = D5E8FF438DF88B
WPDValue6 = 8D194788FFEF72
WPDValue7 = 24448D50242444
WPDValue8 = FFED5D55E8501C
WPDValue9 = 89661824448B66
WPDValue10 = 2424448B661A47
WPDValue11 = E8C78B1C478966
WPDValue12 = C3FFF4544A
Addrs := 0x0058D250
H1 := SubStr(WPDValue, 1, 14)
H2 := Substr(WPDValue2, 1, 14)
H3 := Substr(WPDValue3, 1, 14)
H4 := Substr(WPDValue4, 1, 14)
H5 := Substr(WPDValue5, 1, 14)
H6 := Substr(WPDValue6, 1, 14)
H7 := Substr(WPDValue7, 1, 14)
H8 := Substr(WPDValue8, 1, 14)
H9 := Substr(WPDValue9, 1, 14)
H10 := Substr(WPDValue10, 1, 14)
H11 := Substr(WPDValue11, 1, 14)
H12 := Substr(WPDValue12, 1, 14)
writememory2("0x"H1, Addrs, PID)
writememory2("0x"H2, Addrs+7, PID)
writememory2("0x"H3, Addrs+14, PID)
writememory2("0x"H4, Addrs+21, PID)
writememory2("0x"H5, Addrs+28, PID)
writememory2("0x"H6, Addrs+35, PID)
writememory2("0x"H7, Addrs+42, PID)
writememory2("0x"H8, Addrs+49, PID)
writememory2("0x"H9, Addrs+56, PID)
writememory2("0x"H10, Addrs+63, PID)
writememory2("0x"H11, Addrs+70, PID)
writememory2("0x"H12, Addrs+77, PID)
}
무바활성화()
{
jelan.write(0x004CBE8D, 0xE9, "char", aOffsets*)
jelan.write(0x004CBE8E, 0xAE, "char", aOffsets*)
jelan.write(0x004CBE8F, 0xF8, "char", aOffsets*)
jelan.write(0x004CBE90, 0x12, "char", aOffsets*)
jelan.write(0x004CBE91, 0x00, "char", aOffsets*)
jelan.write(0x004CBE92, 0xC3, "char", aOffsets*)
}
무바비활성화()
{
jelan.write(0x004CBE8D, 0x89, "char", aOffsets*)
jelan.write(0x004CBE8E, 0x85, "char", aOffsets*)
jelan.write(0x004CBE8F, 0xA4, "char", aOffsets*)
jelan.write(0x004CBE90, 0x01, "char", aOffsets*)
jelan.write(0x004CBE91, 0x00, "char", aOffsets*)
jelan.write(0x004CBE92, 0x00, "char", aOffsets*)
}
캐릭제거()
{
jelan.write(0x0045D28F, 0xE9, "Char", aOffsets*)
jelan.write(0x0045D290, 0x8A, "Char", aOffsets*)
jelan.write(0x0045D291, 0x0A, "Char", aOffsets*)
jelan.write(0x0045D292, 0x00, "Char", aOffsets*)
jelan.write(0x0045D293, 0x00, "Char", aOffsets*)
}
캐릭보이기()
{
jelan.write(0x0045D28F, 0x0F, "Char", aOffsets*)
jelan.write(0x0045D290, 0x84, "Char", aOffsets*)
jelan.write(0x0045D291, 0xC2, "Char", aOffsets*)
jelan.write(0x0045D292, 0x00, "Char", aOffsets*)
jelan.write(0x0045D293, 0x00, "Char", aOffsets*)
}
포북캐릭()
{
jelan.write(0x0045DAA9, 0x90, "Char", aOffsets*)
jelan.write(0x0045DAAA, 0x90, "Char", aOffsets*)
jelan.write(0x0045DAAB, 0x90, "Char", aOffsets*)
jelan.write(0x0045D32E, 0x90, "Char", aOffsets*)
jelan.write(0x0045D32F, 0x90, "Char", aOffsets*)
jelan.write(0x0045D330, 0x90, "Char", aOffsets*)
jelan.write(0x0045D4E7, 0x90, "Char", aOffsets*)
jelan.write(0x0045D4E8, 0x90, "Char", aOffsets*)
jelan.write(0x0045D4E9, 0x90, "Char", aOffsets*)
jelan.write(0x0045D43E, 0x90, "Char", aOffsets*)
jelan.write(0x0045D43F, 0x90, "Char", aOffsets*)
jelan.write(0x0045D440, 0x90, "Char", aOffsets*)
jelan.write(0x0045D422, 0x90, "Char", aOffsets*)
jelan.write(0x0045D423, 0x90, "Char", aOffsets*)
jelan.write(0x0045D424, 0x90, "Char", aOffsets*)
jelan.write(0x0045D98B, 0x90, "Char", aOffsets*)
jelan.write(0x0045D98C, 0x90, "Char", aOffsets*)
jelan.write(0x0045D98D, 0x90, "Char", aOffsets*)
jelan.write(0x0045DA94, 0x90, "Char", aOffsets*)
jelan.write(0x0045DA95, 0x90, "Char", aOffsets*)
jelan.write(0x0045DA96, 0x90, "Char", aOffsets*)
}
WPD()
{
SetTitleMatchMode, 3
WinGet, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D250, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
JJscript()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "JJnt", PID, "JJnt")
JJValue = 8B0058EB1CA160
JJValue2 = A53D0000010E80
JJValue3 = 0026840F00000F
JJValue4 = 00000000E90000
JJValue5 = 808B0058EB1CA1
JJValue6 = 0FA33D0000010E
JJValue7 = 00000B840F0000
JJValue8 = E58B5B5E5F6100
JJValue9 = 5256FFED5F61E9
JJValue10 = EA8100590D00BA
JJValue11 = 05EAC100590C00
JJValue12 = 834A00590D00B8
JJValue13 = 000024820F01FA
JJValue14 = 664B8B20E88300
JJValue15 = F929627B8BF08B
JJValue16 = 0FA766F3FCE9D1
JJValue17 = 8B660000000B84
JJValue18 = 0020FF8166FE7F
JJValue19 = 48820F5E5AD275
JJValue20 = 8B28EE83000000
JJValue21 = 2EE8236A605E5E
JJValue22 = FCF9E859FFF68B
JJValue23 = 8810244C8AFFF3
JJValue24 = 8966104E8B1948
JJValue25 = 8B14244C8A1C48
JJValue26 = 881A5889660C5E
JJValue27 = 661F7089661E48
JJValue28 = 8104488B217889
JJValue29 = 0006C5B9390C6C
JJValue30 = 5F6161402454FF
JJValue31 = 5EC3E9E58B5B5E
JJValue32 = FFED
Addrs := 0x00590B00
i1 := SubStr(JJValue, 1, 14)
i2 := Substr(JJValue2, 1, 14)
i3 := Substr(JJValue3, 1, 14)
i4 := Substr(JJValue4, 1, 14)
i5 := Substr(JJValue5, 1, 14)
i6 := Substr(JJValue6, 1, 14)
i7 := Substr(JJValue7, 1, 14)
i8 := Substr(JJValue8, 1, 14)
i9 := Substr(JJValue9, 1, 14)
i10 := Substr(JJValue10, 1, 14)
i11 := Substr(JJValue11, 1, 14)
i12 := Substr(JJValue12, 1, 14)
i13 := Substr(JJValue13, 1, 14)
i14 := Substr(JJValue14, 1, 14)
i15 := Substr(JJValue15, 1, 14)
i16 := Substr(JJValue16, 1, 14)
i17 := Substr(JJValue17, 1, 14)
i18 := Substr(JJValue18, 1, 14)
i19 := Substr(JJValue19, 1, 14)
i20 := Substr(JJValue20, 1, 14)
i21 := Substr(JJValue21, 1, 14)
i22 := Substr(JJValue22, 1, 14)
i23 := Substr(JJValue23, 1, 14)
i24 := Substr(JJValue24, 1, 14)
i25 := Substr(JJValue25, 1, 14)
i26 := Substr(JJValue26, 1, 14)
i27 := Substr(JJValue27, 1, 14)
i28 := Substr(JJValue28, 1, 14)
i29 := Substr(JJValue29, 1, 14)
i30 := Substr(JJValue30, 1, 14)
i31 := Substr(JJValue31, 1, 14)
i32 := Substr(JJValue32, 1, 14)
writememory2("0x"i1, Addrs, PID)
writememory2("0x"i2, Addrs+7, PID)
writememory2("0x"i3, Addrs+14, PID)
writememory2("0x"i4, Addrs+21, PID)
writememory2("0x"i5, Addrs+28, PID)
writememory2("0x"i6, Addrs+35, PID)
writememory2("0x"i7, Addrs+42, PID)
writememory2("0x"i8, Addrs+49, PID)
writememory2("0x"i9, Addrs+56, PID)
writememory2("0x"i10, Addrs+63, PID)
writememory2("0x"i11, Addrs+70, PID)
writememory2("0x"i12, Addrs+77, PID)
writememory2("0x"i13, Addrs+84, PID)
writememory2("0x"i14, Addrs+91, PID)
writememory2("0x"i15, Addrs+98, PID)
writememory2("0x"i16, Addrs+105, PID)
writememory2("0x"i17, Addrs+112, PID)
writememory2("0x"i18, Addrs+119, PID)
writememory2("0x"i19, Addrs+126, PID)
writememory2("0x"i20, Addrs+133, PID)
writememory2("0x"i21, Addrs+140, PID)
writememory2("0x"i22, Addrs+147, PID)
writememory2("0x"i23, Addrs+154, PID)
writememory2("0x"i24, Addrs+161, PID)
writememory2("0x"i25, Addrs+168, PID)
writememory2("0x"i26, Addrs+175, PID)
writememory2("0x"i27, Addrs+182, PID)
writememory2("0x"i28, Addrs+189, PID)
writememory2("0x"i29, Addrs+196, PID)
writememory2("0x"i30, Addrs+203, PID)
writememory2("0x"i31, Addrs+210, PID)
writememory2("0x"i32, Addrs+217, PID)
JJValue33 = 69C758BA85C0DD
JJValue34 = CF
Addrs2 := 0x00590C20
i33 := Substr(JJValue33, 1, 14)
i34 := Substr(JJValue34, 1, 14)
writememory2("0x"i33, Addrs2, PID)
writememory2("0x"i34, Addrs2+7, PID)
JJValue35 = 00B294B098BE5B
JJValue36 = B8E8AC
Addrs3 := 0x00590C40
i35 := Substr(JJValue35, 1, 14)
i36 := Substr(JJValue36, 1, 14)
writememory2("0x"i35, Addrs3, PID)
writememory2("0x"i36, Addrs3+7, PID)
JJValue37 = C25D0012A062E9
Addrs4 := 0x00466A99
i37 := Substr(JJValue37, 1, 14)
writememory2("0x"i37, Addrs4, PID)
}
incineratescript()
{
SetFormat, Integer, HEX
SetTitleMatchMode, 3
WinGet, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
inci = 8B0058DAD4A160
inci2 = 808B0000017880
inci3 = 08408B000000BE
inci4 = 64C283D231188D
inci5 = C3834974D2854A
inci6 = 8BF374003B8304
inci7 = 8B04588B038BCB
inci8 = F63108408B0840
inci9 = 0166388B02C083
inci10 = 8DF375FF8566FE
inci11 = E6C1005909C005
inci12 = 66388B02C08310
inci13 = F375FF8566FE01
inci14 = 8B10EEC1F78966
inci15 = EB0474FE3966D9
inci16 = 5B8B1B8BC361B2
inci17 = F6914CE81E6A04
inci18 = FFF415B7E859FF
inci19 = 641A40C7195888
inci20 = F4215EE8000000
inci21 = DBEBFF
Addrs := 0x00590500
I1 := SubStr(inci, 1, 14)
I2 := Substr(inci2, 1, 14)
I3 := Substr(inci3, 1, 14)
I4 := Substr(inci4, 1, 14)
I5 := Substr(inci5, 1, 14)
I6 := Substr(inci6, 1, 14)
I7 := Substr(inci7, 1, 14)
I8 := Substr(inci8, 1, 14)
I9 := Substr(inci9, 1, 14)
I10 := Substr(inci10, 1, 14)
I11 := Substr(inci11, 1, 14)
I12 := Substr(inci12, 1, 14)
I13 := Substr(inci13, 1, 14)
I14 := Substr(inci14, 1, 14)
I15 := Substr(inci15, 1, 14)
I16 := Substr(inci16, 1, 14)
I17 := Substr(inci17, 1, 14)
I18 := Substr(inci18, 1, 14)
I19 := Substr(inci19, 1, 14)
I20 := Substr(inci20, 1, 14)
I21 := Substr(inci21, 1, 14)
writememory2("0x"I1, Addrs, PID)
writememory2("0x"I2, Addrs+7, PID)
writememory2("0x"I3, Addrs+14, PID)
writememory2("0x"I4, Addrs+21, PID)
writememory2("0x"I5, Addrs+28, PID)
writememory2("0x"I6, Addrs+35, PID)
writememory2("0x"I7, Addrs+42, PID)
writememory2("0x"I8, Addrs+49, PID)
writememory2("0x"I9, Addrs+56, PID)
writememory2("0x"I10, Addrs+63, PID)
writememory2("0x"I11, Addrs+70, PID)
writememory2("0x"I12, Addrs+77, PID)
writememory2("0x"I13, Addrs+84, PID)
writememory2("0x"I14, Addrs+91, PID)
writememory2("0x"I15, Addrs+98, PID)
writememory2("0x"I16, Addrs+105, PID)
writememory2("0x"I17, Addrs+112, PID)
writememory2("0x"I18, Addrs+119, PID)
writememory2("0x"I19, Addrs+126, PID)
writememory2("0x"I20, Addrs+133, PID)
writememory2("0x"I21, Addrs+140, PID)
}
CAGI()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058EDD0, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
incinerate()
{
SetTitleMatchMode, 3
WinGet, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x00590500, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
inviteparty()
{
SetTitleMatchMode, 3
WinGet, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058FE00, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
Poidget(TagetProc)
{
SetTitleMatchMode, 3
CharOID := 0
Get_CharOID := 0
WinGet, pid, PID, %TagetProc%
ProcHwnd := DllCall("OpenProcess", "Int", 24, "Char", 0, "UInt", PID, "UInt")
DllCall("ReadProcessMemory","UInt",ProcHwnd,"UInt",0x0058DAD4,"Str",CharOID,"UInt",4,"UInt *",0)
Loop 4
result += *(&CharOID + A_Index-1) << 8*(A_Index-1)
result := result+98
DllCall("ReadProcessMemory","UInt",ProcHwnd,"UInt",result,"Str",CharOID,"UInt",4,"UInt *",0)
Loop 4
Get_CharOID += *(&CharOID + A_Index-1) << 8*(A_Index-1)
DllCall("CloseHandle", "int", ProcHwnd)
Return, Get_CharOID
}
Poidwrite()
{
SetTitleMatchMode, 3
WinGet, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ChangeAddr := 0x0058FE20
DllCall("WriteProcessMemory", "UInt", ProcHwnd, "UInt", ChangeAddr, "UInt*", ChangeValue, "UInt", 07, "Uint *", 0)
DllCall("CloseHandle", "int", ProcHwnd)
return
}
RPscript()
{
SetFormat, Integer, HEX
SetTitleMatchMode, 3
WinGet, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ChangeValue = FFF698BDE81F6AC6FFF419D0E859FE200D8B011940CCE81A48890058C3FFF428
Addrs := 0x0058FE00
Loop
{
A := SubStr(ChangeValue, 1, 14)
C := Substr(ChangeValue, 15)
ChangeValue := C
writememory2("0x"A, Addrs, PID)
Addrs := Addrs+7
if(Instr(A,"C3"))
{
break
}
}
}
party()
{
Gui, Submit, Nohide
CharID_1 := 0
CharID_2 := 0
CharID_3 := 0
CharID_4 := 0
CharID_5 := 0
CharID_6 := 0
loop, 1
{
Gui, Submit, Nohide
GuiControlGet, CheckName, , Name1
if (CheckName != "파티원")
{
CharID_1 := Poidget(CheckName)
}
GuiControlGet, CheckName, , Name2
if (CheckName != "파티원")
{
CharID_2 := Poidget(CheckName)
}
GuiControlGet, CheckName, , Name3
if (CheckName != "파티원")
{
CharID_3 := Poidget(CheckName)
}
GuiControlGet, CheckName, , Name4
if (CheckName != "파티원")
{
CharID_4 := Poidget(CheckName)
}
GuiControlGet, CheckName, , Name5
if (CheckName != "파티원")
{
CharID_5 := Poidget(CheckName)
}
GuiControlGet, CheckName, , Name6
if (CheckName != "파티원")
{
CharID_6 := Poidget(CheckName)
}
Gui, Submit, Nohide
GuiControlGet, CheckName, , MainName
RPscript()
ChangeValue := CharID_1
Poidwrite()
inviteparty()
Sleep, 200
ChangeValue := CharID_2
Poidwrite()
inviteparty()
Sleep, 200
ChangeValue := CharID_3
Poidwrite()
inviteparty()
Sleep, 200
ChangeValue := CharID_4
Poidwrite()
inviteparty()
Sleep, 200
ChangeValue := CharID_5
Poidwrite()
inviteparty()
Sleep, 200
ChangeValue := CharID_6
Poidwrite()
inviteparty()
Sleep, 200
}
}
ATKM()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATKValue = 0D5ED900046683
ATKValue2 = 00003E000F46C7
ATKValue3 = FFED0907E9
Addrs := 0x0058FF00
U1 := SubStr(ATKValue, 1, 14)
U2 := Substr(ATKValue2, 1, 14)
U3 := Substr(ATKValue3, 1, 14)
writememory2("0x"U1, Addrs, PID)
writememory2("0x"U2, Addrs+7, PID)
writememory2("0x"U3, Addrs+14, PID)
}
ActiveAscript1()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATAValue = DF95E8016A006A
ATAValue2 = C35959FFF2
Addrs := 0x0058D600
AAA1 := SubStr(ATAValue, 1, 14)
AAA2 := Substr(ATAValue2, 1, 14)
writememory2("0x"AAA1, Addrs, PID)
writememory2("0x"AAA2, Addrs+7, PID)
}
ActiveAscript2()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATAValue = DF85E8016A006A
ATAValue2 = C35959FFF2
Addrs := 0x0058D610
AAA1 := SubStr(ATAValue, 1, 14)
AAA2 := Substr(ATAValue2, 1, 14)
writememory2("0x"AAA1, Addrs, PID)
writememory2("0x"AAA2, Addrs+7, PID)
}
ActiveAscript3()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATAValue = DF75E8016A006A
ATAValue2 = C35959FFF2
Addrs := 0x0058D620
AAA1 := SubStr(ATAValue, 1, 14)
AAA2 := Substr(ATAValue2, 1, 14)
writememory2("0x"AAA1, Addrs, PID)
writememory2("0x"AAA2, Addrs+7, PID)
}
ActiveAscript4()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATAValue = DF65E8016A006A
ATAValue2 = C35959FFF2
Addrs := 0x0058D630
AAA1 := SubStr(ATAValue, 1, 14)
AAA2 := Substr(ATAValue2, 1, 14)
writememory2("0x"AAA1, Addrs, PID)
writememory2("0x"AAA2, Addrs+7, PID)
}
ActiveAscript5()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATAValue = DF55E8016A006A
ATAValue2 = C35959FFF2
Addrs := 0x0058D640
AAA1 := SubStr(ATAValue, 1, 14)
AAA2 := Substr(ATAValue2, 1, 14)
writememory2("0x"AAA1, Addrs, PID)
writememory2("0x"AAA2, Addrs+7, PID)
}
ActiveAscript6()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATAValue = DF45E8016A006A
ATAValue2 = C35959FFF2
Addrs := 0x0058D650
AAA1 := SubStr(ATAValue, 1, 14)
AAA2 := Substr(ATAValue2, 1, 14)
writememory2("0x"AAA1, Addrs, PID)
writememory2("0x"AAA2, Addrs+7, PID)
}
ActiveAscript7()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATAValue = DF35E8016A006A
ATAValue2 = C35959FFF2
Addrs := 0x0058D660
AAA1 := SubStr(ATAValue, 1, 14)
AAA2 := Substr(ATAValue2, 1, 14)
writememory2("0x"AAA1, Addrs, PID)
writememory2("0x"AAA2, Addrs+7, PID)
}
ActiveAscript8()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATAValue = DF25E8016A006A
ATAValue2 = C35959FFF2
Addrs := 0x0058D670
AAA1 := SubStr(ATAValue, 1, 14)
AAA2 := Substr(ATAValue2, 1, 14)
writememory2("0x"AAA1, Addrs, PID)
writememory2("0x"AAA2, Addrs+7, PID)
}
ActiveAscript9()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATAValue = DF15E8016A006A
ATAValue2 = C35959FFF2
Addrs := 0x0058D680
AAA1 := SubStr(ATAValue, 1, 14)
AAA2 := Substr(ATAValue2, 1, 14)
writememory2("0x"AAA1, Addrs, PID)
writememory2("0x"AAA2, Addrs+7, PID)
}
ActiveAscript10()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATAValue = DF05E8016A006A
ATAValue2 = C35959FFF2
Addrs := 0x0058D690
AAA1 := SubStr(ATAValue, 1, 14)
AAA2 := Substr(ATAValue2, 1, 14)
writememory2("0x"AAA1, Addrs, PID)
writememory2("0x"AAA2, Addrs+7, PID)
}
ActiveAscript11()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATAValue = DEF5E8016A006A
ATAValue2 = C35959FFF2
Addrs := 0x0058D6A0
AAA1 := SubStr(ATAValue, 1, 14)
AAA2 := Substr(ATAValue2, 1, 14)
writememory2("0x"AAA1, Addrs, PID)
writememory2("0x"AAA2, Addrs+7, PID)
}
ActiveAscript12()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
ATAValue = DEE5E8016A006A
ATAValue2 = C35959FFF2
Addrs := 0x0058D6B0
AAA1 := SubStr(ATAValue, 1, 14)
AAA2 := Substr(ATAValue2, 1, 14)
writememory2("0x"AAA1, Addrs, PID)
writememory2("0x"AAA2, Addrs+7, PID)
}
AA1()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D600, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
AA2()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D610, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
AA3()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D620, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
AA4()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D630, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
AA5()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D640, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
AA6()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D650, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
AA7()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D660, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
AA8()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D670, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
AA9()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D680, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
AA10()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D690, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
AA11()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D6A0, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
AA12()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x0058D6B0, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
MIC()
{
SetFormat, Integer, HEX
SetTitleMatchMode, 3
WinGet, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
MICValue = 2574000004623D
MICValue2 = 0004643D001F0F
MICValue3 = 3D001F0F2E7400
MICValue4 = 0F377400000465
MICValue5 = 500C4EB60F001F
MICValue6 = BF03E93C2474FF
MICValue7 = B80C4EB60FFFEE
MICValue8 = 74FF5000000469
MICValue9 = FFEEBEF0E93C24
MICValue10 = 0469B80C4EB60F
MICValue11 = 3C2474FF500000
MICValue12 = B60FFFEEBEDDE9
MICValue13 = 00000469B80C4E
MICValue14 = CAE93C2474FF50
MICValue15 = FFEEBE
Addrs := 0x0058F400
MC1 := SubStr(MICValue, 1, 14)
MC2 := Substr(MICValue2, 1, 14)
MC3 := Substr(MICValue3, 1, 14)
MC4 := Substr(MICValue4, 1, 14)
MC5 := Substr(MICValue5, 1, 14)
MC6 := Substr(MICValue6, 1, 14)
MC7 := Substr(MICValue7, 1, 14)
MC8 := Substr(MICValue8, 1, 14)
MC9 := Substr(MICValue9, 1, 14)
MC10 := Substr(MICValue10, 1, 14)
MC11 := Substr(MICValue11, 1, 14)
MC12 := Substr(MICValue12, 1, 14)
MC13 := Substr(MICValue13, 1, 14)
MC14 := Substr(MICValue14, 1, 14)
MC15 := Substr(MICValue15, 1, 14)
writememory2("0x"MC1, Addrs, PID)
writememory2("0x"MC2, Addrs+7, PID)
writememory2("0x"MC3, Addrs+14, PID)
writememory2("0x"MC4, Addrs+21, PID)
writememory2("0x"MC5, Addrs+28, PID)
writememory2("0x"MC6, Addrs+35, PID)
writememory2("0x"MC7, Addrs+42, PID)
writememory2("0x"MC8, Addrs+49, PID)
writememory2("0x"MC9, Addrs+56, PID)
writememory2("0x"MC10, Addrs+63, PID)
writememory2("0x"MC11, Addrs+70, PID)
writememory2("0x"MC12, Addrs+77, PID)
writememory2("0x"MC13, Addrs+84, PID)
writememory2("0x"MC14, Addrs+91, PID)
writememory2("0x"MC15, Addrs+98, PID)
}
CNN()
{
SETFORMAT, Integer, HEX
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", PID, "UInt")
cnValue = 00004300547E10
cnValue2 = 547E1C00
cnValue3 = 0101
U1 := SubStr(cnValue, 1, 14)
U2 := Substr(cnValue2, 1, 14)
U3 := Substr(cnValue3, 1, 14)
Addrs := 0x00527ACC
Addrs2 := 0x00527AE4
writememory2("0x"U1, Addrs, PID)
writememory2("0x"U2, Addrs+7, PID)
writememory2("0x"U3, Addrs2, PID)
}
CN()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", 0x00527B4C, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
}
GetPrivateWorkingSet(PID)
{
bytes := ComObjGet("winmgmts:") .ExecQuery("Select * from Win32_PerfFormattedData_PerfProc_Process Where IDProcess=" PID) .ItemIndex(0).WorkingSetPrivate
byte := bytes/1024
Return
}
FormatNumber(Amount) {
StringReplace Amount, Amount, -
IfEqual ErrorLevel,0, SetEnv Sign,-
Loop Parse, Amount, .
If (A_Index = 1)
{
len := StrLen(A_LoopField)
Loop Parse, A_LoopField
If (Mod(len-A_Index,3) = 0 and A_Index != len)?
x = %x%%A_LoopField%,
Else x = %x%%A_LoopField%
}
Else Return Sign x "." A_LoopField
Return Sign x
}
TrackWeaponChange(newWeapon)
{
global RecentWeapons
for index, weapon in RecentWeapons
{
if (weapon == newWeapon)
{
return
}
}
RecentWeapons.InsertAt(1, newWeapon)
if (RecentWeapons.MaxIndex() > 3)
{
RecentWeapons.RemoveAt(4)
}
}
CheckTrackedWeapons() {
global RecentWeapons
return RecentWeapons.MaxIndex()
}
종료:
Gui, Submit, NoHide
CRITICAL,On
Gosub, GuiClose
CRITICAL,OFF
Return
포프OID()
{
Gui, Submit, Nohide
SB_SetText("OID 확인 중")
업데이트체크 := 1
Sleep,1500
OID읽기()
Check_좌표()
GuiControl, , NPC_version,%NPC_수정version%
}

Check_좌표()
{
Gui, Submit, Nohide
FormatTime,점검,A_Now,HHmm
if(A_WDay = 5 && 점검 >= 0920 && 점검 <= 2359)
{
if(업데이트체크 = 1)
{
Entrance = 0
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 업데이트체크, %업데이트체크%
if( 파라스방해감지 = 0 )
{
GuiControl, ,Gui_HuntAuto, 1
MsgBox, , OID업데이트, OID 완료 [ 사냥터 자동변경 ], 1
}
else
{
MsgBox, , OID업데이트, OID 완료 [ 파라스팅으로 사냥터 변경X ], 1
}
SB_SetText("업데이트 완료")
return
}
if(업데이트체크 = 0)
{
Entrance = 0
GuiControl, ,Gui_HuntPobuk, 1
MsgBox, , OID업데이트, OID 실패 [ 포남 > 포북으로 자동변경 ], 1
SB_SetText("업데이트 실패")
return
}
}
if(A_WDay != 5)
{
업데이트체크 = 0
}
}
아이템읽어오기()
{
Gui, Submit, Nohide
Get_Gold()
SETFORMAT, integer, D
invenslot := 0
아이템갯수 := {}
loop, 50
{
invenslot += 4
invenitem := jelan.readString(0x0058DAD4, 50, "UTF-16", 0x178, 0xBE, 0x8, invenslot, 0x8, 0x8, 0x0)
ItemCount := jelan.read(0x0058DAD4, "Uint", 0x178, 0xBE, 0x8, invenslot, 0x8, 0x20)
if (invenitem = "")
continue
아이템갯수[invenitem] := (아이템갯수[invenitem] ? 아이템갯수[invenitem] + ItemCount : ItemCount)
}
라깃카운트 := 아이템갯수["라스의깃"]
정보카운트 := 아이템갯수["정령의보석"]
골드바카운트 := 아이템갯수["골드바"]
if(아이템갯수["골드바"] = "")
{
GuiControl,, Gui_NowGoldbar, 0
}
else
{
골드바갯수 := 아이템갯수["골드바"]
GuiControl,, Gui_NowGoldbar, %골드바갯수%
}
if(아이템갯수["라스의깃"] = "")
{
GuiControl,, Gui_RasCount, 0
}
else
{
라스의깃갯수 := 아이템갯수["라스의깃"]
GuiControl,, Gui_RasCount, %라스의깃갯수%
}
if(아이템갯수["정령의보석"] = "")
{
GuiControl,, Gui_정보Count, 0
}
else
{
정령의보석갯수 := 아이템갯수["정령의보석"]
GuiControl,, Gui_정보Count, %정령의보석갯수%
}
SB_SetText("갈리드 / 라깃 / 정보 수량 확인 완료")
}
정수체크()
{
Gui, Submit, Nohide
itemnum := 0
itemnum1 := 0
itemnum2 := 0
invenslot := 0
Loop,50
{
SetFormat, integer, H
invenslot += 4
itemm := jelan.readString(0x0058DAD4, 50, "UTF-16", 0x178, 0xBE, 0x8, invenslot, 0x8, 0x8, 0x0)
SetFormat, integer, D
IfInString,itemm,빛나는결정
{
itemnum += 1
}
IfInString,itemm,빛나는나뭇가지
{
itemnum1 += 1
}
IfInString,itemm,빛나는가루
{
itemnum2 += 1
}
}
결정갯수 := itemnum
나무갯수 := itemnum1
가루갯수 := itemnum2
return
}
^q::
if( 좌표고정 != 0)
{
CoordMode,mouse,Screen
MouseGetPos,게임시작x,게임시작y
GuiControl, , 좌표x, %게임시작x%
GuiControl, , 좌표y, %게임시작y%
return
}
return
어빌리티탭확인:
{
Gui, Submit, Nohide
Check_SAbilityN()
Check_SAbility()
if(Slot1AN != "")
{
GuiControl, , Gui_WeaponName1, %Slot1AN%
if(Gui_WeaponCheck1 = 1)
{
WeaponAbility1 := Slot1Ability
}
if(Gui_WeaponCheck1 = 0)
{
WeaponAbility1 =
}
}
if(Slot2AN != "")
{
GuiControl, , Gui_WeaponName2, %Slot2AN%
if(Gui_WeaponCheck2 = 1)
{
WeaponAbility2 := Slot2Ability
}
if(Gui_WeaponCheck2 = 0)
{
WeaponAbility2 =
}
}
if(Slot3AN != "")
{
GuiControl, , Gui_WeaponName3, %Slot3AN%
if(Gui_WeaponCheck3 = 1)
{
WeaponAbility3 := Slot3Ability
}
if(Gui_WeaponCheck3 = 0)
{
WeaponAbility3 =
}
}
if(Slot4AN != "")
{
GuiControl, , Gui_WeaponName4, %Slot4AN%
if(Gui_WeaponCheck4 = 1)
{
WeaponAbility4 := Slot4Ability
}
if(Gui_WeaponCheck4 = 0)
{
WeaponAbility4 =
}
}
if(Slot5AN != "")
{
GuiControl, , Gui_WeaponName5, %Slot5AN%
if(Gui_WeaponCheck5 = 1)
{
WeaponAbility5 := Slot5Ability
}
if(Gui_WeaponCheck5 = 0)
{
WeaponAbility5 =
}
}
if(Slot6AN != "")
{
GuiControl, , Gui_WeaponName6, %Slot6AN%
if(Gui_WeaponCheck6 = 1)
{
WeaponAbility6 := Slot6Ability
}
if(Gui_WeaponCheck6 = 0)
{
WeaponAbility6 =
}
}
if(Slot7AN != "")
{
GuiControl, , Gui_WeaponName7, %Slot7AN%
if(Gui_WeaponCheck7 = 1)
{
WeaponAbility7 := Slot7Ability
}
if(Gui_WeaponCheck7 = 0)
{
WeaponAbility7 =
}
}
if(Slot8AN != "")
{
GuiControl, , Gui_WeaponName8, %Slot8AN%
if(Gui_WeaponCheck8 = 1)
{
WeaponAbility8 := Slot8Ability
}
if(Gui_WeaponCheck8 = 0)
{
WeaponAbility8 =
}
}
if(Slot9AN != "")
{
GuiControl, , Gui_WeaponName9, %Slot9AN%
if(Gui_WeaponCheck9 = 1)
{
WeaponAbility9 := Slot9Ability
}
if(Gui_WeaponCheck9 = 0)
{
WeaponAbility9 =
}
}
if(Slot10AN != "")
{
GuiControl, , Gui_WeaponName10, %Slot10AN%
if(Gui_WeaponCheck10 = 1)
{
WeaponAbility10 := Slot10Ability
}
if(Gui_WeaponCheck10 = 0)
{
WeaponAbility10 =
}
}
if(Slot11AN != "")
{
GuiControl, , Gui_WeaponName11, %Slot11AN%
if(Gui_WeaponCheck11 = 1)
{
WeaponAbility11 := Slot11Ability
}
if(Gui_WeaponCheck11 = 0)
{
WeaponAbility11 =
}
}
if(Slot12AN != "")
{
GuiControl, , Gui_WeaponName12, %Slot12AN%
if(Gui_WeaponCheck12 = 1)
{
WeaponAbility12 := Slot12Ability
}
if(Gui_WeaponCheck12 = 0)
{
WeaponAbility12 =
}
}
if(Slot13AN != "")
{
GuiControl, , Gui_WeaponName13, %Slot13AN%
if(Gui_WeaponCheck13 = 1)
{
WeaponAbility13 := Slot13Ability
}
if(Gui_WeaponCheck13 = 0)
{
WeaponAbility13 =
}
}
if(Slot14AN != "")
{
GuiControl, , Gui_WeaponName14, %Slot14AN%
if(Gui_WeaponCheck14 = 1)
{
WeaponAbility14 := Slot14Ability
}
if(Gui_WeaponCheck14 = 0)
{
WeaponAbility14 =
}
}
if(Slot15AN != "")
{
GuiControl, , Gui_WeaponName15, %Slot15AN%
if(Gui_WeaponCheck15 = 1)
{
WeaponAbility15 := Slot15Ability
}
if(Gui_WeaponCheck15 = 0)
{
WeaponAbility15 =
}
}
if(Slot16AN != "")
{
GuiControl, , Gui_WeaponName16, %Slot16AN%
if(Gui_WeaponCheck16 = 1)
{
WeaponAbility16 := Slot16Ability
}
if(Gui_WeaponCheck16 = 0)
{
WeaponAbility16 =
}
}
if(Slot17AN != "")
{
GuiControl, , Gui_WeaponName17, %Slot17AN%
if(Gui_WeaponCheck17 = 1)
{
WeaponAbility17 := Slot17Ability
}
if(Gui_WeaponCheck17 = 0)
{
WeaponAbility17 =
}
}
if(Slot18AN != "")
{
GuiControl, , Gui_WeaponName18, %Slot18AN%
if(Gui_WeaponCheck18 = 1)
{
WeaponAbility18 := Slot18Ability
}
if(Gui_WeaponCheck18 = 0)
{
WeaponAbility18 =
}
}
if(Slot19AN != "")
{
GuiControl, , Gui_WeaponName19, %Slot19AN%
if(Gui_WeaponCheck19 = 1)
{
WeaponAbility19 := Slot19Ability
}
if(Gui_WeaponCheck19 = 0)
{
WeaponAbility19 =
}
}
if(Slot20AN != "")
{
GuiControl, , Gui_WeaponName20, %Slot20AN%
if(Gui_WeaponCheck20 = 1)
{
WeaponAbility20 := Slot20Ability
}
if(Gui_WeaponCheck20 = 0)
{
WeaponAbility20 =
}
}
if(Slot21AN != "")
{
GuiControl, , Gui_WeaponName21, %Slot21AN%
if(Gui_WeaponCheck21 = 1)
{
WeaponAbility21 := Slot21Ability
}
if(Gui_WeaponCheck21 = 0)
{
WeaponAbility21 =
}
}
if(Slot22AN != "")
{
GuiControl, , Gui_WeaponName22, %Slot22AN%
if(Gui_WeaponCheck22 = 1)
{
WeaponAbility22 := Slot22Ability
}
if(Gui_WeaponCheck22 = 0)
{
WeaponAbility22 =
}
}
if(Slot23AN != "")
{
GuiControl, , Gui_WeaponName23, %Slot23AN%
if(Gui_WeaponCheck23 = 1)
{
WeaponAbility23 := Slot23Ability
}
if(Gui_WeaponCheck23 = 0)
{
WeaponAbility23 =
}
}
if(Slot24AN != "")
{
GuiControl, , Gui_WeaponName24, %Slot24AN%
if(Gui_WeaponCheck24 = 1)
{
WeaponAbility24 := Slot24Ability
}
if(Gui_WeaponCheck24 = 0)
{
WeaponAbility24 =
}
}
if(Slot25AN != "")
{
GuiControl, , Gui_WeaponName25, %Slot25AN%
if(Gui_WeaponCheck25 = 1)
{
WeaponAbility25 := Slot25Ability
}
if(Gui_WeaponCheck25 = 0)
{
WeaponAbility25 =
}
}
if(Slot26AN != "")
{
GuiControl, , Gui_WeaponName26, %Slot26AN%
if(Gui_WeaponCheck26 = 1)
{
WeaponAbility26 := Slot26Ability
}
if(Gui_WeaponCheck26 = 0)
{
WeaponAbility26 =
}
}
if(Slot27AN != "")
{
GuiControl, , Gui_WeaponName27, %Slot27AN%
if(Gui_WeaponCheck27 = 1)
{
WeaponAbility27 := Slot27Ability
}
if(Gui_WeaponCheck27 = 0)
{
WeaponAbility27 =
}
}
if(Slot28AN != "")
{
GuiControl, , Gui_WeaponName28, %Slot28AN%
if(Gui_WeaponCheck28 = 1)
{
WeaponAbility28 := Slot28Ability
}
if(Gui_WeaponCheck28 = 0)
{
WeaponAbility28 =
}
}
if(Slot29AN != "")
{
GuiControl, , Gui_WeaponName29, %Slot29AN%
if(Gui_WeaponCheck29 = 1)
{
WeaponAbility29 := Slot29Ability
}
if(Gui_WeaponCheck29 = 0)
{
WeaponAbility29 =
}
}
if(Slot30AN != "")
{
GuiControl, , Gui_WeaponName30, %Slot30AN%
if(Gui_WeaponCheck30 = 1)
{
WeaponAbility30 := Slot30Ability
}
if(Gui_WeaponCheck30 = 0)
{
WeaponAbility30 =
}
}
if(Slot31AN != "")
{
GuiControl, , Gui_WeaponName31, %Slot31AN%
if(Gui_WeaponCheck31 = 1)
{
WeaponAbility31 := Slot31Ability
}
if(Gui_WeaponCheck31 = 0)
{
WeaponAbility31 =
}
}
if(Slot32AN != "")
{
GuiControl, , Gui_WeaponName32, %Slot32AN%
if(Gui_WeaponCheck32 = 1)
{
WeaponAbility32 := Slot32Ability
}
if(Gui_WeaponCheck32 = 0)
{
WeaponAbility32 =
}
}
if(Slot33AN != "")
{
GuiControl, , Gui_WeaponName33, %Slot33AN%
if(Gui_WeaponCheck33 = 1)
{
WeaponAbility33 := Slot33Ability
}
if(Gui_WeaponCheck33 = 0)
{
WeaponAbility33 =
}
}
if(Slot34AN != "")
{
GuiControl, , Gui_WeaponName34, %Slot34AN%
if(Gui_WeaponCheck34 = 1)
{
WeaponAbility34 := Slot34Ability
}
if(Gui_WeaponCheck34 = 0)
{
WeaponAbility34 =
}
}
if(Slot35AN != "")
{
GuiControl, , Gui_WeaponName35, %Slot35AN%
if(Gui_WeaponCheck35 = 1)
{
WeaponAbility35 := Slot35Ability
}
if(Gui_WeaponCheck35 = 0)
{
WeaponAbility35 =
}
}
if(Slot36AN != "")
{
GuiControl, , Gui_WeaponName36, %Slot36AN%
if(Gui_WeaponCheck36 = 1)
{
WeaponAbility36 := Slot36Ability
}
if(Gui_WeaponCheck36 = 0)
{
WeaponAbility36 =
}
}
if(Slot37AN != "")
{
GuiControl, , Gui_WeaponName37, %Slot37AN%
if(Gui_WeaponCheck37 = 1)
{
WeaponAbility37 := Slot37Ability
}
if(Gui_WeaponCheck37 = 0)
{
WeaponAbility37 =
}
}
if(Slot38AN != "")
{
GuiControl, , Gui_WeaponName38, %Slot38AN%
if(Gui_WeaponCheck38 = 1)
{
WeaponAbility38 := Slot38Ability
}
if(Gui_WeaponCheck38 = 0)
{
WeaponAbility38 =
}
}
if(Slot39AN != "")
{
GuiControl, , Gui_WeaponName39, %Slot39AN%
if(Gui_WeaponCheck39 = 1)
{
WeaponAbility39 := Slot39Ability
}
if(Gui_WeaponCheck39 = 0)
{
WeaponAbility39 =
}
}
if(Slot40AN != "")
{
GuiControl, , Gui_WeaponName40, %Slot40AN%
if(Gui_WeaponCheck40 = 1)
{
WeaponAbility40 := Slot40Ability
}
if(Gui_WeaponCheck40 = 0)
{
WeaponAbility40 =
}
}
if(Slot41AN != "")
{
GuiControl, , Gui_WeaponName41, %Slot41AN%
if(Gui_WeaponCheck41 = 1)
{
WeaponAbility41 := Slot41Ability
}
if(Gui_WeaponCheck41 = 0)
{
WeaponAbility41 =
}
}
if(Slot42AN != "")
{
GuiControl, , Gui_WeaponName42, %Slot42AN%
if(Gui_WeaponCheck42 = 1)
{
WeaponAbility42 := Slot42Ability
}
if(Gui_WeaponCheck42 = 0)
{
WeaponAbility42 =
}
}
if(Slot43AN != "")
{
GuiControl, , Gui_WeaponName43, %Slot43AN%
if(Gui_WeaponCheck43 = 1)
{
WeaponAbility43 := Slot43Ability
}
if(Gui_WeaponCheck43 = 0)
{
WeaponAbility43 =
}
}
if(Slot44AN != "")
{
GuiControl, , Gui_WeaponName44, %Slot44AN%
if(Gui_WeaponCheck44 = 1)
{
WeaponAbility44 := Slot44Ability
}
if(Gui_WeaponCheck44 = 0)
{
WeaponAbility44 =
}
}
if(Slot45AN != "")
{
GuiControl, , Gui_WeaponName45, %Slot45AN%
if(Gui_WeaponCheck45 = 1)
{
WeaponAbility45 := Slot45Ability
}
if(Gui_WeaponCheck45 = 0)
{
WeaponAbility45 =
}
}
if(Slot46AN != "")
{
GuiControl, , Gui_WeaponName46, %Slot46AN%
if(Gui_WeaponCheck46 = 1)
{
WeaponAbility46 := Slot46Ability
}
if(Gui_WeaponCheck46 = 0)
{
WeaponAbility46 =
}
}
if(Slot47AN != "")
{
GuiControl, , Gui_WeaponName47, %Slot47AN%
if(Gui_WeaponCheck47 = 1)
{
WeaponAbility47 := Slot47Ability
}
if(Gui_WeaponCheck47 = 0)
{
WeaponAbility47 =
}
}
if(Slot48AN != "")
{
GuiControl, , Gui_WeaponName48, %Slot48AN%
if(Gui_WeaponCheck48 = 1)
{
WeaponAbility48 := Slot48Ability
}
if(Gui_WeaponCheck48 = 0)
{
WeaponAbility48 =
}
}
if(Slot49AN != "")
{
GuiControl, , Gui_WeaponName49, %Slot49AN%
if(Gui_WeaponCheck49 = 1)
{
WeaponAbility49 := Slot49Ability
}
if(Gui_WeaponCheck49 = 0)
{
WeaponAbility49 =
}
}
if(Slot50AN != "")
{
GuiControl, , Gui_WeaponName50, %Slot50AN%
if(Gui_WeaponCheck50 = 1)
{
WeaponAbility50 := Slot50Ability
}
if(Gui_WeaponCheck50 = 0)
{
WeaponAbility50 =
}
}
if(Slot51AN != "")
{
GuiControl, , Gui_WeaponName51, %Slot51AN%
if(Gui_WeaponCheck51 = 1)
{
WeaponAbility51 := Slot51Ability
}
if(Gui_WeaponCheck51 = 0)
{
WeaponAbility51 =
}
}
if(Slot52AN != "")
{
GuiControl, , Gui_WeaponName52, %Slot52AN%
if(Gui_WeaponCheck52 = 1)
{
WeaponAbility52 := Slot52Ability
}
if(Gui_WeaponCheck52 = 0)
{
WeaponAbility52 =
}
}
if(Slot53AN != "")
{
GuiControl, , Gui_WeaponName53, %Slot53AN%
if(Gui_WeaponCheck53 = 1)
{
WeaponAbility53 := Slot53Ability
}
if(Gui_WeaponCheck53 = 0)
{
WeaponAbility53 =
}
}
if(Slot54AN != "")
{
GuiControl, , Gui_WeaponName54, %Slot54AN%
if(Gui_WeaponCheck54 = 1)
{
WeaponAbility54 := Slot54Ability
}
if(Gui_WeaponCheck54 = 0)
{
WeaponAbility54 =
}
}
if(Slot55AN != "")
{
GuiControl, , Gui_WeaponName55, %Slot55AN%
if(Gui_WeaponCheck55 = 1)
{
WeaponAbility55 := Slot55Ability
}
if(Gui_WeaponCheck55 = 0)
{
WeaponAbility55 =
}
}
if(Slot56AN != "")
{
GuiControl, , Gui_WeaponName56, %Slot56AN%
if(Gui_WeaponCheck56 = 1)
{
WeaponAbility56 := Slot56Ability
}
if(Gui_WeaponCheck56 = 0)
{
WeaponAbility56 =
}
}
Check_SMagicN()
Check_SMagic()
if(Slot3MN != "")
{
GuiControl, , Gui_MagicName3, %Slot3MN%
if(Gui_MagicCheck3 = 1)
{
MagicAbility3 := Slot3Magic
}
if(Gui_MagicCheck3 = 0)
{
MagicAbility3 =
}
}
if(Slot4MN != "")
{
GuiControl, , Gui_MagicName4, %Slot4MN%
if(Gui_MagicCheck4 = 1)
{
MagicAbility4 := Slot4Magic
}
if(Gui_MagicCheck4 = 0)
{
MagicAbility4 =
}
}
if(Slot5MN != "")
{
GuiControl, , Gui_MagicName5, %Slot5MN%
if(Gui_MagicCheck5 = 1)
{
MagicAbility5 := Slot5Magic
}
if(Gui_MagicCheck5 = 0)
{
MagicAbility5 =
}
}
if(Slot6MN != "")
{
GuiControl, , Gui_MagicName6, %Slot6MN%
if(Gui_MagicCheck6 = 1)
{
MagicAbility6 := Slot6Magic
}
if(Gui_MagicCheck6 =0)
{
MagicAbility6 =
}
}
if(Slot7MN != "")
{
GuiControl, , Gui_MagicName7, %Slot7MN%
if(Gui_MagicCheck7 = 1)
{
MagicAbility7 := Slot7Magic
}
if(Gui_MagicCheck7 = 0)
{
MagicAbility7 =
}
}
if(Slot8MN != "")
{
GuiControl, , Gui_MagicName8, %Slot8MN%
if(Gui_MagicCheck8 = 1)
{
MagicAbility8 := Slot8Magic
}
if(Gui_MagicCheck8 = 0)
{
MagicAbility8 =
}
}
if(Slot9MN != "")
{
GuiControl, , Gui_MagicName9, %Slot9MN%
if(Gui_MagicCheck9 = 1)
{
MagicAbility9 := Slot9Magic
}
if(Gui_MagicCheck9 = 0)
{
MagicAbility9 =
}
}
if(Slot10MN != "")
{
GuiControl, , Gui_MagicName10, %Slot10MN%
if(Gui_MagicCheck10 = 1)
{
MagicAbility10 := Slot10Magic
}
if(Gui_MagicCheck10 = 0)
{
MagicAbility10 =
}
}
if(Slot11MN != "")
{
GuiControl, , Gui_MagicName11, %Slot11MN%
if(Gui_MagicCheck11 = 1)
{
MagicAbility11 := Slot11Magic
}
if(Gui_MagicCheck11 = 0)
{
MagicAbility11 =
}
}
if(Slot12MN != "")
{
GuiControl, , Gui_MagicName12, %Slot12MN%
if(Gui_MagicCheck12 = 1)
{
MagicAbility12 := Slot12Magic
}
if(Gui_MagicCheck12 =0)
{
MagicAbility12 =
}
}
if(Slot13MN != "")
{
GuiControl, , Gui_MagicName13, %Slot13MN%
if(Gui_MagicCheck13 = 1)
{
MagicAbility13 := Slot13Magic
}
if(Gui_MagicCheck13 = 0)
{
MagicAbility13 =
}
}
if(Slot14MN != "")
{
GuiControl, , Gui_MagicName14, %Slot14MN%
if(Gui_MagicCheck14 = 1)
{
MagicAbility14 := Slot14Magic
}
if(Gui_MagicCheck14 = 0)
{
MagicAbility14 =
}
}
if(Slot15MN != "")
{
GuiControl, , Gui_MagicName15, %Slot15MN%
if(Gui_MagicCheck15 = 1)
{
MagicAbility15 := Slot15Magic
}
if(Gui_MagicCheck15 = 0)
{
MagicAbility15 =
}
}
if(Slot16MN != "")
{
GuiControl, , Gui_MagicName16, %Slot16MN%
if(Gui_MagicCheck16 = 1)
{
MagicAbility16 := Slot16Magic
}
if(Gui_MagicCheck16 =0)
{
MagicAbility16 =
}
}
if(Slot17MN != "")
{
GuiControl, , Gui_MagicName17, %Slot17MN%
if(Gui_MagicCheck17 = 1)
{
MagicAbility17 := Slot17Magic
}
if(Gui_MagicCheck17 = 0)
{
MagicAbility17 =
}
}
if(Slot18MN != "")
{
GuiControl, , Gui_MagicName18, %Slot18MN%
if(Gui_MagicCheck18 = 1)
{
MagicAbility18 := Slot18Magic
}
if(Gui_MagicCheck18 = 0)
{
MagicAbility18 =
}
}
}
return
마법읽어오기()
{
for Index, spell in SpellList
{
%spell%번호 := 0
}
A:=0
Gui,ListView,마법리스트
LV_Delete()
loop,30
{
A := 4 * A_index
마법이름 := "마법" . A_index . "_이름"
마법%A_index%_이름 := jelan.readString(0x0058DAD4, 50, "UTF-16", 0x178, 0xc2, 0x8, A, 0x8, 0xC)
그레이드 := "마법" . A_index . "_그레이드"
마법%A_index% := jelan.read(0x0058DAD4, "UInt", 0x178, 0xc2, 0x8, A, 0x8, 0x42C)
마법레벨 := "마법" . A_index
%그레이드% :=  jelan.read(0x0058DAD4, "UInt", 0x178, 0xc2, 0x8, A, 0x8, 0x430)
Ability_name := %마법이름%
Ability_Grade := %그레이드%
Ability := %마법레벨%
if(마법이름 != Fail && %마법이름% != "")
{
Gui,ListView,마법리스트
LV_Add("",A_Index,Ability_name,Ability_Grade,Ability)
}
else
break
}
}
어빌리티읽어오기()
{
A:=0
gui, default
Gui, Submit, NoHide
Gui,ListView,어빌리티리스트
LV_Delete()
GuiControlGet,무기1,,Gui_BasicWName0,
GuiControlGet,무기2,,Gui_BasicWName1,
GuiControlGet,무기3,,Gui_BasicWName2,
GuiControlGet,무기4,,Gui_BasicWName3,
loop,72
{
A := 4 * A_index
어빌리티이름 := "어빌리티" . A_index . "_이름"
%어빌리티이름% := jelan.readString(0x0058DAD4, 50, "UTF-16", 0x178, 0xc6, 0x8, A, 0x8, 0x4)
그레이드 := "어빌리티" . A_index . "_그레이드"
%그레이드% := jelan.read(0x0058DAD4, "UInt", 0x178, 0xc6, 0x8, A, 0x8, 0x20c)
어빌리티 := "어빌리티" . A_index
%어빌리티% := jelan.read(0x0058DAD4, "UInt", 0x178, 0xc6, 0x8, A, 0x8, 0x208)
Ability_name := %어빌리티이름%
Ability_Grade := %그레이드%
Ability := %어빌리티%
어빌리티 := round(%어빌리티%/100,2)
어빌번호 := A_index
if(어빌리티이름 != Fail && %어빌리티이름% != "")
{
Gui, Submit, NoHide
Gui, ListView,어빌리티리스트
LV_Add("",A_Index,%어빌리티이름%, %그레이드%, 어빌리티)
}
else
break
}
}
그레이드읽어오기()
{
Gui, Submit, Nohide
A:=0
GuiControlGet,무기1,,Gui_BasicWName0,
GuiControlGet,무기2,,Gui_BasicWName1,
GuiControlGet,무기3,,Gui_BasicWName2,
GuiControlGet,무기4,,Gui_BasicWName3,
loop,72
{
A := 4 * A_index
어빌리티이름 := "어빌리티" . A_index . "_이름"
%어빌리티이름% := jelan.readString(0x0058DAD4, 50, "UTF-16", 0x178, 0xc6, 0x8, A, 0x8, 0x4)
그레이드 := "어빌리티" . A_index . "_그레이드"
%그레이드% := jelan.read(0x0058DAD4, "UInt", 0x178, 0xc6, 0x8, A, 0x8, 0x20c)
Ability_name := %어빌리티이름%
Ability_Grade := %그레이드%
if(어빌리티이름 != Fail && %어빌리티이름% != "")
{
if(무기1 = Ability_name)
{
GuiControl,,Gui_Grade0,%Ability_Grade%
}
if(무기2 = Ability_name)
{
GuiControl,,Gui_Grade1,%Ability_Grade%
}
if(무기3 = Ability_name)
{
GuiControl,,Gui_Grade2,%Ability_Grade%
}
if(무기4 = Ability_name)
{
GuiControl,,Gui_Grade3,%Ability_Grade%
}
}
else
break
}
}
기술읽어오기()
{
A:=0
Gui, Submit, NoHide
SendMessage,0x184,,,listbox1
loop,72
{
A := 4 * A_index
어빌리티이름 := "어빌리티" . A_index . "_이름"
%어빌리티이름% := jelan.readString(0x0058DAD4, 50, "UTF-16", 0x178, 0xc6, 0x8, A, 0x8, 0x4)
Ability_name := %어빌리티이름%
어빌번호 := A_index
if( Ability_name = "현혹" )
{
현혹번호 := 어빌번호
}
if( Ability_name = "폭검" )
{
폭검번호 := 어빌번호
}
if( Ability_name = "독침" )
{
독침번호 := 어빌번호
}
if( Ability_name = "무기공격" )
{
무기공격번호 := 어빌번호
}
if( Ability_name = "대화" )
{
대화번호 := 어빌번호
}
if( Ability_name = "명상" )
{
명상번호 := 어빌번호
}
if( Ability_name = "더블어택" )
{
더블어택번호 := 어빌번호
}
if( Ability_name = "체력향상" )
{
체력향상번호 := 어빌번호
}
if( Ability_name = "집중" )
{
집중번호 := 어빌번호
}
if( Ability_name = "회피" )
{
회피번호 := 어빌번호
}
if( Ability_name = "몸통지르기" )
{
몸통지르기번호 := 어빌번호
}
if( Ability_name = "RemoveArmor" )
{
리무브아머번호 := 어빌번호
}
if( Ability_name = "민첩향상" )
{
민첩향상번호 := 어빌번호
}
if( Ability_name = "활방어" )
{
활방어번호 := 어빌번호
}
if( Ability_name = "마력향상" )
{
마력향상번호 := 어빌번호
}
if( Ability_name = "마력방어향상" )
{
마력방어번호 := 어빌번호
}
}
}
파라스대기:
Gui, Submit, Nohide
settimer, 파라스대기, off
파라스타이머시작 := 0
파라스방해감지 := 0
CheckPB = 0
CheckPN = 0
MapNumber = 1
MobNumber = 1
GuiControl, , Gui_NowState, [포남] 파라스 다시 가보기.
GuiControl, , Gui_HuntAuto, 1
SB_SetText("파라스가 풀렸는지 확인")
Step = 8
CheckPB = 0
CheckPN = 0
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
return
Tab:
Gui, Submit, Nohide
GuiControlGet, CurrentTab, , Tabsize
if( CurrentTab = 1 )
{
GuiControl, show, 탭2
GuiControl, show, 탭3
GuiControl, show, 탭4
GuiControl, show, 탭5
}else
{
GuiControl, Hide, 탭2
GuiControl, Hide, 탭3
GuiControl, Hide, 탭4
GuiControl, Hide, 탭5
}
if (CurrentTab != 1 && CurrentTab != 2 && CurrentTab != 3)
{
Gui, Show, w780 h680, 마개조 똘룸체잠 %ProgramVersion%
if( 실행창위치 = 0)
{
GuiControl, move, Tabsize, w780 h680
}else if( 실행창위치 = 1 )
{
GuiControl, move, Tabsize, w780 h680
WinMove, ahk_id %Gui_ID%, , 801,0
}else if( 실행창위치 = 2 )
{
GuiControl, move, Tabsize, w780 h680
WinMove, ahk_id %Gui_ID%, , 368,120
}else if( 실행창위치 = 3 )
{
GuiControl, move, Tabsize, w780 h680
WinMove, ahk_id %Gui_ID%, , 670,0
}
}
else if (CurrentTab = 1 && 시작탭사이즈 = 0)
{
Gui, Show, w780 h680, 마개조 똘룸체잠 %ProgramVersion%
if( 실행창위치 = 0 && 시작탭사이즈 = 0)
{
GuiControl, move, Tabsize, w780 h680
}else if( 실행창위치 = 1 && 시작탭사이즈 = 0)
{
GuiControl, move, Tabsize, w780 h680
WinMove, ahk_id %Gui_ID%, , 801,0
}else if( 실행창위치 = 2 && 시작탭사이즈 = 0)
{
GuiControl, move, Tabsize, w780 h680
WinMove, ahk_id %Gui_ID%, , 368,120
}else if( 실행창위치 = 3 && 시작탭사이즈 = 0)
{
GuiControl, move, Tabsize, w780 h680
WinMove, ahk_id %Gui_ID%, , 670,0
}
}
else if (CurrentTab = 1 && 시작탭사이즈 = 1)
{
Gui, Show, w780 h680, 마개조 똘룸체잠 %ProgramVersion%
if( 실행창위치 = 0 && 시작탭사이즈 = 1)
{
GuiControl, move, Tabsize, w780 h680
}else if( 실행창위치 = 1 && 시작탭사이즈 = 1)
{
GuiControl, move, Tabsize, w780 h680
WinMove, ahk_id %Gui_ID%, , 801,0
}else if( 실행창위치 = 2 && 시작탭사이즈 = 1)
{
GuiControl, move, Tabsize, w780 h680
WinMove, ahk_id %Gui_ID%, , 368,120
}else if( 실행창위치 = 3 && 시작탭사이즈 = 1)
{
GuiControl, move, Tabsize, w780 h680
WinMove, ahk_id %Gui_ID%, , 670,0
}
}
else if (CurrentTab = 2)
{
Gui, Show, w700 h610, 마개조 똘룸체잠 %ProgramVersion%
if( 실행창위치 = 0)
{
GuiControl, move, Tabsize, w700 h610
}else if( 실행창위치 = 1 )
{
GuiControl, move, Tabsize, w700 h610
WinMove, ahk_id %Gui_ID%, , 801,0
}else if( 실행창위치 = 2 )
{
GuiControl, move, Tabsize, w700 h610
WinMove, ahk_id %Gui_ID%, , 368,120
}else if( 실행창위치 = 3 )
{
GuiControl, move, Tabsize, w700 h610
WinMove, ahk_id %Gui_ID%, , 670,0
}
}
else if (CurrentTab = 3)
{
Gui, Show, w700 h610, 마개조 똘룸체잠 %ProgramVersion%
if( 실행창위치 = 0)
{
GuiControl, move, Tabsize, w700 h610
}else if( 실행창위치 = 1 )
{
GuiControl, move, Tabsize, w700 h610
WinMove, ahk_id %Gui_ID%, , 801,0
}else if( 실행창위치 = 2 )
{
GuiControl, move, Tabsize, w700 h610
WinMove, ahk_id %Gui_ID%, , 368,120
}else if( 실행창위치 = 3 )
{
GuiControl, move, Tabsize, w700 h610
WinMove, ahk_id %Gui_ID%, , 670,0
}
}
return
일시정지:
Gui, Submit, Nohide
GuiControl,,Gui_Nowstate, 일시정지 되었습니다.
GuiControl,Disabled,Gui_StopButton
GuiControl,Hide,Gui_StopButton
GuiControl,Enable,Gui_RestartButton
GuiControl,Show,Gui_RestartButton
SplashImage, 1: off
SplashImage, 2: off
SplashImage, 3: off
SplashImage, 4: off
SplashImage, 5: off
SplashImage, 6: off
SplashImage, 7: off
SplashImage, 8: off
SplashImage, 9: off
SplashImage, 10: off
캐릭보이기()
jelan.write(0x0047AA5B, 0x7d, "Char", aOffsets*)
jelan.write(0x0047A196, 0x75, "Char", aOffsets*)
jelan.write(0x0047A18D, 0x75, "Char", aOffsets*)
jelan.write(0x0047AA20, 0x74, "Char", aOffsets*)
jelan.write(0x0045D98B, 0x89, "Char", aOffsets*)
jelan.write(0x0045D98C, 0x43, "Char", aOffsets*)
jelan.write(0x0045D98D, 0x44, "Char", aOffsets*)
jelan.write(0x0045DA94, 0x89, "Char", aOffsets*)
jelan.write(0x0045DA95, 0x43, "Char", aOffsets*)
jelan.write(0x0045DA96, 0x4C, "Char", aOffsets*)
jelan.write(0x0045DAA9, 0x8B, "Char", aOffsets*)
jelan.write(0x0045DAAA, 0x46, "Char", aOffsets*)
jelan.write(0x0045DAAB, 0x50, "Char", aOffsets*)
jelan.write(0x0045D32E, 0x89, "Char", aOffsets*)
jelan.write(0x0045D32F, 0x43, "Char", aOffsets*)
jelan.write(0x0045D330, 0x3C, "Char", aOffsets*)
jelan.write(0x0045D4E7, 0x8B, "Char", aOffsets*)
jelan.write(0x0045D4E8, 0x46, "Char", aOffsets*)
jelan.write(0x0045D4E9, 0x08, "Char", aOffsets*)
jelan.write(0x0045D43E, 0x8B, "Char", aOffsets*)
jelan.write(0x0045D43F, 0x46, "Char", aOffsets*)
jelan.write(0x0045D440, 0x10, "Char", aOffsets*)
jelan.write(0x0045D422, 0x89, "Char", aOffsets*)
jelan.write(0x0045D423, 0x43, "Char", aOffsets*)
jelan.write(0x0045D424, 0x04, "Char", aOffsets*)
jelan.write(0x0047AD18, 0x74, "Char", aOffsets*)
Sleep,2000
Set_MoveSpeed()
AltR()
Pause
SB_SetText("일시정지 상태")
return
재시작:
Gui, Submit, Nohide
GuiControl,,Gui_Nowstate, 곧 재시작 합니다.
SB_SetText("3초후 재시작합니다.")
GuiControl,Disabled,Gui_RestartButton
GuiControl,Hide,Gui_RestartButton
GuiControl,Enable,Gui_StopButton
GuiControl,Show,Gui_StopButton
jelan.write(0x0047AA5B, 0xEB, "Char", aOffsets*)
jelan.write(0x0047A196, 0xEB, "Char", aOffsets*)
jelan.write(0x0047A18D, 0xEB, "Char", aOffsets*)
jelan.write(0x0047AA20, 0xEB, "Char", aOffsets*)
if(HuntPlace = 2)
{
포북캐릭()
}
else if(HuntPlace = 1)
{
캐릭제거()
}
jelan.write(0x0047AD18, 0xEB, "Char", aOffsets*)
Sleep, 3000
Set_MoveSpeed()
AltR()
Pause
SB_SetText("구동 시작")
return
어빌리티리스트갱신:
{
어빌리티읽어오기()
}
return
기술갱신:
{
기술읽어오기()
}
return
마법갱신:
{
마법읽어오기()
}
return
Set_Food()
{
FoodADD := jelan.read(0x0058DAD0, "UInt", 0x74, 0x0)
FoodADD := FoodADD + 0x128 + 0xCC24
value := jelan.write(FoodADD, 100, "UInt")
FoodADD := jelan.read(0x0058DAD0, "UInt", 0x74, 0x0)
FoodADD := FoodADD + 0x120 + 0xCC24
value := jelan.write(FoodADD, 100, "UInt")
}
return
차원체크:
Gui, Submit, Nohide
if( 랜덤차원 = 1 )
{
if(Gui_CheckUseParty = 0)
{
Random, CountPortal, 0, 2
}
현재차원 := CountPortal
}
if ( 알파차원 = 1 )
{
CountPortal = 0
현재차원 := CountPortal
}
if ( 베타차원 = 1 )
{
CountPortal = 1
현재차원 := CountPortal
}
if ( 감마차원 = 1 )
{
CountPortal = 2
현재차원 := CountPortal
}
if( Gui_PartyON = 1 && 차원 = 1 )
{
MsgBox,48, 차원설정,파티를 사용하려면 차원을 지정해주세요.
}
return
원격파티사용:
Gui, Submit, Nohide
if( Gui_PartyOff = 1 && Gui_CheckUseParty = 1)
{
MsgBox,48, 원격파티,파티설정을 자동으로 ON으로 변경합니다.`n파티설정을 OFF하려면 원격파티 사용을 꺼주세요.
GuiControl,, Gui_PartyON,1
}
return
원격파티설명서:
Gui, Submit, Nohide
MsgBox,576, 원격파티,
(
	<원격파티 사용법>`n`n
1. 해당 VMWARE 안에다가 파티할캐릭터 키기.`n
2. 닉네임을 정확하게 입력 후 해당 캐릭터번호를 선택하기.
   캐릭터번호는 ( 캐릭터선택창 순서 입니다. )`n
3. 차원설정을 파티캐릭터위치에 맞게 설정하기
   예를들어서 파티캐릭터들이 베타에있으면`n   차원설정도 베타로설정하기.

차원설정이 안맞으면 프로그램 오류납니다.

)
return
어빌해제:
Gui, Submit, Nohide
GuiControl, , Gui_WeaponCheck1, 0
GuiControl, , Gui_WeaponCheck2, 0
GuiControl, , Gui_WeaponCheck3, 0
GuiControl, , Gui_WeaponCheck4, 0
GuiControl, , Gui_WeaponCheck5, 0
GuiControl, , Gui_WeaponCheck6, 0
GuiControl, , Gui_WeaponCheck7, 0
GuiControl, , Gui_WeaponCheck8, 0
GuiControl, , Gui_WeaponCheck9, 0
GuiControl, , Gui_WeaponCheck10, 0
GuiControl, , Gui_WeaponCheck11, 0
GuiControl, , Gui_WeaponCheck12, 0
GuiControl, , Gui_WeaponCheck13, 0
GuiControl, , Gui_WeaponCheck14, 0
GuiControl, , Gui_WeaponCheck15, 0
GuiControl, , Gui_WeaponCheck16, 0
GuiControl, , Gui_WeaponCheck17, 0
GuiControl, , Gui_WeaponCheck18, 0
GuiControl, , Gui_WeaponCheck19, 0
GuiControl, , Gui_WeaponCheck20, 0
GuiControl, , Gui_WeaponCheck21, 0
GuiControl, , Gui_WeaponCheck22, 0
GuiControl, , Gui_WeaponCheck23, 0
GuiControl, , Gui_WeaponCheck24, 0
GuiControl, , Gui_WeaponCheck25, 0
GuiControl, , Gui_WeaponCheck26, 0
GuiControl, , Gui_WeaponCheck27, 0
GuiControl, , Gui_WeaponCheck28, 0
GuiControl, , Gui_WeaponCheck29, 0
GuiControl, , Gui_WeaponCheck30, 0
GuiControl, , Gui_WeaponCheck31, 0
GuiControl, , Gui_WeaponCheck32, 0
GuiControl, , Gui_WeaponCheck33, 0
GuiControl, , Gui_WeaponCheck34, 0
GuiControl, , Gui_WeaponCheck35, 0
GuiControl, , Gui_WeaponCheck36, 0
GuiControl, , Gui_WeaponCheck37, 0
GuiControl, , Gui_WeaponCheck38, 0
GuiControl, , Gui_WeaponCheck39, 0
GuiControl, , Gui_WeaponCheck40, 0
GuiControl, , Gui_WeaponCheck41, 0
GuiControl, , Gui_WeaponCheck42, 0
GuiControl, , Gui_WeaponCheck43, 0
GuiControl, , Gui_WeaponCheck44, 0
GuiControl, , Gui_WeaponCheck45, 0
GuiControl, , Gui_WeaponCheck46, 0
GuiControl, , Gui_WeaponCheck47, 0
GuiControl, , Gui_WeaponCheck48, 0
GuiControl, , Gui_WeaponCheck49, 0
GuiControl, , Gui_WeaponCheck50, 0
GuiControl, , Gui_WeaponCheck51, 0
GuiControl, , Gui_WeaponCheck52, 0
GuiControl, , Gui_WeaponCheck53, 0
GuiControl, , Gui_WeaponCheck54, 0
GuiControl, , Gui_WeaponCheck55, 0
GuiControl, , Gui_WeaponCheck56, 0
return
어빌선택:
Gui, Submit, Nohide
GuiControl, , Gui_WeaponCheck1, 1
GuiControl, , Gui_WeaponCheck2, 1
GuiControl, , Gui_WeaponCheck3, 1
GuiControl, , Gui_WeaponCheck4, 1
GuiControl, , Gui_WeaponCheck5, 1
GuiControl, , Gui_WeaponCheck6, 1
GuiControl, , Gui_WeaponCheck7, 1
GuiControl, , Gui_WeaponCheck8, 1
GuiControl, , Gui_WeaponCheck9, 1
GuiControl, , Gui_WeaponCheck10, 1
GuiControl, , Gui_WeaponCheck11, 1
GuiControl, , Gui_WeaponCheck12, 1
GuiControl, , Gui_WeaponCheck13, 1
GuiControl, , Gui_WeaponCheck14, 1
GuiControl, , Gui_WeaponCheck15, 1
GuiControl, , Gui_WeaponCheck16, 1
GuiControl, , Gui_WeaponCheck17, 1
GuiControl, , Gui_WeaponCheck18, 1
GuiControl, , Gui_WeaponCheck19, 1
GuiControl, , Gui_WeaponCheck20, 1
GuiControl, , Gui_WeaponCheck21, 1
GuiControl, , Gui_WeaponCheck22, 1
GuiControl, , Gui_WeaponCheck23, 1
GuiControl, , Gui_WeaponCheck24, 1
GuiControl, , Gui_WeaponCheck25, 1
GuiControl, , Gui_WeaponCheck26, 1
GuiControl, , Gui_WeaponCheck27, 1
GuiControl, , Gui_WeaponCheck28, 1
GuiControl, , Gui_WeaponCheck29, 1
GuiControl, , Gui_WeaponCheck30, 1
GuiControl, , Gui_WeaponCheck31, 1
GuiControl, , Gui_WeaponCheck32, 1
GuiControl, , Gui_WeaponCheck33, 1
GuiControl, , Gui_WeaponCheck34, 1
GuiControl, , Gui_WeaponCheck35, 1
GuiControl, , Gui_WeaponCheck36, 1
GuiControl, , Gui_WeaponCheck37, 1
GuiControl, , Gui_WeaponCheck38, 1
GuiControl, , Gui_WeaponCheck39, 1
GuiControl, , Gui_WeaponCheck40, 1
GuiControl, , Gui_WeaponCheck41, 1
GuiControl, , Gui_WeaponCheck42, 1
GuiControl, , Gui_WeaponCheck43, 1
GuiControl, , Gui_WeaponCheck44, 1
GuiControl, , Gui_WeaponCheck45, 1
GuiControl, , Gui_WeaponCheck46, 1
GuiControl, , Gui_WeaponCheck47, 1
GuiControl, , Gui_WeaponCheck48, 1
GuiControl, , Gui_WeaponCheck49, 1
GuiControl, , Gui_WeaponCheck50, 1
GuiControl, , Gui_WeaponCheck51, 1
GuiControl, , Gui_WeaponCheck52, 1
GuiControl, , Gui_WeaponCheck53, 1
GuiControl, , Gui_WeaponCheck54, 1
GuiControl, , Gui_WeaponCheck55, 1
GuiControl, , Gui_WeaponCheck56, 1
return
소각갱신:
Gui, Submit, Nohide
Gui, listview, 포프레스네소각
LV_Delete()
Loop, Read, %A_ScriptDir%\소각리스트.ini
{
    LV_Add(A_Index, A_LoopReadLine)
}
return
좌표입력(X,Y,Z)
{
X := Format("0x{:08X}", X)
Y := Format("0x{:08X}", Y)
Z := Format("0x{:08X}", Z)
gui,submit,nohide
value := jelan.write(0x00590600, X, "UInt", aOffsets*)
value := jelan.write(0x00590604, Y, "UInt", aOffsets*)
value := jelan.write(0x00590608, Z, "UInt", aOffsets*)
}
return
타겟팅:
{
Gui, Submit, Nohide
if(Step = 27 or Step = 1026)
{
for Index, spell in TargetSkillList
{
temp_skill_usecheck := %spell%사용
if (temp_skill_usecheck == 1 && spell = "현혹")
{
타겟스킬사용(현혹번호)
sleep, 10
}
if (temp_skill_usecheck == 1 && spell = "폭검")
{
타겟스킬사용(폭검번호)
sleep, 10
}
if (temp_skill_usecheck == 1 && spell = "독침")
{
타겟스킬사용(독침번호)
sleep, 10
}
if (temp_skill_usecheck == 1 && spell = "무기공격")
{
타겟스킬사용(무기공격번호)
sleep, 10
}
}
}
}
return
타겟스킬사용(스킬번호)
{
타겟스킬대상 := jelan.read(0x00584C2C, "UInt", aOffsets*)
if ( 타겟스킬대상 != 0 )
{
WriteExecutableMemory("타겟스킬호출")
jelan.write(0x0059023A, 스킬번호, "Char", aOffsets*)
Sleep,10
jelan.write(0x0059023B, 타겟스킬대상, "UInt", aOffsets*)
sleep,10
RunMemory("타겟스킬사용")
}
}
마법사용(대상)
{
마법번호 := 0
마법대상 := 0
SetFormat, Integer, H
if (대상 = "자신")
{
마법대상 := jelan.read(0x0058DAD4, "UInt", 0x62)
}
else if (대상 = "클릭된대상")
{
마법대상 := jelan.read(0x00584C2C, "UInt", aOffsets*)
}
else
{
마법대상 := 대상
}
마법번호 := Gui_MagicNStack
Random, 마법번호, 3, 마법번호
if (마법번호 != 0 && 마법대상!= 0)
{
WriteExecutableMemory("마법호출")
jelan.write(0x0059043A, 마법번호, "Char", aOffsets*)
jelan.write(0x0059043B, 마법대상, "UInt", aOffsets*)
sleep,10
RunMemory("마법사용")
}
SetFormat, Integer, D
}
IsDataInList(data, list)
{
for _, item in list
{
if (item = data)
return true
}
return false
}
IsWordInList(word, list)
{
for _, item in list
{
if InStr(word, item)
{
return true
}
}
return false
}
GetOSVersion() {
Path = HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion
RegRead, ProductName, %Path%, ProductName
return ProductName
}
WantedMonsterLength := 0
if WantedMonsters.MaxIndex()
WantedMonsterLength := WantedMonsters.MaxIndex()
DisWantedMonsterLength := 0
if DisWantedMonsters.MaxIndex()
DisWantedMonsterLength := DisWantedMonsters.MaxIndex()
Loop, %LVCount%
{
thisRow := A_Index
LV_GetText(col12Value, thisRow, 12)
LV_GetText(col5Value, thisRow, 5)
LV_GetText(col6Value, thisRow, 6)
SB_SETTEXT(WantedMonsterlength "/" DisWantedMonsterLength,5)
if ((col12Value < lowestCol12Value && !IsDataInList(col6Value, BlackList)) && ((IsDataInList(col5Value, WantedMonsters) && WantedMonsterLength >= 1) || WantedMonsterLength < 1 ) && ((!IsDataInList(col5Value, DisWantedMonsters) && DisWantedMonsterLength>=1) || DisWantedMonsterLength == 0))
{
lowestCol12Value := col12Value
selectedRow := thisRow
}
}
if (selectedRow > 0)
{
LV_Modify(selectedRow, "Select")
}
return
*/
AnimatedGifControl(GuiNameOrHwnd, GifFilePath, ControlOptions="") {
Static
Static CallCount := 0
Local pos, ObjectName, bgColor
ObjectName := "WB" ++CallCount
AnimatedGifControl_GetImageDimensions(GifFilePath, GifWidth, GifHeight)
if RegExMatch(ControlOptions, "O)(\s|^)(w(\d+))(\s|$)", oM) {
GifWidth := oM.Value(3)
StringReplace, ControlOptions, ControlOptions, % oM.Value(2),
}
if pos := RegExMatch(ControlOptions, "O)(\s|^)(h(\d+))(\s|$)", oM) {
GifHeight := oM.Value(3)
StringReplace, ControlOptions, ControlOptions, % oM.Value(2),
}
if RegExMatch(ControlOptions, "O)(\s|^)(bgc(\w{6}))(\s|$)", oM) {
bgColor := oM.Value(3)
StringReplace, ControlOptions, ControlOptions, % oM.Value(2),
} else
bgColor := AnimatedGifControl_GetSysColor(15)
Gui, %GuiNameOrHwnd%: Add, ActiveX, % "v" ObjectName " w" GifWidth " h" GifHeight " Disabled " ControlOptions, Shell.Explorer
%ObjectName%.Navigate("about:blank")
%ObjectName%.Document.Write("
 (Ltrim
  <html>
  <header>
  <style type='text/css'>
    img#bg {
    position:fixed;
    top:0;
    left:0;
    width:100%;
    height:100%;
    }
  </style>
  <!--[if IE 6]>
  <![endif]-->
  <!--[if IE 6]>
    <style type='text/css'>
    html { overflow-y: hidden; }
    body { overflow-y: hidden; }
    img#bg { position:absolute; z-index:-1; }
    #content { position:static; }
    </style>
  <![endif]-->
  </header>
  <body style='height: 100%; width: 100%; margin: 0; padding: 0; overflow-x: hidden; overflow-y: hidden; background-color: #" bgColor ";' />
  <img src='" GifFilePath "' id='bg'  />
  </body>
  </html>
)")
%ObjectName%.Document.close
%ObjectName% := ""
Return
}
AnimatedGifControl_GetImageDimensions(ImgPath, Byref width, Byref height) {
DHW := A_DetectHiddenWIndows
DetectHiddenWindows, ON
Gui, AnimatedGifControl_GetImageDimensions: Add, Picture, hwndhWndImage, % ImgPath
GuiControlGet, Image, AnimatedGifControl_GetImageDimensions:Pos, % hWndImage
Gui, AnimatedGifControl_GetImageDimensions: Destroy
DetectHiddenWindows, % DHW
width := ImageW,  height := ImageH
}
AnimatedGifControl_GetSysColor(d_element) {
A_FI:=A_FormatInteger
SetFormat, Integer, Hex
BGR:=DllCall("GetSysColor",Int,d_element)+0x1000000
SetFormat,Integer,%A_FI%
StringMid,R,BGR,8,2
StringMid,G,BGR,6,2
StringMid,B,BGR,4,2
RGB := R G B
StringUpper,RGB,RGB
Return RGB
}

; ---------------------------
TryLoginFail:
    GuiControl, , 로그인상태정보, [로그인] - 실패 ( %reason% )
    SB_SetText("인터넷 로그인 실패: " . reason)

    WinClose, Elancia
    WinKill, ahk_exe MRMsph.exe

    Gosub, CleanChrome

    Gui_Enable()
    GuiControl, , jTitle, %jTitle%

    SetTimer, Hunt, Off
    SetTimer, AttackCheck, Off
    SetTimer, incineration, Off
    SetTimer, 타겟팅, Off

    CheckPN := 0
    CheckPB := 0
    countsignal := 0
    랜덤감응 := 0
return

CleanChrome:
    Try {
        PageInst.Evaluate("inface.auth.gotoSignOut();")
        Sleep, 1000
        PageInst.WaitForLoad()
        PageInst.Evaluate(removeCookiesScript)
        PageInst.Call("Browser.close")
        PageInst.Disconnect()
    } Catch e {
        ; 무시
    }
    PageInst := ""
    Try ChromeInst.Close()
    ChromeInst := ""
return

; ▼ 크롬 인스턴스 정리 루틴
CleanChrome(ByRef page, ByRef chrome)
{
    Try {
        page.Evaluate("inface.auth.gotoSignOut();")
        Sleep, 1000
        page.WaitForLoad()
        page.Evaluate(removeCookiesScript)
        page.Call("Browser.close")
        page.Disconnect()
    } Catch e {
        ; 무시
    }
    page := ""
    Try chrome.Close()
    chrome := ""
}


OID저장() {
    SetFormat, integer, H
    Get_Location()

    if InStr(Location, "[알파차원]") {
        GuiControl,, A리노아, %A리노아%
        GuiControl,, A동파, %A동파%
        GuiControl,, A서파, %A서파%
        GuiControl,, A길잃파, %A길잃파%
        RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA리노아, %A리노아%
        RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA동파, %A동파%
        RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA서파, %A서파%
        RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA길잃파, %A길잃파%
    }

    if InStr(Location, "[베타차원]") {
        GuiControl,, B리노아, %B리노아%
        GuiControl,, B동파, %B동파%
        GuiControl,, B서파, %B서파%
        GuiControl,, B길잃파, %B길잃파%
        RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB리노아, %B리노아%
        RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB동파, %B동파%
        RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB서파, %B서파%
        RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB길잃파, %B길잃파%
    }

    if InStr(Location, "[감마차원]") {
        GuiControl,, G리노아, %G리노아%
        GuiControl,, G동파, %G동파%
        GuiControl,, G서파, %G서파%
        GuiControl,, G길잃파, %G길잃파%
        RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG리노아, %G리노아%
        RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG동파, %G동파%
        RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG서파, %G서파%
        RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG길잃파, %G길잃파%
    }

    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, S업데이트체크, %업데이트체크%
    FormatTime, 적용날짜,, yyyy-MM-dd HH:mm:ss
    GuiControl,, TextControl, OID : %적용날짜%
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, S적용날짜, %적용날짜%

    SetFormat, integer, D
}

OID리셋() {
    SetFormat, integer, H
    A리노아 := 0x0
    A동파 := 0x0
    A서파 := 0x0
    B리노아 := 0x0
    B동파 := 0x0
    B서파 := 0x0
    G리노아 := 0x0
    G동파 := 0x0
    G서파 := 0x0
    A길잃파 := 0x0
    B길잃파 := 0x0
    G길잃파 := 0x0
    SetFormat, integer, D

    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA리노아, %A리노아%
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA동파, %A동파%
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA서파, %A서파%
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB리노아, %B리노아%
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB동파, %B동파%
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB서파, %B서파%
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG리노아, %G리노아%
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG동파, %G동파%
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG서파, %G서파%
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA길잃파, %A길잃파%
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB길잃파, %B길잃파%
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG길잃파, %G길잃파%

    GuiControl,, A리노아, %A리노아%
    GuiControl,, A동파, %A동파%
    GuiControl,, A서파, %A서파%
    GuiControl,, B리노아, %B리노아%
    GuiControl,, B동파, %B동파%
    GuiControl,, B서파, %B서파%
    GuiControl,, G리노아, %G리노아%
    GuiControl,, G동파, %G동파%
    GuiControl,, G서파, %G서파%
    GuiControl,, A길잃파, %A길잃파%
    GuiControl,, B길잃파, %B길잃파%
    GuiControl,, G길잃파, %G길잃파%
    GuiControl,, Gui_KOFF, 1
}
OID읽기() {
    SetFormat, integer, H
    RegRead, 업데이트체크, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, S업데이트체크
    RegRead, A리노아, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA리노아
    RegRead, A동파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA동파
    RegRead, A서파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA서파
    RegRead, B리노아, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB리노아
    RegRead, B동파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB동파
    RegRead, B서파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB서파
    RegRead, G리노아, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG리노아
    RegRead, G동파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG동파
    RegRead, G서파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG서파
    RegRead, A길잃파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA길잃파
    RegRead, B길잃파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB길잃파
    RegRead, G길잃파, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SG길잃파
    RegRead, 적용날짜, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, S적용날짜
    FormatTime, 적용날짜,, yyyy-MM-dd HH:mm:ss
    GuiControl,, TextControl, OID : %적용날짜%
    GuiControl,, A리노아, %A리노아%
    GuiControl,, A동파, %A동파%
    GuiControl,, A서파, %A서파%
    GuiControl,, B리노아, %B리노아%
    GuiControl,, B동파, %B동파%
    GuiControl,, B서파, %B서파%
    GuiControl,, G리노아, %G리노아%
    GuiControl,, G동파, %G동파%
    GuiControl,, G서파, %G서파%
    GuiControl,, A길잃파, %A길잃파%
    GuiControl,, B길잃파, %B길잃파%
    GuiControl,, G길잃파, %G길잃파%
    SetFormat, integer, D
}

감응() {
    ; 전역 변수 사용 선언
    Gui, Submit, Nohide

    if (Gui_KON = 1) {
        if InStr(Location, "[알파차원] 포프레스네 마을") {
            Sleep, 100
            포남입장시간 := A_TickCount
            countsignal := 0
            return
        }

        if InStr(Location, "[알파차원] 포프레스네 남쪽") {
            if (countsignal = 0) {
                jelan.write(0x00527B1C, A동파, "UInt")
                jelan.write(0x00527B1C, A동파, "UInt")
                Sleep, 30
                Send, {F14}
                Sleep, 100
                Send, {F14}
                Sleep, 100
                countsignal := 1
                호출대상 := "알파 - 동쪽파수꾼"
                return
            }
            if (countsignal = 1) {
                jelan.write(0x00527B1C, A서파, "UInt")
                jelan.write(0x00527B1C, A서파, "UInt")
                Sleep, 30
                Send, {F14}
                Sleep, 100
                Send, {F14}
                Sleep, 100
                포남입장시간 := A_TickCount
                countsignal := 0
                호출대상 := "알파 - 서쪽파수꾼"
                return
            }
        }

        if InStr(Location, "[베타차원] 포프레스네 남쪽") {
            if (countsignal = 0) {
                jelan.write(0x00527B1C, B동파, "UInt")
                jelan.write(0x00527B1C, B동파, "UInt")
                Sleep, 30
                Send, {F14}
                Sleep, 100
                Send, {F14}
                Sleep, 100
                countsignal := 1
                호출대상 := "베타 - 동쪽파수꾼"
                return
            }
            if (countsignal = 1) {
                jelan.write(0x00527B1C, B서파, "UInt")
                jelan.write(0x00527B1C, B서파, "UInt")
                Sleep, 30
                Send, {F14}
                Sleep, 100
                Send, {F14}
                Sleep, 100
                포남입장시간 := A_TickCount
                countsignal := 0
                호출대상 := "베타 - 서쪽파수꾼"
                return
            }
        }

        if InStr(Location, "[감마차원] 포프레스네 남쪽") {
            if (countsignal = 0) {
                jelan.write(0x00527B1C, G동파, "UInt")
                jelan.write(0x00527B1C, G동파, "UInt")
                Sleep, 30
                Send, {F14}
                Sleep, 100
                Send, {F14}
                Sleep, 100
                countsignal := 1
                호출대상 := "감마 - 동쪽파수꾼"
                return
            }
            if (countsignal = 1) {
                jelan.write(0x00527B1C, G서파, "UInt")
                jelan.write(0x00527B1C, G서파, "UInt")
                Sleep, 30
                Send, {F14}
                Sleep, 100
                Send, {F14}
                Sleep, 100
                포남입장시간 := A_TickCount
                countsignal := 0
                호출대상 := "감마 - 서쪽파수꾼"
                return
            }
        }
    }
}
