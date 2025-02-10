#SingleInstance ignore
#NoEnv
#Persistent
#KeyHistory 0
#NoTrayIcon
#Warn All, Off
ListLines, Off
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
PROCESS,Priority,,High
DllCall("SetProcessWorkingSetSize", "UInt", DllCall("GetCurrentProcess"), "Int", -1, "Int", -1)
Global WinVersion := GetOSVersion()

if not A_IsAdmin {
    MsgBox, 4,, 이 스크립트는 관리자 권한으로 실행되어야 합니다. `n관리자 권한으로 다시 실행하시겠습니까?
    IfMsgBox Yes
        Run *RunAs "%A_ScriptFullPath%"  ; 관리자 권한으로 스크립트 재실행
    ExitApp  ; 관리자 권한으로 재실행하지 않을 경우 스크립트 종료
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

텔레그램메시지보내기:
TTSTOP := 1
IF(TTSTOP := 0)
{
텔레그램메시지보내기()
return
}

텔레그램메시지보내기(Message := "")
{

	if (Message = "")
		Message := "자동으로보내지는메시지"
        Message := StrReplace(Message, "`n", "%0A")
	/*
	BotToken := "1111111111:AAAAAAAAAAAAAAAAAAAAAAAAAAaaaaaaaaa" ; API토큰
	ChatID := "1111111111" ;메시지 받을 ID

	Url := "https://api.telegram.org/bot" . BotToken . "/sendMessage?chat_id=" . ChatID . "&text=" . URLEncode(Message)
	WinHttp := ComObjCreate("WinHttp.WinHttpRequest.5.1")
	WinHttp.Open("GET", Url)
	WinHttp.Send()
	*/

	gui, submit, nohide
	GuiControlGet, ChatID
	if (!IsNumber(ChatID) || ChatID == 0 || ChatID = "")
		return
	Url := "https://script.google.com/macros/s/AKfycbztWCnhTweHZyRtDltONV7bjhSGNq7m0OUUO4z9uN8-F-R-EOK4Puyw00JgNvmihgPs/exec"
    EncodedMessage := URLEncode(Message)
    EncodedMessage := StrReplace(EncodedMessage, "%0A", "`n")  ; 인코딩 후에 `%0A`를 줄바꿈으로 교체
	Url := Url . "?chat_id=" . ChatID . "&text=" . URLEncode(Message)
	WinHttp := ComObjCreate("WinHttp.WinHttpRequest.5.1")
	WinHttp.Open("GET", Url)
	try
    {
        WinHttp.Send()
    }
    catch e
   {
        ; Handle the error, e.g., by logging or displaying a message
        ; This block is executed if there's an error in sending the request, which might indicate no internet connection
        return
    }
}
;}

ON:
global TotalPhys,TotalPhy
VarSetCapacity( MEMORYSTATUSEX,64,0 ), NumPut( 64,MEMORYSTATUSEX )
DllCall( "GlobalMemoryStatusEx", UInt,&MEMORYSTATUSEX )
TotalPhys := NumGet( MEMORYSTATUSEX,8,"Int64"),  VarSetCapacity( PhysMem,16,0 )
DllCall( "shlwapi.dll\StrFormatByteSize64A", Int64,TotalPhys, Str,PhysMem, UInt,16 )

filePath := A_ScriptDir . "\NPCOID.ini"
DllCall("psapi.dll\EmptyWorkingSet", "Ptr", -1)
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

URLEncode(str, sExcepts = "!#$&'()*+,-./:;=?@_~")
{
    len := StrPutVar(str, var, "UTF-8") - 1
    result := ""
    i := 0
    oldFmt := A_FormatInteger
    SetFormat, Integer, H
    While (i < len) {
        b := NumGet(var, i, "UChar")
        If (b >= 0x41 && b <= 0x5A ; A-Z
            || b >= 0x61 && b <= 0x7A ; a-z
            || b >= 0x30 && b <= 0x39 ; 0-9
            || InStr(sExcepts, Chr(b), true))
            result .= Chr(b)
        Else {
            hex := SubStr(StrUpper(b), 3)
            If (StrLen(hex) == 1)
                hex := "0" . hex
            result .= "%" . hex
        }
        i++
    }
    SetFormat, Integer, D
    return result
}

StrUpper(str)
{
    StringUpper, out, str
    return out
}
MouseClick(MouseX,MouseY,PID := "")  ; 지정한 X, Y 좌표 마우스왼쪽버튼으로 클릭
		{
			if (PID = "")
				PID := TargetPID
			if (Multiplyer = "없음" || Multiplyer < 1)
				gosub, 일랜시아창크기구하기
			MouseX := MouseX * Multiplyer
			MouseY := MouseY * Multiplyer
			MousePos := MouseX|MouseY<< 16
			PostMessage, 0x200, 0, %MousePos% ,,ahk_pid %PID%
			PostMessage, 0x201, 1, %MousePos% ,,ahk_pid %PID%
			PostMessage, 0x202, 0, %MousePos% ,,ahk_pid %PID%
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
else if(Key = "프로세스종료"){
      loop, 1 {
      Process,Close, %PID%
      sleep,1
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
게임내수련인형 := ["의수련인형","의정령"]
게임내고용상인들 := ["의소야","의터그","의네시","의미피엘","의엘가노","의휘리스", "의Nesi"]
게임내NPC이미지 := [131,83,137]
게임내NPC들 := ["대장로","성향안내","장로","모험가","초보모험가","요리사","초보요리사","사냥꾼","초보무도가","세크티","콥","미너터","카리스","행운장","길드기","길드예선전보로1","길드예선전보로2","길드예선전보로3","길드예선전보로4","길드예선전보로5","길드예선전보로6","길드예선전보로7","길드예선전보로8","우물지기","우물지킴이","파미","실루엣","케이","휴","에레노아","길드만들기","라드","예절보로","할아버지","레나","초브","칼라스","브라키의여전사","테레즈","루비","오크왕자","슈","카딜라","나무보로","이사크","미소니","성궁사","수련장","그레파","미용바니","티포네","홀리","올드스미스","테디","피니","큐트","키드니","스텔라","실비아","네루아","사라","오블로","메티니","무타이","성검사","커스피스","쿠니퍼","라체","지올라","플린","헬러","브레너","에드가","두갈","아이렌","케드마","제프","젠","소니아","아바","네시아","래리","마리오","빈","렉스","다바트","코바니","플라노","미너스","토리온","브로이","카멘","카로에","시상보로","견습미용사","할머니","미스토","브라키의여전사","그라치","드리트","레시트","로크","메크","스타시","스테티나","이스카","호디니","베니","은행가드","파이","샤네트","코페","아일리아","퀘이드","레야","싱","유키오","이시","앨리아","오바","테론","윌라","페툴라","스티븐","우리안","빅터","리프","미네티","피트","비엔","칸느","포럼","콘스티","다인","티셔","백작","보초병","우트","랜스","뮤즈","리즈","브라키의여전사","에스피","코니","스투","라니체","드류","체드","체스터","케인","울드","티모시","포츠","마카","미카","경비병","니키","수라","카르고","엘피","쿠퍼","페니","터크","나크레토","로비어","앤타이","셀포이","비바","마데이아","가토고","엑소포","토이슨","코메이오","저주받은엘프","야노모이","오이피노","카레푸","엠토포","아이보","마나오","클레오","파노아","타키아","카오네자","나노아","미노스","세니코","주사위소녀1","주사위소녀2","주사위소녀3","주사위소녀4","주사위소녀5","주사위소녀6","주사위소녀7","주사위소녀8","주사위소녀9","주사위소녀10","주사위지배인","리노스","투페","히포프","베스","쿠키","소피","포프리아","나무꾼","레아","키아","세르니오","코르티","베커","포비","크로리스","길잃은수색대","동쪽파수꾼","서쪽파수꾼","리노아","펫조련사","게시판","드골"]
로랜시아간판 := [111,608,610,612,614,618,620,622,624,626,630,632,634,636,638,640,642,644,646,648,650]
이름이바뀌는존재들 := [21,751,753,552,554,560,558,556,496,297]
Lists := [ "CheckBoxList", "DropDownList", "EditList", "RadioButton" ]
CheckBoxList := ["수련길탐딜레이","이동속도사용","게임배속사용","길탐색책사용","원거리타겟","리메듐타겟","오란의깃사용여부","길탐색1번사용여부","길탐색2번사용여부","길탐색3번사용여부","길탐색4번사용여부","길탐색5번사용여부","자동재접속사용여부","힐링포션사용여부", "HP마을귀환사용여부", "리메듐사용여부", "마나포션사용여부", "MP마을귀환사용여부", "브렐사용여부", "식빵사용여부", "식빵구매여부", "골드바판매여부", "골드바구매여부", "대화사용", "명상사용", "더블어택사용", "체력향상사용", "민첩향상사용", "활방어사용", "마력향상사용", "마법방어향상사용", "3번", "4번", "5번", "6번", "7번", "8번", "은행넣기활성화", "소각활성화","아템먹기여부","자동이동여부", "훔치기사용", "훔쳐보기사용", "Sense사용", "자동사냥여부", "무기사용여부","특오자동교환여부","행깃구매여부","라깃구매여부","독침사용","현혹사용","폭검사용","무기공격사용","집중사용","회피사용","몸통찌르기사용","리메듐사용","라리메듐사용","엘리메듐사용","쿠로사용","빛의갑옷사용","공포보호사용","다라사용","브렐사용","브레마사용","물의갑옷사용","감속사용","마스사용","라크사용","번개사용","브리스사용","파스티사용","슈키사용","클리드사용","스톤스킨사용","파라스사용","베네피쿠스사용","저주사용","자동파티여부", "포레스트네자동대화","RemoveArmor사용","좀비몹감지", "위치고정", "배경제거", "캐릭제거","버스기사모드","나프사용","제작이동","자동그레이드","무기자동수리여부","사냥터자동복귀여부","재접속알림설정","인벤꽉참알림설정","체력저하알림설정","마을귀환알림설정","그레이드알림설정","상인어빌알림설정","맵변경알림설정"]
SpellList := ["나프", "마스","리메듐","라리메듐","엘리메듐","쿠로","빛의갑옷","공포보호","다라","브렐","브레마","물의갑옷","감속","라크","번개","브리스","파스티","슈키","클리드","스톤스킨","파라스","베네피쿠스","저주"]
DropDownList := ["오란의깃마을","길탐색1번목적지", "길탐색2번목적지", "길탐색3번목적지", "길탐색4번목적지", "길탐색5번목적지", "오란의깃단축키", "길탐색책단축키", "메인캐릭터서버", "메인캐릭터순서", "힐링포션사용단축키", "마나포션사용단축키", "식빵사용단축키", "식빵구매마을" ,"지침서", "오란의깃사용단축키", "포레스트네자동대화딜레이","CurrentMode","링단축키","사냥터이름"]
EditList := ["원거리타겟아이디","리메듐타겟아이디","힐링포션사용제한", "HP마을귀환사용제한", "MP마을귀환사용제한", "리메듐사용제한", "마나포션사용제한", "브렐사용제한", "식빵사용제한", "MP마을귀환사용여부", "넣을아이템","Multiplyer","NPC_MSG_ADR" ,"마지막사냥장소", "수련용길탐색딜레이", "NPC대화딜레이", "MoveSpeed", "게임배속", "특수리메듐타겟OID","수동레벨기입","수리소야이름","수리소야아이템순서","수리소야아이템갯수", "ChatID"]
공격어빌 := ["격투", "봉", "검", "창", "활", "스태프", "현금", "도", "도끼", "단검"]
마통작마법 := ["리메듐","엘리메듐","라리메듐","브렐","마스","마하","엘","다뉴"]
CommonSkillList := ["대화","명상"]
NormalSkillList := ["더블어택","체력향상","민첩향상","활방어","마력향상","마법방어향상","회피", "집중"]
무바여부 := 0
TargetSkillList := ["훔치기","훔쳐보기","Sense","현혹","폭검","독침","무기공격"]
SkillListA := ["훔치기","훔쳐보기","Sense","현혹","폭검","독침","무기공격","더블어택","체력향상","민첩향상","활방어","마력향상","마법방어향상","집중","회피","대화","명상","몸통지르기","RemoveArmor"]
SelectedMummy2 := ""
;메모리검색용
removeCookiesScript := "
(
    document.cookie.split(';').forEach(function(c) {
        document.cookie = c.trim().split('=')[0] + '=;expires=Thu, 01 Jan 1970 00:00:00 UTC;path=/';
    });
    '쿠키가 삭제되었습니다.'
)"
global HourlyIncrease := 0
global PrevCheckUPHP := 0
global LastCheckTime := ProgramStartTime  ; 마지막 1시간 체크 시간
global Myloute
Global 상승어빌주소, randKey, 중복어빌카운트, countsignal
Global 9번사용 = 1, FPcount := 0, Aloute, Bloute,Cloute, 랜덤감응 = 0
Global WantedItemlength := 0
Global WantedMonsters := []
Global DisWantedMonsters := []
Global WantedItems := []
Global BlackList := []
Global MonsterList := []
Global itemList := []
Global PlayerList := []
Global 파라스타이머시작, 파라스타이머카운트, 파라스타이머값 := 3600, 파라스대기값 := 600000 ; 10분을 밀리초로 변환
Global MoveSpeed, Speed:=300, randomX, itemValue

Global 쿨:=100, 수리카운트, 감응감응=0, 포남입장시간, ReadHitOID := 0
Global 동파실패, 서파실패, 포북가자, 저장완료, OID이상=0,차원이동감응,적용날짜,S적용날짜 ; OID제어 변수
Global ChatID,TTM,TMessage,동파감응시간셋팅,서파감응시간셋팅
Global 게임배속, 배속, mem
global jelan,jPID,jTitle,pwb,MapNumber,RunDirect,NowDate,Version,lov,identt,bann,byte,bytes
global Location,jjc,MsgMacro,State,Inven,Buy,Repair,Ras,SelectRas,Map,AAD,MapSize,GAD,Weapon,Chat,Attack,Mount,NPCMenu,AAS,PosX,PosY,MovePosX,MCC,BAI,MovePosY,NowHP,HCC,AAI,MaxHP,NowMP,MaxMP,NowFP,MaxFP,Gold,Gold1,AGI,FormNumber,NPCMsg,NPCMenuBuyPosX,SCC,CTC,NPCMenuBuyPosY,DCC,NPCMenuRepairPosX,BAD,NPCMenuRepairPosY,rCTC,AbilityNameADD,SSC,AbilityValueADD,BAS,AbilityName,SSS,AbilityValue,Moving,Slot1Ability,GAI,SST,Slot2Ability,Slot3Ability,GAS,Slot4Ability,HPPercent,MPPercent,FPPercent,Shield,NowInven,invenstack = 0,invenError = 0,StatePosX,StatePosY,CheckFirstHP,CheckUPHP,RunningTime,ChangeValue,MagicN,CritHP,CritMP,Get_CharOID,CharID_1,CharID_2,CharID_3,CharID_4,CharID_5,CharID_6,ChangeValue,pP1,pP2,pP3,pP4,pP5,pP6,P1,P2,P3,P4,P5,P6,loady,ProgramStartTime,RPST,RPST,BasicWValue0,BasicWValue1,BasicWValue2,BasicWValue3,BWValue0,BWValue1,BWValue2,BWValue3,RMNS,MNS,RMNN,Slot3MN,Slot4MN,Slot5MN,Slot6MN,Slot7MN,Slot8MN,Slot3Magic,Slot4Magic,Slot5Magic,Slot6Magic,Slot7Magic,Slot8Magic,MLimit,incinerateitem,RowNumber,inciNumber = 1,inciItem,CCD,CheckPB,CheckPN,newTime1,nowtime1,PNnowtiem,PNnowTime1,PNnewtime1,PNnewTime,nowtime,RCC,pbtalkcheck,pbtalkcheck1,pbtalkcheck2,UAD,pntalkcheck2,pntalkcheck1
global Slot1Ability,Slot2Ability,Slot3Ability,Slot4Ability,Slot5Ability,Slot6Ability,Slot7Ability,Slot8Ability,Slot9Ability,Slot10Ability,Slot11Ability,Slot12Ability,Slot13Ability,Slot14Ability,Slot15Ability,Slot16Ability,Slot17Ability,Slot18Ability,Slot19Ability,Slot20Ability,Slot21Ability,Slot22Ability,Slot23Ability,Slot24Ability,Slot25Ability,Slot26Ability,Slot27Ability,Slot28Ability,Slot29Ability,Slot30Ability,Slot31Ability,Slot32Ability,Slot33Ability,Slot34Ability,Slot35Ability,Slot36Ability,Slot37Ability,Slot38Ability,Slot39Ability,Slot40Ability,Slot41Ability,Slot42Ability,Slot43Ability,Slot44Ability,Slot45Ability,Slot46Ability,Slot47Ability,Slot48Ability,Slot49Ability,Slot50Ability,Slot51Ability,Slot52Ability,Slot53Ability,Slot54Ability,Slot55Ability,Slot56Ability
global Slot1AN,Slot2AN,Slot3AN,Slot4AN,Slot5AN,Slot6AN,Slot7AN,Slot8AN,Slot9AN,Slot10AN,Slot11AN,Slot12AN,Slot13AN,Slot14AN,Slot15AN,Slot16AN,Slot17AN,Slot18AN,Slot19AN,Slot20AN,Slot21AN,Slot22AN,Slot23AN,Slot24AN,Slot25AN,Slot26AN,Slot27AN,Slot28AN,Slot29AN,Slot30AN,Slot31AN,Slot32AN,Slot33AN,Slot34AN,Slot35AN,Slot36AN,Slot37AN,Slot38AN,Slot39AN,Slot40AN,Slot41AN,Slot42AN,Slot43AN,Slot44AN,Slot45AN,Slot46AN,Slot47AN,Slot48AN,Slot49AN,Slot50AN,Slot51AN,Slot52AN,Slot53AN,Slot54AN,Slot55AN,Slot56AN
Global MonsterPID
global Pro_version,NPC_STARTversion
global 동파, 서파
global A리노아,A동파,A서파,B리노아,B동파,B서파,G리노아,G동파,G서파,A길잃파,B길잃파,G길잃파
global SA리노아,SA동파,SA서파,SB리노아,SB동파,SB서파,SG리노아,SG동파,SG서파,SA길잃파,SB길잃파,SG길잃파
global monitor
global Start_Time
Global 아이템갯수 := {}
Global 소지아이템리스트업데이트딜레이 := A_TickCount
Global RasCount,정보카운트,결정갯수,나무갯수,가루갯수,골드바카운트
global 게임시작x,게임시작y
global 기존버젼,신버젼,패치
global 좌표고정
global 플러그,플러그타이틀
global 점검 = 0
global 업데이트체크,S업데이트체크,골바정보=0
Global StartTime := A_TickCount
Global 소지아이템리스트업데이트딜레이 := A_TickCount
Global RunThreadCounter := A_TickCount
Global PT_Delays := A_TickCount
Global NSK_Counts := A_TickCount
Global NPC_TALK_DELAYCOUNT := A_TickCount
Global Read_Memory_Count := A_TickCount
Global 서버상태,CountPortal,차원
Global 차원체크
Global 빵,몸찌방지,식빵갯수,절반FP,몸찌이동인식 = 0
global 실행창위치 = 0 , 시작탭사이즈 = 0
global 무바중 = 0
global Reserver,파라스감지,수호천사방지,파라스방해감지,인연방지
global 몹찾기 = 0
Global 현혹번호, 폭검번호, 독침번호, 무기공격번호, 타겟팅랜덤
Global 대화번호, 명상번호, 더블어택번호, 체력향상번호, 집중번호, 회피번호, 몸통지르기번호, 리무브아머번호, 민첩향상번호, 활방어번호, 마력향상번호, 마력방어번호
Global 빛의갑옷번호, 물의갑옷번호, 스톤스킨번호
Global Run타겟,타겟number := 0
Global 포북시작, gui_Startmap, 현재차원
Global 포북생콩섭취, 포남생콩섭취, 가방수량체크
Global VMRESET
Global 상승체력평균값, 상승체력평균치
Global jean, Cap, Top, Shoes
Global 호출대상
global filePath
Global 무기종류, 머미맵선택
Global RecentWeapons := []
Global Attacking , AttackingCount
TTSTOP := 0
WinGetTitle, 플러그타이틀, ahk_exe NexonPlug.exe
WinGet, 플러그, PID , ahk_exe NexonPlug.exe
Gui, -MaximizeBox -MinimizeBox
SysGet,monitor,16
Gui, Font
Gui, Font, s8 ,Arial
Gui, Color, FFFFFF  ; 화면을 흰색(#FFFFFF)으로 설정
Gui, Add, Tab, x0 y4 w750 h660, 기본설정|Utility|Grade+|NPC|
Gui, Font
Gui, Font, s8 ,
Gui, Add, StatusBar, , 시작대기중
SB_SetParts(160,200,360)
DetectHiddenWindows, ON
Gui, Add, Checkbox, x590 y2 w110 h18  vGui_relogerror, 재접속 오류
gui, tab, 기본설정
Gui, Font
Gui, Font, s8 ,Arial
Gui, Font
Gui, Font, s8  Bold,Arial
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x10 y30 w330 h145 v로그인, 로그인 설정
Gui, Font
Gui, Font, s8
Gui, Add, Text, x30 y55 +Left vGui_NexonID2 , ID   :
Gui, Add, Text, x30 y80 +Left vGui_NexonPassWord2 , PW   :
Gui, Add, Edit, x110 y52 w140 +Left vGui_NexonID
Gui, Add, Edit, x110 y77 w140 +Left Password vGui_NexonPassWord
Gui, Add, Text, x30 y105 +Left, 서버   :
Gui, Add, Text, x30 y130 +Left, 캐릭터번호   :
Gui, Add, Text, x30 y152 +Left, 로그인방식   :
Gui, Add, DropDownList, x110 y105 w140 +Left vGui_Server, 엘|테스
Gui, Add, DropDownList, x110 y128 w140 +Left vGui_CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Add, DropDownList, x110 y150 w140 +Left vGui_Login gCheck로그인, 인터넷|넥슨플러그|홈페이지클릭[구글]
Gui, Font
Gui, Font, s10 Bold
Gui, Add, Button, x260 y51 w70 h45 +Center vGui_StartButton gStart, 시작
Gui, Font
Gui, Font, s10 Bold
Gui, Add, Button, x260 y111 w70 h45 +Center vGui_Resetting gResetting, 리셋
Gui, Font
Gui, Font, s8 cGreen Bold
Gui, Add, Text, x30 y50 h20 v설명, [컨트롤 + G로 로그인 클릭 좌표를 지정]
Gui, Add, Text, x30 y50 h20 v설명2, [컨트롤 + G로 시작 좌표를 지정]
Gui, Font
Gui, Font, s10 cGreen Bold
Gui, Font
Gui, Font, s8 cGreen Bold
Gui, Add, Text, x30 y70 h20 v넥슨x, 좌표X :
Gui, Add, Text, x140 y70 h20 v넥슨y, 좌표Y :
Gui, Add, Text, x80 y70 w50 h20 v좌표x, Ctrl
Gui, Add, Text, x190 y70 w50 h20 v좌표y, G
Gui, Font
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x10 y180 w330 h105, HP 설정
Gui, Font
Gui, Font, s8
Gui, Add, Checkbox, x30 y196 w15 h15 gCheckUseHPExit vGui_CheckUseHPExit
Gui, Add, Checkbox, x30 y218 w15 h15 gCheckUseHPPortal vGui_CheckUseHPPortal
Gui, Add, Checkbox, x30 y240 w15 h15 gCheckUseHPLimited vGui_CheckUseHPLimited
Gui, Add, Text, x55 y198 +Left, 체력이
Gui, Add, Text, x55 y220 +Left, 체력이
Gui, Add, Text, x55 y242 +Left, 체력이
Gui, Add, Edit, x95 y193 w50 +Right Limit6 number Disabled cRed vGui_HPExit, 0
Gui, Add, Edit, x95 y215 w50 +Right Limit6 number Disabled cRed vGui_HPPortal, 0
Gui, Add, Edit, x95 y237 w50 +Right Limit6 number Disabled cRed vGui_HPLimited, 0
Gui, Add, Text, x150 y198 +Left, 이하시 종료(재접속 하지 않음)
Gui, Add, Text, x150 y220 +Left, 이하시 차원이동
Gui, Add, Text, x150 y242 +Left, 까지 상승시 종료(재접속 하지 않음)

Gui, Add, Checkbox, x30 y262 w15 h15 gCheckUseHPHospital vGui_CheckUseHPHospital
Gui, Add, Text, x55 y264 +Left, 체력이
Gui, Add, Edit, x95 y259 w50 +Right Limit6 number Disabled cRed vGui_HPHospital, 0
Gui, Add, Text, x150 y264 +Left, 까지 낮아졌을 경우 병원가기

Gui, Font
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x10 y288 w330 h105, 무바 설정
Gui, Font
Gui, Font, s8
Gui, Add, Radio, x30 y304 h15 Checked vGui_1Muba gSelectMuba, 1무바
Gui, Add, Radio, x130 y304 h15 vGui_2Muba gSelectMuba, 2무바
Gui, Add, Radio, x230 y304 h15 vGui_3Muba gSelectMuba, 3무바
Gui, Add, Radio, x30 y324 h15 vGui_2ButMuba gSelectMuba, 2벗무바
Gui, Add, Radio, x130 y324 h15 vGui_3ButMuba gSelectMuba, 3벗무바
Gui, Add, Radio, x230 y324 h15 vGui_4ButMuba gSelectMuba, 4벗무바
Gui, Add, Text, x35 y345 +Left, 1번 무기어빌
Gui, Add, Text, x135 y345 +Left, 2번 무기어빌
Gui, Add, Text, x235 y345 +Left, 3번 무기어빌
Gui, Add, DropDownList, x30 y360 w80 +Left vGui_Weapon1 gSelectAbility, 검|단검|도|도끼|대검|대도|창, 특수창|봉, 해머|현금|활|거대검|거대도|거대도끼|양손단검|양손도끼|스태프|지하탐색|발굴
Gui, Add, DropDownList, x130 y360 w80 +Left Disabled vGui_Weapon2 gSelectAbility, 검|단검|도|도끼|대검|대도|창, 특수창|봉, 해머|현금|활|거대검|거대도|거대도끼|양손단검|양손도끼|스태프|지하탐색|발굴
Gui, Add, DropDownList, x230 y360 w80 +Left Disabled vGui_Weapon3 gSelectAbility, 검|단검|도|도끼|대검|대도|창, 특수창|봉, 해머|현금|활|거대검|거대도|거대도끼|양손단검|양손도끼|스태프|지하탐색|발굴

Gui, Font
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x10 y395 w330 h55, 기타 상태
Gui, Font
Gui, Font, s8
Gui, Add, Text, x20 y420 +Left, [라스의깃]  :
Gui, Add, Edit, x95 y417 w30 +Right Limit3 number vGui_RasCount, 0
Gui, Add, Text, x130 y420 +Left, 개
Gui, Font
Gui, Font, s9
Gui, Add, Button, x142 y412 w16 h13 +Center  glagitu, ∧
Gui, Add, Button, x142 y428 w16 h13 +Center  glagitd, ∨
Gui, Font
Gui, Font, s8
Gui, Add, Text, x168 y420 w40, [정보] :
Gui, Add, Edit, x203 y417 w30 +Right Limit3 number vGui_정보Count, 0
Gui, Add, Text, x237 y420 w30, 개
Gui, Font
Gui, Font, s8
Gui, Add, Text, x250 y420 w40, [골바] :
Gui, Add, Edit, x285 y417 w30 +Right Limit3 number vGui_NowGoldbar, 0
Gui, Add, Text, x320 y420 w10, 개

Gui, Font
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x10 y460 w330 h140, 어빌리티
Gui, Font
Gui, Font, s8
Gui, Add, Text, x90 y485 w55 +Center, 격투
Gui, Add, Text, x150 y485 w55 +Center, 1번
Gui, Add, Text, x210 y485 w55 +Center, 2번
Gui, Add, Text, x265 y485 w55 +Center, 3번
Gui, Add, Text, x20 y510, 어빌리티
Gui, Add, Text, x20 y535, 어빌레벨
Gui, Add, Text, x20 y560, 그레이드
;Gui, Add, Text, x20 y578, TagetOID


Gui, Font, s8
Gui, Add, Text, x90 y510 w55 +Center vGui_BasicWName0
Gui, Add, Text, x150 y510 w55 +Center vGui_BasicWName1
Gui, Add, Text, x210 y510 w55 +Center vGui_BasicWName2
Gui, Add, Text, x265 y510 w55 +Center vGui_BasicWName3
Gui, Add, Text, x90 y535 w55 +Center vGui_BasicWValue0
Gui, Add, Text, x150 y535 w55 +Center vGui_BasicWValue1
Gui, Add, Text, x210 y535 w55 +Center vGui_BasicWValue2
Gui, Add, Text, x265 y535 w55 +Center vGui_BasicWValue3
Gui, Add, Text, x90 y560 w55 +Center vGui_Grade0
Gui, Add, Text, x150 y560 w55 +Center vGui_Grade1
Gui, Add, Text, x210 y560 w55 +Center vGui_Grade2
Gui, Add, Text, x265 y560 w55 +Center vGui_Grade3
;Gui, ADD, Edit, x100 y573 w100 Center vCallTarget Disabled, TargetName
;Gui, ADD, Edit, x205 y573 w100 +Center vMonsterTargetPID Disabled, TargetOID


Gui, Font
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x350 y30 w350 h150, 포남 설정
Gui, Font
Gui, Font, s8
Gui, Add, Text, x370 y50 +Left, 만드 체크
Gui, Add, Checkbox, x450 y48 h15 vGui_EvadeMand, 주위에 만드가 있으면 다른몹 찾기
Gui, Add, Text, x370 y75 +Left, 동선 설정
Gui, Add, Radio, x450 y73 h15 Checked vGui_MoveLoute1, 기본
Gui, Add, Radio, x500 y73 h15 vGui_MoveLoute2, 구석
Gui, Add, Radio, x550 y73 h15 vGui_MoveLoute3, 위아래
Gui, Add, Radio, x610 y73 h15 vGui_MoveLoute4, 아래만
Gui, Add, Text, x370 y100 +Left, 몬스터 설정
Gui, Add, Radio, x450 y98 h15 Checked vGui_Ent gCheckMob, 엔트
Gui, Add, Radio, x500 y98 h15 vGui_Rockey gCheckMob, 록키
Gui, Add, Radio, x570 y98 h15 vGui_EntRockey gCheckMob, 엔트록키
Gui, Add, Radio, x450 y123 h15 vGui_Mand gCheckMob, 만드
Gui, Add, Radio, x400 y123 h15 vGui_MobMagic gCheckMob, 마법
Gui, Add, Radio, x500 y123 h15 vGui_AllMobAND gCheckMob, 전체AND
Gui, Add, Radio, x570 y123 h15 vGui_AllMobOR gCheckMob, 전체OR
Gui, Add, Text, x370 y155 +Left, 전체 몬스터 선택시 어빌
Gui, Add, Edit, x495 y152 w35 +Right Limit4 number Disabled vGui_AllMobLimit, 9200
Gui, Add, Text, x535 y155 +Left v포남설정, 이상이면 만드만 공격

Gui, Font
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x350 y185 w350 h145, 공통 설정
Gui, Font
Gui, Font, s8
Gui, Add, Text, x370 y203 +Left, 체작 장소
Gui, Add, Radio, x440 y200 h15 Checked vGui_HuntAuto gSelectHuntPlace, 자동
Gui, Add, Radio, x495 y200 h15 vGui_HuntPonam gSelectHuntPlace, 포남
Gui, Add, Radio, x550 y200 h15 vGui_HuntPobuk gSelectHuntPlace, 포북
Gui, Add, Radio, x605 y200 h18 vGui_Huntmummy gSelectHuntPlace, 머미/포북
Gui, Add, Text, x370 y225 w25, 격투
Gui, Add, Text, x435 y225 w25, 1번
Gui, Add, Text, x500 y225 w25, 2번
Gui, Add, Text, x565 y225 w25, 3번
Gui, Add, Edit, x395 y222 w35 +Center Limit5 number vGui_LimitAbility0, 9200
Gui, Add, Edit, x460 y222 w35 +Center Limit5 number vGui_LimitAbility1, 9200
Gui, Add, Edit, x525 y222 w35 +Center Limit5 number vGui_LimitAbility2, 9200
Gui, Add, Edit, x590 y222 w35 +Center Limit5 number vGui_LimitAbility3, 9200


Gui, Add, Text, x370 y250 +Left, 파티 설정
Gui, Add, Radio, x450 y247 h15 vGui_PartyOn g원격파티사용 Checked, ON
Gui, Add, Radio, x510 y247 h15 vGui_PartyOff g원격파티사용, OFF
Gui, ADD, Edit, x565 y247 h15 w120 Disabled +Center v파라스타이머, 다시 포남까지 0:00
Gui, Add, Text, x370 y270 +Left, 자동 그레이드
Gui, Add, Checkbox, x450 y268 h15 vGui_Grade
Gui, Add, Text, x480 y270 +Left, [묵-찌-빠!]
Gui, Add, Checkbox, x543 y268 h15 vGui_MGB
Gui, Add, Text, x370 y290 +Left, 강제 그레이드
Gui, Add, Button, x535 y284 w40 h21 +Center vGui_FG gforcegrade, 실행
Gui, Add, DropDownList, x450 y285 w80 +Left vGui_forceweapon, 선택|격투|검|단검|도|도끼|거대도끼|대검|대도|창, 특수창|봉, 해머|현금|활|거대검|거대도|양손단검|양손도끼|스태프
Gui, Add, Text, x370 y310 +Left, 차원 설정
Gui, Add, Radio, x450 y310 h15 Checked v랜덤차원 g차원체크, 랜덤
Gui, Add, Radio, x510 y310 h15 v알파차원 g차원체크, 알파
Gui, Add, Radio, x570 y310 h15 v베타차원 g차원체크, 베타
Gui, Add, Radio, x625 y310 h15 v감마차원 g차원체크, 감마


Gui, Font
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x350 y340 w350 h178 , 캐릭터 상태
Gui, Font
Gui, Font, s8
Gui, Add, Text, x360 y365 w60 +Right, 캐릭터명 :
Gui, Add, Text, x360 y387 w60 +Right, 진행상황 :
Gui, Add, Text, x360 y408 w60 +Right, 지역 :
Gui, Add, Text, x360 y428 w60 +Right, 갈리드 :
Gui, Add, Text, x360 y453 w60 +Right, HP :
Gui, Add, Text, x360 y475 w60 +Right, FP :
Gui, Add, Text, x360 y495 w60 +Right, 좌표 :
Gui, Add, Text, x445 y365 w220 vGui_CharName
Gui, Add, Text, x445 y387 w220 vGui_NowState
Gui, Add, Text, x445 y408 w220 vGui_NowLocation
Gui, Add, Text, x445 y428 w220 vGui_NowGold
Gui, Add, Text, x445 y453 w220 cRed vGui_NowHP
Gui, Add, Text, x445 y475 w220 cBlue vGui_NowFP
Gui, Add, Text, x445 y495 w230 v좌표,

Gui, Font
Gui, Font, s8 cGreen Bold
Gui, Add, Text, x10 y610 w330 h30, 단축키 설정 :
Gui, Font
Gui, Font, s7 Bold,Arial
Gui, Add, Text, x90 y608 w100 h30, [1 ~ 3번 무기넣어]
Gui, Font
Gui, Font, s8
Gui, Add, checkbox, x190 y600 w50 h30 v4번사용,4번
Gui, Add, checkbox, x240 y600 w50 h30 v5번사용,5번
Gui, Add, checkbox, x290 y600 w50 h30 v6번사용,6번
Gui, Add, checkbox, x340 y600 w40 h30 v7번사용,7번
Gui, Add, checkbox, x390 y600 w40 h30 v8번사용,8번
Gui, Font
Gui, Font, s7 Bold,Arial
Gui, Add, Text, x440 y608 w125 h30, [9번:식빵 0번:라깃]

Gui, Font
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x350 y520 w350 h80, 기능 설정
Gui, Font
Gui, Font, s8  Bold, Arial
Gui, Add, Text, x370 y540 w220 , 원격 감응 :
Gui, Font
Gui, Font, s9 cBlue Bold
Gui, Add, Radio, x440 y540 w60 vGui_KON, ON
Gui, Font
Gui, Font, s9 cRed Bold
Gui, Add, Radio, x500 y540 w60 vGui_KOFF, OFF

Gui, Font
Gui, Font, s8  Bold, Arial
Gui, Add, Text, x560 y540 w110 , [ ↓ 템뿌 방지 설정 ]
Gui, Font
Gui, Font, s9 cBlue Bold
Gui, Add, Radio, x560 y570 w60 vProtect_AmorON, ON
Gui, Font
Gui, Font, s9 cRed Bold
Gui, Add, Radio, x620 y570 w50 vProtect_AmorOFF, OFF

Gui, Font
Gui, Font, s8  Bold, Arial
Gui, Add, Text, x370 y570 w150 , 줍줍/소각 :
Gui, Font
Gui, Font, s9 cBlue Bold
Gui, Add, Radio, x440 y570 w60 vGui_jjON, ON
Gui, Font
Gui, Font, s9 cRed Bold
Gui, Add, Radio, x500 y570 w50 vGui_jjOFF, OFF


Gui, Font
Gui, Font, s8  Bold, Arial
Gui, Add, Button, x550 y605 w60 Disabled vGui_StopButton g일시정지, 일시정지
Gui, Add, Button, x550 y605 w60 Disabled vGui_RestartButton g재시작, 재시작
Gui, Add, Button, x620 y605 w60 +Center g종료, 종료
GuiControl,HIDE,Gui_RestartButton

Gui, Tab, Utility

Gui, Font,
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x10 y250 w360 h155, 스킬 설정
Gui, Font
Gui, Font, s8
Gui, Add, checkbox, x30 y265 w80 h20 v대화사용, 대화
Gui, Add, checkbox, x30 y285 w80 h20 v명상사용, 명상
Gui, Add, checkbox, x30 y305 w80 h20 v더블어택사용, 더블어택
Gui, Add, checkbox, x30 y325 w80 h20 v체력향상사용, 체력향상
Gui, Add, checkbox, x30 y345 w80 h20 v집중사용, 집중
Gui, Add, checkbox, x30 y365 w80 h20v회피사용, 회피
Gui, Add, checkbox, x120 y265 w80 h20 v몸통지르기사용, 몸통지르기
Gui, Add, checkbox, x120 y285 w80 h20 v리무브아머사용, 리무브아머
Gui, Add, checkbox, x120 y305 w80 h20 v민첩향상사용, 민첩향상
Gui, Add, checkbox, x120 y325 w80 h20 v활방어사용, 활방어
Gui, Add, checkbox, x120 y345 w80 h20 v마력향상사용, 마력향상
Gui, Add, checkbox, x120 y365 w80 h20 v마력방어사용, 마력방어
Gui, Add, checkbox, x230 y265 w120 h20 v현혹사용, 현혹
Gui, Add, checkbox, x230 y285 w120 h20 v폭검사용, 폭검
Gui, Add, checkbox, x230 y305 w120 h20 v독침사용, 독침
Gui, Add, checkbox, x230 y325 w120 h20 v무기공격사용, 무기공격

Gui, Font,
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x375 y250 w305 h70, 가방 설정
Gui, Font
Gui, Font, s8,Arial
Gui, Add, Text, x393 y298 , [인벤토리 줍줍 조정]    :
Gui, Add, DropDownList, x525 y295 h100 w120 +Center v가방설정 g가방수량설정, 줍줍끄기|40개 초과시 정지||45개 초과시 정지|제한없음
Gui, Add, Text, x393 y270 , [8번 생콩 먹기 조정]    :
Gui, Add, DropDownList, x525 y268 h100 w120 +Center v포남생콩설정 g생콩설정, FP 500이하에 먹기||상시사용으로 먹기
Gui, Font,
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x375 y325 w310 h80, 텔레그램 알림 설정
Gui, Font
Gui, Font, s7.5,Arial
Gui, Add, Text, x395 y340 w135 h40, @HelanciaBot 친구추가`n후 아이디 입력하세요
gui, add, Edit, x395 y370 w120 h20 vChatID, 아이디입력
gui, add, button, x535 y345 w120 h40 g텔레그램메시지사용법, 보내기사용법

Gui, Font,
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x235 y30 w220 h205, 마법 설정
Gui, Font
Gui, Font, s8
Gui, Add, Text, x260 y57 +Left, 체크시 원격마법을 사용합니다.
Gui, Add, Checkbox, x240 y55 w15 h15 gCheckUseMagic vGui_CheckUseMagic c000000
Gui, Add, Text, x150 y220 +Left
Gui, Add, Text, x240 y80 +Left, HP가
Gui, Add, Edit, x270 y78 +Left w55 h15 Limit7 number cRed vGui_CHP, 0
Gui, Add, Text, x328 y80 +Left, 되면 리메듐 사용

Gui, Add, Text, x240 y103 +Left, 스펠슬롯
Gui, Add, DropDownList, x295 y100 w80 +Left vGui_MagicNStack, 3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20
Gui, Add, Text, x378 y103 +Left, 번 까지 사용
Gui, Add, Text, x240 y128 +Left, 스펠슬롯 1번 (엘)리메듐을 고정 해 주세요
Gui, Add, Text, x240 y153 +Left, 스펠슬롯 2번 브렐을 고정 해 주세요
Gui, Add, Button, x240 y183 w200 h30 g원격마법설명서, 사용법

Gui, Font,
Gui, Font, s8 cGreen Bold
Gui, Add, Button, x160 y50 w60 h20 g원격파티설명서, 사용법
Gui, Add, Checkbox, x15 y55 w15 h15 vGui_CheckUseParty g원격파티사용
Gui, Add, GroupBox, x10 y30 w220 h205, 파티 설정
Gui, Font
Gui, Font, s8
Gui, Add, Text, x35 y57 +Left, 체크시 원격파티 사용
Gui, Add, Text, x15 y78, 파티장
Gui, add, Edit, x65 y75 w100 vName1
Gui, Add, DropDownList, x178 y75 w38 +Left vGui_P1CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Add, Text, x15 y103, 파티원
Gui, add, Edit, x65 y100 w100 vName2
Gui, Add, DropDownList, x178 y100 w38 +Left vGui_P2CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Add, Text, x15 y128, 파티원
Gui, add, Edit, x65 y125 w100 vName3
Gui, Add, DropDownList, x178 y125 w38 +Left vGui_P3CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Add, Text, x15 y153, 파티원
Gui, add, Edit, x65 y150 w100 vName4
Gui, Add, DropDownList, x178 y150 w38 +Left vGui_P4CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Add, Text, x15 y178, 파티원
Gui, add, Edit, x65 y175 w100 vName5
Gui, Add, DropDownList, x178 y175 w38 +Left vGui_P5CharNumber, 1|2|3|4|5|6|7|8|9|10
Gui, Add, Text, x15 y203, 파티원
Gui, add, Edit, x65 y200 w100 vName6
Gui, Add, DropDownList, x178 y200 w38 +Left vGui_P6CharNumber, 1|2|3|4|5|6|7|8|9|10


Gui, Font,
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x460 y30 w220 h205, 체잠 소각 설정
Gui, Font
Gui, Font, s8
Gui, Add, ListView, x465 y50 h120 w210 -LV0x20  -Multi -HDR v포프레스네소각, 아이템
Gui, add, Edit, x465 y210 w110 vGui_incinerateitem
Gui, Add, Button, x465 y175 w200 h30 g소각갱신, 소각INI새로고침
Gui, Add, Button, x580 y210 w40 h20 gAddincinerate, 추가
Gui, Add, Button, x623 y210 w40 h20 gDelincinerate, 삭제
LV_ModifyCol(1, 320)

Gui, Font,
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x10 y410 w670 h175, Listview 설정
Gui, Font
Gui, Font, s8
Gui,Font
Gui, Font, s8 ,Arial
Gui, Add, Listview, x35 y430 w280 h120 -Multi v어빌리티리스트, 순서 | 어빌명 | 글드 | 어빌레벨
Gui, listview , 어빌리티리스트
LV_ModifyCol(1,"38 Right")
LV_ModifyCol(2,"90 Center")
LV_ModifyCol(3,"55 Center")
LV_ModifyCol(4,"87 Left")
Gui, Add, Button, x120 y555 w90 g어빌리티리스트갱신, 어빌목록스캔
Gui, Font
Gui, Font, s8,Arial

Gui, Add, ListView, x350 y430 w300 h120 v마법리스트,순서 | 스펠명 | 글드 | 스펠레벨
LV_ModifyCol(1,"38 Right")
LV_ModifyCol(2,"90 Center")
LV_ModifyCol(3,"75 Center")
LV_ModifyCol(4,"87 Left")
Gui, Add, Button, x470 y555 w90 g마법갱신,마법목록스캔


Gui, Tab, Grade+
Gui, Font,
Gui, Font, s8 cGreen Bold
Gui, Add, text, x20 y30 w335 h510,[어빌리티 그레이드]
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
Gui, Font,
Gui, Font, s8 cGreen Bold
Gui, Add, text, x20 y545 w335 h510,[스펠 그레이드]
Gui, Font
Gui, Font, s8
Gui, Add, Text, x68 y570 +Center, 스펠 3슬롯
Gui, Add, Text, x166 y570 +Center, 스펠 4슬롯
Gui, Add, Text, x264 y570 +Center, 스펠 5슬롯
Gui, Add, Text, x362 y570 +Center, 스펠 6슬롯
Gui, Add, Text, x460 y570 +Center, 스펠 7슬롯
Gui, Add, Text, x558 y570 +Center, 스펠 8슬롯
Gui, Add, edit, x54 y590 w80 +Left Disabled vGui_MagicName3
Gui, Add, edit, x152 y590 w80 +Left Disabled vGui_MagicName4
Gui, Add, edit, x250 y590 w80 +Left Disabled vGui_MagicName5
Gui, Add, edit, x348 y590 w80 +Left Disabled vGui_MagicName6
Gui, Add, edit, x446 y590 w80 +Left Disabled vGui_MagicName7
Gui, Add, edit, x544 y590 w80 +Left Disabled vGui_MagicName8
Gui, Add, edit, x54 y610 w80 +Left Disabled vGui_MagicValue3
Gui, Add, edit, x152 y610 w80 +Left Disabled vGui_MagicValue4
Gui, Add, edit, x250 y610 w80 +Left Disabled vGui_MagicValue5
Gui, Add, edit, x348 y610 w80 +Left Disabled vGui_MagicValue6
Gui, Add, edit, x446 y610 w80 +Left Disabled vGui_MagicValue7
Gui, Add, edit, x544 y610 w80 +Left Disabled vGui_MagicValue8
Gui, Add, Checkbox, x53 y570 w15 h15 gCheckM3 vGui_MagicCheck3
Gui, Add, Checkbox, x151 y570 w15 h15 gCheckM4 vGui_MagicCheck4
Gui, Add, Checkbox, x249 y570 w15 h15 gCheckM5 vGui_MagicCheck5
Gui, Add, Checkbox, x347 y570 w15 h15 gCheckM6 vGui_MagicCheck6
Gui, Add, Checkbox, x445 y570 w15 h15 gCheckM7 vGui_MagicCheck7
Gui, Add, Checkbox, x543 y570 w15 h15 gCheckM8 vGui_MagicCheck8

gui, tab, NPC
Gui, Font, s8  Bold,Arial
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x10 y30 w300 h355 , NPCOID설정
Gui, Font
Gui, Font, s8
Gui, Add, Text, x30 y60 +Left , 알파 리노아   :
Gui, Add, edit, x150 y50 w120 Disabled va리노아 , 0
Gui, Add, Text, x30 y80 +Left , 알파 동쪽파수꾼   :
Gui, Add, edit, x150 y75 w120 Disabled va동파, 0
Gui, Add, Text, x30 y105 +Left , 알파 서쪽파수꾼   :
Gui, Add, edit, x150 y100 w120 Disabled vA서파, 0
Gui, Add, Text, x30 y130 +Left  , 베타 리노아  :
Gui, Add, edit, x150 y125 w120 Disabled vB리노아, 0
Gui, Add, Text, x30 y155 +Left  , 베타 동쪽파수꾼  :
Gui, Add, edit, x150 y150 w120 Disabled vB동파, 0
Gui, Add, Text, x30 y180 +Left  , 베타 서쪽파수꾼  :
Gui, Add, edit, x150 y175 w120 Disabled vB서파, 0
Gui, Add, Text, x30 y205 +Left , 감마 리노아  :
Gui, Add, edit, x150 y200 w120 Disabled vG리노아, 0
Gui, Add, Text, x30 y230 +Left  , 감마 동쪽파수꾼  :
Gui, Add, edit, x150 y225 w120 Disabled vG동파, 0
Gui, Add, Text, x30 y255 +Left  , 감마 서쪽파수꾼  :
Gui, Add, edit, x150 y250 w120 Disabled vG서파, 0
Gui, Add, Text, x30 y280 +Left  , 알파 길잃은파수꾼  :
Gui, Add, edit, x150 y275 w120 Disabled vA길잃파, 0
Gui, Add, Text, x30 y305 +Left  , 베타 길잃은파수꾼  :
Gui, Add, edit, x150 y300 w120 Disabled vB길잃파, 0
Gui, Add, Text, x30 y330 +Left  , 감마 길잃은파수꾼  :
Gui, Add, edit, x150 y325 w120 Disabled vG길잃파, 0
Gui, Add, Button, x150 y350 w120 gOID리셋, NPCOID리셋
Gui, Add, Text, x30 y350 w100 vTextControl Disabled, OID : %적용날짜%
GuiControl,, TextControl, OID : %적용날짜%
Gui, Font, s8  Bold,Arial
Gui, Font, s8 cGreen Bold
Gui, Add, GroupBox, x10 y390 w300 h50 , NPC감응 상태
Gui, Add, Text, x30 y410 +Left , 감응 NPC   :
Gui, Add, edit, x100 y407 w180 v감응쿨타임 , NPC없음
Gui, Add, GroupBox, x10 y445 w300 h105, 업데이트 기록  ; 높이를 80으로 조정
Gui, Font
Gui, Font, s8
Gui, Add, ListView, x15 y465 w290 h70 vUpdateLogGrid, 날짜|기록  ; 리스트뷰 추가
LV_ModifyCol(1, 85)  ; 첫 번째 열(날짜) 너비 조정
LV_ModifyCol(2, 200) ; 두 번째 열(기록) 너비 조정
LV_Add("", "25.02.09/PM09:58", "배포용 업데이트 시작됨")
LV_Add("", "25.02.09/PM09:58", "업데이터 로그 제작")
LV_Add("", "25.02.09/PM10:40", "'엘의축복포션'없이 사냥 시 알람")
LV_Add("", "25.02.09/PM11:39", "실행 상태로 설정 저장 시 이름모를창 자동끄기")
LV_Add("", "25.02.10/PM02:20", "무기수리시 메모리오류 1차 수정")
x_coord := 320
Gui, Font, s8  Bold,Arial
Gui, Font, s8 cGreen Bold
Gui, Add, Text, x%x_coord% y30 h15 w80, 플레이어
Gui, Font
Gui, Font, s8
Gui, Add, ListView, x%x_coord% y45 h130 w180 v플레이어리스트 +g플레이어리스트실행 +altsubmit, 분류|차원|맵이름|번호|이름|OID|X|Y|Z|주소|삭제카운트
LV_ModifyCol(1,40)
LV_ModifyCol(2,0)
LV_ModifyCol(3,0)
LV_ModifyCol(4,0)
LV_ModifyCol(5,50)
LV_ModifyCol(6,0)
LV_ModifyCol(7,30)
LV_ModifyCol(8,30)
LV_ModifyCol(9,30)
LV_ModifyCol(10,0)
LV_ModifyCol(11,0)
Gui, Font, s8  Bold,Arial
Gui, Font, s8 cGreen Bold
Gui, Add, Text, x%x_coord% y179 h15 w80, 신규플레이어
Gui, Font
Gui, Font, s8
Gui, Add, ListView, x%x_coord% y195 h100 w180 v신규플레이어리스트 +altsubmit, 분류|차원|맵이름|번호|이름|OID|X|Y|Z|주소
LV_ModifyCol(1,40)
LV_ModifyCol(2,0)
LV_ModifyCol(3,0)
LV_ModifyCol(4,0)
LV_ModifyCol(5,50)
LV_ModifyCol(6,0)
LV_ModifyCol(7,30)
LV_ModifyCol(8,30)
LV_ModifyCol(9,30)
LV_ModifyCol(10,0)
Gui, Font, s8  Bold,Arial
Gui, Font, s8 cGreen Bold
x_coord := 15 + 240*2 + 5 *2
Gui, Add, Text, x%x_coord% y30 h15 w80, 몬스터
Gui, Font
Gui, Font, s8
Gui, Add, ListView, x%x_coord% y45 h130 w180 v몬스터리스트 +g몬스터리스트실행 +altsubmit, 분류|차원|맵이름|번호|이름|OID|X|Y|Z|주소|삭제카운트|거리|이미지
LV_ModifyCol(1,40)
LV_ModifyCol(2,0)
LV_ModifyCol(3,0)
LV_ModifyCol(4,0)
LV_ModifyCol(5,50)
LV_ModifyCol(6,0)
LV_ModifyCol(7,30)
LV_ModifyCol(8,30)
LV_ModifyCol(9,30)
LV_ModifyCol(10,0)
LV_ModifyCol(11,0)
LV_ModifyCol(12,0)
LV_ModifyCol(13,0)
x_coord := 15 + 240*2 + 5 *2
Gui, Font, s8  Bold,Arial
Gui, Font, s8 cGreen Bold
Gui, Add, Text, x%x_coord% y179 h15 w80, 신규몬스터
Gui, Font
Gui, Font, s8
Gui, Add, ListView, x%x_coord% y195 h100 w180 v신규몬스터리스트 +altsubmit, 분류|차원|맵이름|번호|이름|OID|X|Y|Z|주소
LV_ModifyCol(1,40)
LV_ModifyCol(2,0)
LV_ModifyCol(3,0)
LV_ModifyCol(4,0)
LV_ModifyCol(5,50)
LV_ModifyCol(6,0)
LV_ModifyCol(7,30)
LV_ModifyCol(8,30)
LV_ModifyCol(9,30)
LV_ModifyCol(10,0)
x_coord := 320
Gui, Font, s8  Bold,Arial
Gui, Font, s8 cGreen Bold
Gui, Add, Text, x%x_coord% y300 h15 w80, 아이템
Gui, Font
Gui, Font, s8
Gui, Add, ListView, x%x_coord% y320 h130 w180 v아이템리스트 +g아이템리스트실행 +altsubmit, 분류|차원|맵이름|번호|이름|OID|X|Y|Z|주소|삭제카운트|거리|이미지
LV_ModifyCol(1,40)
LV_ModifyCol(2,0)
LV_ModifyCol(3,0)
LV_ModifyCol(4,0)
LV_ModifyCol(5,50)
LV_ModifyCol(6,0)
LV_ModifyCol(7,30)
LV_ModifyCol(8,30)
LV_ModifyCol(9,30)
LV_ModifyCol(10,0)
LV_ModifyCol(11,0)
LV_ModifyCol(12,0)
LV_ModifyCol(13,0)
Gui, Add, ListView, x%x_coord% y455 h100 w180 v신규아이템리스트 +altsubmit, 분류|차원|맵이름|번호|이름|OID|X|Y|Z|주소
LV_ModifyCol(1,40)
LV_ModifyCol(2,0)
LV_ModifyCol(3,0)
LV_ModifyCol(4,0)
LV_ModifyCol(5,50)
LV_ModifyCol(6,0)
LV_ModifyCol(7,30)
LV_ModifyCol(8,30)
LV_ModifyCol(9,30)
LV_ModifyCol(10,0)


; GUI 창을 생성하고 배경 색상을 흰색으로 설정
Gui, Color, FFFFFF  ; 화면을 흰색(#FFFFFF)으로 설정
; GUI 창의 위치와 크기를 설정하고 표시
Gui, Show, x0 y0 w710 h655, 공유방 체잠 Ver 2024 [공개용]
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
RegRead, RegHPHospital, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, HPHospital
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
RegRead, RegP1, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, TTM
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
RegRead, RegChatID, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, ChatID
RegRead, Reg동파감응시간셋팅, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 동파감응시간셋팅
RegRead, Reg서파감응시간셋팅, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 서파감응시간셋팅
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
RegRead, RegPass, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Pass
RegRead, RegServer, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Server
RegRead, RegLogin, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Login
RegRead, RegCharNumber, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, CharNumber
RegRead, RegMNS, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MNS
RegRead, RegEvade, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Evade
RegRead, RegDirect, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Direct
RegRead, RegKONOFF, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, KONOFF
RegRead, RegProtect_AmorONOFF, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Protect_AmorONOFF
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
RegRead, RegMGB, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MGB
RegRead, Reg게임시작y, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 게임시작y
RegRead, RegReserver, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Reserver
RegRead, Reg파라스감지, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 파라스감지
RegRead, Reg수호천사방지, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 수호천사방지
RegRead, Reg인연방지, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 인연방지
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
RegRead, Reg퀵슬롯8번,HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯8번
RegRead, Reg포북생콩, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포북생콩
RegRead, Reg포남생콩, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 포남생콩
RegRead, Reg가방, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 가방
gosub, OID읽기
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
if(Reg퀵슬롯8번 = 1)
{
GuiControl, , 8번사용, 1
}
if(Reg퀵슬롯8번 = 0)
{
GuiControl, , 8번사용, 0
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
if(RegReserver = "" or RegReserver == 0)
{
Reserver := 0
}else{
Reserver := RegReserver
}
if(Reg파라스감지 = "" or Reg파라스감지 == 0)
{
파라스감지 := 0
}else{
파라스감지 := Reg파라스감지
}
if(Reg인연방지 = "" or Reg인연방지 == 0)
{
인연방지 := 0
}
else
{
인연방지 := Reg인연방지
}
if(Reg수호천사방지 = "" or Reg수호천사방지 == 0)
{
수호천사방지 := 0
}
else
{
수호천사방지 := Reg수호천사방지
}
if(Reg파라스방해감지 = "" or Reg파라스방해감지 == 0)
{
파라스방해감지 := 0
}
else{
파라스방해감지 := Reg파라스방해감지
}
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
GuiControl, , ChatID, %RegChatID%
GuiControl, , 동파감응시간셋팅, %Reg동파감응시간셋팅%
GuiControl, , 서파감응시간셋팅, %Reg서파감응시간셋팅%
GuiControl, Choose, Gui_CharNumber, %RegCharNumber%
GuiControl, Choose, Gui_Server, %RegServer%
GuiControl, Choose, Gui_Login, %RegLogin%
GuiControl, Choose, 포북생콩설정, %Reg포북생콩%
GuiControl, Choose, 포남생콩설정, %Reg포남생콩%
GuiControl, Choose, 가방설정, %Reg가방%
if(Reg포북생콩 = "FP 500이하에 먹기")
{
포북생콩섭취 := 500
}
if(Reg포북생콩 = "상시사용으로 먹기")
{
포북생콩섭취 := 1000
}
if(Reg포남생콩 = "FP 500이하에 먹기")
{
포남생콩섭취 := 500
}
if(Reg포남생콩 = "상시사용으로 먹기")
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
if(Reg가방 = "40개 초과시 정지")
{
GuiControl,,Gui_jjON,1
가방수량체크 := 40
jelan.write(0x0047B3EC, 0x02, "Char", aOffsets*)
}
if(Reg가방 = "45개 초과시 정지")
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
GuiControl, HIDE, Gui_NexonID2
GuiControl, HIDE, Gui_NexonPassWord2
GuiControl, HIDE, Gui_NexonID
GuiControl, HIDE, Gui_NexonPassWord
GuiControl, HIDE, 설명
GuiControl, SHOW, 설명2
GuiControl, SHOW, 넥슨x
GuiControl, SHOW, 넥슨y
GuiControl, SHOW, 좌표x
GuiControl, SHOW, 좌표y
좌표고정 := 1
}
if(Reglogin = "홈페이지클릭[구글]")
{
GuiControl, SHOW, Gui_NexonID2
GuiControl, SHOW, Gui_NexonPassWord2
GuiControl, SHOW, Gui_NexonID
GuiControl, SHOW, Gui_NexonPassWord
GuiControl, HIDE, 설명
GuiControl, HIDE, 설명2
GuiControl, HIDE, 넥슨x
GuiControl, HIDE, 넥슨y
GuiControl, HIDE, 좌표x
GuiControl, HIDE, 좌표y
좌표고정 := 0
}
if(Reglogin = "인터넷")
{
GuiControl, SHOW, Gui_NexonID2
GuiControl, SHOW, Gui_NexonPassWord2
GuiControl, SHOW, Gui_NexonID
GuiControl, SHOW, Gui_NexonPassWord
GuiControl, HIDE, 설명
GuiControl, HIDE, 설명2
GuiControl, HIDE, 넥슨x
GuiControl, HIDE, 넥슨y
GuiControl, HIDE, 좌표x
GuiControl, HIDE, 좌표y
좌표고정 := 0
}
if(RegUseHPHospital = 1)
{
GuiControl, , Gui_CheckUseHPHospital, 1
GuiControl, Enable, Gui_HPHospital
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
if(RegHPHospital != "")
{
GuiControl, , Gui_HPHospital, %RegHPHospital%
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
if(RegProtect_AmorONOFF = 1)
{
GuiControl, , Protect_AmorON, 1
}
if(RegProtect_AmorONOFF = 2)
{
GuiControl, , Protect_AmorOFF, 1
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
if(RegPlace = 4)
{
GuiControl, , Gui_HuntMummy, 1
GuiControl, Enable, Gui_LimitAbility1
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
if(RegMGB = 1)
{
GuiControl, , Gui_MGB, 1
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
if(RegRelog = 1)
{
GuiControl, , Gui_relogerror, 1
}
if(RegRelog = 0)
{
GuiControl, , Gui_relogerror, 0
}
Gui, listview, 포프레스네소각
Loop, Read, C:\소각리스트.ini
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
if(포북생콩설정 = "FP 500이하에 먹기")
{
포북생콩섭취 := 500
}
if(포북생콩설정 = "상시사용으로 먹기")
{
포북생콩섭취 := 1000
}
if(포남생콩설정 = "FP 500이하에 먹기")
{
포남생콩섭취 := 500
}
if(포남생콩설정 = "상시사용으로 먹기")
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
if(가방설정 = "40개 초과시 정지")
{
GuiControl,,Gui_jjON,1
가방수량체크 := 40
jelan.write(0x0047B3EC, 0x02, "Char", aOffsets*)
}
if(가방설정 = "45개 초과시 정지")
{
GuiControl,,Gui_jjON,1
가방수량체크 := 45
jelan.write(0x0047B3EC, 0x02, "Char", aOffsets*)
}
return
lagitu: ;라깃업
Gui, Submit, Nohide
RasCount := Gui_RasCount+1
GuiControl, , Gui_RasCount, %RasCount%
return
lagitd: ;라깃다운
Gui, Submit, Nohide
RasCount := Gui_RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
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
if(Gui_HuntMummy = 1)
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
if(Gui_HuntMummy = 1)
{
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility0
GuiControl, Disable, Gui_LimitAbility2
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
if(Gui_HuntMummy = 1)
{
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility0
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Disable, Gui_LimitAbility3
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
if(Gui_HuntMummy = 1)
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
if(Gui_HuntMummy = 1)
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
if(Gui_HuntMummy = 1)
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
if(Gui_huntmummy = 1)
{
GuiControl, Enable, Gui_LimitAbility0
GuiControl, Enable, Gui_LimitAbility1
GuiControl, Enable, Gui_LimitAbility2
GuiControl, Enable, Gui_LimitAbility3
}
}
if(Gui_HuntPobuk = 1 || Gui_huntmummy = 1 )
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
GuiControl, SHOW, Gui_NexonID2
GuiControl, SHOW, Gui_NexonPassWord2
GuiControl, HIDE, 설명
GuiControl, HIDE, 설명2
GuiControl, HIDE, 넥슨x
GuiControl, HIDE, 넥슨y
GuiControl, HIDE, 좌표x
GuiControl, HIDE, 좌표y
좌표고정 := 0
}
if( Gui_login = "홈페이지클릭[구글]" )
{
GuiControl, SHOW, Gui_NexonID
GuiControl, SHOW, Gui_NexonPassWord
GuiControl, SHOW, Gui_NexonID2
GuiControl, SHOW, Gui_NexonPassWord2
GuiControl, HIDE, 설명
GuiControl, HIDE, 설명2
GuiControl, HIDE, 넥슨x
GuiControl, HIDE, 넥슨y
GuiControl, HIDE, 좌표x
GuiControl, HIDE, 좌표y
좌표고정 := 0
}
if( Gui_login = "넥슨플러그" )
{
GuiControl, HIDE, Gui_NexonID2
GuiControl, HIDE, Gui_NexonPassWord2
GuiControl, HIDE, Gui_NexonID
GuiControl, HIDE, Gui_NexonPassWord
GuiControl, HIDE, 설명
GuiControl, SHOW, 설명2
GuiControl, SHOW, 넥슨x
GuiControl, SHOW, 넥슨y
GuiControl, SHOW, 좌표x
GuiControl, SHOW, 좌표y
좌표고정 := 1
}
return
CheckUseHPHospital:
Gui, Submit, Nohide
GuiControl, % (Gui_CheckUseHPHospital ? "enable":"disable"), Gui_HPHospital
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
resetting:
Gui, Submit, Nohide
step = 0
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
RandomRadio()
MsgBox,48, 차원설정,
(
<파티사용시>
파티캘이 위치한 차원으로 이동해라 게이야
일단 내가 따!악 좋은 차원으로 설정 해 줬다!
),2
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
if(Gui_CheckUseHPHospital = 1)
{
if(Gui_HPHospital = "")
{
SB_SetText("병원 갈 체력을 정확히 입력 해 주세요.")
return
}
if(Gui_HPHospital = 0)
{
SB_SetText("병원 갈 체력은 0보다 높아야 합니다.")
return
}
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
if(Gui_CheckUseHPHospital = 1 and Gui_CheckUseHPPortal)
{
if(Gui_HPHospital > Gui_HPPortal)
{
SB_SetText("병원이동 체력 설정은 차원이동 체력보다 높아야 합니다.")
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
NexonID := Gui_NexonID
NexonPassword := Gui_NexonPassword
GuiControl, Disable, Gui_NexonID
GuiControl, Disable, Gui_NexonPassWord
GuiControl, Disable, Gui_Server
GuiControl, Disable, Gui_Login
GuiControl, Disable, Gui_CharNumber
GuiControl, Disable, Gui_CheckUseHPHospital
GuiControl, Disable, Gui_CheckUseHPExit
GuiControl, Disable, Gui_CheckUseHPPortal
GuiControl, Disable, Gui_CheckUseHPLimited
GuiControl, Disable, Gui_HPExit
GuiControl, Disable, Gui_HPHospital
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
SB_SetText("로그인 중")
실행초기화 := 0
이전스텝 := step
Step = 0
FirstCheck = 1
MagicN = 3
FirstPortal = 1
Entrance = 0
ipmak = 0
callid = 1
RCC = 0
ProgramStartTime = 0
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
SetTimer, Hunt, 10
SetTimer, AttackCheck, 10
SetTimer, AttackMGB, 1000
SetTimer, 타겟팅, 100
SetTimer, GetMemory, off
SetTimer, ClearMem, off
SetTimer, RL, 15000000
시작탭사이즈 := 1
return

RL:
Gui, Submit, NoHide
loady = 2
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid Jelancia.exe
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
}
if(Gui_MGB = 1)
{
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MGB, 1
}
if(Gui_MGB = 0)
{
    RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MGB, 0
}
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, VMRE, 0
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 실행시간, %지금시각_R%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Reserver, %Reserver%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 파라스감지,%파라스감지%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 수호천사방지,%수호천사방지%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 인연방지,%인연방지%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 파라스방해감지,%파라스방해감지%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, TTM, %PID%
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
;RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 업데이트체크, %업데이트체크%
if(Gui_CheckUseHPHospital = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPHospital, 1
}
if(Gui_CheckUseHPHospital = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPHospital, 0
}
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
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, HPHospital, %Gui_HPHospital%
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
if(Protect_AmorON = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Protect_AmorONOFF, 1
}
if(Protect_AmorOFF = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Protect_AmorONOFF, 2
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
if(Gui_HuntMummy = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Place, 4
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
if(Gui_MGB = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MGB, 1
}
if(Gui_MGB = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MGB, 0
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
if(8번사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯8번, 1
}
if(8번사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯8번, 0
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
FileDelete, C:\소각리스트.ini
save := LV_GetCount()
loop, %save%{
lv_gettext(savefile1,a_index)
FileAppend, %savefile1%`n, C:\소각리스트.ini
FileSetAttrib, +H, C:\소각리스트.ini
}
Sleep, 100
;ReloadScript()
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
incineration: ;줍줍 제어 용
Gui, Submit, Nohide
if((Step >= 19 and Step < 90) || (Step >= 1013 and Step < 1030) || (Step >= 3023 and Step < 3031)) ; 포남 포북 머미면
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
incinerate_item()
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
else IfInString,Location,크로노시스
{
Gui, listview, 포프레스네소각
IfInString,Location,1F
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
Sleep, 10
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
Get_Location()
GuiControl, , Gui_NowLocation, %Location%
Get_inven()
if(NowInven = 50)
{
invenstack += 1
if(invenstack > 150)
{
SLEEP, 100
이전스텝 := step
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
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 인벤칸 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMsph.exe
}
GUICONTROL, , Gui_NowState, 인벤토리에 빈공간이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("인벤칸 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
settimer, 감응,off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
invenError = 0
return
}
Hunt:
GUI, Submit, Nohide
if(A_WDay = 5 && 서버쳌쳌 = 1)
{
try {
    ; Internet Explorer COM 객체 생성
    ie := ComObjCreate("InternetExplorer.Application")
    ie.Visible := false ; 브라우저 창 숨김 (true로 설정하면 창이 표시됨)
    url := "https://elancia.nexon.com/"
    ie.Navigate(url)

    ; 로딩 완료 대기
    while ie.Busy or ie.ReadyState != 4
        Sleep, 100

    ; 페이지 HTML 가져오기
    html := ie.document.body.innerHTML

    ; 1단계: "서버 현황" 단어 위치 찾기 및 300자 추출
    target1 := "서버 현황"
    pos1 := InStr(html, target1) ; "서버 현황" 단어 위치 찾기

    if (pos1 > 0) {
        extractedText := SubStr(html, pos1, 300) ; "서버 현황" 위치부터 300자 추출

        ; 2단계: 추출된 300자 내에서 "엘" 단어 찾기
        target2 := "엘"
        pos2 := InStr(extractedText, target2) ; "엘" 단어 위치 찾기

        if (pos2 > 0) {
            ; 3단계: "엘" 위치 이후 <dd>와 </dd> 사이 텍스트 추출
            ddStart := InStr(extractedText, "<dd>", false, pos2) ; <dd> 시작 위치
            ddEnd := InStr(extractedText, "</dd>", false, ddStart) ; </dd> 종료 위치

            if (ddStart > 0 and ddEnd > ddStart) {
                result := SubStr(extractedText, ddStart + 4, ddEnd - ddStart - 4) ; <dd> 태그 이후 텍스트 추출
                Server := result
                TMessage :="추출된 데이터: " result Server
                텔레그램메시지보내기(TMessage)
            }
            else
            {
        TMessage :="추출된 텍스트 내에서 단어 '" result "'을 찾을 수 없습니다."
        텔레그램메시지보내기(TMessage)
            }
        }
        else
        {
        TMessage :="추출된 텍스트 내에서 단어 '" target2 "'을 찾을 수 없습니다."
        텔레그램메시지보내기(TMessage)
        }
    }
    else
    {
        TMessage :="HTML에서 단어 '" target1 "'을 찾을 수 없습니다."
        텔레그램메시지보내기(TMessage)
    }

    ; IE 객체 종료
    ie.Quit()
}
catch e
{
TMessage :="예외 발생! 상세 정보:" . e.Message . "Line: " . e.Line
텔레그램메시지보내기(TMessage)
GROUPADD, ie_gruop, ahk_exe iexplore.exe
WINKILL, ahk_exe iexplore.exe
WINKILL, ahk_group ie_gruop
GOSUB, RL
}
if ((Trim(Server) = "정상") && 서버쳌쳌 = 1 )
{
GuiControl, , Gui_NowState, 일랜시아 게임 서버 정상.
Sleep, 1000
Step = 0
서버쳌쳌 = 0
return
}
if(Trim(Server) != "정상")
{
서버쳌쳌 = 1
GUICONTROL, , Gui_NowState, 일랜시아 홈페이지 서버 점검 중. 5분 대기2
GUICONTROL, , Gui_KOFF, 1
WinKill, ahk_exe NGM64.exe
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
Sleep, 300000
return
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
WinKill, ahk_exe MRMsph.exe
}
}
IfWinExist,Microsoft Visual C++ Runtime Library
{
WinClose
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
}
}
IfWinExist,ahk_exe WerFault.exe
{
ControlClick, Button2, ahk_exe WerFault.exe
Process, Close, WerFault.exe
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
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
WinKill, ahk_exe MRMsph.exe
}
}

IfWinExist,ahk_pid %jPID%
{
if((Step >= 11 and Step < 90) or (Step >= 1004 and Step < 1030) or (Step >= 3000 and Step =< 3031)) ;템뿌 파트
{
if( Protect_AmorON = 1 )
{
AmorCheck()
if( Top = 0 or shoes = 0 or jean = 0 or Cap = 0 )
{
IfWinExist,ahk_pid %jPID%
{
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 캐릭터 템 일부 벗겨짐. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMsph.exe
}
GUICONTROL, , Gui_NowState, 캐릭터 사망 방지.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("캐릭사망방지, 자동정지")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
invenError = 0
MsgBox,48, 캐릭터사망방지, 템이 벗겨졌습니다.
return
}
}
}


if((Step >= 7 and Step < 507) or (Step >= 512 and Step < 10000)) ;수호천사 파트
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
수호천사방지++
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
수호천사방지++
}
}
IfInString,NPCMsg,인연
{
SLEEP, 500
SetFormat, Integer, H
startAddress := 0x00100000
endAddress := 0x00200000
SetFormat, Integer, D
NPC_MSG_ADR := Check_NPCMsg_address()
FormNumber := jelan.read(0x0058DAD0, "UInt", 0xC, 0x10, 0x8, 0xA0)
NPCMsg := jelan.readString(NPC_MSG_ADR, 52, "UTF-16", aOffsets*)
TMessage := "[ Helancia_Log ]>>" jTitle FormNumber "<<: 애미뒤진 인연버그 발생. " Location "/" "발생 메시지 전문 : " NPCMsg
텔레그램메시지보내기(TMessage)
keyclick("프로세스종료")
이전스텝 := step
step = 0
return
}
IfInString,NPCMsg,렉스
{
SLEEP, 500
SetFormat, Integer, H
startAddress := 0x00100000
endAddress := 0x00200000
SetFormat, Integer, D
NPC_MSG_ADR := Check_NPCMsg_address()
FormNumber := jelan.read(0x0058DAD0, "UInt", 0xC, 0x10, 0x8, 0xA0)
NPCMsg := jelan.readString(NPC_MSG_ADR, 52, "UTF-16", aOffsets*)
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle FormNumber "<<: 애미뒤진 길드버그 발생. " Location "/" "발생 메시지 전문 : " NPCMsg
텔레그램메시지보내기(TMessage)
keyclick("프로세스종료")
이전스텝 := step
step = 0
return
}
Get_Location()
GuiControl, , Gui_NowLocation, %Location%
SB_SetText(Location,2)
IfInString,Location,포프레스네 마을
{
if( Protect_AmorON = 1 )
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
GuiControl,,Gui_NowFP,%NowFP% / %MaxFP% (%FPPercent%`%)

GUIDimension := jelan.read(0x0058EB1C, "UInt", 0x10A)
	if(GUIDimension>20000)
		GUI차원:="감마"
	else if(GUIDimension>10000)
		GUI차원:="베타"
	else if(GUIDimension<10000)
		GUI차원:="알파"
GUI좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
GUI좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
GUI좌표Z := jelan.read(0x0058DAD4, "UInt", 0x18)
GUIStep := step
GUIMyloute := Myloute
GuiControl,, 좌표,(%GUI차원%)X:%GUI좌표X%.Y:%GUI좌표Y%.Z:%GUI좌표Z%에 있습니다.[%GUIStep%/%GUIMyloute%/%mapnumber%]
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
if ((A_TickCount - LastMessageTime) >= 3600000)  ; 1시간(3600초 = 3600000밀리초)
{
    상승체력_1시간 := CheckUPHP - LastCheckUPHP
    상승체력평균값_1시간 := (상승체력_1시간 * 60) / 60  ; 1시간 동안의 평균 상승 체력 계산

    TMessage := "[ Helancia_Log ]>>" . jTitle "<<: 시작 체력 : " . CheckFirstHP . " / 1시간 상승 체력 : " . 상승체력_1시간 . " ( " . 상승체력평균값_1시간 . " ) " . " / 경과 시간 :  " . RunningTime . " 한시간동안 오른 체력 입니다."
    텔레그램메시지보내기(TMessage)

    ; 1시간 단위 상승 체력 업데이트 및 LastMessageTime 리셋
    LastCheckUPHP := CheckUPHP
    LastMessageTime := A_TickCount
}
CheckUPHP := MaxHP - CheckFirstHP
SB_SetText(" 시작 체력 : " . CheckFirstHP . " / 상승 체력 : " . CheckUPHP . " ( " . 상승체력평균값 . " ) " . " / 경과 시간 :  " . RunningTime ,3 )
}
if(파라스방해감지 = 1)
{
파라스타이머카운트 := FormatSeconds(3600-((A_TickCount-파라스타이머시작)/1000))
GuiControl,,파라스타이머,파라스대기 = %파라스타이머카운트%
}
else
{
GuiControl,,파라스타이머, 다시포남까지 = 0:00:00
}
if(Gui_CheckUseHPExit = 1)
{
if(NowHP <= Gui_HPExit and NowHP != "")
{
IfWinExist,ahk_pid %jPID%
{
Get_Location()
좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
좌표Z := jelan.read(0x0058DAD4, "UInt", 0x18)
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
TMessage := "[ Helancia_Log ]>>" jTitle  "<< : 체력이" . NowHP . "가 되어 종료 확인 필요. 위치보고: " Location . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ") 시작 체력 : " . CheckFirstHP . " / 상승 체력 : " . CheckUPHP . " ( " . 상승체력평균값 . " ) / 경과 시간 : " RunningTime
텔레그램메시지보내기(TMessage)
}
GuiControl, , Gui_NowState, 체력이 %NowHP%가 되어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("헬퍼실행. 자동정지")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF

CheckPB = 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
return
}
}
internet := ConnectedToInternet()
if(internet = 0)
{
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
}
이전스텝 := step
Step = 10000
return
}
ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16", aOffsets*)
IfInString,ServerMsg,오랜 시간 아무것도 하지 않으면
{
IfWinExist,ahk_pid %jPID%
{
TMessage := "[ Helancia_Log ]>>" . jTitle "<<:로그인 시 오랜시간 아무것도 하지 않음."
텔레그램메시지보내기(TMessage)
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
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
Reserver++
실행초기화 += 1
step := 8
sleep,10000
return
}
}
IfInString,ServerMsg,서버와의 연결이
{
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
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
IfnotInString,Location,북쪽 필드
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
if(ParasCount >= 3)
{
GuiControl, , Gui_NowState, [포남] 파라스를 감지하여 포북 이동.2
ParasCount = 3
TMessage := "[ Helancia_Log ]>>" . jTitle "<<: 포북으로 잠시 이동."
텔레그램메시지보내기(TMessage)
파라스방해감지 := 1
GuiControl,,Gui_huntpobuk,1
파라스감지++
}
}
Reserver++
실행초기화 += 1
if(Step = 17 or step = 18)
{
Entrance += 1
}
if(Entrance > 2)
{
MsgBox, , 비정상종료감지, OID리셋, 3
TMessage := "[ Helancia_Log ]>>" . jTitle "<<: 초기 입구 감응 실패. OID 리셋."
텔레그램메시지보내기(TMessage)
gosub,OID리셋
step := 8
sleep,1000
return
}
}
이전스텝 := step
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
		맵이름 := jelan.readString(jelan.getModuleBaseAddress("jelancia_core.dll")+0x44A28, 50, "UTF-16", 0xC)
		Dimension := mem.read(0x0058EB1C, "UInt", 0x10A)
		if(Dimension>20000)
			현재차원 := "감마"
		else if(Dimension>10000)
			현재차원 := "베타"
		else if(Dimension<10000)
			현재차원 := "알파"
		좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
		좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
		좌표Z := jelan.read(0x0058DAD4, "UInt", 0x18)
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle " <<: 캐릭터 설정된 체력에 도달. 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
}
GuiControl, , Gui_NowState, 설정된 체력에 도달하여 강제 종료합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("헬퍼실행. 자동정지")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
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
이전스텝 := step
step = 10000
Sleep, 100
}
}
if((Step >= 19 and Step < 90) or (Step >= 1013 and Step < 1030) or (Step >= 3000 and Step =< 3031))
{
callid := jelan.read(0x0058EDB8, "UInt", aOffsets*)
if(callid > 2500)
{
value := jelan.write(0x0058EDB8, 0, "UInt")
SLEEP, 500
Entrance = 0
}
}
if((Step >= 19 and Step < 90) or (Step >= 1013 and Step < 1030) or (Step >= 3000 and Step =< 3031))
{

name := jelan.readString(jelan.read(0x0058F058)+0x184,,"UTF-16")
if( name != "" )
{
Loop,
{
SB_SetText("거래창 방해감지")
GuiControl, , jTitle, %jTitle%
TMessage :="[ Helancia_Log ]>>" jTitle "<<: [거래방지] 이 시벌놈이 거래겁니다 거래건놈 :" name
텔레그램메시지보내기(TMessage)
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
if(Step >= 17 and Step < 90)
{
GuiControl,,CallTarget,%호출대상%
;감응감응 := (A_TickCount -포남입장시간) / 1000
GuiControl,, 감응쿨타임, [%호출대상%] 호출완료
}
}
if((Step >= 19 and Step < 90) or (Step >= 1013 and Step < 1030) or (Step >= 3023 and Step < 3031)) ; 퀵슬롯 및 체크
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
if(HuntPlace = 1 and NowFp <= 포남생콩섭취 and 8번사용 = 0)
{
PostMessage, 0x100, 56, 589825, , ahk_pid %jPID%
PostMessage, 0x101, 56, 589825, , ahk_pid %jPID%
}
if(HuntPlace = 2 and NowFp <= 포남생콩섭취 and 8번사용 = 0)
{
PostMessage, 0x100, 56, 589825, , ahk_pid %jPID%
PostMessage, 0x101, 56, 589825, , ahk_pid %jPID%
}
if(8번사용 = 1)
{
PostMessage, 0x100, 56, 589825, , ahk_pid %jPID%
PostMessage, 0x101, 56, 589825, , ahk_pid %jPID%
}
if(9번사용 = 1)
{
if(NowFP < 1)
{
PostMessage, 0x100, 57, 589825, , ahk_pid %jPID%
PostMessage, 0x101, 57, 589825, , ahk_pid %jPID%
}
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
if((Step >= 19 and Step < 90) or (Step > 1013 and step < 1030) or (Step >= 3023 and Step < 3031)) ; 조정 부분
{
if(Gui_CheckUseHPPortal = 1)
{
if(NowHP <= Gui_HPPortal and NowHP != "")
{
if(HuntPlace = 1)
{
타겟number := 0
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
settimer, 감응,off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
GuiControl, , Gui_NowState, 체력이 %NowHP%가 되어 차원이동 합니다.
GuiControl, , jTitle, %jTitle%
TMessage :="[ Helancia_Log ]>>" jTitle "<<: 체력:" NowHP "입니다. 차원이동해서 정상 진행 합니다."
텔레그램메시지보내기(TMessage)
CheckPB = 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
Step = 9
return
}
if(HuntPlace = 3)
{
CheckPB = 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
타겟number := 0
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
settimer, 감응,off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
GuiControl, , Gui_NowState, 체력이 %NowHP%가 되어 차원이동 합니다.
TMessage :="[ Helancia_Log ]>>" jTitle "<<: 체력:" NowHP "입니다. 차원이동해서 정상 진행 합니다."
텔레그램메시지보내기(TMessage)
Step = 3000
return
}
if(HuntPlace = 2)
{
CheckPB = 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
타겟number := 0
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
settimer, 감응,off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
GuiControl, , Gui_NowState, 체력이 %NowHP%가 되어 차원이동 합니다.
TMessage :="[ Helancia_Log ]>>" jTitle "<<: 체력:" NowHP "입니다. 차원이동해서 정상 진행 합니다."
텔레그램메시지보내기(TMessage)
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
CheckPN := 0
countsignal := 0
랜덤감응 = 0
keyclick("tab")
Step = 600
}
}
if(MagicAbility3 = 100 or MagicAbility4 = 100 or MagicAbility5 = 100 or MagicAbility6 = 100 or MagicAbility7 = 100 or MagicAbility8 = 100)
{
if(Gui_Grade = 1)
{
CheckPB = 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
keyclick("tab")
Step = 650
}
}
get_FP()
if(NowFP = 0)
{
FPcount ++
if(nowFP = 0 && FPcount >= 20)
{
keyclick("tab")
FPcount := 0
CheckPB := 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
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
else
{
    FPcount := 0
}
if(Gui_CheckUseHPhospital = 1) ;병원가기 함수
{
if(NowHP <= Gui_HPHospital and NowHP != "")
{
keyclick("tab")
CheckPB = 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
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
GuiControl, , Gui_NowState, 체력이 %NowHP%가 되어 병원에 갑니다.
SB_SetText("병원이동")
step = 750
return
}
}
}
Check_Chat()
if(Chat = 1)
{
PostMessage, 0x100, 13, 1835009, , ahk_pid %jPID%
PostMessage, 0x101, 13, 1835009, , ahk_pid %jPID%
}
}
if((Step >= 19 and Step < 90) || (step >= 1061 && step <= 1067))
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
if(ParasCount >= 3)
{
GuiControl, , Gui_NowState, [포남] 파라스를 감지하여 포북 이동.
ParasCount = 3
TMessage := "[ Helancia_Log ]>>" . jTitle "<<: 파라스로 잠시 포북 이동 "
텔레그램메시지보내기(TMessage)
파라스방해감지 := 1
Settimer, 파라스대기, %파라스대기값%
파라스타이머시작 := A_TickCount
GuiControl,,Gui_huntpobuk,1
파라스감지++
step := 8
return
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
if(Step = 27 or Step = 1026 or step = 3030)
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
GUICONTROL, , Gui_NowState, [포남] 헉 만드다. . . !
GuiControl, , jTitle, %jTitle%
TMessage :="[ Helancia_Log ]>>" jTitle "<<: X:" .  MandX . "와 Y : " . MandY . "에"  . "만드발견. 튑니다. "
텔레그램메시지보내기(TMessage)
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
if(HuntPlace = 3)
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

if(Gui_HuntMummy = 1)
{
if(HuntPlace = 3)
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
IfWinNotExist,ahk_pid %jPID%
{
if(Step >= 5 and Step < 10000)
{
GuiControl, , 로그인상태정보, 오류로 인해 재접속 합니다.
Step = 0
}
}
if(Step = 0)
{
GuiControl, , 로그인상태정보, 초기 세팅 중 입니다.
settimer, 감응,off
SetTimer, incineration, off
CheckPB = 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
WinKill, ahk_pid %jPID%
GroupAdd, ie_gruop, ahk_exe iexplore.exe
WinKill, ahk_exe iexplore.exe
WinKill, ahk_group ie_gruop
WinKill, ahk_exe MRMsph.exe
countsignal = 0
MapNumber = 1
MoveWaitCount = 0
MobNumber = 1
AttackLoopCount = 0
AttackCount = 0
pbtalkcheck = 0
RunDirect = 0
Run타겟 := 0
gui_Startmap := 0
getidc = 1
callid = 1
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
WinKill, ahk_exe MRMsph.exe
if(실행초기화 = 0)
{
SB_SetText("프로그램 구동중")
WinKill, ahk_pid %jPID%
GroupAdd, ie_gruop, ahk_exe iexplore.exe
WinKill, ahk_exe iexplore.exe
WinKill, ahk_group ie_gruop
WinKill, ahk_exe MRMsph.exe
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
SB_SetText("서버연결종료. 재접속 설정중")
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
TMessage :="[ Helancia_Log ]>>" jTitle "<<: 서버 연결 종료로 재시작 [ 이전스텝 : " 이전스텝 " / " 상태 "]"
텔레그램메시지보내기(TMessage)
Sleep, 5000
실행초기화 := 0
return
}
if( 파라스방해감지 = 1 )
{
Settimer, 파라스대기, %파라스대기값%
파라스타이머시작 := A_TickCount
}
Step = 1
}
if( Gui_Login = "인터넷" )
{
if(Step = 1)
{
GUICONTROL, , 로그인상태정보, [로그인] - 접속 중
FileCreateDir, ChromeProfile
ProfilePath := A_ScriptDir . "\ChromeProfile" ; 사용자 프로파일 경로 지정
    ChromeInst := new Chrome(ProfilePath, , , , , False) ; Headless 모드를 끔(False)
    ; 새로운 페이지 탭 가져오기
    PageInst := ChromeInst.GetPage()
    sleep,300
    PageInst.Call("Page.navigate", {"url": "https://elancia.nexon.com/"})
    SB_SetText("홈페이지 접속중")
    while (PageInst.Evaluate("document.readyState").value != "complete")
{
    sleep, 100  ; 0.5초 대기 후 다시 확인
}
    PageInst.Evaluate("PS.game.startGame({ gameCode:74276 });")
    while (PageInst.Evaluate("document.readyState").value != "complete")
{
    sleep, 100  ; 0.5초 대기 후 다시 확인
}
LoginURL := PageInst.Evaluate("window.location.href").value
;    MsgBox, % "현재 URL은: " LoginURL
If (LoginURL != "https://nxlogin.nexon.com/common/login.aspx?redirect=https%3A%2F%2Felancia.nexon.com%2F")
{
    GuiControl, , 로그인상태정보, [로그인] - 실패 ( 접속불량 )
    Gui_Enable()
    SetTimer, Hunt, Off
    SetTimer, AttackCheck, Off
    SetTimer, AttackMGB, off
    SetTimer, 타겟팅, Off
    SetTimer, incineration, off
    CheckPB = 0
    CheckPN := 0
    countsignal := 0
    랜덤감응 = 0
    return
}
If (LoginURL = "https://elancia.nexon.com/")
{
GuiControl, , 로그인상태정보, [로그인] - 로그인이 되어있습니다. ( 중복 )
PageInst.WaitForLoad()
PageInst.Evaluate("inface.auth.gotoSignOut();")
; JavaScript 실행
PageInst.Evaluate(removeCookiesScript)
PageInst.Call("Browser.close")
PageInst.Disconnect()
ChromeInst.Close() ; 크롬 인스턴스 종료
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
return
}
    PageInst.Evaluate("document.querySelector('#txtNexonID').value = '" Gui_NexonID "';") ; ID 입력
    PageInst.Evaluate("document.querySelector('#txtPWD').value = '" Gui_NexonPassWord "';") ; 비밀번호 입력
    PageInst.Evaluate("document.querySelector('.button01').click();") ; 로그인 버튼 클릭
sleep,1000
while (PageInst.Evaluate("document.readyState").value != "complete")
{
    sleep, 100  ; 0.5초 대기 후 다시 확인
}
SB_SetText("넥슨 로그인 체크")
LoginURL := PageInst.Evaluate("window.location.href").value
;MsgBox, % "현재 URL은: " LoginURL
if(LoginURL != "https://nxlogin.nexon.com/common/login.aspx?redirect=https%3A%2F%2Felancia.nexon.com%2F")
{
IfInString,LoginURL,errorcode=1
{
GuiControl, , 로그인상태정보, [로그인] - 실패 ( ID,비번 틀림 )
 PageInst.WaitForLoad()
 PageInst.Evaluate("inface.auth.gotoSignOut();")
 ; JavaScript 실행
PageInst.Evaluate(removeCookiesScript)
PageInst.Call("Browser.close")
PageInst.Disconnect()
ChromeInst.Close() ; 크롬 인스턴스 종료
Gui_Enable()
GuiControl, , jTitle, %jTitle%
TMessage :="[ Helancia_Log ]>>" jTitle "<<:자동 로그인 실패"
텔레그램메시지보내기(TMessage)
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
CheckPN := 0
CheckPB = 0
countsignal := 0
랜덤감응 = 0
return
}
}
while !PageInst.Evaluate("document.querySelector('.game_start')")
{
    Sleep, 100  ; 0.5초 대기
}
sleep,1000
PageInst.Evaluate("document.querySelector('.game_start').click();") ; 로그인 버튼 클릭
WinWait, ahk_exe jElancia.exe, , 15
PageInst.Evaluate("inface.auth.gotoSignOut();")
PageInst.Evaluate(removeCookiesScript)
; 테스트 종료: 크롬 브라우저 닫기
PageInst.Call("Browser.close")
PageInst.Disconnect()
ChromeInst.Close() ; 크롬 인스턴스 종료
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
TMessage :="[ Helancia_Log ] [플러그]로그인 실패, 확인 필요." Location
텔레그램메시지보내기(TMessage)
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
CheckPN := 0
CheckPB = 0
countsignal := 0
랜덤감응 = 0
return
}
else
{
WinShow, ahk_exe NexonPlug.exe
sleep,1000
WinActivate, ahk_exe NexonPlug.exe
Sleep, 1500
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
Sleep,2000
IfWinExist ahk_exe Jelancia.exe
{
WinHide, ahk_exe NexonPlug.exe
Step = 2
}
}
}
}
if( Gui_Login = "홈페이지클릭[구글]" )
{
if(Step = 1)
{
GUICONTROL, , 로그인상태정보, [로그인] - 접속 중
FileCreateDir, ChromeProfile
ProfilePath := A_ScriptDir . "\ChromeProfile" ; 사용자 프로파일 경로 지정
ChromeInst := new Chrome(ProfilePath, , , , , False) ; Headless 모드를 끔(False)
    ; 새로운 페이지 탭 가져오기
PageInst := ChromeInst.GetPage()
PageInst.Call("Page.navigate", {"url": "https://elancia.nexon.com/"})
while (PageInst.Evaluate("document.readyState").value != "complete")
{
    sleep, 100  ; 0.5초 대기 후 다시 확인
}
PageInst.Evaluate("PS.game.startGame({ gameCode:74276 });")
SB_SetText("크롬 실행")
while (PageInst.Evaluate("document.readyState").value != "complete")
{
    sleep, 500  ; 0.5초 대기 후 다시 확인
}
LoginURL := PageInst.Evaluate("window.location.href").value
if (InStr(LoginURL, "https://elancia.nexon.com/"))
{
SB_SetText("일랜시아 확인1")
sleep,4000 ; 만약 그대로면 클라 켜진거니 그냥 step = 2진행
;PageInst.Evaluate("inface.auth.gotoSignOut();")
    ; 테스트 종료: 크롬 브라우저 닫기
PageInst.Evaluate("inface.auth.gotoSignOut();")
; JavaScript 실행
PageInst.Evaluate(removeCookiesScript)
PageInst.Call("Browser.close")
PageInst.Disconnect()
ChromeInst.Close() ; 크롬 인스턴스 종료

step = 2
return
}
else if (InStr(LoginURL, "https://nxlogin.nexon.com/"))
{
SB_SetText("일랜시아 확인2")
PageInst.Evaluate("document.querySelector('.btGoogle').click();") ; 넥슨 로그인 창이면 구글 로그인 버튼 클릭
SB_SetText("구글 로그인 버튼")
while (PageInst.Evaluate("document.readyState").value != "complete")
{
    sleep, 500  ; 0.5초 대기 후 다시 확인
}
LoginURL := PageInst.Evaluate("window.location.href").value
if (InStr(LoginURL, "https://accounts.google.com/")) ;구글 계정선택 창이면?
{
SB_SetText("일랜시아 확인3")
sleep,2000
PageInst.WaitForLoad() ;기다렸다가
PageInst.Evaluate("document.querySelector('[data-email=""" Gui_NexonID """').click();") ; 아이디 누르고
sleep,5000
PageInst.Evaluate("document.querySelector('[data-initial-value]').value = '" Gui_NexonPassWord "';")
sleep, 2000
    PageInst.Evaluate("document.querySelector('#passwordNext').click();") ; 다음 버튼 클릭
while (PageInst.Evaluate("document.readyState").value != "complete")
{
    sleep, 500  ; 0.5초 대기 후 다시 확인
}
LoginURL := PageInst.Evaluate("window.location.href").value ; URL 바뀌었는지 체크
if (InStr(LoginURL, "https://elancia.nexon.com/")) ;만약 일랜시아로 바뀌었으면
{
SB_SetText("일랜시아 확인4")
PageInst.Evaluate("document.querySelector('.game_start').click();") ; 넥슨 로그인 창이면 구글 로그인 버튼 클릭
sleep,3000 ; 만약 일랜시아로 가면 자동로그인 된거니 그냥 게임실행하고 진행
PageInst.Evaluate("inface.auth.gotoSignOut();")
; JavaScript 실행
PageInst.Evaluate(removeCookiesScript)
PageInst.Call("Browser.close")
PageInst.Disconnect() ;꺼
ChromeInst.Close() ; 크롬 인스턴스 종료
step = 2
return
}
else
{
GuiControl, , 로그인상태정보, [로그인] - 실패 ( 접속오류 )
PageInst.Evaluate("inface.auth.gotoSignOut();")
; JavaScript 실행
PageInst.Evaluate(removeCookiesScript)
sleep,4000
PageInst.Call("Browser.close")
PageInst.Disconnect() ;꺼
ChromeInst.Close() ; 크롬 인스턴스 종료
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
CheckPB = 0
}
}
else if (InStr(LoginURL, "https://elancia.nexon.com/"))
{
SB_SetText("구글 로그인 완료")
sleep,4000
PageInst.Evaluate("document.querySelector('.game_start').click();") ; 넥슨 로그인 창이면 구글 로그인 버튼 클릭
sleep, 6000 ; 만약 일랜시아로 가면 자동로그인 된거니 그냥 게임실행하고 진행
PageInst.Evaluate("inface.auth.gotoSignOut();")
; JavaScript 실행
PageInst.Evaluate(removeCookiesScript)
PageInst.Call("Browser.close")
PageInst.Disconnect() ;꺼
ChromeInst.Close() ; 크롬 인스턴스 종료
step = 2
return
}
}
Step = 2
}
}
if(Step = 2)
{
Sleep, 2000
SB_SetText("로그인 상태 체크")
GuiControl, , 로그인상태정보, [로그인] - 실행중
WinKill, ahk_exe MRMsph.exe
WinActivate, ahk_exe Jelancia.exe
Step = 3
}
if(Step = 3)
{
WINWAIT, ahk_exe jElancia.exe, , 15
Sleep, 1000
ControlGetText, Patch, Static2, Elancia
Sleep, 3000
SB_SetText("일랜실행중", 1)
sb_settext("서버메시지 - " Patch "젤랜:" ,2)
IfnotInString,Patch,최신 버전입니다. 게임을 시작하세요.
{
SetTitleMatchMode, 1 ; 부분 일치 모드 활성화
WinClose, Elancia
WinKill, ahk_exe MRMsph.exe
TMessage :="[ Helancia_Log ] 패치 이상 재설정. [추정오류 : 서버 점검 및 인터넷이상]"
텔레그램메시지보내기(TMessage)
Sleep, 2000
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
Sleep, 1000
GuiControl, , 로그인상태정보, [로그인] - 서버 선택 중
WinGetTitle, jTitle, ahk_pid %jPID%
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
Sleep, 10
PostMessage, 0x100, 13, 1835009, , ahk_pid %jPID%
PostMessage, 0x101, 13, 1835009, , ahk_pid %jPID%
}
if(Gui_Server = "테스")
{
PostMessage, 0x200, 0, 17826096, , ahk_pid %jPID%
PostMessage, 0x201, 1, 17826096, , ahk_pid %jPID%
PostMessage, 0x202, 0, 17826096, , ahk_pid %jPID%
Sleep, 10
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
Sleep, 1000
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
TMessage :="[ Helancia_Log ]>>" jTitle "<<: 접속 오류로 인한 재시작 및 기존 젤랜시아 종료"
텔레그램메시지보내기(TMessage)
WinKill, ahk_pid %jPID%
WinKill, ahk_exe CTEXE.exe
WinKill, ahk_exe Jelancia.exe
}
GuiControl, , 로그인상태정보, 접속 오류로 대기 후 재시작 합니다.
Sleep, 60000
이전스텝 := step
Step = 10000
return
}
ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16", aOffsets*)
IfInString,ServerMsg,일랜시아 서버에
{
TMessage := "[ Helancia_Log ]" ServerMsg
텔레그램메시지보내기(TMessage)
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
Step = 10000
sleep,50000
return
}
ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16", aOffsets*)
IfInString,ServerMsg,서버와의 연결이
{
IfWinExist,ahk_pid %jPID%
{
TMessage := "[ Helancia_Log ]" ServerMsg
텔레그램메시지보내기(TMessage)
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
}
이전스텝 := step
Step = 10000
return
}
IfInString,ServerMsg,인증시간이
{
IfWinExist,ahk_pid %jPID%
{
TMessage := "[ Helancia_Log ]" ServerMsg
텔레그램메시지보내기(TMessage)
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
}
Step = 10000
return
}
WinGetTitle, jTitle, ahk_pid %jPID%
if(jTitle != "일랜시아" and jTitle != "일랜시아 - 엘" and jTitle != "일랜시아 - 테스" )
{
GuiControl, , 로그인상태정보, [로그인] - 접속 완료
GuiControl, , Gui_CharName, %jTitle%
GuiControl,Show,Gui_StopButton
SB_SetText("접속 완료")
Sleep, 100
step =7
gui_Startmap := 1
Sleep, 300
}
}
if(Step = 7 and gui_Startmap = 1)
{
GuiControl, , Gui_NowState, 마을로 이동.
SB_SetText("마을로 라깃이동")
PostRClick(420,330)
sleep,10
gosub, 차원체크
if( 현재차원 = CountPortal )
{
if(Gui_huntMummy = 1)
{
Get_Location()
if InStr(Location, "1F Cell")
{
현재차원 := CountPortal
gui_Startmap := 2
return
}
else if InStr(Location, "2F")
{
현재차원 := CountPortal
gui_Startmap := 2
return
}
else if InStr(Location, "1F Cell2")
{
현재차원 := CountPortal
gui_Startmap := 2
return
}
else
{
    Sleep, 10
}
}
}
else
{
    sleep,10
    현재차원 := CountPortal
}
Check_Map()
sleep,300
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
GuiControl, , Gui_NowState, 라깃 사용. . .
WriteExecutableMemory("퀵슬롯사용")
sleep,1
KeyClick(0)
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 300
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(630,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
gui_Startmap := 2
Sleep,2000
return
}
if(CountPortal = 1)
{
PostClick(645,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
gui_Startmap := 2
Sleep,2000
return
}
if(CountPortal = 2)
{
PostClick(660,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
gui_Startmap := 2
Sleep,2000
return
}
}
}
if(Step = 7 and gui_Startmap = 2)
{
GuiControl, , Gui_NowState, 초기 메모리CT 적용 중. . .
SB_SetText("메모리 적용")
빵 := 0
몸찌이동방지 := 0
gui_Startmap := 3
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
Sleep,500
Run,*RunAs %A_ScriptDir%\MRMSPH.exe
WinWait, ahk_exe MRMSPH.exe,,15
Sleep, 3000
IfWinExist, ahk_exe MRMSPH.exe
{
WinHide, ahk_exe MRMSPH.exe
}
if(Gui_1Muba = 1)
{
WriteExecutableMemory("1무바")
무바활성화()
sleep,100
}
if(Gui_2Muba = 1)
{
WriteExecutableMemory("2무바")
무바활성화()
sleep,100
}
if(Gui_3Muba = 1)
{
WriteExecutableMemory("3무바")
무바활성화()
sleep,100
}
if(Gui_2butMuba = 1)
{
WriteExecutableMemory("2벗무바")
무바활성화()
sleep,100
}
if(Gui_3butMuba = 1)
{
WriteExecutableMemory("3벗무바")
무바활성화()
sleep,100
}
if (Gui_4butMuba = 1)
{
;WriteExecutableMemory("4벗무바")
;무바활성화()
;sleep,100
SendInput, {F21}
sleep,100
}
WriteExecutableMemory("좌표이동")
sleep,1
WriteExecutableMemory("몬스터주소기록함수")
sleep,1
WriteExecutableMemory("몬스터주소기록켜기")
sleep,1
WriteExecutableMemory("공격하기")
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
sleep,100
Set_MoveSpeed()
sleep,100
WriteExecutableMemory("게임내시간제어")
	if ( jelan.read(0x0040FB07,"Uint", aOffsets*) != 402945257 ) ; 0x180474E9
	{
		jelan.write(0x0040FB07,0xE9,"Char", aOffsets*)
		jelan.write(0x0040FB08,0x74,"Char", aOffsets*)
		jelan.write(0x0040FB09,0x04,"Char", aOffsets*)
		jelan.write(0x0040FB0A,0x18,"Char", aOffsets*)
		jelan.write(0x0040FB0B,0x00,"Char", aOffsets*)
	}
if(Gui_jjOn = 1)
{
GuiControl, , Gui_NowState, 줍줍이 적용 중. . .
JJscript() ;MRMSPH영향 줍줍메모리적용스크립트
SetTimer, incineration, 250
}
;핫키 적용
SendInput, {F19}
sleep, 100
if(Gui_KON = 1)
{
gosub, OID읽기
Sleep,1
}
gosub, 어빌리티탭확인
Sleep,200
아이템읽어오기()
Sleep,200
어빌리티읽어오기()
Sleep,200
마법읽어오기()
Sleep,200
기술읽어오기()
Sleep,200
gosub, Check_좌표
Sleep,200
GuiControl, , Gui_NowState, 기본 메모리 적용 중. . .
Sleep,100
WinHide, ahk_exe MRMSPH.exe
WPdisablescript() ;무기탈거
incineratescript() ;MRMSPH영향 줍줍
SetFormat, integer, h
AbilityADD := jelan.processPatternScan(, 0x7FFFFFFF, 0xB0, 0x62, 0x53, 0x00, 0x01, 0x03, 0x00)
AbilityNameADD := AbilityADD + 0x64
AbilityValueADD := AbilityADD + 0x264
SetFormat, integer, d
Sleep, 100
GuiControl, , Gui_NowState, 어빌리티 인식 중. . .
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
GuiControl, , Gui_NowState, 캐릭터상태 인식 중. . .
if(Regloady = 0)
{
Get_HP()
CheckFirstHP := MaxHP
ProgramStartTime := A_TickCount
LastMessageTime := A_TickCount ;추가분
}
GuiControl, , Gui_NowState, 대화 설정 중 움직이지 마세요. . .
if(Regloady = 1)
{
CheckFirstHP := RCFH
ProgramStartTime := RPST
}
FirstCheck = 0
}
Send, !m
Sleep, 200
ime_status := % IME_CHECK("A")
if (ime_status = "0")  ; IME가 꺼져 있을 경우
{
    Send, {vk15sc138}  ; 한/영 키를 강제로 눌러 IME를 켭니다
    Sleep, 200          ; 전환을 위한 대기 시간 추가
    ime_status := IME_CHECK("A")  ; 다시 IME 상태 확인
    if (ime_status = "0")  ; 그래도 IME가 꺼져 있으면 다시 한/영 키를 눌러 강제 전환
    {
        Send, {vk15sc138}
        Sleep, 200
    }
}
WinActivate,ahk_pid %jPID%
Sleep, 150
Send, flshdk{Space}apsb{Space}{Tab}zldk{Space}apsb{Space}{Tab}znzl{Space}apsb{Space}{Tab}zmfhfltm{Space}apsb{Space}{Tab}flshtm{Space}apsb{Space}{Tab}emrhf{Space}apsb{Space}{Tab}wlrdjq{Space} {Tab}rlfdlfgdmstntoreo{Space}apsb{Space}{Enter}
Sleep, 100
GuiControl, , Gui_NowState, 인벤토리 인식 중. . .
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
If ( 아이템갯수["엘의축복포션(30일)"] <= 0 )
{
If ( 아이템갯수["엘의축복포션(7일)"] <= 0 )
{
If ( 아이템갯수["엘의축복포션(1일)"] <= 0 )
{
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 엘의축복포션이 없이 체잠중입니다. 엘의축복포션을 사먹여주세요"
        텔레그램메시지보내기(TMessage)
}
}
}
Step = 8
}
if(Step = 8)
{
GuiControl, , Gui_NowState, 인식한 어빌에 맞춰 체잠 초기 장소 세팅 중...
SB_SetText("체작장소 세팅")
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
if(Gui_HuntMummy = 1)
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
HuntPlace = 3
Step = 3000
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
HuntPlace = 3
Step = 3000
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
HuntPlace = 3
Step = 3000
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
HuntPlace = 3
Step = 3000
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
HuntPlace = 3
Step = 3000
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
HuntPlace = 3
Step = 3000
}
}
}
sleep,200
}
;=====크로노시스 체잠 파트=============
if(Step = 3000) ;머미시작
{
GuiControl, , Gui_NowState, [머미] 크로노시스로 가기
SB_SetText("차원체크" )
sleep,100
상승체력평균치 := (A_TickCount-ProgramStartTime)/1000
RunningTime := FormatSeconds((A_TickCount-ProgramStartTime)/1000)
상승체력평균값 := (CheckUPHP * 60) / (상승체력평균치/60)
GuiControl,,시작체력,%CheckFirstHP%
GuiControl,,상승체력,%CheckUPHP% (%상승체력평균값%)
GuiControl,,경과시간,%RunningTime%
CheckPN := 0
countsignal := 0
CheckPB = 0
MapNumber := 1
gosub, 차원체크
Check_Dimension()
if(차원체크 = "알파")
{
    현재차원 := 0
}
else if(차원체크 = "베타")
{
    현재차원 := 1
}
else if(차원체크 = "감마")
{
    현재차원 := 2
}
if( 현재차원 == CountPortal )
{
머미시작 := 1
MapNumber := 1
현재차원 := CountPortal
Get_Location()
if InStr(Location, "1F Cell")
{
Step = 3001
return
}
else if InStr(Location, "2F")
{
Step = 3001
return
}
else if InStr(Location, "1F Cell2")
{
Step = 3001
return
}
else
{
    sleep,100
}
}
else
{
머미시작 := 0
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
GuiControl, , Gui_NowState, [머미] 크로노시스 라깃 사용
WriteExecutableMemory("퀵슬롯사용")
sleep,100
KeyClick(0)
}
if(Ras = 1 and SelectRas = 0) ;라깃열렸으면
{
PostClick(428,406) ;크로노시스 클릭하는 곳
Sleep, 500
SelectRas = 1
}
if(Ras = 1 and SelectRas = 1) ;라깃열리고 크로노시스 눌렀으면
{
if(CountPortal = 0) ;알파면
{
PostClick(432,389) ;크로노시스 [ 알파 ]클릭하는 곳
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 3001
Sleep,500
}
if(CountPortal = 1) ;베타면
{
PostClick(447,384) ;크로노시스 [ 베타 ]클릭하는 곳 [수정해야하는 부분]
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 3001
Sleep,500
}
if(CountPortal = 2) ;감마면
{
PostClick(462,386)  ;크로노시스 [ 감마 ]클릭하는 곳 [수정해야하는 부분]
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 3001
Sleep,500
}
}
}
if(Step = 3001) ;위치확인
{
GuiControl, , Gui_NowState, [머미] 사냥터로 가기.
SB_SetText("차원 및 위치 확인 중")
keyclick("AltR")
if(Gui_CheckUseMagic = 1) ;마법이면
{
Send, {F17 Down}
Sleep, 200
Send, {F17 UP}
Send, {F17 Down}
Sleep, 200
Send, {F17 UP}
} ;아니면
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
Get_Location()
if(Gui_HuntMummy = 1)
{
Get_Location()
if InStr(Location, "1F Cell")
{
Step = 3055
return
}
else if InStr(Location, "2F")
{
Step = 3055
return
}
else if InStr(Location, "1F Cell2")
{
Step = 3055
return
}
else
{
    Sleep, 100
}
}
IfInString,Location,[알파차원] 크로노시스 마을 ;크로노시스 마을 맞는지 확인하는 부분 [체크필요]
{
Step = 3002
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
IfInString,Location,[베타차원] 크로노시스 마을
{
Step = 3002
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
IfInString,Location,[감마차원] 크로노시스 마을
{
Step = 3002
}
}
if(Step = 3002) ;도착한 크로노시스에서 라깃 및 설정 체크
{
SB_SetText("라깃 갯수 체크 중")
if(Gui_CheckUseMagic = 1) ;마법이면
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
if(RasCount <= 5) ; 라깃사러가기
{
Step = 100 ;[템뿌나 광피 때매 혹시몰라 포프로 사러 감] [ 마지막 체크필요 ]
}
if(RasCount > 5)
{
Step = 3055 ;메모리 체크
}
}
if(Step = 3055)
{
SB_SetText("메모리 점유율 체크 중")
Step = 3003

GetPrivateWorkingSet(jPID)
if(TotalPhy > 2000000)
{
if(byte > 1000000) ;초과시 끄기 [마지막 체크필요]
{
이전스텝 := step
step = 10000
}
if(byte <= 1000000)
{
step = 3003
}
}
if(TotalPhy <= 2000000)
{
if(byte > 620000)
{
이전스텝 := step
step = 10000  ;초과시 끄기 [마지막 체크필요]
}
if(byte <= 620000)
{
step = 3003
}
}
}
if(Step = 3003) ;파티 설정
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
if(Gui_PartyON = 1)
{
Move_StateForMount()
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
SLEEP, 100
PostClick(190,310)
SLEEP, 100
PostDClick(225,310)
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 100
}
else if(Gui_PartyOff = 1)
{
Move_StateForMount()
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 100
PostDClick(225,310)
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 100
}
Move_State() ;상태창
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 500
Check_State()
Check_StatePos()
if(StatePosX = 565 and StatePosY = 655 and State = 1)
{
if(Gui_CheckUseParty = 1)
{
Step = 910 ;파티하는 step [마지막 체크 필요]
}
if(Gui_CheckUseParty = 0)
{
Step = 3004  ;파티하는 step
}
}
}
if(Step = 3004) ; 소지품 체크하는 step
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
Step = 500  ; 식빵체크 부분 광피 어떨지 몰라서 포프감 [체크 필요]
return
}
}
get_FP()
if(NowFP = 0)
{
FPcount ++
if(nowFP = 0 && FPcount >= 20)
{
SB_SetText("FP 확인 중")
Sleep, 200
Step = 201 ; 식빵체크 부분 광피 어떨지 몰라서 포프감 [체크 필요]
return
}
}
if(Gui_CheckUseParty = 1)
{
party()
}
SB_SetText("[머미] 현재 위치 체크 중") ; 크로노시스 사냥터 초입 함수
if(BWValue0 = 9999) ;어빌리티 체크
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
sleep, 100
if(Gui_huntMummy = 1)
{
Get_Location()
if InStr(Location, "1F Cell")
{
Step = 3023
return
}
else if InStr(Location, "2F")
{
Step = 3023
return
}
else if InStr(Location, "1F Cell2")
{
Step = 3023
return
}
else
{
    Sleep, 1000
}
}
Step = 3005
sleep,100
}
if(Step = 3005) ;도착한 맵에서 정확히 멈춰있는지, 그리고 현재 맵이 크로노시스남동쪽인지 체크
{
GuiControl, , Gui_NowState, [머미] 남쪽 사냥터로 가기.
SB_SetText("남쪽 사냥터로 이동 중")
Check_Map()
if(Map = 1)
{
OpenMap()
}
Move_Map()
Sleep, 100
OpenMap()
PostClick(479,203)
PostClick(562,105)
PostClick(272,485)
OpenMap()
Sleep, 100
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
sleep,2000
Step = 3006
}
if(Step = 3006)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 1000
Check_Moving()
if(Moving = 0)
{
keyclick("AltR")
Step = 3007
}
}
}
if(Step = 3007)
{
SB_SetText("[머미] 현재위치가 크로노시스 남쪽인지 확인 중")
Get_Location()
if InStr(Location, "크로노시스 남쪽")
{
Step = 3008
}
else if InStr(Location, "크로노시스 남서쪽")
{
Step = 3011
return
}
else if InStr(Location, "크로노시스 마을") ; 좌표이동 함수 다시
{
Step = 3005
return
}
else if InStr(Location, "크로노시스 서쪽")
{
Step = 3000
return
}
}
if(STep = 3008)
{
GuiControl, , Gui_NowState, [머미] 남서쪽 사냥터로 가기.
Get_Location()
SB_SetText("남서쪽 사냥터로 이동 중")
if InStr(Location, "남쪽")
{
XX := 16
YY := 28
Z := 0
; XX값을 -1에서 1 사이의 무작위 값으로 변동
Random, randomY, -15, 3
YY := YY + randomY
; 좌표 입력
좌표입력(XX, YY, Z)
RunMemory("좌표이동")
sleep,100
Step = 3009
}
}
if(Step = 3009)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 100
Check_Moving()
if(Moving = 0)
{
keyclick("AltR")
Step = 3010
}
}
}
if(Step = 3010)
{
SB_SetText("[머미] 현재위치가 남서쪽인지 확인 중")
Get_Location()
if InStr(Location, "크로노시스 남서쪽")
{
Step = 3011
}
else if InStr(Location, "크로노시스 남쪽")
{
Step = 3008
}
else
{
Step = 3000
}
}
if(Step = 3011)
{
GuiControl, , Gui_NowState, [머미] 피라미드로 가기.
SB_SetText("피라미드로 이동 중")
XX := 43
YY := 92
Z := 0
; XX값을 -1에서 1 사이의 무작위 값으로 변동
Random, randomX, -1, 1
XX := XX + randomX
; 좌표 입력
좌표입력(XX, YY, Z)
RunMemory("좌표이동")
sleep,100
Step = 3012
}
if(Step = 3012)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 100
Check_Moving()
if(Moving = 0)
{
keyclick("AltR")
Step = 3013
}
}
}
if(Step = 3013)
{
SB_SetText("[머미] 현재위치가 피라미드B2인지 확인 중")
Get_Location()
IfInString,Location,B2
{
Step = 3014
}
else IfInString,Location,남서쪽
{
Step = 3011
}
else
{
Step = 3000
}
}
if(Step = 3014)
{
GuiControl, , Gui_NowState, [머미] 피라미드 B1로 가기.
;RandomMummy1()
;if(SelectedMummy1 = "왼쪽")
;{
SB_SetText("피라미드 B1 왼쪽 이동")
XX := 26
YY := 14
Z := 0
; XX값을 -1에서 1 사이의 무작위 값으로 변동
Random, randomX, -1, 1
XX := XX + randomX
좌표입력(XX, YY, Z)
RunMemory("좌표이동")
sleep,100
Step = 3015
;}
;else if(SelectedMummy1 = "오른쪽")
;{
;SB_SetText("피라미드 B1 오른쪽 이동")
;좌표입력(122,14,0)
;RunMemory("좌표이동")
;sleep,5000
;Step = 3015
}
if(Step = 3015)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 100
Check_Moving()
if(Moving = 0)
{
Sleep, 200
keyclick("AltR")
Step = 3016
}
}
}
if(Step = 3016)
{
SB_SetText("[머미] 현재위치가 피라미드B1인지 확인 중")
Get_Location()
IfInString,Location,피라미드 B1
{
Step = 3017
}
else IfInString,Location,피라미드 B2
{
Step = 3014
}
else
{
Step = 3000
}
}
if(Step = 3017)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 100
Check_Moving()
if(Moving = 0)
{
keyclick("AltR")
Step = 3018
}
}
}
if(Step = 3018)
{
GuiControl, , Gui_NowState, [머미] 피라미드 1층으로 가기.
SB_SetText("피라미드 1층 이동")
; 좌표
XX := 26
YY := 14
Z := 0
; XX값을 -1에서 1 사이의 무작위 값으로 변동
Random, randomX, -1, 1
XX := XX + randomX
; 좌표 입력
좌표입력(XX, YY, Z)
RunMemory("좌표이동")
sleep,100
Step = 3019
}
if(Step = 3019)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 100
Check_Moving()
if(Moving = 0)
{
keyclick("AltR")
Step = 3020
}
}
}
if(Step = 3020)
{
SB_SetText("[머미] 현재위치가 피라미드 1F인지 확인 중")
Get_Location()
IfInString,Location,피라미드 1F
{
Step = 3021
}
else IfInString,Location,피라미드 B1
{
Step = 3018
}
else
{
Step = 3000
}
}
if (Step = 3021)
{
    keyclick("AltR")
    if ( SelectedMummy2 = "" || SelectedMummy2 = 0 )
    {
    RandomMummy2()  ; 무작위 선택된 값 설정
    }
    GuiControl, , Gui_NowState, [머미] 피라미드 1층 %SelectedMummy2% 이동.
    if (SelectedMummy2 = "왼쪽")
    {
        SB_SetText("피라미드 1층 왼쪽 이동")
        XX := 32
        YY := 15
        Z := 0
        ; XX값을 -1에서 1 사이의 무작위 값으로 변동
        Random, randomX, -1, 1
        XX := XX + randomX
        ; 좌표 입력
        좌표입력(XX, YY, Z)
        RunMemory("좌표이동")
        Sleep, 100
        Step = 3022
    }
    else if (SelectedMummy2 = "중앙")  ; "가운데" 대신 "중앙"으로 변경
    {
        SB_SetText("피라미드 1층 중앙 이동")
        XX := 77
        YY := 15
        Z := 0
        ; XX값을 -1에서 1 사이의 무작위 값으로 변동
        Random, randomX, -1, 1
        XX := XX + randomX
        ; 좌표 입력
        좌표입력(XX, YY, Z)
        RunMemory("좌표이동")
        Sleep, 100
        Step = 3022
    }
    else if (SelectedMummy2 = "오른쪽")
    {
        SB_SetText("피라미드 1층 오른쪽 이동")
        XX := 118
        YY := 15
        Z := 0
        ; XX값을 -1에서 1 사이의 무작위 값으로 변동
        Random, randomX, -1, 1
        XX := XX + randomX
        ; 좌표 입력
        좌표입력(XX, YY, Z)
        RunMemory("좌표이동")
        Sleep, 100
        Step = 3022
    }
}
if(Step = 3022)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 100
Check_Moving()
if(Moving = 0)
{
keyclick("AltR")
Step = 3023
}
}
}
if(Step = 3023)
{
SB_SetText("[머미] 머미체잠장소 도착 확인 중")
Get_Location()
if InStr(Location, "1F Cell")
{
Step = 3024
return
}
else if InStr(Location, "1F") && InStr(Location, "2F")
{
Step = 3024
return
}
else if InStr(Location, "1F Cell2")
{
Step = 3024
return
}
else if InStr(Location, "1F")
{
Step = 3021
return
}
else
{
Step = 3000
}
}
if(Step = 3024)
{
GuiControl, , Gui_NowState, [머미] 체잠 시작.
SB_SetText("맵 이동 중")
머미캐릭()
if(Gui_CheckWPDMagic = 1)
{
WPD()
}
Get_Location()
if InStr(Location, "1F Cell")
{
머미맵선택 := 0 ; 왼쪽
if(MapNumber >= 79)
{
MapNumber = 1
Step = 3000
return
}
}
else if InStr(Location, "1F") && InStr(Location, "2F")
{
머미맵선택 := 1 ; 중앙
if(MapNumber >= 177)
{
MapNumber = 1
Step = 3000
return
}
}
else if InStr(Location, "1F Cell2")
{
머미맵선택 := 2 ; 오른쪽
if(MapNumber >= 92)
{
MapNumber = 1
Step = 3000
return
}
}
else if InStr(Location, "1F")
{
Step = 3021
return
}
CharMoveMummy()
Step = 3025
}
if(Step = 3025)
{
SB_SetText("움직임 체크 중")
Check_Moving()
if(Moving = 0)
{
Sleep, 10
Check_Moving()
if(Moving = 0)
{
keyclick("AltR")
Step = 3027
}
}
Get_Pos()
Get_MovePos()
거리범위 := 5
if (Abs(PosX - MovePosX) <= 거리범위 && Abs(PosY - MovePosY) <= 거리범위)
{
    MoveWaitCount := 0
    Step := 3027
}
if((PosX >= MovePosX and PosX <= MovePosX) and (PosY >= MovePosY and PosY <= MovePosY))
{
MoveWaitCount = 0
Step = 3027
}
}
if(Step = 3026)
{
Get_Pos()
Get_MovePos()
if((PosX >= MovePosX and PosX <= MovePosX) and (PosY >= MovePosY and PosY <= MovePosY))
{
MoveWaitCount = 0
Step = 3027
}
if(!((PosX >= MovePosX and PosX <= MovePosX) and (PosY >= MovePosY and PosY <= MovePosY)))
{
if(MoveWaitCount >= 2)
{
MoveWaitCount = 0
Step = 3000
}
else
{
Step = 3027
}
}
}
if(Step = 3027)
{
SB_SetText("몬스터 찾는 중")
IfWinNotActive, ahk_pid %jPID%
{
WinActivate, ahk_pid %jPID%
}
주황색 := 0xDE7D29
주황색2 := 0xC65D08
갈색 := 0xC6A6A5
갈색2 := 0x634142
AttackingCount3 := 0
PixelSearch, MobX, MobY, 350, 160, 410, 260, 주황색, 5, *ScanBR *Fast  *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 350, 160, 410, 260, 갈색, 5, *ScanBR *Fast  *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 350, 160, 410, 260, 갈색2, 5, *ScanBR *Fast  *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 350, 160, 410, 260, 주황색2, 5, *ScanBR *Fast  *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 360, 209, 437, 260, 주황색, 5, *ScanLB *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 360, 209, 437, 260, 갈색, 5, *ScanLB *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 360, 209, 437, 260, 주황색2, 5, *ScanLB *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 360, 209, 437, 260, 갈색2, 5, *ScanLB *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 362, 186, 432, 255, 갈색, 5, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 362, 186, 432, 255, 주황색, 5, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 362, 186, 432, 255, 갈색2, 5, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 362, 186, 432, 255, 주황색2, 5, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 333, 161, 460, 281, 갈색, 5, *ScanRT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 333, 161, 460, 281, 주황색, 5, *ScanRT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 333, 161, 460, 281, 주황색2, 5, *ScanRT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 333, 161, 460, 281, 갈색2, 5, *ScanRT *Fast *RGB
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
}
}
if(ErrorLevel = 0)
{
PostClick(MobX,MobY)
Monster_OID()
WinGetPos, ElanciaClientX, ElanciaClientY, Width, Height, ahk_pid %jPID%
SplashX := MobX + ElanciaClientX - 13
SplashY := MobY + ElanciaClientY + 15
SplashImage, %MobNumber%:, b X%SplashX% Y%SplashY% W60 H80 CW000000
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
Step = 3024
return
}
AttackLoopCount = 0
AttackCount = 0
Sleep, 10
movmob := A_TickCount
Step = 3029
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
Step = 3024
return
}
}
if(Step = 3028)
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
Step = 3027
return
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
Step = 3030
AttackingCount := A_TickCount
AttackingCount2 := A_TickCount
return
}
}
}
if(Step = 3029)
{
SB_SetText("몬스터가 가까이 있는지 확인 중")
Check_Moving()
if(Moving = 0)
{
sleep,100
Check_Moving()
if(Moving = 0)
{
keyclick("AltR")
Step = 3028
return
}
}
movmob2 := A_TickCount - movmob
if(movmob2 >= 2000)
{
SB_SetText("거리가 멉니다.")
Sleep, 50
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 3027
}
}
if(Step = 3030) ;무바파트
{
GUICONTROL, , Gui_NowState, [머미] 무바 중
SB_SetText("머미 무바 중" , 1)
if(Gui_1Muba = 1||Gui_2Muba = 1||Gui_3Muba = 1||Gui_2ButMuba = 1||Gui_3ButMuba = 1||Gui_4ButMuba = 1) ; 무바 수정
{
ReadAbilityNameValue()
if(AbilityName = "격투")
{
BWValue0 := AbilityValue
}
if(AbilityName = Gui_Weapon1 &&(Gui_1Muba = 1 || Gui_2butMuba = 1))
{
BWValue1 := AbilityValue
}
if(AbilityName = Gui_Weapon2&&(Gui_2Muba = 1 || Gui_3butMuba = 1))
{
BWValue2 := AbilityValue
}
if(AbilityName = Gui_Weapon3&&(Gui_3Muba = 1 || Gui_4butMuba = 1))
{
BWValue3 := AbilityValue
}
}
if(Gui_CheckUseMagic = 1)
{
if(BWValue0 = "격투" or Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프" || Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프" || Gui_Weapon3 = "현금" or Gui_Weapon3 =  "스태프")
{
RemoteM()
}
}
현재무기 := jelan.read(0x0058DAD4, "UInt", 0x121)
if (현재무기 != 0) ;4벗무바무기수리로직
{
    if(Gui_2Muba = 1 || Gui_3butMuba = 1||Gui_3Muba = 1 || Gui_4butMuba = 1)
    {
    TrackWeaponChange(현재무기)
    }
    if(Gui_1Muba = 1 || Gui_2butMuba = 1)
    {
    RepairWeaponCount = 0
    }
}
if (현재무기 = 0)
{
    if(Gui_1Muba = 1 || Gui_2butMuba = 1)
    {
    RepairWeaponCount += 1
    }
    else if(Gui_2Muba = 1 || Gui_3butMuba = 1)
    {
    RecentWeapons.RemoveAt(2)
    }
    else if(Gui_3Muba = 1 || Gui_4butMuba = 1)
    {
    RecentWeapons.RemoveAt(3)
    }
}
무바여부 := CheckTrackedWeapons()
if(Gui_1Muba = 1 || Gui_2butMuba = 1)
{
사용할무기수량 := 1
}
else if(Gui_2Muba = 1 || Gui_3butMuba = 1)
{
사용할무기수량 := 2
}
else if(Gui_3Muba = 1 || Gui_4butMuba = 1)
{
사용할무기수량 := 3
}
if (무바여부 = 사용할무기수량)
{
RepairWeaponCount := 0
}
if (무바여부 != 사용할무기수량)
{
RepairWeaponCount += 1
sleep,100
}
else
{
  RepairWeaponCount := 0
}
if (RepairWeaponCount >= 150)
{
RepairWeaponCount = 0
MapNumber = 1
Keyclick("tab")
step = 300
return
}
}
if(Step = 9 and gui_Startmap = 3)
{
GuiControl, , Gui_NowState, [포남] 사냥터로 가기.
sleep,100
value := jelan.write(0x0045D28F, 0xE9, "Char", aOffsets*)
value := jelan.write(0x0045D290, 0x8A, "Char", aOffsets*)
value := jelan.write(0x0045D291, 0x0A, "Char", aOffsets*)
value := jelan.write(0x0045D292, 0x00, "Char", aOffsets*)
value := jelan.write(0x0045D293, 0x00, "Char", aOffsets*)
CheckPN := 0
countsignal := 0
CheckPB = 0
랜덤감응 = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Get_Location()
if(Gui_huntPonam = 1 || Gui_HuntAuto = 1)
{
Get_Location()
if InStr(Location, "포프레스네 남쪽")
{
Step = 10
gui_Startmap := 4
return
}
else
{
    Sleep, 100
}
}
Step = 10
gui_Startmap := 4
}
if(Step = 9 and gui_Startmap = 4)
{
GuiControl, , Gui_NowState, [포남] 사냥터로 가기.
sleep,100
value := jelan.write(0x0045D28F, 0xE9, "Char", aOffsets*)
value := jelan.write(0x0045D290, 0x8A, "Char", aOffsets*)
value := jelan.write(0x0045D291, 0x0A, "Char", aOffsets*)
value := jelan.write(0x0045D292, 0x00, "Char", aOffsets*)
value := jelan.write(0x0045D293, 0x00, "Char", aOffsets*)
CheckPN := 0
countsignal := 0
CheckPB = 0
랜덤감응 = 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
gosub, 차원체크
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
GuiControl, , Gui_NowState, 라깃 사용. . .
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
SelectRas = 1
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(630,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 10
Sleep,500
return
}
if(CountPortal = 1)
{
PostClick(645,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 10
Sleep,500
return
}
if(CountPortal = 2)
{
PostClick(660,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 10
Sleep,500
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
Sleep, 30
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
STEP := 11
}
if(Step = 11)
{
타겟number := 0
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
if(RasCount <= 5)
{
Step = 100
}
if(RasCount > 5)
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
if(Gui_PartyON = 1)
{
Move_StateForMount()
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
SLEEP, 100
PostClick(190,310)
SLEEP, 100
PostDClick(225,310)
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 100
}
else if(Gui_PartyOff = 1)
{
Move_StateForMount()
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
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
if(StatePosX = 565 and StatePosY = 655 and State = 1)
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
if(NowFP = 0)
{
FPcount ++
if(nowFP = 0 && FPcount >= 20)
{
SB_SetText("FP 확인 중")
Sleep, 200
Step = 201
return
}
}
if(Gui_CheckUseParty = 1)
{
party()
}
if(Gui_KON = 1)
{
GuiControl, , Gui_NowState, [포남] 사냥터로 가기.
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
;if(byte <= 1000000)
;{
;}
}
if(TotalPhy <= 2000000)
{
if(byte > 620000)
{
이전스텝 := step
step = 10000
return
}
;if(byte <= 620000)
;{
;}
}
}
if(Gui_KON = 0 || 차원이동감응 = 1)
{
IfInString,Location,남쪽
{
    SB_SetText("현재 남쪽 위치함. 마을로 가기")
    keyclick("tab")
    Step = 9
    gui_Startmap = 4
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
Sleep, 200
Check_Moving()
if(Moving = 0)
{
AltR()
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
포북캐릭()
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
gosub,OID저장
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
gosub,OID저장
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
gosub,OID저장
return
}
}
ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16", aOffsets*)
IfInString,ServerMsg,서버와의 연결이
{
gosub,OID리셋
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
PixelSearch, MobX, MobY,  0, 0, 775, 460, 0x7351AD, 3, *Fast  *RGB
if(ErrorLevel = 1)
{
Sleep, 200
AltR()
Sleep, 200
PixelSearch, MobX, MobY,  0, 0, 775, 460, 0x7351AD, 3, *Fast  *RGB
}
if(ErrorLevel = 0)
{
Sleep, 300
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
        GuiControl, , jTitle, %jTitle%
        TMessage := "[ Helancia_Log ]>>" . jTitle "<<:" . 차원 . G리노아 "감마 리노아 감응 완료. "
        텔레그램메시지보내기(TMessage)
        sleep,100
        Step := 18
    }
    else if (Dimension > 10000)
    {
        차원 := "베타"
        B리노아 := NPCOID
        GuiControl,, B리노아, %B리노아%
        SB_SETTEXT(차원 . B리노아 "입력완료", 2)
        GuiControl, , jTitle, %jTitle%
        TMessage := "[ Helancia_Log ]>>" jTitle "<<:" . 차원 . B리노아 "베타 리노아 감응 완료. "
        텔레그램메시지보내기(TMessage)
        sleep,100
        Step := 18
    }
    else if (Dimension < 10000)
    {
        차원 := "알파"
        A리노아 := NPCOID
        GuiControl,, A리노아, %A리노아%
        SB_SETTEXT(차원 . A리노아 "입력완료", 2)
        GuiControl, , jTitle, %jTitle%
        TMessage := "[ Helancia_Log ]>>" jTitle "<<:" . 차원 . A리노아 "알파 리노아 감응 완료. "
        텔레그램메시지보내기(TMessage)
        sleep,100
        Step := 18
    }
}
Sleep, 500
}
if(Gui_KON = 1)
{
if(ipmak > 3)
{
GUICONTROL, , Gui_NowState, 리노아 호출오류 감응 OFF
SLEEP, 500
        TMessage := "[ Helancia_Log ]>>" jTitle "<<:알파 리노아 호출 오류"
        텔레그램메시지보내기(TMessage)
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
ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16", aOffsets*)
if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
{
gosub,OID리셋
차원이동감응 := 1
GuiControl, ,Gui_KOFF, 1
}
}
NPCTalkTime := A_TickCount - NPCTalkedTime
if(NPCTalkTime >= 5000)
{
AltR()
Sleep, 500
ipmak += 1
Step = 13
return
}
Check_FormNumber()
Check_NPCMsg()
Sleep, 100
PostClick(395,325)
Sleep, 100
if(FormNumber = 97)
{
IfInString,NPCMsg,100
{
Entrance = 0
Sleep, 100
PostClick(90,80)
Sleep, 100
GuiControl, , Gui_NowState, [포남] 사냥터에 도착하였습니다.
SB_SetText("포남 사냥터에 입장 하였습니다.")
Entrance = 0
PostMessage, 0x100, 54, 458753, , ahk_pid %jPID%
PostMessage, 0x101, 54, 458753, , ahk_pid %jPID%
JoinTime := A_TickCount
if(Gui_jjOn = 1)
{
Send, {F18 Down}
Sleep, 40
Send, {F18 Up}
Sleep, 10
Send, {F18 Down}
Sleep, 40
Send, {F18 Up}
PickUp_itemsetPS()
}
Step = 19
if(Gui_KON = 1)
{
Sleep, 300
Get_Location()
IfInString,Location,남쪽
{
Send, {F14}
Sleep, 100
Send, {F14}
Sleep, 100
SB_SetText("좌표 이동")
XX := 127
YY := 27
Z := 1
; XX값을 -1에서 1 사이의 무작위 값으로 변동
Random, randomY, -3, 3
YY := YY + randomY
좌표입력(XX, YY, Z)
RunMemory("좌표이동")
Sleep, 2000
GuiControl, , jTitle, %jTitle%
GuiControl,,시작체력,%CheckFirstHP%
GuiControl,,상승체력,%CheckUPHP% (%상승체력평균값%)
GuiControl,,경과시간,%RunningTime%
CheckPN = 1
Sleep, 1000
}
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
            Sleep, 2000
            IfWinNotActive, ahk_pid %jPID%
            {
            WinActivate, ahk_pid %jPID%
            }
            PixelSearch, MobX, MobY,  310, 100, 580, 235, 0xEF8AFF, 5,Fast
            if(ErrorLevel = 1)
            {
            AltR()
            sleep,100
            PixelSearch, MobX, MobY,  0, 0, 760, 450, 0xEF8AFF, 5, Fast
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
            Sleep, 3500
            step = 1061
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
                    GuiControl, , jTitle, %jTitle%
                    TMessage :=  "[ Helancia_Log ]>>" jTitle "<<:" . 차원 . G동파 "감마 동쪽파수꾼 감응 완료. "
                    텔레그램메시지보내기(TMessage)
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
                    GuiControl, , jTitle, %jTitle%
                    TMessage := "[ Helancia_Log ]>>" jTitle "<<:" 차원 . B동파 "베타 동쪽파수꾼 감응 완료. "
                    텔레그램메시지보내기(TMessage)
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
                    GuiControl, , jTitle, %jTitle%
                    TMessage := "[ Helancia_Log ]>>" jTitle "<<:" . 차원 . A동파 "알파 동쪽파수꾼 감응 완료. "
                    텔레그램메시지보내기(TMessage)
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
                step = 10000
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
                step = 10000
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
                    PixelSearch, MobX, MobY,  0, 0, 760, 450, 0xEF8AFF, 1, Fast
                    if(ErrorLevel = 1)
                    {
                    AltR()
                    Sleep,200
                    PixelSearch, MobX, MobY,  0, 0, 760, 450, 0xEF8AFF, 1, Fast
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
                    GuiControl, , jTitle, %jTitle%
                    TMessage := "[ Helancia_Log ]>>" jTitle "<<:" 차원 . G서파 "감마 서쪽파수꾼 감응 완료. "
                    텔레그램메시지보내기(TMessage)
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
                    GuiControl, , jTitle, %jTitle%
                    TMessage := "[ Helancia_Log ]>>" jTitle "<<:" 차원 . B서파 "알파 서쪽파수꾼 감응 완료. "
                    텔레그램메시지보내기(TMessage)
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
                    GuiControl, , jTitle, %jTitle%
                    TMessage := "[ Helancia_Log ]>>" jTitle "<<:" 차원 . A서파 "알파 서쪽파수꾼 감응 완료. "
                    텔레그램메시지보내기(TMessage)
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
                step = 10000
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
            gosub,OID저장
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
            gosub,OID저장
            return
            }
            ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16",aOffsets*)
            if InStr(ServerMsg, "서버와의 연결이")
 || InStr(ServerMsg, "오랜 시간 아무것도 하지 않으면")
 || InStr(ServerMsg, "인증시간이")
            {
                step = 10000
                ipmak += 1
                return
            }
        }
    }
}
}
}
}
if(Step = 19)
{
SB_SetText("맵 이동 중")
if(Gui_CheckWPDMagic = 1)
{
WPD()
}
if(Gui_MoveLoute1 = 1)
{
if(Aloute = 1)
{
if(MapNumber >= 159) ; 똘룸체잠 원래 루트 -> 가로세로 루트
{
MapNumber = 1
Step = 9
return
}
}
if(Bloute = 1)
{
if(MapNumber >= 285) ;수정한거
{
MapNumber = 1
Step = 9
return
}
}
if(Cloute = 1)
{
if(MapNumber >= 256) ;수정한거 2
{
MapNumber = 1
Step = 9
return
}
}
}
if(Gui_MoveLoute2 = 1) ;구석구석만 돌아다니는 방향
{
if(MapNumber >= 55)
{
MapNumber = 1
Step = 9
return
}
}
if(Gui_MoveLoute3 = 1) ; 세로 지그재그 방향
{
if(MapNumber >= 160)
{
MapNumber = 1
Step = 9
return
}
}
if(Gui_MoveLoute4 = 1) ; 아래쪽만 공략하는 방향
{
if(MapNumber >= 86)
{
MapNumber = 1
Step = 9
return
}
}
CharMovePonam(Gui_MoveLoute1,Gui_MoveLoute2,Gui_MoveLoute3,Gui_MoveLoute4)
Step = 20
}
if(Step = 20)
{
SB_SetText("움직임 체크 중")
OID이상 = 0
ipmak = 0
Check_Moving()
Get_Pos()
Get_MovePos()
if(Gui_MoveLoute1 = 1)
{
거리범위 := 5
}
if(Gui_MoveLoute2 = 1) ;구석구석만 돌아다니는 방향
{
거리범위 := 10
}
if(Gui_MoveLoute3 = 1) ; 세로 지그재그 방향
{
거리범위 := 6
}
if(Gui_MoveLoute4 = 1) ; 아래쪽만 공략하는 방향
{
거리범위 := 10
}
if(Moving = 0)
{
SLEEP, 200
Check_Moving()
if(Moving = 0)
{
Step = 21
한번만 := 1
}
}
if (Abs(PosX - MovePosX) <= 거리범위 && Abs(PosY - MovePosY) <= 거리범위)
{
    MoveWaitCount := 0
    한번만 := 1
    Step := 24
}
if((PosX >= MovePosX and PosX <= MovePosX) and (PosY >= MovePosY and PosY <= MovePosY))
{
MoveWaitCount = 0
한번만 := 1
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
한번만 := 1
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
한번만 := 1
Step = 24
}
}
}
if(Step = 24)
{
GuiControl, , Gui_NowState, [포남] 몹 딱 대라 . . .
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
;CheckPN := 0
Monster_OID()
WinGetPos, ElanciaClientX, ElanciaClientY, Width, Height, ahk_pid %jPID%
SplashX := MobX + ElanciaClientX - 30
SplashY := MobY + ElanciaClientY - 20
if (MobNumber <= 10) {
    SplashImage, %MobNumber%:, b X%SplashX% Y%SplashY% W80 H80 CW000000
    MobNumber += 1
}
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
keyclick("AltR")
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
GUICONTROL, , Gui_NowState, [포남] 몹 공격 체크 중
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
AttackMissCount := 0
한번만 := 1
}
}
}
if(Step = 26) ;포남무바수정파트
{
GUICONTROL, , Gui_NowState, [포남] 몹 근접 체크 중
SB_SetText("몹 근접 체크 중")
;keyclick("AltR")
Check_Moving()
if(Moving = 0)
{
Check_Moving()
if(Moving = 0)
{
keyclick("AltR")
Step = 27
}
}
movmob2 := A_TickCount - movmob
SB_SetText("거리가 멉니다.")
if(movmob2 >= 2500)
{
sleep, 100
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 24
return
}
}
if(Step = 27) ;포남 무바 파트
{
GUICONTROL, , Gui_NowState, [포남] 무바 중
SB_SetText("포남 메모리 무바 중", 1)
AttackMissCount ++
if(AttackMissCount >= 800 and 한번만 = 1)
{
    keyclick("AltR")
    AttackMissCount := 0
    한번만 :=0
}
if(gui_1muba = 1)
{
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
}
if(gui_2muba = 1)
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
}
if(gui_3muba = 1)
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
}
if(Gui_2butmuba = 1)
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
}
if(Gui_3butmuba = 1)
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
}
if(Gui_4butMuba = 1)
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
if(AbilityName = Gui_Weapon3)
{
BWValue3 := AbilityValue
}
}
if(Gui_CheckUseMagic = 1)
{
if(BWValue0 = "격투" or Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프" || Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프" || Gui_Weapon3 = "현금" or Gui_Weapon3 =  "스태프")
{
RemoteM()
}
}
현재무기 := jelan.read(0x0058DAD4, "UInt", 0x121)
if (현재무기 != 0) ;4벗무바무기수리로직
{
    if(Gui_2Muba = 1 || Gui_3butMuba = 1||Gui_3Muba = 1 || Gui_4butMuba = 1)
    {
    TrackWeaponChange(현재무기)
    }
    if(Gui_1Muba = 1 || Gui_2butMuba = 1)
    {
    RepairWeaponCount = 0
    }
}
if (현재무기 = 0)
{
    if(Gui_1Muba = 1 || Gui_2butMuba = 1)
    {
    RepairWeaponCount += 1
    }
    else if(Gui_2Muba = 1 || Gui_3butMuba = 1)
    {
    RecentWeapons.RemoveAt(2)
    }
    else if(Gui_3Muba = 1 || Gui_4butMuba = 1)
    {
    RecentWeapons.RemoveAt(3)
    }
}
무바여부 := CheckTrackedWeapons()
if(Gui_1Muba = 1 || Gui_2butMuba = 1)
{
사용할무기수량 := 1
}
else if(Gui_2Muba = 1 || Gui_3butMuba = 1)
{
사용할무기수량 := 2
}
else if(Gui_3Muba = 1 || Gui_4butMuba = 1)
{
사용할무기수량 := 3
}
if (무바여부 = 사용할무기수량)
{
RepairWeaponCount := 0
}
if (무바여부 != 사용할무기수량)
{
RepairWeaponCount += 1
}
else
{
  RepairWeaponCount := 0
}
if (RepairWeaponCount >= 800)
{
RepairWeaponCount = 0
MapNumber = 1
step = 300
return
}
}
if(Step = 30)
{
SB_SetText("감응 버프 받는 중")
CheckPN := 0
RepairWeaponCount := 0
countsignal := 0
Sleep, 300
Keyclick("tab")
Sleep, 300
Step = 31
}
if(step = 31)
{GuiControl, , Gui_NowState, [포남] 감응 전 메모리 점유율 확인

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
    gosub, 감응
    sleep,500
    TMessage := "[ Helancia_Log ]>>" jTitle "<<: [원격] 감응 성공.[" 호출대상 ":" countsignal "]"  Location "시작 체력 : " . CheckFirstHP . " / 상승 체력 : " . CheckUPHP . " ( " . 상승체력평균값 . " ) " . " / 경과 시간 : " . RunningTime
    텔레그램메시지보내기(TMessage)
    sleep,200
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
if(step = 750)
{
GuiControl, , Gui_NowState, [병원] 체력회복을 위한 이동 중.
SB_SetText("병원이동 - 회복하러 이동 중")
CheckPB := 0
CheckPN := 0
countsignal := 0
Check_Map()
if(Map = 1)
{
OpenMap()
Sleep, 100
}
Check_Ras()
if(Ras = 0)
{
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Sleep, 100
PostMessage, 0x100, 48, 720897, , ahk_pid %jPID%
PostMessage, 0x101, 48, 720897, , ahk_pid %jPID%
Sleep, 100
}
if(Ras = 1 and SelectRas = 0)
{
PostClick(625,365)
Sleep, 100
}
if(Ras = 1 and SelectRas = 1)
{
if(CountPortal = 0)
{
PostClick(630,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 751
return
}
if(CountPortal = 1)
{
PostClick(645,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 751
return
}
if(CountPortal = 2)
{
PostClick(660,345)
라깃카운트 := 라깃카운트-1
GuiControl, , Gui_RasCount, %라깃카운트%
Step = 751
return
}
}
IfInString,Location,신전
{
keyclick("프로세스종료")
step = 751
return
}
}
if(Step = 751)
{
Check_NPCMsg()
IfInString,NPCMsg,라스의깃
{
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMsph.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, 타겟팅, Off
SetTimer, incineration, off
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
return
}
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle  "<<: 체력이" . NowHP . "가 되어 병원 회복"
텔레그램메시지보내기(TMessage)
Get_Location()
IfInString,Location,[알파차원] 포프레스네 마을
{
Step = 752
}
IfInString,Location,[베타차원] 포프레스네 마을
{
Step = 752
}
IfInString,Location,[감마차원] 포프레스네 마을
{
Step = 752
}
IfInString,Location,병원
{
Step = 754
}
IfInString,Location,신전
{
keyclick("프로세스종료")
step = 751
return
}
}
if(Step = 752)
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
PostClick(465,400)
OpenMap()
Sleep, 500
Check_Map()
Sleep, 5000
Step = 753
}
if(Step = 753)
{
Check_Moving()
if(Moving = 0)
{
Sleep, 200
Check_Moving()
if(Moving = 0)
{
Step = 754
}
}
}
if(Step = 754)
{
SB_SetText("현재위치가 병원인지 확인 중")
Get_Location()
IfInString,Location,포프레스네 병원
{
Step = 755
}
IfNotInString,Location,포프레스네 병원
{
AltR()
Step = 751
}
}
if(Step = 755)
{
GuiControl, , Gui_NowState, [병원] 병원 도착.
SB_SetText("병원 - 현재소지 갈리드 체크 중")
Get_Gold()

if(Gold <= 100000)
{
Step = 500
}
else
{
Step = 756
}
}
if(Step = 756)
{
GuiControl, , Gui_NowState, [병원] 병원 위치 이동
SB_SetText("자리이동 중")
좌표입력(26,24,0)
RunMemory("좌표이동")
Sleep, 1500
Step = 757
}
if(Step = 757)
{
좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
GuiControl, , Gui_NowState, [병원] 위치 확인 중 . .
SB_SetText("몸찌방지 자리확인 중")
Sleep,500
if((좌표X = 26 and 좌표Y = 24))
{
Step = 758
Sleep, 100
}
else
{
Step = 756
Sleep, 100
}
}
if(Step = 758)
{
GuiControl, , Gui_NowState, [병원] 치료 대화 중 . .
SB_SetText("치료 시작")
PostClick(324,133)
Sleep, 200
PostClick(358,91)
Sleep, 500
PostClick(386,319)
Sleep, 500
keyclick("K6")
Sleep, 200
step = 759
}
if(Step = 759)
{
Get_HP()
if(NowHP != MaxHP)
{
Sleep, 200
Step = 756
}
if(NowHP = MaxHP)
{
GuiControl, , Gui_NowState, [병원] 치료 완료 . .
SB_SetText("HP채우기 완료")
Sleep, 200
Step = 760
}
}
if(Step = 760)
{
SB_SetText("병원 밖으로 이동 중")
좌표입력(32,31,0)
RunMemory("좌표이동")
Sleep,1500
step = 761
AltR()
}
if(Step = 761)
{
SB_SetText("병원을 나왔는지 체크 중")
Get_Location()
IfInString,Location,병원
{
AltR()
Step = 760
}
IfNotInString,Location,병원
{
Step = 762
}
}
if(Step = 762)
{
TMessage := "[ Helancia_Log ]>>" jTitle  "<<: 체력 회복 완료, 다시 사냥하러 갑니다."
텔레그램메시지보내기(TMessage)
sleep,500
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
Step = 763
}
if(Step = 763)
{
if(HuntPlace = 1)
{
Step = 11
}
if(HuntPlace = 2)
{
Step = 1002
}
if(HuntPlace = 3)
{
Step = 3000
}
}
if(Step = 100)
{
GuiControl, , Gui_NowState, [잡화점] 상점으로 이동 중.
SB_SetText("라깃구매 - 잡화점으로 이동 중")
Get_Location()
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
GuiControl, , Gui_NowState, [잡화점] 몸찌방지 이동. . .
좌표입력(41,30,0)
RunMemory("좌표이동")
Sleep, 3500
Step = 1116
}
if(Step = 1116)
{
좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
좌표Z := jelan.read(0x0058DAD4, "UInt", 0x18)
GuiControl, , Gui_NowState, [잡화점] 몸찌방지 자리 확인 중. . .
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
RasCount := RasCount+55
GuiControl, , Gui_RasCount, %RasCount%
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
if(HuntPlace = 3)
{
Step = 3000
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
CheckPN := 0
countsignal := 0
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
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 201
return
}
if(CountPortal = 1)
{
PostClick(645,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 201
return
}
if(CountPortal = 2)
{
PostClick(660,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
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
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<:라깃 부족. 게임 확인 요망."
텔레그램메시지보내기(TMessage)
IfWinExist,ahk_pid %jPID%
{
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMsph.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 부족합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
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
GuiControl, , Gui_NowState, [베이커리] 상점 이동
SB_SetText("베이커리 이동")
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
Step = 1111
}
}
if(Step = 1111)
{
GuiControl, , Gui_NowState, [베이커리] 상점 위치 이동
SB_SetText("자리이동 중")
좌표입력(32,30,0)
RunMemory("좌표이동")
Sleep, 100
Step = 1112
}
if(Step = 1112)
{
좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
좌표Z := jelan.read(0x0058DAD4, "UInt", 0x18)
SB_SetText("자리확인 중")
if (Abs(좌표X = 32) <= 2 && Abs(좌표Y = 30) <= 2 && 좌표Z = 0)
{
SB_SetText("자리 도착")
Step = 206
}
else
{
    step = 1111
}
}
if(Step = 206)
{
SB_SetText("식빵 구매 중")
몸찌이동인식 = 0
Move_Buy()
Sleep, 100
PostMessage, 0x100, 17, 1900545, , ahk_pid %jPID%
PostMessage, 0x100, 51, 262145, , ahk_pid %jPID%
PostMessage, 0x101, 51, 262145, , ahk_pid %jPID%
PostMessage, 0x101, 17, 1900545, , ahk_pid %jPID%
Sleep, 500
ShopOpendTime := A_TickCount
Step = 207
}
if(Step = 207)
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
Step = 208
}
}
if(NPCMenu = 0)
{
ShopOpenTime := A_TickCount - ShopOpendTime
if(ShopOpenTime >= 10000)
{
Step = 206
}
}
}
if(Step = 208)
{
Check_Shop()
if(Buy = 1)
{
Sleep, 500
빵++
PostClick(115,60)
Step = 209
Sleep,500
}
if(Buy = 0)
{
BuyCheckTime := A_TickCount - BuyCheckedTime
if(BuyCheckTime >= 10000)
{
Step = 206
}
}
}
if(Step = 209)
{
Get_inven()
if(빵 = 1)
{
if(Nowinven != 50)
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
PostClick(424,323)
KeyClick("Enter")
Sleep, 300
PostMessage, 0x100, 27, 65537, , ahk_pid %jPID%
PostMessage, 0x101, 27, 65537, , ahk_pid %jPID%
Sleep, 300
Step = 206
}
if(Nowinven = 50)
{
Sleep,100
Step = 210
빵 = 1
}
}
if(빵 > 1)
{
Get_inven()
if(InvenCount != 50)
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
Loop,
{
PostClick(424,323)
Sleep,10
POSTMESSAGE,0x100,13,1835009 ,,ahk_pid %jPID%
POSTMESSAGE,0x101,13,1835009 ,,ahk_pid %jPID%
Sleep,10
POSTMESSAGE,0x100,13,1835009 ,,ahk_pid %jPID%
POSTMESSAGE,0x101,13,1835009 ,,ahk_pid %jPID%
Sleep,10
Get_inven()
if(Nowinven = 50)
{
SLEEP,100
Step = 210
빵 = 1
break
}
}
}
}
}
if(Step = 210)
{
Check_Shop()
if(Buy = 1)
{
Sleep, 300
PostMessage, 0x100, 27, 65537, , ahk_pid %jPID%
PostMessage, 0x101, 27, 65537, , ahk_pid %jPID%
Sleep, 300
Step = 211
}
}
if(Step = 211)
{
Check_Shop()
if(Buy = 0)
{
Step = 212
}
if(Buy = 1)
{
Step = 210
}
}
if(Step = 212)
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
Step = 213
}
}
if(Step = 213)
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
Step = 2141
}
}

if(Step = 2141)
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
Step = 2142
}
if(Step = 2142)
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
Step = 2143
}
}
if(NPCMenu = 0)
{
ShopOpenTime := A_TickCount - ShopOpendTime
if(ShopOpenTime >= 10000)
{
Step = 2141
}
}
}
if(Step = 2143)
{
Check_Shop()
if(Buy = 1)
{
Sleep, 500
빵++
PostClick(115,60)
Step = 2144
Sleep,500
}
if(Buy = 0)
{
BuyCheckTime := A_TickCount - BuyCheckedTime
if(BuyCheckTime >= 10000)
{
Step = 2141
}
}
}
if(Step = 2144)
{
Get_inven()
if(빵 = 1)
{
if(Nowinven < 38)
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
PostClick(424,323)
KeyClick("Enter")
Sleep, 300
PostMessage, 0x100, 27, 65537, , ahk_pid %jPID%
PostMessage, 0x101, 27, 65537, , ahk_pid %jPID%
Sleep, 300
Step = 2141
}
if(Nowinven > 38)
{
Sleep,100
Step = 2145
빵 = 1
}
}
if(빵 > 1)
{
Get_inven()
if(InvenCount < 38)
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
Loop,
{
PostClick(424,323)
Sleep,10
POSTMESSAGE,0x100,13,1835009 ,,ahk_pid %jPID%
POSTMESSAGE,0x101,13,1835009 ,,ahk_pid %jPID%
Sleep,10
POSTMESSAGE,0x100,13,1835009 ,,ahk_pid %jPID%
POSTMESSAGE,0x101,13,1835009 ,,ahk_pid %jPID%
Sleep,10
Get_inven()
if(Nowinven > 38)
{
SLEEP,100
Step = 2145
빵 = 1
break
}
}
}
}
}
if(Step = 2145)
{
Check_Shop()
if(Buy = 1)
{
Sleep, 300
PostMessage, 0x100, 27, 65537, , ahk_pid %jPID%
PostMessage, 0x101, 27, 65537, , ahk_pid %jPID%
Sleep, 300
Step = 2146
}
}
if(Step = 2146)
{
Check_Shop()
if(Buy = 0)
{
Step = 214
}
if(Buy = 1)
{
Step = 2145
}
}

if(Step = 214)
{
SB_SetText("상점 밖으로 이동 중")
좌표입력(32,31,0)
RunMemory("좌표이동")
Sleep,500
Step = 215
}
if(Step = 215)
{
SB_SetText("베이커리 상점을 나왔는지 체크 중")
Get_Location()
IfInString,Location,베이커리
{
AltR()
Step = 214
}
IfNotInString,Location,베이커리
{
좌표입력(56,93,1)
RunMemory("좌표이동")
Sleep,500
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 식빵 정상 구매 완료. 다시 사냥하러 갑니다."
텔레그램메시지보내기(TMessage)
if(HuntPlace = 1)
{
Step = 11
}
if(HuntPlace = 2)
{
Step = 1002
}
if(HuntPlace = 3)
{
Step = 3000
}
}
}
if(Step = 300)
{
GuiControl, , Gui_NowState, [수리점] 상점으로 이동 중.
SB_SetText("무기수리 - 수리점으로 이동 중")
CheckPB = 0
CheckPN := 0
countsignal := 0
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
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 301
return
}
if(CountPortal = 1)
{
PostClick(645,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 301
return
}
if(CountPortal = 2)
{
PostClick(660,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
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
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 라스의깃 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMsph.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
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
GuiControl, , Gui_NowState, [수리점] 상점 도착. . .
좌표입력(32,30,0)
RunMemory("좌표이동")
Sleep, 100
Step = 1114
}
if(Step = 1114)
{
GuiControl, , Gui_NowState, [수리점] 좌표 확인
좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
좌표Z := jelan.read(0x0058DAD4, "UInt", 0x18)
SB_SetText("몸찌 자리확인 중")
if (Abs(좌표X = 32) <= 3 && Abs(좌표Y = 30) <= 3 && 좌표Z = 0)
{
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<< : 정상 수리장소 도착."
텔레그램메시지보내기(TMessage)
Step = 306
SB_SetText("자리 도착")
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
Sleep, 300
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
Sleep, 500
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
좌표입력(75,69,1)
RunMemory("좌표이동")
sleep,200
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<< : 정상 수리 완료."
텔레그램메시지보내기(TMessage)
sleep,100
Step = 314
}
}
if(Step = 314)
{
Sleep, 200
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
if(HuntPlace = 3)
{
Step = 3000
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
CheckPN := 0
countsignal := 0
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
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
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
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 라스의깃 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMsph.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
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
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 골드바 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
}
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
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
if(HuntPlace = 3)
{
Step = 3000
}
}
}
if(Step = 550)
{
GuiControl, , Gui_NowState, [은행] 상점으로 이동 중.
SB_SetText("골드바 > 갈리드로 변경하러 가는 중")
CheckPB = 0
CheckPN := 0
countsignal := 0
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
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
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
WINKILL, ahk_exe MRMsph.exe
GuiControl, , jTitle, %jTitle%
TMessage :="[ Helancia_Log ]>>" jTitle "<<: 라스의깃 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
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
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 골드바 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
}
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
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
if(HuntPlace = 3)
{
Step = 3000
}
}
}
if(Step = 800)
{
GuiControl, , Gui_NowState, [은행] 상점으로 이동 중.
SB_SetText("강제그렐 골드바 > 갈리드 이동중")
CheckPB = 0
CheckPN := 0
countsignal := 0
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
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
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
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 라스의깃 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMsph.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
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
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 골드바 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
GuiControl, , Gui_NowState, 골드바가 없어 종료합니다.
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
}
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
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
CheckPN := 0
countsignal := 0
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
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 601
return
}
if(CountPortal = 1)
{
PostClick(645,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 601
return
}
if(CountPortal = 2)
{
PostClick(660,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
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
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 라스의깃 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMsph.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
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
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 갈리드 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
GuiControl, , Gui_NowState, 갈리드가 부족하여 종료합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("갈리드 부족")
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
}
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
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
SB_SetText("신전 밖으로 이동 중") ;신전밖 안나가는 이슈 해결
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
if(BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2 or BWValue3 >= Gui_LimitAbility35)
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
}
if(Gui_HuntPonam = 1)
{
HuntPlace = 1
Step = 11
}
if(Gui_HuntMummy = 1)
{
HuntPlace = 3
Step = 3003
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
CheckPN := 0
countsignal := 0
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
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 651
return
}
if(CountPortal = 1)
{
PostClick(645,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 651
return
}
if(CountPortal = 2)
{
PostClick(660,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
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
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 라스의깃 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMsph.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
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
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 갈리드 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
GuiControl, , Gui_NowState, 갈리드가 부족하여 종료합니다.
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
}
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
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
}
}
if(Step = 658)
{
SB_SetText("스펠 그레이드할 어빌리티 체크 중")
if(MagicAbility3 = 100 or MagicAbility4 = 100 or MagicAbility5 = 100 or MagicAbility6 = 100 or MagicAbility7 = 100 or MagicAbility8 = 100)
{
Step = 655
}
if(MagicAbility3 != 100 and MagicAbility4 != 100 and MagicAbility5 != 100 and MagicAbility6 != 100 and MagicAbility7 != 100 and MagicAbility3=8 != 100)
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
if(BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2 or BWValue3 >= Gui_LimitAbility35)
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
if(Gui_HuntMummy = 1)
{
if(Gui_1Muba = 1)
{
if(BWValue1 < Gui_LimitAbility1)
{
HuntPlace = 3
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
HuntPlace = 3
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
HuntPlace = 3
Step = 8
}
if(BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2 or BWValue3 >= Gui_LimitAbility35)
{
HuntPlace = 2
Step = 8
}
}
if(Gui_2ButMuba = 1)
{
if(BWValue0 < Gui_LimitAbility0 and BWValue1 < Gui_LimitAbility1)
{
HuntPlace = 3
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
HuntPlace = 3
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
if(BWValue0 < Gui_LimitAbility0 and BWValue1 < Gui_LimitAbility1 and BWValue2 < Gui_LimitAbility2)
{
HuntPlace = 3
Step = 8
}
if(BWValue0 >= Gui_LimitAbility0 or BWValue1 >= Gui_LimitAbility1 or BWValue2 >= Gui_LimitAbility2)
{
HuntPlace = 2
Step = 8
}
}
}
}
}
if(Step = 700)
{
GuiControl, , Gui_NowState, [신전/성당] 기도하러 가는 중.
SB_SetText("강제그레이드 - 신전으로 이동 중")
CheckPB = 0
CheckPN := 0
countsignal := 0
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
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 701
return
}
if(CountPortal = 1)
{
PostClick(645,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 701
return
}
if(CountPortal = 2)
{
PostClick(660,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
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
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 라스의깃 부족. 강제 종료, 위치보고: " . "(" . 현재차원 . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
텔레그램메시지보내기(TMessage)
WINKILL, ahk_pid %jPID%
WINKILL, ahk_exe MRMsph.exe
}
GUICONTROL, , Gui_NowState, 라스의깃이 없어 강제 종료 합니다.
GuiControl, , 로그인상태정보, 자동정지
SB_SetText("라깃 부족")
Gui_Enable()
SetTimer, Hunt, Off
SetTimer, AttackCheck, Off
SetTimer, AttackMGB, off
SetTimer, incineration, off
SetTimer, GetMemory, OFF
SetTimer, ClearMem, OFF
SetTimer, 타겟팅, OFF
SetTimer, RL, OFF
CheckPB = 0
CheckPN := 0
countsignal := 0
return
}
Get_Location()
IfInString,Location,포프레스네 마을
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
if(Step = 1000 and gui_Startmap = 3)
{
GuiControl, , Gui_NowState, [포북] 사냥터로 가기.
상승체력평균치 := (A_TickCount-ProgramStartTime)/1000
RunningTime := FormatSeconds((A_TickCount-ProgramStartTime)/1000)
상승체력평균값 := (CheckUPHP * 60) / (상승체력평균치/60)
GuiControl,,시작체력,%CheckFirstHP%
GuiControl,,상승체력,%CheckUPHP% (%상승체력평균값%)
GuiControl,,경과시간,%RunningTime%
CheckPB = 0
CheckPN := 0
countsignal := 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
Step = 1001
gui_Startmap := 4
MapNumber := 1
}
if(Step = 1000 and gui_Startmap = 4)
{
GuiControl, , Gui_NowState, [포북] 사냥터로 가기.
CheckPB = 0
CheckPN := 0
Send, {F16 Down}
Send, {F16 Up}
Send, {F16 Down}
Send, {F16 Up}
gosub, 차원체크
if( 현재차원 = CountPortal )
{
MapNumber := 5
현재차원 := CountPortal
Get_Location()
if InStr(Location, "포프레스네 북쪽")
{
Step = 1002
MapNumber := 10
return
}
}
else
{
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
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 1001
Sleep,5000
return
}
if(CountPortal = 1)
{
PostClick(645,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 1001
Sleep,5000
return
}
if(CountPortal = 2)
{
PostClick(660,345)
RasCount := RasCount-1
GuiControl, , Gui_RasCount, %RasCount%
Step = 1001
Sleep,5000
return
}
}
}
if(Step = 1001)
{
GuiControl, , Gui_NowState, [포북] 사냥터로 가기.
SB_SetText("차원 확인 중")
포북캐릭()
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
IfInString,Location,크로노시스
{
Step := 1000
gui_Startmap = 4
return
}
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
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
IfInString,Location,[알파차원] 포프레스네 북쪽
{
Step = 1002
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
IfInString,Location,[베타차원] 포프레스네 북쪽
{
Step = 1002
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
IfInString,Location,[감마차원] 포프레스네 북쪽
{
Step = 1002
}
Send, {F13 Down}
Sleep, 30
Send, {F13 Up}
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
if(RasCount <= 5)
{
Step = 100
}
if(RasCount > 5)
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
이전스텝 := step
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
이전스텝 := step
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
if(Gui_PartyON = 1)
{
Move_StateForMount()
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
SLEEP, 100
PostClick(190,310)
SLEEP, 100
PostDClick(225,310)
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
Sleep, 100
}
else if(Gui_PartyOff = 1)
{
Move_StateForMount()
Sleep, 100
PostMessage, 0x100, 18, 540540929, , ahk_pid %jPID%
PostMessage, 0x100, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 80, 1638401, , ahk_pid %jPID%
PostMessage, 0x101, 18, 540540929, , ahk_pid %jPID%
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
if(StatePosX = 565 and StatePosY = 655 and State = 1)
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
get_FP()
if(NowFP = 0)
{
FPcount ++
if(nowFP = 0 && FPcount >= 20)
{
SB_SetText("FP 확인 중")
Sleep, 200
Step = 201
return
}
}
if(Gui_CheckUseParty = 1)
{
party()
}
SB_SetText("포북 사냥터로 이동 중")
Get_Location()
if InStr(Location, "포프레스네 북쪽")
{
    step = 1006
    return
}
랜덤감응 = 0
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
GuiControl, , Gui_NowState, [포북] 사냥터 이동 중
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
GuiControl, , Gui_NowState, [포북] 사냥터노?
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
if( GUI_KON = 0 )
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
Sleep, 500
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
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 포북 사냥터 파수꾼 위치 도착. |" Location "시작 체력 : " . CheckFirstHP . " / 상승 체력 : " . CheckUPHP . " ( " . 상승체력평균값 . " ) " . " / 경과 시간 :  " . RunningTime
텔레그램메시지보내기(TMessage)
sleep,1000
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
SB_SetText("파수꾼과 직접 대화 중")
IfWinNotActive, ahk_pid %jPID%
{
WINACTIVATE, ahk_pid %jPID%
}
Move_NPCTalkForm()
callid = 1
Sleep, 500
PixelSearch, MobX, MobY, 410, 100, 580, 235, 0xEF8AFF, 1, Fast
if(ErrorLevel = 1)
{
AltR()
Sleep,500
PixelSearch, MobX, MobY,  0, 0, 775, 460, 0xEF8AFF, 1, Fast
}
if(ErrorLevel = 0)
{
PostClick(MobX,MobY)
Get_Location()
Loop, 10
{
IfInString,Location,[알파차원]
{
차원 := "알파"
SetFormat, integer, H
A길잃파 := jelan.read(0x00584C2C, "UInt", aOffsets*)
SetFormat, integer, D
GuiControl,, A길잃파, %A길잃파%
SB_SETTEXT(차원 . A길잃파 "-길잃은 수색대", 2)
Sleep, 100
if (A길잃파 != 0x0000)
{
    SB_SETTEXT(차원 . A길잃파 . FormNumber "-길잃은 수색대 완료", 2)
    gosub,OID저장
    sleep,100
    break  ; 루프를 멈추고 정상적으로 진행
}
}
IfInString,Location,[베타차원]
{
차원 := "베타"
SetFormat, integer, H
B길잃파 := jelan.read(0x00584C2C, "UInt", aOffsets*)
SetFormat, integer, D
GuiControl,, B길잃파, %B길잃파%
SB_SETTEXT(차원 . B길잃파 "-길잃은 수색대", 2)
Sleep, 100
if (B길잃파 != 0x0000)
{
    SB_SETTEXT(차원 . B길잃파 . FormNumber "-길잃은 수색대 완료", 2)
    gosub,OID저장
    sleep,100
    break  ; 0x로 시작하고 0이 아닌 값일 때 루프 멈춤
}
}
IfInString,Location,[감마차원]
{
차원 := "감마"
SetFormat, integer, H
G길잃파 := jelan.read(0x00584C2C, "UInt", aOffsets*)
SetFormat, integer, D
GuiControl,, G길잃파, %G길잃파%
SB_SETTEXT(차원 . G길잃파 "-길잃은 수색대", 2)
Sleep, 100
if (G길잃파 != 0x0000)
{
    SB_SETTEXT(차원 . G길잃파 . FormNumber "-길잃은 수색대 완료", 2)
    gosub,OID저장
    sleep,100
    break  ; 0x로 시작하고 0이 아닌 값일 때 루프 멈춤
}
}
}
Check_FormNumber()
if(FormNumber = 117)
{
Step = 1011
sleep,100
}
}
}
if(Step = 1011)
{
SB_SetText("피부 버프 받는 중")
Sleep, 100
Check_FormNumber()
Sleep, 100
if(FormNumber = 117)
{
Sleep, 200
PostClick(110,85)
Sleep, 200
}
if(FormNumber = 93)
{
Sleep, 200
PostClick(130,90)
Sleep, 200
}
if(FormNumber = 81)
{
Sleep, 200
PostClick(120,80)
Sleep, 200
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
Sleep, 200
PostClick(121,104)
Sleep, 200
PostClick(90,90)
Sleep, 200
}
}
if(나무갯수 >= 2)
{
나무갯수 := 나무갯수-1
Loop, %나무갯수%
{
SB_SetText("나무 > 결정으로 교환 중")
Sleep, 200
PostClick(121,104)
Sleep, 200
PostClick(90,78)
Sleep, 200
}
}
if(가루갯수 >= 3)
{
가루갯수 := 가루갯수-2
Loop, %가루갯수%
{
SB_SetText("가루 > 결정으로 교환 중")
Sleep, 200
PostClick(121,104)
Sleep, 200
PostClick(90,65)
Sleep, 200
}
}
Step = 1012
}
if(Step = 1012)
{
Check_FormNumber()
Sleep, 200
if(FormNumber = 117)
{
Sleep, 200
PostClick(85,113)
Sleep, 200
newTime = %A_Now%
EnvAdd, newTime, 27, Minutes
FormatTime, newTime1, %newTime%, yyyyMMddHHmm
CheckPB = 1
pbtalkcheck = 0
Sleep, 50
step = 1013
GuiControl, , Gui_KON, 1
MapNumber = 1
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 길잃은 파수꾼 감응 정상 적용 완료.|" Location " 시작 체력 : " . CheckFirstHP . " / 상승 체력 : " . CheckUPHP . " ( " . 상승체력평균값 . " ) " . " / 경과 시간 :  " . RunningTime
텔레그램메시지보내기(TMessage)
Sleep,200
return
}
}
}
if( GUI_KON = 1 )
{
if(Step = 1007)
{
정수체크()
GuiControl, , Gui_NowState, [포북] 사냥터 도착.
SB_SetText("파수꾼과 원격 대화 중")
Get_Location()
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
Get_Location()
Loop,
{
IfInString,Location,[알파차원]
{
WriteExecutableMemory("NPC호출용1")
WriteExecutableMemory("NPC호출용2")
차원 := "알파"
jelan.write(0x00527b54, A길잃파, "UInt", aOffset*)
SB_SETTEXT(차원 . A길잃파 "-길잃은 수색대", 2)
sleep, 500
RunMemory("NPC호출")
}
IfInString,Location,[베타차원]
{
WriteExecutableMemory("NPC호출용1")
WriteExecutableMemory("NPC호출용2")
차원 := "베타"
jelan.write(0x00527b54, B길잃파, "UInt", aOffset*)
SB_SETTEXT(차원 . B길잃파 "-길잃은 수색대", 2)
sleep, 500
RunMemory("NPC호출")
}
IfInString,Location,[감마차원]
{
WriteExecutableMemory("NPC호출용1")
WriteExecutableMemory("NPC호출용2")
차원 := "감마"
jelan.write(0x00527b54, G길잃파, "UInt", aOffset*)
SB_SETTEXT(차원 . G길잃파 "-길잃은 수색대", 2)
sleep, 500
RunMemory("NPC호출")
}
ServerMsg := jelan.readString(0x0017E574, 40, "UTF-16", aOffsets*)
IfInString,ServerMsg,서버와의 연결이
{
    gosub, OID리셋
   TMessage := "[ Helancia_Log ]>>" jTitle "<<: 포북 맞지않는 OID로 리셋 |" Location "시작 체력 : " . CheckFirstHP . " / 상승 체력 : " . CheckUPHP . " ( " . 상승체력평균값 . " ) " . " / 경과 시간 : " . RunningTime
텔레그램메시지보내기(TMessage)
return
}
Check_FormNumber()
if( FormNumber != 0 )
{
Step = 1011
break
}
포북대화경과 := A_TickCount - 포북대화시도
if(포북대화경과 >= 10000)
{
AltR()
Sleep, 1000
IfInString,Location,[알파차원]
{
차원 := "알파"
A길잃파 := 0x0
GuiControl,, A길잃파, %A길잃파%
SB_SETTEXT(차원 . A길잃파 "-길잃은 수색대만 리셋", 2)
gosub, OID저장
}
IfInString,Location,[베타차원]
{
차원 := "베타"
B길잃파 := 0x0
GuiControl,, B길잃파, %B길잃파%
SB_SETTEXT(차원 . B길잃파 "-길잃은 수색대만 리셋", 2)
gosub, OID저장
}
IfInString,Location,[감마차원]
{
차원 := "감마"
G길잃파 := 0x0
GuiControl,, G길잃파, %G길잃파%
SB_SETTEXT(차원 . G길잃파 "-길잃은 수색대만 리셋", 2)
gosub, OID저장
}
   TMessage := "[ Helancia_Log ]>>" jTitle "<<: 포북 사냥터 대화 실패, 다른방식으로 감응 재시도.|" Location "시작 체력 : " . CheckFirstHP . " / 상승 체력 : " . CheckUPHP . " ( " . 상승체력평균값 . " ) " . " / 경과 시간 : " . RunningTime
텔레그램메시지보내기(TMessage)
GuiControl, , Gui_KOFF, 1
Step = 1006
break
}
}
}
if(Step = 1011)
{
SB_SetText("피부 버프 받는 중")
Sleep, 200
Check_FormNumber()
Sleep, 100
if(FormNumber = 117)
{
Sleep, 200
PostClick(110,85)
Sleep, 200
}
if(FormNumber = 93)
{
Sleep, 200
PostClick(130,90)
Sleep, 200
}
if(FormNumber = 81)
{
Sleep, 200
PostClick(120,80)
Sleep, 200
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
Sleep, 200
PostClick(121,104)
Sleep, 200
PostClick(90,90)
Sleep, 200
}
}
if(나무갯수 >= 2)
{
나무갯수 := 나무갯수-1
Loop, %나무갯수%
{
SB_SetText("나무 > 결정으로 교환 중")
Sleep, 200
PostClick(121,104)
Sleep, 200
PostClick(90,78)
Sleep, 200
}
}
if(가루갯수 >= 3)
{
가루갯수 := 가루갯수-2
Loop, %가루갯수%
{
SB_SetText("가루 > 결정으로 교환 중")
Sleep, 200
PostClick(121,104)
Sleep, 200
PostClick(90,65)
Sleep, 200
}
}
Step = 1012
}
if(Step = 1012)
{
Check_FormNumber()
Sleep, 200
if(FormNumber = 117)
{
Sleep, 200
PostClick(85,113)
Sleep, 200
newTime = %A_Now%
EnvAdd, newTime, 27, Minutes
FormatTime, newTime1, %newTime%, yyyyMMddHHmm
CheckPB = 1
pbtalkcheck = 0
Sleep, 50
step = 1013
차원이동감응 := 0
MapNumber := 5
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 포북 사냥터 [원격] 시작.|" Location "시작 체력 : " . CheckFirstHP . " / 상승 체력 : " . CheckUPHP . " ( " . 상승체력평균값 . " ) " . " / 경과 시간 : " . RunningTime
텔레그램메시지보내기(TMessage)
Sleep,200
return
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
if(Aloute = 1)
{
if(MapNumber >= 121)
{
MapNumber = 1
Step = 1000
return
}
}
if(Bloute = 1)
{
if(MapNumber >= 347)
{
MapNumber = 1
Step = 1000
return
}
}
if(Cloute = 1)
{
if(MapNumber >= 185)
{
MapNumber = 1
Step = 1000
return
}
}
CharMovePobuk()
Step = 1014
}
if(Step = 1014)
{
SB_SetText("움직임 체크 중")
IfWinNotActive, ahk_pid %jPID%
{
WinActivate, ahk_pid %jPID%
}
Check_Moving()
Get_Pos()
Get_MovePos()
if(Moving = 0)
{
sleep,200
Check_Moving()
if(Moving = 0)
{
Step = 1015
}
}
거리범위 := 3
if (Abs(PosX - MovePosX) <= 거리범위 && Abs(PosY - MovePosY) <= 거리범위)
{
    MoveWaitCount := 0
    Step := 1016
    한번만 := 1
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
return

}
if(Step = 1016)
{
SB_SetText("몬스터 찾는 중")
IfWinNotActive, ahk_pid %jPID%
{
WinActivate, ahk_pid %jPID%
}
AttackingCount3 := 0
포북몹 := 0xB5F5F7
PixelSearch, MobX, MobY, 350, 160, 410, 260, 포북몹, 10, *ScanBR *Fast  *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 350, 160, 410, 260, 포북몹, 10, *ScanBR *Fast  *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 360, 209, 437, 260, 포북몹, 10, *ScanLB *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 360, 209, 437, 260, 포북몹, 10, *ScanLB *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 362, 186, 432, 255, 포북몹, 10, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 362, 186, 432, 255, 포북몹, 10, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 333, 161, 460, 281, 포북몹, 10, *ScanRT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 333, 161, 460, 281, 포북몹, 10, *ScanRT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 315, 138, 483, 305, 포북몹, 10, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 315, 138, 483, 305, 포북몹, 10, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 260, 92, 533, 352, 포북몹, 10, *ScanRT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 260, 92, 533, 352, 포북몹, 10, *ScanRT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 214, 44, 580, 400, 포북몹, 10, *ScanLT *Fast *RGB
if(ErrorLevel = 1)
{
PixelSearch, MobX, MobY, 214, 44, 580, 400, 포북몹, 10, *ScanLT *Fast *RGB
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
sleep,200
keyclick("AltR")
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
return
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
AttackingCount2 := A_TickCount
return
}
}
}
if(Step = 1019)
{
SB_SetText("몬스터가 가까이 있는지 확인 중")
;sleep,100
keyclick("AltR")
Check_Moving()
if(Moving = 0)
{
Check_Moving()
if(Moving = 0)
{
Step = 1018
AttackMissCount := 0
한번만 := 1
}
}
movmob2 := A_TickCount - movmob
if(movmob2 >= 2500)
{
SB_SetText("거리가 멉니다.")
Sleep, 100
PostMessage, 0x100, 9, 983041, , ahk_pid %jPID%
PostMessage, 0x101, 9, 983041, , ahk_pid %jPID%
Step = 1016
}
}
if(Step = 1026) ;무바파트
{
GUICONTROL, , Gui_NowState, [포북] 무바 중
SB_SetText("포북 메모리 무바", 1)
AttackMissCount ++
if(AttackMissCount >= 800 and 한번만 = 1)
{
    keyclick("AltR")
    AttackMissCount := 0
    한번만 :=0
}
if(gui_1muba = 1)
{
ReadAbilityNameValue()
if(AbilityName = Gui_Weapon1)
{
BWValue1 := AbilityValue
}
}
if(gui_2muba = 1)
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
}
if(gui_3muba = 1)
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
}
if(Gui_2butmuba = 1)
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
}
if(Gui_3butmuba = 1)
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
}
if(Gui_4butMuba = 1)
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
if(AbilityName = Gui_Weapon3)
{
BWValue3 := AbilityValue
}
}
if(Gui_CheckUseMagic = 1)
{
if(BWValue0 = "격투" or Gui_Weapon1 = "현금" or Gui_Weapon1 =  "스태프" || Gui_Weapon2 = "현금" or Gui_Weapon2 =  "스태프" || Gui_Weapon3 = "현금" or Gui_Weapon3 =  "스태프")
{
RemoteM()
}
}
현재무기 := jelan.read(0x0058DAD4, "UInt", 0x121)
if (현재무기 != 0) ;4벗무바무기수리로직
{
    if(Gui_2Muba = 1 || Gui_3butMuba = 1||Gui_3Muba = 1 || Gui_4butMuba = 1)
    {
    TrackWeaponChange(현재무기)
    }
    if(Gui_1Muba = 1 || Gui_2butMuba = 1)
    {
    RepairWeaponCount = 0
    }
}
if (현재무기 = 0)
{
    if(Gui_1Muba = 1 || Gui_2butMuba = 1)
    {
    RepairWeaponCount += 1
    }
    else if(Gui_2Muba = 1 || Gui_3butMuba = 1)
    {
    RecentWeapons.RemoveAt(2)
    }
    else if(Gui_3Muba = 1 || Gui_4butMuba = 1)
    {
    RecentWeapons.RemoveAt(3)
    }
}
무바여부 := CheckTrackedWeapons()
if(Gui_1Muba = 1 || Gui_2butMuba = 1)
{
사용할무기수량 := 1
}
else if(Gui_2Muba = 1 || Gui_3butMuba = 1)
{
사용할무기수량 := 2
}
else if(Gui_3Muba = 1 || Gui_4butMuba = 1)
{
사용할무기수량 := 3
}
if (무바여부 = 사용할무기수량)
{
RepairWeaponCount := 0
}
if (무바여부 != 사용할무기수량)
{
RepairWeaponCount += 1
}
else
{
  RepairWeaponCount := 0
}
if (RepairWeaponCount >= 800)
{
RepairWeaponCount = 0
MapNumber = 1
step = 300
return
}
}
if(Step = 1030)
{
SB_SetText("피부 버프 받는 중")
CheckPB = 0
RepairWeaponCount = 0
Sleep, 500
Keyclick("tab")
Sleep, 300
CheckPB = 0
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
이전스텝 := step
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
이전스텝 := step
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
SB_SetText("원격대화 시도 중2")
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
sleep,300
Loop,
{
WriteExecutableMemory("NPC호출용1")
WriteExecutableMemory("NPC호출용2")
IfInString,Location,[알파차원]
{
차원 := "알파"
jelan.write(0x00527b54, A길잃파, "UInt", aOffset*)
SB_SETTEXT(차원 . A길잃파 "-길잃은 수색대", 2)
sleep, 100
RunMemory("NPC호출")
}
IfInString,Location,[베타차원]
{
차원 := "베타"
jelan.write(0x00527b54, B길잃파, "UInt", aOffset*)
SB_SETTEXT(차원 . B길잃파 "-길잃은 수색대", 2)
sleep, 100
RunMemory("NPC호출")
}
IfInString,Location,[감마차원]
{
차원 := "감마"
jelan.write(0x00527b54, G길잃파, "UInt", aOffset*)
SB_SETTEXT(차원 . G길잃파 "-길잃은 수색대", 2)
sleep, 100
RunMemory("NPC호출")
}
Check_FormNumber()
if( FormNumber != 0 )
{
Step = 1032
sleep,100
break
}
포북대화경과 := A_TickCount - 포북대화시도
if(포북대화경과 >= 5000)
{
AltR()
Sleep,1000
Step = 1006
   TMessage := "[ Helancia_Log ]>>" jTitle "<<: 포북 사냥터 대화 실패, 다른방식으로 감응 재시도.|" Location "시작 체력 : " . CheckFirstHP . " / 상승 체력 : " . CheckUPHP . " ( " . 상승체력평균값 . " ) " . " / 경과 시간 : " . RunningTime
텔레그램메시지보내기(TMessage)
Sleep,500
GuiControl, , Gui_KOFF, 1
차원이동감응 := 1
break
}
}
}
if(Step = 1032)
{
SB_SetText("피부 버프 갱신 중")
Sleep, 200
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
TMessage := "[ Helancia_Log ]>>" jTitle "<<: 포북 사냥터 [원격] 시간별 정상작동.|" Location "시작 체력 : " . CheckFirstHP . " / 상승 체력 : " . CheckUPHP . " ( " . 상승체력평균값 . " ) " . " / 경과 시간 : " . RunningTime
텔레그램메시지보내기(TMessage)
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
    ; Internet Explorer COM 객체 생성
    ie := ComObjCreate("InternetExplorer.Application")
    ie.Visible := false ; 브라우저 창 숨김 (true로 설정하면 창이 표시됨)
    url := "https://elancia.nexon.com/"
    ie.Navigate(url)

    ; 로딩 완료 대기
    while ie.Busy or ie.ReadyState != 4
        Sleep, 1000

    ; 페이지 HTML 가져오기
    html := ie.document.body.innerHTML

    ; 1단계: "서버 현황" 단어 위치 찾기 및 300자 추출
    target1 := "서버 현황"
    pos1 := InStr(html, target1) ; "서버 현황" 단어 위치 찾기

    if (pos1 > 0) {
        extractedText := SubStr(html, pos1, 300) ; "서버 현황" 위치부터 300자 추출

        ; 2단계: 추출된 300자 내에서 "엘" 단어 찾기
        target2 := "엘"
        pos2 := InStr(extractedText, target2) ; "엘" 단어 위치 찾기

        if (pos2 > 0) {
            ; 3단계: "엘" 위치 이후 <dd>와 </dd> 사이 텍스트 추출
            ddStart := InStr(extractedText, "<dd>", false, pos2) ; <dd> 시작 위치
            ddEnd := InStr(extractedText, "</dd>", false, ddStart) ; </dd> 종료 위치

            if (ddStart > 0 and ddEnd > ddStart) {
                result := SubStr(extractedText, ddStart + 4, ddEnd - ddStart - 4) ; <dd> 태그 이후 텍스트 추출
                Server := result
            }
            else
            {
        TMessage :="일랜시아 홈페이지 점검 중"
        텔레그램메시지보내기(TMessage)
            }
        }
        else
        {
        TMessage :="추출된 텍스트 내에서 단어 '" target2 "'을 찾을 수 없습니다."
        텔레그램메시지보내기(TMessage)
        }
    }
    else
    {
        TMessage :="HTML에서 단어 '" target1 "'을 찾을 수 없습니다."
        텔레그램메시지보내기(TMessage)
    }

    ; IE 객체 종료
    ie.Quit()
}
catch e
{
TMessage :="예외 발생! 상세 정보:" . e.Message . "Line: " . e.Line
텔레그램메시지보내기(TMessage)
GROUPADD, ie_gruop, ahk_exe iexplore.exe
WINKILL, ahk_exe iexplore.exe
WINKILL, ahk_group ie_gruop
GOSUB, RL
}
if((Trim(Server) = "정상"))
{
GuiControl, , 로그인상태정보, 일랜시아 서버 정상. 재접속 중
SB_SetText("재접속 시도 중")
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
else
{
GUICONTROL, , Gui_NowState, 일랜시아 홈페이지 서버 점검 중. 15분 대기
GUICONTROL, , Gui_KOFF, 1
WinKill, ahk_exe NGM64.exe
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
ParasCount = 0
실행초기화 := 0
SLEEP, 900000
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

AttackMGB:
Gui, Submit, Nohide
if(Gui_mgb = 1)
{
if(Step = 27 or Step = 1026 or step = 3030)
{
RandomSendCtrlKey()
}
}
return


AttackCheck:
Gui, Submit, Nohide
if (Step >= 7 && Step < 10000)
{
    Set_MoveSpeed()
}
if(Step = 27 or Step = 1026 or step = 3030)
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
}
if(HuntPlace = 2)
{
Step = 1016
}
if(HuntPlace = 3)
{
Step = 3027
}
}
else
{
AttackLoopCount = 0
AttackCount = 0
}
}
if(Step = 27)
{
Attacking := A_TickCount - AttackingCount
if(Attacking >= 50000)
{
keyclick("AltR")
AttackingCount := A_TickCount
}
}
if(Step = 1026)
{
Attacking := A_TickCount - AttackingCount
if(Attacking >= 50000)
{
keyclick("AltR")
AttackingCount := A_TickCount
}
}
if(Step = 3030)
{
Attacking := A_TickCount - AttackingCount
if(Attacking >= 10000)
{
AttackingCount := A_TickCount
keyclick("AltR")
}
}
}
return
GuiClose:
Gui, Submit, NoHide
jelan.write(0x0058FFE0,0,"UInt", aOffsets*)
jelan.write(0x0058DAD4, 400, "UInt", 0x178, 0x9C)
jelan.write(0x0058DAD4, 400, "UInt", 0x178, 0x98)
무바비활성화()
IfWinExist,ahk_pid %jPID%
{
WinKill, ahk_pid %jPID%
WinKill, ahk_exe MRMsph.exe
}
if(Gui_MGB = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MGB, 1
}
if(Gui_MGB = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MGB, 0
}
if(Gui_CheckUseHPHospital = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPHospital, 1
}
if(Gui_CheckUseHPHospital = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, UseHPHospital, 0
}
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, HPHospital, %Gui_HPHospital%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, VMRE, 0
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 실행시간, 0
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam,ChatID, %ChatID%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam,동파감응시간셋팅, %동파감응시간셋팅%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam,서파감응시간셋팅, %서파감응시간셋팅%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 게임시작x, %게임시작x%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 게임시작y, %게임시작y%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Reserver,0
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 파라스감지,0
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 수호천사방지,0
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 인연방지,0
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 파라스방해감지,0
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, TTM, %ChatID%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam,동파감응시간셋팅, %동파감응시간셋팅%
RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Nexon\MRMChezam,서파감응시간셋팅, %서파감응시간셋팅%
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
if(Protect_AmorON = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Protect_AmorONOFF, 1
}
if(Protect_AmorOFF = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Protect_AmorONOFF, 2
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
if(Gui_HuntMummy = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, Place, 4
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
if(Gui_MGB = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MGB, 1
}
if(Gui_MGB = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, MGB, 0
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
;RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 업데이트체크, %업데이트체크%
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
if(8번사용 = 1)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯8번, 1
}
if(8번사용 = 0)
{
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, 퀵슬롯8번, 0
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
FileDelete, C:\소각리스트.ini
save := LV_GetCount()
loop, %save%
{
lv_gettext(savefile1,a_index)
FileAppend, %savefile1%`n, C:\소각리스트.ini
FileSetAttrib, +H, C:\소각리스트.ini
}
WinKill, ahk_exe MRMsph.exe
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
GuiControl, Enable, Gui_MGB
GuiControl, Enable, Gui_StartButton
GuiControl, Enable, Gui_WindowSettingButton
GuiControl, Enable, Gui_Agree
}
PlugClick(x,y)
{
MousePos := x|y<<16
PostMessage, 0x200, 0, %MousePos%, Chrome_WidgetWin_01, ahk_pid %플러그%
PostMessage, 0x201, 1, %MousePos%, Chrome_WidgetWin_01, ahk_pid %플러그%
PostMessage, 0x202, 0, %MousePos%, Chrome_WidgetWin_01, ahk_pid %플러그%
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
keyclick("AltR")
}
Check_Dimension()
{
Dimension := jelan.read(0x0058EB1C, "UInt", 0x10A)
if(Dimension>20000)
차원체크:="감마"
else if(Dimension>10000)
차원체크:="베타"
else if(Dimension<10000)
차원체크:="알파"
}
return

Set_nomalSpeed()
{
jelan.write(0x0058DAD4, 750, "UInt", 0x178, 0x9C)
jelan.write(0x0058DAD4, 750, "UInt", 0x178, 0x98)
jelan.write(0x0058FFE0,0,"UInt", aOffsets*)
}
return

Set_MoveSpeed()
{
;jelan.write(0x0058DAD4, 750, "UInt", 0x178, 0x9C)
;jelan.write(0x0058DAD4, 750, "UInt", 0x178, 0x98)
jelan.write(0x0058FFE0,45,"UInt", aOffsets*)
jelan.write(0x0058DAD4, 2300, "UInt", 0x178, 0x9C)
jelan.write(0x0058DAD4, 2300, "UInt", 0x178, 0x98)
}
return
Check_Moving()
{
Moving := jelan.read(0x0058EB1C, "UInt", 0x174)
}
return
Check_OID()
{
Get_Location()
CCD := jelan.read(0x00584C2C, "UInt", aOffsets*)
}
return
Monster_OID()
{
MonsterPID :=jelan.read(0x00584C2C, "UInt", aOffsets*)
GuiControl,,MonsterTargetPID,0x%MonsterPID%
}
return
Check_State()
{
State := jelan.read(0x0058EB98, "UInt", aOffsets*)
if(State != 0)
{
State = 1
}
}
return
Check_StatePos()
{
StatePosX := jelan.read(0x0058EB48, "UInt", 0x44)
StatePosY := jelan.read(0x0058EB48, "UInt", 0x48)
}
return
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
}
Check_SMagic()
{
Slot3Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0xC, 0x8, 0x42C)
Slot4Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x10, 0x8, 0x42C)
Slot5Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x14, 0x8, 0x42C)
Slot6Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x18, 0x8, 0x42C)
Slot7Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x1C, 0x8, 0x42C)
Slot8Magic := jelan.read(0x0058DAD4, "UInt", 0x178, 0xC2, 0x8, 0x20, 0x8, 0x42C)
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
PickUp_itemsetMM() {
value := jelan.writeString(0x00590A00, "천", "UTF-16")
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
value := jelan.write(0x0058EB48, 306, "UInt", 0x5C)
value := jelan.write(0x0058EB48, 534, "UInt", 0x60)
}
Move_State()
{
value := jelan.write(0x0058EB48, 565, "UInt", 0x44)
value := jelan.write(0x0058EB48, 655, "UInt", 0x48)
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
Get_NowDate()
{
TempMont := A_MM
if(TempMont < 10)
{
StringTrimLeft, TempMont, TempMont, 1
}
TempDay := A_DD
if(TempDay < 10)
{
StringTrimLeft, TempDay, TempDay, 1
}
TempWDay := A_WDay
if(TempWDay = 1)
{
TempWDay = 일
}
if(TempWDay = 2)
{
TempWDay = 월
}
if(TempWDay = 3)
{
TempWDay = 화
}
if(TempWDay = 4)
{
TempWDay = 수
}
if(TempWDay = 5)
{
TempWDay = 목
}
if(TempWDay = 6)
{
TempWDay = 금
}
if(TempWDay = 7)
{
TempWDay = 토
}
NowDate = %TempMont%/%TempDay%(%TempWDay%)
}
tac109()
{
RunWait, %comspec% /c wmic bios get serialnumber > bal.txt
Loop,Read, bal.txt
{
ifinstring, A_LoopReadLine,VMware-
{
lov = %A_LoopReadLine%
break
}
}
}
CharMovePobuk()
{
if(랜덤감응 = 0)
{
Random, Myloute, 1, 3
    if (Myloute = 1)
    {
        Aloute := 1
        Bloute := 0
        Cloute := 0
    }
    else if (Myloute = 2)
    {
        Aloute := 0
        Bloute := 1
        Cloute := 0
    }
    else if (Myloute = 3)
    {
    Aloute := 0
    Bloute := 0
    Cloute := 1
    }
    랜덤감응 := 1
}
if(Aloute = 1) ;바꾼 루트 120개
{
if (MapNumber = 1) {
    좌표입력(191, 175, 1)
    RunMemory("좌표이동")
    RunDirect = 0
}
if (MapNumber = 2) {
    좌표입력(189, 166, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 3) {
    좌표입력(180, 157, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 4) {
    좌표입력(164, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 5) {
    좌표입력(158, 135, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 6) {
    좌표입력(153, 128, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 7) {
    좌표입력(144, 123, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 8) {
    좌표입력(136, 121, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 9) {
    좌표입력(132, 127, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 10) {
    좌표입력(130, 132, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 11) {
    좌표입력(131, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 12) {
    좌표입력(137, 147, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 13) {
    좌표입력(138, 157, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 14) {
    좌표입력(125, 154, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 15) {
    좌표입력(124, 147, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 16) {
    좌표입력(128, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 17) {
    좌표입력(137, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 18) {
    좌표입력(130, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 19) {
    좌표입력(129, 153, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 20) {
    좌표입력(135, 160, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 21) {
    좌표입력(142, 151, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 22) {
    좌표입력(134, 147, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 23) {
    좌표입력(126, 149, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 24) {
    좌표입력(122, 152, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 25) {
    좌표입력(127, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 26) {
    좌표입력(131, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 27) {
    좌표입력(128, 129, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 28) {
    좌표입력(123, 123, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 29) {
    좌표입력(117, 118, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 30) {
    좌표입력(112, 113, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 31) {
    좌표입력(106, 107, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 32) {
    좌표입력(100, 101, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 33) {
    좌표입력(95, 96, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 34) {
    좌표입력(89, 92, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 35) {
    좌표입력(83, 92, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 36) {
    좌표입력(78, 95, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 37) {
    좌표입력(73, 100, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 38) {
    좌표입력(68, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 39) {
    좌표입력(63, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 40) {
    좌표입력(58, 114, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 41) {
    좌표입력(53, 117, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 42) {
    좌표입력(49, 117, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 43) {
    좌표입력(49, 113, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 44) {
    좌표입력(52, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 45) {
    좌표입력(54, 104, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 46) {
    좌표입력(52, 99, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 47) {
    좌표입력(52, 95, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 48) {
    좌표입력(52, 92, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 49) {
    좌표입력(49, 87, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 50) {
    좌표입력(49, 83, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 51) {
    좌표입력(49, 78, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 52) {
    좌표입력(49, 72, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 53) {
    좌표입력(49, 67, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 54) {
    좌표입력(49, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 55) {
    좌표입력(44, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 56) {
    좌표입력(39, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 57) {
    좌표입력(33, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 58) {
    좌표입력(28, 61, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 59) {
    좌표입력(23, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 60) {
    좌표입력(18, 67, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 61) {
    좌표입력(18, 71, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 62) {
    좌표입력(18, 76, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 63) {
    좌표입력(18, 81, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 64) {
    좌표입력(18, 86, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 65) {
    좌표입력(20, 92, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 66) {
    좌표입력(21, 97, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 67) {
    좌표입력(18, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 68) {
    좌표입력(24, 106, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 69) {
    좌표입력(29, 106, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 70) {
    좌표입력(36, 106, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 71) {
    좌표입력(37, 101, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 72) {
    좌표입력(37, 96, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 73) {
    좌표입력(37, 91, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 74) {
    좌표입력(44, 85, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 75) {
    좌표입력(36, 76, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 76) {
    좌표입력(38, 66, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 77) {
    좌표입력(45, 61, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 78) {
    좌표입력(44, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 79) {
    좌표입력(49, 60, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 80) {
    좌표입력(53, 70, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 81) {
    좌표입력(58, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 82) {
    좌표입력(50, 84, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 83) {
    좌표입력(47, 91, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 84) {
    좌표입력(42, 97, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 85) {
    좌표입력(36, 94, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 86) {
    좌표입력(27, 98, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 87) {
    좌표입력(21, 83, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 88) {
    좌표입력(26, 76, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 89) {
    좌표입력(31, 70, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 90) {
    좌표입력(23, 68, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 91) {
    좌표입력(23, 61, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 92) {
    좌표입력(32, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 93) {
    좌표입력(38, 69, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 94) {
    좌표입력(43, 76, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 95) {
    좌표입력(44, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 96) {
    좌표입력(48, 80, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 97) {
    좌표입력(47, 88, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 98) {
    좌표입력(53, 94, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 99) {
    좌표입력(55, 94, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 100) {
    좌표입력(50, 98, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 101) {
    좌표입력(39, 95, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 102) {
    좌표입력(37, 94, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 103) {
    좌표입력(31, 96, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 104) {
    좌표입력(29, 96, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 105) {
    좌표입력(24, 92, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 106) {
    좌표입력(21, 86, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 107) {
    좌표입력(23, 82, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 108) {
    좌표입력(28, 76, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 109) {
    좌표입력(28, 69, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 110) {
    좌표입력(25, 63, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 111) {
    좌표입력(32, 57, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 112) {
    좌표입력(24, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 113) {
    좌표입력(23, 47, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 114) {
    좌표입력(36, 30, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 115) {
    좌표입력(28, 17, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 116) {
    좌표입력(19, 20, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 117) {
    좌표입력(19, 27, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 118) {
    좌표입력(20, 36, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 119) {
    좌표입력(22, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 120) {
    좌표입력(22, 51, 1)
    RunMemory("좌표이동")
    RunDirect = 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,100
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,100
}
Step = 1014
}
if(Bloute = 1) ;내가 만든 루트 346개
{
if (MapNumber = 1) {
    좌표입력(196, 168, 1)
    RunMemory("좌표이동")
    RunDirect := 0
}
if (MapNumber = 2) {
    좌표입력(194, 160, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 3) {
    좌표입력(189, 156, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 4) {
    좌표입력(186, 149, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 5) {
    좌표입력(189, 141, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 6) {
    좌표입력(185, 136, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 7) {
    좌표입력(190, 131, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 8) {
    좌표입력(187, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 9) {
    좌표입력(188, 121, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 10) {
    좌표입력(197, 119, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 11) {
    좌표입력(200, 123, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 12) {
    좌표입력(207, 127, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 13) {
    좌표입력(209, 131, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 14) {
    좌표입력(203, 136, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 15) {
    좌표입력(207, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 16) {
    좌표입력(211, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 17) {
    좌표입력(215, 145, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 18) {
    좌표입력(207, 147, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 19) {
    좌표입력(205, 153, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 20) {
    좌표입력(207, 157, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 21) {
    좌표입력(214, 159, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 22) {
    좌표입력(215, 164, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 23) {
    좌표입력(222, 167, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 24) {
    좌표입력(222, 172, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 25) {
    좌표입력(218, 177, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 26) {
    좌표입력(224, 178, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 27) {
    좌표입력(234, 181, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 28) {
    좌표입력(223, 177, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 29) {
    좌표입력(228, 165, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 30) {
    좌표입력(227, 157, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 31) {
    좌표입력(229, 145, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 32) {
    좌표입력(231, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 33) {
    좌표입력(227, 124, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 34) {
    좌표입력(218, 104, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 35) {
    좌표입력(212, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 36) {
    좌표입력(199, 98, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 37) {
    좌표입력(198, 103, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 38) {
    좌표입력(195, 106, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 39) {
    좌표입력(189, 104, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 40) {
    좌표입력(186, 99, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 41) {
    좌표입력(177, 92, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 42) {
    좌표입력(185, 87, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 43) {
    좌표입력(176, 81, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 44) {
    좌표입력(171, 91, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 45) {
    좌표입력(155, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 46) {
    좌표입력(136, 81, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 47) {
    좌표입력(126, 70, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 48) {
    좌표입력(127, 66, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 49) {
    좌표입력(123, 61, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 50) {
    좌표입력(134, 56, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 51) {
    좌표입력(124, 58, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 52) {
    좌표입력(125, 63, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 53) {
    좌표입력(123, 69, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 54) {
    좌표입력(116, 70, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 55) {
    좌표입력(109, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 56) {
    좌표입력(105, 79, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 57) {
    좌표입력(95, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 58) {
    좌표입력(91, 83, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 59) {
    좌표입력(85, 75, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 60) {
    좌표입력(77, 70, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 61) {
    좌표입력(76, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 62) {
    좌표입력(71, 68, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 63) {
    좌표입력(74, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 64) {
    좌표입력(74, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 65) {
    좌표입력(71, 82, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 66) {
    좌표입력(71, 91, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 67) {
    좌표입력(72, 96, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 68) {
    좌표입력(70, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 69) {
    좌표입력(73, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 70) {
    좌표입력(76, 114, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 71) {
    좌표입력(69, 121, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 72) {
    좌표입력(75, 130, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 73) {
    좌표입력(67, 136, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 74) {
    좌표입력(66, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 75) {
    좌표입력(70, 148, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 76) {
    좌표입력(77, 157, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 77) {
    좌표입력(76, 165, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 78) {
    좌표입력(80, 173, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 79) {
    좌표입력(73, 181, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 80) {
    좌표입력(87, 182, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 81) {
    좌표입력(96, 180, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 82) {
    좌표입력(106, 176, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 83) {
    좌표입력(108, 169, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 84) {
    좌표입력(109, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 85) {
    좌표입력(105, 156, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 86) {
    좌표입력(101, 148, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 87) {
    좌표입력(108, 138, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 88) {
    좌표입력(110, 129, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 89) {
    좌표입력(112, 118, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 90) {
    좌표입력(126, 119, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 91) {
    좌표입력(131, 130, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 92) {
    좌표입력(129, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 93) {
    좌표입력(123, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 94) {
    좌표입력(126, 153, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 95) {
    좌표입력(132, 159, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 96) {
    좌표입력(141, 152, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 97) {
    좌표입력(140, 144, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 98) {
    좌표입력(136, 139, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 99) {
    좌표입력(129, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 100) {
    좌표입력(124, 147, 1)
    RunMemory("좌표이동")
}if (MapNumber = 101) {
    좌표입력(129, 152, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 102) {
    좌표입력(128, 158, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 103) {
    좌표입력(129, 151, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 104) {
    좌표입력(129, 144, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 105) {
    좌표입력(130, 137, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 106) {
    좌표입력(132, 129, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 107) {
    좌표입력(125, 120, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 108) {
    좌표입력(118, 116, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 109) {
    좌표입력(105, 112, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 110) {
    좌표입력(93, 117, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 111) {
    좌표입력(73, 120, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 112) {
    좌표입력(61, 125, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 113) {
    좌표입력(54, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 114) {
    좌표입력(53, 131, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 115) {
    좌표입력(47, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 116) {
    좌표입력(46, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 117) {
    좌표입력(46, 120, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 118) {
    좌표입력(46, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 119) {
    좌표입력(46, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 120) {
    좌표입력(48, 99, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 121) {
    좌표입력(51, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 122) {
    좌표입력(49, 87, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 123) {
    좌표입력(60, 76, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 124) {
    좌표입력(58, 72, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 125) {
    좌표입력(56, 66, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 126) {
    좌표입력(57, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 127) {
    좌표입력(67, 47, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 128) {
    좌표입력(79, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 129) {
    좌표입력(80, 57, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 130) {
    좌표입력(88, 60, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 131) {
    좌표입력(97, 60, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 132) {
    좌표입력(106, 60, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 133) {
    좌표입력(109, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 134) {
    좌표입력(114, 49, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 135) {
    좌표입력(106, 45, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 136) {
    좌표입력(110, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 137) {
    좌표입력(125, 34, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 138) {
    좌표입력(137, 40, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 139) {
    좌표입력(144, 41, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 140) {
    좌표입력(153, 35, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 141) {
    좌표입력(161, 42, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 142) {
    좌표입력(171, 33, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 143) {
    좌표입력(184, 42, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 144) {
    좌표입력(194, 46, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 145) {
    좌표입력(199, 51, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 146) {
    좌표입력(211, 57, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 147) {
    좌표입력(212, 63, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 148) {
    좌표입력(220, 70, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 149) {
    좌표입력(225, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 150) {
    좌표입력(225, 91, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 151) {
    좌표입력(209, 80, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 152) {
    좌표입력(200, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 153) {
    좌표입력(188, 80, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 154) {
    좌표입력(168, 91, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 155) {
    좌표입력(163, 90, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 156) {
    좌표입력(152, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 157) {
    좌표입력(159, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 158) {
    좌표입력(160, 60, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 159) {
    좌표입력(150, 72, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 160) {
    좌표입력(149, 84, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 161) {
    좌표입력(149, 92, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 162) {
    좌표입력(142, 91, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 163) {
    좌표입력(136, 89, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 164) {
    좌표입력(136, 84, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 165) {
    좌표입력(130, 83, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 166) {
    좌표입력(120, 78, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 167) {
    좌표입력(112, 74, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 168) {
    좌표입력(114, 68, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 169) {
    좌표입력(106, 71, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 170) {
    좌표입력(94, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 171) {
    좌표입력(89, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 172) {
    좌표입력(81, 74, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 173) {
    좌표입력(72, 71, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 174) {
    좌표입력(75, 65, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 175) {
    좌표입력(76, 74, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 176) {
    좌표입력(73, 79, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 177) {
    좌표입력(71, 88, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 178) {
    좌표입력(65, 101, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 179) {
    좌표입력(66, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 180) {
    좌표입력(63, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 181) {
    좌표입력(60, 113, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 182) {
    좌표입력(55, 115, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 183) {
    좌표입력(50, 118, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 184) {
    좌표입력(49, 114, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 185) {
    좌표입력(54, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 186) {
    좌표입력(51, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 187) {
    좌표입력(41, 107, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 188) {
    좌표입력(31, 104, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 189) {
    좌표입력(19, 103, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 190) {
    좌표입력(16, 114, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 191) {
    좌표입력(20, 118, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 192) {
    좌표입력(16, 125, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 193) {
    좌표입력(25, 128, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 194) {
    좌표입력(27, 136, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 195) {
    좌표입력(30, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 196) {
    좌표입력(24, 148, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 197) {
    좌표입력(35, 155, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 198) {
    좌표입력(35, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 199) {
    좌표입력(23, 163, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 200) {
    좌표입력(21, 170, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 201) {
    좌표입력(29, 171, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 202) {
    좌표입력(38, 177, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 203) {
    좌표입력(24, 183, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 204) {
    좌표입력(19, 183, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 205) {
    좌표입력(23, 169, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 206) {
    좌표입력(29, 153, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 207) {
    좌표입력(41, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 208) {
    좌표입력(31, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 209) {
    좌표입력(27, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 210) {
    좌표입력(22, 124, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 211) {
    좌표입력(22, 117, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 212) {
    좌표입력(23, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 213) {
    좌표입력(19, 86, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 214) {
    좌표입력(16, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 215) {
    좌표입력(20, 71, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 216) {
    좌표입력(20, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 217) {
    좌표입력(25, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 218) {
    좌표입력(24, 45, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 219) {
    좌표입력(24, 39, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 220) {
    좌표입력(29, 34, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 221) {
    좌표입력(30, 28, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 222) {
    좌표입력(33, 17, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 223) {
    좌표입력(41, 23, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 224) {
    좌표입력(16, 21, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 225) {
    좌표입력(19, 29, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 226) {
    좌표입력(23, 40, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 227) {
    좌표입력(22, 53, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 228) {
    좌표입력(23, 57, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 229) {
    좌표입력(31, 60, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 230) {
    좌표입력(39, 63, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 231) {
    좌표입력(45, 63, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 232) {
    좌표입력(52, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 233) {
    좌표입력(53, 56, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 234) {
    좌표입력(55, 51, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 235) {
    좌표입력(59, 46, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 236) {
    좌표입력(64, 38, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 237) {
    좌표입력(68, 30, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 238) {
    좌표입력(66, 24, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 239) {
    좌표입력(88, 29, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 240) {
    좌표입력(97, 33, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 241) {
    좌표입력(91, 40, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 242) {
    좌표입력(84, 41, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 243) {
    좌표입력(86, 48, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 244) {
    좌표입력(94, 51, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 245) {
    좌표입력(99, 49, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 246) {
    좌표입력(103, 46, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 247) {
    좌표입력(108, 48, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 248) {
    좌표입력(110, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 249) {
    좌표입력(114, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 250) {
    좌표입력(106, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 251) {
    좌표입력(102, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 252) {
    좌표입력(94, 60, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 253) {
    좌표입력(89, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 254) {
    좌표입력(92, 44, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 255) {
    좌표입력(104, 38, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 256) {
    좌표입력(119, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 257) {
    좌표입력(125, 44, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 258) {
    좌표입력(133, 39, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 259) {
    좌표입력(147, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 260) {
    좌표입력(148, 33, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 261) {
    좌표입력(147, 27, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 262) {
    좌표입력(146, 20, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 263) {
    좌표입력(153, 17, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 264) {
    좌표입력(158, 19, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 265) {
    좌표입력(162, 18, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 266) {
    좌표입력(174, 15, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 267) {
    좌표입력(179, 20, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 268) {
    좌표입력(184, 22, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 269) {
    좌표입력(194, 19, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 270) {
    좌표입력(200, 24, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 271) {
    좌표입력(200, 29, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 272) {
    좌표입력(209, 28, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 273) {
    좌표입력(222, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 274) {
    좌표입력(221, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 275) {
    좌표입력(223, 51, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 276) {
    좌표입력(230, 53, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 277) {
    좌표입력(234, 44, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 278) {
    좌표입력(231, 29, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 279) {
    좌표입력(223, 18, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 280) {
    좌표입력(208, 15, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 281) {
    좌표입력(196, 18, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 282) {
    좌표입력(182, 18, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 283) {
    좌표입력(176, 18, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 284) {
    좌표입력(143, 15, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 285) {
    좌표입력(140, 15, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 286) {
    좌표입력(144, 29, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 287) {
    좌표입력(153, 42, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 288) {
    좌표입력(158, 45, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 289) {
    좌표입력(163, 41, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 290) {
    좌표입력(167, 44, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 291) {
    좌표입력(173, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 292) {
    좌표입력(178, 45, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 293) {
    좌표입력(183, 47, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 294) {
    좌표입력(188, 49, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 295) {
    좌표입력(192, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 296) {
    좌표입력(193, 54, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 297) {
    좌표입력(197, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 298) {
    좌표입력(197, 63, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 299) {
    좌표입력(196, 68, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 300) {
    좌표입력(196, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 301) {
    좌표입력(192, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 302) {
    좌표입력(188, 81, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 303) {
    좌표입력(182, 87, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 304) {
    좌표입력(176, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 305) {
    좌표입력(171, 96, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 306) {
    좌표입력(167, 100, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 307) {
    좌표입력(163, 104, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 308) {
    좌표입력(158, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 309) {
    좌표입력(153, 114, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 310) {
    좌표입력(149, 118, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 311) {
    좌표입력(144, 123, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 312) {
    좌표입력(142, 128, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 313) {
    좌표입력(138, 128, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 314) {
    좌표입력(133, 127, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 315) {
    좌표입력(129, 131, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 316) {
    좌표입력(128, 136, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 317) {
    좌표입력(128, 141, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 318) {
    좌표입력(128, 145, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 319) {
    좌표입력(128, 150, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 320) {
    좌표입력(128, 152, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 321) {
    좌표입력(126, 154, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 322) {
    좌표입력(135, 158, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 323) {
    좌표입력(143, 149, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 324) {
    좌표입력(137, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 325) {
    좌표입력(129, 136, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 326) {
    좌표입력(136, 130, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 327) {
    좌표입력(139, 130, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 328) {
    좌표입력(149, 135, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 329) {
    좌표입력(156, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 330) {
    좌표입력(165, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 331) {
    좌표입력(166, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 332) {
    좌표입력(176, 153, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 333) {
    좌표입력(179, 158, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 334) {
    좌표입력(183, 163, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 335) {
    좌표입력(192, 170, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 336) {
    좌표입력(184, 173, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 337) {
    좌표입력(189, 176, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 338) {
    좌표입력(196, 180, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 339) {
    좌표입력(204, 165, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 340) {
    좌표입력(213, 170, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 341) {
    좌표입력(220, 173, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 342) {
    좌표입력(215, 175, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 343) {
    좌표입력(219, 174, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 344) {
    좌표입력(218, 171, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 345) {
    좌표입력(220, 163, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 346) {
    좌표입력(225, 157, 1)
    RunMemory("좌표이동")
    RunDirect := 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,100
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,100
}
Step = 1014
}
if(Cloute = 1) ;내가 만든 루트 184개
{
if(MapNumber = 1)
{
좌표입력(185,124,1)
RunMemory("좌표이동")
RunDirect := 0
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
RunDirect := 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,100
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,100
}
Step = 1014
}
}
CharMovePonam(Loute1,Loute2,Loute3,Loute4)
{
if (Loute1 = 1) ; 시계
{
if(랜덤감응 = 0)
{
Random, Myloute, 1, 3
    if (Myloute = 1)
    {
        Aloute := 1
        Bloute := 0
        Cloute := 0
    }
    else if (Myloute = 2)
    {
        Aloute := 0
        Bloute := 1
        Cloute := 0
    }
    else if (Myloute = 3)
    {
    Aloute := 0
    Bloute := 0
    Cloute := 1
    }
    랜덤감응 := 1
}
if (ALoute = 1)
{
;Gosub, 감응
if(MapNumber = 1)
{
RunDirect = 0
}
if (MapNumber = 2) {
    좌표입력(126, 67, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 3) {
    좌표입력(126, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 4) {
    좌표입력(128, 86, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 5) {
    좌표입력(121, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 6) {
    좌표입력(130, 98, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 7) {
    좌표입력(124, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 8) {
    좌표입력(129, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 9) {
    좌표입력(125, 119, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 10) {
    좌표입력(131, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 11) {
    좌표입력(127, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 12) {
    좌표입력(133, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 13) {
    좌표입력(124, 149, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 14) {
    좌표입력(114, 153, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 15) {
    좌표입력(104, 165, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 16) {
    좌표입력(97, 174, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 17) {
    좌표입력(95, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 18) {
    좌표입력(89, 174, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 19) {
    좌표입력(88, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 20) {
    좌표입력(85, 150, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 21) {
    좌표입력(73, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 22) {
    좌표입력(60, 127, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 23) {
    좌표입력(62, 115, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 24) {
    좌표입력(64, 107, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 25) {
    좌표입력(75, 98, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 26) {
    좌표입력(64, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 27) {
    좌표입력(60, 82, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 28) {
    좌표입력(56, 71, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 29) {
    좌표입력(58, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 30) {
    좌표입력(62, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 31) {
    좌표입력(50, 45, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 32) {
    좌표입력(41, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 33) {
    좌표입력(34, 29, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 34) {
    좌표입력(34, 16, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 35) {
    좌표입력(22, 21, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 36) {
    좌표입력(16, 29, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 37) {
    좌표입력(16, 41, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 38) {
    좌표입력(21, 51, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 39) {
    좌표입력(20, 68, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 40) {
    좌표입력(21, 79, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 41) {
    좌표입력(25, 91, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 42) {
    좌표입력(29, 101, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 43) {
    좌표입력(30, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 44) {
    좌표입력(23, 119, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 45) {
    좌표입력(25, 132, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 46) {
    좌표입력(24, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 47) {
    좌표입력(25, 157, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 48) {
    좌표입력(33, 166, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 49) {
    좌표입력(39, 176, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 50) {
    좌표입력(60, 176, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 51) {
    좌표입력(50, 163, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 52) {
    좌표입력(34, 148, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 53) {
    좌표입력(27, 137, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 54) {
    좌표입력(23, 124, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 55) {
    좌표입력(34, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 56) {
    좌표입력(28, 96, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 57) {
    좌표입력(22, 78, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 58) {
    좌표입력(25, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 59) {
    좌표입력(21, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 60) {
    좌표입력(30, 26, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 61) {
    좌표입력(28, 18, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 62) {
    좌표입력(47, 22, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 63) {
    좌표입력(43, 33, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 64) {
    좌표입력(46, 44, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 65) {
    좌표입력(55, 51, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 66) {
    좌표입력(64, 58, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 67) {
    좌표입력(76, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 68) {
    좌표입력(89, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 69) {
    좌표입력(93, 84, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 70) {
    좌표입력(99, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 71) {
    좌표입력(105, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 72) {
    좌표입력(110, 115, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 73) {
    좌표입력(116, 125, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 74) {
    좌표입력(120, 137, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 75) {
    좌표입력(135, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 76) {
    좌표입력(140, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 77) {
    좌표입력(144, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 78) {
    좌표입력(145, 117, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 79) {
    좌표입력(144, 107, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 80) {
    좌표입력(144, 98, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 81) {
    좌표입력(145, 87, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 82) {
    좌표입력(147, 75, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 83) {
    좌표입력(151, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 84) {
    좌표입력(162, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 85) {
    좌표입력(162, 85, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 86) {
    좌표입력(165, 97, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 87) {
    좌표입력(166, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 88) {
    좌표입력(168, 121, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 89) {
    좌표입력(167, 132, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 90) {
    좌표입력(170, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 91) {
    좌표입력(172, 152, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 92) {
    좌표입력(179, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 93) {
    좌표입력(182, 178, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 94) {
    좌표입력(186, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 95) {
    좌표입력(198, 183, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 96) {
    좌표입력(203, 180, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 97) {
    좌표입력(203, 173, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 98) {
    좌표입력(202, 164, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 99) {
    좌표입력(204, 153, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 100) {
    좌표입력(205, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 101) {
    좌표입력(208, 129, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 102) {
    좌표입력(207, 118, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 103) {
    좌표입력(209, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 104) {
    좌표입력(210, 99, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 105) {
    좌표입력(213, 84, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 106) {
    좌표입력(208, 74, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 107) {
    좌표입력(206, 67, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 108) {
    좌표입력(213, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 109) {
    좌표입력(219, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 110) {
    좌표입력(216, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 111) {
    좌표입력(215, 33, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 112) {
    좌표입력(215, 24, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 113) {
    좌표입력(224, 30, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 114) {
    좌표입력(230, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 115) {
    좌표입력(227, 46, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 116) {
    좌표입력(230, 47, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 117) {
    좌표입력(233, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 118) {
    좌표입력(232, 67, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 119) {
    좌표입력(233, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 120) {
    좌표입력(234, 86, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 121) {
    좌표입력(229, 107, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 122) {
    좌표입력(232, 116, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 123) {
    좌표입력(231, 130, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 124) {
    좌표입력(233, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 125) {
    좌표입력(233, 152, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 126) {
    좌표입력(233, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 127) {
    좌표입력(234, 171, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 128) {
    좌표입력(226, 181, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 129) {
    좌표입력(229, 183, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 130) {
    좌표입력(233, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 131) {
    좌표입력(222, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 132) {
    좌표입력(213, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 133) {
    좌표입력(204, 179, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 134) {
    좌표입력(197, 173, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 135) {
    좌표입력(185, 164, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 136) {
    좌표입력(175, 167, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 137) {
    좌표입력(178, 154, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 138) {
    좌표입력(174, 147, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 139) {
    좌표입력(172, 133, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 140) {
    좌표입력(170, 123, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 141) {
    좌표입력(171, 113, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 142) {
    좌표입력(164, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 143) {
    좌표입력(177, 95, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 144) {
    좌표입력(167, 86, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 145) {
    좌표입력(179, 76, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 146) {
    좌표입력(169, 68, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 147) {
    좌표입력(178, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 148) {
    좌표입력(168, 51, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 149) {
    좌표입력(176, 41, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 150) {
    좌표입력(164, 32, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 151) {
    좌표입력(151, 30, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 152) {
    좌표입력(149, 40, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 153) {
    좌표입력(147, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 154) {
    좌표입력(128, 57, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 155) {
    좌표입력(125, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 156) {
    좌표입력(114, 49, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 157) {
    좌표입력(113, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 158) {
    좌표입력(123, 65, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 159) {
    좌표입력(138, 68, 1)
    RunMemory("좌표이동")
    RunDirect = 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,200
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,200
}
}
if (BLoute = 1)
{
;Gosub, 감응
if(MapNumber = 1)
{
RunDirect = 0
}
if (MapNumber = 2) {
    좌표입력(127, 23, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 3) {
    좌표입력(127, 27, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 4) {
    좌표입력(121, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 5) {
    좌표입력(124, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 6) {
    좌표입력(137, 46, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 7) {
    좌표입력(132, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 8) {
    좌표입력(128, 60, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 9) {
    좌표입력(138, 60, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 10) {
    좌표입력(147, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 11) {
    좌표입력(156, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 12) {
    좌표입력(158, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 13) {
    좌표입력(163, 45, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 14) {
    좌표입력(167, 39, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 15) {
    좌표입력(179, 27, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 16) {
    좌표입력(196, 26, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 17) {
    좌표입력(210, 24, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 18) {
    좌표입력(220, 19, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 19) {
    좌표입력(230, 17, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 20) {
    좌표입력(218, 24, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 21) {
    좌표입력(226, 30, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 22) {
    좌표입력(229, 36, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 23) {
    좌표입력(218, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 24) {
    좌표입력(219, 46, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 25) {
    좌표입력(226, 46, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 26) {
    좌표입력(227, 53, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 27) {
    좌표입력(226, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 28) {
    좌표입력(230, 69, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 29) {
    좌표입력(220, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 30) {
    좌표입력(207, 66, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 31) {
    좌표입력(196, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 32) {
    좌표입력(185, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 33) {
    좌표입력(178, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 34) {
    좌표입력(174, 53, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 35) {
    좌표입력(162, 65, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 36) {
    좌표입력(152, 58, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 37) {
    좌표입력(144, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 38) {
    좌표입력(134, 70, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 39) {
    좌표입력(128, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 40) {
    좌표입력(119, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 41) {
    좌표입력(112, 65, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 42) {
    좌표입력(105, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 43) {
    좌표입력(95, 54, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 44) {
    좌표입력(93, 54, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 45) {
    좌표입력(87, 47, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 46) {
    좌표입력(88, 40, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 47) {
    좌표입력(92, 33, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 48) {
    좌표입력(97, 26, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 49) {
    좌표입력(99, 18, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 50) {
    좌표입력(84, 16, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 51) {
    좌표입력(78, 23, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 52) {
    좌표입력(71, 28, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 53) {
    좌표입력(65, 34, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 54) {
    좌표입력(58, 44, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 55) {
    좌표입력(45, 48, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 56) {
    좌표입력(56, 53, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 57) {
    좌표입력(63, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 58) {
    좌표입력(73, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 59) {
    좌표입력(84, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 60) {
    좌표입력(92, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 61) {
    좌표입력(99, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 62) {
    좌표입력(107, 56, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 63) {
    좌표입력(110, 49, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 64) {
    좌표입력(106, 42, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 65) {
    좌표입력(100, 39, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 66) {
    좌표입력(96, 46, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 67) {
    좌표입력(92, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 68) {
    좌표입력(87, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 69) {
    좌표입력(84, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 70) {
    좌표입력(78, 38, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 71) {
    좌표입력(67, 33, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 72) {
    좌표입력(55, 33, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 73) {
    좌표입력(53, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 74) {
    좌표입력(61, 47, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 75) {
    좌표입력(67, 47, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 76) {
    좌표입력(77, 51, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 77) {
    좌표입력(79, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 78) {
    좌표입력(89, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 79) {
    좌표입력(89, 54, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 80) {
    좌표입력(99, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 81) {
    좌표입력(108, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 82) {
    좌표입력(112, 65, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 83) {
    좌표입력(122, 69, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 84) {
    좌표입력(134, 71, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 85) {
    좌표입력(146, 67, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 86) {
    좌표입력(155, 72, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 87) {
    좌표입력(167, 69, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 88) {
    좌표입력(177, 68, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 89) {
    좌표입력(188, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 90) {
    좌표입력(200, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 91) {
    좌표입력(208, 68, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 92) {
    좌표입력(215, 72, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 93) {
    좌표입력(222, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 94) {
    좌표입력(232, 81, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 95) {
    좌표입력(238, 83, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 96) {
    좌표입력(249, 84, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 97) {
    좌표입력(253, 91, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 98) {
    좌표입력(264, 96, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 99) {
    좌표입력(274, 101, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 100) {
    좌표입력(281, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 101) {
    좌표입력(82, 99, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 102) {
    좌표입력(91, 106, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 103) {
    좌표입력(100, 99, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 104) {
    좌표입력(106, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 105) {
    좌표입력(115, 98, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 106) {
    좌표입력(125, 103, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 107) {
    좌표입력(132, 99, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 108) {
    좌표입력(141, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 109) {
    좌표입력(147, 97, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 110) {
    좌표입력(153, 101, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 111) {
    좌표입력(160, 96, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 112) {
    좌표입력(170, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 113) {
    좌표입력(180, 97, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 114) {
    좌표입력(192, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 115) {
    좌표입력(199, 94, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 116) {
    좌표입력(209, 101, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 117) {
    좌표입력(216, 94, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 118) {
    좌표입력(226, 98, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 119) {
    좌표입력(234, 94, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 120) {
    좌표입력(227, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 121) {
    좌표입력(217, 104, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 122) {
    좌표입력(208, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 123) {
    좌표입력(197, 104, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 124) {
    좌표입력(186, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 125) {
    좌표입력(178, 104, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 126) {
    좌표입력(168, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 127) {
    좌표입력(157, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 128) {
    좌표입력(147, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 129) {
    좌표입력(139, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 130) {
    좌표입력(130, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 131) {
    좌표입력(120, 99, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 132) {
    좌표입력(109, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 133) {
    좌표입력(100, 97, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 134) {
    좌표입력(88, 103, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 135) {
    좌표입력(80, 99, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 136) {
    좌표입력(67, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 137) {
    좌표입력(57, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 138) {
    좌표입력(61, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 139) {
    좌표입력(70, 112, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 140) {
    좌표입력(80, 106, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 141) {
    좌표입력(88, 111, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 142) {
    좌표입력(96, 114, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 143) {
    좌표입력(102, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 144) {
    좌표입력(111, 113, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 145) {
    좌표입력(118, 107, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 146) {
    좌표입력(127, 112, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 147) {
    좌표입력(137, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 148) {
    좌표입력(146, 114, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 149) {
    좌표입력(155, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 150) {
    좌표입력(164, 117, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 151) {
    좌표입력(172, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 152) {
    좌표입력(175, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 153) {
    좌표입력(183, 115, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 154) {
    좌표입력(192, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 155) {
    좌표입력(200, 116, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 156) {
    좌표입력(206, 112, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 157) {
    좌표입력(215, 116, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 158) {
    좌표입력(224, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 159) {
    좌표입력(227, 116, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 160) {
    좌표입력(224, 125, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 161) {
    좌표입력(224, 133, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 162) {
    좌표입력(224, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 163) {
    좌표입력(218, 128, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 164) {
    좌표입력(207, 133, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 165) {
    좌표입력(198, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 166) {
    좌표입력(188, 133, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 167) {
    좌표입력(178, 125, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 168) {
    좌표입력(170, 131, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 169) {
    좌표입력(161, 125, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 170) {
    좌표입력(151, 130, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 171) {
    좌표입력(141, 135, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 172) {
    좌표입력(133, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 173) {
    좌표입력(127, 124, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 174) {
    좌표입력(117, 129, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 175) {
    좌표입력(112, 135, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 176) {
    좌표입력(98, 121, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 177) {
    좌표입력(87, 132, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 178) {
    좌표입력(78, 127, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 179) {
    좌표입력(77, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 180) {
    좌표입력(69, 131, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 181) {
    좌표입력(67, 131, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 182) {
    좌표입력(60, 125, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 183) {
    좌표입력(55, 132, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 184) {
    좌표입력(57, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 185) {
    좌표입력(51, 145, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 186) {
    좌표입력(62, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 187) {
    좌표입력(72, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 188) {
    좌표입력(86, 148, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 189) {
    좌표입력(94, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 190) {
    좌표입력(105, 149, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 191) {
    좌표입력(111, 159, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 192) {
    좌표입력(111, 167, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 193) {
    좌표입력(102, 173, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 194) {
    좌표입력(104, 183, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 195) {
    좌표입력(85, 179, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 196) {
    좌표입력(85, 165, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 197) {
    좌표입력(89, 158, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 198) {
    좌표입력(104, 151, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 199) {
    좌표입력(115, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 200) {
    좌표입력(123, 141, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 201) {
    좌표입력(131, 136, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 202) {
    좌표입력(141, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 203) {
    좌표입력(148, 138, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 204) {
    좌표입력(158, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 205) {
    좌표입력(168, 138, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 206) {
    좌표입력(175, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 207) {
    좌표입력(185, 133, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 208) {
    좌표입력(192, 141, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 209) {
    좌표입력(199, 133, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 210) {
    좌표입력(205, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 211) {
    좌표입력(217, 144, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 212) {
    좌표입력(230, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 213) {
    좌표입력(231, 144, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 214) {
    좌표입력(231, 151, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 215) {
    좌표입력(225, 155, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 216) {
    좌표입력(224, 164, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 217) {
    좌표입력(226, 177, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 218) {
    좌표입력(219, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 219) {
    좌표입력(206, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 220) {
    좌표입력(195, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 221) {
    좌표입력(180, 181, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 222) {
    좌표입력(184, 169, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 223) {
    좌표입력(198, 165, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 224) {
    좌표입력(186, 155, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 225) {
    좌표입력(172, 151, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 226) {
    좌표입력(168, 141, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 227) {
    좌표입력(158, 138, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 228) {
    좌표입력(148, 141, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 229) {
    좌표입력(139, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 230) {
    좌표입력(140, 124, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 231) {
    좌표입력(137, 112, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 232) {
    좌표입력(138, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 233) {
    좌표입력(147, 96, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 234) {
    좌표입력(135, 87, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 235) {
    좌표입력(145, 78, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 236) {
    좌표입력(132, 70, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 237) {
    좌표입력(126, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 238) {
    좌표입력(115, 60, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 239) {
    좌표입력(102, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 240) {
    좌표입력(85, 56, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 241) {
    좌표입력(69, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 242) {
    좌표입력(57, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 243) {
    좌표입력(42, 39, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 244) {
    좌표입력(38, 24, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 245) {
    좌표입력(24, 18, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 246) {
    좌표입력(16, 31, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 247) {
    좌표입력(16, 45, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 248) {
    좌표입력(20, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 249) {
    좌표입력(24, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 250) {
    좌표입력(26, 74, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 251) {
    좌표입력(28, 87, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 252) {
    좌표입력(22, 94, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 253) {
    좌표입력(19, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 254) {
    좌표입력(29, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 255) {
    좌표입력(30, 115, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 256) {
    좌표입력(20, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 257) {
    좌표입력(21, 139, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 258) {
    좌표입력(24, 151, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 259) {
    좌표입력(21, 164, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 260) {
    좌표입력(22, 175, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 261) {
    좌표입력(33, 182, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 262) {
    좌표입력(36, 191, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 263) {
    좌표입력(28, 199, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 264) {
    좌표입력(17, 205, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 265) {
    좌표입력(10, 204, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 266) {
    좌표입력(5, 198, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 267) {
    좌표입력(5, 187, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 268) {
    좌표입력(10, 178, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 269) {
    좌표입력(18, 167, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 270) {
    좌표입력(29, 158, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 271) {
    좌표입력(31, 145, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 272) {
    좌표입력(29, 137, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 273) {
    좌표입력(27, 124, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 274) {
    좌표입력(21, 118, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 275) {
    좌표입력(10, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 276) {
    좌표입력(3, 101, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 277) {
    좌표입력(2, 90, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 278) {
    좌표입력(8, 80, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 279) {
    좌표입력(10, 67, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 280) {
    좌표입력(8, 53, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 281) {
    좌표입력(14, 47, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 282) {
    좌표입력(17, 34, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 283) {
    좌표입력(28, 30, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 284) {
    좌표입력(39, 35, 1)
    RunMemory("좌표이동")
    RunDirect = 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,200
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,200
}
}
if (CLoute = 1)
{
;gosub, 감응
if (MapNumber = 1)
{
    RunDirect = 0
}
if (MapNumber = 2) {
    좌표입력(134, 24, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 3) {
    좌표입력(125, 30, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 4) {
    좌표입력(133, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 5) {
    좌표입력(121, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 6) {
    좌표입력(130, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 7) {
    좌표입력(123, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 8) {
    좌표입력(130, 65, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 9) {
    좌표입력(123, 72, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 10) {
    좌표입력(135, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 11) {
    좌표입력(146, 68, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 12) {
    좌표입력(155, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 13) {
    좌표입력(159, 63, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 14) {
    좌표입력(162, 57, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 15) {
    좌표입력(160, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 16) {
    좌표입력(157, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 17) {
    좌표입력(158, 35, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 18) {
    좌표입력(163, 30, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 19) {
    좌표입력(170, 36, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 20) {
    좌표입력(176, 34, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 21) {
    좌표입력(181, 30, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 22) {
    좌표입력(184, 22, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 23) {
    좌표입력(181, 16, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 24) {
    좌표입력(174, 18, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 25) {
    좌표입력(169, 19, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 26) {
    좌표입력(154, 26, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 27) {
    좌표입력(164, 32, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 28) {
    좌표입력(169, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 29) {
    좌표입력(176, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 30) {
    좌표입력(185, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 31) {
    좌표입력(196, 49, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 32) {
    좌표입력(205, 48, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 33) {
    좌표입력(208, 45, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 34) {
    좌표입력(217, 47, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 35) {
    좌표입력(223, 51, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 36) {
    좌표입력(231, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 37) {
    좌표입력(224, 65, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 38) {
    좌표입력(217, 69, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 39) {
    좌표입력(220, 74, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 40) {
    좌표입력(223, 78, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 41) {
    좌표입력(214, 81, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 42) {
    좌표입력(208, 79, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 43) {
    좌표입력(200, 72, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 44) {
    좌표입력(190, 72, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 45) {
    좌표입력(193, 78, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 46) {
    좌표입력(199, 83, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 47) {
    좌표입력(205, 85, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 48) {
    좌표입력(212, 88, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 49) {
    좌표입력(221, 90, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 50) {
    좌표입력(225, 97, 1)
    RunMemory("좌표이동")
}

if (MapNumber = 51) {
    좌표입력(226, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 52) {
    좌표입력(223, 106, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 53) {
    좌표입력(221, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 54) {
    좌표입력(214, 114, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 55) {
    좌표입력(218, 120, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 56) {
    좌표입력(223, 123, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 57) {
    좌표입력(230, 132, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 58) {
    좌표입력(225, 137, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 59) {
    좌표입력(218, 144, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 60) {
    좌표입력(223, 148, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 61) {
    좌표입력(222, 154, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 62) {
    좌표입력(229, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 63) {
    좌표입력(225, 168, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 64) {
    좌표입력(223, 172, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 65) {
    좌표입력(222, 177, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 66) {
    좌표입력(219, 181, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 67) {
    좌표입력(206, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 68) {
    좌표입력(200, 179, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 69) {
    좌표입력(193, 174, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 70) {
    좌표입력(198, 169, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 71) {
    좌표입력(204, 172, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 72) {
    좌표입력(195, 168, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 73) {
    좌표입력(188, 174, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 74) {
    좌표입력(178, 172, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 75) {
    좌표입력(181, 180, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 76) {
    좌표입력(180, 172, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 77) {
    좌표입력(177, 164, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 78) {
    좌표입력(184, 159, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 79) {
    좌표입력(179, 152, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 80) {
    좌표입력(185, 147, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 81) {
    좌표입력(178, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 82) {
    좌표입력(172, 141, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 83) {
    좌표입력(166, 147, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 84) {
    좌표입력(154, 147, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 85) {
    좌표입력(139, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 86) {
    좌표입력(130, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 87) {
    좌표입력(120, 139, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 88) {
    좌표입력(116, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 89) {
    좌표입력(114, 152, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 90) {
    좌표입력(106, 157, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 91) {
    좌표입력(105, 165, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 92) {
    좌표입력(99, 170, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 93) {
    좌표입력(93, 165, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 94) {
    좌표입력(89, 158, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 95) {
    좌표입력(92, 166, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 96) {
    좌표입력(94, 173, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 97) {
    좌표입력(95, 181, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 98) {
    좌표입력(106, 181, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 99) {
    좌표입력(110, 174, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 100) {
    좌표입력(106, 166, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 101) {
    좌표입력(104, 158, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 102) {
    좌표입력(98, 151, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 103) {
    좌표입력(104, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 104) {
    좌표입력(99, 138, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 105) {
    좌표입력(102, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 106) {
    좌표입력(108, 130, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 107) {
    좌표입력(103, 124, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 108) {
    좌표입력(97, 120, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 109) {
    좌표입력(94, 114, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 110) {
    좌표입력(91, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 111) {
    좌표입력(85, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 112) {
    좌표입력(91, 100, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 113) {
    좌표입력(98, 100, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 114) {
    좌표입력(107, 98, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 115) {
    좌표입력(114, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 116) {
    좌표입력(109, 88, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 117) {
    좌표입력(105, 84, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 118) {
    좌표입력(98, 83, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 119) {
    좌표입력(106, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 120) {
    좌표입력(97, 72, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 121) {
    좌표입력(100, 67, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 122) {
    좌표입력(106, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 123) {
    좌표입력(98, 58, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 124) {
    좌표입력(92, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 125) {
    좌표입력(85, 46, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 126) {
    좌표입력(82, 42, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 127) {
    좌표입력(84, 36, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 128) {
    좌표입력(79, 40, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 129) {
    좌표입력(79, 47, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 130) {
    좌표입력(76, 41, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 131) {
    좌표입력(76, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 132) {
    좌표입력(83, 31, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 133) {
    좌표입력(89, 25, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 134) {
    좌표입력(97, 24, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 135) {
    좌표입력(103, 23, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 136) {
    좌표입력(104, 27, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 137) {
    좌표입력(106, 33, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 138) {
    좌표입력(96, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 139) {
    좌표입력(89, 41, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 140) {
    좌표입력(82, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 141) {
    좌표입력(74, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 142) {
    좌표입력(64, 46, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 143) {
    좌표입력(55, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 144) {
    좌표입력(57, 57, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 145) {
    좌표입력(59, 63, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 146) {
    좌표입력(51, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 147) {
    좌표입력(40, 32, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 148) {
    좌표입력(28, 22, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 149) {
    좌표입력(16, 15, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 150) {
    좌표입력(21, 32, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 151) {
    좌표입력(16, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 152) {
    좌표입력(30, 53, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 153) {
    좌표입력(41, 67, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 154) {
    좌표입력(30, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 155) {
    좌표입력(18, 83, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 156) {
    좌표입력(33, 92, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 157) {
    좌표입력(42, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 158) {
    좌표입력(44, 112, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 159) {
    좌표입력(35, 118, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 160) {
    좌표입력(27, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 161) {
    좌표입력(16, 117, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 162) {
    좌표입력(17, 106, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 163) {
    좌표입력(26, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 164) {
    좌표입력(34, 82, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 165) {
    좌표입력(34, 72, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 166) {
    좌표입력(39, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 167) {
    좌표입력(28, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 168) {
    좌표입력(20, 41, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 169) {
    좌표입력(26, 27, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 170) {
    좌표입력(36, 32, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 171) {
    좌표입력(44, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 172) {
    좌표입력(55, 49, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 173) {
    좌표입력(67, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 174) {
    좌표입력(75, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 175) {
    좌표입력(83, 69, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 176) {
    좌표입력(93, 78, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 177) {
    좌표입력(101, 85, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 178) {
    좌표입력(111, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 179) {
    좌표입력(122, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 180) {
    좌표입력(130, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 181) {
    좌표입력(138, 115, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 182) {
    좌표입력(144, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 183) {
    좌표입력(150, 118, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 184) {
    좌표입력(145, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 185) {
    좌표입력(154, 132, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 186) {
    좌표입력(155, 141, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 187) {
    좌표입력(165, 145, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 188) {
    좌표입력(178, 151, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 189) {
    좌표입력(181, 156, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 190) {
    좌표입력(188, 161, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 191) {
    좌표입력(193, 171, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 192) {
    좌표입력(189, 176, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 193) {
    좌표입력(180, 179, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 194) {
    좌표입력(187, 174, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 195) {
    좌표입력(196, 169, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 196) {
    좌표입력(208, 176, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 197) {
    좌표입력(215, 180, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 198) {
    좌표입력(221, 174, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 199) {
    좌표입력(215, 169, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 200) {
    좌표입력(220, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 201) {
    좌표입력(213, 155, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 202) {
    좌표입력(210, 148, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 203) {
    좌표입력(218, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 204) {
    좌표입력(210, 137, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 205) {
    좌표입력(219, 130, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 206) {
    좌표입력(214, 123, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 207) {
    좌표입력(206, 121, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 208) {
    좌표입력(202, 112, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 209) {
    좌표입력(209, 104, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 210) {
    좌표입력(212, 99, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 211) {
    좌표입력(208, 92, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 212) {
    좌표입력(211, 83, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 213) {
    좌표입력(203, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 214) {
    좌표입력(196, 75, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 215) {
    좌표입력(188, 79, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 216) {
    좌표입력(182, 87, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 217) {
    좌표입력(178, 92, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 218) {
    좌표입력(174, 97, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 219) {
    좌표입력(170, 104, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 220) {
    좌표입력(160, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 221) {
    좌표입력(157, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 222) {
    좌표입력(147, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 223) {
    좌표입력(143, 114, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 224) {
    좌표입력(139, 117, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 225) {
    좌표입력(130, 122, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 226) {
    좌표입력(130, 125, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 227) {
    좌표입력(125, 128, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 228) {
    좌표입력(122, 133, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 229) {
    좌표입력(114, 136, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 230) {
    좌표입력(104, 144, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 231) {
    좌표입력(115, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 232) {
    좌표입력(107, 151, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 233) {
    좌표입력(104, 156, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 234) {
    좌표입력(100, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 235) {
    좌표입력(105, 166, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 236) {
    좌표입력(98, 171, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 237) {
    좌표입력(113, 158, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 238) {
    좌표입력(119, 151, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 239) {
    좌표입력(125, 145, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 240) {
    좌표입력(132, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 241) {
    좌표입력(139, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 242) {
    좌표입력(141, 128, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 243) {
    좌표입력(144, 122, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 244) {
    좌표입력(139, 114, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 245) {
    좌표입력(142, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 246) {
    좌표입력(135, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 247) {
    좌표입력(139, 95, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 248) {
    좌표입력(142, 87, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 249) {
    좌표입력(134, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 250) {
    좌표입력(142, 71, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 251) {
    좌표입력(131, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 252) {
    좌표입력(122, 57, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 253) {
    좌표입력(116, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 254) {
    좌표입력(127, 44, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 255) {
    좌표입력(137, 51, 1)
    RunMemory("좌표이동")
    RunDirect = 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,200
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,200
}
}
}
if (Loute2 = 1) ; 구석만
{
;Gosub, 감응
if(MapNumber = 1)
{
RunDirect = 0
}
if (MapNumber = 2) {
    좌표입력(126, 31, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 3) {
    좌표입력(137, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 4) {
    좌표입력(108, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 5) {
    좌표입력(76, 42, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 6) {
    좌표입력(56, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 7) {
    좌표입력(62, 83, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 8) {
    좌표입력(61, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 9) {
    좌표입력(51, 145, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 10) {
    좌표입력(96, 178, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 11) {
    좌표입력(128, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 12) {
    좌표입력(162, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 13) {
    좌표입력(178, 153, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 14) {
    좌표입력(188, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 15) {
    좌표입력(192, 173, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 16) {
    좌표입력(205, 182, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 17) {
    좌표입력(218, 182, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 18) {
    좌표입력(225, 173, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 19) {
    좌표입력(229, 164, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 20) {
    좌표입력(228, 153, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 21) {
    좌표입력(226, 131, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 22) {
    좌표입력(218, 118, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 23) {
    좌표입력(218, 80, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 24) {
    좌표입력(222, 71, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 25) {
    좌표입력(206, 66, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 26) {
    좌표입력(187, 79, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 27) {
    좌표입력(169, 88, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 28) {
    좌표입력(148, 88, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 29) {
    좌표입력(108, 82, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 30) {
    좌표입력(78, 79, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 31) {
    좌표입력(69, 81, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 32) {
    좌표입력(55, 57, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 33) {
    좌표입력(40, 19, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 34) {
    좌표입력(23, 28, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 35) {
    좌표입력(20, 53, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 36) {
    좌표입력(30, 54, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 37) {
    좌표입력(36, 74, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 38) {
    좌표입력(25, 97, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 39) {
    좌표입력(43, 106, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 40) {
    좌표입력(30, 128, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 41) {
    좌표입력(20, 148, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 42) {
    좌표입력(24, 111, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 43) {
    좌표입력(30, 90, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 44) {
    좌표입력(34, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 45) {
    좌표입력(29, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 46) {
    좌표입력(20, 27, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 47) {
    좌표입력(35, 20, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 48) {
    좌표입력(55, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 49) {
    좌표입력(74, 45, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 50) {
    좌표입력(91, 49, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 51) {
    좌표입력(108, 53, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 52) {
    좌표입력(123, 56, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 53) {
    좌표입력(130, 33, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 54) {
    좌표입력(136, 27, 1)
    RunMemory("좌표이동")
    RunDirect = 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,200
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,200
}
}
if (Loute3 = 1) ; 가로세로
{
;Gosub, 감응
if(MapNumber = 1)
{
RunDirect = 0
}
if (MapNumber = 2) {
    좌표입력(126, 67, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 3) {
    좌표입력(126, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 4) {
    좌표입력(128, 86, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 5) {
    좌표입력(121, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 6) {
    좌표입력(130, 98, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 7) {
    좌표입력(124, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 8) {
    좌표입력(129, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 9) {
    좌표입력(125, 119, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 10) {
    좌표입력(131, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 11) {
    좌표입력(127, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 12) {
    좌표입력(133, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 13) {
    좌표입력(124, 149, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 14) {
    좌표입력(114, 153, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 15) {
    좌표입력(104, 165, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 16) {
    좌표입력(97, 174, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 17) {
    좌표입력(95, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 18) {
    좌표입력(89, 174, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 19) {
    좌표입력(88, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 20) {
    좌표입력(85, 150, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 21) {
    좌표입력(73, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 22) {
    좌표입력(60, 127, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 23) {
    좌표입력(62, 115, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 24) {
    좌표입력(64, 107, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 25) {
    좌표입력(75, 98, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 26) {
    좌표입력(64, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 27) {
    좌표입력(60, 82, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 28) {
    좌표입력(56, 71, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 29) {
    좌표입력(58, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 30) {
    좌표입력(62, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 31) {
    좌표입력(50, 45, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 32) {
    좌표입력(41, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 33) {
    좌표입력(34, 29, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 34) {
    좌표입력(34, 16, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 35) {
    좌표입력(22, 21, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 36) {
    좌표입력(16, 29, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 37) {
    좌표입력(16, 41, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 38) {
    좌표입력(21, 51, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 39) {
    좌표입력(20, 68, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 40) {
    좌표입력(21, 79, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 41) {
    좌표입력(25, 91, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 42) {
    좌표입력(29, 101, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 43) {
    좌표입력(30, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 44) {
    좌표입력(23, 119, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 45) {
    좌표입력(25, 132, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 46) {
    좌표입력(24, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 47) {
    좌표입력(25, 157, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 48) {
    좌표입력(33, 166, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 49) {
    좌표입력(39, 176, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 50) {
    좌표입력(60, 176, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 51) {
    좌표입력(50, 163, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 52) {
    좌표입력(34, 148, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 53) {
    좌표입력(27, 137, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 54) {
    좌표입력(23, 124, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 55) {
    좌표입력(34, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 56) {
    좌표입력(28, 96, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 57) {
    좌표입력(22, 78, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 58) {
    좌표입력(25, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 59) {
    좌표입력(21, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 60) {
    좌표입력(30, 26, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 61) {
    좌표입력(28, 18, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 62) {
    좌표입력(47, 22, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 63) {
    좌표입력(43, 33, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 64) {
    좌표입력(46, 44, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 65) {
    좌표입력(55, 51, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 66) {
    좌표입력(64, 58, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 67) {
    좌표입력(76, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 68) {
    좌표입력(89, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 69) {
    좌표입력(93, 84, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 70) {
    좌표입력(99, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 71) {
    좌표입력(105, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 72) {
    좌표입력(110, 115, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 73) {
    좌표입력(116, 125, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 74) {
    좌표입력(120, 137, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 75) {
    좌표입력(135, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 76) {
    좌표입력(140, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 77) {
    좌표입력(144, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 78) {
    좌표입력(145, 117, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 79) {
    좌표입력(144, 107, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 80) {
    좌표입력(144, 98, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 81) {
    좌표입력(145, 87, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 82) {
    좌표입력(147, 75, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 83) {
    좌표입력(151, 64, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 84) {
    좌표입력(162, 73, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 85) {
    좌표입력(162, 85, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 86) {
    좌표입력(165, 97, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 87) {
    좌표입력(166, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 88) {
    좌표입력(168, 121, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 89) {
    좌표입력(167, 132, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 90) {
    좌표입력(170, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 91) {
    좌표입력(172, 152, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 92) {
    좌표입력(179, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 93) {
    좌표입력(182, 178, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 94) {
    좌표입력(186, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 95) {
    좌표입력(198, 183, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 96) {
    좌표입력(203, 180, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 97) {
    좌표입력(203, 173, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 98) {
    좌표입력(202, 164, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 99) {
    좌표입력(204, 153, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 100) {
    좌표입력(205, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 101) {
    좌표입력(208, 129, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 102) {
    좌표입력(207, 118, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 103) {
    좌표입력(209, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 104) {
    좌표입력(210, 99, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 105) {
    좌표입력(213, 84, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 106) {
    좌표입력(208, 74, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 107) {
    좌표입력(206, 67, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 108) {
    좌표입력(213, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 109) {
    좌표입력(219, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 110) {
    좌표입력(216, 43, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 111) {
    좌표입력(215, 33, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 112) {
    좌표입력(215, 24, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 113) {
    좌표입력(224, 30, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 114) {
    좌표입력(230, 37, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 115) {
    좌표입력(227, 46, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 116) {
    좌표입력(230, 47, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 117) {
    좌표입력(233, 55, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 118) {
    좌표입력(232, 67, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 119) {
    좌표입력(233, 77, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 120) {
    좌표입력(234, 86, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 121) {
    좌표입력(229, 107, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 122) {
    좌표입력(232, 116, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 123) {
    좌표입력(231, 130, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 124) {
    좌표입력(233, 143, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 125) {
    좌표입력(233, 152, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 126) {
    좌표입력(233, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 127) {
    좌표입력(234, 171, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 128) {
    좌표입력(226, 181, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 129) {
    좌표입력(229, 183, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 130) {
    좌표입력(233, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 131) {
    좌표입력(222, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 132) {
    좌표입력(213, 184, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 133) {
    좌표입력(204, 179, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 134) {
    좌표입력(197, 173, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 135) {
    좌표입력(185, 164, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 136) {
    좌표입력(175, 167, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 137) {
    좌표입력(178, 154, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 138) {
    좌표입력(174, 147, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 139) {
    좌표입력(172, 133, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 140) {
    좌표입력(170, 123, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 141) {
    좌표입력(171, 113, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 142) {
    좌표입력(164, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 143) {
    좌표입력(177, 95, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 144) {
    좌표입력(167, 86, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 145) {
    좌표입력(179, 76, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 146) {
    좌표입력(169, 68, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 147) {
    좌표입력(178, 59, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 148) {
    좌표입력(168, 51, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 149) {
    좌표입력(176, 41, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 150) {
    좌표입력(164, 32, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 151) {
    좌표입력(151, 30, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 152) {
    좌표입력(149, 40, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 153) {
    좌표입력(147, 50, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 154) {
    좌표입력(128, 57, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 155) {
    좌표입력(125, 52, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 156) {
    좌표입력(114, 49, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 157) {
    좌표입력(113, 62, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 158) {
    좌표입력(123, 65, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 159) {
    좌표입력(138, 68, 1)
    RunMemory("좌표이동")
    RunDirect = 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,200
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,200
}
}
if (Loute4 = 1) ; 아래만
{
;Gosub, 감응
if(MapNumber = 1)
{
RunDirect = 0
}
if (MapNumber = 2) {
    좌표입력(68, 123, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 3) {
    좌표입력(66, 133, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 4) {
    좌표입력(65, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 5) {
    좌표입력(52, 148, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 6) {
    좌표입력(69, 151, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 7) {
    좌표입력(82, 149, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 8) {
    좌표입력(93, 171, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 9) {
    좌표입력(92, 166, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 10) {
    좌표입력(92, 142, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 11) {
    좌표입력(92, 139, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 12) {
    좌표입력(98, 129, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 13) {
    좌표입력(94, 119, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 14) {
    좌표입력(99, 107, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 15) {
    좌표입력(100, 91, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 16) {
    좌표입력(108, 87, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 17) {
    좌표입력(109, 97, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 18) {
    좌표입력(110, 109, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 19) {
    좌표입력(113, 121, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 20) {
    좌표입력(110, 131, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 21) {
    좌표입력(112, 141, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 22) {
    좌표입력(121, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 23) {
    좌표입력(127, 127, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 24) {
    좌표입력(130, 119, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 25) {
    좌표입력(129, 108, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 26) {
    좌표입력(131, 100, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 27) {
    좌표입력(142, 94, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 28) {
    좌표입력(148, 104, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 29) {
    좌표입력(151, 113, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 30) {
    좌표입력(152, 121, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 31) {
    좌표입력(152, 131, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 32) {
    좌표입력(151, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 33) {
    좌표입력(148, 149, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 34) {
    좌표입력(162, 150, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 35) {
    좌표입력(174, 154, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 36) {
    좌표입력(162, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 37) {
    좌표입력(170, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 38) {
    좌표입력(174, 121, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 39) {
    좌표입력(162, 113, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 40) {
    좌표입력(170, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 41) {
    좌표입력(162, 97, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 42) {
    좌표입력(170, 89, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 43) {
    좌표입력(177, 85, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 44) {
    좌표입력(186, 93, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 45) {
    좌표입력(181, 101, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 46) {
    좌표입력(188, 110, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 47) {
    좌표입력(180, 117, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 48) {
    좌표입력(188, 125, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 49) {
    좌표입력(181, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 50) {
    좌표입력(189, 144, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 51) {
    좌표입력(183, 152, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 52) {
    좌표입력(190, 162, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 53) {
    좌표입력(181, 173, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 54) {
    좌표입력(197, 181, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 55) {
    좌표입력(207, 183, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 56) {
    좌표입력(215, 179, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 57) {
    좌표입력(209, 174, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 58) {
    좌표입력(216, 167, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 59) {
    좌표입력(209, 160, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 60) {
    좌표입력(213, 152, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 61) {
    좌표입력(206, 146, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 62) {
    좌표입력(213, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 63) {
    좌표입력(206, 133, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 64) {
    좌표입력(216, 125, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 65) {
    좌표입력(209, 122, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 66) {
    좌표입력(218, 113, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 67) {
    좌표입력(223, 127, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 68) {
    좌표입력(224, 137, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 69) {
    좌표입력(206, 134, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 70) {
    좌표입력(198, 126, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 71) {
    좌표입력(190, 116, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 72) {
    좌표입력(183, 106, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 73) {
    좌표입력(176, 97, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 74) {
    좌표입력(169, 105, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 75) {
    좌표입력(164, 116, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 76) {
    좌표입력(163, 124, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 77) {
    좌표입력(162, 135, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 78) {
    좌표입력(154, 139, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 79) {
    좌표입력(152, 148, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 80) {
    좌표입력(145, 140, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 81) {
    좌표입력(142, 132, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 82) {
    좌표입력(142, 121, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 83) {
    좌표입력(144, 112, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 84) {
    좌표입력(146, 102, 1)
    RunMemory("좌표이동")
}
if (MapNumber = 85) {
    좌표입력(141, 90, 1)
    RunMemory("좌표이동")
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,200
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,200
}
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
감응:
Gui, Submit, Nohide
if(Gui_KON = 1)
{
IfInString,Location,[알파차원] 포프레스네 마을
{
Sleep, 100
포남입장시간 := A_TickCount
countsignal := 0
}
IfInString,Location,[알파차원] 포프레스네 남쪽
{
if (countsignal = 0)
{
value := jelan.write(0x00527B1C, A동파, "UInt")
value := jelan.write(0x00527B1C, A동파, "UInt")
Sleep, 30
Send, {F14}
Sleep, 100
Send, {F14}
Sleep, 100
countsignal += 1
호출대상 := "알파 - 동쪽파수꾼"
return
}
if (countsignal = 1)
{
value := jelan.write(0x00527B1C, A서파, "UInt")
value := jelan.write(0x00527B1C, A서파, "UInt")
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
IfInString,Location,[베타차원] 포프레스네 남쪽
{
if (countsignal = 0)
{
value := jelan.write(0x00527B1C, B동파, "UInt")
value := jelan.write(0x00527B1C, B동파, "UInt")
Sleep, 30
Send, {F14}
Sleep, 100
Send, {F14}
Sleep, 100
countsignal += 1
호출대상 := "베타 - 동쪽파수꾼"
return
}
if (countsignal = 1)
{
value := jelan.write(0x00527B1C, B서파, "UInt")
value := jelan.write(0x00527B1C, B서파, "UInt")
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
IfInString,Location,[감마차원] 포프레스네 남쪽
{
if (countsignal = 0)
{
value := jelan.write(0x00527B1C, G동파, "UInt")
value := jelan.write(0x00527B1C, G동파, "UInt")
Sleep, 30
Send, {F14}
Sleep, 100
Send, {F14}
Sleep, 100
countsignal := 1
호출대상 := "감마 - 동쪽파수꾼"
return
}
if (countsignal = 1)
{
value := jelan.write(0x00527B1C, G서파, "UInt")
value := jelan.write(0x00527B1C, G서파, "UInt")
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
        TMessage := "[ Helancia_Log ]>>" jTitle "<<: 인터넷 오류. 리로드, 위치보고: " . "(" . 현재차원  . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
        텔레그램메시지보내기(TMessage)
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
        TMessage := "[ Helancia_Log ]>>" jTitle "<<: 인터넷 오류 2. 리로드, 위치보고: " . "(" . 현재차원  . ")" 맵이름 . Gui_NowLocation . ", 좌표: (" . 좌표X . "," . 좌표Y . "," . 좌표Z . ")"
        텔레그램메시지보내기(TMessage)
GOSUB, RL
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
SB_SetText("좌표이동",4)
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
else if (코드 = "아이템줍기코드") {
			Addrs := 0x005902E5
			RegionSize := 0x1400
			target = 04EE3005038B600178808B008B000000BE808B000032F88314408B008000000111840F0F00005901E53D3D81000000718474C544005901E500000061840FC704361D89625B8B460500038B00593B83664300590401E51D8DF175004A0500038B00593B8366430059040446158BF1750059044A3D8B00590020840FFA39005904361D8B00000059044605C7004A05C70000000000000000005904C700000093850F0000005904460559044A05C70000EE8300000000007C1D895E5E8B280C4E8B600059048B0059043E0D885904420D88104EF69305E8236A00FFF404D0E859FF19488810244C8A0D8B14244C8B66488966005904421D8B14244C8A1C5889660059043E7089661E48881A488B217889661FC5B9390C6C8104C7402454FF000600000059047C055B5E5F61610000C70010C25DE58B0000005904460559044A05C700005F61000000000010C25DE58B5B5E00000000000000000000000000000000000000000074C54400000000C7740020D15CC70000000000B984000000000000000000000000000000000000000000546E3400000000FFFF000000000000FFFFFFFFFFFF0000000000000000000000000000000000000000000000000000000000
			executable := jelan.executable(Addrs, RegionSize)
			}
else if (코드 = "아이템줍기실행") {
			Addrs := 0x00590A00
			target = C29000129847E9
			}
else if (코드 = "아이템줍기정지") {
			Addrs := 0x00590A00
			target = C25DE58B5B5E5F
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
useRAS()
{
SETTITLEMATCHMODE, 3
WINGET, pid, PID, ahk_pid %jPID%
ProcHwnd := DllCall("OpenProcess", "Int", 2035711, "Char", 0, "UInt", pid, "UInt")
DllCall("CreateRemoteThread", "Ptr", ProcHwnd, "Ptr", 0, "Ptr", 0, "Ptr", UAD, "Ptr", 0, "UInt", 0, "Ptr", 0,"Ptr")
DllCall("CloseHandle", "int", ProcHwnd)
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
GetMemory:
GMS := GlobalMemoryStatusEx()
TPM := Round(GMS[2] / 1024**2, 2)
APM := Round(GMS[3] / 1024**2, 2)
UPM := Round(TPM - APM, 2)
UPP := Round(UPM / TPM * 100, 2)
GuiControl,, TMemory, % TPM . " MB"
GuiControl,, FMemory, % APM . " MB"
GuiControl,, UMemory, % UPM . " MB"
GuiControl,, FreePerc, % Round((100 - (TPM - APM) / TPM * 100), 2) " %"
GuiControl,, UsedPerc, % Round(((TPM - APM) / TPM * 100), 2) " %"
GuiControl +Range0-%TPM%, PFMemory
GuiControl,, PFMemory, % APM
GuiControl, % ((UPP < 70)? "+c5BB75E" : ((UPP < 80) ? "+cFFC266" : "+cDA4F49")), PFMemory
GuiControl +Range0-%TPM%, PUMemory
GuiControl,, PUMemory, % UPM
GuiControl, % ((UPP < 70)? "+c5BB75E" : ((UPP < 80) ? "+cFFC266" : "+cDA4F49")), PUMemory
return
ClearMem:
GMSC := GlobalMemoryStatusEx()
GMSCA := Round(GMSC[3] / 1024**2, 2)
ClearMemory()
FreeMemory()
GMSC := GlobalMemoryStatusEx()
GMSCB := Round(GMSC[3] / 1024**2, 2)
GuiControl,, CMMemory, % Round(GMSCB - GMSCA, 2) . " MB 확보"
return
GlobalMemoryStatusEx()
{
static MEMORYSTATUSEX, init := VarSetCapacity(MEMORYSTATUSEX, 64, 0) && NumPut(64, MEMORYSTATUSEX, "UInt")
if (DllCall("Kernel32.dll\GlobalMemoryStatusEx", "Ptr", &MEMORYSTATUSEX))
{
return { 2 : NumGet(MEMORYSTATUSEX, 8, "UInt64")
, 3 : NumGet(MEMORYSTATUSEX, 16, "UInt64") }
}
}
ClearMemory()
{
for process in ComObjGet("winmgmts:\\.\root\CIMV2").ExecQuery("SELECT * FROM Win32_Process")
{
handle := DllCall("Kernel32.dll\OpenProcess", "UInt", 0x001F0FFF, "Int", 0, "Int", process.ProcessID)
DllCall("Kernel32.dll\SetProcessWorkingSetSize", "UInt", handle, "Int", -1, "Int", -1)
DllCall("Psapi.dll\EmptyWorkingSet", "UInt", handle)
DllCall("Kernel32.dll\CloseHandle", "Int", handle)
}
return
}
FreeMemory()
{
return DllCall("Psapi.dll\EmptyWorkingSet", "UInt", -1)
}
종료:
Gui, Submit, NoHide
if( Gui_login = "넥슨플러그" )
{
WinShow, ahk_exe NexonPlug.exe
}
CRITICAL,On
Gosub, GuiClose
CRITICAL,OFF
Return
어빌리티탭확인:
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
return

Check_좌표:
{
Gui, Submit, Nohide
FormatTime,점검,A_Now,HHmm
if(A_WDay = 5)
{
if(업데이트체크 = 1)
{
Entrance = 0
SB_SetText("OID 업데이트 완료")
return
}
if(업데이트체크 = 0)
{
Entrance = 0
MsgBox, , 비정상종료감지, OID리셋, 3
TMessage := "[ Helancia_Log ]>>" . jTitle "<<: 점검요일. OID 리셋."
텔레그램메시지보내기(TMessage)
gosub, OID리셋
if(Gui_KON = 1)
{
MsgBox, , 자동감응설정, 감응 OFF로 변경, 1
GuiControl, , Gui_KOFF, 1
업데이트체크 := 1
}
SB_SetText("점검 요일로 OID리셋")
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
RasCount := 아이템갯수["라스의깃"]
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
^g::
if( 좌표고정 != 0)
{
CoordMode,mouse,Screen
MouseGetPos,게임시작x,게임시작y
GuiControl, , 좌표x, %게임시작x%
GuiControl, , 좌표y, %게임시작y%
return
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
settimer, 파라스대기, off
파라스타이머시작 := 0
파라스방해감지 := 0
CheckPB = 0
CheckPN := 0
countsignal := 0
랜덤감응 = 0
MapNumber = 1
타겟number := 0
MobNumber = 1
GuiControl, , Gui_NowState, [포남] 파라스 다시 가보기.
SB_SetText("파라스가 풀렸는지 확인")
   TMessage :="[ Helancia_Log ]>>" jTitle . "<<: 포남이 정상화 됐는지 다시 가봅니다"
GuiControl, , Gui_HuntAuto, 1
Step = 8
랜덤감응 = 0
keyclick("tab")
return

return
일시정지:
Gui, Submit, Nohide
keyclick("tab")
if( Gui_login = "넥슨플러그" )
{
WinMinimize, ahk_exe NexonPlug.exe
}
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
jelan.write(0x0058FFE0,0,"UInt", aOffsets*)
jelan.write(0x0058DAD4, 400, "UInt", 0x178, 0x9C)
jelan.write(0x0058DAD4, 400, "UInt", 0x178, 0x98)
sleep,100
jelan.write(0x0045D28F, 0x0F, "Char", aOffsets*)
jelan.write(0x0045D290, 0x84, "Char", aOffsets*)
jelan.write(0x0045D291, 0xC2, "Char", aOffsets*)
jelan.write(0x0045D292, 0x00, "Char", aOffsets*)
jelan.write(0x0045D293, 0x00, "Char", aOffsets*)
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
Sleep,100
Set_nomalSpeed()
AltR()
Pause
SB_SetText("일시정지 상태")
return
재시작:
keyclick("tab")
Gui, Submit, Nohide
GuiControl,,Gui_Nowstate, 곧 재시작 합니다.
SB_SetText("3초후 재시작합니다.")
sleep,3000
GuiControl,Disabled,Gui_RestartButton
GuiControl,Hide,Gui_RestartButton
GuiControl,Enable,Gui_StopButton
GuiControl,Show,Gui_StopButton
Set_MoveSpeed()
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
포북캐릭()
}
else if(HuntPlace = 3)
{
머미캐릭()
}
jelan.write(0x0047AD18, 0xEB, "Char", aOffsets*)
Sleep, 100
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
RandomRadio()
MsgBox,48, 차원설정,파티를 사용하기 위해 따악 좋은 위치 추천해준다.,1
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
원격마법설명서:
Gui, Submit, Nohide
MsgBox,576, 원격파티,
(
	<원격마법 사용법>`n`n
1. 체크시 원격마법사용 체크 해라.`n
2. 1번엔 (엘)리메듐을 놓고, 2번엔 브렐을 놔라, 아몰랑과 만능링을 끼면 좋다.
   무기는 스태프와 하프 등 사용해라.[마법사,악사 계열만 원격마법을 사용가능]`n
3. HP가 어느정도 되면 체력을 회복할지 적어라. 그리고 인게임 내 스펠슬롯 칸 어디까지 사용할 건지를 체크해라. 병원회복설정 해뒀으면 그거보다 체력이 높게 설정해라`n
4. 엘리메듐과 나프작 어느정도 어빌 셋팅은 기본적으로 되어있는게 좋다.`n
   나머진 나도 안해봐서 하면서 공유해라ㅋ
)
return
원격파티설명서:
Gui, Submit, Nohide
MsgBox,576, 원격파티,
(
	<원격파티 사용법>`n`n
1. 해당 VMWARE 안에다가 파티할캐릭터 켜라.`n
2. 닉네임을 정확하게 입력 후 해당 캐릭터번호를 선택해라.
   캐릭터번호는 ( 캐릭터선택창 순서 입니다. )`n
3. 차원설정을 파티캐릭터위치에 맞게 설정하기
   예를들어서 파티캐릭터들이 베타에있으면`n   차원설정도 베타로설정해라.

[차원설정이 안맞으면 프로그램 오류]`n
[파티캘은 점검시에도 모두 재접속해줍니다]

)
return
텔레그램메시지사용법:
Gui, Submit, Nohide
GuiControl, , jTitle, %jTitle%
TMessage := "[ Helancia_Log ]>>" . jTitle "<< : [테스트성공] 테스트 메세지 발송."
텔레그램메시지보내기(TMessage)
MsgBox,576, 원격파티,
(
	<텔레그램 사용법>`n`n
해당 시스템은 광피, 혹은 거래방해 등 여러 방해들이나 체력상승 상황을 확인 할 수 있는 유용한 텔레그램 알리미이다. `n
1. 텔레그램 HelanciaBot을 친추해라.`n
2. 친추한 Bot이 번호를 줄거다 예:) 12345678910. 준 번호를 가지고 입력해라.`n
3. 입력을 정확히 했으면 외부에 나와있을때도 텍스트알림으로 상황들을 알 수 있다. 오류, 광피 등 피해를 입을 경우 해당 알람이 간다.`n

Bot 가상 번호를 지우면 알람이 안간다. `n
가상번호를 입력 후 해당 사용법을 누를 경우 테스트메세지가 발송된다.
그걸로 테스트완료가 뜬다면 적용 완료다.

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
GuiControl, , Gui_MagicCheck3, 0
GuiControl, , Gui_MagicCheck4, 0
GuiControl, , Gui_MagicCheck5, 0
GuiControl, , Gui_MagicCheck6, 0
GuiControl, , Gui_MagicCheck7, 0
GuiControl, , Gui_MagicCheck8, 0
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
GuiControl, , Gui_WeaponCheck56, 1
GuiControl, , Gui_MagicCheck3, 1
GuiControl, , Gui_MagicCheck4, 1
GuiControl, , Gui_MagicCheck5, 1
GuiControl, , Gui_MagicCheck6, 1
GuiControl, , Gui_MagicCheck7, 1
GuiControl, , Gui_MagicCheck8, 1
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
return
}
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
BGR:=DllCall("GetSysColor"
,Int,d_element)+0x1000000
SetFormat,Integer,%A_FI%
StringMid,R,BGR,8,2
StringMid,G,BGR,6,2
StringMid,B,BGR,4,2
RGB := R G B
StringUpper,RGB,RGB
Return RGB
}

PixelColor(pc_x, pc_y, pc_wID)
{
	If pc_wID
	{
		pc_hDC := DllCall("GetDC", "UInt", pc_wID)
		WinGetPos, , , pc_w, pc_h, ahk_id %pc_wID%
		pc_hCDC := CreateCompatibleDC(pc_hDC)
		pc_hBmp := CreateCompatibleBitmap(pc_hDC, pc_w, pc_h)
		pc_hObj := SelectObject(pc_hCDC, pc_hBmp)

		pc_hmCDC := CreateCompatibleDC(pc_hDC)
		pc_hmBmp := CreateCompatibleBitmap(pc_hDC, 1, 1)
		pc_hmObj := SelectObject(pc_hmCDC, pc_hmBmp)

		DllCall("PrintWindow", "UInt", pc_wID, "UInt", pc_hCDC, "UInt", 0)
		DllCall("BitBlt" , "UInt", pc_hmCDC, "Int", 0, "Int", 0, "Int", 1, "Int", 1, "UInt", pc_hCDC, "Int", pc_x, "Int", pc_y, "UInt", 0xCC0020)
		pc_fmtI := A_FormatInteger
		SetFormat, Integer, Hex
		DllCall("GetBitmapBits", "UInt", pc_hmBmp, "UInt", VarSetCapacity(pc_bits, 4, 0), "UInt", &pc_bits)
		pc_c := NumGet(pc_bits, 0)
		SetFormat, Integer, %pc_fmtI%

		DeleteObject(pc_hBmp), DeleteObject(pc_hmBmp)
		DeleteDC(pc_hCDC), DeleteDC(pc_hmCDC)
		DllCall("ReleaseDC", "UInt", pc_wID, "UInt", pc_hDC)
		Return pc_c
	}
}


CreateCompatibleDC(hdc=0) {
	return DllCall("CreateCompatibleDC", "UInt", hdc)
}

CreateCompatibleBitmap(hdc, w, h) {
	return DllCall("CreateCompatibleBitmap", UInt, hdc, Int, w, Int, w)
}

SelectObject(hdc, hgdiobj) {
	return DllCall("SelectObject", "UInt", hdc, "UInt", hgdiobj)
}

DeleteObject(hObject) {
   Return, DllCall("DeleteObject", "UInt", hObject)
}

DeleteDC(hdc) {
	Return, DllCall("DeleteDC", "UInt", hdc)
}

일랜시아창크기구하기:
{
	IfWinNotActive, %TargetTitle%
	{
		WinActivate,%TargetTitle%
		sleep,1000
	}
	WinGetPos, OutX, OutY, OutWidth, OutHeight, A
	Multiplyer := round(OutWidth/100,0)*100 / 800
	Multiplyer := round(Multiplyer,2)
	if Multiplyer < 1
	{
		Multiplyer := 0
		MSGBOX, 해상도 구하기 실패
		return
	}
	SB_SetText("해상도:" OutWidth " * " OutHeight , 2)
	GuiControl,, Multiplyer, %Multiplyer%
	return
}
CheatEngine_GameSpeedTo(배속)
		{
			/*
			MRM 님의 블로그에 공유해주신 스피드핵 CT 입니다.
				define(speedhack,58FF80)
				fullaccess(speedhack,200)
				define(time,58FF9A)
				define(ChangeSpeed,0058FFE0)
				registersymbol(ChangeSpeed)

				0040FB07:
				jmp speedhack

				speedhack:
				mov edi,[ChangeSpeed]
				add [time],edi
				add eax,[time]
				mov [esi+6C],eax
				jmp 0040FB49

				time:
				dd 0 0

				ChangeSpeed:
				dd #0

			0040FB07 를 E9 74 04 18 00 로 변경해야함
			만약 0040FB07 의 값이 402945257 이 아니라면
			0040FB07의 값을 바꿔야함
			그리고 배속함수를 써줘야함
			시간 0058FF9A 32bytes
			배속 0058FFE0 4bytes
			배속 0 추천이동속도 180
			배속 40 추천이동속도 1650
			유져가 배속을 변경했거나 혹은 추천이동속도가 게임내에서 자동으로 변경되었다면,
			해당 변경사항을 게임에 적용해야함
			현재 게임 속도를 읽어서 0058FFE0 가 설정된 게임 배속이 아니라면 다시 변경하고
			이동속도도 동일
			예제결과물 )
			if ( mem.read(0x0040FB07,"Uint", aOffsets*) != 402945257 ) ; 0x180474E9
			{
			mem.write(0x0040FB07,0xE9,"Char", aOffsets*)
			mem.write(0x0040FB08,0x74,"Char", aOffsets*)
			mem.write(0x0040FB09,0x04,"Char", aOffsets*)
			mem.write(0x0040FB0A,0x18,"Char", aOffsets*)
			mem.write(0x0040FB0B,0x00,"Char", aOffsets*)
			WriteExecutableMemory("게임내시간제어")
			CheatEngine_GameSpeedTo(게임배속)
			}
			*/

			if (jelan.read(0x0058FFE0,"UInt", aOffsets*) != 배속)
			{
				jelan.write(0x0058FFE0,배속,"UInt", aOffsets*)
			}
		}


	IsNumber(value) {
    return RegExMatch(value, "^\d+$") > 0
	}

OID리셋:
{
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
GuiControl, , Gui_KOFF, 1
}
return

Check_Chat()
{
Chat := jelan.read(0x0058DAD4, "UInt", 0x1AC)
}

RandomRadio() {
    RadioList := ["알파차원", "베타차원", "감마차원"]
    Random, RandIndex, 1, 3
    SelectedRadio := RadioList[RandIndex]
    GuiControl,, %SelectedRadio%, 1
}

px(color) {
    static hdc, hbm, obm, pBits
    if !hdc {
        ; Device Context 및 Bitmap 생성
        hdc := DllCall("CreateCompatibleDC", "ptr", 0, "ptr")
        VarSetCapacity(bi, 40, 0)
        NumPut(40, bi, 0, "uint")              ; BITMAPINFOHEADER 크기
        NumPut(A_ScreenWidth, bi, 4, "uint")   ; 너비
        NumPut(-A_ScreenHeight, bi, 8, "int")  ; 높이 (위에서 아래로)
        NumPut(1, bi, 12, "ushort")            ; 평면 수
        NumPut(16, bi, 14, "ushort")           ; 비트 카운트 (16비트 색상)
        hbm := DllCall("CreateDIBSection", "ptr", hdc, "ptr", &bi, "uint", 0, "ptr*", pBits:=0, "ptr", 0, "uint", 0, "ptr")
        obm := DllCall("SelectObject", "ptr", hdc, "ptr", hbm, "ptr")
    }

    ; Retrieve the device context for the screen.
    static sdc := DllCall("GetDC", "ptr", 0, "ptr")

    ; Copies a portion of the screen to a new device context.
    DllCall("gdi32\BitBlt"
        , "ptr", hdc, "int", 0, "int", 0, "int", A_ScreenWidth, "int", A_ScreenHeight
        , "ptr", sdc, "int", 0, "int", 0, "uint", 0x00CC0020 | 0x40000000) ; SRCCOPY | CAPTUREBLT

    static bin := 0
    if !bin {
        ; C source code - https://godbolt.org/z/oYx39nr5s
        code := (A_PtrSize == 4)
        ? "VYnli1UIi0UMi00QjQSCOcJzDTkKdQSJ0OsFg8IE6+9dww=="
        : "idJIjQSRSDnBcw9EOQF1BInI6wZIg8EE6+zD"
        padding := (code ~= "==$") ? 2 : (code ~= "=$") ? 1 : 0
        size := 3 * (StrLen(code) / 4) - padding
        bin := DllCall("GlobalAlloc", "uint", 0, "uptr", size, "ptr")
        DllCall("VirtualProtect", "ptr", bin, "ptr", size, "uint", 0x40, "uint*", old:=0)
        DllCall("crypt32\CryptStringToBinary", "str", code, "uint", 0, "uint", 0x1, "ptr", bin, "uint*", size, "ptr", 0, "ptr", 0)
    }

    ; Pass the width * height, but the size is returned due to C interpreting Scan0 as an integer pointer.
    ; For 16-bit color, use 2 bytes per pixel.
    byte := DllCall(bin, "ptr", pBits, "uint", A_ScreenWidth * A_ScreenHeight * 2, "uint", color, "int")
    if (byte == pBits + A_ScreenWidth * A_ScreenHeight * 2)
        throw Exception("pixel not found")
    PX := mod((byte - pBits) / 2, A_ScreenWidth)
    PY := ((byte - pBits) / 2) // A_ScreenWidth
    MsgBox, Pixel found at: X=%PX% Y=%PY%
    return
}
return

OID저장:
{
SetFormat, integer, H
Get_Location()
IfInString,Location,[알파차원]
{
GuiControl,, A리노아, %A리노아%
GuiControl,, A동파, %A동파%
GuiControl,, A서파, %A서파%
GuiControl,, A길잃파, %A길잃파%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA리노아, %A리노아%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA동파, %A동파%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA서파, %A서파%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SA길잃파, %A길잃파%
}
IfInString,Location,[베타차원]
{
GuiControl,, B리노아, %B리노아%
GuiControl,, B동파, %B동파%
GuiControl,, B서파, %B서파%
GuiControl,, B길잃파, %B길잃파%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB리노아, %B리노아%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB동파, %B동파%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB서파, %B서파%
RegWrite, REG_DWORD, HKEY_CURRENT_USER, Software\Nexon\MRMChezam, SB길잃파, %B길잃파%
}
IfInString,Location,[감마차원]
{
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
}
SetFormat, integer, D
return

OID읽기:
{
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
return
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
머미캐릭()
{
jelan.write(0x0047B3EC, 0x02, "Char", aOffsets*)
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
무바활성화() ;//////////무바활성화
{
	jelan.write(0x004CBE8D, 0xE9, "char", aOffsets*)
	jelan.write(0x004CBE8E, 0xAE, "char", aOffsets*)
	jelan.write(0x004CBE8F, 0xF8, "char", aOffsets*)
	jelan.write(0x004CBE90, 0x12, "char", aOffsets*)
	jelan.write(0x004CBE91, 0x00, "char", aOffsets*)
	jelan.write(0x004CBE92, 0xC3, "char", aOffsets*)
}

무바비활성화() ;//////////무바비활성화
{
	jelan.write(0x004CBE8D, 0x89, "char", aOffsets*)
	jelan.write(0x004CBE8E, 0x85, "char", aOffsets*)
	jelan.write(0x004CBE8F, 0xA4, "char", aOffsets*)
	jelan.write(0x004CBE90, 0x01, "char", aOffsets*)
	jelan.write(0x004CBE91, 0x00, "char", aOffsets*)
	jelan.write(0x004CBE92, 0x00, "char", aOffsets*)
}


포남몬스터서치()
{
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
if(MagicAbility3 < MLimit and MagicAbility4 < MLimit and MagicAbility5 < MLimit and MagicAbility6 < MLimit and MagicAbility7 < MLimit and MagicAbility8 < MLimit)
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
if(MagicAbility3 >= MLimit or MagicAbility4 >= MLimit or MagicAbility5 >= MLimit or MagicAbility6 >= MLimit or MagicAbility7 >= MLimit or MagicAbility8 >= MLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_2Muba = 1)
{
if(MagicAbility3 < MLimit and MagicAbility4 < MLimit and MagicAbility5 < MLimit and MagicAbility6 < MLimit and MagicAbility7 < MLimit and MagicAbility8 < MLimit)
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
if(MagicAbility3 >= MLimit or MagicAbility4 >= MLimit or MagicAbility5 >= MLimit or MagicAbility6 >= MLimit or MagicAbility7 >= MLimit or MagicAbility8 >= MLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_3Muba = 1)
{
if(MagicAbility3 < MLimit and MagicAbility4 < MLimit and MagicAbility5 < MLimit and MagicAbility6 < MLimit and MagicAbility7 < MLimit and MagicAbility8 < MLimit)
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
if(MagicAbility3 >= MLimit or MagicAbility4 >= MLimit or MagicAbility5 >= MLimit or MagicAbility6 >= MLimit or MagicAbility7 >= MLimit or MagicAbility8 >= MLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_2ButMuba = 1)
{
if(MagicAbility3 < MLimit and MagicAbility4 < MLimit and MagicAbility5 < MLimit and MagicAbility6 < MLimit and MagicAbility7 < MLimit and MagicAbility8 < MLimit)
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
if(MagicAbility3 >= MLimit or MagicAbility4 >= MLimit or MagicAbility5 >= MLimit or MagicAbility6 >= MLimit or MagicAbility7 >= MLimit or MagicAbility8 >= MLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_3ButMuba = 1)
{
if(MagicAbility3 < MLimit and MagicAbility4 < MLimit and MagicAbility5 < MLimit and MagicAbility6 < MLimit and MagicAbility7 < MLimit and MagicAbility8 < MLimit)
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
if(MagicAbility3 >= MLimit or MagicAbility4 >= MLimit or MagicAbility5 >= MLimit or MagicAbility6 >= MLimit or MagicAbility7 >= MLimit or MagicAbility8 >= MLimit)
{
PixelSearch, MobX, MobY, 0, 0, 775, 460, 0x4A044A, 10, *fast
}
}
if(Gui_4ButMuba = 1)
{
if(MagicAbility3 < MLimit and MagicAbility4 < MLimit and MagicAbility5 < MLimit and MagicAbility6 < MLimit and MagicAbility7 < MLimit and MagicAbility8 < MLimit)
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
if(MagicAbility3 >= MLimit or MagicAbility4 >= MLimit or MagicAbility5 >= MLimit or MagicAbility6 >= MLimit or MagicAbility7 >= MLimit or MagicAbility8 >= MLimit)
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
if (MobNumber <= 10) {
    SplashImage, %MobNumber%:, b X%SplashX% Y%SplashY% W80 H80 CW000000
    MobNumber += 1
}
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
Step = 25
}
}
return

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
if (weapon == newWeapon)
{
RecentWeapons.RemoveAt(1)
; 현재 무기가 newWeapon과 같으면 제거
}
if (RecentWeapons.MaxIndex() > 3)
{
RecentWeapons.RemoveAt(4)
}
}
CheckTrackedWeapons()
{
global RecentWeapons
return RecentWeapons.MaxIndex()
}

		GetLVRowByResult(result)
		{
			loop % LV_GetCount()
			{
				LV_GetText(currentResult, A_Index, 10) ; Assuming 'result' is in the 10th column
				if (currentResult == result)
					return A_Index
			}
			return -1 ; Return -1 if not found
		}

		GetLVRowByOID(OID)
		{
			loop % LV_GetCount()
			{
				LV_GetText(currentOID, A_Index, 6) ; Assuming 'result' is in the 10th column
				if (currentOID == OID)
					return A_Index
			}
			return -1 ; Return -1 if not found
		}

;메모리서치

목표리스트추가메뉴:
{
if (rn=0)
return
MouseGetPos, musX, musY
Menu, DMenu, Show, %musX%,%musY%
return
}

플레이어리스트실행:
{
gui,listview,플레이어리스트
RN:=LV_GetNext(0)
if (rn=0)
return
Row := A_EventInfo
;LV_Add("", "플레이어", 차원, 맵이름, 맵번호, find_name, find_object_id, find_x, find_y, find_z)
LV_GetText(C1,row,1)
LV_GetText(C2,row,2)
LV_GetText(C3,row,3)
LV_GetText(C4,row,4)
LV_GetText(C5,row,5)
LV_GetText(C6,row,6)
LV_GetText(C7,row,7)
LV_GetText(C8,row,8)
LV_GetText(C9,row,9)
if (A_GuiEvent = "DoubleClick")
{

}
if (A_GuiEvent = "DoubleClick")
{
gui,listview,플레이어리스트
}
if A_GuiEvent = click
{
gui,listview,플레이어리스트
goto,목표리스트추가메뉴
}
if A_GuiEvent = Rightclick
{
gui,listview,플레이어리스트
goto,목표리스트추가메뉴
}
return
}

아이템리스트실행:
{
gui,listview,아이템리스트
RN:=LV_GetNext(0)
if (rn=0)
return
Row := A_EventInfo
;LV_Add("", "플레이어", 차원, 맵이름, 맵번호, find_name, find_object_id, find_x, find_y, find_z)
LV_GetText(C1,row,1)
LV_GetText(C2,row,2)
LV_GetText(C3,row,3)
LV_GetText(C4,row,4)
LV_GetText(C5,row,5)
LV_GetText(C6,row,6)
LV_GetText(C7,row,7)
LV_GetText(C8,row,8)
LV_GetText(C9,row,9)
if (A_GuiEvent = "DoubleClick")
{
gui,listview,아이템리스트
}
if A_GuiEvent = click
{
gui,listview,아이템리스트
goto,목표리스트추가메뉴
}
if A_GuiEvent = Rightclick
{
gui,listview,아이템리스트
goto,목표리스트추가메뉴
}
return
}

몬스터리스트실행:
{
gui,listview,몬스터리스트
RN:=LV_GetNext(0)
if (rn=0)
return
Row := A_EventInfo
;LV_Add("", "플레이어", 차원, 맵이름, 맵번호, find_name, find_object_id, find_x, find_y, find_z)
LV_GetText(C1,row,1)
LV_GetText(C2,row,2)
LV_GetText(C3,row,3)
LV_GetText(C4,row,4)
LV_GetText(C5,row,5)
LV_GetText(C6,row,6)
LV_GetText(C7,row,7)
LV_GetText(C8,row,8)
LV_GetText(C9,row,9)
if (A_GuiEvent = "DoubleClick")
{
gui,listview,아이템리스트
}
if A_GuiEvent = click
{
gui,listview,몬스터리스트
goto,목표리스트추가메뉴
}
if A_GuiEvent = Rightclick
{
gui,listview,몬스터리스트
goto,목표리스트추가메뉴
}
return
}

메모리검색_몬스터:
;{
; Constants
AddressToCheck := 0x005420AC
ListGUI := "몬스터리스트"
gui,submit,nohide
; Initialize ListView
gui, listview, %ListGUI%

; Read memory and populate MonsterList
startAddress := 0x005907D4
endAddress := 0x00590800
while (startAddress <= endAddress)
{
    data := jelan.read(startAddress, "UInt", aOffsets*)
    if (!IsDataInList(data, MonsterList))
        MonsterList.Push(data)
    startAddress += 4
}

좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
좌표Z := jelan.read(0x0058DAD4, "UInt", 0x18)

for index, result in MonsterList
{
    resultHex := Format("0x{:08X}", result)
    addr := jelan.read(resultHex, "UInt", aOffsets*)
	find_object_id := jelan.read(result + 0x5E, "UInt",aOffsets*)
	find_object_id := Format("0x{:08X}", find_object_id)
	LV_Row := GetLVRowByResult(result)
	gui, listview, %ListGUI%
	LV_GetText(분류,LV_Row,1)
	if (addr != AddressToCheck)
    {
        MonsterList.RemoveAt(index)
		gui, listview, %ListGUI%
		LV_Delete(LV_Row)
        continue
    }
    find_x := jelan.read(result + 0x0C, "UInt", aOffsets*)
	find_y := jelan.read(result + 0x10, "UInt", aOffsets*)
	find_z := jelan.read(result + 0x14, "UInt", aOffsets*)
	distanceXYZ := Abs(find_x - 좌표X) + Abs(find_y - 좌표Y) + Abs(find_z - 좌표Z) * 20
	find_name := jelan.readString(jelan.read(result + 0x62, "UInt", aOffsets*), 20, "UTF-16",aOffsets*)
	findMID := jelan.read(result + 0x82, "UInt", aOffsets*)

	if (LV_Row > 0)
	{
		gui, listview, %ListGUI%
		LV_Modify(LV_Row,"", kind, 차원, 맵이름, 맵번호, find_name, find_object_id, find_x, find_y, find_z, , , distanceXYZ ,findMID)
	}

Gui, listview, 몬스터리스트
i := 1
loop % LV_GetCount()
{
	LV_GetText(몬스터분류,i,1)
	LV_GetText(몬스터이름,i,5)
	LV_GetText(몬스터위치X,i,7)
	LV_GetText(몬스터위치Y,i,8)
	LV_GetText(몬스터위치Z,i,9)
	LV_GetText(몬스터주소,i,10)
	좌표X := jelan.read(0x0058DAD4, "UInt", 0x10)
	좌표Y := jelan.read(0x0058DAD4, "UInt", 0x14)
	거리X := ABS(좌표X-몬스터위치X)
	거리Y := ABS(좌표Y-몬스터위치Y)
	거리 := 거리X + 거리Y
	addr := jelan.read(몬스터주소, "UInt", aOffsets*)
	find_name := jelan.readString(jelan.read(몬스터주소 + 0x62, "UInt", aOffsets*), 20, "UTF-16",aOffsets*)
	if ((addr != AddressToCheck) || ( find_name != 몬스터이름) || (몬스터분류 != "몬스터"))
    {
		LV_Delete(i)
		continue
	}
	LV_Modify(i,"Col12",거리)
	i++
}
}
return
;}

몬스터_선택:
;{

; ListView에서 Col12 가 가장 낮은 값을 찾고, Col5가 WantedList에 포함되며, Col6가 BlackList에 포함되지 않는 항목을 선택합니다.
gui, listview, 몬스터리스트
LVCount := LV_GetCount()
LVSelect := LV_GetNext(0)
if ((LVSelect != 0) || !(LVCount = 0))
{
	LV_GetText(col5Value,LVSelect,5)
	if (!IsDataInList(col5Value, WantedMonsters) && WantedMonsterlength >= 1)
	{
		gui, listview, 몬스터리스트
		LV_Modify(0,"-Select")
	}
}

if (LVSelect != 0)
	return

lowestCol12Value := 999999 ; 초기 높은 값으로 설정
selectedRow := 0 ; 선택할 행 초기화
/*
	WantedItems
	WantedMonsters
	BlackList
	WantedItemlength := WantedItems.MaxIndex()
	WantedMonsterlength := WantedMonsters.MaxIndex()
	BlackListlength := BlackList.MaxIndex()
blackList := ["값1", "값2", "값3"] ; 블랙리스트 값들의 배열
wantedList := ["원하는값1", "원하는값2", "원하는값3"] ; 원하는 값들의 배열
*/

; ListView의 모든 행을 검색합니다
 ; ListView의 항목 수를 가져옵니다

WantedMonsterLength := 0 ; 기본값을 0으로 설정
if WantedMonsters.MaxIndex() ; 배열이 비어있지 않은 경우
    WantedMonsterLength := WantedMonsters.MaxIndex()

DisWantedMonsterLength := 0 ; 기본값을 0으로 설정
if DisWantedMonsters.MaxIndex() ; 배열이 비어있지 않은 경우
    DisWantedMonsterLength := DisWantedMonsters.MaxIndex()


Loop, %LVCount%
{
    thisRow := A_Index
    LV_GetText(col12Value, thisRow, 12) ; 현재 행의 Col12 값을 가져옵니다
    LV_GetText(col5Value, thisRow, 5) ; 현재 행의 Col5 값을 가져옵니다
    LV_GetText(col6Value, thisRow, 6) ; 현재 행의 Col6 값을 가져옵니다
	;SB_SetText("비교중" A_Index " " WantedMonsterlength, 5)
	SB_SETTEXT(WantedMonsterlength "/" DisWantedMonsterLength,5)
    ; Col12 값이 현재 가장 낮은 값보다 낮고, Col5 값이 WantedList에 포함되고, Col6 값이 BlackList에 없는 경우
    if ((col12Value < lowestCol12Value && !IsDataInList(col6Value, BlackList)) && ((IsDataInList(col5Value, WantedMonsters) && WantedMonsterLength >= 1) || WantedMonsterLength < 1 ) && ((!IsDataInList(col5Value, DisWantedMonsters) && DisWantedMonsterLength>=1) || DisWantedMonsterLength == 0))
	{
		lowestCol12Value := col12Value
		selectedRow := thisRow
    }
}

; 가장 낮은 Col12 값을 가지고 WantedList에 포함되며 BlackList에 없는 행을 선택합니다
if (selectedRow > 0)
{
    LV_Modify(selectedRow, "Select") ; 해당 행을 선택합니다
}
guicontrol,,몬스터수,몬스터: %LVCount% 마리
return
;}


RandomMummy2()
{
    Global SelectedMummy2
    MummyList2 := ["왼쪽", "중앙", "오른쪽"]
    Random, RandIndex, 1, 3
    SelectedMummy2 := MummyList2[RandIndex]
}

CharMoveMummy() ;머미이동 함수
{
    if(머미맵선택 = 0)
{
if(MapNumber = 1)
{
RunDirect = 0
}
if(MapNumber = 2)
{
좌표입력(31,75,0)
RunMemory("좌표이동")
}
if(MapNumber = 3)
{
좌표입력(30,66,0)
RunMemory("좌표이동")
}
if(MapNumber = 4)
{
좌표입력(35,67,0)
RunMemory("좌표이동")
}
if(MapNumber = 5)
{
좌표입력(41,67,0)
RunMemory("좌표이동")
}
if(MapNumber = 6)
{
좌표입력(48,67,0)
RunMemory("좌표이동")
}
if(MapNumber = 7)
{
좌표입력(42,77,0)
RunMemory("좌표이동")
}
if(MapNumber = 8)
{
좌표입력(37,66,0)
RunMemory("좌표이동")
}
if(MapNumber = 9)
{
좌표입력(31,66,0)
RunMemory("좌표이동")
}
if(MapNumber = 10)
{
좌표입력(26,66,0)
RunMemory("좌표이동")
}
if(MapNumber = 11)
{
좌표입력(21,66,0)
RunMemory("좌표이동")
}
if(MapNumber = 12)
{
좌표입력(18,66,0)
RunMemory("좌표이동")
}
if(MapNumber = 13)
{
좌표입력(18,62,0)
RunMemory("좌표이동")
}
if(MapNumber = 14)
{
좌표입력(18,58,0)
RunMemory("좌표이동")
}
if(MapNumber = 15)
{
좌표입력(198,101,0)
RunMemory("좌표이동")
}
if(MapNumber = 16)
{
좌표입력(23,58,0)
RunMemory("좌표이동")
}
if(MapNumber = 17)
{
좌표입력(28,58,0)
RunMemory("좌표이동")
}
if(MapNumber = 18)
{
좌표입력(35,58,0)
RunMemory("좌표이동")
}
if(MapNumber = 19)
{
좌표입력(41,58,0)
RunMemory("좌표이동")
}
if (MapNumber = 20)
{
    좌표입력(47, 58, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 21)
{
    좌표입력(42, 51, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 22)
{
    좌표입력(34, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 23)
{
    좌표입력(30, 44, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 24)
{
    좌표입력(26, 47, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 25)
{
    좌표입력(21, 47, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 26)
{
    좌표입력(17, 47, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 27)
{
    좌표입력(17, 43, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 28)
{
    좌표입력(17, 39, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 29)
{
    좌표입력(20, 37, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 30)
{
    좌표입력(20,33, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 31)
{
    좌표입력(20, 29, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 32)
{
    좌표입력(24, 29, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 33)
{
    좌표입력(28, 29, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 34)
{
    좌표입력(31, 29, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 35)
{
    좌표입력(38, 30, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 36)
{
    좌표입력(47, 30, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 37)
{
    좌표입력(41, 24, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 38)
{
    좌표입력(43, 18,0)
    RunMemory("좌표이동")
}
if (MapNumber = 39)
{
    좌표입력(43, 13, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 40)
{
    좌표입력(35, 14, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 41)
{
    좌표입력(25, 14, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 42)
{
    좌표입력(14, 14, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 43)
{
    좌표입력(5, 16, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 44)
{
    좌표입력(17, 15, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 45)
{
    좌표입력(22, 15, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 46)
{
    좌표입력(29, 16, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 47)
{
    좌표입력(35, 16, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 48)
{
    좌표입력(40, 16, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 49)
{
    좌표입력(43, 22, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 50)
{
    좌표입력(42, 27, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 51)
{
    좌표입력(42, 30, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 52)
{
    좌표입력(37, 30, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 53)
{
    좌표입력(31, 32, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 54)
{
    좌표입력(24, 34, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 55)
{
    좌표입력(20, 35, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 56)
{
    좌표입력(18, 40, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 57)
{
    좌표입력(19, 44, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 58)
{
    좌표입력(20, 47, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 59)
{
    좌표입력(26, 46, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 60)
{
    좌표입력(31, 45, 0)
    RunMemory("좌표이동")
    RunDirect = 1
}
if (MapNumber = 61)
{
    좌표입력(33, 44, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 62)
{
    좌표입력(35, 43, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 63)
{
    좌표입력(38, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 64)
{
    좌표입력(41, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 65)
{
    좌표입력(45, 45, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 66)
{
    좌표입력(45, 49, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 67)
{
    좌표입력(44, 53, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 68)
{
    좌표입력(43, 57, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 69)
{
    좌표입력(41, 59, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 70)
{
    좌표입력(41, 66, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 71)
{
    좌표입력(35, 69, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 72)
{
    좌표입력(32, 69, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 73)
{
    좌표입력(28, 69, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 74)
{
    좌표입력(23, 69, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 75)
{
    좌표입력(18, 67, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 76)
{
    좌표입력(10, 68, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 77)
{
    좌표입력(7, 66, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 78)
{
    좌표입력(14, 69, 0)
    RunMemory("좌표이동")
    RunDirect = 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,10
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,10
}
Step = 3025
}

if(머미맵선택 = 1) ; 중앙
{
if(MapNumber = 1)
{
RunDirect = 0
}
if(MapNumber = 2)
{
좌표입력(30,95,0)
RunMemory("좌표이동")
}
if(MapNumber = 3)
{
좌표입력(26,88,0)
RunMemory("좌표이동")
}
if(MapNumber = 4)
{
좌표입력(22,88,0)
RunMemory("좌표이동")
}
if(MapNumber = 5)
{
좌표입력(17,88,0)
RunMemory("좌표이동")
}
if(MapNumber = 6)
{
좌표입력(11,86,0)
RunMemory("좌표이동")
}
if(MapNumber = 7)
{
좌표입력(7,88,0)
RunMemory("좌표이동")
}
if(MapNumber = 8)
{
좌표입력(7,85,0)
RunMemory("좌표이동")
}
if(MapNumber = 9)
{
좌표입력(13,88,0)
RunMemory("좌표이동")
}
if(MapNumber = 10)
{
좌표입력(18,88,0)
RunMemory("좌표이동")
}
if(MapNumber = 11)
{
좌표입력(25,88,0)
RunMemory("좌표이동")
}
if(MapNumber = 12)
{
좌표입력(34,84,0)
RunMemory("좌표이동")
}
if(MapNumber = 13)
{
좌표입력(38,87,0)
RunMemory("좌표이동")
}
if(MapNumber = 14)
{
좌표입력(43,88,0)
RunMemory("좌표이동")
}
if(MapNumber = 15)
{
좌표입력(49,88,0)
RunMemory("좌표이동")
}
if(MapNumber = 16)
{
좌표입력(44,88,0)
RunMemory("좌표이동")
}
if(MapNumber = 17)
{
좌표입력(39,85,0)
RunMemory("좌표이동")
}
if(MapNumber = 18)
{
좌표입력(34,81,0)
RunMemory("좌표이동")
}
if(MapNumber = 19)
{
좌표입력(35,76,0)
RunMemory("좌표이동")
}
if(MapNumber = 20)
{
좌표입력(35,76,0)
RunMemory("좌표이동")
}
if(MapNumber = 21)
{
좌표입력(40,75,0)
RunMemory("좌표이동")
}
if(MapNumber = 22)
{
좌표입력(44,75,0)
RunMemory("좌표이동")
}
if(MapNumber = 23)
{
좌표입력(47,73,0)
RunMemory("좌표이동")
}
if(MapNumber = 24)
{
좌표입력(51,70,0)
RunMemory("좌표이동")
}
if(MapNumber = 25)
{
좌표입력(54,70,0)
RunMemory("좌표이동")
}if(MapNumber = 26)
{
좌표입력(50,73,0)
RunMemory("좌표이동")
}if(MapNumber = 27)
{
좌표입력(48,77,0)
RunMemory("좌표이동")
}if(MapNumber = 28)
{
좌표입력(52,78,0)
RunMemory("좌표이동")
}if(MapNumber = 29)
{
좌표입력(47,78,0)
RunMemory("좌표이동")
}if(MapNumber = 30)
{
좌표입력(42,74,0)
RunMemory("좌표이동")
}if(MapNumber = 31)
{
좌표입력(37,77,0)
RunMemory("좌표이동")
}if(MapNumber = 32)
{
좌표입력(33,74,0)
RunMemory("좌표이동")
}if(MapNumber = 33)
{
좌표입력(34,69,0)
RunMemory("좌표이동")
}if(MapNumber = 35)
{
좌표입력(34,65,0)
RunMemory("좌표이동")
}if(MapNumber = 36)
{
좌표입력(38,63,0)
RunMemory("좌표이동")
}if(MapNumber = 37)
{
좌표입력(44,62,0)
RunMemory("좌표이동")
}if(MapNumber = 38)
{
좌표입력(39,63,0)
RunMemory("좌표이동")
}if(MapNumber = 39)
{
좌표입력(34,64,0)
RunMemory("좌표이동")
}if(MapNumber = 40)
{
좌표입력(30,65,0)
RunMemory("좌표이동")
}if(MapNumber = 41)
{
좌표입력(24,69,0)
RunMemory("좌표이동")
}
if(MapNumber = 42)
{
좌표입력(19,65,0)
RunMemory("좌표이동")
}
if(MapNumber = 43)
{
좌표입력(12,62,0)
RunMemory("좌표이동")
}
if(MapNumber = 44)
{
좌표입력(13,59,0)
RunMemory("좌표이동")
}
if(MapNumber = 45)
{
좌표입력(14,56,0)
RunMemory("좌표이동")
}
if(MapNumber = 46)
{
좌표입력(14,52,0)
RunMemory("좌표이동")
}
if(MapNumber = 47)
{
좌표입력(17,51,0)
RunMemory("좌표이동")
}
if (MapNumber = 48)
{
    좌표입력(19, 54, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 49)
{
    좌표입력(22, 54, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 50)
{
    좌표입력(25, 52, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 51)
{
    좌표입력(29, 52, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 52)
{
    좌표입력(32, 50, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 53)
{
    좌표입력(35, 49, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 54)
{
    좌표입력(40, 49, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 55)
{
    좌표입력(42, 47, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 56)
{
    좌표입력(45, 47, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 57)
{
    좌표입력(48, 47, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 58)
{
    좌표입력(52, 46, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 59)
{
    좌표입력(53, 42, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 60)
{
    좌표입력(52, 38, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 61)
{
    좌표입력(49, 36, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 62)
{
    좌표입력(44, 35, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 63)
{
    좌표입력(40, 35, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 64)
{
    좌표입력(37, 37, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 65)
{
    좌표입력(34, 40, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 66)
{
    좌표입력(31, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 67)
{
    좌표입력(27, 40, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 68)
{
    좌표입력(24, 40, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 69)
{
    좌표입력(20, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 70)
{
    좌표입력(16, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 71)
{
    좌표입력(12, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 72)
{
    좌표입력(7, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 73)
{
    좌표입력(11, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 74)
{
    좌표입력(19, 40, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 75)
{
    좌표입력(23, 38, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 76)
{
    좌표입력(23, 34, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 77)
{
    좌표입력(23, 30, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 78)
{
    좌표입력(19, 29, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 79)
{
    좌표입력(14, 30, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 80)
{
    좌표입력(11, 29, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 81)
{
    좌표입력(9, 29, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 82)
{
    좌표입력(9, 26, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 83)
{
    좌표입력(13, 27, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 84)
{
    좌표입력(17, 27, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 85)
{
    좌표입력(20, 28, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 86)
{
    좌표입력(24, 28, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 87)
{
    좌표입력(30, 28, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 88)
{
    좌표입력(33, 28, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 89)
{
    좌표입력(37, 28, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 90)
{
    좌표입력(40, 27, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 91)
{
    좌표입력(41, 26, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 92)
{
    좌표입력(45, 24, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 93)
{
    좌표입력(41, 25, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 94)
{
    좌표입력(36, 29, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 95)
{
    좌표입력(32, 31, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 96)
{
    좌표입력(27, 29, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 97)
{
    좌표입력(24, 30, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 98)
{
    좌표입력(24, 31, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 99)
{
    좌표입력(24, 35, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 100)
{
    좌표입력(22, 37, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 101)
{
    좌표입력(22, 40, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 102)
{
    좌표입력(25, 43, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 103)
{
    좌표입력(32, 43, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 104)
{
    좌표입력(32, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 105)
{
    좌표입력(36, 39, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 106)
{
    좌표입력(41, 35, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 107)
{
    좌표입력(46, 35, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 108)
{
    좌표입력(44, 36, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 109)
{
    좌표입력(40, 36, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 110)
{
    좌표입력(36, 38, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 111)
{
    좌표입력(34, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 112)
{
    좌표입력(30, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 113)
{
    좌표입력(25, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 114)
{
    좌표입력(19, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 115)
{
    좌표입력(17, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 116)
{
    좌표입력(17, 44, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 117)
{
    좌표입력(18, 44, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 118)
{
    좌표입력(21, 44, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 119)
{
    좌표입력(26, 44, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 120)
{
    좌표입력(31, 43, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 121)
{
    좌표입력(36, 43, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 122)
{
    좌표입력(37, 39, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 123)
{
    좌표입력(44, 39, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 124)
{
    좌표입력(49, 40, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 125)
{
    좌표입력(53, 43, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 126)
{
    좌표입력(50, 47, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 127)
{
    좌표입력(40, 46, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 128)
{
    좌표입력(XX, YY, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 129)
{
    좌표입력(40, 48, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 130)
{
    좌표입력(43, 51, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 131)
{
    좌표입력(39, 52, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 132)
{
    좌표입력(36, 54, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 133)
{
    좌표입력(33, 55, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 134)
{
    좌표입력(29, 55, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 135)
{
    좌표입력(18, 54, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 136)
{
    좌표입력(13, 55, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 137)
{
    좌표입력(15, 60, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 138)
{
    좌표입력(16, 63, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 139)
{
    좌표입력(22, 65, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 140)
{
    좌표입력(26, 66, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 141)
{
    좌표입력(28, 67, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 142)
{
    좌표입력(31, 69, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 143)
{
    좌표입력(32, 72, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 144)
{
    좌표입력(33, 77, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 145)
{
    좌표입력(31, 82, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 146)
{
    좌표입력(35, 79, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 147)
{
    좌표입력(41, 76, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 148)
{
    좌표입력(46, 73, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 149)
{
    좌표입력(41, 75, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 150)
{
    좌표입력(35, 80, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 151)
{
    좌표입력(35, 84, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 152)
{
    좌표입력(XX, YY, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 153)
{
    좌표입력(34, 87, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 154)
{
    좌표입력(31, 90, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 155)
{
    좌표입력(37, 88, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 156)
{
    좌표입력(42, 88, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 157)
{
    좌표입력(45, 88, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 158)
{
    좌표입력(48, 88, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 159)
{
    좌표입력(51, 89, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 160)
{
    좌표입력(49, 89, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 161)
{
    좌표입력(45, 90, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 162)
{
    좌표입력(40, 90, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 163)
{
    좌표입력(36, 90, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 164)
{
    좌표입력(32, 91, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 165)
{
    좌표입력(28, 91, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 166)
{
    좌표입력(23, 89, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 167)
{
    좌표입력(19, 90, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 168)
{
    좌표입력(15, 88, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 169)
{
    좌표입력(11, 89, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 170)
{
    좌표입력(8, 87, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 171)
{
    좌표입력(5, 86, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 172)
{
    좌표입력(10, 89, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 173)
{
    좌표입력(16, 90, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 174)
{
    좌표입력(24, 90, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 175)
{
    좌표입력(25, 90, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 176)
{
    좌표입력(29, 91, 0)
    RunMemory("좌표이동")
    RunDirect = 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,10
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,10
}
Step = 3025
}

if(머미맵선택 = 2)
{
if (MapNumber = 1)
{
    RunDirect = 0
}
if (MapNumber = 2)
{
    좌표입력(30, 76, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 3)
{
    좌표입력(29, 69, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 4)
{
    좌표입력(26, 70, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 5)
{
    좌표입력(23, 71, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 6)
{
    좌표입력(21, 71, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 7)
{
    좌표입력(17, 71, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 8)
{
    좌표입력(13, 71, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 9)
{
    좌표입력(10, 71, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 10)
{
    좌표입력(11, 62, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 11)
{
    좌표입력(15, 66, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 12)
{
    좌표입력(17, 68, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 13)
{
    좌표입력(20, 68, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 14)
{
    좌표입력(23, 68, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 15)
{
    좌표입력(26, 68, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 16)
{
    좌표입력(29, 68, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 17)
{
    좌표입력(33, 68, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 18)
{
    좌표입력(37, 68, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 19)
{
    좌표입력(40, 68, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 20)
{
    좌표입력(44, 68, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 21)
{
    좌표입력(47, 68, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 22)
{
    좌표입력(50, 68, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 23)
{
    좌표입력(48, 66, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 24)
{
    좌표입력(46, 64, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 25)
{
    좌표입력(42, 63, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 26)
{
    좌표입력(39, 66, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 27)
{
    좌표입력(37, 67, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 28)
{
    좌표입력(33, 67, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 29)
{
    좌표입력(30, 67, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 30)
{
    좌표입력(28, 67, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 31)
{
    좌표입력(24, 65, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 32)
{
    좌표입력(24, 62, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 33)
{
    좌표입력(18, 53, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 34)
{
    좌표입력(16, 51, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 35)
{
    좌표입력(12, 48, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 36)
{
    좌표입력(9, 47, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 37)
{
    좌표입력(6, 47, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 38)
{
    좌표입력(10, 46, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 39)
{
    좌표입력(13, 48, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 40)
{
    좌표입력(16, 49, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 41)
{
    좌표입력(19, 52, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 42)
{
    좌표입력(22, 55, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 43)
{
    좌표입력(25, 56, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 44)
{
    좌표입력(30, 56, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 45)
{
    좌표입력(34, 56, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 46)
{
    좌표입력(37, 54, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 47)
{
    좌표입력(41, 50, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 48)
{
    좌표입력(44, 47, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 49)
{
    좌표입력(47, 45, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 50)
{
    좌표입력(51, 46, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 51)
{
    좌표입력(48, 41, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 52)
{
    좌표입력(44, 40, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 53)
{
    좌표입력(41, 40, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 54)
{
    좌표입력(37, 39, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 55)
{
    좌표입력(33, 37, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 56)
{
    좌표입력(30, 39, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 57)
{
    좌표입력(27, 39, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 58)
{
    좌표입력(24, 39, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 59)
{
    좌표입력(21, 39, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 60)
{
    좌표입력(18, 38, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 61)
{
    좌표입력(13, 34, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 62)
{
    좌표입력(10, 34, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 63)
{
    좌표입력(5, 34, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 64)
{
    좌표입력(8, 30, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 65)
{
    좌표입력(12, 26, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 66)
{
    좌표입력(16, 26, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 67)
{
    좌표입력(20, 26, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 68)
{
    좌표입력(23, 26, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 69)
{
    좌표입력(28, 26, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 70)
{
    좌표입력(31, 26, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 71)
{
    좌표입력(37, 26, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 72)
{
    좌표입력(41, 26, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 73)
{
    좌표입력(45, 26, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 74)
{
    좌표입력(41, 23, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 75)
{
    좌표입력(38, 21, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 76)
{
    좌표입력(37, 17, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 77)
{
    좌표입력(39, 16, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 78)
{
    좌표입력(43, 15, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 79)
{
    좌표입력(46, 16, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 80)
{
    좌표입력(49, 16, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 81)
{
    좌표입력(47, 17, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 82)
{
    좌표입력(45, 19, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 83)
{
    좌표입력(43, 19, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 84)
{
    좌표입력(40, 15, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 85)
{
    좌표입력(33, 16, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 86)
{
    좌표입력(29, 18, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 87)
{
    좌표입력(24, 19, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 88)
{
    좌표입력(20, 17, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 89)
{
    좌표입력(16, 17, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 90)
{
    좌표입력(12, 17, 0)
    RunMemory("좌표이동")
}
if (MapNumber = 91)
{
    좌표입력(9, 17, 0)
    RunMemory("좌표이동")
    RunDirect = 1
}
if(RunDirect = 0)
{
MapNumber += 1
Sleep,10
}
if(RunDirect = 1)
{
MapNumber -= 1
Sleep,10
}
Step = 3025
}
}

ClearChromeHistory()
{
    ; Chrome의 쿠키, 캐시, 기록 파일이 있는 폴더 경로 (사용자마다 다를 수 있음)
    cookiesPath := A_LocalAppData "\Google\Chrome\User Data\Default\Cookies"
    cachePath := A_LocalAppData "\Google\Chrome\User Data\Default\Cache"
    historyPath := A_LocalAppData "\Google\Chrome\User Data\Default\History"
    webCachePath := A_LocalAppData "\Google\Chrome\User Data\Default\Web Data"

    ; 쿠키 삭제
    if FileExist(cookiesPath)
    {
        FileDelete, %cookiesPath%
    }

    ; 캐시 삭제
    if FileExist(cachePath)
    {
        FileRemoveDir, %cachePath%, 1  ; 하위 폴더까지 모두 삭제
    }

    ; 히스토리 삭제
    if FileExist(historyPath)
    {
        FileDelete, %historyPath%
    }

    ; 웹 데이터 삭제 (양식 데이터 등)
    if FileExist(webCachePath)
    {
        FileDelete, %webCachePath%
    }

    ; DNS 캐시 비우기
    RunWait, %ComSpec% /c "ipconfig /flushdns"
}

		CheatEngine_NoAttackMotion() ;게임핵: 공격모션 제거 - 빠른 공격
		{
			jelan.write(0x0047C1A9,0x6A,"char",aOffset*)
			jelan.write(0x0047C1AA,0x00,"char",aOffset*)
			jelan.write(0x0047C1AB,0x90,"char",aOffset*)
			jelan.write(0x0047C1AC,0x90,"char",aOffset*)
			jelan.write(0x0047C1AD,0x90,"char",aOffset*)
		}

        CheatEngine_NoShowRide() ; 게임핵: 탈것 안보이기 - 이동불가 체력회복용 탈것 장착상태로 이동 가능
        {
            jelan.write(0x0046035B, 0x90, "Char", aOffsets*)
            jelan.write(0x0046035C, 0x90, "Char", aOffsets*)
            jelan.write(0x0046035D, 0x90, "Char", aOffsets*)
            jelan.write(0x0046035E, 0x90, "Char", aOffsets*)
            jelan.write(0x0046035F, 0x90, "Char", aOffsets*)
            jelan.write(0x00460360, 0x90, "Char", aOffsets*)
        }
        CheatEngine_AttackAlwaysMiss() ;게임핵: 항상 Miss 하기 - 0.5배의 데미지
		{
			jelan.write(0x004cfbc5,0xb2,"char",aOffset*)
			jelan.write(0x004d05cd,0xb2,"char",aOffset*)
		}
        CheatEngine_ShowBack() ; 게임핵: 배경보기
		{
			jelan.write(0x0047A18D,0x75,"char",aOffset*)
		}
        CheatEngine_NoShowBlock() ; 게임핵: 환경이미지 제거
		{
			jelan.write(0x0047aa20,0xEB,"char",aOffset*)
		}
; Ctrl+W, Ctrl+Q, Ctrl+E, Ctrl+R 중 하나를 랜덤으로 보내는 함수
RandomSendCtrlKey() {
    Random, randKey, 1, 4  ; 1에서 4 사이의 랜덤 숫자를 생성
    If (randKey = 1) {
        Send, ^w  ; Ctrl+W
    } Else If (randKey = 2) {
        Send, ^q  ; Ctrl+Q
    } Else If (randKey = 3) {
        Send, ^e  ; Ctrl+E
    } Else If (randKey = 4) {
        Send, ^r  ; Ctrl+R
    }
}

	Check_NPCMsg_address()
	{
		SetFormat, Integer, H
		startAddress := 0x00100000
		endAddress := 0x00200000
		sleep, 500
		NPCMsg_address := jelan.processPatternScan(startAddress, endAddress, 0x3A, 0x00, 0x20, 0x00, 0xE4, 0xB9, 0x6C, 0xD0, 0x5C, 0xB8, 0x20, 0x00, 0x20, 0x00, 0x20, 0x00, 0x20)
		sleep, 500
		SetFormat, Integer, D
		return NPCMsg_address
	}

    GetPrivateWorkingSet(PID)
{
try {
        wbem := ComObjGet("winmgmts:")
        query := wbem.ExecQuery("SELECT * FROM Win32_PerfFormattedData_PerfProc_Process WHERE IDProcess=" . Pid)
        if query.Count = 0
            throw, "프로세스를 찾을 수 없습니다."

        item := query.ItemIndex(0)
        bytes := item.WorkingSetPrivate
        return bytes / 1024  ; 바이트를 KB로 변환하여 반환
    } catch e
    {
        TMessage := "[ Helancia_Log ]>>" jTitle "<<: 메모리 부족. 리로드"
        텔레그램메시지보내기(TMessage)
        gosub, RL
    }
}
