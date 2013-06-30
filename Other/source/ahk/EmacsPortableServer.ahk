#SingleInstance, Force
#NoEnv

;; http://www.autohotkey.com/community/viewtopic.php?t=80397
GetModuleExeName(p_id) {
   for process in ComObjGet("winmgmts:").ExecQuery("Select * from Win32_Process where ProcessId=" p_id)
      return process.ExecutablePath
}

DetectHiddenWindows, on


EnvGet EPOTHER, EPOTHER
EnvGet TEMP, TEMP

CmdLst=
Care = 0
Loop, Read, %EPOTHER%..\App\ini\server.ini
{
  If (A_LoopReadLine == "[Commands]"){
     Care = 1
  } Else If (Care == 1){
    tmp = %A_LoopReadLine%
    NextSec := InStr(tmp,"[")
    If (NextSec == 1){
       Care = 0
    } else {
      NextSec := InStr(tmp,"=")
      If (NextSec != 0){
         NextSec := SubStr(tmp,1,NextSec-1)
         CmdLst  = %CmdLst%%NextSec%`n
      }
    }
  }
}

I = 0
PLst=
Loop, %EPOTHER%..\App\emacs-*.*,2
{
        StringReplace EmacsVer, A_LoopFileName, `emacs-`
        PLst= %PLst%emacs-%EmacsVer%.exe`n
        I += 1
        did%I% := % "___EmacsPortableDaemon_" . EmacsVer . "___"
        desc%I% := % "Emacs " . EmacsVer
        menu%I% := % "Start Emacs " . EmacsVer
        ver%I% := % EmacsVer
        args%I% := % "/VERSION=" . EmacsVer . " /START=None"
        pid%I% := 0
        hideshow%I% := 1
        NStart=0
        Loop %EPOTHER%..\Data\start\*.*,2
        {
                If (A_LoopFileName == "shared" || A_LoopFileName == "system" || A_LoopFileName == "user"){
                
                } else {
                    NStart := NStart + 1
                    PLst= %PLst%emacs-%EmacsVer%-%A_LoopFileName%.exe`n
                    I += 1
                    did%I% := % "___EmacsPortableDaemon_" . EmacsVer . "_" . A_LoopFileName . "___"
                    desc%I% := % "Emacs " . EmacsVer . " " . A_LoopFileName
                    menu%I% := % "Start Emacs " . EmacsVer . " " . A_LoopFileName
                    ver%I% := % EmacsVer
                    args%I% := % "/VERSION=" . EmacsVer . " /START=" . A_LoopFileName
                    pid%I% := 0
                    hideshow%I% := 1
                }
        }
}
MaxI := I
; Wait for at least one process to exist.
Pexist=0
While (Pexist == 0)
{
        Loop, parse, Plst, `n 
        {
              If (Pexist == 0)
              {
                 Process, exist, %A_LoopField%
                 If (Errorlevel != 0){
                    Pexist = 1
                 }
              }
        }
}
; Now that process exists, Hide any running Emacs Daemons
Pexist = 1
ChangeMenu = 1
ReqChangeMenu = 0
While (Pexist == 1) 
{ 
  Pexist = 0
  If (ChangeMenu == 1){
     FirstLine = 1
     Menu, Tray, DeleteAll
     Menu, Tray, NoStandard
  }
  LastVersion := -1
  I = 0
  Loop, parse, Plst, `n 
  {
      I += 1
      If (A_LoopField != "")
      {
        cDesc   := desc%I%
        CurVer  := ver%I%
        tit     := did%I%
        LastPID := pid%I%
        DoHide  := hideshow%I%

        If (CurVer != LastVer && ChangeMenu == 1 && FirstLine == 0){
           Menu, tray, add  ; Creates a separator line.
        }
        FirstLine = 0
        LastVer := CurVer
        Process, exist, %A_LoopField%
        If (Errorlevel != 0){
           pid%I% := Errorlevel
           If (LastPID == 0){
              ReqChangeMenu = 1
              ;;TrayTip, %cDesc%, Detected Running %cDesc%, 10, 1
           }
           Pexist = 1

           SetTitleMatchMode 3
           If (ChangeMenu == 1){
             tmp := GetModuleExeName(pid%I%)
             IniWrite %tmp%, %TEMP%\ep-pid.ini ,exec, %A_LoopField%  
             Menu, Menu%I%, add, New Frame, MenuHandler
             IfWinExist, %tit%
             {
                Menu, Menu%I%, add, Hidden Daemon, MenuHandler
             }
             Else 
             {
                Menu, Menu%I%, add, Start Psuedo-Daemon, MenuHandler
             } 
             Loop, parse, CmdLst, `n 
             {
                    If (A_LoopField != ""){
                         Menu, Menu%I%, add, %A_LoopField%, MenuHandler
                    }
             }
             ;A_LoopField = %tmp%
           }
           IfWinExist, %tit%
           {
              If (DoHide == 1)
              {
                 If (ChangeMenu == 1){
                   Menu, Menu%I%, Check, Hidden Daemon
                 }                
                 WinHide %tit%   
              } else {
                 If (ChangeMenu == 1){
                   Menu, Menu%I%, UnCheck, Hidden Daemon                  
                 }
                 WinShow %tit%
              }
           }
           If (ChangeMenu == 1){
              Menu, tray, add, %cDesc% , :Menu%I%
           }
        } else {
           pid%I% := 0
           If (LastPID != 0){
              ReqChangeMenu = 1
              ;;TrayTip, %cDesc%, Detected Stopped %cDesc%, 10, 1
           }
           If (ChangeMenu == 1){
             IniWrite 0, %TEMP%\ep-pid.ini ,exec, %A_LoopField%
             Menu, tray, add, Start %cDesc% ,MenuHandler 
           } 
        }
        If (ChangeMenu == 1){
          tmp := pid%I%
          IniWrite %tmp%, %TEMP%\ep-pid.ini ,pid, %A_LoopField%
        }
      }
  }
  If (ChangeMenu == 1){
            
    Menu, tray, add  ; Creates a separator line.
    Menu, tray, add, Options, MenuHandler
    Menu, tray, add, Refresh, MenuHandler
    Menu, tray, add, Exit, MenuHandler
  }
  ChangeMenu = 0
  If (ReqChangeMenu == 1){
     ChangeMenu = 1
     ReqChangeMenu = 0
  }
  IfExist, %TEMP%\ep-refresh-server.ini 
  {
     FileDelete %TEMP%\ep-refresh-server.ini
     Reload
  }
  IfExist, %TEMP%\ep\ep-refresh-server.ini
  { 
     FileDelete %TEMP%\ep\ep-refresh-server.ini
     Reload
  }
  Sleep 50
}
TrayTip, Remove Tray Icon, EmacsPortable.App does not detect any running instances of emacs, 10, 1
ExitApp
return

MenuHandler:
If (A_ThisMenuItem == "Hidden Daemon"){
   StringReplace J, A_ThisMenu, Menu
   DoHide := hideshow%J%
   If (DoHide == 1){
      DoHide = 0
   } else {
      DoHide = 1
   }
   hideshow%J% := DoHide
   Sleep 50
   ReqChangeMenu = 1
} Else If (A_ThisMenuItem == "Start Psuedo-Daemon"){
  StringReplace J, A_ThisMenu, Menu
  Run %EPOTHER%..\EmacsPortableApp.exe %cArgs% --eval "()", %EPOTHER%..\
} Else If (A_ThisMenuItem == "Refresh" && A_ThisMenu == "Tray"){
  IniWrite one, %TEMP%\ep-refresh-server.ini ,two, three
} Else If (A_ThisMenuItem == "Options" && A_ThisMenu == "Tray"){
  Run %EPOTHER%..\EmacsOptions.exe, %EPOTHER%..\
} Else If (A_ThisMenuItem == "Exit" && A_ThisMenu == "Tray"){
  Loop %MaxI%
  {
     hideshow%I% := 0
     tit := did%I%
     WinShow %tit%
  }
  ExitApp
} Else If (A_ThisMenu == "Tray"){
  J := 0
  Loop %MaxI%
  {
        Temp := menu%A_Index%
        If (A_ThisMenuItem == Temp){
           J := A_Index
        }
  }
  cArgs := args%J%
  Run %EPOTHER%..\EmacsPortableApp.exe %cArgs%, %EPOTHER%..\
} Else If (A_ThisMenuItem == "New Frame") {
  StringReplace J, A_ThisMenu, Menu
  cArgs := args%J%
  Run %EPOTHER%..\EmacsPortableApp.exe %cArgs%, %EPOTHER%..\
} Else If (A_ThisMenu != "Tray"){
  IniRead xArgs, %EPOTHER%..\App\ini\server.ini ,Commands, %A_ThisMenuItem%
  StringReplace J, A_ThisMenu, Menu
  cArgs := args%J%
  Run %EPOTHER%..\EmacsPortableApp.exe %cArgs% %xArgs%, %EPOTHER%..\
}
;MsgBox You selected %A_ThisMenuItem% from menu %A_ThisMenu%.
return
