DetectHiddenWindows On
Loop %1%\*.*tf {
  ffile=%SYSTEMROOT%\Fonts\%A_LoopFileName%
    If (FileExist(ffile)){
    
  } else{
    PID=
      IniRead PID, %TEMP%\ep-reg.ini,fonts, %A_LoopFileLongPath%
      If (PID == "ERROR"){
      OutputVarPID=
        Title=
        Run, fontview.exe %A_LoopFileLongPath%, ,Min, OutputVarPID
        WinWait, ahk_pid %OutputVarPID%
        WinGetTitle Title, ahk_pid %OutputVarPID%
        IniWrite %Title%, %1%\fonts.ini, fonts, %A_LoopFileName%
        WinHide, ahk_pid %OutputVarPID%
        IniWrite %OutputVarPID%, %TEMP%\ep-reg.ini, fonts, %A_LoopFileLongPath%
        } else {
      Process, Exist, %PID%
        if ErrorLevel {
          } else {
          OutputVarPID=
            Run, fontview.exe %A_LoopFileLongPath%, ,Min, OutputVarPID
            WinWait, ahk_pid %OutputVarPID%
            WinGetTitle Title, ahk_pid %OutputVarPID%
            IniWrite %Title%, %1%\fonts.ini, fonts, %A_LoopFileName%
            WinHide, ahk_pid %OutputVarPID%
            IniWrite %OutputVarPID%, %TEMP%\ep-reg.ini, fonts, %A_LoopFileLongPath%
            }
    }
    
  }
  
}
