DetectHiddenWindows, On
Loop  %1%\*.*tf {
  PID=
  IniRead PID, %TEMP%\ep\ep-reg.ini,fonts, %A_LoopFileLongPath%
  If (PID == "ERROR"){
    
  } else {
    WinShow, ahk_pid %PID%
    WinClose, ahk_pid %PID%  
    Process, Exist, %PID%  
    if ErrorLevel {
    } else {
      IniDelete, %TEMP%\ep\ep-reg.ini, fonts, %A_LoopFileLongPath%
    }
  }
}
