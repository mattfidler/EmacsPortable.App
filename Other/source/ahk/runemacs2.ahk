epenv = %TEMP%\ep-env.ini
  If (FileExist(epenv)){
  N=
  IniRead N, %TEMP%\ep-env.ini,path,n
  If (N == "ERROR"){
    ;;MsgBox "ERROR"
  } Else {
    I = 1
    While (I <= N) {
      P=
      IniRead P, %TEMP%\ep-env.ini,path,%I%
      EnvGet P2, PATH
      P := P . ";" . P2
      EnvSet PATH, %P%
      I := I + 1
    }  
  }
  
  N=
  IniRead N, %TEMP%\ep-env.ini,info,n
  If (N == "ERROR"){
    ;MsgBox "ERROR"
  } Else {
    I = 1
    While (I <= N) {
      P=
      IniRead P, %TEMP%\ep-env.ini,info,%I%
      EnvGet P2, INFOPATH
      If (P2 == "ERROR"){
        
      } Else {
         P := P . ";" . P2
      }
      EnvSet INFOPATH, %P%
      I := I + 1
    }  
  }

  N=
  IniRead N, %TEMP%\ep-env.ini,man,n
  If (N == "ERROR"){
    ;MsgBox "ERROR"
  } Else {
    I = 1
    While (I <= N) {
      P=
      IniRead P, %TEMP%\ep-env.ini,man,%I%
      EnvGet P2, MANPATH
      If (P2 == "ERROR"){
        
      } Else {
      P := P . ";" . P2
      }
      EnvSet MANPATH, %P%
      I := I + 1
    }  
  }
}
  FileDelete, %epenv%
  exe = 
  
  
  Loop %0% {
  parm := %A_Index%
  exe = %exe% %parm%
  }
    H=
  EnvGet H, HOME
  Run, %exe%,%H%,
  
  
  
