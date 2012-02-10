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
      If (P == "ERROR"){
        
      } else {
        EnvGet P2, PATH
        If (P2 == "ERROR"){
          MsgBox Could not read path %I%
        } Else {
        P := P . ";" . P2
        }
        EnvSet PATH, %P%
      }
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
      If (P == "ERROR") {
        MsgBox Could not read info %I%
      } Else {
        EnvGet P2, INFOPATH
        If (P2 == "ERROR"){
          
        } Else {
        P := P . ";" . P2
        }
        EnvSet INFOPATH, %P%
      }
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
      If (P == "ERROR"){
        
      } Else {
        EnvGet P2, MANPATH
        If (P2 == "ERROR"){
          MsgBox Could not read man %I%
        } Else {
        P := P . ";" . P2
        }
        EnvSet MANPATH, %P%
      }
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
