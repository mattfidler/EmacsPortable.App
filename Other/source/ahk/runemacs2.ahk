exe = 
  
Loop %0% {
  parm := %A_Index%
 exe = %exe% %parm%
}
;MsgBox ,1,, EXE %exe% 
; Run the passed emacs script
Run, %exe%,
  
  
  
  
  
  
  
  
  
  
  
  
