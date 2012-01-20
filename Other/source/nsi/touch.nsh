;----------------------------------------------------------------------------
; Title             : Touch File
; Short Name        : Touch
; Last Changed      : 29/Mar/2005
; Code Type         : Function
; Code Sub-Type     : Macro
;----------------------------------------------------------------------------
; Requires          : System plugin.
; Description       : Changes the file modified date to the current date.
;----------------------------------------------------------------------------
; Macro Call        : ${Touch} "[path/]file.ext"
;----------------------------------------------------------------------------
; Author            : Sam Hasler
; Author Reg. Name  : SamHasler
;----------------------------------------------------------------------------

Function Touch
  !define Touch `!insertmacro TouchCall`
  
  !macro TouchCall _FILE
    Push `${_FILE}`
    Call Touch
  !macroend
  
  Exch $0 # Get filename
  Push $1               
  Push $2
  Push $3
  
  DetailPrint "Touching $0"
  ClearErrors
  FileOpen $1 "$0" a
  IfErrors error
  
  # Big assumption: FileOpen handles are equivalent to
  #                 the handles used in system calls
  # i.e. those used by:
  # BOOL SetFileTime(
  #   HANDLE hFile,
  #   const FILETIME* lpCreationTime,
  #   const FILETIME* lpLastAccessTime,
  #   const FILETIME* lpLastWriteTime
  # );
  
  System::Call '*(&i2,&i2,&i2,&i2,&i2,&i2,&i2,&i2) i .r2'
  System::Call 'kernel32::GetSystemTimeAsFileTime(i)i(r2)'
  System::Call 'kernel32::SetFileTime(i,i,i,i) i(r1,,,r2) .r3'
  System::Free $2
  FileClose $1
  IntCmp $3 -1 error
  goto end
  
  error:
    SetErrors
    MessageBox MB_OK "Failed to touch $0"
    
  end:
    pop $3
    pop $2
    pop $1
    pop $0
    
FunctionEnd
