/***************************************************************************
Copyright 2007 Karl Loncarek
Copyright 2007-2008 wraithdu

USAGE:

REQUIRED - Registry plugin

insert regkeys to use separated by "||"
!define REGKEYS "HKLM\SOFTWARE\Microsoft\etc||HKCU\Second\Key"

name of app for backup keys
!define APPNAME "name of app"

skip saving reg keys to file on close
!define NOSAVE

Call InitReg
Call CleanReg
***************************************************************************/

!include Registry.nsh
!include WordFunc.nsh
!insertmacro WordFind

; **************************************************************************
; *  Function: Backup registry keys, apply portable registry keys
; **************************************************************************
Function InitReg
	StrCpy $R8 "0" ; reset variable
	StrCpy $R0 "${REGKEYS}" ; copy constant to working variable
	Call ValuesToStack ; separate values from REGKEYS to stack
	InitRegLoop:
		Pop $R9 ; obtain registry key from stack
		StrCmp $R9 "EndOfStack" InitRegApply ; do not do registry parsing, when no keys given anymore
			IntOp $R8 $R8 + 1 ; increase counter
			; --------------------------------------------------------------------------
			; Backup registry key
			; --------------------------------------------------------------------------
			${registry::KeyExists} "$R9" $R7 ; check whether registry key exists
			StrCmp $R7 "0" 0 InitRegLoop ; registry key does not exist, do not save anything
				${registry::MoveKey} "$R9" "$R9_${APPNAME}-bkup" $R7 ; Rename registry key
		Goto InitRegLoop
	InitRegApply:
	; --------------------------------------------------------------------------
	; Apply portable registry key
	; --------------------------------------------------------------------------
	IfFileExists "$EXEDIR\Data\settings\${APPNAME}_portable.reg" 0 InitRegEnd ; only apply if a registry file exists
		ExecWait 'regedit /s "$EXEDIR\Data\settings\${APPNAME}_portable.reg"' ; Restore saved registry keys
	InitRegEnd:
FunctionEnd

; **************************************************************************
; *  Function: Copy registry key (portable), restore oroginal registry key
; **************************************************************************
Function CleanReg
	StrCpy $R8 "0" ; reset variable
	StrCpy $R0 "${REGKEYS}" ; copy constant to working variable
	Call ValuesToStack ; separate values from REGKEYS to stack
	IfFileExists "$EXEDIR\Data\settings\*.*" +3
		CreateDirectory "$EXEDIR\Data\settings" ; create registry directory if it does not exist
		Goto CleanRegLoop
	IfFileExists "$EXEDIR\Data\settings\${APPNAME}_portable.reg" 0 +2
		Delete "$EXEDIR\Data\settings\${APPNAME}_portable.reg" ; delete portable registry file if it exists to write a new one
	CleanRegLoop:
		Pop $R9 ; obtain registry key from stack
		StrCmp $R9 "EndOfStack" CleanRegEnd ; do not do registry parsing, when no keys given anymore
			IntOp $R8 $R8 + 1 ; increase counter
			; --------------------------------------------------------------------------
			; Copy actual registry key to portable folder
			; --------------------------------------------------------------------------
			${registry::KeyExists} "$R9" $R7 ; check whether registry key exists
			StrCmp $R7 "0" 0 CleanRegRename ; registry key does not exist, do not save anything
				!ifndef NOSAVE
					${registry::SaveKey} "$R9" "$EXEDIR\Data\settings\${APPNAME}_portable.reg" "/G=1 /A=1" $R7 ; Backup registry key
				!endif
				; --------------------------------------------------------------------------
				; Delete actual actual registry key (with portable content)
				; --------------------------------------------------------------------------
				${registry::DeleteKey} "$R9" $R7 ; Delete registry key
				; --------------------------------------------------------------------------
				; Restore original registry key if backup exists
				; --------------------------------------------------------------------------
				CleanRegRename:
				${registry::KeyExists} "$R9_${APPNAME}-bkup" $R7
				StrCmp $R7 "0" 0 CleanRegLoop ; only rename if a backup key exists
					${registry::MoveKey} "$R9_${APPNAME}-bkup" "$R9" $R7 ; Rename backup registry key
		Goto CleanRegLoop
	CleanRegEnd:
FunctionEnd

; **************************************************************************
; *  Helper Function: Move value of constants onto stack, $R0 holds values separated by "||"
; **************************************************************************
Function ValuesToStack
	StrCpy $0 "0" ; reset counter
	; --------------------------------------------------------------------------
	; Get single parameter out of list, i.e. obtain next single registry key
	; --------------------------------------------------------------------------
	Push "EndOfStack" ; set end marker for stack
	ValuesToStackStart:
		StrCmp $R0 "" ValuesToStackEnd ; do not do registry parsing, when no keys given anymore
			IntOp $0 $0 + 1 ; increase counter
			${WordFind} "$R0" "||" "-01" $9 ; save last parameter to register
			${WordFind} "$R0" "||" "-02{*"  $R0 ; remove last part from saved value
			Push $9 ; save parameter to stack
			StrCmp $R0 $9 ValuesToStackEnd ; if values are identical (last parameter) -> no more delimiters
		Goto ValuesToStackStart
	ValuesToStackEnd:
FunctionEnd