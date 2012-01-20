	!define GetSection `!insertmacro GetSectionCall`

	!macro GetSectionCall _FILE _SECTION _FUNC
		Push $0
		Push `${_FILE}`
		Push `${_SECTION}`
		GetFunctionAddress $0 `${_FUNC}`
		Push `$0`
		Call GetSection
		Pop $0
	!macroend

Function GetSection
	Exch $2
        Exch
	Exch $1
	Exch
	Exch 2
	Exch $0
	Exch 2
	Push $3
	Push $4
	Push $5
	Push $6
	Push $8
	Push $9

	System::Alloc 1024
	Pop $3
        StrCpy $4 $3

        System::Call "kernel32::GetPrivateProfileSectionA(t, i, i, t) i(r1, r4, 1024, r0) .r5"

	enumok:
        System::Call 'kernel32::lstrlenA(t) i(i r4) .r6'
	StrCmp $6 '0' enumex

	System::Call '*$4(&t1024 .r9)'

	Push $0
	Push $1
	Push $2
	Push $3
	Push $4
	Push $5
	Push $6
	Push $8
	Call $2
	Pop $9
	Pop $8
	Pop $6
	Pop $5
	Pop $4
	Pop $3
	Pop $2
	Pop $1
	Pop $0
        StrCmp $9 'StopGetSection' enumex

	IntOp $4 $4 + $6
	IntOp $4 $4 + 1
	goto enumok

	enumex:
	System::Free $3

	Pop $9
	Pop $8
	Pop $6
	Pop $5
	Pop $4
	Pop $3
	Pop $2
	Pop $1
	Pop $0
FunctionEnd