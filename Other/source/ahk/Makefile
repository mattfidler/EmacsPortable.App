eps=..\..\..\App\eps\EmacsPortableServer.exe
e_ico=..\img\ico\appicon.ico
win_dir=..\..\..\App\eps

all:  ${eps}

${eps}: EmacsPortableServer.ahk
	..\..\..\App\ahk\App\AutoHotkey\Compiler\ahk2exe /in EmacsPortableServer.ahk /out ..\..\..\App\eps\EmacsPortableServer.exe /icon ..\img\ico\appicon.ico

clean:
	rm ${eps}
run: 
	${win_dir}\EmacsPortableServer.exe
