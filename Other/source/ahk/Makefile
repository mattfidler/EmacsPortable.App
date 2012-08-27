eps=..\..\..\App\eps\EmacsPortableServer.exe
e_ico=..\img\ico\appicon.ico
win_dir=..\..\..\App\eps

all:  ${eps}

${eps}:
	cmd /c "@ahk2exe /in EmacsPortableServer.ahk /out ${win_dir}\EmacsPortableServer.exe /icon ${e_ico}"

clean:
	rm ${eps}
