eps=../../../App/eps/EmacsPortableServer.exe
e_ico=..\img\ico\appicon.ico
win_dir=..\..\..\App\eps

all:  ${eps}

${addfont}: addfont.ahk ../img/ico/fonts.ico
	cmd /c "@ahk2exe /in addfont.ahk /out ${win_dir}\addfont.exe /icon ${font_ico}"

clean:
	rm ${eps}
