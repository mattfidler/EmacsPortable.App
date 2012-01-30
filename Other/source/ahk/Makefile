addfont=../../../App/eps/addfont.exe
rmfont=../../../App/eps/rmfont.exe
eps=../../../App/eps/EmacsPortableServer.exe
run=../../../App/eps/run.exe
run2=../../../App/eps/run2.exe
font_ico=..\img\ico\fonts.ico
e_ico=..\img\ico\appicon.ico
win_dir=..\..\..\App\eps

all: ${addfont} ${rmfont} ${eps} ${run} ${run2}

${addfont}: addfont.ahk ../img/ico/fonts.ico
	cmd /c "@ahk2exe /in addfont.ahk /out ${win_dir}\addfont.exe /icon ${font_ico}"

${rmfont}: rmfont.ahk ../img/ico/fonts.ico
	cmd /c "@ahk2exe /in rmfont.ahk /out ${win_dir}\rmfont.exe /icon ${font_ico}"

${eps}: EmacsPortableServer.ahk ../img/ico/appicon.ico
	cmd /c "@ahk2exe /in EmacsPortableServer.ahk /out ${win_dir}\EmacsPortableServer.exe /icon ${e_ico}"

${run}: runemacs.ahk ../img/ico/appicon.ico runemacs-header.ahk
	cmd /c "@ahk2exe /in runemacs.ahk /out ${win_dir}\run.exe /icon ${e_ico}"
${run2}: runemacs2.ahk ../img/ico/appicon.ico runemacs-header.ahk
	cmd /c "@ahk2exe /in runemacs2.ahk /out ${win_dir}\run2.exe /icon ${e_ico}"

clean:
	rm ${addfont}
	rm ${rmfont}
	rm ${eps}
	rm ${run}
	rm ${run2}
