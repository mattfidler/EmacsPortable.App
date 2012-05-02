download_extras=EmacsDownload-vers.nsi EmacsDownload-pae.nsi EmacsDownloadShared.nsh emacsproxy.nsh EmacsCompileTools.nsh emacsdownload-gw32.nsi build.el emacsdownload-ezwin.nsi EmacsCall.nsh
EMACS=emacs
MAKENSIS=makensis

all: ../../../EmacsPortableApp.exe ../../../EmacsOptions.exe ../../../App/eps/RemoveReg.exe ../../../App/eps/set-proxy.exe ../../../EmacsInstall-Net.exe ../../../EmacsDos.exe ../../../EmacsDebug.exe ../../../Emacs-Q.exe  ../../../EmacsDownload.exe ../../../help.html

../../../App/eps/set-proxy.exe: SetProxy.nsi
	$(MAKENSIS) SetProxy.nsi

../../../EmacsPortableApp.exe: EmacsPortableApp.nsi EmacsCall.nsh
	$(MAKENSIS) EmacsPortableApp.nsi

../../../EmacsDownload.exe: EmacsDownload.nsi
	$(MAKENSIS) EmacsDownload.nsi

../../../EmacsOptions.exe: EmacsPortableOptions.nsi EmacsCall.nsh
	$(MAKENSIS) EmacsPortableOptions.nsi

../../../EmacsInstall-Net.exe: install-net.nsi EmacsDownload-pae.nsi
	$(MAKENSIS) install-net.nsi

../../../EmacsDos.exe: EmacsDos.nsi
	$(MAKENSIS) EmacsDos.nsi

../../../EmacsDebug.exe: EmacsDebug.nsi
	$(MAKENSIS) EmacsDebug.nsi

../../../Emacs-Q.exe: Emacs-Q.nsi 
	$(MAKENSIS) Emacs-Q.nsi

../../../help.html: ../../../README.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-help)"

EmacsDos.nsi: EmacsShortcuts.el
	$(EMACS) -Q --batch -l ./EmacsShortcuts.el 

Emacs-Q.nsi: EmacsShortcuts.el
	$(EMACS) -Q --batch -l ./EmacsShortcuts.el

EmacsDebug.nsi: EmacsShortcuts.el
	$(EMACS) -Q --batch -l ./EmacsShortcuts.el

EmacsPortableOptions.nsi: EmacsPortableOptions.org emacsproxy.nsh setupEmacsData.nsh
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'EmacsPortableOptions.org)"

setupEmacsData.nsh: setupEmacsData.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'setupEmacsData.org)"

EmacsPortableApp.nsi: EmacsPortableApp.org emacsproxy.nsh emacsPathIfExists.nsh setupEmacsData.nsh
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'EmacsPortableApp.org)"

emacsPathIfExists.nsh: emacsPathIfExists.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsPathIfExists.org)"

EmacsCall.nsh: EmacsCall.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsCall.org)"

EmacsDownload.nsi: EmacsDownload.org ${download_extras}
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'EmacsDownload.org)"

emacsproxy.nsh: emacsproxy.org 
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsproxy.org)"

EmacsDownload-ezwin.nsi: emacsdownload-ezw.el ../../../App/ini/mirrors.ini
	$(EMACS) -Q --batch -l ./emacsdownload-ezw.el

EmacsDownload-vers.nsi: emacsdownload-vers.el ../../../App/ini/mirrors.ini
	$(EMACS) -Q --batch -l ./emacsdownload-vers.el

EmacsDownload-pae.nsi: emacsdownload-pae.el ../../../App/ini/mirrors.ini
	$(EMACS) -Q --batch -l ./emacsdownload-pae.el

emacsdownload-gw32.nsi: emacsdownload-gw32.el ../../../App/ini/mirrors.ini
	$(EMACS) -Q --batch -l ./emacsdownload-gw32.el

SetProxy.nsi: SetProxy.org emacsproxy.nsh
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'SetProxy.org)"

install-net.nsi: install-net.org ${download_extras}
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'install-net.org)"

EmacsDownloadShared.nsh: EmacsDownloadShared.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'EmacsDownloadShared.org)"

EmacsCompileTools.nsh: emacsCompileTools.org emacsCall.nsh
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsCompileTools.org)"

clean:
	rm EmacsCompileTools.nsi
	rm EmacsDownloadShared.nsh
	rm EmacsDos.nsi
	rm EmacsDebug.nsi
	rm Emacs-Q.nsi
	rm ./install-net.nsi
	rm ./SetProxy.nsi
	rm ./EmacsDownload*.nsi
	rm ./EmacsPortableApp.nsi
	rm ./EmacsPortableOptions.nsi
	rm ../../../help.html
	rm ../../../App/eps/EmacsDos.exe
	rm ../../../App/eps/EmacsDebug.exe
	rm ../../../App/eps/Emacs-Q.exe
	rm ../../../EmacsInstall-Net.exe
	rm ../../../EmacsDownload.exe
	rm ../../../EmacsPortableApp*.exe
	rm ../../../EmacsOptions.exe
	rm ../../../App/eps/set-proxy.exe





