download_extras=EmacsDownload-pae.nsi EmacsDownloadShared.nsh emacsproxy.nsh EmacsCompileTools.nsh emacsdownload-gw32.nsi build.el emacsdownload-ezwin.nsi EmacsCall.nsh
emacs_extras=../../../App/eps/ep-reg.exe emacsproxy.nsh emacsPathIfExists.nsh setupEmacsData.nsh EmacsEnv.nsh
EMACS=emacs
MAKENSIS=makensis

all: ../../../EmacsPortableApp.exe ../../../App/eps/ep.exe ../../../App/eps/epl.exe ../../../EmacsOptions.exe  ../../../EmacsDos.exe ../../../EmacsDebug.exe ../../../Emacs-Q.exe ../../../App/eps/ep-reg.exe ../../../App/eps/ep-font.exe ../../../App/eps/ver-shortcut.exe ../../../App/eps/ep-assoc.exe ../../../App/eps/ep-liberkey.exe ../../../App/eps/ep-proxy.exe ../../../App/eps/EmacsDoc.exe ../../../App/eps/download-unix.exe ../../../App/eps/ep-local.exe ../../../App/eps/ep-sendto.exe ../../../help.html

../../../EmacsPortableApp.exe: emacsportableLaunch.nsi
	$(MAKENSIS) emacsportableLaunch.nsi

emacsportableLaunch.nsi: emacsportableLaunch.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsportableLaunch.org)"

../../../App/eps/ep-sendto.exe: emacsSendTo.nsi
	$(MAKENSIS) emacsSendTo.nsi

emacsSendTo.nsi: emacsSendTo.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsSendTo.org)"

../../../App/eps/ep-local.exe: emacsLocal.nsi
	$(MAKENSIS) emacsLocal.nsi

emacsLocal.nsi: emacsLocal.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsLocal.org)"

../../../App/eps/download-unix.exe: EmacsUnix.nsi
	$(MAKENSIS) EmacsUnix.nsi


EmacsUnix.nsi: emacsUnix.org emacsdownload-gw32.nsi emacsdownload-ezwin.nsi EmacsDownloadShared.nsh EmacsCall.nsh
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsUnix.org)"

EmacsEnv.nsh: EmacsEnv.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsEnv.org)"

../../../App/eps/EmacsDoc.exe: EmacsDoc.nsi
	$(MAKENSIS) EmacsDoc.nsi

EmacsDoc.nsi: EmacsShortcuts.el
	$(EMACS) -Q --batch -l ./EmacsShortcuts.el 

../../../App/eps/ep-proxy.exe: emacsproxyplugin.nsi emacsproxy.nsh EmacsEnv.nsh
	$(MAKENSIS) emacsproxyplugin.nsi

emacsproxyplugin.nsi: emacsproxyplugin.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsproxyplugin.org)"


../../../App/eps/ep-liberkey.exe: emacsLiberKey.nsi
	$(MAKENSIS) emacsLiberKey.nsi

emacsLiberKey.nsi: emacsLiberKey.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsLiberKey.org)"

../../../App/eps/ep-assoc.exe: emacsAssoc.nsi
	$(MAKENSIS) emacsAssoc.nsi

emacsAssoc.nsi: emacsAssoc.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsAssoc.org)"

../../../App/eps/ver-shortcut.exe: emacsportablever.nsi
	$(MAKENSIS) emacsportablever.nsi

emacsportablever.nsi: emacsportablever.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsportablever.org)"

../../../App/eps/ep-font.exe: EmacsFonts.nsi
	$(MAKENSIS) EmacsFonts.nsi

EmacsFonts.nsi: EmacsFonts.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'EmacsFonts.org)"

../../../App/eps/ep-reg.exe: EmacsRegistry.nsi
	$(MAKENSIS) EmacsRegistry.nsi

EmacsRegistry.nsi: EmacsRegistry.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'EmacsRegistry.org)"

EmacsPortableDebugLaunch.nsi: EmacsPortableDebugLaunch.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'EmacsPortableDebugLaunch.org)"

../../../App/eps/epl.exe: EmacsPortableDebugLaunch.nsi EmacsPortableApp.nsi
	$(MAKENSIS) EmacsPortableDebugLaunch.nsi

../../../App/eps/ep.exe: EmacsPortableApp.nsi EmacsCall.nsh
	$(MAKENSIS) EmacsPortableApp.nsi

../../../EmacsOptions.exe: EmacsPortableOptions.nsi EmacsCall.nsh EmacsEnv.nsh EmacsDownloadShared.nsh
	$(MAKENSIS) EmacsPortableOptions.nsi

../../../EmacsInstall-Net.exe: install-net.nsi EmacsDownload-pae.nsi
	$(MAKENSIS) install-net.nsi

../../../EmacsDos.exe: EmacsDos.nsi
	$(MAKENSIS) EmacsDos.nsi

../../../EmacsDebug.exe: EmacsDebug.nsi
	$(MAKENSIS) EmacsDebug.nsi

../../../Emacs-Q.exe: Emacs-Q.nsi 
	$(MAKENSIS) Emacs-Q.nsi

../../../help.html: ../../../README.org ../../style.css
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

EmacsPortableApp.nsi: EmacsPortableApp.org $(emacs_extras)
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'EmacsPortableApp.org)"

emacsPathIfExists.nsh: emacsPathIfExists.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsPathIfExists.org)"

EmacsCall.nsh: EmacsCall.org
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsCall.org)"

emacsproxy.nsh: emacsproxy.org 
	$(EMACS) -Q --batch -l ./build.el --eval "(build-nsi 'emacsproxy.org)"

EmacsDownload-ezwin.nsi: emacsdownload-ezw.el ../../../App/ini/gw32-install.ini
	$(EMACS) -Q --batch -l ./emacsdownload-ezw.el

EmacsDownload-pae.nsi: emacsdownload-pae.el ../../../App/ini/mirrors.ini
	$(EMACS) -Q --batch -l ./emacsdownload-pae.el

emacsdownload-gw32.nsi: emacsdownload-gw32.el ../../../App/ini/gw32-install.ini
	$(EMACS) -Q --batch -l ./emacsdownload-gw32.el

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
	rm ./EmacsDownload*.nsi
	rm ./EmacsPortableApp.nsi
	rm ./EmacsPortableOptions.nsi
	rm ../../../App/eps/EmacsDos.exe
	rm ../../../App/eps/EmacsDebug.exe
	rm ../../../App/eps/Emacs-Q.exe
	rm ../../../EmacsInstall-Net.exe
	rm ../../../EmacsDownload.exe
	rm ../../../EmacsPortableApp*.exe
	rm ../../../EmacsOptions.exe





