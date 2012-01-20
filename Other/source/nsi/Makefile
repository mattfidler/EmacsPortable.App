download_extras=EmacsDownload-vers.nsi EmacsDownload-pae.nsi

all: ../../../EmacsPortableApp.exe ../../../EmacsDownload.exe ../../../EmacsOptions.exe ../../../App/eps/RemoveReg.exe ../../../App/eps/set-proxy.exe ../../../EmacsInstall.exe ../../../EmacsDos.exe ../../../EmacsDebug.exe ../../../Emacs-Q.exe

../../../App/eps/set-proxy.exe: SetProxy.nsi
	makensis SetProxy.nsi

../../../App/eps/RemoveReg.exe: EmacsRemoveRegistry.nsi
	makensis EmacsRemoveRegistry.nsi

../../../EmacsPortableApp.exe: EmacsPortableApp.nsi
	makensis EmacsPortableApp.nsi

../../../EmacsDownload.exe: EmacsDownload.nsi
	makensis EmacsDownload.nsi

../../../EmacsOptions.exe: EmacsPortableOptions.nsi
	makensis EmacsPortableOptions.nsi

../../../EmacsInstall.exe: install-net.nsi
	makensis install-net.nsi

../../../EmacsDos.exe: EmacsDos.nsi
	makensis EmacsDos.nsi

../../../EmacsDebug.exe: EmacsDebug.nsi
	makensis EmacsDebug.nsi

../../../Emacs-Q.exe: Emacs-Q.nsi
	makensis Emacs-Q.nsi

EmacsDos.nsi: EmacsShortcuts.el
	cmd /c "emacs -Q --batch -l ./EmacsShortcuts.el"

Emacs-Q.nsi: EmacsShortcuts.el
	cmd /c "emacs -Q --batch -l ./EmacsShortcuts.el"

EmacsDebug.nsi: EmacsShortcuts.el
	cmd /c "emacs -Q --batch -l ./EmacsShortcuts.el"

EmacsPortableOptions.nsi: EmacsPortableOptions.org emacsproxy.nsh
	cmd /c "set ORG_FILE=""EmacsPortableOptions.org"" & emacs -Q --batch -l ./build.el --eval ""(build-nsi)"""

EmacsPortableApp.nsi: EmacsPortableApp.org emacsproxy.nsh
	cmd /c "set ORG_FILE=""EmacsPortableApp.org"" & emacs -Q --batch -l ./build.el --eval ""(build-nsi)"""

EmacsDownload.nsi: EmacsDownload.org emacsproxy.nsh ${download_extras}
	cmd /c "set ORG_FILE=""EmacsDownload.org"" & emacs -Q --batch -l ./build.el --eval ""(build-nsi)"""

emacsproxy.nsh: emacsproxy.org 
	cmd /c "set ORG_FILE=""emacsproxy.org"" & emacs -Q --batch -l ./build.el --eval ""(build-nsi)"""

EmacsDownload-vers.nsi: emacsdownload-vers.el ../../../App/mirrors.ini
	cmd /c "emacs -Q --batch -l ./emacsdownload-vers.el"

EmacsDownload-pae.nsi: emacsdownload-pae.el ../../../App/mirrors.ini
	cmd /c "emacs -Q --batch -l ./emacsdownload-pae.el"

EmacsRemoveRegistry.nsi: EmacsRemoveRegistry.org emacsproxy.nsh
	cmd /c "set ORG_FILE=""EmacsRemoveRegistry.org"" & emacs -Q --batch -l ./build.el --eval ""(build-nsi)"""

SetProxy.nsi: SetProxy.org emacsproxy.nsh
	cmd /c "set ORG_FILE=""SetProxy.org"" & emacs -Q --batch -l ./build.el --eval ""(build-nsi)"""

install-net.nsi: install-net.org
	cmd /c "set ORG_FILE=""install-net.org"" & emacs -Q --batch -l ./build.el --eval ""(build-nsi)"""

clean:
	rm EmacsDos.nsi
	rm EmacsDebug.nsi
	rm Emacs-Q.nsi
	rm ../../../EmacsDos.exe
	rm ../../../EmacsDebug.exe
	rm ../../../Emacs-Q.exe
	rm ../../../EmacsInstall.exe
	rm ./install-net.nsi
	rm ./SetProxy.nsi
	rm ../../../App/eps/set-proxy.exe
	rm ../../../App/eps/RemoveReg.exe
	rm ./EmacsRemoveRegistry.nsi
	rm ./EmacsDownload*.nsi
	rm ./EmacsPortableApp.nsi
	rm ../../../EmacsDownload.exe
	rm ../../../EmacsPortableApp*.exe
	rm ../../../EmacsOptions.exe
	rm ./EmacsPortableOptions.nsi


