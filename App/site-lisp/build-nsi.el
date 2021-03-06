;;; build-nsi.el --- Builds NSIS installation file based on current install.
;; 
;; Filename: build-nsi.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Aug 29 11:58:51 2012 (-0500)
;; Version:
;; Last-Updated: Mon Sep 10 11:17:47 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 112
;; URL: 
;; Keywords: 
;; Compatibility:
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun build-nsi-file (file-list out-path &optional exclude-reg)
  "Builds NSIS file information"
  (let (changed-out
        out2
        lst)
    (unless (string-match ";" out-path)
      (insert (format "\nSetOutPath \"%s\"" out-path))
      (message "Constructing %s" out-path))
    (when file-list
      (mapc 
       (lambda(file)
         (cond
          ((string-match "/[.][.]?$" file))
          ((file-directory-p file)
           (setq out2 (concat out-path "\\" (file-name-nondirectory file)))
           (setq lst (remove-if '(lambda(x)
                                   (string-match (or exclude-reg "/\\([.][.]?\\|.*~\\|[#].*[#]\\|[.]git\\|[.]bzr\\)$") x))
                                (directory-files (concat file "/") t)))
           (setq changed-out t)
           (build-nsi-file lst out2 exclude-reg))
          (t
           (when changed-out
             (insert (format "\nSetOutPath \"%s\"" out-path))
             (setq changed-out nil))
           (insert (format "\nFile \"%s\""
                           (replace-regexp-in-string "/" "\\\\" file))))))
       file-list))
    (symbol-value 'ret)))

(defvar build-nsi-batch-file nil
  "Batch file that is building the nsis installers.")

(defun build-nsi-setinel (process event)
  (when (string= event "finished\n")
    (when build-nsi-batch-file
      (delete-file build-nsi-batch-file)
      (setq build-nsi-batch-file nil))))

;;;###autoload
(defun build-nsi ()
  "Build NSI based on current install"
  (interactive)
  (let (ret
        (ep-ver (format-time-string "%y.%m.%d"))
        tmp
        tmp2
        (tmp-file (make-temp-file "emacs-make" nil "bat"))
        (makensis (concat "@" (replace-regexp-in-string "/" "\\"
                                                        (expand-file-name "~app/nsis/makensis.exe") t t)))
        (bat ""))
    (setq bat
          (concat makensis " "
                  (replace-regexp-in-string "/" "\\" (expand-file-name "~nsi/EmacsInstall.nsi") t t) "\n"))
    (with-temp-file "~nsi/EmacsInstall.nsi"
      (setq buffer-file-coding-system 'raw-text)
      (insert (replace-regexp-in-string
               "[.][.]\\\\" (concat
                             (replace-regexp-in-string "/" "\\\\\\\\"
                                                       (expand-file-name "~other/source/")))
               "CRCCheck On
RequestExecutionLevel user

; Best Compression
SetCompress Auto
SetCompressor /SOLID lzma
SetCompressorDictSize 32
SetDatablockOptimize On
;SetCompress off
;automatically close the installer when done.
AutoCloseWindow true 
!include \"FileFunc.nsh\"

!include \"MUI2.nsh\"

Name \"EmacsPortable.App\"
BrandingText \"EmacsPortable.App\"

OutFile \"\"

!define MUI_HEADERIMAGE

!define MUI_HEADERIMAGE_BITMAP \"..\\img\\headerimage.bmp\" ; 150x57 pixels
!define MUI_HEADERIMAGE_BITMAP_NOSTRETCH
!define MUI_HEADERIMAGE_UNBITMAP \"..\\img\\headerimage.bmp\" ; 150x57 pixels

!define MUI_WELCOMEFINISHPAGE_BITMAP \"..\\img\\welcome.bmp\" ;164x314 pixels
!define MUI_UNWELCOMEFINISHPAGE_BITMAP \"..\\img\\welcome.bmp\" ;164x314 pixels

!define MUI_ABORTWARNING
!define MUI_UNABORTWARNING

!define MUI_PAGE_HEADER_TEXT \"EmacsPortable.app\"
!define MUI_PAGE_HEADER_SUBTEXT \"Emacs on the Go\"

!define MUI_COMPONENTSPAGE_SMALLDESC
!define MUI_HEADERIMAGE_RIGHT

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE \"..\\gpl-3.0.rtf\"
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_COMPONENTS

;MUI_PAGE_STARTMENU pageid variable
!insertmacro MUI_PAGE_INSTFILES

!insertmacro MUI_LANGUAGE \"English\"

Var PA
Var PG
Function GetDriveVars
  StrCmp $9 \"c:\\\" git
  StrCmp $8 \"HDD\" gpa
  StrCmp $9 \"a:\\\" spa
  StrCmp $9 \"b:\\\" spa
  
  gpa:
  IfFileExists \"$9PortableApps\" 0 git
  StrCpy $PA \"$9PortableApps\"
  git:
  IfFileExists \"$9cygwin\bin\git.exe\" 0 +2
  StrCpy $PG \"$9cygwin\bin\git.exe\"
  
  IfFileExists \"$9msysgit\msysgit\cmd\git.cmd\" 0 +2
  StrCpy $PG \"$9msysgit\msysgit\cmd\git.cmd\"
  
  IfFileExists \"$9msysgit\cmd\git.cmd\" 0 +2
  StrCpy $PG \"$9msysgit\cmd\git.cmd\"
  
  IfFileExists \"$9PortableGit\cmd\git.cmd\" 0 +2
  StrCpy $PG \"$9PortableGit\cmd\git.cmd\"
  
  IfFileExists \"$9PortableApps\PortableGit\cmd\git.cmd\" 0 +2
  StrCpy $PG \"$9PortableApps\PortableGit\cmd\git.cmd\"
  
  IfFileExists \"$9Apps\PortableGit\cmd\git.cmd\" 0 +2
  StrCpy $PG   \"$9Apps\PortableGit\cmd\git.cmd\"
  
  IfFileExists \"$9GitPortable\cmd\git.cmd\" 0 +2
  StrCpy $PG \"$9GitPortable\cmd\git.cmd\"
  
  IfFileExists \"$9PortableApps\GitPortable\cmd\git.cmd\" 0 +2
  StrCpy $PG \"$9PortableApps\GitPortable\cmd\git.cmd\"
  
  IfFileExists \"$9Apps\GitPortable\cmd\git.cmd\" 0 +2
  StrCpy $PG   \"$9Apps\GitPortable\cmd\git.cmd\"
  spa:
  Push $0
  
  FunctionEnd
Function .onInit
StrCpy $PA \"c:\\\"
${GetDrives} \"FDD+HDD\" \"GetDriveVars\"
StrCpy $INSTDIR \"$PA\EmacsPortable.App\"
FunctionEnd
"))
      
      (insert (format "\n!include \"%s\"\n"
                      (replace-regexp-in-string "/" "\\\\"
                                                (expand-file-name "setupEmacsData.nsh" "~nsi/"))))
      (insert "Section Main sec_main\n")
      (insert "SetOutPath \"$INSTDIR\"\n")
      (setq tmp (replace-regexp-in-string "/" "\\" (expand-file-name "~ep/") t t))
      (mapc
       (lambda(x)
         (insert (format "File \"%s\"\n"
                         (replace-regexp-in-string "/" "\\" x t t))))
       (remove-if (lambda(x)
                    (string-match "\\(EmacsPortableApp-\\|EmacsInstall\\)" x))
                  (directory-files (expand-file-name "~ep/") t ".*[.]exe")))
      (insert (format "File \"%s*.html\"\n" tmp))
      (insert (format "File \"%s*.org\"\n" tmp))
      (insert (format "File \"%sApp*.xml\"\n" tmp))
      (insert (format "File \"%sAppRun\"\n" tmp))
      (insert (format "File \"%s.DirIcon\"\n" tmp))
      (insert (format "File \"%s*.desktop\"\n" tmp))
      (build-nsi-file (directory-files "~ep/Contents" t)
                      "$INSTDIR\\Contents")
      (build-nsi-file (directory-files (concat "~app/emacs-" emacs-ver) t)
                      (concat "$INSTDIR\\App\\emacs-" emacs-ver))
      (build-nsi-file
       (remove-if '(lambda(x) (string-match
                          (format "/\\(%s\\|emacs-.*\\|.*~\\|[#].*[#]\\)$"
                                  (regexp-opt
                                   '("." ".."
                                     "eps" "NSIS" "ahk" "7z" ".git"
                                     ))) x))
                  (directory-files "~app/" t))
       "$INSTDIR\\App")
      (build-nsi-file
       (remove-if '(lambda(x) (string-match
                          (format "/\\(%s\\|EmacsPortableServer-.*exe\\|rm-.*exe\\|.*~\\|[#].*[#]\\)$"
                                  (regexp-opt
                                   '("." ".." ".git"))) x))
                  (directory-files "~app/eps" t))
       "$INSTDIR\\App\\eps")
      (build-nsi-file (directory-files "~other/" t) "$INSTDIR\\Other")
      
      (insert "\n${setupInstallData}\n")
      (insert "SectionEnd\n")
      (goto-char (point-min))
      (when (re-search-forward "OutFile.*")
        (replace-match (format "OutFile \"%s\\EmacsInstall-%s-ep%s.exe\""
                               (replace-regexp-in-string "/" "\\"
                                                         (expand-file-name "~ep/") t t)
                               emacs-ver ep-ver) t t))
      (setq ret (buffer-string))
      (goto-char (point-min))
      (when (search-forward "!insertmacro MUI_PAGE_COMPONENTS")
        (replace-match ""))
      (goto-char (point-max))
      (insert "\nSection \"-hidden\"\nExec '\"$INSTDIR\\EmacsOptions.exe\" /all'\nSectionEnd\n"))
    (setq tmp
          (remove-if (lambda(x) (string-match "/\\([.][.]?\\|user\\|system\\|shared\\|[.]git\\)$" x))
                     (directory-files "~start/" t)))
    (when tmp
      (mapc
       (lambda(x)
         (setq bat
               (concat bat makensis " "
                       (replace-regexp-in-string "/" "\\" (expand-file-name (format "~nsi/EmacsInstall-%s.nsi" (file-name-nondirectory x))) t t) "\n"))
         (with-temp-file (format "~nsi/EmacsInstall-%s.nsi" (file-name-nondirectory x))
           (setq buffer-file-coding-system 'raw-text)
           (insert ret)
           (insert (format "\nSection \"%s\"" (file-name-nondirectory x)))
           (build-nsi-file (remove-if
                            (lambda(x)
                              (string-match "\\([.][.]?\\(git\\|bzr\\|svn\\)\\|.*[#~]\\)$" x))
                            (directory-files (format "~start/%s/" (file-name-nondirectory x)) t))
                           (format "$INSTDIR\\Data\\start\\%s" (file-name-nondirectory x)))
           (setq tmp2 (remove-if
                       (lambda(x)
                         (string-match (format "/\\(%s\\|.*~\\)$"
                                               (regexp-opt
                                                '("." ".."
                                                  ".git"
                                                  "abbrev_defs"
                                                  "history"
                                                  "auto-save-list"
                                                  "eshell"
                                                  "url"
                                                  "var"
                                                  ".emacs.desktop"
                                                  ".emacs.keyfreq"
                                                  ".org-id-locations"
                                                  ".recentf"
                                                  "ac-comphist.dat"
                                                  "bookmarks"
                                                  "org-clock-save.el"
                                                  "org-clock-save.el~"
                                                  "tramp"))) x))
                       (directory-files "~/.emacs.d/" t)))
           (when tmp2
             (build-nsi-file tmp2
                             "$INSTDIR\\Data\\Home\\.emacs.d"))
           
           (insert "\nSectionEnd")
           (goto-char (point-min))
           (when (re-search-forward "OutFile.*")
             (replace-match (format "OutFile \"%s\\EmacsInstall-%s-ep%s-%s.exe\""
                                    (replace-regexp-in-string "/" "\\"
                                                              (expand-file-name "~ep/") t t)
                                    emacs-ver ep-ver (file-name-nondirectory x))
                            t t))
           (goto-char (point-max))
           (insert "\nSection \"-hidden\"\nWriteIniStr \"$INSTDIR\\Data\\ini\\EmacsPortableApp.ini\" \"EmacsPortableApp\" \"Startup\" \"")
           (insert (file-name-nondirectory x))
           (insert "\" z\nExec '\"$INSTDIR\\EmacsOptions.exe\" /all'\nSectionEnd\n")))
       tmp))
    (with-temp-file tmp-file
      (insert bat))
    (start-process-shell-command "build-nsi" "*build-nsi*" bat)
    (set-process-sentinel (get-process "build-nsi") 'build-nsi-setinel)))

;;;###autoload
(defun build-nsi-check ()
  "Checks the capabilities of the current emacs."
  (interactive)
  (message "Additional Support\nPNG: %s\nJPEG: %s\nTIFF: %s\nGIF: %s\nGnuTLS: %s"
           (image-type-available-p 'png)
           (image-type-available-p 'jpeg)
           (image-type-available-p 'tiff)
           (image-type-available-p 'gif)
           (gnutls-available-p)))
;;;###autoload
(defun build-nsi-zip ()
  "Converts NSIS files to zip files using 7zip"
  (interactive)
  (let ((zip (expand-file-name "~app/7z/7zG.exe")))
    (when (file-exists-p zip)
      (mapc
       (lambda(x)
         (let ((zip-file (concat (file-name-sans-extension x) ".zip")))
           (unless (file-exists-p zip-file) 
             (let ((default-directory (make-temp-file "epi-" t)))
               (shell-command-to-string (format "%s x %s -o%s" zip x default-directory))
               (delete-directory (concat default-directory "/$PLUGINSDIR") t)
               (shell-command-to-string (format "cmd /c \"cd %s && %s a -mx9 -tzip -r %s *.*\""
                                                (replace-regexp-in-string "/" "\\" default-directory t t)
                                                zip zip-file))
               (delete-directory default-directory t)))))
       (directory-files "~ep/" t "EmacsInstall-.*exe$")))))

;;;###autoload
(defun build-nsi-publish-doc ()
  "Publishes EmacsPortable.App Documentation."
  (interactive)
  ;; First publish to wiki
  (require 'org-readme nil t)
  (find-file "~ep/README.org")
  (when (fboundp 'org-readme-convert-to-emacswiki)
    (org-readme-convert-to-emacswiki))
  ;; Publish to website as well
  (when (file-exists-p "~ep/.git/config")
    (when (with-temp-buffer
            (insert-file-contents "~ep/.git/config")
            (search-forward "git@github.com:mlf176f2/EmacsPortable.App.git"))
      (unless (file-exists-p "~ep/pages")
        (shell-command (concat "git clone git@github.com:mlf176f2/EmacsPortable.App.git " (expand-file-name "~ep/pages"))))
      (find-file "~ep/pages")
      (shell-command "git checkout gh-pages")
      (load "~nsi/build")
      (build-help)
      (copy-file "~ep/help.html" "~ep/pages/index.html" t t)
      (shell-command "git add index.html")
      (copy-file "~ep/Other/style.css" "~ep/pages/Other/style.css" t t)
      (shell-command "git add Other/style.css")
      (mapc
       (lambda(x)
         (copy-file (concat "~ep/Other/img/" x) (concat "~ep/pages/Other/img/" x) t t)
         (shell-command (concat "git add Other/img/" x)))
       (remove-if (lambda(x) (string-match "^[.]*$" x))
                  (directory-files "~ep/Other/img" nil)))
      (mapc
       (lambda(x)
         (copy-file (concat "~ep/Other/images/" x) (concat "~ep/pages/Other/images/" x) t t)
         (shell-command (concat "git add Other/images/" x)))
       (remove-if (lambda(x) (string-match "^[.]*$" x))
                  (directory-files "~ep/Other/images" nil)))
      (shell-command (format "git commit -m \"%s\"" (format-time-string "Automatic Website Update %Y.%m.%d %T")))
      (shell-command "git push origin gh-pages"))))

(provide 'build-nsi)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; build-nsi.el ends here






