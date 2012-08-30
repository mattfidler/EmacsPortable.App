;;; build-nsi.el --- Builds NSIS installation file based on current install.
;; 
;; Filename: build-nsi.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Aug 29 11:58:51 2012 (-0500)
;; Version: 
;; Last-Updated: Wed Aug 29 16:34:19 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 88
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   Required feature `build-nsi' was not provided.
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

(defvar ep-ver 0.01
  "Version for EmacsPortable.")

(defun build-nsi-file (file-list out-path &optional exclude-reg)
  "Builds NSIS file information"
  (let (changed-out
        out2
        lst)
    (insert (format "\nSetOutPath \"%s\"" out-path))
    (message "Constructing %s" out-path)
    (when file-list
      (mapc 
       (lambda(file)
         (cond
          ((string-match "/[.][.]?$" file))
          ((file-directory-p file)
           (setq out2 (concat out-path "\\" (file-name-nondirectory file)))
           (setq lst (remove-if '(lambda(x)
                                   (string-match (or exclude-reg "/\\([.][.]?\\|.*~\\|[#].*[#]\\)$") x))
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

(defun build-nsi ()
  "Build NSI based on current install"
  (interactive)
  (let (ret tmp tmp2)
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
${GetDrives} \"FDD+HDD\" \"GetDriveVars\"
StrCpy $INSTDIR \"$PA\EmacsPortable.App\"
FunctionEnd
"))
      
      (insert (format "\n!include \"%s\"\n"
                      (replace-regexp-in-string "/" "\\\\"
                                                (expand-file-name "setupEmacsData.nsh" "~nsi/"))))
      (insert "Section Main\n")
      (insert "SetOutPath \"$INSTDIR\"\n")
      (setq tmp (replace-regexp-in-string "/" "\\" (expand-file-name "~ep/") t t))
      (insert (format "File \"%s\*.exe\"\n" tmp))
      (insert (format "File \"%s\*.html\"\n" tmp))
      (insert (format "File \"%s\*.org\"" tmp))
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
                          (format "/\\(%s\\|EmacsPortableServer-.*\\|.*~\\|[#].*[#]\\)$"
                                  (regexp-opt
                                   '("." ".." ".git"))) x))
                  (directory-files "~app/eps" t))
       "$INSTDIR\\App\\eps")
      (build-nsi-file (directory-files "~other/" t) "$INSTDIR\\Other")
      
      (insert "\n${SetupData}\n")
      (insert "SectionEnd\n")
      (goto-char (point-min))
      (when (re-search-forward "OutFile.*")
        (replace-match (format "OutFile \"%s\\EmacsInstall-%s-ep%s.exe\""
                               (replace-regexp-in-string "/" "\\"
                                                 (expand-file-name "~ep/") t t)
                               emacs-ver ep-ver) t t))
      (setq ret (buffer-string)))
    (setq tmp
          (remove-if (lambda(x) (string-match "/\\([.][.]?\\|user\\|system\\|shared\\|[.]git\\)$" x))
                     (directory-files "~start/" t)))
    (when tmp
      (mapc
       (lambda(x)
         (with-temp-file (format "~nsi/EmacsInstall-%s.nsi" (file-name-nondirectory x))
           (setq buffer-file-coding-system 'raw-text)
           (insert ret)
           (insert (format "\nSection \"%s\"" (file-name-nondirectory x)))
           (build-nsi-file (directory-files (format "~start/%s/" (file-name-nondirectory x)) t)
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
                            t t))))
       tmp))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; build-nsi.el ends here






