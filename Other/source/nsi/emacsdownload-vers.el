;;; emacsdownload-vers.el --- What versions of Emacs should be downloaded
;; 
;; Filename: emacsdownload-vers.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Thu Jan 19 21:51:16 2012 (-0600)
;; Version:
;; Last-Updated: Wed Jan 25 13:59:10 2012 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 16
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   Cannot open load file: emacsdownload-vers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This is based on the Emacs versions in the mirrors.ini
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 20-Jan-2012    Matthew L. Fidler  
;;    Last-Updated: Fri Jan 20 08:41:41 2012 (-0600) #2 (Matthew L. Fidler)
;;    Changed EXEDIR to INSTDIR so it can be used by EmacsInstall.nsi
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

(progn
  (with-temp-buffer
    (insert-file-contents "../../../App/ini/mirrors.ini")
    (let ((ret "")
          (grp-ro "")
          (vals "var nemacs\nFunction SetValues\n  IntOp $nemacs $nemacs + 1\n  StrLen $R1 \"$INSTDIR\\App\\emacs-\"\n  StrLen $R2 $R9\n  IntOp $R2 $R1 - $R2\n  StrCpy $R0 $R9 \"\" $R2\n")
          (lang "LangString DESC_sec_emacs_binaries ${LANG_ENGLISH} \"This allows different Emacs Binaries to be downloaded\"\n")
          (desc "!insertmacro MUI_DESCRIPTION_TEXT ${sec_emacs_binaries} $(DESC_sec_emacs_binaries)\n")
          default-ver
          ver-define
          (sel-change "!macro VER_SEL\n  StrCmp \"0\" \"$nemacs\" 0 skip_ver_sel\n")
          (toggle-mac "")
          ver ver2)
      (goto-char (point-min))
      (when (re-search-forward "default[.]ver *= *\\([0-9.]+\\)" nil t)
        (setq default-ver (match-string 1)))
      (goto-char (point-min))
      (while (re-search-forward "\\[\\([0-9.]+\\)\\]" nil t)
        (setq ver (match-string 1))
        (setq ver2 ver)
        (while (string-match "[.]" ver2)
          (setq ver2 (replace-match "_" t t ver2)))
        (setq toggle-mac
              (format "%s\n${selectSectionIf} ${sec_emacs_%s_mac} ${sec_emacs_%s}\n" toggle-mac ver2 ver2))
        (setq sel-change
              (format "%s  SectionGetFlags ${sec_emacs_%s} $0
     IfErrors 0 +4
     ClearErrors
     MessageBox MB_OK \"Emacs Section %s out of range\"
     Goto +3
     IntOp $0 $0 & ${SF_SELECTED}
     StrCmp \"0\" \"$0\" 0 skip_ver_sel\n"
                      sel-change ver2 ver))
        (when (and default-ver
                   (string= default-ver ver))
          (setq ver-define (format "!define sec_emacs_default `${sec_emacs_%s}`\n!define sec_emacs_default_mac `${sec_emacs_%s_mac}`"
                                   ver2 ver2)))
        (setq vals (format
                    "%s
  StrCmp \"$R0\" \"%s\" 0 +23
  SectionSetFlags ${sec_emacs_%s} ${SF_RO}
  IfErrors 0 +4
  ClearErrors 
  MessageBox MB_OK \"Emacs Section %s out of range\"
  Goto +4
  SectionGetText ${sec_emacs_%s} $0
  StrCpy $0 \"$0 (Installed)\"
  SectionSetText ${sec_emacs_%s} $0

  IfFileExists \"$INSTDIR\\App\\emacs-%s\\MacOS\" 0 +14
  SectionSetFlags ${sec_emacs_%s_mac} ${SF_RO}
  IfErrors 0 +4
  ClearErrors
  MessageBox MB_OK \"Emacs Section %s out of range\"
  Goto +9
  SectionGetText ${sec_emacs_%s_mac} $0
  StrCpy $0 \"$0 (Installed)\"
  SectionSetText ${sec_emacs_%s_mac} $0
  IntOp $0 ${SF_RO} | ${SF_SECGRP}
  SectionSetFlags ${sec_emacs_%s_grp} $0
  SectionGetText ${sec_emacs_%s_grp} $0
  StrCpy $0 \"$0 (Win & Mac Installed)\"
  SectionSetText ${sec_emacs_%s_grp} $0\n"
                    vals ver ver2 ver ver2 ver2 ver ver2 ver ver2 ver2 ver2 ver2 ver2))
        (setq grp-ro
              (format
               "%s\n${ifSecNotRO} ${sec_emacs_%s_grp} skip_emacs_grp\n"
               grp-ro ver2))
        (setq lang (format
                    "%sLangString DESC_sec_emacs_%s ${LANG_ENGLISH} \"Download Windows Binaries for Emacs %s.\"\nLangString DESC_sec_emacs_%s_mac ${LANG_ENGLISH} \"Download Mac Binaries for Emacs %s.\"\nLangString DESC_sec_emacs_%s_grp ${LANG_ENGLISH} \"Emacs %s Binaries\"\n"
                    lang ver2 ver ver2 ver ver2 ver))
        (setq desc (format   
                    "%s!insertmacro MUI_DESCRIPTION_TEXT ${sec_emacs_%s} $(DESC_sec_emacs_%s)\n!insertmacro MUI_DESCRIPTION_TEXT ${sec_emacs_%s_mac} $(DESC_sec_emacs_%s_mac)\n!insertmacro MUI_DESCRIPTION_TEXT ${sec_emacs_%s_grp} $(DESC_sec_emacs_%s_grp)\n"
                    desc ver2 ver2 ver2 ver2 ver2 ver2))
        (setq ret (format
                   "%sSectionGroup \"Emacs %s\" sec_emacs_%s_grp\nSection /o \"Emacs %s Windows Binaries\" sec_emacs_%s\n${getEmacsWin} \"%s\"\nSectionEnd\nSection /o \"Emacs %s Mac Binaries\" sec_emacs_%s_mac\n${getEmacsMac} \"%s\"\nSectionEnd\nSectionGroupEnd\n"
                   ret ver ver2 ver ver2 ver ver ver2 ver)))
      (setq ret (format
                 "SectionGroup \"Emacs Binaries\" sec_emacs_binaries\n%s\nSectionGroupEnd\n%s\n!macro EMACS_DESC\n%s\n!macroend\n!define EMACS_DESC `!insertmacro EMACS_DESC`\n%s
%s
${setInstallGroup} ${sec_emacs_binaries}
skip_emacs_grp:
  Push $0
FunctionEnd\n%s\n%s" ret lang desc vals grp-ro (or ver-define "") (if (not ver-define) ""
                                                                    (format "%s
  MessageBox MB_OK \"Since there is no emacs binaries installed, at least one Emacs Binary Selection must be made.\"
  ;SectionSetFlags ${sec_emacs_default} ${SF_SELECTED}
  ;SectionSetFlags ${sec_emacs_default_mac} ${SF_SELECTED}
  skip_ver_sel:
  %s
!macroend
!define VER_SEL `!insertmacro VER_SEL`
" sel-change toggle-mac))))
      (with-temp-file "EmacsDownload-vers.nsi"
        (insert ret)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bmacsdownload-vers.el ends here
