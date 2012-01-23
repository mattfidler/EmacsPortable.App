;;; emacsdownload-pae.el --- Portable Apps that Emacs Recognizes
;; 
;; Filename: emacsdownload-pae.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Jan 19 21:56:36 2012 (-0600)
;; Version: 
;; Last-Updated: Sun Jan 22 15:08:25 2012 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 3
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   Cannot open load file: emacsdownload-pae.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Genereated from mirrors.ini
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

(with-temp-buffer
  (insert-file-contents "../../../App/mirrors.ini")
  (let ((sec "")
        (sec-cnt 0)
        (txt "")
        (id "")
        (lang "")
        (desc "")
        (desc-tmp "")
        (group-ro "")
        (group-hid "")
        (ret ""))
    (goto-char (point-min))
    (while (re-search-forward "\\[pa:e:\\(.*?\\)\\]" nil t)
      (setq id (match-string 1))
      (setq group-ro
            (format
             "%s${ifSecNotRO} ${sec_pa_e_%s} skip_pa_e\n"
             group-ro id))
      (setq group-hid
            (format
             "%s${ifSecNotHidden} ${sec_pa_e_%s} skip_pa_e_hid\n"
             group-hid id))
      (save-excursion
        (when (re-search-forward "desc *= *\\(.*\\)" nil t)
          (setq desc-tmp (match-string 1))
          (when (re-search-backward "\\[pa:e:\\(.*\\)\\]" nil t)
            (when (string= id (match-string 1))
              (setq desc (format
                          "%s\n!insertmacro MUI_DESCRIPTION_TEXT ${sec_pa_e_%s} $(DESC_sec_pa_e_%s)" desc id id))
              (setq lang (format
                          "%s\nLangString DESC_sec_pa_e_%s ${LANG_ENGLISH} \"%s\"\n"
                          lang id desc-tmp))))))
      (setq sec-cnt (+ sec-cnt 1))
      (setq sec
            (format
             "%s\nSection /o \"Portable App %s\" sec_pa_e_%s\n${installPA} \"\pa:e:%s\"\nSectionEnd"
             sec id id id))
      (setq txt
            (format
             "%s\nReadINIStr $R0 \"$EXEDIR\\App\\mirrors.ini\" \"pa:e:%s\" \"name\"
  IfErrors 0 +3
  SectionSetText ${sec_pa_e_%s} \"\"
  Goto +2
  SectionSetText ${sec_pa_e_%s} $R0
  ClearErrors
  ReadINIStr $R0 \"$EXEDIR\\App\\mirrors.ini\" \"pa:e:%s\" \"exist\"
  IfErrors ___err_pa_e_%s
  ${setInstallIfExists} \"$PA\\$R0\" ${sec_pa_e_%s}
  ___err_pa_e_%s:
  ClearErrors
  " txt id id id id id id id)))
    (setq ret (format "SectionGroup \"Recognized PortableApps\" sec_pa_e_grp
  %s
  SectionGroupEnd
  !define PA_INI `!insertmacro PA_INI`
  !macro PA_INI
  %s
  %s
  ${setInstallGroup} ${sec_pa_e_grp}
  skip_pa_e:
  %s
  SectionSetText ${sec_pa_e_grp} \"\"
  skip_pa_e_hid:
  !macroend
  %s
  LangString DESC_sec_pa_e_grp ${LANG_ENGLISH} \"These are Portable Applications that EmacsPortable.App can use.\"
  !define PA_DESC `!insertmacro PA_DESC`
  !macro PA_DESC
  !insertmacro MUI_DESCRIPTION_TEXT ${sec_pa_e_grp} $(DESC_sec_pa_e_grp)
  %s
  !macroend" sec txt group-ro group-hid lang desc))
    (with-temp-file "./EmacsDownload-pae.nsi"
      (insert ret))
    "./EmacsDownload-pae.nsi"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacsdownload-pae.el ends here
