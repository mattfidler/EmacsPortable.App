;;; emacsdownload-ezw.el --- Download Eli Zaretskii's windows builds
;; 
;; Filename: emacsdownload-ezw.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Jan 26 16:33:22 2012 (-0600)
;; Version: 
;; Last-Updated: Fri Aug 24 00:24:20 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 26
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   Cannot open load file: emacsdownload-ezw.
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

(let ((ezwin-emacs-suggested
       '(
         ("FindUtils" "for grep-find and other file searches.")
         ("Grep" "for searching through files with grep.")
         ;;("TexInfo" "used to build Emacs' manuals.")
         ("GnuTLS" "Adds network transport support for Emacs")
         ("libxml2" "Adds XML support for Emacs 24.1+")
         ))
      (fn "")
      (ini "")
      (ini-file "[ezw]\n")
      (ini2 "")
      (ret "")
      (dt ""))
  (mapc
   (lambda(x)
     (let ((pkg (downcase (nth 0 x)))
           (pkg-d (nth 0 x))
           (desc (nth 1 x)))
       (setq ini-file (concat ini-file pkg "=1\n"))
       (setq ini
             (format "%s\n${ezwininstalled} \"%s\" ${sec_ezwine_%s}"
                     ini pkg pkg))
       (setq ini2
             (format "%s\n${ifSecNotRO} ${sec_ezwine_%s} skip_ezwine_group_ro"
                     ini2 pkg))
       (setq ret
             (format "%s\nSection /o \"%s\" sec_ezwine_%s\n!insertmacro ezwindown \"%s\"\nSectionEnd\nLangString DESC_sec_ezwine_%s ${LANG_ENGLISH} \"%s - %s\""
                     ret pkg-d pkg pkg pkg pkg-d desc))
       (setq dt
             (format "%s\n!insertmacro MUI_DESCRIPTION_TEXT ${sec_ezwine_%s} $(DESC_sec_ezwine_%s)"
                     dt pkg pkg))))
   ezwin-emacs-suggested)
  (with-temp-file "./unix-download-ezw.ini"
    (insert ini-file))
  (setq ini2 (format
              "%s\n${setInstallGroup} ${sec_ezwine_grp}\nskip_ezwine_group_ro:\n"
              ini2))
  (setq ini (concat ini "\n" ini2))
  (setq ini2 "")
  (setq ret
        (format
         "SectionGroup \"Suggested EzWinPorts Applications\" sec_ezwine_grp%s
SectionGroupEnd
LangString DESC_sec_ezwine_grp ${LANG_ENGLISH} \"The Emacs Windows FAQ suggests these EzWinPorts packages.\"
!define EZWINE_DESC `!insertmacro EZWINE_DESC`
!macro EZWINE_DESC
  !insertmacro MUI_DESCRIPTION_TEXT ${sec_ezwine_grp} $(DESC_sec_ezwine_grp)
%s\n!macroend" ret dt))
  ;; Now get all gnuwin32 packages listed in the mirrors.ini
  (with-temp-buffer
    (insert-file-contents "../../../App/ini/gw32-install.ini")
    (let ((sec "")
          (sec-cnt 0)
          (txt "")
          (id "")
          (lang "")
          (desc "")
          (desc-tmp "")
          (group-ro "")
          (group-hid "")
          (ret2 ""))
      (goto-char (point-min))
      (while (re-search-forward "\\[ezw:\\(.*?\\)\\]" nil t)
        (setq id (match-string 1))
        (save-excursion
          (when (re-search-forward "desc *= *\\(.*\\)" nil t)
            (setq desc-tmp (match-string 1))
            (with-temp-buffer
              (insert desc-tmp)
              (goto-char (point-min))
              (while (re-search-forward "\"" nil t)
                (replace-match "$\\\"" t t))
              (setq desc-tmp (buffer-substring-no-properties (point-min) (point-max))))
            (when (re-search-backward "\\[ezw:\\(.*\\)\\]" nil t)
              (when (string= id (match-string 1))
                (setq desc (format
                            "%s\n!insertmacro MUI_DESCRIPTION_TEXT ${sec_ezwin_%s} $(DESC_sec_ezwin_%s)" desc (downcase id) (downcase id)))
                (setq lang (format
                            "%s\nLangString DESC_sec_ezwin_%s ${LANG_ENGLISH} \"%s - %s\"\n"
                            lang (downcase id) id desc-tmp))))))
        (setq sec-cnt (+ sec-cnt 1))
        (setq fn
              (concat fn "\nPush \"" (downcase id) "\""))
        (setq sec
              (format
               "%s\nSection /o \"%s\" sec_ezwin_%s\n!insertmacro ezwindown \"%s\"\nSectionEnd"
               sec id (downcase id)  (downcase id)))
        (setq ini2
              (format "%s\n${ifSecNotRO} ${sec_ezwin_%s} skip_ezwin_group_ro"
                      ini2 (downcase id)))
        (setq ini
              (format "%s\n${ezwininstalled} \"%s\" ${sec_ezwin_%s}"
                      ini (downcase id) (downcase id))))
      (setq ini2 (format
                  "%s\n${setInstallGroup} ${sec_ezwin_grp}\nskip_ezwin_group_ro:\n"
                  ini2))
      (setq ini (concat ini "\n" ini2))
      (setq fn (concat "\nFunction PushEZW\n"
                       fn "\nPush " (number-to-string sec-cnt)
                       "\nFunctionEnd\n"))
      (setq ret2 (format "SectionGroup \"All EzWinPorts Utilities\" sec_ezwin_grp
  %s
  SectionGroupEnd
  %s
  LangString DESC_sec_ezwin_grp ${LANG_ENGLISH} \"These are the EzWinPorts Apps that EmacsPortable.App is aware of.\"
  !define EZWIN_DESC `!insertmacro EZWIN_DESC`
  !macro EZWIN_DESC
  !insertmacro MUI_DESCRIPTION_TEXT ${sec_ezwin_grp} $(DESC_sec_ezwin_grp)
  %s
  !macroend
  !define EZWIN_INI `!insertmacro EZWIN_INI`
  !macro EZWIN_INI
    %s
  !macroend" sec lang desc ini))
      (setq ret (concat ret "\n" ret2))))
  (with-temp-file "./emacsdownload-ezwin.nsi"
    (insert ret)
    (insert "\n!include \"emacsdownload-ezwin-push.nsi\"\n"))
  (with-temp-file "./emacsdownload-ezwin-push.nsi"
    (insert fn))
  (with-temp-file "./emacsdownload-rezwin.nsi"
    (insert ret)
    (goto-char (point-min))
    (while (re-search-forward "ezwin" nil t)
      (replace-match "rezwin"))
    (goto-char (point-min))
    (while (re-search-forward "!insertmacro rezwindown" nil t)
      (replace-match "${ezwinrm}"))
    (goto-char (point-min))
    (while (re-search-forward "EZWIN_INI" nil t)
      (replace-match "REZWIN_INI"))
    (goto-char (point-min))
    (while (re-search-forward "ezwininstalled" nil t)
      (replace-match "ezwinremoved"))
    (goto-char (point-min))
    (when (re-search-forward "SectionGroup" nil t)
      (let ((one (match-beginning 0)))
        (goto-char (point-min))
        (when (re-search-forward "SectionGroupEnd" nil t)
          (end-of-line)
          (delete-region one (point)))))
    (goto-char (point-min)) 
    (while (re-search-forward ".*sec_rezwine_.*" nil t)
      (replace-match ""))
    (when (re-search-forward "SectionGroup \".*\"" nil t)
      (replace-match "SectionGroup \"Remove EZWinPorts Utilities\""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacsdownload-ezw.el ends here
