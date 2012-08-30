;;; emacsdownload-gw32.el --- Generate GnuWin32 NSI
;; 
;; Filename: emacsdownload-gw32.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Sat Jan 21 13:06:23 2012 (-0600)
;; Version: 
;; Last-Updated: Thu Aug 30 12:30:54 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 104
;; URL:
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   Cannot open load file: emacsdownload-gw32.
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

(let ((gw32-emacs-suggested
       '(("Arc" "used by archive-mode to edit .arc files.")
         ("Bzip2" "used by Emacs to automatically decompress .bz2 files.")
         ("BsdTar" "used by tar-mode to edit tar files.")
         ("CompFace" "used by gnus to display XFace headers in messages.")
         ("CoreUtils" "GNU file, shell and text utilities (also in MSYS)")
         ("DiffUtils" "for ediff and producing patches")
         ;;("FindUtils" "for grep-find and other file searches.")
         ("GifLib" "library to support GIF images.")
         ;;("Grep" "for searching through files with grep.")
         ("Gzip" "used by Emacs to automatically decompress .gz files.")
         ("Jpeg" "library to support JPEG images (also in GTK).")
         ("Lha" "used by archive-mode to edit .lzh files.")
         ("LibPng" "library to support PNG images (also in GTK).")
         ("Tiff" "library to support TIFF images (also in GTK).")
         ("Make" "used by compile for building projects (also in MinGW)")
         ("OpenSSL" "used by gnus to talk to servers over SSL.")
         ("Patch" "used by ediff-patch-file and others to apply patches.")
         ("TexInfo" "used to build Emacs' manuals.")
	 ("Sed" "Used to build magit")
	 ("Unzip" "used by archive-mode for extracting zip files.")
         ("Xpm" "library to support XPM images (bundled with Emacs binaries)")
         ("Zip" "used by archive-mode for editing zip files.")
         ("Zlib" "required by LibPng (also in GTK). ")))
      (ini-file "[gw32]\n")
      (ini "")
      (ini3 "")
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
             (format "%s\n${g32installed} \"%s\" ${sec_gw32e_%s}"
                     ini pkg pkg))
       (setq ini2
             (format "%s\n${ifSecNotRO} ${sec_gw32e_%s} skip_gw32e_group_ro"
                     ini2 pkg))
       (setq ini3
             (format "%s\n${ifSecRO} ${sec_gw32e_%s} +2\n SectionSetFlags ${sec_gw32e_%s} ${SF_SELECTED}"
                     ini3 pkg pkg))
       (setq ret
             (format "%s\nSection /o \"%s\" sec_gw32e_%s\n!insertmacro g32down \"%s\"\nSectionEnd\nLangString DESC_sec_gw32e_%s ${LANG_ENGLISH} \"%s - %s\""
                     ret pkg-d pkg pkg pkg pkg-d desc))
       (setq dt
             (format "%s\n!insertmacro MUI_DESCRIPTION_TEXT ${sec_gw32e_%s} $(DESC_sec_gw32e_%s)"
                     dt pkg pkg))))
   gw32-emacs-suggested)
  (setq ini2 (format
              "%s\n${setInstallGroup} ${sec_gw32e_grp}\nskip_gw32e_group_ro:\n"
              ini2))
  (setq ini (concat ini "\n" ini3 "\n" ini2))
  (setq ini2 "")
  (setq ret
        (format
         "SectionGroup \"Suggested GnuWin32 Applications\" sec_gw32e_grp%s
SectionGroupEnd
LangString DESC_sec_gw32e_grp ${LANG_ENGLISH} \"The Emacs Windows FAQ suggests these GnuWin32 packages.\"
!define GW32E_DESC `!insertmacro GW32E_DESC`
!macro GW32E_DESC
  !insertmacro MUI_DESCRIPTION_TEXT ${sec_gw32e_grp} $(DESC_sec_gw32e_grp)
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
      (while (re-search-forward "\\[gw32:\\(.*?\\)\\]" nil t)
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
            (when (re-search-backward "\\[gw32:\\(.*\\)\\]" nil t)
              (when (string= id (match-string 1))
                (setq desc (format
                            "%s\n!insertmacro MUI_DESCRIPTION_TEXT ${sec_gw32_%s} $(DESC_sec_gw32_%s)" desc (downcase id) (downcase id)))
                (setq lang (format
                            "%s\nLangString DESC_sec_gw32_%s ${LANG_ENGLISH} \"%s - %s\"\n"
                            lang (downcase id) id desc-tmp))))))
        (setq sec-cnt (+ sec-cnt 1))
        (setq sec
              (format
               "%s\nSection /o \"%s\" sec_gw32_%s\n!insertmacro g32down \"%s\"\nSectionEnd"
               sec id (downcase id)  (downcase id)))
        (setq ini2
              (format "%s\n${ifSecNotRO} ${sec_gw32_%s} skip_gw32_group_ro"
                      ini2 (downcase id)))
        (setq ini
              (format "%s\n${g32installed} \"%s\" ${sec_gw32_%s}"
                      ini (downcase id) (downcase id))))
      (setq ini2 (format
                  "%s\n${setInstallGroup} ${sec_gw32_grp}\nskip_gw32_group_ro:\n"
                  ini2))
      (setq ini (concat ini "\n" ini2)) 
      (setq ret2 (format "SectionGroup \"All GnuWin32 Utilities\" sec_gw32_grp
  %s
  SectionGroupEnd
  %s
  LangString DESC_sec_gw32_grp ${LANG_ENGLISH} \"These are the GnuWin32 Apps that EmacsPortable.App is aware of.\"
  !define GW32_DESC `!insertmacro GW32_DESC`
  !macro GW32_DESC
  !insertmacro MUI_DESCRIPTION_TEXT ${sec_gw32_grp} $(DESC_sec_gw32_grp)
  %s
  !macroend
  !define G32_INI `!insertmacro G32_INI`
  !macro G32_INI
    %s
  !macroend" sec lang desc ini))
      (setq ret (concat ret "\n" ret2))))
  (with-temp-file "./emacsdownload-gw32.nsi"
    (insert ret))
  (with-temp-file "./emacsdownload-rgw32.nsi"
    (insert ret)
    (goto-char (point-min))
    (while (re-search-forward "gw32" nil t)
      (replace-match "rgw32"))
    (goto-char (point-min))
    (while (re-search-forward "!insertmacro g32down" nil t)
      (replace-match "${g32rm}"))
    (goto-char (point-min))
    (while (re-search-forward "G32_INI" nil t)
      (replace-match "RG32_INI"))
    (goto-char (point-min))
    (while (re-search-forward "g32installed" nil t)
      (replace-match "g32removed"))
    (goto-char (point-min))
    (when (re-search-forward "SectionGroup" nil t)
      (let ((one (match-beginning 0)))
        (goto-char (point-min))
        (when (re-search-forward "SectionGroupEnd" nil t)
          (end-of-line)
          (delete-region one (point)))))
    (goto-char (point-min)) 
    (while (re-search-forward ".*sec_rgw32e_.*" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (when (re-search-forward "SectionGroup \"All GnuWin32 Utilities\"" nil t)
      (replace-match "SectionGroup \"Remove GnuWin32 Utilities\""))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacsdownload-gw32.el ends here
