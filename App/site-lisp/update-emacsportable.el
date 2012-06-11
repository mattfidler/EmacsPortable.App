;;; update-emacsportable.el --- Update EmacsPortable.App mirrors.ini
;; 
;; Filename: update-emacsportable.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Jun  7 16:07:47 2012 (-0500)
;; Version:
;; Last-Updated: Mon Jun 11 08:45:04 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 26
;; URL: 
;; Keywords: 
;; Compatibility: 
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

(setq usb-app-dir (expand-file-name (concat (file-name-directory (or
                                                                  load-file-name
                                                                  buffer-file-name)) "../")))

(defun offical-emacs ()
  "Update Official releases"
  (interactive)
  (let ((buf (url-retrieve-synchronously "http://ftpmirror.gnu.org/emacs/windows/"))
        (emacs-vers '())
        (emacs-pretest '())
        (ret ""))
    (when buf
      (save-excursion
        (switch-to-buffer buf)
        (setq emacs-vers (get-emacs-ver-from-buffer))
        (kill-buffer (current-buffer))))
    (setq buf (url-retrieve-synchronously "http://alpha.gnu.org/gnu/emacs/pretest/windows/"))
    (when buf
      (save-excursion
        (switch-to-buffer buf)
        (setq emacs-pretest (get-emacs-ver-from-buffer))
        (kill-buffer (current-buffer))))
    (setq buf (url-retrieve-synchronously "http://emacsformacosx.com/builds"))
    (when buf
      (save-excursion
        (switch-to-buffer buf)
        (setq ret (concat
                   (get-mac-emacs-ver-from-buffer emacs-vers "http://ftpmirror.gnu.org/emacs/windows/emacs-") "\n"
                   (get-mac-emacs-ver-from-buffer emacs-pretest "http://alpha.gnu.org/gnu/emacs/pretest/windows/emacs-")))
        
        (kill-buffer (current-buffer))))
    (fix-mirrors.ini (nth 0 (reverse (sort emacs-vers 'string-lessp))) ret)))

(defun fix-mirrors.ini (ver what)
  "Fix Mirrors.ini"
  (let ((mirrors.ini (concat usb-app-dir "ini/mirrors.ini"))
        pt)
    (with-temp-buffer
      (insert-file-contents mirrors.ini)
      (goto-char (point-min))
      (while (re-search-forward "\\[[0-9.]+\\(?:-rc[0-9.]*\\)?\\]" nil t)
        (delete-region (point-at-bol)
                       (save-excursion
                         (if (re-search-forward "^[ \t]*\\[" nil t)
                             (point-at-bol)
                           (point-at-eol)))))
      (goto-char (point-min))
      (when (re-search-forward "^ *default[.]ver *=" nil t)
        (delete-region (point) (point-at-eol))
        (insert ver)
        (insert what))
      (write-file mirrors.ini))))

(defun get-mac-emacs-ver-from-buffer (known-versions prefix)
  "Gets the mac emacs versions from the buffer and the known versions of emacs.  Will create INI file strings for buffers"
  (let ((re (regexp-opt known-versions 't))
        (ret "")
        (found '()))
    (goto-char (point-min))
    (while (re-search-forward (format "/Emacs-\\(?:pretest-\\)?\\(%s\\).*?[.]dmg" re) nil t)
      (unless (member (match-string 1) found)
        (setq ret
              (format "%s\n[%s]\nwin=%s%s-bin-i386.zip\nmac=http://emacsformacosx.com/emacs-builds%s"
                      ret (match-string 1) prefix (match-string 1) (match-string 0))))
      (add-to-list 'found (match-string 1)))
    (mapc (lambda(x)
            (unless (member x found)
              (setq ret
                    (format "%s\n[%s]\nwin=%s%s-bin-i386.zip"
                            ret x prefix x))))
          known-versions)
    (symbol-value 'ret)))

(defun get-emacs-ver-from-buffer ()
  "Gets the emacs versions from the buffer"
  (goto-char (point-min))
  (let (emacs-vers)
    (goto-char (point-min))
    (while (re-search-forward "emacs-\\([0-9.]+\\(?:-rc[0-9.]*\\)?\\)-bin-i386.zip" nil t)
      (add-to-list 'emacs-vers (match-string 1)))
    (symbol-value 'emacs-vers)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; update-emacsportable.el ends here
