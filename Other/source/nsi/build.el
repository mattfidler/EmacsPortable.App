;;; build.el --- Build EmacsPortable.App nsi scripts from org-mode
;; 
;; Filename: build.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Thu Jan 19 19:13:25 2012 (-0600)
;; Version: 
;; Last-Updated: Fri Feb  3 15:19:34 2012 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 69
;; URL: 
;; Keywords: 
;; Compatibility:
;; 
;; Features that might be required by this library:
;;
;;   Cannot open load file: build.
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

(setq debug-on-error t)

(setq build-dir
      (file-name-directory
       (or
        load-file-name
        buffer-file-name)))

(defun one-nsi ()
  (let (tmp nsi-file)
    (when (getenv "NSI_FILE")
      (setq nsi-file (getenv "NSI_FILE"))
      (while (string-match "^\"+" nsi-file)
        (setq nsi-file (replace-match "" t t nsi-file)))
      (while (string-match "[.]nsi.+" nsi-file)
        (setq nsi-file (replace-match ".org" t t nsi-file)))
      (find-file (expand-file-name nsi-file build-dir))
      (while (re-search-forward "!include \"\\(.*[.]nsi\\)\"" nil t)
        (message "Found %s, inserting" tmp)
        (setq tmp (match-string 1))
        (replace-match "\n")
        (insert-file-contents (expand-file-name tmp build-dir)))
      (save-buffer (current-buffer)))))

(defun build-nsi (&optional org-f eval-lisp )
  "Build NSIS script from org-mode."
  (let ((case-fold-search t)
        org-dir
        (org-file (format "%s" (symbol-name org-f)))
        org-files)
    (if org-file
        (setq org-files (list (expand-file-name org-file build-dir)))
      (if (getenv "ORG_FILE")
          (progn
            (setq org-files (getenv "ORG_FILE"))
            ;; fix the differences between eterm make and command
            ;; prompt make
            (while (string-match "^\"+" org-files)
              (setq org-files (replace-match "" t t org-files)))
            (while (string-match "[.]org.+" org-files)
              (setq org-files (replace-match ".org" t t org-files)))
            (message "Trying Org files: %s" org-files)
            (setq org-files (list (expand-file-name org-files
                                                    build-dir))))
        (setq org-files (directory-files build-dir nil ".org"))))
    (setq org-dir (nth 0 (directory-files (concat build-dir "../../../App/lisp/src") t "\\<org")))
    (load (concat org-dir "/lisp/org-install"))
    (setq load-path (cons (concat org-dir "/lisp") load-path))
    (setq load-path (cons (concat org-dir "/lisp/contrib") load-path))
    (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))
    (require 'org)(require 'org-exp)(require 'ob)(require 'ob-tangle)
    (mapc
     (lambda(file)
       (let ((org-confirm-babel-evaluate nil))
         (message "Processing %s" file)
         (find-file (expand-file-name file build-dir))
         (when eval-lisp
           (goto-char (point-min))
           (while (re-serach-forward "#+BEGIN_SRC[ \t]+emacs-lisp" nil t)
             (org-babel-execute-src-block)))
         (org-babel-tangle)
         (kill-buffer)))
     org-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; build.el ends here
