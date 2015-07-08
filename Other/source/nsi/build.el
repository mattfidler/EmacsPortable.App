;;; build.el --- Build EmacsPortable.App nsi scripts from org-mode
;; 
;; Filename: build.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Thu Jan 19 19:13:25 2012 (-0600)
;; Version: 
;; Last-Updated: Tue Aug 28 19:34:53 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 120
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
    (when (> 24 emacs-major-version )
      (setq org-dir (nth 0 (directory-files (concat build-dir "../../../Data/lisp/src") t "\\<org")))
      (load (concat org-dir "/lisp/org-install"))
      (setq load-path (cons (concat org-dir "/lisp") load-path))
      (setq load-path (cons (concat org-dir "/lisp/contrib") load-path)))
    (require 'org)(require 'org-exp nil t)(require 'ob nil t)(require 'ob-tangle nil t)
    (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))
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

(require 'cl)

(unless (fboundp 'remove-from-list)
  (defalias 'remove-from-list 'remove))

(defun build-help ()
  "Builds the help.html from README.org"
  (interactive)
  (let (str
        (org-export-html-style-extra "<link href=\"http://fonts.googleapis.com/css?family=Oswald\" rel=\"stylesheet\" type=\"text/css\" />\n<link rel=\"stylesheet\" href=\"Other/style.css\" type=\"text/css\" />")
        (org-export-html-style-include-default nil)
        (org-export-htmlize-output-type 'css)
        (menu "")
        p1 p2 p3)
    (find-file (concat build-dir "../../../README.org"))
    (execute-kbd-macro (kbd "C-c C-e h"))
    (with-temp-buffer
      (insert-file-contents (concat build-dir "../../../README.html"))
      (setq str (buffer-string)))
    (delete-file (concat build-dir "../../../README.html"))
    (with-temp-file (concat build-dir "../../../help.html")
      (insert str)
      (goto-char (point-min))
      ;; <div id=\"ep\"><img src=\"Other/emacs-p-132.png\"/></div>
      ;; Change this to freecsstemplates.org
      (goto-char (point-min))
      (when (re-search-forward "<h1 class=\"title\">")
        (goto-char (match-beginning 0))
        (when (re-search-backward "<div id=\"content\">" nil t)
          (replace-match "<div id=\"header-wrapper\"><div id=\"header\"><div id=\"logo\">")
          (when (re-search-forward "</h1>" nil t)
            (insert "<p>Run Emacs Portably on Windows, Linux and Mac OSX</p></div></div></div><div id=\"page\"><div id=\"page-bgtop\"><div id=\"page-bgbtm\"><div id=\"page-content\">")
            (when (re-search-forward "<div id=\"outline-container-1\"" nil t)
              (goto-char (match-beginning 0))
              (insert "<div id=\"content\">"))
            (when (re-search-forward "<div id=\"postamble\">" nil t)
              (replace-match "</div></div></div></div><div id=\"footer\">" t t)))))
      (goto-char (point-min))
      (when (re-search-forward "id=\"table-of-contents\"" nil t)
        (replace-match "id=\"sidebar\""))
      (goto-char (point-min))
      (setq p3 1)
      (while (re-search-forward "<a href=\"#sec-\\([0-9]+\\)\">" nil t)
        (setq p1 (+ 4 (point)))
        (setq p2 (match-string 1))
        (goto-char (match-beginning 0))
        (insert "<h2>")
        (when (re-search-forward "</a>" nil t)
          (when (< p3 7)
            (setq menu (format "%s<li><a href=\"#sec-%s\">%s</a></li>"
                             menu p2 (replace-regexp-in-string "^[ \t]*[0-9]+[ \t]*" "" (buffer-substring p1 (match-beginning 0))))))
          (setq p3 (+ p3 1))
          (replace-match "</a></h2>")))
      (setq menu (format "<div id=\"menu-wrapper\"><div id=\"menu\"><ul>%s</ul></div></div>"
                         menu))
      (goto-char (point-min))
      (when (re-search-forward "<div id=\"page\">" nil t)
        (goto-char (match-beginning 0))
        (insert menu))
      (goto-char (point-min))
      (while (re-search-forward "<h2>Table of Contents</h2>" nil t)
        (replace-match "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; build.el ends here
