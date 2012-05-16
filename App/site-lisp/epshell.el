;;; epshell.el --- Helpers for inferior shells on EmacsPortable.
;; 
;; Filename: epshell.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Tue May 15 13:42:14 2012 (-0500)
;; Version: 
;; Last-Updated: Tue May 15 14:58:04 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 9
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
;; This is mostly a copy of Lennart Borgman's w32shell.
;;
;; w32-shell assumes you are running in EmacsW32, and we are running
;; under windows, so some file directory locations have been changed.
;; This is also likely not to work with any other distributions
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


(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers
(defun epshell-in-exec-path (path)
  (let ((dc-exec-path (mapcar (lambda (elt)
                                (downcase elt))
                              exec-path)))
    (member (downcase path) dc-exec-path)))

(defun epshell-add-exec-path (path &optional append)
  (unless (epshell-in-exec-path path)
    (add-to-list 'exec-path path append)))

(defun epshell-remove-exec-path (path)
  (unless (listp path) (setq path (list path)))
  (let ((dcpath (mapcar (lambda (elt)
                          (downcase elt))
                        path)))
    (dolist (dc dcpath)
      (setq exec-path (mapcar (lambda (elt)
                                (unless (equal dc (downcase elt))
                                  elt))
                              exec-path)))
    (setq exec-path (delete nil exec-path))))


(defun epshell-in-envpath (path)
  (let ((envpath (replace-regexp-in-string "\\\\" "/" (getenv "PATH")))
        (norpath (replace-regexp-in-string "\\\\" "/" path))
        (case-fold-search t))
    (string-match (concat "\\(?:^\\|;\\)" (regexp-quote norpath) "\\($\\|;\\)") envpath)))

(defun epshell-add-envpath (path &optional append)
  (unless (epshell-in-envpath path)
    (let ((bslash-path (replace-regexp-in-string "/" "\\\\" path)))
      (if append
          (setenv "PATH" (concat (getenv "PATH") ";" bslash-path))
        (setenv "PATH" (concat bslash-path ";" (getenv "PATH")))))))

(defun epshell-remove-envpath (path)
  (let ((paths path))
    (unless (listp paths) (setq paths (list paths)))
    (dolist (path path)
      (let ((envpath (replace-regexp-in-string "\\\\" "/" (getenv "PATH")))
            (pos (epshell-in-envpath path)))
        (while pos
          (let* (
                 (sub1 (if (= 0 pos) "" (substring envpath 0 pos)))
                 (sub2 (substring envpath (+ pos 1 (length path))))
                 (newenvpath (replace-regexp-in-string "/" "\\\\" (concat sub1 sub2))))
            (setenv "PATH" newenvpath))
          (setq envpath (replace-regexp-in-string "\\\\" "/" (getenv "PATH")))
          (setq pos (epshell-in-envpath path))
          )))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs itself in path
;; FIx-me: is this needed?
;; (defun epshell-emacs-path ()
;;   (file-name-as-directory exec-directory))
;;   (if (string= "/" (substring exec-directory -1))
;;                         (setq emacs-path (substring exec-directory 0 -1))
;;                       exec-directory))
(defun epshell-emacsw32-gnuwin32-bindir ()
  ;;(lwarn '(epshell-emacsw32-gnuwin32-bindir) :warning "exec-directory=%s" exec-directory)
  (let* ((bin (list (file-name-as-directory (expand-file-name "bin" (expand-file-name "gw32" usb-app-dir)))
                    (file-name-as-directory (expand-file-name "bin" (expand-file-name "ezwin"))))))
    ;;(lwarn '(epshell-emacsw32-gnuwin32-bindir) :warning "top=%s" top)
    ;;(lwarn '(epshell-emacsw32-gnuwin32-bindir) :warning "emacsw32=%s" emacsw32)
    ;;(lwarn '(epshell-emacsw32-gnuwin32-bindir) :warning "bin=%s"
    ;;bin)
    bin))

(defun epshell-add-emacs (&optional append)
  "Add Emacs itself to the path of inferior shells."
  (interactive)
  (epshell-add-envpath exec-directory)
  (epshell-add-exec-path exec-directory))
(defun epshell-remove-emacs ()
  "Remove Emacs itself from the path of inferior shells."
  (interactive)
  (epshell-remove-envpath exec-directory)
  (epshell-remove-exec-path exec-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Choosing a w32 shell for Emacs

(defun epshell-delayed-customize (msg symbol)
  (let ((use-dialog-box nil)
        (is-group (get symbol 'custom-group)))
    (when (y-or-n-p (format "%s. Do you want to customize %s now? " msg symbol))
      (if is-group
          (customize-group symbol)
        (customize-option symbol)))))

(defun epshell-verify-bindir (bin-sym shexe)
  "If BIN-SYM value is a sh bin directory name return it.
Otherwise offer the user at idle time to customize it.

Helper for `epshell-set-shell'."
  (let ((standard-value (car (get bin-sym 'standard-value)))
        (bindir (symbol-value bin-sym))
        errmsg exefile)
    (if (equal bindir standard-value)
        (setq errmsg (concat "You must set " (symbol-name bin-sym)))
      (if (file-directory-p bindir)
          (progn
            ;;(setq exefile (concat bindir "/" shexe))
            (setq exefile (expand-file-name shexe (file-name-as-directory bindir)))
            (unless (file-executable-p exefile)
              (setq errmsg (concat "Can't find file " exefile))))
        (setq errmsg (concat "Can't find directory " bindir))))
    (if errmsg
        (let ((use-dialog-box nil))
          (lwarn '(epshell) :warning errmsg)
          (unless (eq major-mode 'custom-mode)
            (with-timeout (6 (progn
                               (lwarn '(epshell) :warning "Ok, please customize epshell later!")
                               (message "Time out, continuing")))
              (epshell-delayed-customize errmsg 'epshell)))
          nil)
      bindir)))

(defvar epshell-current-shell-path nil)

(defcustom epshell-wanted-progs
  '("grep" "find" "xargs" "cmp" "diff" "diff3" "cmp" "patch" "locate")
  "List of programs that are checked for availability.
This list of programs are searched for in your path by
`executable-find' when calling `epshell-set-shell'.  If any of
them is not found a warning is given."
  :type '(repeat string)
  :group 'epshell)

;; Fix-me: write a function that checks consistency against this!
(defun epshell-set-shell (shellname)
  "Set shell to use for inferior shells.
This sets `shell-file-name' and the environment variable SHELL.
It also changes the environment variable PATH and `exec-path'.

Accepted values for SHELLNAME are \"cmd\", \"cygwin\" and
\"msys\".

If SHELLNAME is \"cygwin\" then it calls `cygwin-mount-activate'.

If SHELLNAME is \"cygwin\" or \"msys\" then the corresponding bin
directory is added to path.

This function checks if the programs in `epshell-wanted-progs'
that may be used from the inferior shells are available.  If they
are not a warning will be given. It also checks if 'find' is the
unix style find or not.

Returns non-nil if success."
  (interactive
   (list
    (let* ( (history '("cmd" "msys" "cygwin"))
            (history-length (length history)) )
      (completing-read "Choose shell: " '("cmd" "msys" "cygwin") nil t "cygwin" 'history))))
  ;;(lwarn '(epshell) :warning "Calling epshell-set-shell %s" shellname)
  (let (bin shell)
    (cond (  (equal shellname "cygwin")
             (setq bin (epshell-verify-bindir 'epshell-cygwin-bin "bash.exe"))
             (when bin
               (unless (file-directory-p bin) (error "Can't find directory %s" bin))
               (let ((usr-bin (expand-file-name "../usr/bin" bin))
                     (usr-local-bin (expand-file-name "../usr/local/bin" bin)))
                 (setq bin (expand-file-name "../bin" bin))
                 (setq bin (list bin usr-bin usr-local-bin)))
               ;; in cygwin use "cygpath -a -w /bin" and "echo $PATH" to check.
               ;;
               ;; c:/cygwin/bin shows up in cygwin as /usr/bin,looks
               ;; like a cygwin bug.  I have reported it, but cygwin
               ;; maintainers does not seem to think it is important
               ;; (and I can agree).
               (setq shell "bash")
               (setenv "PS1" "Cygwin \\w > "))
             )
          (  (equal shellname "msys")
             (setq bin (epshell-verify-bindir 'epshell-msys-bin "sh.exe"))
             (when bin
               (setq shell "sh")
               (setenv "PS1" "MSYS \\w > "))
             )
          (  (equal shellname "cmd")
             (setq bin (epshell-emacsw32-gnuwin32-bindir))
             ;;(lwarn '(epshell) :warning "cmd cond, bin=%s" bin)
             (setq shell (expand-file-name "cmdproxy.exe" exec-directory))
             )
          (  t
             (error "Unrecognized shell name: %s" shellname)))
    (when (or bin shell)
      ;; (when bin
      ;;   (unless (listp bin) (setq bin (cons bin nil)))
      ;;   (dolist (b bin)
      ;;     (unless (file-directory-p b)
      ;;       (error "Can't find directory %s" b))))
      (when epshell-current-shell-path
        (epshell-remove-exec-path epshell-current-shell-path)
        (epshell-remove-envpath   epshell-current-shell-path)
        (setq epshell-current-shell-path nil))
      (cond ( (equal shellname "cmd")
              (setq process-coding-system-alist nil)
              (setq w32-process-args nil)
              (remove-hook 'comint-output-filter-functions
                           'comint-strip-ctrl-m)
              )
            ( (or (equal shellname "cygwin") (equal shellname "msys"))
              ;;(setq process-coding-system-alist '((shell-file-name . undecided-unix)))
              ;;(setq process-coding-system-alist (list (cons shell-file-name 'undecided-unix)))
              (setq w32-process-args ?\")
              ;; For Java?:
              (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
              )
            )
      (when shell
        ;;(lwarn '(epshell) :warning "bin=%s, shell-name=%s" bin shellname)
        (when bin
          (unless (listp bin) (setq bin (list bin)))
          (dolist (b bin)
            (epshell-add-exec-path b)
            (epshell-add-envpath b))
          ;;(message "exec-path=%s" exec-path)
          ;;(message "evn PATH=%s" (getenv "PATH"))
          )
        (setq epshell-current-shell-path bin)
        ;; Call cygwin-mount. After an idea by Ismael Valladolid Torres:
        (when (equal shellname "cygwin")
          (when (require 'cygwin-mount nil t)
            (cygwin-mount-activate)))
        (setq shell-file-name shell) ; Single shell
        ;;(setenv "SHELL" shell-file-name) ; Single shell
        ;;(setq explicit-shell-file-name shell-file-name)
        ;; Some sanity checks:
        (epshell-check-wanted-progs)
        ))
    bin))

(defun epshell-get-missing-progs ()
  (let ((missing))
    (dolist (prog epshell-wanted-progs)
      (unless (executable-find prog)
        (add-to-list 'missing prog)))
    missing))

(defun epshell-find-is-unix-find ()
  (let ((find-prog (executable-find "find"))
        (findstr-prog (executable-find "findstr")))
    (not (string= (file-name-directory find-prog)
                  (file-name-directory findstr-prog)))))

(defun epshell-check-wanted-progs ()
  "Checks if `epshell-wanted-progs' are available.
This depends on `epshell'."
  (interactive)
  (dolist (prog (epshell-get-missing-progs))
    (lwarn '(epshell) :warning
           (concat "When using '" shellname "' program '" prog "' can't be found")))
  (unless (epshell-find-is-unix-find)
    (lwarn '(epshell) :warning
           (concat "When using '" shellname "' program 'find'"
                   " will be Windows' find, should be unix' find"))))

(defun epshell-quote-argument (argument)
  "Like `shell-quote-argument' but knows about epshell."
  (unless (eq system-type 'windows-nt) (error "You can only use this on w32"))
  (let ((system-type (if (string= "cmd" shell-file-name)
                         system-type
                       'gnu/linux)))
    (shell-quote-argument argument)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom

(defgroup epshell nil
  "Customization group for epshell"
  :group 'w32)

(defcustom epshell-cygwin-bin (if (getenv "CYGWIN_DIR")
                                  (expand-file-name "bin"
                                                    (getenv "CYGWIN_DIR"))
                                "")
  "Path to Cygwin bin directory"
  :type 'directory
  :group 'epshell)

(defcustom epshell-msys-bin (if (and (getenv "MSYS")
                                     (file-exists-p (expand-file-name "bin"(expand-file-name "1.0" (expand-file-name "msys" (getenv "MSYS"))))))
                                (expand-file-name "bin"(expand-file-name "1.0" (expand-file-name "msys" (getenv "MSYS"))))
                              "")
  "Path to MSYS bin directory"
  :type 'directory
  :group 'epshell)

(defvar epshell-old nil)
(defcustom epshell-shell nil
  "Shell to use for `shell' command.
Value should be 'cmd, 'cygwin or 'msys.  If it is cygwin or msys
those utilities bin path are put first in path.

Setting is done with `epshell-set-shell'."
  ;; Make sure emacsw32 is loaded:
  ;;:set-after '(emacsw32-style-frame-title)
  :type '(choice
          (const :tag "(unset)" nil)
          (const :tag "Windows cmd.exe - uses unix progs from EmacsPortable.App" cmd)
          (const :tag "Cygwin" cygwin)
          (const :tag "MSYS" msys)
          )
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (unless (eq epshell-old value)
             (setq epshell-old value)
             (epshell-set-shell (format "%s" value))
             ))
         t)
  :group 'epshell)

(defcustom epshell-add-emacs-to-path t
  "Add Emacs bin directory to path when non-nil."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (unless (epshell-in-exec-path exec-directory)
               (epshell-add-emacs))
           (epshell-remove-emacs)))
  :group 'epshell)

(defvar epshell-with-shell-internal nil)

(defmacro epshell-with-shell (use-shell &rest body)
  "Execute the BODY forms with shell temporary set to USE-SHELL."
  (declare (indent 0) (debug t))
  `(let ((shell-file-name)
         (exec-path exec-path)
         (envpath (getenv "PATH")))
     (condition-case err
         (when (epshell-set-shell ,use-shell)
           ,@body)
       (error (message "%s" (error-message-string err))))
     (setenv "PATH" envpath)))


(defun cygwin-shell ()
  "Run `shell' with Cygwin as the shell.
Does not affect the setting of `epshell-shell' but otherwise
works as if you had set this to 'cygwin.

See also `msys-shell' and `cmd-shell'."
  "Run `shell' with MSYS as the shell.
Is otherwise similar to `cygwin-shell'."
  (interactive)
  (epshell-with-shell "cygwin" (shell "*cygwin shell*")))

(defun msys-shell ()
  "Run `shell' with MSYS as the shell.
Is otherwise similar to `cygwin-shell'."
  (interactive)
  (epshell-with-shell "msys" (shell "*msys shell*")))

(defun cmd-shell ()
  "Run `shell' with Windows Command Prompt as the shell.
File name completion with Tab/Shift-Tab is done in the style that
Windows Command Prompt does it.

Is otherwise similar to `cygwin-shell'."
  (interactive)
  (epshell-with-shell
    "cmd"
    (progn
      (shell "*cmd shell*")
      ;; fix-me: Temporary, until removed from viper
      ;; (when (and (boundp 'viper-insert-basic-map)
      ;;            (keymapp viper-insert-basic-map))
      ;;   (define-key viper-insert-basic-map
      ;;     (if viper-xemacs-p [(shift tab)] [S-tab]) nil))
      (local-set-key [tab] 'epshell-dynamic-complete-filename-like-cmd-fw)
      (local-set-key [(shift tab)]
                     'epshell-dynamic-complete-filename-like-cmd-bw))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun epshell-dynamic-complete-filename-like-cmd-fw ()
  "Tab style file name completion like cmd.exe.
Tries to do Tab style file name completion like cmd.exe on w32
does it.

See also `epshell-dynamic-complete-filename-like-cmd-bw'."
  (interactive)
  (epshell-dynamic-complete-filename-like-cmd t))

(defun epshell-dynamic-complete-filename-like-cmd-bw ()
  "Shift-Tab style file name completion like cmd.exe.
Tries to do Shift-Tab style file name completion like cmd.exe on
w32 does it.

See also `epshell-dynamic-complete-filename-like-cmd-fw'."
  (interactive)
  (epshell-dynamic-complete-filename-like-cmd nil))

(defconst epshell-dynamic-complete-state nil)

(defcustom epshell-dynamic-complete-sync-dirs t
  "Synchronize process directory and `default-directory' if non-nil.
If non-nil then `epshell-dynamic-complete-filename-like-cmd-fw' (and
dito -bw) will send an invisible \"cd\" to the process running
cmd.exe to find out what directory the cmd.exe process
uses. `default-directory' is then set to this directory."
  :type 'boolean
  :group 'epshell)

(defcustom epshell-dynamic-complete-only-dirs '("cd" "pushd")
  "Commands for which only directories should be shown.
When doing file name completion the commands in this list will
only get directory names.

This is used in `epshell-dynamic-complete-filename-like-cmd-fw' (and
dito -bw)."
  :type '(repeat string)
  :group 'epshell)

(defun epshell-dynamic-complete-filename-like-cmd (forward)
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (process-mark proc))
         (point (point))
         (cmdstr (buffer-substring-no-properties pmark point))
         (argv (epshell-get-argv cmdstr))
         (first-arg (car argv))
         (last-arg (car (reverse argv)))
         (only-dirs (member (car first-arg) epshell-dynamic-complete-only-dirs))
         (prev-cmdstr          (nth 0 epshell-dynamic-complete-state))
         (prev-completion      (nth 1 epshell-dynamic-complete-state))
         (prev-begin-filename  (nth 2 epshell-dynamic-complete-state))
         (in-completion (string= cmdstr prev-cmdstr))
         (begin-filename prev-begin-filename)
         new-completion
         new-full-completion
         completion-dir
         completion-dir-given
         dir-files
         pick-next
         beginning-last
         )
    ;; Initialize
    (setq epshell-dynamic-complete-state nil)
    (when last-arg
      (setq completion-dir-given (file-name-directory (car last-arg))))
    (if completion-dir-given
        (setq completion-dir-given
              (file-name-as-directory completion-dir-given))
      (setq completion-dir-given ""))
    ;; Not continuing completion set up for completion
    (unless in-completion
      (setq prev-completion nil)
      (if last-arg
          (setq begin-filename
                (concat "^" (file-name-nondirectory (car last-arg))))
        (setq begin-filename nil))
      ;; Sync process directory and default-directory
      (when epshell-dynamic-complete-sync-dirs
        (let ((old-out-filter (process-filter proc)))
          (condition-case err
              (progn
                (set-process-filter
                 proc
                 (lambda (proc str)
                   (let ((lstr (split-string str "[\r\n]+")))
                     (setq default-directory
                           (file-name-as-directory (nth 1 lstr))))))
                (process-send-string proc "cd\n")
                (accept-process-output proc))
            (error (message "%s" (error-message-string err))))
          (set-process-filter proc old-out-filter))))
    ;; Find completion
    (setq completion-dir (expand-file-name completion-dir-given))
    (setq dir-files (directory-files completion-dir nil begin-filename))
    (unless forward
      (setq dir-files (reverse dir-files)))
    (dolist (f dir-files)
      (when (and (not (member f '("." "..")))
                 (or (not only-dirs)
                     (file-directory-p (expand-file-name f completion-dir))))
        (unless new-completion
          (setq new-completion f))
        (if (string= f prev-completion)
            (setq pick-next t)
          (when pick-next
            (setq pick-next nil)
            (setq new-completion f)))))
    (setq new-full-completion
          (convert-standard-filename
           (concat completion-dir-given new-completion)))
    ;; Replase last argument
    (setq beginning-last (nth 1 last-arg))
    (unless beginning-last
      (setq beginning-last 0))
    (goto-char (+ pmark beginning-last))
    (unless (eolp) (kill-line))
    ;; The code below should probably use shell-quote-argument, but
    ;; because of trouble with this function I am using a more
    ;; w32 specific quoting here at the moment.
    (if (not (memq ?\  (append new-full-completion nil)))
        (insert new-full-completion)
      (insert ?\")
      (insert new-full-completion)
      (insert ?\"))
    ;; Save completion state
    ;;
    ;; return non-nil to show completion has been done!
    (setq epshell-dynamic-complete-state
          (list
           (buffer-substring-no-properties pmark (point))
           new-completion
           begin-filename))))

(defun epshell-get-argv (cmdline)
  "Split CMDLINE into args.
The splitting is done using the syntax used on MS Windows.

Return a list where each element is a list in the form

  \(arg arg-begin arg-end)

where ARG is the argument stripped from any \". ARG-BEGIN and
ARG-END are the beginning and end of the argument in cmdline.

If CMDLINE ends with a space or is \"\" a list consisting of
\(\"\" LEN nil) is added. LEN is the length of CMDLINE."
  (let ((lcmd (append cmdline nil))
        (len (length cmdline))
        argv
        state
        arg
        arg-begin
        arg-end
        c
        )
    (while lcmd
      (setq c (car lcmd))
      (setq lcmd (cdr lcmd))
      (cond
       (  (not state)
          (when arg (error "arg not nil"))
          (cond
           ( (= c ?\ ))
           ( (= c ?\")
             (setq arg-begin (- len 1 (length lcmd)))
             (setq state 'state-qarg))
           ( t
             (setq arg-begin (- len 1 (length lcmd)))
             (setq state 'state-arg)
             (setq arg (cons c arg)))))
       (  (eq state 'state-arg)
          (cond
           ( (= c ?\ )
             (setq state nil)
             (setq arg-end (- len 1 (length lcmd)))
             (setq argv (cons
                         (list (concat (nreverse arg))
                               arg-begin
                               arg-end)
                         argv))
             (setq arg nil))
           ( (= c ?\")
             (setq state 'state-arg-q))
           ( t
             (setq arg (cons c arg)))))
       (  (eq state 'state-arg-q)
          (cond
           ( (= c ?\")
             (setq state 'state-arg))
           ( t
             (setq arg (cons c arg)))))
       (  (eq state 'state-qarg)
          (cond
           ( (= c ?\")
             (setq state 'state-qarg-q))
           ( t
             (setq arg (cons c arg)))))
       (  (eq state 'state-qarg-q)
          (cond
           ( (= c ?\ )
             (setq state nil)
             (setq arg-end (- len 1 (length lcmd)))
             (setq argv (cons
                         (list (concat (nreverse arg))
                               arg-begin
                               arg-end)
                         argv))
             (setq arg nil))
           ( (= c ?\")
             (setq arg (cons c arg))
             (setq state 'state-qarg))
           ( t
             (setq arg (cons c arg)))))
       (  t
          (error "unknown state=%s" state))
       ))
    (if arg
        (progn
          (setq arg-end (- len 0 (length lcmd)))
          (setq argv (cons
                      (list
                       (concat (nreverse arg))
                       arg-begin
                       arg-end)
                      argv)))
      (when (or (not c) (= c ?\ ))
        (setq argv (cons (list "" (length cmdstr) nil) argv))))
    (reverse argv)))

;; For testing:
(when nil
  (global-set-key [f9]         'epshell-dynamic-complete-filename-like-cmd-fw)
  (global-set-key [(shift f9)] 'epshell-dynamic-complete-filename-like-cmd-bw)
  )

(when nil
  (let* ((cmd "cd \\\"hej\"\\du \"sista\"")
         (argv (epshell-get-argv cmd)))
    (dolist (a argv)
      (message "%s %s %s (%s)"
               (nth 0 a)
               (nth 1 a)
               (nth 2 a)
               (substring cmd (nth 1 a) (nth 2 a)))))
  )

;;(w32-shell-execute nil (concat (getenv "SystemRoot") "\\explorer.exe") "/n,/select,c:\\test\\temp.htm")
(defun epshell-explorer-file (file)
  "Open Windows Explorer with file FILE selected."
  (interactive "fFile to focus in Explorer: ")
  (let ((full-file (expand-file-name file)))
    (setq full-file (replace-regexp-in-string "/" "\\" full-file t t))
    (w32-shell-execute nil (concat (getenv "SystemRoot") "\\explorer.exe")
                       (concat "/n,/select," full-file))))

(defun epshell-explorer-current-file ()
  "Open Windows Explorer with current file selected."
  (interactive)
  (if buffer-file-name
      (epshell-explorer-file buffer-file-name)
    (message "Buffer has no file name")))

(defun epshell-explorer-old (dir)
  "Open Windows Explorer in directory DIR.
For some reason with this function Explorer does not get
focus. Use the new version instead."
  (interactive "DStart in directory: ")
  (setq dir (expand-file-name dir))
  (w32-shell-execute nil dir))

(defun epshell-explorer (dir)
  "Open Windows Explorer in directory DIR."
  (interactive "DStart in directory: ")
  (setq dir (expand-file-name dir))
  ;;(setq dir (directory-file-name dir))
  (message "dir=%s" dir) (sit-for 2)
  (w32-shell-execute
   "explore" ;;nil
   "" ;(concat (getenv "SystemRoot") "\\explorer.exe")
   (concat "/n," dir)
   ))

(defun epshell-explorer-here ()
  "Open Windows Explorer in current directory."
  (interactive)
  (epshell-explorer default-directory))

(defun epshell-cmd (dir)
  "Open a Windows command prompt in directory DIR.
Emacs bin dir is added to path in the started command window."
  (interactive "DStart in directory: ")
  (let ((default-directory (expand-file-name dir))
        (old-emacs-dir (getenv "emacs_dir"))
        (old-path (getenv "PATH")))
    (setenv "emacs_dir"
            (save-match-data
              (replace-regexp-in-string "/" "\\" old-emacs-dir t t)))
    (epshell-add-envpath exec-directory)
    (unwind-protect
        (condition-case err
            (progn
              ;;(call-process "cmd.exe" nil 0 nil "/c" "start" (concat '(?\") "hej4" '(?\")) "cmd.exe")
              ;; Bug in call-process quoting, use this instead this:
              ;;(w32-shell-execute nil "cmd.exe" "/c start \"Command Prompt with Emacs in PATH\"")
              ;; Nope, that will not give the correct path ... - turn off quoting is spawnve instead:
              (let ((w32-quote-process-args nil))
                (call-process "cmd.exe" nil 0 nil "/c" "start"
                              (concat '(?\") "Command Prompt with Emacs in PATH" '(?\")) "cmd.exe")))
          (error (message "%s" (error-message-string err))))
      (setenv "emacs_dir" old-emacs-dir)
      (setenv "PATH" old-path))))

(defun epshell-cmd-here ()
  "Open a Windows command prompt in current directory.
Emacs bin dir is added to path in the started command window."
  (interactive)
  (epshell-cmd default-directory))

(provide 'epshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; epshell.el ends here
