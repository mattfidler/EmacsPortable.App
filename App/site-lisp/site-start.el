(defvar usb-app-dir (if (getenv "EPOTHER")
                        (expand-file-name (concat (getenv "EPOTHER") "/../App/"))
                      (let ((ret (or buffer-file-name load-file-name)))
                        (when (string-match "EmacsPortable[.]App.*" ret)
                          (setq ret (replace-match "EmacsPortable.App/App/" nil nil ret)))
                        (symbol-value 'ret))))

(defun ep-byte-compile-dest-file-function (file)
  (let ((f (expand-file-name file)))
    (cond
     ((string-match (regexp-quote (expand-file-name (getenv "emacs_dir"))) f)
      (concat file "c"))
     (t
      (format "%s%s%s.%s" file "c-" emacs-major-version emacs-minor-version )))))

;;(setq byte-compile-dest-file-function 'ep-byte-compile-dest-file-function)

(defun refresh-proxy (&optional process event sync)
  "Refreshes proxy settings"
  (interactive)
  (when (eq system-type 'windows-nt)
    (let ((el-proxy (expand-file-name (concat usb-app-dir "../App/eps/ep-proxy.exe")))
          (delete-by-moving-to-trash nil)
          (proxy (expand-file-name (concat (getenv "TEMP") "/proxy.el")))
          (proc process))
      (when process
        (message "Proxy settings were retrieved (%s)." event))
      (if (file-exists-p proxy)
          (progn
            (load-file proxy)
            (delete-file proxy))
        (when (and (eq system-type 'windows-nt) (file-exists-p el-proxy)
                   (not proc))
          (message "Starting to lookup proxy information (%s)" el-proxy)
          (if (not sync)
              (progn
                (setq proc (start-process "ep-refresh-proxy" " ep-refresh-proxy"
                                          el-proxy))
                (set-process-sentinel proc 'refresh-proxy))
            (shell-command-to-string el-proxy))
          
          (when (file-exists-p proxy)
            (load-file proxy)
            (delete-file proxy)
            (message "Proxy settings were retrieved.")
            (setq process t))))
      ;; Setup Git
      (if (and process
               (executable-find "git")
               (getenv "HTTP_PROXY"))
          (progn
            (shell-command-to-string (concat "git config --global http.proxy "
                                             (getenv "HTTP_PROXY"))))
        (if (and process
                 (executable-find "git"))
            (shell-command-to-string (concat "git config --global --unset http.proxy")))))))
(refresh-proxy nil nil t)

;; (when (eq emacs-major-version 24)
;;   ;; Add auto-compile functions.
;;   (require 'package)
;;   (package-initialize)
;;   (let ((package-user-dir
;;          (expand-file-name "elpa"
;;                            (file-name-directory
;;                             (or load-file-name (buffer-file-name))))))
;;     (unless (package-installed-p 'auto-compile)
;;       (setq package-archives
;;             '(("original"    . "http://tromey.com/elpa/")
;;               ("gnu"         . "http://elpa.gnu.org/packages/")
;;               ("marmalade"   . "http://marmalade-repo.org/packages/")
;;               ("melpa"       . "http://melpa.milkbox.net/packages/")))
;;       (setq ep-proxy-refresh t)
;;       (package-initialize)
;;       (package-refresh-contents)
;;       (package-install 'auto-compile)))
;;   (auto-compile-on-save-mode)
;;   (auto-compile-on-load-mode))


;; Load Latest Org-mode
(require 'cl)
;; from www.emacswiki.org/emacs/LoadPath
(let ((default-directory
        (expand-file-name "../../Data/lisp/src/"
                          (file-name-directory (or load-file-name (buffer-file-name))))))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (normal-top-level-add-subdirs-to-load-path))
         load-path)))

(let ((default-directory
        (expand-file-name "../../Data/src/"
                          (file-name-directory (or load-file-name (buffer-file-name))))))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (normal-top-level-add-subdirs-to-load-path))
         load-path)))

;; (require 'org nil t)


(defun ep-load-org (file)
  "Loads Emacs Lisp source code blocks like `org-babel-load-file'.  However, byte-compiles the files as well as tangles them..."
  (flet ((age (file)
              (float-time
               (time-subtract (current-time)
                              (nth 5 (or (file-attributes (file-truename file))
                                         (file-attributes file)))))))
    (let* ((base-name (file-name-sans-extension file))
           (exported-file (concat base-name ".el")))
      (unless (and (file-exists-p exported-file)
                   (> (age file) (age exported-file)))
        (message "Trying to Tangle %s" file)
        (condition-case err
            (progn
	      (when (fboundp 'package-initialize)
                (package-initialize))
	      (require 'org)
              (org-babel-tangle-file file exported-file "emacs-lisp")
              (message (format "Tangled %s to %s"
                                     file exported-file)))
          (error (message "Error Tangling %s; %s" file err))))
      (when (file-exists-p exported-file)
        (load exported-file)))))

(ep-load-org (expand-file-name 
              "start.org"
              (file-name-directory (or load-file-name (buffer-file-name)))))
