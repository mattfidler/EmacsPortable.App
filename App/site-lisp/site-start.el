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



(require 'org-install nil t)
(require 'org nil t) 

(defun ep-load-org (file)
  "Loads Emacs Lisp source code blocks like `org-babel-load-file'.  However, byte-compiles the files as well as tangles them..."
  (flet ((age (file)
              (float-time
               (time-subtract (current-time)
                              (nth 5 (or (file-attributes (file-truename file))
                                         (file-attributes file)))))))
    (let* ((base-name (file-name-sans-extension file))
           (exported-file (concat base-name ".el"))
           (compiled-file (concat base-name ".elc")))
      (unless (and (file-exists-p exported-file)
                   (> (age file) (age exported-file)))
        (message "Trying to Tangle %s" file)
        (condition-case err
            (progn
              (org-babel-tangle-file file exported-file "emacs-lisp")
              (message (format "Tangled %s to %s"
                                     file exported-file)))
          (error (message "Error Tangling %s; %s" file err))))
      (when (file-exists-p exported-file)
        (if (and (file-exists-p compiled-file)
                 (> (age exported-file) (age compiled-file)))
            (progn
              (condition-case err
                  (load-file compiled-file)
                (error (message "Error Loading %s" compiled-file)))
              (message (format "Loaded %s" compiled-file)))
          (condition-case err
              (byte-compile-file exported-file t)
            (error (message "Error Byte-compiling and loading %s" exported-file)))
          (message (format "Byte-compiled & loaded %s" exported-file))
          ;; Fallback and load source
          (if (file-exists-p compiled-file)
              (set-file-times compiled-file) ; Touch file.
            (condition-case err
                (load-file exported-file)
              (error (message "Error loading %s" exported-file)))
            (message (format "Loaded %s since byte-compile failed."
                                   exported-file))))))))

(ep-load-org (expand-file-name "start.org"
                               (file-name-directory (or load-file-name (buffer-file-name)))))
