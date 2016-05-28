(require 'ansi-color)


(defun compile++-colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only)
  )

(defun compile++-arrange-windows ()
  (let* ((exists  (member "*compilation*" (mapcar 'buffer-name (mapcar 'window-buffer (window-list))))))
    (if (not exists)
        (progn
          (delete-other-windows)
          (split-window-vertically)
          (split-window-horizontally)
          (windmove-down)
          (switch-to-buffer "*compilation*")
          (set-window-text-height (selected-window) compile++-buffer-height)
          (set-window-dedicated-p (selected-window) t)
          (set-window-point       (selected-window) (point-max))
          (windmove-up)
          ))
    )
  )

(defun compile++-get-nearest-filename (filename)
  (setq origin (buffer-file-name))
  (if (not origin)
      (setq origin compile++-last-buffer))
  (setq compile++-last-buffer origin)
  (let* ((dir (file-name-directory origin))
         (dirs (split-string dir "/"))
         (result nil))
    (while (and (> (length dirs) 1) (equal nil result))
      (if (file-exists-p (concat (mapconcat 'identity dirs "/") "/" filename))
          (setq result (concat (mapconcat 'identity dirs "/") "/" filename))
        (nbutlast dirs 1))
      )
    result)
  )

(defun compile++-safe-kill ()
  (interactive)
  (if (equal (get-buffer "*compilation*") nil)
      nil
    (kill-buffer "*compilation*"))
  )

(defun compile++-previous-error ()
  (interactive)
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (setq compilation-skip-threshold 2)
  (previous-error)
  )

(defun compile++-next-error ()
  (interactive)
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (setq compilation-skip-threshold 2)
  (next-error)
  )

(defun compile++-previous-warning ()
  (interactive)
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (setq compilation-skip-threshold 1)
  (previous-error)
  )

(defun compile++-next-warning ()
  (interactive)
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (setq compilation-skip-threshold 1)
  (next-error)
  )

(defun compile++-get-dir-locals-directory ()
  (car (dir-locals-find-file (buffer-file-name))))

(defun compile++-get-dir-git ()
  (file-name-directory (compile++-get-nearest-filename ".git")))

(defun compile++-guess-directory ()
  (let* ((makefile   (compile++-get-nearest-filename "Makefile.am"))
         (builddir   (compile++-get-nearest-filename ".release")))
    (if (or (equal makefile nil) (equal builddir nil))
        nil
      (let*
          ((moduledir  (file-name-directory makefile))
           (rootdir    (file-name-directory builddir))
           (subtarget  (substring moduledir (length rootdir)))
           (compiledir (concat builddir "/" subtarget)))
        (file-truename compiledir))))
  )

;; --------------------------------------------------------------

(defvar compile++-last-buffer       nil nil)
(defcustom compile++-buffer-height  13     "Command to run to start compilation."                           :group 'compile++ :type 'integer)
(defcustom compile++-scroll-output  t      "Should we scroll compilation buffer while compiling ?"          :group 'compile++ :type 'boolean)
(defcustom compile++-buffer-local   nil    "Set compile/deploy and test paramters buffer local."            :group 'compile++ :type 'boolean :safe (lambda(val) t))


(defun compile++-run (prompt type)
  (compile++-arrange-windows)
  (let* ((config         (cdr (assoc type compile++-config-alist)))
         (get-params     (cdr (assoc "get-params" config)))
         (command        (cdr (assoc "command"    config))))
    (if prompt
        (funcall get-params))
    (compile (funcall-or-value command) t))
  )

(defun compile++-docker-params (type)
  (compile++-default-params type)
  (let* ((config (cdr (assoc type compile++-config-alist)))
         (service (cdr (assoc "service" config))))
    (setcdr (assoc "service" config) (read-from-minibuffer "Service: " (funcall-or-value service))))
  )

(defun compile++-docker-run-command (type &optional cmd)
  (let* ((config (cdr (assoc type compile++-config-alist)))
         (dir     (cdr (assoc "dir"     config)))
         (env     (cdr (assoc "env"     config)))
         (bin     (cdr (assoc "bin"     config)))
         (service (cdr (assoc "service" config)))
         (dockerenv (if (string= env "")
                        env
                      (mapconcat 'identity (mapcar (lambda (el) (concat "-e " el)) (split-string env " ")) " "))))

    (if (not cmd)
        (setq cmd "run"))

    (format "cd %s && SRCDIR=%s docker-compose -f extra/docker-compose.yml %s %s %s %s"
            (funcall-or-value dir)
            (funcall-or-value dir)
            cmd
            dockerenv
            (funcall-or-value service)
            (funcall-or-value bin)))
  )

(defun compile++-docker-exec-command (type)
  (let* ((config (cdr (assoc type compile++-config-alist)))
         (dir     (cdr (assoc "dir"     config)))
         (bin     (cdr (assoc "bin"     config)))
         (service (cdr (assoc "service" config))))
    (format "cd %s && SRCDIR=%s docker-compose -f extra/docker-compose.yml exec %s %s"
            (funcall-or-value dir)
            (funcall-or-value dir)
            (funcall-or-value service)
            (funcall-or-value bin)))
  )

(defun compile++-default-params (type)
  (let* ((config (cdr (assoc type compile++-config-alist)))
         (dir (cdr (assoc "dir" config)))
         (env (cdr (assoc "env" config)))
         (bin (cdr (assoc "bin" config))))
    (setcdr (assoc "dir" config) (read-directory-name  "Directory: "   (funcall-or-value dir)))
    (setcdr (assoc "env" config) (read-from-minibuffer "Environment: " (funcall-or-value env)))
    (setcdr (assoc "bin" config) (read-from-minibuffer "Binary: "      (funcall-or-value bin))))
  )

(defun compile++-default-command (type)
  (let* ((config (cdr (assoc type compile++-config-alist)))
         (dir (cdr (assoc "dir" config)))
         (env (cdr (assoc "env" config)))
         (bin (cdr (assoc "bin" config))))
    (format "cd %s && %s %s"
            (funcall-or-value dir)
            (funcall-or-value env)
            (funcall-or-value bin)))
  )

(defcustom compile++-config-alist
  '(("compile" .
     (("dir"        . compile++-guess-directory)
      ("env"        . "")
      ("bin"        . "make -j")
      ("get-params" . (lambda() (compile++-default-params  "compile")))
      ("command"    . (lambda() (compile++-default-command "compile")))))
    ("test" .
     '(("dir"        . compile++-guess-directory)
       ("env"        . "")
       ("bin"        . "make -j")
       ("get-params" . (lambda() (compile++-default-params  "test")))
       ("command"    . (lambda() (compile++-default-command "test")))))
    ("deploy" .
     '(("dir"        . compile++-guess-directory)
       ("env"        . "")
       ("bin"        . "make -j")
       ("get-params" . (lambda() (compile++-default-params  "deploy")))
       ("command"    . (lambda() (compile++-default-command "deploy")))))
    )
  "Compile++ callback configuration"
  :group 'compile++
  :safe '(lambda(p) t)
  :type '(alist :key 'string :value '(alias :key string :value '(choice (string) (function))))
  )


;; --------------------------------------------------------------------------

(defun compile++-mode-construct()
  (add-hook 'compilation-filter-hook 'compile++-colorize-compilation-buffer)
  (if compile++-buffer-local
      (make-local-variable 'compile++-config-alist))
  (message "enabled : compile++-mode")
  )

(defun compile++-mode-destroy()
  (if compile++-buffer-local
      (kill-local-variable compile++-config-alist))
  (remove-hook 'compilation-filter-hook 'compile++-colorize-compilation-buffer)
  (message "disabled : compile++-mode")
  )


;;;###autoload
(define-minor-mode compile++-mode
  "Set of function beyond compilation-mode" nil " Compile++"
  '(

    ([f6]                . (lambda () (interactive) (compile++-run nil "compile")))
    ([C-f6]              . (lambda () (interactive) (compile++-run t   "compile")))
    ([M-f6]              . kill-compilation)

    ([C-S-f6]            . (lambda () (interactive) (compile++-run t   "doc")))

    ([C-e]               . code-cpp-rename-variable)

    ([f7]                . (lambda () (interactive) (compile++-run nil "test")))
    ([C-f7]              . (lambda () (interactive) (compile++-run t   "test")))
    ([M-f7]              . kill-compilation)

    ([f8]                . (lambda () (interactive) (compile++-run nil "deploy")))
    ([C-f8]              . (lambda () (interactive) (compile++-run t   "deploy")))
    ([M-f8]              . kill-compilation)

    ([f9]                . compile++-next-error)
    ([C-f9]              . compile++-next-warning)
    )

  (if compile++-mode
      (compile++-mode-construct)
    (compile++-mode-destroy))
  )

;; --------------------------------------------------------------------------

;;;###autoload
(put 'compile++-buffer-height 'safe-local-variable 'integerp)
;;;###autoload
(put 'compile++-scroll-output 'safe-local-variable 'booleanp)
;;;###autoload
(put 'compile++-buffer-local 'safe-local-variable 'booleanp)
;;;###autoload
(put 'compile++-config-alist 'safe-local-variable '(lambda(p) t))

(provide 'compile++)

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
