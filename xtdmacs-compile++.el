;;(require 'ansi-color)
(require 'xterm-color)
(eval-when-compile (require 'subr-x))

(defface xtdmacs-compile++-compiling-face
  '((t (:background "green")))
  "Overriding face of mode-line and mode-line-inactive when compilation is running"
  :group 'xtdmacs-compile++
  )

(defface xtdmacs-compile++-error-face
  '((t (:background "blue")))
  "Overriding face of buffer when compilation exited in error"
  :group 'xtdmacs-compile++
  )

(defcustom xtdmacs-compile++-buffer-height  13     "Command to run to start compilation."                           :group 'xtdmacs-compile++ :type 'integer)
(defcustom xtdmacs-compile++-scroll-output  t      "Should we scroll compilation buffer while compiling ?"          :group 'xtdmacs-compile++ :type 'boolean)
(defcustom xtdmacs-compile++-buffer-local   nil    "Set compile/deploy and test paramters buffer local."            :group 'xtdmacs-compile++ :type 'boolean :safe (lambda(val) t))

(defcustom xtdmacs-compile++-iwyu-build-directory-name
  ".release"
  "Standard build directory name"
  :group 'xtdmacs-compile++
  :type 'string
  :safe 'stringp
  )

(defcustom xtdmacs-compile++-config-alist
  '(("compile" .
     (("dir"        . xtdmacs-compile++-guess-directory)
      ("env"        . "")
      ("bin"        . "make -j")
      ("get-params" . (lambda() (xtdmacs-compile++-default-params  "compile")))
      ("command"    . (lambda() (xtdmacs-compile++-default-command "compile")))))
    ("test" .
     (("dir"        . xtdmacs-compile++-guess-directory)
      ("env"        . "")
      ("bin"        . "make -j")
      ("get-params" . (lambda() (xtdmacs-compile++-default-params  "test")))
      ("command"    . (lambda() (xtdmacs-compile++-default-command "test")))))
    ("deploy" .
     (("dir"        . xtdmacs-compile++-guess-directory)
      ("env"        . "")
      ("bin"        . "make -j")
      ("get-params" . (lambda() (xtdmacs-compile++-default-params  "deploy")))
      ("command"    . (lambda() (xtdmacs-compile++-default-command "deploy")))))
    ("syntax" .
     (("dir"        . xtdmacs-compile++-guess-directory)
      ("env"        . "")
      ("bin"        . "make -j")
      ("get-params" . (lambda() (xtdmacs-compile++-default-params  "syntax")))
      ("command"    . (lambda() (xtdmacs-compile++-default-command "syntax")))))
    )
  "xtdmacs-compile++ callback configuration"
  :group 'xtdmacs-compile++
  :safe '(lambda(p) t)
  :type '(alist :key 'string :value '(alias :key string :value '(choice (string) (function))))
  )

(defcustom xtdmacs-compile++-command-1
  "compile"
  "Set the key to use in xtdmacs-compile++-config-alist for command 1"
  :group 'xtdmacs-compile++
  :type '(choice (const "compile")
                 (const "test")
                 (const "deploy")
                 (const "doc")
                 (const "syntax")
                 (other :tag "Other" ""))
  :safe 'stringp)


(defcustom xtdmacs-compile++-command-2
  "test"
  "Set the key to use in xtdmacs-compile++-config-alist for command 2"
  :group 'xtdmacs-compile++
  :type '(choice (const "compile")
                 (const "test")
                 (const "deploy")
                 (const "doc")
                 (const "syntax")
                 (other :tag "Other" ""))
  :safe 'stringp)


(defcustom xtdmacs-compile++-command-3
  "deploy"
  "Set the key to use in xtdmacs-compile++-config-alist for command 3"
  :group 'xtdmacs-compile++
  :type '(choice (const "compile")
                 (const "test")
                 (const "deploy")
                 (const "doc")
                 (const "syntax")
                 (other :tag "Other" ""))
  :safe 'stringp)


(defcustom xtdmacs-compile++-command-4
  "deploy"
  "Set the key to use in xtdmacs-compile++-config-alist for command 4"
  :group 'xtdmacs-compile++
  :type '(choice (const "compile")
                 (const "test")
                 (const "deploy")
                 (const "doc")
                 (const "syntax")
                 (other :tag "Other" ""))
  :safe 'stringp)


(defcustom xtdmacs-compile++-command-5
  "syntax"
  "Set the key to use in xtdmacs-compile++-config-alist for command 5"
  :group 'xtdmacs-compile++
  :type '(choice (const "compile")
                 (const "test")
                 (const "deploy")
                 (const "doc")
                 (const "syntax")
                 (other :tag "Other" ""))
  :safe 'stringp)


(defun xtdmacs-compile++-get-current-branch ()
  (let* ((target-dir (file-name-directory (buffer-file-name)))
         (cmd        (format "cd %s && git rev-parse --abbrev-ref HEAD" target-dir))
         (raw-branch (shell-command-to-string cmd))
         (branch     (string-trim raw-branch)))
    branch))

(defun --xtdmacs-compile++-get-value (type key)
  (let* ((config (cdr (assoc type xtdmacs-compile++-config-alist)))
         (value  (cdr (assoc key config))))
    value
    )
  )

(defun --xtdmacs-compile++-set-value (type key value &optional global)
  (let* ((target (if global
                     xtdmacs-compile++-config-alist
                   (copy-tree xtdmacs-compile++-config-alist)))
         (config (cdr (assoc type target))))
    (setcdr (assoc key config) value)
    (unless global
      (setq xtdmacs-compile++-config-alist target))
    )
  )

(defun --xtdmacs-compile++-promt-value (type key label)
  (let* ((value (--xtdmacs-compile++-get-value type key)))
    (read-from-minibuffer (format "%s : " label) (funcall-or-value value))
    )
  )

(defun xtdmacs-compile++-colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode)
  )

(defun xtdmacs-compile++-arrange-windows ()
  (let* ((exists  (member "*compilation*" (mapcar 'buffer-name (mapcar 'window-buffer (window-list))))))
    (if (not exists)
        (progn
          (delete-other-windows)
          (split-window-vertically)
          (split-window-horizontally)
          (windmove-down)
          (switch-to-buffer "*compilation*")
          (set-window-text-height (selected-window) xtdmacs-compile++-buffer-height)
          (set-window-dedicated-p (selected-window) t)
          (set-window-point       (selected-window) (point-max))
          (windmove-up)
          ))
    )
  )

(defun xtdmacs-compile++-get-nearest-filename (filename)
  (let* ((origin (buffer-file-name))
         (dir (file-name-directory origin))
         (dirs (split-string dir "/"))
         (result nil))
    (while (and (> (length dirs) 1) (equal nil result))
      (if (file-exists-p (concat (mapconcat 'identity dirs "/") "/" filename))
          (setq result (concat (mapconcat 'identity dirs "/") "/" filename))
        (nbutlast dirs 1))
      )
    result)
  )

(defun xtdmacs-compile++-previous-error ()
  (interactive)
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (setq compilation-skip-threshold 2)
  (previous-error)
  )

(defun xtdmacs-compile++-next-error ()
  (interactive)
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (setq compilation-skip-threshold 2)
  (next-error)
  )

(defun xtdmacs-compile++-previous-warning ()
  (interactive)
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (setq compilation-skip-threshold 1)
  (previous-error)
  )

(defun xtdmacs-compile++-next-warning ()
  (interactive)
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (setq compilation-skip-threshold 1)
  (next-error)
  )

(defun xtdmacs-compile++-get-dir-locals-directory ()
  (car (dir-locals-find-file (buffer-file-name))))

(defun xtdmacs-compile++-get-dir-git ()
  (file-name-directory (xtdmacs-compile++-get-nearest-filename ".git")))

(defun xtdmacs-compile++-guess-directory ()
  (let* ((makefile   (xtdmacs-compile++-get-nearest-filename "CMakeLists.txt"))
         (builddir   (xtdmacs-compile++-get-nearest-filename ".release")))
    (if (or (equal makefile nil) (equal builddir nil))
        (if buffer-file-name
            (file-name-directory (file-truename buffer-file-name))
          default-directory)
      (let*
          ((moduledir  (file-name-directory makefile))
           (rootdir    (file-name-directory builddir))
           (subtarget  (substring moduledir (length rootdir)))
           (compiledir (concat builddir "/" subtarget)))
        (file-truename compiledir))))
  )

;; --------------------------------------------------------------



(defun xtdmacs-compile++-run (prompt type)
  (xtdmacs-compile++-arrange-windows)

  (let* ((config         (cdr (assoc type xtdmacs-compile++-config-alist)))
         (get-params     (cdr (assoc "get-params" config)))
         (command        (cdr (assoc "command"    config)))
         (string-command ""))

    (if prompt
        (funcall get-params))

    (setq string-command (funcall-or-value command))
    (compile string-command t)

    ;; we load xtdmacs-compile++ on *compilation* buffer with a configuration that runs
    ;; the current command
    (with-current-buffer "*compilation*"
      ;;(ansi-color-for-comint-mode-on)
      (face-remap-add-relative 'mode-line          'xtdmacs-compile++-compiling-face)
      (face-remap-add-relative 'mode-line-inactive 'xtdmacs-compile++-compiling-face)
      (face-remap-set-base     'default            nil)
      (if (mode-enabled 'xtdmacs-compile++)
          (message "compile++ alread enabled on *compilation*")
        (progn
          (xtdmacs-compile++-mode t)))
      (setq-local xtdmacs-compile++-config-alist
                  `((,type .
                           (("get-params" . (lambda()))
                            ("command"    . ,string-command))))))
    )
  )


(defun xtdmacs-compile++-default-params (type)
  (let* ((dir    (--xtdmacs-compile++-promt-value type "dir" "Directory"))
         (env    (--xtdmacs-compile++-promt-value type "env" "Environment"))
         (bin    (--xtdmacs-compile++-promt-value type "bin" "Binary"))
         (global (y-or-n-p "Default for all buffers ?")))
    (--xtdmacs-compile++-set-value type "dir" dir global)
    (--xtdmacs-compile++-set-value type "env" env global)
    (--xtdmacs-compile++-set-value type "bin" bin global)
    global)
  )

(defun xtdmacs-compile++-compose-params (type)
  (let* ((global  (xtdmacs-compile++-default-params type))
         (compose (--xtdmacs-compile++-promt-value type "compose-file" "Compose-file"))
         (service (--xtdmacs-compile++-promt-value type "service"      "Service")))
    (--xtdmacs-compile++-set-value type "compose-file" compose global)
    (--xtdmacs-compile++-set-value type "service"      service global)
    global)
  )

(defun xtdmacs-compile++-docker-exec-params (type)
  (let* ((global    (xtdmacs-compile++-default-params type))
         (container (--xtdmacs-compile++-promt-value type "container" "Container")))
    (--xtdmacs-compile++-set-value type "container" container global)
    global)
  )

(defun xtdmacs-compile++-docker-run-params (type)
  (let* ((global (xtdmacs-compile++-default-params type))
         (image  (--xtdmacs-compile++-promt-value type "image" "Image")))
    (--xtdmacs-compile++-set-value type "image" image global)
    global)
  )

(defun xtdmacs-compile++-compose-run-command (type)
  (let* ((config (cdr (assoc type xtdmacs-compile++-config-alist)))
         (dir     (cdr (assoc "dir"     config)))
         (env     (cdr (assoc "env"     config)))
         (bin     (cdr (assoc "bin"     config)))
         (compose (cdr (assoc "compose-file" config)))
         (service (cdr (assoc "service" config)))
         (dockerenv (if (string= env "")
                        env
                      (mapconcat 'identity (mapcar (lambda (el) (concat "-e " el)) (split-string env " ")) " "))))
    (format "cd %s && SRCDIR=%s docker-compose -f %s run --rm %s %s %s"
            (funcall-or-value dir)
            (funcall-or-value dir)
            (funcall-or-value compose)
            dockerenv
            (funcall-or-value service)
            (funcall-or-value bin)))
  )

(defun xtdmacs-compile++-default-command (type)
  (let* ((config (cdr (assoc type xtdmacs-compile++-config-alist)))
         (dir (cdr (assoc "dir" config)))
         (env (cdr (assoc "env" config)))
         (bin (cdr (assoc "bin" config))))
    (format "cd %s && %s %s"
            (funcall-or-value dir)
            (funcall-or-value env)
            (funcall-or-value bin)))
  )

(defun xtdmacs-compile++-compose-exec-command (type)
  (let* ((config (cdr (assoc type xtdmacs-compile++-config-alist)))
         (dir     (cdr (assoc "dir"     config)))
         (bin     (cdr (assoc "bin"     config)))
         (compose (cdr (assoc "compose-file" config)))
         (service (cdr (assoc "service" config))))
    (format "cd %s && SRCDIR=%s docker-compose -f %s exec %s %s"
            (funcall-or-value dir)
            (funcall-or-value dir)
            (funcall-or-value compose)
            (funcall-or-value service)
            (funcall-or-value bin)))
  )

(defun xtdmacs-compile++-docker-exec-command (type)
  (let* ((config   (cdr (assoc type xtdmacs-compile++-config-alist)))
         (dir       (cdr (assoc "dir"       config)))
         (bin       (cdr (assoc "bin"       config)))
         (env       (cdr (assoc "env"       config)))
         (container (cdr (assoc "container" config)))
         )
    (format "docker exec -t %s /bin/bash -c 'cd %s && %s %s'"
            (funcall-or-value container)
            (funcall-or-value dir)
            env
            (funcall-or-value bin)))
  )
(defun xtdmacs-compile++-docker-run-command (type)
  (let* ((config (cdr (assoc type xtdmacs-compile++-config-alist)))
         (dir     (cdr (assoc "dir"     config)))
         (env     (cdr (assoc "env"     config)))
         (bin     (cdr (assoc "bin"     config)))
         (image   (cdr (assoc "image"   config)))
         (dockerenv (if (string= env "")
                        env
                      (mapconcat 'identity (mapcar (lambda (el) (concat "-e " el)) (split-string env " ")) " ")))
         )
    (format "docker run --rm=true %s %s /bin/bash -c 'cd %s && %s'"
            dockerenv
            image
            (funcall-or-value dir)
            (funcall-or-value bin))
    )
  )


(defun xtdmacs-compile++-iwyu-find-compile-commands ()
  (let* ((topbuilddir (xtdmacs-compile++-get-nearest-filename xtdmacs-compile++-iwyu-build-directory-name)))
    (concat topbuilddir "/compile_commands.json"))
  )

(defun xtdmacs-compile++-iwyu-default-cmd()
  (format "iwyu-wrapper.py -c %s %s"
          (xtdmacs-compile++-iwyu-find-compile-commands)
          (buffer-file-name))
  )

(defun xtdmacs-compile++-command-1(interactive)
  (xtdmacs-compile++-run interactive xtdmacs-compile++-command-1)
  )

(defun xtdmacs-compile++-command-2(interactive)
  (xtdmacs-compile++-run interactive xtdmacs-compile++-command-2)
  )

(defun xtdmacs-compile++-command-3(interactive)
  (xtdmacs-compile++-run interactive xtdmacs-compile++-command-3)
  )

(defun xtdmacs-compile++-command-4(interactive)
  (xtdmacs-compile++-run interactive xtdmacs-compile++-command-4)
  )

(defun xtdmacs-compile++-command-5(interactive)
  (xtdmacs-compile++-run interactive xtdmacs-compile++-command-5)
  )

(defun xtdmacs-compile++-compilation-finished(buffer status)
  (with-current-buffer buffer
    (if (not (string-prefix-p "finished" status))
        (progn
          (face-remap-add-relative 'mode-line          'xtdmacs-compile++-error-face)
          (face-remap-add-relative 'mode-line-inactive 'xtdmacs-compile++-error-face)
          )
      (progn
        (face-remap-set-base 'mode-line          nil)
        (face-remap-set-base 'mode-line-inactive nil))
      )))

;; --------------------------------------------------------------------------

(defun xtdmacs-compile++-mode-construct()
  (add-hook 'compilation-filter-hook 'xtdmacs-compile++-colorize-compilation-buffer)
  (make-local-variable 'xtdmacs-compile++-config-alist)
  (make-local-variable 'mode-line)
  (make-local-variable 'mode-line-inactive)
  (make-local-variable 'default)
  (message "enabled : xtdmacs-compile++-mode")
  (add-to-list 'compilation-finish-functions 'xtdmacs-compile++-compilation-finished)

  ;; comint install
  (progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
         (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
         (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))

  )

(defun xtdmacs-compile++-mode-destroy()
  (remove-hook 'compilation-filter-hook 'xtdmacs-compile++-colorize-compilation-buffer)



  ;; comint uninstall
  (progn (remove-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
         (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
         (setq font-lock-unfontify-region-function 'font-lock-default-unfontify-region))

  (message "disabled : xtdmacs-compile++-mode")
  )


;;;###autoload
(define-minor-mode xtdmacs-compile++-mode
  "Set of function beyond compilation-mode" nil " xtdmacs-compile++"
  '(
    ([f6]                . (lambda () (interactive) (xtdmacs-compile++-command-1 nil)))
    ([C-f6]              . (lambda () (interactive) (xtdmacs-compile++-command-1 t)))

    ([M-f6]              . kill-compilation)
    ([C-S-f6]            . (lambda () (interactive) (xtdmacs-compile++-command-4 t)))

    ([f7]                . (lambda () (interactive) (xtdmacs-compile++-command-2 nil)))
    ([C-f7]              . (lambda () (interactive) (xtdmacs-compile++-command-2 t)))
    ([M-f7]              . kill-compilation)
    ([C-S-f7]            . (lambda () (interactive) (xtdmacs-compile++-command-5 nil)))

    ([f8]                . (lambda () (interactive) (xtdmacs-compile++-command-3 nil)))
    ([C-f8]              . (lambda () (interactive) (xtdmacs-compile++-command-3 t)))
    ([M-f8]              . kill-compilation)

    ([f9]                . xtdmacs-compile++-next-error)
    ([C-f9]              . xtdmacs-compile++-next-warning)
    )

  (if xtdmacs-compile++-mode
      (xtdmacs-compile++-mode-construct)
    (xtdmacs-compile++-mode-destroy))
  )

;; --------------------------------------------------------------------------

(make-variable-buffer-local 'xtdmacs-compile++-config-alist)

;;;###autoload
(put 'xtdmacs-compile++-buffer-height 'safe-local-variable 'integerp)
;;;###autoload
(put 'xtdmacs-compile++-scroll-output 'safe-local-variable 'booleanp)
;;;###autoload
(put 'xtdmacs-compile++-buffer-local 'safe-local-variable 'booleanp)
;;;###autoload
(put 'xtdmacs-compile++-config-alist 'safe-local-variable '(lambda(p) t))
;;;###autoload
(put 'xtdmacs-compile++-iwyu-build-directory-name 'safe-local-variable 'stringp)

(provide 'xtdmacs-compile++)

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
