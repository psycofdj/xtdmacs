;;; xtdmacs-compile++.el --- Extended compilation-mode helpers  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs-compile++-mode is a buffer-local minor mode that wraps
;; `compile' with: per-major-mode and per-buffer command configuration,
;; six F-key compile actions, mode-line color while compiling, and
;; xterm-color filtering of compilation output.

;;; Code:

(require 'xterm-color)
(require 'xtdmacs-lang)
(eval-when-compile (require 'subr-x))

(defface xtdmacs-compile++-compiling-face
  '((t (:background "green")))
  "Overriding face of mode-line and mode-line-inactive while compiling."
  :group 'xtdmacs-compile++)

(defface xtdmacs-compile++-error-face
  '((t (:background "blue")))
  "Overriding face of buffer when compilation exited in error."
  :group 'xtdmacs-compile++)

(defcustom xtdmacs-compile++-buffer-height 13
  "Compilation buffer height in lines."
  :group 'xtdmacs-compile++ :type 'integer)

(defcustom xtdmacs-compile++-scroll-output t
  "Whether to scroll the compilation buffer while compiling."
  :group 'xtdmacs-compile++ :type 'boolean)

(defcustom xtdmacs-compile++-iwyu-build-directory-name
  ".release"
  "Standard build directory name."
  :group 'xtdmacs-compile++
  :type 'string
  :safe 'stringp)

(defcustom xtdmacs-compile++-default-config-alist
  '((:compile . ((:dir        . xtdmacs-compile++-guess-directory)
                 (:env        . "")
                 (:bin        . "make -j")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))
    (:test .    ((:dir        . xtdmacs-compile++-guess-directory)
                 (:env        . "")
                 (:bin        . "make -j")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))
    (:deploy .  ((:dir        . xtdmacs-compile++-guess-directory)
                 (:env        . "")
                 (:bin        . "make -j")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))
    (:doc .     ((:dir        . xtdmacs-compile++-guess-directory)
                 (:env        . "")
                 (:bin        . "make -j")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))
    (:lint .    ((:dir        . xtdmacs-compile++-guess-directory)
                 (:env        . "")
                 (:bin        . "make -j")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))
    (:manual . ((:dir        . xtdmacs-compile++-guess-directory)
                (:env        . "")
                (:bin        . "make -j")
                (:get-params . xtdmacs-compile++-default-params)
                (:command    . xtdmacs-compile++-default-command)))
    )
  "Default xtdmacs-compile++ callback configuration."
  :group 'xtdmacs-compile++
  :safe '(lambda(p) t)
  :type '(alist :key 'string :value '(alias :key string :value '(choice (string) (function))))
  )


(defvar xtdmacs-compile++-config-alist
  `(("default" . ,xtdmacs-compile++-default-config-alist))
  "Per-major-mode compile configuration.
Populated via `xtdmacs-compile++-register-config'.")

(defcustom xtdmacs-compile++-command-1
  :compile
  "Compile-config key bound to F-key #1."
  :group 'xtdmacs-compile++
  :type '(choice (const :compile)
                 (const :test)
                 (const :deploy)
                 (const :doc)
                 (const :lint)
                 (const :manual)
                 (other :tag "Other" ""))
  :safe 'stringp)

(defcustom xtdmacs-compile++-command-2
  :test
  "Compile-config key bound to F-key #2."
  :group 'xtdmacs-compile++
  :type '(choice (const :compile)
                 (const :test)
                 (const :deploy)
                 (const :doc)
                 (const :lint)
                 (const :manual)
                 (other :tag "Other" ""))
  :safe 'stringp)


(defcustom xtdmacs-compile++-command-3
  :deploy
  "Compile-config key bound to F-key #3."
  :group 'xtdmacs-compile++
  :type '(choice (const :compile)
                 (const :test)
                 (const :deploy)
                 (const :doc)
                 (const :lint)
                 (const :manual)
                 (other :tag "Other" ""))
  :safe 'stringp)

(defcustom xtdmacs-compile++-command-4
  :doc
  "Compile-config key bound to F-key #4."
  :group 'xtdmacs-compile++
  :type '(choice (const :compile)
                 (const :test)
                 (const :deploy)
                 (const :doc)
                 (const :lint)
                 (const :manual)
                 (other :tag "Other" ""))
  :safe 'stringp)

(defcustom xtdmacs-compile++-command-5
  :lint
  "Compile-config key bound to F-key #5."
  :group 'xtdmacs-compile++
  :type '(choice (const :compile)
                 (const :test)
                 (const :deploy)
                 (const :doc)
                 (const :lint)
                 (const :manual)
                 (other :tag "Other" ""))
  :safe 'stringp)


(defcustom xtdmacs-compile++-command-6
  :manual
  "Compile-config key bound to F-key #6."
  :group 'xtdmacs-compile++
  :type '(choice (const :compile)
                 (const :test)
                 (const :deploy)
                 (const :doc)
                 (const :lint)
                 (const :manual)
                 (other :tag "Other" ""))
  :safe 'stringp)


(defun xtdmacs-compile++-register-config (mode config)
  "Register CONFIG as the compile configuration for major mode MODE."
  (add-to-list 'xtdmacs-compile++-config-alist (cons mode config)))


(defun --xtdmacs-compile++-get-config (&optional mode)
  "Return the compile configuration for MODE, falling back to the default one."
  (let* ((name   (symbol-name (or mode major-mode)))
         (global (cdr (assoc "default" xtdmacs-compile++-config-alist)))
         (target (cdr (assoc name      xtdmacs-compile++-config-alist))))
    (or target global)))

(defun --xtdmacs-compile++-get-value (mode type key)
  "Return the value of KEY in the entry TYPE of MODE's compile configuration."
  (let* ((data      (--xtdmacs-compile++-get-config mode))
         (config    (cdr (assoc type data)))
         (valueitem (assoc key config))
         (value     (if valueitem (cdr valueitem) nil)))
    value))

(defun --xtdmacs-compile++-set-value (mode type key value)
  "Set KEY to VALUE in the entry TYPE of MODE's compile configuration."
  (let* ((data    (--xtdmacs-compile++-get-config mode))
         (config  (cdr (assoc type data)))
         (keylist (assoc key config)))
    (if keylist
        (setcdr keylist value)
      (nconc config (list (cons key value))))))

(defun --xtdmacs-compile++-prompt-value (mode type key label)
  "Prompt the user for KEY of entry TYPE in MODE, using LABEL as prompt prefix."
  (let* ((value (--xtdmacs-compile++-get-value mode type key)))
    (cond
     ((string= key :dir)
      (read-directory-name (format "%s : " label) (funcall-or-value value)))
     ((string= key :file)
      (read-file-name (format "%s : " label) (funcall-or-value value)))
     (t
      (read-from-minibuffer (format "%s : " label) (funcall-or-value value))))))

(defun xtdmacs-compile++-colorize-compilation-buffer ()
  "Apply ANSI color to the entire compilation buffer."
  (read-only-mode)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode))

(defun xtdmacs-compile++-arrange-windows ()
  "Split the frame and dedicate a window to the *compilation* buffer."
  (let* ((exists  (member "*compilation*" (mapcar 'buffer-name (mapcar 'window-buffer (window-list))))))
    (unless exists
      (delete-other-windows)
      (split-window-vertically)
      (split-window-horizontally)
      (windmove-down)
      (switch-to-buffer "*compilation*")
      (set-window-text-height (selected-window) xtdmacs-compile++-buffer-height)
      (set-window-dedicated-p (selected-window) t)
      (set-window-point       (selected-window) (point-max))
      (windmove-up))))

(defun xtdmacs-compile++-previous-error ()
  "Jump to the previous compilation error, skipping warnings."
  (interactive)
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (setq compilation-skip-threshold 2)
  (previous-error))

(defun xtdmacs-compile++-next-error ()
  "Jump to the next compilation error, skipping warnings."
  (interactive)
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (setq compilation-skip-threshold 2)
  (next-error))

(defun xtdmacs-compile++-previous-warning ()
  "Jump to the previous compilation warning."
  (interactive)
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (setq compilation-skip-threshold 1)
  (previous-error))

(defun xtdmacs-compile++-next-warning ()
  "Jump to the next compilation warning."
  (interactive)
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (setq compilation-skip-threshold 1)
  (next-error))




;; --------------------------------------------------------------

(defun xtdmacs-compile++-run (prompt type &optional mode)
  "Run compile entry TYPE in MODE, prompting for parameters if PROMPT is non-nil."
  (xtdmacs-compile++-arrange-windows)

  (let* ((get-params (--xtdmacs-compile++-get-value mode type :get-params))
         (command    (--xtdmacs-compile++-get-value mode type :command))
         (generated  (--xtdmacs-compile++-get-value mode type :generated))
         (cache      (--xtdmacs-compile++-get-value mode type :cache))
         )

    ;; when interactive requested, prompt params and delete final command
    (when prompt
      (funcall get-params type)
      (setq cache t)
      (setq generated nil))

    (unless generated
      (setq generated (funcall command type))
      (when cache
        (--xtdmacs-compile++-set-value mode type :cache     t)
        (--xtdmacs-compile++-set-value mode type :generated generated)))

    ;; run compilation
    (compile generated t)

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
      )
    )
  )



(defun xtdmacs-compile++-query-local ()
  "Ask whether to keep the compile config buffer-local; if yes, copy and localize."
  (unless (y-or-n-p "Apply to all buffers ? ")
    (let* ((tmp (copy-tree xtdmacs-compile++-config-alist)))
      (make-local-variable 'xtdmacs-compile++-config-alist)
      (setq xtdmacs-compile++-config-alist tmp))))

(defun xtdmacs-compile++-iwyu-find-compile-commands ()
  "Return the path to compile_commands.json under the nearest build directory."
  (let* ((topbuilddir (xtdmacs-compile++-get-nearest-filename xtdmacs-compile++-iwyu-build-directory-name)))
    (concat topbuilddir "/compile_commands.json")))

;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;


(defun xtdmacs-compile++-get-nearest-filename (filename)
  "Search upwards from the buffer's directory for FILENAME; return its full path."
  (let* ((origin (buffer-file-name))
         (dir (file-name-directory origin))
         (dirs (split-string dir "/"))
         (result nil))
    (while (and (> (length dirs) 1) (equal nil result))
      (if (file-exists-p (concat (mapconcat 'identity dirs "/") "/" filename))
          (setq result (concat (mapconcat 'identity dirs "/") "/" filename))
        (nbutlast dirs 1)))
    result))

(defun xtdmacs-compile++-get-dir-locals-directory ()
  "Return the directory of the nearest .dir-locals.el for the current buffer."
  (car (dir-locals-find-file (buffer-file-name))))

(defun xtdmacs-compile++-get-dir-git ()
  "Return the path of the nearest enclosing git repository root."
  (file-name-directory (xtdmacs-compile++-get-nearest-filename ".git")))

(defun xtdmacs-compile++-get-dir-buffer ()
  "Return the directory containing the current buffer's file."
  (file-name-directory (buffer-file-name)))

(defun xtdmacs-compile++-guess-directory ()
  "Heuristically pick a build directory based on CMakeLists.txt and .release."
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
        (file-truename compiledir)))))

(defun xtdmacs-compile++-get-current-branch ()
  "Return the name of the current git branch (or empty string)."
  (let* ((target-dir (file-name-directory (buffer-file-name)))
         (cmd        (format "cd %s && git rev-parse --abbrev-ref HEAD" target-dir))
         (raw-branch (shell-command-to-string cmd))
         (branch     (string-trim raw-branch)))
    branch))

(defun xtdmacs-compile++-iwyu-default-cmd ()
  "Return the default include-what-you-use command for the current buffer."
  (format "iwyu-wrapper.py -c %s %s"
          (xtdmacs-compile++-iwyu-find-compile-commands)
          (buffer-file-name)))

;;;;;;;;;;;;;;
;; Commands ;;
;;;;;;;;;;;;;;


(defun xtdmacs-compile++-default-command (type &optional mode)
  "Build a `cd DIR && ENV BIN' command for compile entry TYPE in MODE."
  (let* ((dir    (--xtdmacs-compile++-get-value mode type :dir))
         (env    (--xtdmacs-compile++-get-value mode type :env))
         (bin    (--xtdmacs-compile++-get-value mode type :bin)))
    (format "cd %s && %s %s"
            (funcall-or-value dir)
            (funcall-or-value env)
            (funcall-or-value bin))))

(defun xtdmacs-compile++-simple-file-command (type &optional mode)
  "Build a `BIN FILE' command for compile entry TYPE in MODE."
  (let* ((file   (--xtdmacs-compile++-get-value mode type :file))
         (bin    (--xtdmacs-compile++-get-value mode type :bin)))
    (format "%s %s"
            (funcall-or-value bin)
            (funcall-or-value file))))

(defun xtdmacs-compile++-compose-run-command (type &optional mode)
  "Build a `docker-compose run' command for compile entry TYPE in MODE."
  (let* ((dir     (--xtdmacs-compile++-get-value mode type :dir))
         (env     (--xtdmacs-compile++-get-value mode type :env))
         (bin     (--xtdmacs-compile++-get-value mode type :bin))
         (compose (--xtdmacs-compile++-get-value mode type :compose-file))
         (service (--xtdmacs-compile++-get-value mode type :service))
         (dockerenv (if (string= env "")
                        env
                      (mapconcat 'identity (mapcar (lambda (el) (concat "-e " el)) (split-string env " ")) " "))))
    (format "cd %s && SRCDIR=%s docker-compose -f %s run --rm %s %s %s"
            (funcall-or-value dir)
            (funcall-or-value dir)
            (funcall-or-value compose)
            dockerenv
            (funcall-or-value service)
            (funcall-or-value bin))))

(defun xtdmacs-compile++-compose-exec-command (type &optional mode)
  "Build a `docker-compose exec' command for compile entry TYPE in MODE."
  (let* ((dir     (--xtdmacs-compile++-get-value mode type :dir))
         (bin     (--xtdmacs-compile++-get-value mode type :bin))
         (compose (--xtdmacs-compile++-get-value mode type :compose-file))
         (service (--xtdmacs-compile++-get-value mode type :service)))
    (format "cd %s && SRCDIR=%s docker-compose -f %s exec %s %s"
            (funcall-or-value dir)
            (funcall-or-value dir)
            (funcall-or-value compose)
            (funcall-or-value service)
            (funcall-or-value bin))))


(defun xtdmacs-compile++-docker-run-command (type &optional mode)
  "Build a `docker run' command for compile entry TYPE in MODE."
  (let* ((dir     (--xtdmacs-compile++-get-value mode type :dir))
         (env     (--xtdmacs-compile++-get-value mode type :env))
         (bin     (--xtdmacs-compile++-get-value mode type :bin))
         (image   (--xtdmacs-compile++-get-value mode type :image))
         (dockerenv (if (string= env "")
                        env
                      (mapconcat 'identity (mapcar (lambda (el) (concat "-e " el)) (split-string env " ")) " "))))
    (format "docker run --rm=true %s %s /bin/bash -c 'cd %s && %s'"
            dockerenv
            image
            (funcall-or-value dir)
            (funcall-or-value bin))))

(defun xtdmacs-compile++-docker-exec-command (type &optional mode)
  "Build a `docker exec' command for compile entry TYPE in MODE."
  (let* ((dir       (--xtdmacs-compile++-get-value mode type :dir))
         (bin       (--xtdmacs-compile++-get-value mode type :bin))
         (env       (--xtdmacs-compile++-get-value mode type :env))
         (container (--xtdmacs-compile++-get-value mode type "container")))
    (format "docker exec -t %s /bin/bash -c 'cd %s && %s %s'"
            (funcall-or-value container)
            (funcall-or-value dir)
            env
            (funcall-or-value bin))))

;;;;;;;;;;;;
;; Params ;;
;;;;;;;;;;;;

(defun xtdmacs-compile++-default-params (type &optional mode)
  "Prompt for :dir, :env, :bin of compile entry TYPE in MODE and store them."
  (let* ((dir    (--xtdmacs-compile++-prompt-value mode type :dir "Directory"))
         (env    (--xtdmacs-compile++-prompt-value mode type :env "Environment"))
         (bin    (--xtdmacs-compile++-prompt-value mode type :bin "Binary")))
    (xtdmacs-compile++-query-local)
    (--xtdmacs-compile++-set-value mode type :dir dir)
    (--xtdmacs-compile++-set-value mode type :env env)
    (--xtdmacs-compile++-set-value mode type :bin bin)))

(defun xtdmacs-compile++-current-file-params (type &optional mode)
  "Prompt for :bin, :file of compile entry TYPE in MODE and store them."
  (let* ((bin  (--xtdmacs-compile++-prompt-value mode type :bin  "Binary"))
         (file (--xtdmacs-compile++-prompt-value mode type :file "File")))
    (xtdmacs-compile++-query-local)
    (--xtdmacs-compile++-set-value mode type :bin  bin)
    (--xtdmacs-compile++-set-value mode type :file file)))

(defun xtdmacs-compile++-compose-params (type &optional mode)
  "Prompt for default + compose params of compile entry TYPE in MODE."
  (xtdmacs-compile++-default-params type mode)
  (let* ((compose (--xtdmacs-compile++-prompt-value mode type :compose-file "Compose-file"))
         (service (--xtdmacs-compile++-prompt-value mode type :service      "Service")))
    (--xtdmacs-compile++-set-value mode type :compose-file compose)
    (--xtdmacs-compile++-set-value mode type :service      service)))

(defun xtdmacs-compile++-docker-exec-params (type &optional mode)
  "Prompt for default + container params of compile entry TYPE in MODE."
  (xtdmacs-compile++-default-params type mode)
  (let* ((container (--xtdmacs-compile++-prompt-value mode type "container" "Container")))
    (--xtdmacs-compile++-set-value mode type "container" container)))

(defun xtdmacs-compile++-docker-run-params (type &optional mode)
  "Prompt for default + image params of compile entry TYPE in MODE."
  (xtdmacs-compile++-default-params type mode)
  (let* ((image  (--xtdmacs-compile++-prompt-value mode type :image "Image")))
    (--xtdmacs-compile++-set-value mode type :image image)))


;;;;;;;;;
;; End ;;
;;;;;;;;;


(defun xtdmacs-compile++-command-1 (interactive)
  "Run F-key #1 compile entry; with INTERACTIVE non-nil, prompt for parameters."
  (xtdmacs-compile++-run interactive xtdmacs-compile++-command-1))

(defun xtdmacs-compile++-command-2 (interactive)
  "Run F-key #2 compile entry; with INTERACTIVE non-nil, prompt for parameters."
  (xtdmacs-compile++-run interactive xtdmacs-compile++-command-2))

(defun xtdmacs-compile++-command-3 (interactive)
  "Run F-key #3 compile entry; with INTERACTIVE non-nil, prompt for parameters."
  (xtdmacs-compile++-run interactive xtdmacs-compile++-command-3))

(defun xtdmacs-compile++-command-4 (interactive)
  "Run F-key #4 compile entry; with INTERACTIVE non-nil, prompt for parameters."
  (xtdmacs-compile++-run interactive xtdmacs-compile++-command-4))

(defun xtdmacs-compile++-command-5 (interactive)
  "Run F-key #5 compile entry; with INTERACTIVE non-nil, prompt for parameters."
  (xtdmacs-compile++-run interactive xtdmacs-compile++-command-5))

(defun xtdmacs-compile++-command-6 (interactive)
  "Run F-key #6 compile entry; with INTERACTIVE non-nil, prompt for parameters."
  (xtdmacs-compile++-run interactive xtdmacs-compile++-command-6))

(defun xtdmacs-compile++-compilation-finished (buffer status)
  "Color BUFFER's mode-line red on failure (STATUS not starting with `finished')."
  (with-current-buffer buffer
    (if (not (string-prefix-p "finished" status))
        (progn
          (face-remap-add-relative 'mode-line          'xtdmacs-compile++-error-face)
          (face-remap-add-relative 'mode-line-inactive 'xtdmacs-compile++-error-face))
      (face-remap-set-base 'mode-line          nil)
      (face-remap-set-base 'mode-line-inactive nil))))

;; --------------------------------------------------------------------------

(defun xtdmacs-compile++-mode-construct ()
  "Initialize buffer-local state for `xtdmacs-compile++-mode'."
  (add-hook 'compilation-filter-hook 'xtdmacs-compile++-colorize-compilation-buffer)
  (make-local-variable 'mode-line)
  (make-local-variable 'mode-line-inactive)
  (message "enabled : xtdmacs-compile++-mode")
  (add-to-list 'compilation-finish-functions 'xtdmacs-compile++-compilation-finished)
  ;; comint install
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
  (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))

(defun xtdmacs-compile++-mode-destroy ()
  "Tear down buffer-local state when `xtdmacs-compile++-mode' is disabled."
  (remove-hook 'compilation-filter-hook 'xtdmacs-compile++-colorize-compilation-buffer)
  ;; comint uninstall
  (remove-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (setq font-lock-unfontify-region-function 'font-lock-default-unfontify-region)
  (message "disabled : xtdmacs-compile++-mode"))


;;;###autoload
(define-minor-mode xtdmacs-compile++-mode
  "Set of functions beyond `compilation-mode'."
  :init-value nil
  :lighter " xtdmacs-compile++"
  :keymap
  '(
    ([f6]               . (lambda () (interactive) (xtdmacs-compile++-command-1 nil)))
    ([21 f6]            . (lambda () (interactive) (xtdmacs-compile++-command-1 t)))
    ([C-f6]             . (lambda () (interactive) (xtdmacs-compile++-command-4 nil)))
    ([21 C-f6]          . (lambda () (interactive) (xtdmacs-compile++-command-4 t)))
    ([21 f30]           . (lambda () (interactive) (xtdmacs-compile++-command-4 t)))

    ([f7]               . (lambda () (interactive) (xtdmacs-compile++-command-2 nil)))
    ([21 f7]            . (lambda () (interactive) (xtdmacs-compile++-command-2 t)))
    ([C-f7]             . (lambda () (interactive) (xtdmacs-compile++-command-5 nil)))
    ([21 C-f7]          . (lambda () (interactive) (xtdmacs-compile++-command-5 t)))
    ([21 f31]           . (lambda () (interactive) (xtdmacs-compile++-command-5 t)))

    ([f8]               . (lambda () (interactive) (xtdmacs-compile++-command-3 nil)))
    ([21 f8]            . (lambda () (interactive) (xtdmacs-compile++-command-3 t)))
    ([C-f8]             . (lambda () (interactive) (xtdmacs-compile++-command-6 nil)))
    ([21 C-f8]          . (lambda () (interactive) (xtdmacs-compile++-command-6 t)))
    ([21 f32]           . (lambda () (interactive) (xtdmacs-compile++-command-6 t)))

    ([M-f8]              . kill-compilation)
    ([M-f7]              . kill-compilation)
    ([M-f6]              . kill-compilation)

    ([f9]                . xtdmacs-compile++-next-error)
    ([C-f9]              . xtdmacs-compile++-next-warning)
    )

  (if xtdmacs-compile++-mode
      (xtdmacs-compile++-mode-construct)
    (xtdmacs-compile++-mode-destroy))
  )

;; --------------------------------------------------------------------------


;;;###autoload
(put 'xtdmacs-compile++-buffer-height 'safe-local-variable 'integerp)
;;;###autoload
(put 'xtdmacs-compile++-scroll-output 'safe-local-variable 'booleanp)
;;;###autoload
(put 'xtdmacs-compile++-default-config-alist 'safe-local-variable '(lambda(p) t))
;;;###autoload
(put 'xtdmacs-compile++-iwyu-build-directory-name 'safe-local-variable 'stringp)

(provide 'xtdmacs-compile++)

;;; xtdmacs-compile++.el ends here

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
