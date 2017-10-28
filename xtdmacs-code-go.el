(require 'xtdmacs-compile++)
(require 'package)
(require 'go-autocomplete)
(require 'auto-complete-config)

(defcustom xtdmacs-code-go-compile-alist
  '(("compile" .
     (("dir"        . xtdmacs-compile++-get-dir-git)
      ("bin"        . xtdmacs-code-go-get-project-name)
      ("env"        . "")
      ("get-params" . (lambda() (xtdmacs-compile++-default-params  "compile")))
      ("command"    . (lambda() (xtdmacs-code-go-command           "compile")))))
    ("test" .
     (("dir"        . xtdmacs-compile++-get-dir-git)
      ("bin"        . xtdmacs-code-go-get-project-name)
      ("env"        . "")
      ("get-params" . (lambda() (xtdmacs-compile++-default-params  "test")))
      ("command"    . (lambda() (xtdmacs-code-go-command "test"))))))
  "Xtdmacs-Code-go compilation configuration"
  :group 'xtdmacs-code-go
  :safe '(lambda(p) t)
  :type '(alist :key-type string :value-type (alist :key-type string :value-type (choice (string) (function))))
  )

(defcustom xtdmacs-code-go-keywords-alist
  '(("\\<g_[_a-zA-Z0-9]+\\>" .       'xtdmacs-code-face-global-variable)
    ("\\<l_[_a-zA-Z0-9]+\\>" .       'xtdmacs-code-face-local-variable)
    ("\\<\\(p_[_a-zA-Z0-9]+\\)\\>" . 'xtdmacs-code-face-param)
    ("\\<c_[_a-zA-Z0-9]+\\>" .       'xtdmacs-code-face-counter)
    )
  "List of additional go font-lock keywords"
  :group 'xtdmacs-code-go
  :safe '(lambda(p) t))

(defcustom xtdmacs-code-go-indent-load-auto
  nil
  "Enables go code auto-indentation on load."
  :group 'xtdmacs-code-go
  :type 'boolean
  :safe 'booleanp)

(defcustom xtdmacs-code-go-indent-save-auto
  nil
  "Enables go code auto-indentation on save."
  :group 'xtdmacs-code-go
  :type 'boolean
  :safe 'booleanp
  )

;; --------------------------------------------------------------------------- ;
(defun xtdmacs-code-go-get-project-name ()
  (file-name-nondirectory
   (directory-file-name
    (xtdmacs-compile++-get-dir-git)))
  )

(defun xtdmacs-code-go-command (mode)
  (let* ((config (cdr (assoc mode xtdmacs-compile++-config-alist)))
         (dir    (cdr (assoc "dir"    config)))
         (env    (cdr (assoc "env"    config)))
         (bin    (cdr (assoc "bin"    config))))
    (format "cd %s && %s go build -o %s *.go"
            (funcall-or-value dir)
            (funcall-or-value env)
            (funcall-or-value bin)))
  )

;; --------------------------------------------------------------------------- ;

(defun --xtdmacs-code-go-construct()
  (font-lock-add-keywords nil xtdmacs-code-go-keywords-alist)

  (ac-config-default)
  (setq ac-sources '(ac-source-go))
  (go-eldoc-setup)
  (define-key go-mode-map (kbd "<f12>")   'godef-jump)
  (define-key go-mode-map (kbd "C-<f12>") 'godef-jump-other-window)


  (when (and (boundp 'xtdmacs-compile++-mode) xtdmacs-compile++-mode)
    (setcdr (assoc "compile" xtdmacs-compile++-config-alist)
            (cdr (assoc "compile" xtdmacs-code-go-compile-alist)))
    (setcdr (assoc "test" xtdmacs-compile++-config-alist)
            (cdr (assoc "test" xtdmacs-code-go-compile-alist)))
    )

  (if xtdmacs-code-go-indent-save-auto
      (add-hook 'before-save-hook '(lambda() (xtdmacs-code-format-buffer t nil)) t t))
  (if xtdmacs-code-go-indent-load-auto
      (xtdmacs-code-format-buffer-with-ident))

  (message "enabled : xtdmacs-code-go-mode")
  )

(defun --xtdmacs-code-go-destroy()
  (if xtdmacs-code-go-indent-save-auto
      (remove-hook 'before-save-hook '(lambda() (xtdmacs-code-format-buffer t nil))))
  ;; (when (mode-enabled 'go-mode)
  ;;   (go-mode nil))
  (font-lock-remove-keywords nil xtdmacs-code-go-keywords-alist)
  (message "disabled : xtdmacs-code-go-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-go-mode
  "Code for Go" nil "Code" nil
  (if xtdmacs-code-go-mode
      (--xtdmacs-code-go-construct)
    (--xtdmacs-code-go-destroy))
  )


;; --------------------------------------------------------------------------- ;

;;;###autoload
(put 'xtdmacs-code-go-compile-alist 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-go-keywords-alist 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-go-indent-load-auto 'safe-local-variable 'booleanp)
;;;###autoload
(put 'xtdmacs-code-go-indent-save-auto 'safe-local-variable 'booleanp)
;;;###autoload
(put 'xtdmacs-code-go-pylint-args 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-go-pylint-bin-path 'safe-local-variable 'file-exists-p)
;;;###autoload
(put 'xtdmacs-code-go-test-args 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-go-test-bin-path 'safe-local-variable 'file-exists-p)

(provide 'xtdmacs-code-go)

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
