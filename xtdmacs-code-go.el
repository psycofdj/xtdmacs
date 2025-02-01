;; -*- lexical-binding: t -*-

(require 'xtdmacs-compile++)
(require 'package)
(require 'go-mode)
(require 'yasnippet)
(require 'dap-dlv-go)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; ;; Optionally enable completion-as-you-type behavior.
  ;; (setq company-idle-delay 0)
  ;; (setq company-minimum-prefix-length 1)
  )

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(defface xtdmacs-code-go-face-indent-error
  '((t (:foreground "color-124" :underline t)))
  "Indicates spaces instead of tabs for indentation"
  :group 'xtdmacs-code-go
  )

(defcustom xtdmacs-code-go-compile-alist
  '((:compile . ((:dir        . xtdmacs-compile++-get-dir-git)
                 (:bin        . xtdmacs-code-go-get-project-name)
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-code-go-command)))

    (:test .    ((:dir        . xtdmacs-compile++-get-dir-git)
                 (:bin        . xtdmacs-code-go-get-project-name)
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-code-go-command)))

    (:doc .     ((:dir        . xtdmacs-compile++-get-dir-buffer)
                 (:bin        . "nakedret")
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))

    (:lint .    ((:dir        . xtdmacs-compile++-get-dir-buffer)
                 (:bin        . "gometalinter.v2 -D gocyclo -D errcheck")
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))

    (:manual .  ((:dir        . xtdmacs-compile++-get-dir-git)
                 (:bin        . "true")
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command))))
  "Xtdmacs-Code-go compilation configuration"
  :group 'xtdmacs-code-go
  :safe '(lambda(p) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function))))
  )

(defcustom xtdmacs-code-go-keywords-alist
  '(("\\<g_[_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-global-variable)
    ("\\<l_[_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-local-variable)
    ("\\<\\(p_[_a-zA-Z0-9]+\\)\\>"     . 'xtdmacs-code-face-param)
    ("\\<c_[_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-counter)
    ("\\<g[A-Z][_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-global-variable)
    ("\\<l[A-Z][_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-local-variable)
    ("\\<\\(p[A-Z][_a-zA-Z0-9]+\\)\\>" . 'xtdmacs-code-face-param)
    ("\\<c[A-Z][_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-counter)
    ("\\<r[A-Z][_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-return)
    ("[&*]"                            . 'font-lock-constant-face)
    ("self"                            . 'font-lock-keyword-face)
    ("^ +"                             . 'xtdmacs-code-go-face-indent-error)
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

(defun xtdmacs-code-go-command (type &optional mode)
  (let* ((dir    (--xtdmacs-compile++-get-value mode type :dir))
         (env    (--xtdmacs-compile++-get-value mode type :env))
         (bin    (--xtdmacs-compile++-get-value mode type :bin)))
    (format "cd %s && %s go build -o %s *.go"
            (funcall-or-value dir)
            (funcall-or-value env)
            (funcall-or-value bin)))
  )

(defun --xtdmacs-lsp-find-references-other-window()
  (interactive)
  (lsp-find-references t :display-action 'window))

(defun --xtdmacs-lsp-find-references()
  (interactive)
  (lsp-find-references nil :display-action 'window))

;; --------------------------------------------------------------------------- ;

(defun --xtdmacs-code-go-construct()
  (yas-minor-mode t)
  (lsp)
  (font-lock-add-keywords nil xtdmacs-code-go-keywords-alist)
  (define-key go-mode-map (kbd "<f12>")   'lsp-find-definition)
  (define-key go-mode-map (kbd "C-<f12>") '--xtdmacs-lsp-find-definition-other-window)
  (define-key go-mode-map (kbd "<f11>")   '--xtdmacs-lsp-find-references)
  (define-key go-mode-map (kbd "C-<f11>") '--xtdmacs-lsp-find-references-other-window)
  (define-key go-mode-map (kbd "<f10>") 'lsp-ui-doc-glance)
  (define-key go-mode-map (kbd "C-<f10>") 'lsp-ui-imenu)
  (define-key go-mode-map (kbd "C-e <f12>") 'dap-debug)
  (define-key go-mode-map (kbd "C-e s") 'dap-step-in)
  (define-key go-mode-map (kbd "C-e o") 'dap-step-out)
  (define-key go-mode-map (kbd "C-e n") 'dap-next)
  (define-key go-mode-map (kbd "C-e c") 'dap-continue)
  (define-key go-mode-map (kbd "C-e r") 'dap-debug-restart)
  (define-key go-mode-map (kbd "C-e b") 'dap-breakpoint-toggle)

  (when (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "go-mode" xtdmacs-code-go-compile-alist))

  (if xtdmacs-code-go-indent-save-auto
      (progn
        (add-hook 'before-save-hook #'lsp-organize-imports t t)
        (add-hook 'before-save-hook #'lsp-format-buffer t t)))

  (if xtdmacs-code-go-indent-load-auto
      (progn
        (add-hook 'before-save-hook #'lsp-organize-imports t t)
        (add-hook 'before-save-hook #'lsp-format-buffer t t)))

  (add-to-list 'compilation-error-regexp-alist 'nakedret)
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(nakedret
     "\\(.+?\\):\\([0-9]+\\).*naked returns on.*" 1 2))
  (message "enabled : xtdmacs-code-go-mode")
  )

(defun --xtdmacs-code-go-destroy()
  (if xtdmacs-code-go-indent-save-auto
      (remove-hook 'before-save-hook '(lambda() (xtdmacs-code-format-buffer t nil))))
  (when (mode-enabled 'yas-minor-mode)
    (yas-minor-mode nil))
  (font-lock-remove-keywords nil xtdmacs-code-go-keywords-alist)
  (message "disabled : xtdmacs-code-go-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-go-mode
  "Code for Go" nil "Code"
  '(("\M-t"    . lsp-format-region)
    ("\C-\M-t" . lsp-format-buffer)
    ("\M-r"    . lsp-rename)
    ("\M-."    . company-complete)
    )
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

(provide 'xtdmacs-code-go)

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
