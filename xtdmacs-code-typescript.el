;; -*- lexical-binding: t -*-

(require 'xtdmacs-compile++)
(require 'package)
(require 'typescript-mode)
(require 'yasnippet)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (typescript-mode . lsp-deferred))

(use-package company
  :ensure t
  :config
  )

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (typescript-mode . yas-minor-mode))

(defface xtdmacs-code-typescript-face-indent-error
  '((t (:foreground "color-124" :underline t)))
  "Indicates spaces instead of tabs for indentation"
  :group 'xtdmacs-code-typescript
  )

(defcustom xtdmacs-code-typescript-compile-alist
  '((:compile . ((:dir        . xtdmacs-compile++-get-dir-git)
                 (:bin        . "npm run tsc:abs")
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))

    (:test .    ((:dir        . xtdmacs-compile++-get-dir-git)
                 (:bin        . "npm run test")
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))

    (:deploy .   ((:dir        . xtdmacs-compile++-get-dir-git)
                  (:bin        . "npm run lint:fix")
                  (:env        . "")
                  (:get-params . xtdmacs-compile++-default-params)
                  (:command    . xtdmacs-compile++-default-command))))
  "Xtdmacs-Code-typescript compilation configuration"
  :group 'xtdmacs-code-typescript
  :safe '(lambda(p) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function))))
  )

(defcustom xtdmacs-code-typescript-keywords-alist
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
    )
  "List of additional typescript font-lock keywords"
  :group 'xtdmacs-code-typescript
  :safe '(lambda(p) t))

(defcustom xtdmacs-code-typescript-indent-load-auto
  nil
  "Enables typescript code auto-indentation on load."
  :group 'xtdmacs-code-typescript
  :type 'boolean
  :safe 'booleanp)

(defcustom xtdmacs-code-typescript-indent-save-auto
  nil
  "Enables typescript code auto-indentation on save."
  :group 'xtdmacs-code-typescript
  :type 'boolean
  :safe 'booleanp
  )

;; --------------------------------------------------------------------------- ;

(defun xtdmacs-code-typescript-get-project-name ()
  (file-name-nondirectory
   (directory-file-name
    (xtdmacs-compile++-get-dir-git)))
  )

;; --------------------------------------------------------------------------- ;

(defun --xtdmacs-code-typescript-construct()
  (yas-minor-mode t)
  (lsp)
  (font-lock-add-keywords nil xtdmacs-code-typescript-keywords-alist)
  ;; (define-key typescript-mode-map (kbd "<f12>")   'lsp-find-definition)
  ;; (define-key typescript-mode-map (kbd "C-<f12>") '--xtdmacs-lsp-find-definition-other-window)
  ;; (define-key typescript-mode-map (kbd "<f11>")   '--xtdmacs-lsp-find-references)
  ;; (define-key typescript-mode-map (kbd "C-<f11>") '--xtdmacs-lsp-find-references-other-window)
  ;; (define-key typescript-mode-map (kbd "<f10>") 'lsp-ui-doc-glance)
  ;; (define-key typescript-mode-map (kbd "C-<f10>") 'lsp-ui-imenu)
  ;; (define-key typescript-mode-map (kbd "C-e <f12>") 'dap-debug)
  ;; (define-key typescript-mode-map (kbd "C-e s") 'dap-step-in)
  ;; (define-key typescript-mode-map (kbd "C-e o") 'dap-step-out)
  ;; (define-key typescript-mode-map (kbd "C-e n") 'dap-next)
  ;; (define-key typescript-mode-map (kbd "C-e c") 'dap-continue)
  ;; (define-key typescript-mode-map (kbd "C-e r") 'dap-debug-restart)
  ;; (define-key typescript-mode-map (kbd "C-e b") 'dap-breakpoint-toggle)
  (define-key xtdmacs-code-typescript-mode-map (kbd "<f12>")   'lsp-find-definition)
  (define-key xtdmacs-code-typescript-mode-map (kbd "C-<f12>") '--xtdmacs-lsp-find-definition-other-window)
  (define-key xtdmacs-code-typescript-mode-map (kbd "<f11>")   '--xtdmacs-lsp-find-references)
  (define-key xtdmacs-code-typescript-mode-map (kbd "C-<f11>") '--xtdmacs-lsp-find-references-other-window)
  (define-key xtdmacs-code-typescript-mode-map (kbd "<f10>") 'lsp-ui-doc-glance)
  (define-key xtdmacs-code-typescript-mode-map (kbd "C-<f10>") 'lsp-ui-imenu)
  (define-key xtdmacs-code-typescript-mode-map (kbd "C-e <f12>") 'dap-debug)
  (define-key xtdmacs-code-typescript-mode-map (kbd "C-e s") 'dap-step-in)
  (define-key xtdmacs-code-typescript-mode-map (kbd "C-e o") 'dap-step-out)
  (define-key xtdmacs-code-typescript-mode-map (kbd "C-e n") 'dap-next)
  (define-key xtdmacs-code-typescript-mode-map (kbd "C-e c") 'dap-continue)
  (define-key xtdmacs-code-typescript-mode-map (kbd "C-e r") 'dap-debug-restart)
  (define-key xtdmacs-code-typescript-mode-map (kbd "C-e b") 'dap-breakpoint-toggle)

  (when (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "typescript-mode" xtdmacs-code-typescript-compile-alist))

  (if xtdmacs-code-typescript-indent-save-auto
      (progn
        (add-hook 'before-save-hook #'lsp-organize-imports t t)
        (add-hook 'before-save-hook #'lsp-format-buffer t t)))

  (if xtdmacs-code-typescript-indent-load-auto
      (progn
        (add-hook 'before-save-hook #'lsp-organize-imports t t)
        (add-hook 'before-save-hook #'lsp-format-buffer t t)))

  (message "enabled : xtdmacs-code-typescript-mode")
  )

(defun --xtdmacs-code-typescript-destroy()
  (if xtdmacs-code-typescript-indent-save-auto
      (remove-hook 'before-save-hook '(lambda() (xtdmacs-code-format-buffer t nil))))
  (when (mode-enabled 'yas-minor-mode)
    (yas-minor-mode nil))
  (font-lock-remove-keywords nil xtdmacs-code-typescript-keywords-alist)
  (message "disabled : xtdmacs-code-typescript-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-typescript-mode
  "Code for Typescript" nil "Code"
  '(("\M-t"    . lsp-format-region)
    ("\C-\M-t" . lsp-format-buffer)
    ("\M-r"    . lsp-rename)
    ("\M-."    . company-complete)
    )
  (if xtdmacs-code-typescript-mode
      (--xtdmacs-code-typescript-construct)
    (--xtdmacs-code-typescript-destroy))
  )


;; --------------------------------------------------------------------------- ;

;;;###autoload
(put 'xtdmacs-code-typescript-compile-alist 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-typescript-keywords-alist 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-typescript-indent-load-auto 'safe-local-variable 'booleanp)
;;;###autoload
(put 'xtdmacs-code-typescript-indent-save-auto 'safe-local-variable 'booleanp)

(provide 'xtdmacs-code-typescript)

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
