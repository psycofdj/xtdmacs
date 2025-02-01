(require 'xtdmacs-compile++)
(require 'package)
(require 'terraform-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (terraform-mode . lsp-deferred))

(use-package company
  :ensure t
  :config
  )

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (terraform-mode . yas-minor-mode))

(defcustom xtdmacs-code-terraform-indent-load-auto
  nil
  "Enables terraform code auto-indentation on load."
  :group 'xtdmacs-code-terraform
  :type 'boolean
  :safe 'booleanp)

(defcustom xtdmacs-code-terraform-indent-save-auto
  nil
  "Enables terraform code auto-indentation on save."
  :group 'xtdmacs-code-terraform
  :type 'boolean
  :safe 'booleanp
  )


(defun xtdmacs-code-terraform-command (type &optional mode)
  (let* ((dir    (--xtdmacs-compile++-get-value mode type :dir)))
    (format "cd %s && terraform validate"
            (funcall-or-value dir)))
  )

(defcustom xtdmacs-code-terraform-compile-alist
  '((:compile . ((:dir        . xtdmacs-compile++-get-dir-buffer)
                 (:get-params . xtdmacs-compile++-only-dir)
                 (:command    . xtdmacs-code-terraform-command)))

    (:test .    ((:dir        . xtdmacs-compile++-get-dir-buffer)
                 (:get-params . xtdmacs-compile++-only-dir)
                 (:command    . xtdmacs-code-terraform-command))))
  "Xtdmacs-Code-terraform compilation configuration"
  :group 'xtdmacs-code-terraform
  :safe '(lambda(p) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function))))
  )

(defun --xtdmacs-code-terraform-construct()
  (yas-minor-mode t)
  (lsp)
  (define-key terraform-mode-map (kbd "<f12>")   'lsp-find-definition)
  (define-key terraform-mode-map (kbd "C-<f12>") '--xtdmacs-lsp-find-definition-other-window)
  (define-key terraform-mode-map (kbd "<f11>")   '--xtdmacs-lsp-find-references)
  (define-key terraform-mode-map (kbd "C-<f11>") '--xtdmacs-lsp-find-references-other-window)

  (when (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "terraform-mode" xtdmacs-code-terraform-compile-alist))

  (if xtdmacs-code-terraform-indent-save-auto
      (progn
        (add-hook 'before-save-hook #'lsp-format-buffer t t)))

  (if xtdmacs-code-terraform-indent-load-auto
      (progn
        (add-hook 'before-save-hook #'lsp-format-buffer t t)))
  (message "enabled : xtdmacs-code-terraform-mode")
  )

(defun --xtdmacs-code-terraform-destroy()
  (if xtdmacs-code-terraform-indent-save-auto
      (remove-hook 'before-save-hook '(lambda() (xtdmacs-code-format-buffer t nil))))
  (when (mode-enabled 'yas-minor-mode)
    (yas-minor-mode nil))
  (message "disabled : xtdmacs-code-terraform-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-terraform-mode
  "Code for Terraform" nil "Code"
  '(("\M-t"    . lsp-format-region)
    ("\C-\M-t" . lsp-format-buffer)
    ("\M-."    . company-complete)
    )
  (if xtdmacs-code-terraform-mode
      (--xtdmacs-code-terraform-construct)
    (--xtdmacs-code-terraform-destroy))
  )


;; --------------------------------------------------------------------------- ;

;;;###autoload
(put 'xtdmacs-code-terraform-compile-alist 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-terraform-indent-load-auto 'safe-local-variable 'booleanp)
;;;###autoload
(put 'xtdmacs-code-terraform-indent-save-auto 'safe-local-variable 'booleanp)

(provide 'xtdmacs-code-terraform)
