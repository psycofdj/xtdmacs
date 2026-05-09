;;; xtdmacs-code-terraform.el --- Terraform support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for terraform-mode buffers: LSP, yas, optional
;; format-on-save, and a `terraform validate' compile recipe.

;;; Code:

(require 'xtdmacs-code)

(declare-function xtdmacs-compile++-register-config "xtdmacs-compile++")
(declare-function --xtdmacs-compile++-get-value     "xtdmacs-compile++")

(defcustom xtdmacs-code-terraform-format-on-save nil
  "When non-nil, run lsp-format-buffer on save."
  :group 'xtdmacs-code-terraform :type 'boolean :safe #'booleanp)

(defun xtdmacs-code-terraform-command (type &optional mode)
  "Build the `terraform validate' command for compile entry TYPE in MODE."
  (let ((dir (--xtdmacs-compile++-get-value mode type :dir)))
    (format "cd %s && terraform validate" (funcall-or-value dir))))

(defcustom xtdmacs-code-terraform-compile-alist
  '((:compile . ((:dir        . xtdmacs-compile++-get-dir-buffer)
                 (:get-params . xtdmacs-compile++-only-dir)
                 (:command    . xtdmacs-code-terraform-command)))
    (:test    . ((:dir        . xtdmacs-compile++-get-dir-buffer)
                 (:get-params . xtdmacs-compile++-only-dir)
                 (:command    . xtdmacs-code-terraform-command))))
  "Terraform compilation configuration."
  :group 'xtdmacs-code-terraform
  :safe (lambda (_) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function)))))


;;;###autoload
(defun xtdmacs-code-terraform-setup ()
  "Configure a Terraform buffer with xtdmacs conventions."
  (xtdmacs-code-setup)
  (when (bound-and-true-p xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "terraform-mode" xtdmacs-code-terraform-compile-alist))
  (when xtdmacs-code-terraform-format-on-save
    (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

;;;###autoload
(add-hook 'terraform-mode-hook #'xtdmacs-code-terraform-setup)

;;;###autoload (put 'xtdmacs-code-terraform-compile-alist 'safe-local-variable (lambda (_) t))
;;;###autoload (put 'xtdmacs-code-terraform-format-on-save 'safe-local-variable #'booleanp)

(provide 'xtdmacs-code-terraform)

;;; xtdmacs-code-terraform.el ends here
