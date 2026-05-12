;;; xtdmacs-code-terraform.el --- Terraform support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for terraform-mode buffers: LSP, yas, optional
;; format-on-save, and a `terraform validate' compile recipe.

;;; Code:

(require 'xtdmacs-code)

(declare-function lsp-terraform-ls--modules-to-tf-module   "lsp-terraform")
(declare-function lsp-terraform-ls--providers-to-tf-package "lsp-terraform")
(defvar lsp-terraform-ls-server)
(defvar terraform-command)

(defcustom xtdmacs-code-terraform-format-on-save nil
  "When non-nil, run lsp-format-buffer on save."
  :group 'xtdmacs-code-terraform :type 'boolean :safe #'booleanp)

(defcustom xtdmacs-code-terraform-backend 'terraform
  "Backend to use for Terraform buffers.
When set to `tofu', the `lsp-terraform' module functions are
overridden to invoke `tofu-ls' commands instead of the default
`terraform-ls' ones."
  :group 'xtdmacs-code-terraform
  :type '(choice (const :tag "Terraform" terraform)
                 (const :tag "OpenTofu"  tofu))
  :safe #'symbolp)


(defvar xtdmacs-code-terraform--tofu-overrides-installed nil
  "Non-nil once tofu overrides have been installed into `lsp-terraform'.")

(defun xtdmacs-code-terraform--install-tofu-overrides ()
  "Override `lsp-terraform' module functions to use tofu commands."
  (unless xtdmacs-code-terraform--tofu-overrides-installed
    (setq xtdmacs-code-terraform--tofu-overrides-installed t)
    (with-eval-after-load 'lsp-terraform
      (setq lsp-terraform-ls-server "tofu-ls")
      (defun lsp-terraform-ls-validate ()
        "Execute terraform validate on project root."
        (interactive)
        (lsp-request
         "workspace/executeCommand"
         (list :command "tofu-ls.tofu.validate"
               :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root))))
               )
         :no-wait t
         :no-merge t))
      (defun lsp-terraform-ls-init ()
        "Execute terraform init on project root.
This is a synchronous action."
        (interactive)
        (lsp-request
         "workspace/executeCommand"
         (list :command "tofu-ls.tofu.init"
               :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root)))))
         :no-wait nil
         :no-merge t))
      (defun lsp-terraform-ls-version ()
        "Get information about the terraform binary version for the current module."
        (interactive)
        (let ((terraform-data (lsp-request
                               "workspace/executeCommand"
                               (list :command "tofu-ls.module.terraform"
                                     :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root))))))))
          (lsp--info "Required: %s, Current: %s"
                     (lsp:terraform-ls-module-terraform-required-version terraform-data)
                     (lsp:terraform-ls-module-terraform-discovered-version terraform-data))))
      (defun lsp-terraform-ls--fetch-modules-data (project-root)
        "Fetch modules data and set it in `lsp-terraform-ls--modules-call-tree-data'."
        (let* ((tree-data (lsp-request
                           "workspace/executeCommand"
                           (list :command "tofu-ls.module.calls"
                                 :arguments (vector (format "uri=%s" (lsp--path-to-uri project-root))))
                           :no-wait nil
                           :no-merge nil))
               (modules (lsp-terraform-ls--modules-to-tf-module tree-data)))
          (setq-local lsp-terraform-ls--modules-call-tree-data modules)))
      (defun lsp-terraform-ls--fetch-providers ()
        "Fetch modules call data and set it in `lsp-terraform-ls--providers-tree-data'."
        (let* ((tree-data (lsp-request
                           "workspace/executeCommand"
                           (list :command "tofu-ls.module.providers"
                                 :arguments (vector (format "uri=%s" (lsp--path-to-uri (lsp-workspace-root)))))
                           :no-wait nil
                           :no-merge nil))
               (tf-packages (lsp-terraform-ls--providers-to-tf-package tree-data)))
          (setq-local lsp-terraform-ls--providers-tree-data tf-packages))))))

(defface xtdmacs-code-terraform-face-data
  '((t (:foreground "#875f00" :weight bold))) "Data reference." :group 'xtdmacs-code-terraform)
(defface xtdmacs-code-terraform-face-module
  '((t (:foreground "#d70087"))) "Module reference." :group 'xtdmacs-code-terraform)
(defface xtdmacs-code-terraform-face-local
  '((t (:foreground "#7f7f7f"))) "Local reference." :group 'xtdmacs-code-terraform)
(defface xtdmacs-code-terraform-face-var
  '((t (:foreground "#87afff"))) "Variable reference." :group 'xtdmacs-code-terraform)

(defcustom xtdmacs-code-terraform-keywords-alist
  '(("\\<\\(module\\)\\."             (1 'xtdmacs-code-terraform-face-module))
    ("\\<\\(data\\)\\."               (1 'xtdmacs-code-terraform-face-data))
    ("\\<\\(local\\)\\."              (1 'xtdmacs-code-terraform-face-local))
    ("\\<\\(var\\)\\."                (1 'xtdmacs-code-terraform-face-var)))
  "Additional Python font-lock keywords."
  :group 'xtdmacs-code-python
  :safe (lambda (_) t)
  :type '(alist :key-type string :value-type sexp))

;;;###autoload
(defun xtdmacs-code-terraform-setup ()
  "Configure a Terraform buffer with xtdmacs conventions."
  (font-lock-add-keywords nil xtdmacs-code-terraform-keywords-alist)
  (when (eq xtdmacs-code-terraform-backend 'tofu)
    (setq terraform-command "tofu")
    (xtdmacs-code-terraform--install-tofu-overrides))
  (xtdmacs-code-setup :with-lsp t)
  (setq lsp-enable-links t)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.terraform.*\\'")
  (local-set-key (kbd "C-e") #'lsp-describe-thing-at-point)
  (when xtdmacs-code-terraform-format-on-save
    (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

;;;###autoload
(add-hook 'terraform-mode-hook #'xtdmacs-code-terraform-setup)

;;;###autoload (put 'xtdmacs-code-terraform-compile-alist 'safe-local-variable (lambda (_) t))
;;;###autoload (put 'xtdmacs-code-terraform-format-on-save 'safe-local-variable #'booleanp)

(provide 'xtdmacs-code-terraform)

;;; xtdmacs-code-terraform.el ends here
