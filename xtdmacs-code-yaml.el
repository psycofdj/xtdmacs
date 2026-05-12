;;; xtdmacs-code-yaml.el --- YAML support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for yaml-ts-mode buffers: LSP via yaml-lsp (when
;; available) and a yamllint compile recipe.
;; Requires libtree-sitter-yaml.so, you can use M-x treesit-install-language-grammar RET yaml RET
;; to install from source.  Repo: https://github.com/ikatyang/tree-sitter-yaml.

;;; Code:

(require 'xtdmacs-code)

(declare-function xtdmacs-compile++-register-config "xtdmacs-compile++")
(declare-function yaml-lsp-which-func-mode          "yaml-lsp")
(declare-function yaml-lsp-reload                   "yaml-lsp")

(use-package yaml-lsp
  :commands (yaml-lsp-which-func-mode yaml-lsp-reload)
  :bind (:map lsp-mode-map
              ("C-e" . yaml-lsp-copy-address-at-point)
              ("M-f" . yaml-lsp-element-toggle)
              ("C-M-f" . yaml-lsp-region-toggle)))

(defcustom xtdmacs-code-yaml-compile-alist
  '((:compile . ((:file       . buffer-file-name)
                 (:bin        . "yamllint -f parsable -d '{extends: relaxed, rules: {indentation: {spaces: consistent}, line-length: {max: 300}}}'")
                 (:get-params . xtdmacs-compile++-current-file-params)
                 (:command    . xtdmacs-compile++-simple-file-command))))
  "YAML compilation configuration."
  :group 'xtdmacs-code-yaml
  :safe (lambda (_) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function)))))

(defun xtdmacs-code-yaml--init-yaml-lsp ()
  "Set up yaml-lsp in the current buffer."
  (yaml-lsp-which-func-mode 1)
  (yaml-lsp-reload)
  ;; override yafolding-mode-map bindings defined in xtdmacs-code
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-e")   #'yaml-lsp-copy-address-at-point)
    (define-key map (kbd "C-f")   #'yaml-lsp-element-toggle)
    (define-key map (kbd "C-M-f") #'yaml-lsp-region-toggle)
    (push (cons 'yafolding-mode map) minor-mode-overriding-map-alist)))

;;;###autoload
(defun xtdmacs-code-yaml-setup ()
  "Configure a YAML buffer with xtdmacs conventions."
  (if (xtdmacs-helm-detect-p)
      (xtdmacs-helm-init)
    (xtdmacs-code-yaml--init-yaml-lsp))
  (xtdmacs-code-setup :with-lsp t)
  (when (bound-and-true-p xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "yaml-ts-mode" xtdmacs-code-yaml-compile-alist)))

;;;###autoload
(defun xtdmacs-code-yaml-force-yaml-lsp ()
  "Re-initialize the current YAML buffer with yaml-lsp instead of helm-ls.
Useful in Helm chart buffers where the default setup picks helm-ls."
  (interactive)
  (unless (derived-mode-p 'yaml-ts-mode 'yaml-mode)
    (user-error "Not a YAML buffer"))
  (when (bound-and-true-p lsp-mode)
    (lsp-disconnect))
  (xtdmacs-code-yaml--init-yaml-lsp)
  (lsp-deferred))

;;;###autoload
(add-hook 'yaml-ts-mode-hook #'xtdmacs-code-yaml-setup)

;;;###autoload (put 'xtdmacs-code-yaml-compile-alist 'safe-local-variable (lambda (_) t))

(provide 'xtdmacs-code-yaml)

;;; xtdmacs-code-yaml.el ends here
