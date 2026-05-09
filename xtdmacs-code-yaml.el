;;; xtdmacs-code-yaml.el --- YAML support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for yaml-mode buffers: LSP via yaml-lsp (when
;; available) and a yamllint compile recipe.

;;; Code:

(require 'xtdmacs-code)

(eval-when-compile
  (when (locate-library "yaml-lsp") (require 'yaml-lsp)))

(declare-function xtdmacs-compile++-register-config "xtdmacs-compile++")
(declare-function yaml-lsp-which-func-mode          "yaml-lsp")
(declare-function yaml-lsp-reload                   "yaml-lsp")

(use-package yaml-lsp
  :load-path "~/dev/yaml-lsp/emacs"
  ;; :if (locate-library "yaml-lsp")
  :hook ((yaml-mode    . yaml-lsp-which-func-mode)
         (yaml-ts-mode . yaml-lsp-which-func-mode))
  :bind (:map lsp-mode-map
              ("C-e" . yaml-lsp-copy-address-at-point)))

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

;;;###autoload
(defun xtdmacs-code-yaml-setup ()
  "Configure a YAML buffer with xtdmacs conventions."
  (yaml-lsp-which-func-mode 1)
  (yaml-lsp-reload)
  (xtdmacs-code-setup)
  (when (bound-and-true-p xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "yaml-mode" xtdmacs-code-yaml-compile-alist)))

;;;###autoload
(add-hook 'yaml-mode-hook #'xtdmacs-code-yaml-setup)

;;;###autoload (put 'xtdmacs-code-yaml-compile-alist 'safe-local-variable (lambda (_) t))

(provide 'xtdmacs-code-yaml)

;;; xtdmacs-code-yaml.el ends here
