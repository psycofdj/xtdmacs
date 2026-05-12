;;; xtdmacs-helm.el --- web-mode support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for helm charts.

;;; Code:

(require 'lsp-mode)
(require 'lsp-kubernetes-helm)

;;;###autoload
(defun xtdmacs-helm-init ()
  "Register helm-ls."
  (add-to-list 'lsp-language-id-configuration '(yaml-ts-mode . "helm-ls"))
  (add-to-list 'lsp-language-id-configuration '(web-mode . "helm-ls")))

;;;###autoload
(defun xtdmacs-helm-detect-p ()
  "Return non-nil if the current buffer's file belongs to a Helm chart."
  (when-let* ((file (buffer-file-name))
              (dir  (file-name-directory file))
              (chart-root (locate-dominating-file dir "Chart.yaml"))
              (relative (file-relative-name
                         (expand-file-name file)
                         (expand-file-name chart-root))))
    (or (string= relative "Chart.yaml")
        (string= relative "values.yaml")
        (string-match-p "\\`templates/[^/]+\\.ya?ml\\'"  relative)
        (string-match-p "\\`templates/_[^/]*\\.tpl\\'" relative))))


(provide 'xtdmacs-helm)

;;; xtdmacs-helm.el ends here
