;;; xtdmacs-code-web.el --- web-mode support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for web-mode buffers: HTML or golang templates.

;;; Code:

(require 'xtdmacs-code)

(eval-when-compile (require 'web-mode nil 'noerror))

(declare-function web-mode-element-beginning "web-mode")
(declare-function web-mode-element-end       "web-mode")

(use-package web-mode
  :ensure t)

(with-eval-after-load 'web-mode
  (define-key web-mode-map [C-M-up]   #'web-mode-element-beginning)
  (define-key web-mode-map [C-M-down] #'web-mode-element-end))

;;;###autoload
(defun xtdmacs-code-web-setup ()
  "Configure a web-mode buffer with xtdmacs conventions."
  (if (xtdmacs-helm-detect-p)
      (progn
        (xtdmacs-helm-init)
        (setq-local web-mode-engine "go")
        (xtdmacs-code-setup :with-lsp t))
    (progn
      (xtdmacs-code-setup))))

;;;###autoload
(add-hook 'web-mode-hook #'xtdmacs-code-web-setup)

(provide 'xtdmacs-code-web)

;;; xtdmacs-code-web.el ends here
