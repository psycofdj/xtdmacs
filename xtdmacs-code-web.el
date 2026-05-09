;;; xtdmacs-code-web.el --- web-mode support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for web-mode buffers: HTML comment delimiters,
;; markup indent offset, and element-traversal key bindings.

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
  (xtdmacs-code-setup)
  (setq-local comment-start "<!--")
  (setq-local comment-end   "-->")
  (setq web-mode-markup-indent-offset 2))

;;;###autoload
(add-hook 'web-mode-hook #'xtdmacs-code-web-setup)

(provide 'xtdmacs-code-web)

;;; xtdmacs-code-web.el ends here
