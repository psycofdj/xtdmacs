;;; xtdmacs-code-json.el --- JSON support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for json-mode buffers: indent level, jsonlint compile
;; configuration, and base xtdmacs-code-setup.

;;; Code:

(require 'xtdmacs-code)
(require 'js)

(declare-function xtdmacs-compile++-register-config "xtdmacs-compile++")

(use-package json-mode
  :ensure t)

(defcustom xtdmacs-code-json-compile-alist
  '((:compile . ((:file       . buffer-file-name)
                 (:bin        . "jsonlint-php -q")
                 (:get-params . xtdmacs-compile++-current-file-params)
                 (:command    . xtdmacs-compile++-simple-file-command))))
  "JSON compilation configuration."
  :group 'xtdmacs-code-json
  :safe (lambda (_) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function)))))

;;;###autoload
(defun xtdmacs-code-json-setup ()
  "Configure a JSON buffer with xtdmacs conventions."
  (xtdmacs-code-setup)
  (setq-local js-indent-level 2)
  (when (bound-and-true-p xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "json-mode" xtdmacs-code-json-compile-alist)))

;;;###autoload
(add-hook 'json-mode-hook #'xtdmacs-code-json-setup)

(provide 'xtdmacs-code-json)

;;; xtdmacs-code-json.el ends here
