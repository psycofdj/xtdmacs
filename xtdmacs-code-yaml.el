;; -*- lexical-binding: t -*-

(require 'yaml-path)

(eval-when-compile
  (defvar xtdmacs-code-yaml-mode-map))

(defcustom xtdmacs-code-yaml-compile-alist
  '(("compile" .
     (("file"       . buffer-file-name)
      ("bin"        . "yamllint -f parsable -d '{extends: relaxed, rules: {indentation: {spaces: consistent}, line-length: {max: 300}}}'")
      ("get-params" . xtdmacs-compile++-current-file-params)
      ("command"    . xtdmacs-compile++-simple-file-command)))
    )
  "xtdmacs yaml compilation configuration"
  :group 'xtdmacs-code-yaml
  :safe '(lambda(p) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function))))
  )

(defun --xtdmacs-code-yaml-mode-construct()
  (unless (mode-enabled 'yafolding-mode)
    (yafolding-mode t))
  (when (mode-enabled 'which-function-mode)
    (yaml-path-which-func))
  (when (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "yaml-mode" xtdmacs-code-yaml-compile-alist))
  (message "enabled : xtdmacs-code-yaml-mode")
  )

(defun --xtdmacs-code-yaml-mode-destroy()
  (when (mode-enabled 'yafolding-mode)
    (yafolding-mode nil))
  (message "disabled : xtdmacs-code-yaml-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-yaml-mode
  "Code for yaml" nil "Code"
  '(("\C-e"    . yaml-path-at-point))
  (if xtdmacs-code-yaml-mode
      (--xtdmacs-code-yaml-mode-construct)
    (--xtdmacs-code-yaml-mode-destroy))
  )

(provide 'xtdmacs-code-yaml)
