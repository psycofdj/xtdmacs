;; -*- lexical-binding: t -*-

(eval-when-compile
  (defvar xtdmacs-code-yaml-mode-map))

(defcustom xtdmacs-code-yaml-compile-alist
  '((:compile . ((:file       . buffer-file-name)
                 (:bin        . "yamllint -f parsable -d '{extends: relaxed, rules: {indentation: {spaces: consistent}, line-length: {max: 300}}}'")
                 (:get-params . xtdmacs-compile++-current-file-params)
                 (:command    . xtdmacs-compile++-simple-file-command)))
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

  (if (require 'yaml-path nil 'noerror)
      (when (mode-enabled 'which-function-mode)
        (yaml-path-which-func))
    (message "package yaml-path not found, which-func function is disabled"))

  (if (require 'paas-manifest-helper nil 'noerror)
      (when (paas-manifest-helper-is-manifest)
        (define-key xtdmacs-code-yaml-mode-map [f12]           'paas-manifest-helper-open-at-point)
        (define-key xtdmacs-code-yaml-mode-map (kbd "C-<f12>") '(lambda () (interactive) (paas-manifest-helper-open-at-point t)))
        (define-key xtdmacs-code-yaml-mode-map "\C-e"          'paas-manifest-helper-print-at-point)))

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
