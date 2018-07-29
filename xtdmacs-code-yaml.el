(require 'yaml-path)

(eval-when-compile
  (defvar xtdmacs-code-yaml-mode-map))

(defun --xtdmacs-code-yaml-mode-construct()
  (unless (mode-enabled 'yafolding-mode)
    (yafolding-mode t))
  (when (mode-enabled 'which-function-mode)
    (yaml-path-which-func))
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
