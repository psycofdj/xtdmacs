(require 'yaml-path)

(eval-when-compile
  (defvar xtdmacs-code-yaml-mode-map))

(defcustom xtdmacs-code-yaml-compile-alist
  '(("compile" .
     (("file"       . buffer-file-name)
      ("bin"        . "yamllint -f parsable -d '{extends: relaxed, rules: {indentation: {spaces: consistent}, line-length: {max: 300}}}'")
      ("get-params" . xtdmacs-code-yaml-get-params)
      ("command"    . xtdmacs-code-yaml-command)))
    )
  "xtdmacs yaml compilation configuration"
  :group 'xtdmacs-code-yaml
  :safe '(lambda(p) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function))))
  )

(defun xtdmacs-code-yaml-get-params (type &optional mode)
  (let* ((bin (--xtdmacs-compile++-prompt-value mode type "bin" "Binary")))
    (xtdmacs-compile++-query-local)
    (--xtdmacs-compile++-set-value mode type "bin" bin))
  )

(defun xtdmacs-code-yaml-command (type &optional mode)
  (let* ((file   (--xtdmacs-compile++-get-value mode type "file"))
         (bin    (--xtdmacs-compile++-get-value mode type "bin")))
    (format "%s %s"
            (funcall-or-value bin)
            (funcall-or-value file)))
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
