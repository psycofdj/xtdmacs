(require 'js)

(eval-when-compile
  (defvar xtdmacs-code-json-mode-map))


(defcustom xtdmacs-code-json-compile-alist
  '(("compile" .
     (("file"       . buffer-file-name)
      ("bin"        . "jsonlint-php -q")
      ("get-params" . xtdmacs-compile++-current-file-params)
      ("command"    . xtdmacs-compile++-simple-file-command)))
    )
  "xtdmacs json compilation configuration"
  :group 'xtdmacs-code-json
  :safe '(lambda(p) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function))))
  )

(defun --xtdmacs-code-json-mode-construct()
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2)
  (when (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "json-mode" xtdmacs-code-json-compile-alist))
  (message "enabled : xtdmacs-code-json-mode")
  )

(defun --xtdmacs-code-json-mode-destroy()
  (kill-local-variable 'js-indent-level)
  (message "disabled : xtdmacs-code-json-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-json-mode "Code for json" nil "Code" nil
  (if xtdmacs-code-json-mode
      (--xtdmacs-code-json-mode-construct)
    (--xtdmacs-code-json-mode-destroy))
  )

(provide 'xtdmacs-code-json)
