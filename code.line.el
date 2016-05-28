(defun --code-line-mode-construct()
  (unless (mode-enabled 'which-function-mode)
    (which-function-mode t))
  (setq mode-line-format
        (list
         ;; value of current buffer name
         "%&%&%& "
         '(:propertize "%b " face bold)
         "- (%l:%c) - %p - ["
         '(:propertize (:eval (replace-regexp-in-string
                               "%" "%%"
                               (or (gethash (selected-window) which-func-table)
                                   which-func-unknown))) face which-func)
         "] %-"
         ))
  (message "enabled : code-line-mode")
  )

(defun --code-line-mode-destroy()
  (when (mode-enabled 'which-function-mode)
    (which-function-mode nil))
  (message "disabled : code-line-mode")
  )


;;;###autoload
(define-minor-mode code-line-mode
  "Specialized modeline" nil "Code" nil
  (if code-line-mode
      (--code-line-mode-construct)
    (--code-line-mode-destroy))
  )
