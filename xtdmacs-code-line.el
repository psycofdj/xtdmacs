(defun --xtdmacs-code-line-mode-construct()
  (unless (mode-enabled 'which-function-mode)
    (which-function-mode t))
  (setq mode-line-format
        (list
         ;; value of current buffer name
         "%& "
         '(:propertize "%b " face mode-line-buffer-id)
         "- (%l:%c) - ["
         '(:propertize (:eval (replace-regexp-in-string
                               "%" "%%"
                               (or (gethash (selected-window) which-func-table)
                                   which-func-unknown))) face which-func)
         "] %-"
         ))
  (message "enabled : xtdmacs-code-line-mode")
  )

(defun --xtdmacs-code-line-mode-destroy()
  (when (mode-enabled 'which-function-mode)
    (which-function-mode nil))
  (message "disabled : xtdmacs-code-line-mode")
  )


;;;###autoload
(define-minor-mode xtdmacs-code-line-mode
  "Specialized modeline" nil "Code"
  '()
  (if xtdmacs-code-line-mode
      (--xtdmacs-code-line-mode-construct)
    (--xtdmacs-code-line-mode-destroy))
  )

(provide 'xtdmacs-code-line)
