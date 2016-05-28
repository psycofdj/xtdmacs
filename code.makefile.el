(defun --code-makefile-mode-construct()
  (highlight-regexp "\t" 'hi-yellow)
  (delete-trailing-whitespace)
  (message "enabled : code-makefile-mode")
  )

(defun --code-makefile-mode-destroy()
  (unhighlight-regexp "\t")
  (message "disabled : code-makefile-mode")
  )

;;;###autoload
(define-minor-mode code-makefile-mode "Code for Makefiles" nil "Code"
  nil

  (if code-makefile-mode
      (--code-makefile-mode-construct)
    (--code-makefile-mode-destroy))
  )
