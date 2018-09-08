;; -*- lexical-binding: t -*-

(defun --xtdmacs-code-makefile-mode-construct()
  (highlight-regexp "\t" 'hi-yellow)
  (delete-trailing-whitespace)
  (message "enabled : xtdmacs-code-makefile-mode")
  )

(defun --xtdmacs-code-makefile-mode-destroy()
  (unhighlight-regexp "\t")
  (message "disabled : xtdmacs-code-makefile-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-makefile-mode "Code for Makefiles" nil "Code"
  nil

  (if xtdmacs-code-makefile-mode
      (--xtdmacs-code-makefile-mode-construct)
    (--xtdmacs-code-makefile-mode-destroy))
  )

(provide 'xtdmacs-code-makefile)
