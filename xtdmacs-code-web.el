(defun --xtdmacs-code-web-mode-construct()
  (set (make-local-variable 'comment-start)  "<!--")
  (set (make-local-variable 'comment-end)     "-->")
  (message "enabled : xtdmacs-code-web-mode")
  )

(defun --xtdmacs-code-web-mode-destroy()
  (kill-local-variable 'comment-start)
  (kill-local-variable 'comment-end)
  (message "disabled : xtdmacs-code-web-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-web-mode "Code for web" nil "Code Web"
  '(([C-M-up]   . web-mode-element-beginning)
    ([C-M-down] . web-mode-element-end))

  (if xtdmacs-code-web-mode
      (--xtdmacs-code-web-mode-construct)
    (--xtdmacs-code-web-mode-destroy))
  )

(provide 'xtdmacs-code-web)
