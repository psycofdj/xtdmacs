(defun --code-web-mode-construct()
  (set (make-local-variable 'comment-start)  "<!--")
  (set (make-local-variable 'comment-end)     "-->")
  (message "enabled : code-web-mode")
  )

(defun --code-web-mode-destroy()
  (kill-local-variable 'comment-start)
  (kill-local-variable 'comment-end)
  (message "disabled : code-web-mode")
  )

;;;###autoload
(define-minor-mode code-web-mode "Code for web" nil "Code Web"
  '(([C-M-up]   . web-mode-element-beginning)
    ([C-M-down] . web-mode-element-end))

  (if code-web-mode
      (--code-web-mode-construct)
    (--code-web-mode-destroy))
  )
