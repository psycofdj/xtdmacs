(defun --code-json-mode-construct()
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2)
  (unless (mode-enabled 'yafolding-mode)
    (yafolding-mode t))
  (define-key code-json-mode-map (kbd "C-c C-f")  'yafolding-toggle-element)
  (message "enabled : code-json-mode")
  )

(defun --code-json-mode-destroy()
  (kill-local-variable 'js-indent-level)
  (message "disabled : code-json-mode")
  )

;;;###autoload
(define-minor-mode code-json-mode "Code for json" nil "Code"
  '()
  (if code-json-mode
      (--code-json-mode-construct)
    (--code-json-mode-destroy))
  )
