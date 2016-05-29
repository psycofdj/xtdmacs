(defun --xtdmacs-code-json-mode-construct()
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2)
  (unless (mode-enabled 'yafolding-mode)
    (yafolding-mode t))
  (define-key xtdmacs-code-json-mode-map (kbd "C-c C-f")  'yafolding-toggle-element)
  (message "enabled : xtdmacs-code-json-mode")
  )

(defun --xtdmacs-code-json-mode-destroy()
  (kill-local-variable 'js-indent-level)
  (message "disabled : xtdmacs-code-json-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-json-mode "Code for json" nil "Code"
  '()
  (if xtdmacs-code-json-mode
      (--xtdmacs-code-json-mode-construct)
    (--xtdmacs-code-json-mode-destroy))
  )

(provide 'xtdmacs-code-json)
