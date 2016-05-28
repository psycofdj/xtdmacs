(defcustom code-lisp-indent-load-auto t "Enables code auto-indentation on load." :group 'code-lisp :type 'boolean :safe 'booleanp)
(defcustom code-lisp-indent-save-auto t "Enables code auto-indentation on save." :group 'code-lisp :type 'boolean :safe 'booleanp)

(defun --code-lisp-mode-construct()
  (when code-lisp-indent-save-auto
    (add-hook 'before-save-hook 'code-format-buffer-with-ident t t))
  (when code-lisp-indent-load-auto
    (code-format-buffer-with-ident))
  (message "enabled : code-lisp-mode")
  )

(defun --code-lisp-mode-destroy()
  (when code-lisp-indent-save-auto
    (remove-hook 'before-save-hook 'code-format-buffer-with-ident t))
  (message "disabled : code-lisp-mode")
  )

;;;###autoload
(define-minor-mode code-lisp-mode "Code for Lisp" nil "Code" nil
  (if code-lisp-mode
      (--code-lisp-mode-construct)
    (--code-lisp-mode-destroy))
  )
