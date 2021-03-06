;; -*- lexical-binding: t -*-
(require 'auto-complete)

(defcustom xtdmacs-code-lisp-indent-load-auto t
  "Enables code auto-indentation on load."
  :group 'xtdmacs-code-lisp
  :type 'boolean
  :safe 'booleanp)

(defcustom xtdmacs-code-lisp-indent-save-auto t
  "Enables code auto-indentation on save."
  :group 'xtdmacs-code-lisp
  :type 'boolean
  :safe 'booleanp)

(defun --xtdmacs-code-lisp-mode-construct()
  (when xtdmacs-code-lisp-indent-save-auto
    (add-hook 'before-save-hook 'xtdmacs-code-format-buffer-with-ident t t))
  (when xtdmacs-code-lisp-indent-load-auto
    (xtdmacs-code-format-buffer-with-ident))
  (unless (mode-enabled 'auto-complete-mode)
    (auto-complete-mode t))
  (message "enabled : xtdmacs-code-lisp-mode")
  )

(defun --xtdmacs-code-lisp-mode-destroy()
  (when (mode-enabled 'auto-complete-mode)
    (auto-complete-mode nil))
  (when xtdmacs-code-lisp-indent-save-auto
    (remove-hook 'before-save-hook 'xtdmacs-code-format-buffer-with-ident t))
  (message "disabled : xtdmacs-code-lisp-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-lisp-mode "Code for Lisp" nil "Code"
  '(("\M-."    . ac-start))
  (if xtdmacs-code-lisp-mode
      (--xtdmacs-code-lisp-mode-construct)
    (--xtdmacs-code-lisp-mode-destroy))
  )

(provide 'xtdmacs-code-lisp)
