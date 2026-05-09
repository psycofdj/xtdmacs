;;; xtdmacs-code-lisp.el --- Emacs Lisp support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for emacs-lisp-mode and lisp-mode buffers: optional
;; indent-on-load/save and auto-complete integration.

;;; Code:

(require 'xtdmacs-code)
(require 'auto-complete)

(defcustom xtdmacs-code-lisp-indent-load-auto t
  "Enables code auto-indentation on load."
  :group 'xtdmacs-code-lisp :type 'boolean :safe #'booleanp)

(defcustom xtdmacs-code-lisp-indent-save-auto t
  "Enables code auto-indentation on save."
  :group 'xtdmacs-code-lisp :type 'boolean :safe #'booleanp)

(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "M-.") #'ac-start)
  (define-key lisp-interaction-mode-map (kbd "M-.") #'ac-start))

;;;###autoload
(defun xtdmacs-code-lisp-setup ()
  "Configure a Lisp buffer with xtdmacs conventions."
  (xtdmacs-code-setup)
  (when xtdmacs-code-lisp-indent-save-auto
    (add-hook 'before-save-hook #'xtdmacs-code-format-buffer-with-ident nil t))
  (when xtdmacs-code-lisp-indent-load-auto
    (xtdmacs-code-format-buffer-with-ident))
  (auto-complete-mode 1))

;;;###autoload
(add-hook 'emacs-lisp-mode-hook #'xtdmacs-code-lisp-setup)
;;;###autoload
(add-hook 'lisp-mode-hook       #'xtdmacs-code-lisp-setup)

(provide 'xtdmacs-code-lisp)

;;; xtdmacs-code-lisp.el ends here
