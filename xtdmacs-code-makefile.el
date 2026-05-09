;;; xtdmacs-code-makefile.el --- Makefile support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for makefile-mode buffers: highlights tabs and strips
;; trailing whitespace on entry.

;;; Code:

(require 'xtdmacs-code)

;;;###autoload
(defun xtdmacs-code-makefile-setup ()
  "Configure a Makefile buffer with xtdmacs conventions."
  (xtdmacs-code-setup)
  (highlight-regexp "\t" 'hi-yellow)
  (delete-trailing-whitespace))

;;;###autoload
(add-hook 'makefile-mode-hook #'xtdmacs-code-makefile-setup)

(provide 'xtdmacs-code-makefile)

;;; xtdmacs-code-makefile.el ends here
