;;; xtdmacs-code-line.el --- Custom mode-line with which-function  -*- lexical-binding: t -*-

;;; Commentary:

;; Installs a buffer-local mode-line that includes the current
;; which-function alongside buffer name, line and column.

;;; Code:

(defcustom xtdmacs-code-line-format
  (list "%& "
        '(:propertize "%b " face mode-line-buffer-id)
        "- (%l:%c) - ["
        '(:propertize (:eval (replace-regexp-in-string
                              "%" "%%"
                              (or (gethash (selected-window) which-func-table)
                                  which-func-unknown)))
                      face which-func)
        "] %-")
  "Buffer-local mode-line format installed by `xtdmacs-code-line-setup'."
  :group 'xtdmacs-code-line
  :type 'sexp)

;;;###autoload
(defun xtdmacs-code-line-setup ()
  "Install the xtdmacs mode-line in the current buffer.
Also enables `which-function-mode' globally if it isn't already."
  (unless (bound-and-true-p which-function-mode)
    (which-function-mode 1))
  (setq mode-line-format xtdmacs-code-line-format))

(provide 'xtdmacs-code-line)

;;; xtdmacs-code-line.el ends here
