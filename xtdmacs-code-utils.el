;; Remplace les tabulations dans tout le buffer
;;;###autoload
(defun xtdmacs-code-untabify-buffer ()
  (interactive)
  (untabify 0 (point-max))
  )

;; Idente le buffer
;;;###autoload
(defun xtdmacs-code-indent-buffer ()
  (interactive)
  (if (< (count-lines (point-min) (point-max)) xtdmacs-code-indent-max-lines)
      (indent-region (point-min) (point-max)))
  )

;; Supprime les trailing whitespace et supprime les tabulations
;;;###autoload
(defun xtdmacs-code-format-buffer-without-ident()
  (interactive)
  (delete-trailing-whitespace)
  (xtdmacs-code-untabify-buffer)
  )

;; Supprime les trailing whitespace, indente et supprime les tabulations
;;;###autoload
(defun xtdmacs-code-format-buffer-with-ident()
  (interactive)
  (delete-trailing-whitespace)
  (xtdmacs-code-indent-buffer)
  (xtdmacs-code-untabify-buffer)
  )

;;;###autoload
(defun xtdmacs-code-align-vars ()
  (interactive)
  (setq old-case-fold case-fold-search)
  (setq case-fold-search nil)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\b\\(\\($\\)?[lpmcg][cs]?[cs]?\\([A-Z]\\|_\\).*\\)\\b" 1 1 0)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)=" 1 1 0)
  (setq case-fold-search old-case-fold)
  )

;;;###autoload
(defun xtdmacs-code-align-args ()
  (interactive)
  (align-regexp (region-beginning) (region-end) ",\\(\\s-*\\)" 1 1 t)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)<<" 1 1 t)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)=>" 1 1 t)
  )

(provide 'xtdmacs-code-utils)
