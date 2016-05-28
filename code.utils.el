;; Remplace les tabulations dans tout le buffer
;;;###autoload
(defun code-untabify-buffer ()
  (interactive)
  (untabify 0 (point-max))
  )

;; Idente le buffer
;;;###autoload
(defun code-indent-buffer ()
  (interactive)
  (if (< (count-lines (point-min) (point-max)) code-indent-max-lines)
      (indent-region (point-min) (point-max)))
  )

;; Supprime les trailing whitespace et supprime les tabulations
;;;###autoload
(defun code-format-buffer-without-ident()
  (interactive)
  (delete-trailing-whitespace)
  (code-untabify-buffer)
  )

;; Supprime les trailing whitespace, indente et supprime les tabulations
;;;###autoload
(defun code-format-buffer-with-ident()
  (interactive)
  (delete-trailing-whitespace)
  (code-indent-buffer)
  (code-untabify-buffer)
  )

;;;###autoload
(defun code-align-vars ()
  (interactive)
  (setq old-case-fold case-fold-search)
  (setq case-fold-search nil)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\b\\(\\($\\)?[lpmcg][cs]?[cs]?\\([A-Z]\\|_\\).*\\)\\b" 1 1 0)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)=" 1 1 0)
  (setq case-fold-search old-case-fold)
  )

;;;###autoload
(defun code-align-args ()
  (interactive)
  (align-regexp (region-beginning) (region-end) ",\\(\\s-*\\)" 1 1 t)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)<<" 1 1 t)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)=>" 1 1 t)
  )
