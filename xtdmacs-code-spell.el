(defcustom xtdmacs-code-spell-on-load
  t
  "Maximum number of line in buffer to permit auto-indentation."
  :group 'xtdmacs-code-spell
  :type 'boolean
  :safe 'booleanp
  )

(defcustom xtdmacs-code-spell-ignore-regexp
  '("^ \\*\\* @subsubsection [^ ]+ " "@image html [^ ]+ ") "ingore patterns"
  :group 'xtdmacs-code-spell
  :type '(repeat string)
  :safe 'listp
  )

(defun xtdmacs-code-spell-next-word ()
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )

(defun xtdmacs-code-spell-prev-word ()
  (interactive)
  (flyspell-check-previous-highlighted-word)
  )


(defun xtdmacs-code-spell-change-dictionary ()
  (interactive)
  (call-interactively 'ispell-change-dictionary)
  (add-file-local-variable 'ispell-local-dictionary ispell-local-dictionary)
  (flyspell-buffer)
  )

(defun xtdmacs-code-spell-test-regex (content regex begin end)
  ;; (message "test '%s' against '%s' (%d - %d)" content regex begin end)
  (let* ((match_begin (string-match regex content))
         (match_end   (match-end 0)))
    (if (and match_begin match_end)
        (progn
          ;; (message "match begin : %s, match end : %s" match_begin match_end)
          (if (and (<= match_begin begin) (>= match_end end))
              t
            nil))
      ;; (message "no match" match_begin match_end)
      nil)))

(defun xtdmacs-code-spell-check-patterns (content begin end)
  ;; (message "checking patterns on : %s" content)
  (let* ((found nil)
         (patterns xtdmacs-code-spell-ignore-regexp))
    (while (and (not found) (car patterns))
      ;; (message "-> pattern : %s" (car patterns))
      (setq found (xtdmacs-code-spell-test-regex content (car patterns) begin end))
      (setq patterns (cdr patterns))
      )
    found))

(defun xtdmacs-code-spell-ignore-patterns (begin end type)
  (save-excursion
    (goto-char begin)
    (let* ((start   (line-beginning-position))
           (stop    (line-end-position))
           (content (buffer-substring start stop)))
      ;; (message "\n\n begin : %d, end : %d, start : %d, stop : %d" begin end start stop)
      ;; (message " begin : %d, end : %d, start : %d, stop : %d" begin end (- begin start) (- end start))
      (xtdmacs-code-spell-check-patterns content (- begin start) (- end start)))))

(defun --xtdmacs-code-spell-mode-construct()
  (unless (mode-enabled 'flyspell-prog-mode)
    (flyspell-prog-mode))
  (add-hook 'flyspell-incorrect-hook 'xtdmacs-code-spell-ignore-patterns t t)
  (when xtdmacs-code-spell-on-load
    (flyspell-buffer))
  (define-key xtdmacs-code-spell-mode-map (kbd "C-c C-c")       'flyspell-buffer)
  (define-key xtdmacs-code-spell-mode-map (kbd "C-c C-<down>")  'xtdmacs-code-spell-change-dictionary)
  (define-key xtdmacs-code-spell-mode-map (kbd "C-c C-<right>") 'xtdmacs-code-spell-next-word)
  (define-key xtdmacs-code-spell-mode-map (kbd "C-c C-<left>")  'xtdmacs-code-spell-prev-word)
  (message "enabled : xtdmacs-code-spell-mode")
  )

(defun --xtdmacs-code-spell-mode-destroy()
  (remove-hook 'flyspell-incorrect-hook 'xtdmacs-code-spell-ignore-patterns t)
  (when (mode-enabled 'flyspell-prog-mode)
    (flyspell-prog-mode nil))

  (message "disabled : xtdmacs-code-spell-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-spell-mode "Spell comments and strings" nil "Code"
  '()
  (if xtdmacs-code-spell-mode
      (--xtdmacs-code-spell-mode-construct)
    (--xtdmacs-code-spell-mode-destroy))
  )


(provide 'xtdmacs-code-spell)

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
