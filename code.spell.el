(defcustom code-spell-on-load
  t
  "Maximum number of line in buffer to permit auto-indentation."
  :group 'code-spell
  :type 'boolean
  :safe 'booleanp
  )

(defcustom code-spell-ignore-regexp
  '("^ \\*\\* @subsubsection [^ ]+ " "@image html [^ ]+ ") "ingore patterns"
  :group 'code-spell
  :type '(repeat string)
  :safe 'listp
  )

(defun code-spell-next-word ()
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )

(defun code-spell-prev-word ()
  (interactive)
  (flyspell-check-previous-highlighted-word)
  )


(defun code-spell-change-dictionary ()
  (interactive)
  (call-interactively 'ispell-change-dictionary)
  (add-file-local-variable 'ispell-local-dictionary ispell-local-dictionary)
  (flyspell-buffer)
  )

(defun code-spell-test-regex (content regex begin end)
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

(defun code-spell-check-patterns (content begin end)
  ;; (message "checking patterns on : %s" content)
  (let* ((found nil)
         (patterns code-spell-ignore-regexp))
    (while (and (not found) (car patterns))
      ;; (message "-> pattern : %s" (car patterns))
      (setq found (code-spell-test-regex content (car patterns) begin end))
      (setq patterns (cdr patterns))
      )
    found))

(defun code-spell-ignore-patterns (begin end type)
  (save-excursion
    (goto-char begin)
    (let* ((start   (line-beginning-position))
           (stop    (line-end-position))
           (content (buffer-substring start stop)))
      ;; (message "\n\n begin : %d, end : %d, start : %d, stop : %d" begin end start stop)
      ;; (message " begin : %d, end : %d, start : %d, stop : %d" begin end (- begin start) (- end start))
      (code-spell-check-patterns content (- begin start) (- end start)))))

(defun --code-spell-mode-construct()
  (unless (mode-enabled 'flyspell-prog-mode)
    (flyspell-prog-mode))
  (add-hook 'flyspell-incorrect-hook 'code-spell-ignore-patterns t t)
  (when code-spell-on-load
    (flyspell-buffer))
  (define-key code-spell-mode-map (kbd "C-c C-c")       'flyspell-buffer)
  (define-key code-spell-mode-map (kbd "C-c C-<down>")  'code-spell-change-dictionary)
  (define-key code-spell-mode-map (kbd "C-c C-<right>") 'code-spell-next-word)
  (define-key code-spell-mode-map (kbd "C-c C-<left>")  'code-spell-prev-word)
  (message "enabled : code-spell-mode")
  )

(defun --code-spell-mode-destroy()
  (remove-hook 'flyspell-incorrect-hook 'code-spell-ignore-patterns t)
  (when (mode-enabled 'flyspell-prog-mode)
    (flyspell-prog-mode nil))

  (message "disabled : code-spell-mode")
  )

;;;###autoload
(define-minor-mode code-spell-mode "Spell comments and strings" nil "Code"
  '()
  (if code-spell-mode
      (--code-spell-mode-construct)
    (--code-spell-mode-destroy))
  )

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
