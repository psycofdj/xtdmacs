;; -*- lexical-binding: t -*-

(require 'flyspell)

(defcustom xtdmacs-code-spell-max-lines
  9999999
  "Maximum number of line in buffer to permit automatic activation of spell."
  :group 'xtdmacs-code-spell
  :type 'integer
  :safe 'integerp
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

(defun xtdmacs-code-spell-ignore-patterns (begin end _type)
  (save-excursion
    (goto-char begin)
    (let* ((start   (line-beginning-position))
           (stop    (line-end-position))
           (content (buffer-substring start stop)))
      ;; (message "\n\n begin : %d, end : %d, start : %d, stop : %d" begidsdn end start stop)
      ;; (message " begin : %d, end : %d, start : %d, stop : %d" begin end (- begin start) (- end start))
      (xtdmacs-code-spell-check-patterns content (- begin start) (- end start)))))


(defun --xtdmacs-code-spell-mode-construct(isprog)
  (if isprog
      (unless (mode-enabled 'flyspell-prog-mode)
        (flyspell-prog-mode))
    (unless (mode-enabled 'flyspell-mode)
      (flyspell-mode)))

  (add-hook 'flyspell-incorrect-hook 'xtdmacs-code-spell-ignore-patterns t t)
  (when (< (count-lines (point-min) (point-max)) xtdmacs-code-spell-max-lines)
    (flyspell-buffer))

  (if isprog
      (message "enabled : xtdmacs-code-spell-prog-mode")
    (message "enabled : xtdmacs-code-spell-mode"))
  )

(defun --xtdmacs-code-spell-mode-destroy(isprog)
  (remove-hook 'flyspell-incorrect-hook 'xtdmacs-code-spell-ignore-patterns t)
  (if isprog
      (when (mode-enabled 'flyspell-prog-mode)
        (flyspell-prog-mode))
    (when (mode-enabled 'flyspell-mode)
      (flyspell-mode)))
  (if isprog
      (message "disabled : xtdmacs-code-spell-prog-mode")
    (message "disabled : xtdmacs-code-spell-mode"))
  )

;;;###autoload
(define-minor-mode xtdmacs-code-spell-prog-mode "On the fly spelling for code modes" nil "Code"
  `((,(kbd "C-c C-c")       . flyspell-buffer)
    (,(kbd "C-c C-<down>")  . xtdmacs-code-spell-change-dictionary)
    (,(kbd "C-c C-<right>") . xtdmacs-code-spell-next-word)
    (,(kbd "C-c C-<left>")  . xtdmacs-code-spell-prev-word))
  (if xtdmacs-code-spell-prog-mode
      (--xtdmacs-code-spell-mode-construct t)
    (--xtdmacs-code-spell-mode-destroy t))
  )

;;;###autoload
(define-minor-mode xtdmacs-code-spell-mode "On the fly spelling for text modes" nil "Code"
  `((,(kbd "C-c C-c")       . flyspell-buffer)
    (,(kbd "C-c C-<down>")  . xtdmacs-code-spell-change-dictionary)
    (,(kbd "C-c C-<right>") . xtdmacs-code-spell-next-word)
    (,(kbd "C-c C-<left>")  . xtdmacs-code-spell-prev-word))
  (if xtdmacs-code-spell-mode
      (--xtdmacs-code-spell-mode-construct nil)
    (--xtdmacs-code-spell-mode-destroy nil))
  )


(provide 'xtdmacs-code-spell)

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
