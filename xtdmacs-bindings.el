;; -*- lexical-binding: t -*-

(require 'swbuff)

(eval-when-compile
  (defvar shell-last-visited-buffer))

(defun xtdmacs-insert-date ()
  (interactive)
  (insert (format-time-string "%e-%m-%Y"))
  )

(defun xtdmacs-shell-toggle ()
  (interactive)
  (if (eq nil (get-buffer "*shell*"))
      (progn
        (setq shell-last-visited-buffer (current-buffer))
        (shell)
        (switch-to-buffer "*shell*"))
    (if (equal (buffer-name (current-buffer)) "*shell*")
        (switch-to-buffer shell-last-visited-buffer)
      (progn
        (setq shell-last-visited-buffer (current-buffer))
        (switch-to-buffer "*shell*")
        )))
  )


(defun --xtdmacs-bindings-mode-construct()
  (setq comint-prompt-read-only t)
  (add-hook 'shell-mode-hook (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

  (if (equal (getenv "PUTTY") "1")
      (progn
        (message "Putty detected, hacking key codes")
        (define-key input-decode-map "\M-[A"   [C-up])
        (define-key input-decode-map "\M-[B"   [C-down])
        (define-key input-decode-map "\M-[C"   [C-right])
        (define-key input-decode-map "\M-[D"   [C-left])
        (define-key input-decode-map "\e\M-OA" [M-up])
        (define-key input-decode-map "\e\M-OB" [M-down])))
  (ido-mode 'buffers)
  )


;;;###autoload
(define-minor-mode xtdmacs-bindings-mode
  "Emacs custom bindings" nil "Emacs custom bindings"
  '(([home]       . beginning-of-line)
    ([select]     . end-of-line)
    ([M-up]       . beginning-of-buffer)
    ([A-up]       . beginning-of-buffer)
    ([M-down]     . end-of-buffer)
    ([A-down]     . end-of-buffer)
    ([C-right]    . forward-word)
    ([C-left]     . backward-word)
    ("\C-c\C-g"   . goto-line)
    ("\C-d"       . query-replace)
    ("\C-f"       . query-replace-regexp)
    ([24 down]    . windmove-down)
    ([24 right]   . windmove-right)
    ([24 left]    . windmove-left)
    ([24 up]      . windmove-up)
    ([24 C-right] . swbuff-switch-to-next-buffer)
    ([24 C-left]  . swbuff-switch-to-previous-buffer)
    ([24 C-down]  . ido-switch-buffer)
    ("\C-xk"      . kill-buffer)
    ("\C-x\C-f"   . find-file)
    ("\M-+"       . enlarge-window)
    ("\M--"       . shrink-window)
    ([M-delete]   . kill-word)
    ("\e "        . dabbrev-expand)
    ("\es"        . speedbar-get-focus)
    ("C"          . self-insert-command)
    ("\C-xl"      . xtdmacs-insert-date)
    ("\M-q"       . comment-region)
    ("\M-a"       . uncomment-region)
    ([f5]         . delete-trailing-whitespace)
    ([C-f5]       . font-lock-fontify-buffer)
    ([C-f11]      . xtdmacs-shell-toggle)
    ([f11]        . tmm-menubar))
  :global t

  (if xtdmacs-bindings-mode
      (--xtdmacs-bindings-mode-construct))
  )


(provide 'xtdmacs-bindings)
