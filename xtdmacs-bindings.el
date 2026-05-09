;;; xtdmacs-bindings.el --- Global Emacs key bindings  -*- lexical-binding: t -*-

;;; Commentary:

;; Defines the `xtdmacs-bindings-mode' global minor mode, a handful of
;; small interactive commands it binds, and a buffer predicate that
;; hides "* internal *" buffers from the next-buffer cycle.

;;; Code:

(require 'iflipb)

(eval-when-compile
  (defvar shell-last-visited-buffer))

(eval-when-compile
  (defvar xtdmacs-bindings-mode-map))

(defun xtdmacs-insert-date ()
  "Insert today's date in dd-mm-yyyy format at point."
  (interactive)
  (insert (format-time-string "%e-%m-%Y")))

(defun xtdmacs-backward-delete-word ()
  "Delete one word backwards without saving it to the `kill-ring'."
  (interactive)
  (backward-kill-word 1)
  (setq kill-ring (cdr kill-ring)))

(defun xtdmacs-buffer-predicate (buffer)
  "Return nil if BUFFER's name start with `*' (i.e. internal buffer)."
  (if (string-match-p "^ *[*]" (buffer-name buffer))
      nil
    t))

(defun xtdmacs-shell-toggle ()
  "Toggle between the *shell* buffer and the previously visited one."
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
        (switch-to-buffer "*shell*")))))


(defun --xtdmacs-bindings-mode-construct ()
  "Initialize global state when `xtdmacs-bindings-mode' is enabled."
  (setq comint-prompt-read-only t)
  (add-hook 'shell-mode-hook (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))
  (set-frame-parameter nil 'buffer-predicate 'xtdmacs-buffer-predicate)
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
  "Emacs custom bindings."
  :init-value nil
  :lighter "Emacs custom bindings"
  :global t
  :group 'xtdmacs
  :keymap `(([home]       . beginning-of-line)
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
            ([24 C-right] . iflipb-next-buffer)
            ([24 C-left]  . iflipb-previous-buffer)
            ([24 C-down]  . ido-switch-buffer)
            ("\C-xk"      . ido-kill-buffer)
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
            (,(kbd "M-<DEL>") . xtdmacs-backward-delete-word)
            ([f5]         . delete-trailing-whitespace)
            ([C-f5]       . font-lock-fontify-buffer))
  (when xtdmacs-bindings-mode
    (--xtdmacs-bindings-mode-construct)))


(provide 'xtdmacs-bindings)

;;; xtdmacs-bindings.el ends here
