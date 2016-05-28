(autoload 'edmacro-parse-keys "edmacro" "" nil)
(require 'swbuff)

(eval-when-compile
  (defvar shell-last-visited-buffer))

(defun reload-conf ()
  (interactive)
  (load-file "~/.emacs")
  )

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%e-%m-%Y"))
  )

;; Defini les touches pour se balader dans le proposition de swbuf lors de l'appel a
;; iswitchb-buffer
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             )))
  )

;; bascule shell
(defun shell-toggle ()
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

(define-minor-mode bindings-mode
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
    ("\M-d"       . align-regexp)
    ([24 down]    . windmove-down)
    ([24 right]   . windmove-right)
    ([24 left]    . windmove-left)
    ([24 up]      . windmove-up)
    ([24 C-right] . swbuff-switch-to-next-buffer)
    ([24 C-left]  . swbuff-switch-to-previous-buffer)
    ([24 C-down]  . iswitchb-buffer)
    ("\C-xk"      . kill-buffer)
    ("\C-x\C-f"   . find-file)
    ("\M-+"       . enlarge-window)
    ("\M--"       . shrink-window)
    ([M-delete]   . kill-word)
    ("\e "        . dabbrev-expand)
    ("\es"        . speedbar-get-focus)
    ("C"          . self-insert-command)
    ("\C-xl"      . insert-date)
    ("\M-q"       . comment-region)
    ("\M-a"       . uncomment-region)
    ([f5]         . delete-trailing-whitespace)
    ([C-f5]       . font-lock-fontify-buffer)
    ([C-f11]      . shell-toggle)
    ([f11]        . tmm-menubar))
  :global t


  (if (not (local-variable-p 'bindings-mode-enabled))
      (progn
        (make-local-variable 'bindings-mode-enabled)
        (setq bindings-mode-enabled nil)))

  (if (and bindings-mode (not bindings-mode-enabled))
      (progn
        (setq bindings-mode-enabled t)
        (setq comint-prompt-read-only t)
        (add-hook 'shell-mode-hook
                  (lambda ()
                    (set-process-query-on-exit-flag
                     (get-buffer-process (current-buffer)) nil)))

        ;; si putty, on remap les keycodes des touches de fleches
        (if (equal (getenv "PUTTY") "1")
            (progn
              (message "Putty detected, hacking key codes")
              (define-key input-decode-map "\M-[A"   [C-up])
              (define-key input-decode-map "\M-[B"   [C-down])
              (define-key input-decode-map "\M-[C"   [C-right])
              (define-key input-decode-map "\M-[D"   [C-left])
              (define-key input-decode-map "\e\M-OA" [M-up])
              (define-key input-decode-map "\e\M-OB" [M-down])))

        (iswitchb-mode 1)
        (setq iswitchb-buffer-ignore         '("^ " "*Help*" "*scratch*" "*Messages*" "*compilation*" "*shell*" "*Completions*" "*Buffer List*"))
        (setq swbuff-exclude-buffer-regexps  '("^ " "*Help*" "*scratch*" "*Messages*" "*compilation*" "*shell*" "*Completions*" "*Buffer List*"))
        (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
        ))

  )

(bindings-mode)

(provide 'bindings)
