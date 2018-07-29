(require 'linum)
(require 'yafolding)
(require 'align)
(require 'xtdmacs-compile++)
(require 'column-enforce-mode)

(defface xtdmacs-code-face-status-ok
  '((t (:foreground "#005f00")))
  "status codes KEERROR."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-status-error
  '((t (:foreground "#870000")))
  "status codes KEERROR."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-status-ok
  '((t (:foreground "#af5fd7")))
  "status code KEOK."
  :group 'xtdmacs-code
  )
(defface xtdmacs-code-face-status-other
  '((t (:foreground "#af5fd7"))) "other status code (KETIMEOUT... etc)."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-log
  '((t (:foreground "#5f5f00")))
  "logging."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-global-variable
  '((t (:foreground "#ff00ff")))
  "global variable."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-global-variable-const
  '((t (:foreground "#ff00ff" :weight bold)))
  "const global variable."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-global-variable-const-static
  '((t (:foreground "#ff00ff" :underline t :weight bold)))
  "const static global variable."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-global-variable-static
  '((t (:foreground "#ff00ff" :underline t)))
  "static global variable."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-local-variable
  '((t (:foreground "#7f7f7f")))
  "local variable."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-local-variable-const
  '((t (:foreground "#4e4e4e" :weight bold)))
  "const local variable."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-local-variable-const-static
  '((t (:foreground "#7f7f7f" :underline t :weight bold)))
  "const static local variable."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-local-variable-static
  '((t (:foreground "#7f7f7f" :underline t)))
  "static local variable."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-param
  '((t (:foreground "#008787")))
  "parameter."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-param-const
  '((t (:foreground "#008787" :weight bold)))
  "const parameter."
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-macro
  '((t (:foreground "#008787" :underline t)))
  "macro"
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-class-member
  '((t (:foreground "#87afff")))
  "member variable"
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-class-member-const
  '((t (:foreground "#87afff" :weight bold)))
  "const member variable"
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-class-member-const-static
  '((t (:foreground "#87afff" :underline t :weight bold)))
  "static const member variable"
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-class-member-static
  '((t (:foreground "#87afff" :underline t)))
  "static member variable"
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-counter
  '((t (:foreground "#875f00")))
  "iterator/counter"
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-counter-const
  '((t (:foreground "#875f00" :weight bold)))
  "const iterator/counter"
  :group 'xtdmacs-code
  )

(defface xtdmacs-code-face-return
  '((t (:foreground "#d70087")))
  "return value"
  :group 'xtdmacs-code
  )



(defcustom xtdmacs-code-indent-max-lines 2000 "Maximum number of line in buffer to permit auto-indentation." :group 'xtdmacs-code :type 'integer)

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
(defun xtdmacs-code-format-buffer(&optional indent untab)
  (delete-trailing-whitespace)
  (if indent
      (xtdmacs-code-indent-buffer))
  (if untab
      (xtdmacs-code-untabify-buffer))
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
  (let* ((old-case-fold case-fold-search))
    (setq case-fold-search nil)
    (xtdmacs-code-align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\b\\(\\($\\)?[lpmcg][cs]?[cs]?\\([A-Z]\\|_\\).*\\)\\b" 1 1 0)
    (xtdmacs-code-align-regexp (region-beginning) (region-end) "\\(\\s-*\\):?=" 1 1 0)
    (setq case-fold-search old-case-fold)
    )
  )

;;;###autoload
(defun xtdmacs-code-align-args ()
  (interactive)
  (xtdmacs-code-align-regexp (region-beginning) (region-end) ",\\(\\s-*\\)" 1 1 t)
  (xtdmacs-code-align-regexp (region-beginning) (region-end) "\\(\\s-*\\)<<" 1 1 t)
  (xtdmacs-code-align-regexp (region-beginning) (region-end) "\\(\\s-*\\)=>" 1 1 t)
  )

;;;###autoload
(defun xtdmacs-code-align-regexp (beg end regexp &optional group spacing repeat)
  (interactive
   (append
    (list (region-beginning) (region-end))
    (if current-prefix-arg
        (list (read-string "Complex align using regexp: "
                           "\\(\\s-*\\)")
              (string-to-number
               (read-string
                "Parenthesis group to modify (justify if negative): " "1"))
              (string-to-number
               (read-string "Amount of spacing (or column if negative): "
                            (number-to-string align-default-spacing)))
              (y-or-n-p "Repeat throughout line? "))
      (list (concat "\\(\\s-*\\)"
                    (read-string "Align regexp: "))
            1 align-default-spacing nil))))
  (or group (setq group 1))
  (or spacing (setq spacing align-default-spacing))
  (let ((tab-mode indent-tabs-mode)
        (rule (list (list nil (cons 'regexp regexp)
                          (cons 'group (abs group))
                          (if (< group 0)
                              (cons 'justify t)
                            (cons 'bogus nil))
                          (if (>= spacing 0)
                              (cons 'spacing spacing)
                            (cons 'column (abs spacing)))
                          (cons 'repeat repeat)))))
    (setq indent-tabs-mode nil)
    (align-region beg end 'entire rule nil nil)
    (setq indent-tabs-mode tab-mode)
    )
  )

(defun --xtdmacs-code-mode-construct()
  (unless (mode-enabled 'linum-mode)
    (linum-mode t))
  (unless (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-mode t))
  (unless (mode-enabled 'yafolding-mode)
    (yafolding-mode t)
    (define-key yafolding-mode-map (kbd "M-f")   'yafolding-toggle-element)
    (define-key yafolding-mode-map (kbd "C-M-f") 'yafolding-toggle-all)
    )
  (unless (mode-enabled 'column-enforce-mode)
    (column-enforce-mode t))
  (highlight-regexp " +$" 'trailing-whitespace)
  (message "enabled : xtdmacs-code-mode")
  )

(defun --xtdmacs-code-mode-detroy()
  (when column-enforce-mode
    (column-enforce-mode nil))
  (when (mode-enabled 'yafolding-mode)
    (yafolding-mode nil))
  (when xtdmacs-compile++-mode
    (xtdmacs-compile++-mode nil))
  (when linum-mode
    (linum-mode nil))
  (message "disabled : xtdmacs-code-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-mode "Bunch of configuration development" nil "Code"
  '(([C-M-up]     . backward-sexp)
    ([C-M-down]   . forward-sexp)
    ([M-q]        . comment-region)
    ([M-a]        . uncomment-region)
    ([f4]         . indent-region)
    ([C-f4]       . xtdmacs-code-indent-buffer)
    ([C-f1]       . xtdmacs-code-align-vars)
    ([C-f2]       . xtdmacs-code-align-args)
    ("\M-d"       . xtdmacs-code-align-regexp)
    )
  (if xtdmacs-code-mode
      (--xtdmacs-code-mode-construct)
    (--xtdmacs-code-mode-detroy))
  )

(provide 'xtdmacs-code)
