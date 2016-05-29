(require 'linum)
(require 'xtdmacs-code-spell)
(require 'xtdmacs-compile++)

(defface xtdmacs-code-face-status-ok
  '((t (:foreground "#005f00")))
  "status codes KEERROR."
  :group 'code
  )

(defface xtdmacs-code-face-status-error
  '((t (:foreground "#870000")))
  "status codes KEERROR."
  :group 'code
  )

(defface xtdmacs-code-face-status-ok
  '((t (:foreground "#af5fd7")))
  "status code KEOK."
  :group 'code
  )

(defface xtdmacs-code-face-status-other
  '((t (:foreground "#af5fd7"))) "other status code (KETIMEOUT... etc)."
  :group 'code
  )

(defface xtdmacs-code-face-log
  '((t (:foreground "#5f5f00")))
  "logging."
  :group 'code
  )

(defface xtdmacs-code-face-global-variable
  '((t (:foreground "#ff00ff")))
  "global variable."
  :group 'code
  )

(defface xtdmacs-code-face-global-variable-const
  '((t (:foreground "#ff00ff" :weight bold)))
  "const global variable."
  :group 'code
  )

(defface xtdmacs-code-face-global-variable-const-static
  '((t (:foreground "#ff00ff" :underline t :weight bold)))
  "const static global variable."
  :group 'code
  )

(defface xtdmacs-code-face-global-variable-static
  '((t (:foreground "#ff00ff" :underline t)))
  "static global variable."
  :group 'code
  )

(defface xtdmacs-code-face-local-variable
  '((t (:foreground "#7f7f7f")))
  "local variable."
  :group 'code
  )

(defface xtdmacs-code-face-local-variable-const
  '((t (:foreground "#4e4e4e" :weight bold)))
  "const local variable."
  :group 'code
  )

(defface xtdmacs-code-face-local-variable-const-static
  '((t (:foreground "#7f7f7f" :underline t :weight bold)))
  "const static local variable."
  :group 'code
  )

(defface xtdmacs-code-face-local-variable-static
  '((t (:foreground "#7f7f7f" :underline t)))
  "static local variable."
  :group 'code
  )

(defface xtdmacs-code-face-param
  '((t (:foreground "#008787")))
  "parameter."
  :group 'code
  )

(defface xtdmacs-code-face-param-const
  '((t (:foreground "#008787" :weight bold)))
  "const parameter."
  :group 'code
  )

(defface xtdmacs-code-face-macro
  '((t (:foreground "#008787" :underline t)))
  "macro"
  :group 'code
  )

(defface xtdmacs-code-face-class-member
  '((t (:foreground "#87afff")))
  "member variable"
  :group 'code
  )

(defface xtdmacs-code-face-class-member-const
  '((t (:foreground "#87afff" :weight bold)))
  "const member variable"
  :group 'code
  )

(defface xtdmacs-code-face-class-member-const-static
  '((t (:foreground "#87afff" :underline t :weight bold)))
  "static const member variable"
  :group 'code
  )

(defface xtdmacs-code-face-class-member-static
  '((t (:foreground "#87afff" :underline t)))
  "static member variable"
  :group 'code
  )

(defface xtdmacs-code-face-counter
  '((t (:foreground "#875f00")))
  "iterator/counter"
  :group 'code
  )

(defface xtdmacs-code-face-counter-const
  '((t (:foreground "#875f00" :weight bold)))
  "const iterator/counter"
  :group 'code
  )


(defcustom xtdmacs-code-indent-max-lines 2000 "Maximum number of line in buffer to permit auto-indentation." :group 'code :type 'integer)

(defadvice linum-update-window (after linum-update-window-after (win) activate)
  "fix linum for scaled text"
  (set-window-margins win
                      (+ 1 (ceiling (* (if (boundp 'text-scale-mode-step)
                                           (expt text-scale-mode-step
                                                 text-scale-mode-amount) 1)
                                       (if (car (window-margins))
                                           (car (window-margins)) 1)
                                       )))))

(defun --xtdmacs-code-mode-construct()
  (unless (mode-enabled 'highlight-80+-mode)
    (highlight-80+-mode t))
  (unless (mode-enabled 'linum-mode)
    (linum-mode t))
  (unless (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-mode t))
  (unless (mode-enabled 'xtdmacs-code-spell-mode)
    (xtdmacs-code-spell-mode t))
  (message "enabled : xtdmacs-code-mode")
  )

(defun --xtdmacs-code-mode-detroy()
  (when highlight-80+-mode
    (highlight-80+-mode nil))
  (when linum-mode
    (linum-mode nil))
  (when xtdmacs-code-spell-mode
    (xtdmacs-code-spell-mode nil))
  (when xtdmacs-compile++-mode
    (xtdmacs-compile++-mode nil))
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
    ([C-f2]       . xtdmacs-code-align-args))
  (if xtdmacs-code-mode
      (--xtdmacs-code-mode-construct)
    (--xtdmacs-code-mode-detroy))
  )

(provide 'xtdmacs-code)
