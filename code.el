(defface code-face-status-ok                    '((t (:foreground "color-134"                                :weight normal ))) "status code KEOK."                     :group 'code)
(defface code-face-status-error                 '((t (:foreground "color-134"                                :weight normal ))) "status codes KEERROR."                 :group 'code)
(defface code-face-status-other                 '((t (:foreground "color-134"                                :weight normal ))) "other status code (KETIMEOUT... etc)." :group 'code)
(defface code-face-log                          '((t (:foreground "color-58"                                 :weight normal ))) "logging."                              :group 'code)
(defface code-face-global-variable              '((t (:foreground "brightmagenta"                            :weight normal ))) "global variable."                      :group 'code)
(defface code-face-global-variable-const        '((t (:foreground "brightmagenta"                            :weight bold   ))) "const global variable."                :group 'code)
(defface code-face-global-variable-const-static '((t (:foreground "brightmagenta" :underline t               :weight bold   ))) "const static global variable."         :group 'code)
(defface code-face-global-variable-static       '((t (:foreground "brightmagenta" :underline t               :weight normal ))) "static global variable."               :group 'code)
(defface code-face-local-variable               '((t (:foreground "brightblack"                              :weight normal ))) "local variable."                       :group 'code)
(defface code-face-local-variable-const         '((t (:foreground "#4e4e4e"                                  :weight bold   ))) "const local variable."                 :group 'code)
(defface code-face-local-variable-const-static  '((t (:foreground "brightblack"   :underline t               :weight bold   ))) "const static local variable."          :group 'code)
(defface code-face-local-variable-static        '((t (:foreground "brightblack"   :underline t               :weight normal ))) "static local variable."                :group 'code)
(defface code-face-param                        '((t (:foreground "#008787"                                  :weight normal ))) "parameter."                            :group 'code)
(defface code-face-param-const                  '((t (:foreground "#008787"                                  :weight bold   ))) "const parameter."                      :group 'code)
(defface code-face-param-const-static           '((t (:foreground "#008787"       :underline t               :weight bold   ))) "const static parameter."               :group 'code)
(defface code-face-param-static                 '((t (:foreground "#008787"       :underline t               :weight normal ))) "static parameter."                     :group 'code)
(defface code-face-macro                        '((t (:foreground "#008787"       :underline t               :weight normal ))) "macro"                                 :group 'code)
(defface code-face-class-member                 '((t (:foreground "#87afff"                                  :weight normal ))) "member variable"                       :group 'code)
(defface code-face-class-member-const           '((t (:background "black" :foreground "#87afff"              :weight bold   ))) "const member variable"                 :group 'code)
(defface code-face-class-member-const-static    '((t (:background "black" :foreground "#87afff" :underline t :weight bold   ))) "static const member variable"          :group 'code)
(defface code-face-class-member-static          '((t (:background "black" :foreground "#87afff" :underline t :weight normal ))) "static member variable"                :group 'code)
(defface code-face-counter                      '((t (:foreground "#875f00")))                                                  "iterator/counter"                      :group 'code)
(defface code-face-counter-const                '((t (:foreground "#875f00" :weight bold)))                                     "const iterator/counter"                :group 'code)

(defcustom code-indent-max-lines 2000 "Maximum number of line in buffer to permit auto-indentation." :group 'code :type 'integer)

(defun --code-mode-construct()
  (unless (mode-enabled 'highlight-80+-mode)
    (highlight-80+-mode t))
  (unless (mode-enabled 'linum-mode)
    (linum-mode t))
  (unless (mode-enabled 'compile++-mode)
    (compile++-mode t))
  (unless (mode-enabled 'code-spell-mode)
    (code-spell-mode t))
  (message "enabled : code-mode")
  )

(defun --code-mode-detroy()
  (when highlight-80+-mode
    (highlight-80+-mode nil))
  (when linum-mode
    (linum-mode nil))
  (when code-spell-mode
    (code-spell-mode nil))
  (when compile++-mode
    (compile++-mode nil))
  (message "disabled : code-mode")
  )

;;;###autoload
(define-minor-mode code-mode "Bunch of configuration development" nil "Code"
  '(([C-M-up]     . backward-sexp)
    ([C-M-down]   . forward-sexp)
    ([M-q]        . comment-region)
    ([M-a]        . uncomment-region)
    ([f4]         . indent-region)
    ([C-f4]       . code-indent-buffer)
    ([C-f1]       . code-align-vars)
    ([C-f2]       . code-align-args))
  (if code-mode
      (--code-mode-construct)
    (--code-mode-detroy))
  )


(provide 'code)
