(autoload 'highlight-80+-mode               "highlight-80+" "" t)
(autoload 'linum-mode                       "linum"         "" t)
(autoload 'js2-mode                         "js2-mode"      "" t)
(autoload 'compile++-mode                   "compile++"     "" t)
(autoload 'php-mode                         "pi-php-mode"   "" t)
(autoload 'web-mode                         "web-mode"      "" t)
(autoload 'json-mode                        "json-mode"     "" t)
(autoload 'code-untabify-buffer             "code.utils"    "" t)
(autoload 'code-indent-buffer               "code.utils"    "" t)
(autoload 'code-format-buffer-without-ident "code.utils"    "" t)
(autoload 'code-format-buffer-with-ident    "code.utils"    "" t)
(autoload 'code-align-vars                  "code.utils"    "" t)
(autoload 'code-align-args                  "code.utils"    "" t)
(autoload 'code-shell-toggle                "code.utils"    "" t)
(autoload 'code-cpp-mode                    "code.cpp"      "" t)
(autoload 'code-doxymacs-mode               "code.doxymacs" "" t)
(autoload 'code-makefile-mode               "code.makefile" "" t)
(autoload 'code-line-mode                   "code.line"     "" t)
(autoload 'code-php-mode                    "code.php"      "" t)
(autoload 'code-web-mode                    "code.web"      "" t)
(autoload 'code-python-mode                 "code.python"   "" t)
(autoload 'code-js-mode                     "code.js"       "" t)
(autoload 'code-lisp-mode                   "code.lisp"     "" t)
(autoload 'code-java-mode                   "code.java"     "" t)

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

(defconst code-php-doxymacs-function-comment-template
  '((let ((next-func (doxymacs-find-next-func)))
      (if next-func
          (list
           'l
           "/**" '> 'n
           "* <brief>" '> 'n
           " *" '> 'n
           " * <details>" '> 'n
           " *" '> 'n
           (code-php-doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
           (unless (string-match
                    (regexp-quote (cdr (assoc 'return next-func)))
                    doxymacs-void-types)
             '(l " * " (doxymacs-doxygen-command-char) "return" (p "Returns: ") > n))
           " */" '>)
        (progn
          (error "Can't find next function declaration.")
          nil))))
  "Doc-style template code-php-mode documentation.")

(defconst code-doxymacs-function-comment-template
  '((let ((next-func (doxymacs-find-next-func)))
      (if next-func
          (list
           'l
           "/**" '> 'n
           " ** @brief" '> 'n
           (code-doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
           (unless (string-match
                    (regexp-quote (cdr (assoc 'return next-func)))
                    doxymacs-void-types)
             '(l " ** " (doxymacs-doxygen-command-char) "return" (p "Returns: ") > n))
           " ** @details" '> 'n
           " **" '> 'n
           " */" '>)
        (progn
          (error "Can't find next function declaration.")
          nil))))
  "Doc-style template code-doxymacs-mode documentaiton.")

(defcustom code-indent-load-auto t    "Enables code auto-indentation on load."                       :group 'code :type 'boolean)
(defcustom code-indent-save-auto t    "Enables code auto-indentation on save."                       :group 'code :type 'boolean)
(defcustom code-indent-max-lines 2000 "Maximum number of line in buffer to permit auto-indentation." :group 'code :type 'integer)

(add-to-list 'auto-mode-alist '("\\.php\\'"    . php-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'"     . python-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"     . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'"   . json-mode))

(add-hook 'web-mode-hook        '(lambda () (code-web-mode      t)))
(add-hook 'php-mode-hook        '(lambda () (code-php-mode      t)))
(add-hook 'makefile-mode-hook   '(lambda () (code-makefile-mode t)))
(add-hook 'python-mode-hook     '(lambda () (code-python-mode   t)))
(add-hook 'emacs-lisp-mode-hook '(lambda () (code-lisp-mode     t)))
(add-hook 'js2-mode-hook        '(lambda () (code-js-mode       t)))
(add-hook 'c++-mode-hook        '(lambda () (code-cpp-mode      t)))
(add-hook 'java-mode-hook       '(lambda () (code-java-mode     t)))


(define-minor-mode code-mode "Bunch of configuration developpement" nil "Code"
  '(([C-M-up]     . backward-sexp)
    ([C-M-down]   . forward-sexp)
    ([M-q]        . comment-region)
    ([M-a]        . uncomment-region)
    ([f6]         . indent-region)
    ([C-f6]       . code-indent-buffer)
    ([C-f1]       . code-align-vars)
    ([C-f2]       . code-align-args))
  (if code-mode
      (progn
        (highlight-80+-mode  t)
        (linum-mode          t)
        (compile++-mode      t)
        (message "enabled : code-mode")))
  )

(provide 'code)
