(defface code-php-operator '((t (:foreground "color-134" :weight normal ))) "arrow operator." :group 'code)

(defcustom code-php-indent-load-auto
  t
  "Enables code auto-indentation on load."
  :group 'code-php
  :type 'boolean
  :safe 'booleanp
  )

(defcustom code-php-indent-save-auto
  t
  "Enables code auto-indentation on save."
  :group 'code-php
  :type 'boolean
  :safe 'booleanp)

(defcustom code-php-keywords-alist
  '(("\\<\\$\\(g_[_a-zA-Z0-9]+\\)\\>"              (1 'code-face-global-variable))
    ("\\<\\$\\(l_[_a-zA-Z0-9]+\\)\\>"              (1 'code-face-local-variable))
    ("\\<\\$\\(mcs_[_a-zA-Z0-9]+\\)\\>"            (1 'code-face-class-member-const-static))
    ("\\<\\$\\(ms_[_a-zA-Z0-9]+\\)\\>"             (1 'code-face-class-member-static))
    ("\\<\\$\\(mc_[_a-zA-Z0-9]+\\)\\>"             (1 'code-face-class-member-const))
    ("\\<\\$\\(m_[_a-zA-Z0-9]+\\)\\>"              (1 'code-face-class-member))
    ("\\(::\\|;\\|\-\>\\|=\\|\+\\|\-\\|/\\|\\.\\)" (1 'code-php-operator))
    ("\\(\(\\|\)\\|\\$\\)"                         (1 'code-php-operator))
    ("\\([_a-zA-Z0-9]+\\)::"                       (1 'font-lock-constant-face))
    ("\\(__[_A-Z]+__\\)"                           (1 'code-face-macro))
    (" +static +" .                                   'font-lock-keyword-face)
    (" +const +" .                                    'font-lock-keyword-face)
    ("\\<\$?\\(p[ajmix6ubs]*_[_a-zA-Z0-9]+\\)\\>"  (1 'code-face-param))
    ("\\<\$?\\(c_[_a-zA-Z0-9]+\\)\\>"              (1 'code-face-counter)))
  "Additional php font-lock keywords"
  :group 'code-php
  :safe '(lambda(p) t))


(defun --code-php-mode-construct()
  (font-lock-add-keywords nil code-php-keywords-alist)
  (c-set-offset   'arglist-cont-nonempty 'c-lineup-arglist)
  (c-set-offset   'arglist-close         'c-lineup-arglist-close-under-paren)
  (c-set-offset   'arglist-intro         'c-lineup-arglist-intro-after-paren)
  (custom-set-variables
   '(doxymacs-function-comment-template code-doxymacs-template-phpdoc))
  (modify-syntax-entry ?_ "w")
  (when code-php-indent-save-auto
    (add-hook 'before-save-hook 'code-format-buffer-with-ident t t))
  (when code-php-indent-load-auto
    (code-format-buffer-with-ident))
  (message "enabled : code-php-mode")
  )

(defun --code-php-mode-destroy()
  (font-lock-remove-keywords nil code-php-keywords-alist)
  (when code-php-indent-save-auto
    (remove-hook 'before-save-hook 'code-format-buffer-with-ident t))
  (message "disabled : code-php-mode")
  )

;;;###autoload
(define-minor-mode code-php-mode "Code for PHP" nil "Code" nil
  (if code-php-mode
      (--code-php-mode-construct)
    (--code-php-mode-destroy))
  )
