;; -*- lexical-binding: t -*-

(defcustom xtdmacs-code-php-indent-load-auto
  t
  "Enables code auto-indentation on load."
  :group 'xtdmacs-code-php
  :type 'boolean
  :safe 'booleanp
  )

(defcustom xtdmacs-code-php-indent-save-auto
  t
  "Enables code auto-indentation on save."
  :group 'xtdmacs-code-php
  :type 'boolean
  :safe 'booleanp)

(defcustom xtdmacs-code-php-keywords-alist
  '(("\\<\\$\\(g_[_a-zA-Z0-9]+\\)\\>"              (1 'xtdmacs-code-face-global-variable))
    ("\\<\\$\\(l_[_a-zA-Z0-9]+\\)\\>"              (1 'xtdmacs-code-face-local-variable))
    ("\\<\\$\\(mcs_[_a-zA-Z0-9]+\\)\\>"            (1 'xtdmacs-code-face-class-member-const-static))
    ("\\<\\$\\(ms_[_a-zA-Z0-9]+\\)\\>"             (1 'xtdmacs-code-face-class-member-static))
    ("\\<\\$\\(mc_[_a-zA-Z0-9]+\\)\\>"             (1 'xtdmacs-code-face-class-member-const))
    ("\\<\\$\\(m_[_a-zA-Z0-9]+\\)\\>"              (1 'xtdmacs-code-face-class-member))
    ("\\(::\\|;\\|\-\>\\|=\\|\+\\|\-\\|/\\|\\.\\)" (1 'xtdmacs-code-php-operator))
    ("\\(\(\\|\)\\|\\$\\)"                         (1 'xtdmacs-code-php-operator))
    ("\\([_a-zA-Z0-9]+\\)::"                       (1 'font-lock-constant-face))
    ("\\(__[_A-Z]+__\\)"                           (1 'xtdmacs-code-face-macro))
    (" +static +" .                                   'font-lock-keyword-face)
    (" +const +" .                                    'font-lock-keyword-face)
    ("\\<\$?\\(p[ajmix6ubs]*_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-param))
    ("\\<\$?\\(c_[_a-zA-Z0-9]+\\)\\>"              (1 'xtdmacs-code-face-counter)))
  "Additional php font-lock keywords"
  :group 'xtdmacs-code-php
  :safe '(lambda(p) t))


(defface xtdmacs-code-php-operator
  '((t (:foreground "#af5fd7")))
  "Used to fontify PHP language operators such as ';' or '::'"
  :group 'code
  )


(defun --xtdmacs-code-php-mode-construct()
  (font-lock-add-keywords nil xtdmacs-code-php-keywords-alist)
  (c-set-offset   'arglist-cont-nonempty 'c-lineup-arglist)
  (c-set-offset   'arglist-close         'c-lineup-arglist-close-under-paren)
  (c-set-offset   'arglist-intro         'c-lineup-arglist-intro-after-paren)
  (modify-syntax-entry ?_ "w")
  (when xtdmacs-code-php-indent-save-auto
    (add-hook 'before-save-hook 'xtdmacs-code-format-buffer-with-ident t t))
  (when xtdmacs-code-php-indent-load-auto
    (xtdmacs-code-format-buffer-with-ident))
  (message "enabled : xtdmacs-code-php-mode")
  )

(defun --xtdmacs-code-php-mode-destroy()
  (font-lock-remove-keywords nil xtdmacs-code-php-keywords-alist)
  (when xtdmacs-code-php-indent-save-auto
    (remove-hook 'before-save-hook 'xtdmacs-code-format-buffer-with-ident t))
  (message "disabled : xtdmacs-code-php-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-php-mode "Code for PHP" nil "Code" nil
  (if xtdmacs-code-php-mode
      (--xtdmacs-code-php-mode-construct)
    (--xtdmacs-code-php-mode-destroy))
  )


(provide 'xtdmacs-code-php)

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
