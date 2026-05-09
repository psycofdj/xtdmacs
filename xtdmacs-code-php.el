;;; xtdmacs-code-php.el --- PHP support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for php-mode buffers: Hungarian-style font-lock
;; keywords, custom indentation offsets, and optional auto-format.

;;; Code:

(require 'xtdmacs-code)

(use-package php-mode)

(defcustom xtdmacs-code-php-indent-load-auto t
  "Enables code auto-indentation on load."
  :group 'xtdmacs-code-php :type 'boolean :safe #'booleanp)

(defcustom xtdmacs-code-php-indent-save-auto t
  "Enables code auto-indentation on save."
  :group 'xtdmacs-code-php :type 'boolean :safe #'booleanp)

(defface xtdmacs-code-php-operator
  '((t (:foreground "#af5fd7")))
  "Used to fontify PHP language operators (';', '::', etc.)."
  :group 'xtdmacs-code-php)

(defcustom xtdmacs-code-php-keywords-alist
  '(("\\<\\$\\(g_[_a-zA-Z0-9]+\\)\\>"              (1 'xtdmacs-code-face-global-variable))
    ("\\<\\$\\(l_[_a-zA-Z0-9]+\\)\\>"              (1 'xtdmacs-code-face-local-variable))
    ("\\<\\$\\(mcs_[_a-zA-Z0-9]+\\)\\>"            (1 'xtdmacs-code-face-class-member-const-static))
    ("\\<\\$\\(ms_[_a-zA-Z0-9]+\\)\\>"             (1 'xtdmacs-code-face-class-member-static))
    ("\\<\\$\\(mc_[_a-zA-Z0-9]+\\)\\>"             (1 'xtdmacs-code-face-class-member-const))
    ("\\<\\$\\(m_[_a-zA-Z0-9]+\\)\\>"              (1 'xtdmacs-code-face-class-member))
    ("\\(::\\|;\\|->\\|=\\|+\\|-\\|/\\|\\.\\)"     (1 'xtdmacs-code-php-operator))
    ("\\((\\|)\\|\\$\\)"                           (1 'xtdmacs-code-php-operator))
    ("\\([_a-zA-Z0-9]+\\)::"                       (1 'font-lock-constant-face))
    ("\\(__[_A-Z]+__\\)"                           (1 'xtdmacs-code-face-macro))
    (" +static +"                                  .  'font-lock-keyword-face)
    (" +const +"                                   .  'font-lock-keyword-face)
    ("\\<\$?\\(p[ajmix6ubs]*_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-param))
    ("\\<\$?\\(c_[_a-zA-Z0-9]+\\)\\>"              (1 'xtdmacs-code-face-counter)))
  "Additional PHP font-lock keywords."
  :group 'xtdmacs-code-php
  :safe (lambda (_) t)
  :type '(alist :key-type string :value-type sexp))


;;;###autoload
(defun xtdmacs-code-php-setup ()
  "Configure a PHP buffer with xtdmacs conventions."
  (xtdmacs-code-setup)
  (font-lock-add-keywords nil xtdmacs-code-php-keywords-alist)
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-arglist)
  (c-set-offset 'arglist-close         'c-lineup-arglist-close-under-paren)
  (c-set-offset 'arglist-intro         'c-lineup-arglist-intro-after-paren)
  (setq c-basic-offset 2)
  (modify-syntax-entry ?_ "w")
  (when xtdmacs-code-php-indent-save-auto
    (add-hook 'before-save-hook #'xtdmacs-code-format-buffer-with-ident nil t))
  (when xtdmacs-code-php-indent-load-auto
    (xtdmacs-code-format-buffer-with-ident)))

;;;###autoload
(add-hook 'php-mode-hook #'xtdmacs-code-php-setup)

(provide 'xtdmacs-code-php)

;;; xtdmacs-code-php.el ends here

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
