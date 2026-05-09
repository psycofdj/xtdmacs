;;; xtdmacs-code-js.el --- JavaScript font-lock keywords  -*- lexical-binding: t -*-

;;; Commentary:

;; Adds Hungarian-style variable face keywords on top of js-mode / js2-mode.

;;; Code:

(require 'xtdmacs-code)

(use-package js2-mode
  :ensure t)

(defcustom xtdmacs-code-js-keywords-alist
  '(("\\<\\(gcs_[_a-zA-Z0-9]+\\)\\>" (1 'xtdmacs-code-face-global-variable-const-static))
    ("\\<\\(gs_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-global-variable-static))
    ("\\<\\(gc_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-global-variable-const))
    ("\\<\\(g_[_a-zA-Z0-9]+\\)\\>"   (1 'xtdmacs-code-face-global-variable))
    ("\\<\\(lcs_[_a-zA-Z0-9]+\\)\\>" (1 'xtdmacs-code-face-local-variable-const-static))
    ("\\<\\(ls_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-local-variable-static))
    ("\\<\\(lc_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-local-variable-const))
    ("\\<\\(l_[_a-zA-Z0-9]+\\)\\>"   (1 'xtdmacs-code-face-local-variable))
    ("\\<\\(mcs_[_a-zA-Z0-9]+\\)\\>" (1 'xtdmacs-code-face-class-member-const-static))
    ("\\([_a-zA-Z0-9]+\\)::"         (1 'font-lock-constant-face))
    ("\\(__[_A-Z]+__\\)"             (1 'xtdmacs-code-face-macro))
    (" +static +"                    .  'font-lock-keyword-face)
    (" +const +"                     .  'font-lock-keyword-face)
    ("\\<\\(self\\)\\>"              .  'font-lock-keyword-face)
    ("\\<\\(ms_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-class-member-static))
    ("\\<\\(mc_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-class-member-const))
    ("\\<\\(m_[_a-zA-Z0-9]+\\)\\>"   (1 'xtdmacs-code-face-class-member))
    ("\\<\\(my_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-class-member))
    ("\\<\\(p_[_a-zA-Z0-9]+\\)\\>"   (1 'xtdmacs-code-face-param))
    ("\\<\\(c_[_a-zA-Z0-9]+\\)\\>"   (1 'xtdmacs-code-face-counter))
    ("\\<\\(cc_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-counter-const)))
  "Additional JavaScript font-lock keywords."
  :group 'xtdmacs-code-js
  :safe (lambda (_) t)
  :type '(alist :key-type string :value-type sexp))

;;;###autoload
(defun xtdmacs-code-js-setup ()
  "Configure a JavaScript buffer with xtdmacs conventions."
  (xtdmacs-code-setup)
  (font-lock-add-keywords nil xtdmacs-code-js-keywords-alist))

;;;###autoload
(add-hook 'js2-mode-hook #'xtdmacs-code-js-setup)
;;;###autoload
(add-hook 'js-mode-hook  #'xtdmacs-code-js-setup)

(provide 'xtdmacs-code-js)

;;; xtdmacs-code-js.el ends here
