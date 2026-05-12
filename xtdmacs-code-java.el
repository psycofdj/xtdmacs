;;; xtdmacs-code-java.el --- Java font-lock keywords  -*- lexical-binding: t -*-

;;; Commentary:

;; Adds Hungarian-style variable face keywords on top of the built-in
;; java-mode major mode.

;;; Code:

(require 'xtdmacs-code)

(defcustom xtdmacs-code-java-keywords-alist
  '(("\\<gcs_[_a-zA-Z0-9]+\\>"     . 'xtdmacs-code-face-global-variable-const-static)
    ("\\<gs_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-global-variable-static)
    ("\\<gc_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-global-variable-const)
    ("\\<g_[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-global-variable)
    ("\\<lcs_[_a-zA-Z0-9]+\\>"     . 'xtdmacs-code-face-local-variable-const-static)
    ("\\<ls_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-local-variable-static)
    ("\\<lc_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-local-variable-const)
    ("\\<l_[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-local-variable)
    ("\\<mcs_[_a-zA-Z0-9]+\\>"     . 'xtdmacs-code-face-class-member-const-static)
    ("\\<ms_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-class-member-static)
    ("\\<mc_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-class-member-const)
    ("\\<m_[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-class-member)
    ("\\<my_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-class-member)
    ("\\<\\(p_[_a-zA-Z0-9]+\\)\\>" . 'xtdmacs-code-face-param)
    ("\\<c_[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-counter)
    ("\\<cc_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-counter-const))
  "Additional Java font-lock keywords."
  :group 'xtdmacs-code-java
  :safe (lambda (_) t)
  :type '(alist :key-type string :value-type sexp))

;;;###autoload
(defun xtdmacs-code-java-setup ()
  "Configure a Java buffer with xtdmacs conventions."
  (xtdmacs-code-setup :with-lsp t)
  (font-lock-add-keywords nil xtdmacs-code-java-keywords-alist))

;;;###autoload
(add-hook 'java-mode-hook #'xtdmacs-code-java-setup)

(provide 'xtdmacs-code-java)

;;; xtdmacs-code-java.el ends here
