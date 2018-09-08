;; -*- lexical-binding: t -*-

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
    (" +static +" .                     'font-lock-keyword-face)
    (" +const +" .                      'font-lock-keyword-face)
    ("\\<\\(self\\)\\>" .               'font-lock-keyword-face)
    ("\\<\\(ms_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-class-member-static))
    ("\\<\\(mc_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-class-member-const))
    ("\\<\\(m_[_a-zA-Z0-9]+\\)\\>"   (1 'xtdmacs-code-face-class-member))
    ("\\<\\(my_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-class-member))
    ("\\<\\(pcs_[_a-zA-Z0-9]+\\)\\>" (1 'xtdmacs-code-face-param-const-static))
    ("\\<\\(ps_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-param-static))
    ("\\<\\(pc_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-param-const))
    ("\\<\\(p_[_a-zA-Z0-9]+\\)\\>"   (1 'xtdmacs-code-face-param))
    ("\\<\\(c_[_a-zA-Z0-9]+\\)\\>"   (1 'xtdmacs-code-face-counter))
    ("\\<\\(cc_[_a-zA-Z0-9]+\\)\\>"  (1 'xtdmacs-code-face-counter-const)))
  "List of additional js font-lock keywords"
  :group 'xtdmacs-code-js
  :safe '(lambda(p) t))


(defun --xtdmacs-code-js-mode-construct()
  (font-lock-add-keywords nil xtdmacs-code-js-keywords-alist)
  (message "enabled : xtdmacs-code-js-mode")
  )

(defun --xtdmacs-code-js-mode-destroy()
  (font-lock-remove-keywords nil xtdmacs-code-js-keywords-alist)
  (message "disabled : xtdmacs-code-js-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-js-mode "Code for Javascript" nil "Code" nil
  (if xtdmacs-code-js-mode
      (--xtdmacs-code-js-mode-construct)
    (--xtdmacs-code-js-mode-destroy))
  )

(provide 'xtdmacs-code-js)
