(defcustom xtdmacs-code-java-keywords-alist
  '(("\\<gcs_[_a-zA-Z0-9]+\\>" .     'xtdmacs-code-face-global-variable-const-static)
    ("\\<gs_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-global-variable-static)
    ("\\<gc_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-global-variable-const)
    ("\\<g_[_a-zA-Z0-9]+\\>" .       'xtdmacs-code-face-global-variable)
    ("\\<lcs_[_a-zA-Z0-9]+\\>" .     'xtdmacs-code-face-local-variable-const-static)
    ("\\<ls_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-local-variable-static)
    ("\\<lc_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-local-variable-const)
    ("\\<l_[_a-zA-Z0-9]+\\>" .       'xtdmacs-code-face-local-variable)
    ("\\<mcs_[_a-zA-Z0-9]+\\>" .     'xtdmacs-code-face-class-member-const-static)
    ("\\<ms_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-class-member-static)
    ("\\<mc_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-class-member-const)
    ("\\<m_[_a-zA-Z0-9]+\\>" .       'xtdmacs-code-face-class-member)
    ("\\<my_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-class-member)
    ("\\<pcs_[_a-zA-Z0-9]+\\>" .     'xtdmacs-code-face-param-const-static)
    ("\\<ps_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-param-static)
    ("\\<pc_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-param-const)
    ("\\<\\(p_[_a-zA-Z0-9]+\\)\\>" . 'xtdmacs-code-face-param)
    ("\\<c_[_a-zA-Z0-9]+\\>" .       'xtdmacs-code-face-counter)
    ("\\<cc_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-counter-const))
  "List of additional java font-lock keywords"
  :group 'xtdmacs-code-java
  :safe '(lambda(p) t)
  )

(defun --xtdmacs-code-java-mode-constrcut()
  (font-lock-add-keywords nil xtdmacs-code-java-keywords-alist)
  (message "enabled : xtdmacs-code-java-mode")
  )

(defun --xtdmacs-code-java-mode-destroy()
  (font-lock-remove-keywords nil xtdmacs-code-java-keywords-alist)
  (message "disabled : xtdmacs-code-java-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-java-mode "Code for Java" nil "Code" nil
  (if xtdmacs-code-java-mode
      (--xtdmacs-code-java-mode-constrcut)
    (--xtdmacs-code-java-mode-destroy))
  )

(provide 'xtdmacs-code-java)
