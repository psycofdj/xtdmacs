(defcustom code-java-keywords-alist
  '(("\\<gcs_[_a-zA-Z0-9]+\\>" .     'code-face-global-variable-const-static)
    ("\\<gs_[_a-zA-Z0-9]+\\>" .      'code-face-global-variable-static)
    ("\\<gc_[_a-zA-Z0-9]+\\>" .      'code-face-global-variable-const)
    ("\\<g_[_a-zA-Z0-9]+\\>" .       'code-face-global-variable)
    ("\\<lcs_[_a-zA-Z0-9]+\\>" .     'code-face-local-variable-const-static)
    ("\\<ls_[_a-zA-Z0-9]+\\>" .      'code-face-local-variable-static)
    ("\\<lc_[_a-zA-Z0-9]+\\>" .      'code-face-local-variable-const)
    ("\\<l_[_a-zA-Z0-9]+\\>" .       'code-face-local-variable)
    ("\\<mcs_[_a-zA-Z0-9]+\\>" .     'code-face-class-member-const-static)
    ("\\<ms_[_a-zA-Z0-9]+\\>" .      'code-face-class-member-static)
    ("\\<mc_[_a-zA-Z0-9]+\\>" .      'code-face-class-member-const)
    ("\\<m_[_a-zA-Z0-9]+\\>" .       'code-face-class-member)
    ("\\<my_[_a-zA-Z0-9]+\\>" .      'code-face-class-member)
    ("\\<pcs_[_a-zA-Z0-9]+\\>" .     'code-face-param-const-static)
    ("\\<ps_[_a-zA-Z0-9]+\\>" .      'code-face-param-static)
    ("\\<pc_[_a-zA-Z0-9]+\\>" .      'code-face-param-const)
    ("\\<\\(p_[_a-zA-Z0-9]+\\)\\>" . 'code-face-param)
    ("\\<c_[_a-zA-Z0-9]+\\>" .       'code-face-counter)
    ("\\<cc_[_a-zA-Z0-9]+\\>" .      'code-face-counter-const))
  "List of additional java font-lock keywords"
  :group 'code-java
  :safe '(lambda(p) t)
  )

(defun --code-java-mode-constrcut()
  (font-lock-add-keywords nil code-java-keywords-alist)
  (message "enabled : code-java-mode")
  )

(defun --code-java-mode-destroy()
  (font-lock-remove-keywords nil code-java-keywords-alist)
  (message "disabled : code-java-mode")
  )

;;;###autoload
(define-minor-mode code-java-mode "Code for Java" nil "Code" nil
  (if code-java-mode
      (--code-java-mode-constrcut)
    (--code-java-mode-destroy))
  )
