(require 'xtdmacs-compile++)

(defun xtdmacs-code-python-module-root ()
  (interactive)
  (let* ((origin (buffer-file-name))
         (dir (file-name-directory origin))
         (dirs (split-string dir "/"))
         (result nil))
    (while (and (> (length dirs) 1) (equal nil result))
      (if (not (file-exists-p (concat (mapconcat 'identity dirs "/") "/__init__.py")))
          (setq result (directory-file-name (concat (mapconcat 'identity dirs "/"))))
        (nbutlast dirs 1))
      )
    result))

(defun xtdmacs-code-python-pylint-bin ()
  (let* ((root       (xtdmacs-code-python-module-root))
         (sourcefile (file-truename buffer-file-name))
         (rcpath     (concat root "/.pylintrc"))
         (cmd        (concat "./devtools/xtdlint.py -j4 -f parseable --reports=n")))
    (if (file-exists-p rcpath)
        (concat cmd " --rcfile=" rcpath " " sourcefile)
      (concat cmd " " sourcefile))
    ))


(defun xtdmacs-code-python-test-bin ()
  (concat "./devtools/unittests.py -v " (file-truename buffer-file-name))
  )

(defun xtdmacs-code-python-params (mode)
  (let* ((config (cdr (assoc mode xtdmacs-compile++-config-alist)))
         (dir  (cdr (assoc "dir"  config)))
         (bin  (cdr (assoc "bin"  config))))
    (setcdr (assoc "dir"  config) (read-directory-name  "Directory: "  (funcall-or-value dir)))
    (setcdr (assoc "bin"  config) (read-from-minibuffer "Command: "    (funcall-or-value bin)))
    )
  )

(defun xtdmacs-code-python-command (mode)
  (let* ((config (cdr (assoc mode xtdmacs-compile++-config-alist)))
         (dir  (cdr (assoc "dir"  config)))
         (bin  (cdr (assoc "bin"  config))))
    (format "cd %s && %s" dir bin))
  )

;; --------------------------------------------------------------------------- ;

(defcustom xtdmacs-code-python-indent-load-auto
  nil
  "Enables code auto-indentation on load."
  :group 'xtdmacs-code-python
  :type 'boolean)

(defcustom xtdmacs-code-python-indent-save-auto
  nil
  "Enables code auto-indentation on save."
  :group 'xtdmacs-code-python
  :type 'boolean
  )

(defcustom xtdmacs-code-python-keywords-alist
  '(("\\<gcs_[_a-zA-Z0-9]+\\>" .     'xtdmacs-code-face-global-variable-const-static)
    ("\\<gs_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-global-variable-static)
    ("\\<gc_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-global-variable-const)
    ("\\<g_[_a-zA-Z0-9]+\\>" .       'xtdmacs-code-face-global-variable)
    ("\\<lcs_[_a-zA-Z0-9]+\\>" .     'xtdmacs-code-face-local-variable-const-static)
    ("\\<ls_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-local-variable-static)
    ("\\<lc_[_a-zA-Z0-9]+\\>" .      'xtdmacs-code-face-local-variable-const)
    ("\\<l_[_a-zA-Z0-9]+\\>" .       'xtdmacs-code-face-local-variable)
    ("\\<mcs_[_a-zA-Z0-9]+\\>" .     'xtdmacs-code-face-class-member-const-static)
    ("\\<metaclass\\>" .             'font-lock-keyword-face)
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
  "List of additional python font-lock keywords"
  :group 'xtdmacs-code-python
  :safe '(lambda(p) t))

(defcustom xtdmacs-code-python-compile-alist
  '(("compile" .
     (("dir"        . xtdmacs-code-python-module-root)
      ("bin"        . xtdmacs-code-python-pylint-bin)
      ("get-params" . (lambda() (xtdmacs-code-python-params  "compile")))
      ("command"    . (lambda() (xtdmacs-code-python-command "compile")))))
    ("test" .
     (("dir"        . xtdmacs-code-python-module-root)
      ("bin"        . xtdmacs-code-python-test-bin)
      ("get-params" . (lambda() (xtdmacs-code-python-params  "test")))
      ("command"    . (lambda() (xtdmacs-code-python-command "test"))))))
  "Xtdmacs-Code-python compilation configuration"
  :group 'xtdmacs-code-python
  :safe '(lambda(p) t)
  :type '(alist :key-type string :value-type (alist :key-type string :value-type (choice (string) (function))))
  )

;; --------------------------------------------------------------------------- ;

(defun --xtdmacs-code-python-construct()
  (font-lock-add-keywords nil xtdmacs-code-python-keywords-alist)
  (when (and (boundp 'xtdmacs-compile++-mode) xtdmacs-compile++-mode)
    (setcdr (assoc "compile" xtdmacs-compile++-config-alist)
            (cdr (assoc "compile" xtdmacs-code-python-compile-alist)))
    (setcdr (assoc "test" xtdmacs-compile++-config-alist)
            (cdr (assoc "test" xtdmacs-code-python-compile-alist)))
    )

  (if xtdmacs-code-python-indent-save-auto
      (add-hook 'before-save-hook 'xtdmacs-code-format-buffer-with-ident t t))
  (if xtdmacs-code-python-indent-load-auto
      (xtdmacs-code-format-buffer-with-ident))
  (message "disabled : xtdmacs-code-python-mode")
  )

(defun --xtdmacs-code-python-destroy()
  (if xtdmacs-code-python-indent-save-auto
      (remove-hook 'before-save-hook 'xtdmacs-code-format-buffer-with-ident))
  (font-lock-remove-keywords nil xtdmacs-code-python-keywords-alist)
  (message "disabled : xtdmacs-code-python-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-python-mode
  "Code for Python" nil "Code" nil
  (if xtdmacs-code-python-mode
      (--xtdmacs-code-python-construct)
    (--xtdmacs-code-python-destroy))
  )

(provide 'xtdmacs-code-python)
