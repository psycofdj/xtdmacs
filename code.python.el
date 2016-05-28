;; --------------------------------------------------------------------------- ;

(defun code-python-module-root ()
  (interactive)
  (setq origin (buffer-file-name))
  (let* ((dir (file-name-directory origin))
         (dirs (split-string dir "/"))
         (result nil))
    (while (and (> (length dirs) 1) (equal nil result))
      (if (not (file-exists-p (concat (mapconcat 'identity dirs "/") "/__init__.py")))
          (setq result (directory-file-name (concat (mapconcat 'identity dirs "/"))))
        (nbutlast dirs 1))
      )
    result))

(defun code-python-pylint-bin ()
  (let* ((root       (code-python-module-root))
         (sourcefile (file-truename buffer-file-name))
         (rcpath     (concat root "/.pylintrc"))
         (cmd        (concat "./devtools/xtdlint.py -j4 -f parseable --reports=n")))
    (if (file-exists-p rcpath)
        (concat cmd " --rcfile=" rcpath " " sourcefile)
      (concat cmd " " sourcefile))
    ))


(defun code-python-test-bin ()
  (concat "./devtools/unittests.py -v " (file-truename buffer-file-name))
  )

(defun code-python-params (mode)
  (let* ((config (cdr (assoc mode compile++-config-alist)))
         (dir  (cdr (assoc "dir"  config)))
         (bin  (cdr (assoc "bin"  config))))
    (setcdr (assoc "dir"  config) (read-directory-name  "Directory: "  (funcall-or-value dir)))
    (setcdr (assoc "bin"  config) (read-from-minibuffer "Command: "    (funcall-or-value bin)))
    )
  )

(defun code-python-command (mode)
  (let* ((config (cdr (assoc mode compile++-config-alist)))
         (dir  (cdr (assoc "dir"  config)))
         (bin  (cdr (assoc "bin"  config))))
    (format "cd %s && %s" dir bin))
  )

;; --------------------------------------------------------------------------- ;

(defcustom code-python-indent-load-auto
  nil
  "Enables code auto-indentation on load."
  :group 'code-python
  :type 'boolean)

(defcustom code-python-indent-save-auto
  nil
  "Enables code auto-indentation on save."
  :group 'code-python
  :type 'boolean
  )

(defcustom code-python-keywords-alist
  '(("\\<gcs_[_a-zA-Z0-9]+\\>" .     'code-face-global-variable-const-static)
    ("\\<gs_[_a-zA-Z0-9]+\\>" .      'code-face-global-variable-static)
    ("\\<gc_[_a-zA-Z0-9]+\\>" .      'code-face-global-variable-const)
    ("\\<g_[_a-zA-Z0-9]+\\>" .       'code-face-global-variable)
    ("\\<lcs_[_a-zA-Z0-9]+\\>" .     'code-face-local-variable-const-static)
    ("\\<ls_[_a-zA-Z0-9]+\\>" .      'code-face-local-variable-static)
    ("\\<lc_[_a-zA-Z0-9]+\\>" .      'code-face-local-variable-const)
    ("\\<l_[_a-zA-Z0-9]+\\>" .       'code-face-local-variable)
    ("\\<mcs_[_a-zA-Z0-9]+\\>" .     'code-face-class-member-const-static)
    ("\\<metaclass\\>" .             'font-lock-keyword-face)
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
  "List of additional python font-lock keywords"
  :group 'code-python
  :safe '(lambda(p) t))

(defcustom code-python-compile-alist
  '(("compile" .
     (("dir"        . code-python-module-root)
      ("bin"        . code-python-pylint-bin)
      ("get-params" . (lambda() (code-python-params  "compile")))
      ("command"    . (lambda() (code-python-command "compile")))))
    ("test" .
     (("dir"        . code-python-module-root)
      ("bin"        . code-python-test-bin)
      ("get-params" . (lambda() (code-python-params  "test")))
      ("command"    . (lambda() (code-python-command "test"))))))
  "Code-python compilation configuration"
  :group 'code-python
  :safe '(lambda(p) t)
  :type '(alist :key-type string :value-type (alist :key-type string :value-type (choice (string) (function))))
  )

;; --------------------------------------------------------------------------- ;

(defun --code-python-construct()
  (font-lock-add-keywords nil code-python-keywords-alist)
  (when (and (boundp 'compile++-mode) compile++-mode)
    (setcdr (assoc "compile" compile++-config-alist)
            (cdr (assoc "compile" code-python-compile-alist)))
    (setcdr (assoc "test" compile++-config-alist)
            (cdr (assoc "test" code-python-compile-alist)))
    )

  (if code-python-indent-save-auto
      (add-hook 'before-save-hook 'code-format-buffer-with-ident t t))
  (if code-python-indent-load-auto
      (code-format-buffer-with-ident))
  (message "disabled : code-python-mode")
  )

(defun --code-python-destroy()
  (if code-python-indent-load-auto
      (remove-format-buffer-with-ident))
  (if code-python-indent-save-auto
      (remove-hook 'before-save-hook 'code-format-buffer-with-ident))
  (font-lock-remove-keywords nil code-python-keywords-alist)
  (message "disabled : code-python-mode")
  )

;;;###autoload
(define-minor-mode code-python-mode
  "Code for Python" nil "Code" nil
  (if code-python-mode
      (--code-python-construct)
    (--code-python-destroy))
  )
