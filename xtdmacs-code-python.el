(require 'xtdmacs-compile++)
(require 'package)

(defcustom xtdmacs-code-python-compile-alist
  '(("compile" .
     (("dir"        . xtdmacs-code-python-project-root)
      ("bin"        . xtdmacs-code-python-pylint-bin)
      ("get-params" . xtdmacs-compile++-default-params)
      ("command"    . xtdmacs-compile++-default-command)))
    ("test" .
     (("dir"        . xtdmacs-code-python-project-root)
      ("bin"        . xtdmacs-code-python-test-bin)
      ("get-params" . xtdmacs-compile++-default-params)
      ("command"    . xtdmacs-compile++-default-command))))
  "Xtdmacs-Code-python compilation configuration"
  :group 'xtdmacs-code-python
  :safe '(lambda(p) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function))))
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

(defcustom xtdmacs-code-python-indent-load-auto
  nil
  "Enables python code auto-indentation on load."
  :group 'xtdmacs-code-python
  :type 'boolean
  :safe 'booleanp)

(defcustom xtdmacs-code-python-indent-save-auto
  nil
  "Enables python code auto-indentation on save."
  :group 'xtdmacs-code-python
  :type 'boolean
  :safe 'booleanp
  )

(defcustom xtdmacs-code-python-pylint-args 'xtdmacs-code-python-pylint-getargs
  "Static string or function to use as pylint script argument"
  :group 'xtdmacs-code-python
  :type '(choice (string   :tag "string")
                 (function :tag "function"))
  :safe '(lambda(p) t))

(defcustom xtdmacs-code-python-pylint-bin-path "/usr/local/bin/pylint"
  "pylint static code checker file path"
  :group 'xtdmacs-code-python
  :type 'file
  :safe 'file-exists-p)

(defcustom xtdmacs-code-python-test-args "-v"
  "Static string or function to use as test binary arguments"
  :group 'xtdmacs-code-python
  :type '(choice (string :tag "string")
                 (function :tag "function"))
  :safe '(lambda(p) t))

(defcustom xtdmacs-code-python-test-bin-path nil
  "Unit test runner file path. If nil, use default xtdmacs runner"
  :group 'xtdmacs-code-python
  :type 'file
  :safe 'file-exists-p)

;; --------------------------------------------------------------------------- ;

(defun xtdmacs-code-python-module-root ()
  (let* ((origin   (buffer-file-name))
         (dir      (file-name-directory origin))
         (dirs     (split-string dir "/"))
         (last-dir (car (last dirs)))
         (result   nil))

    (while (and (> (length dirs) 1) (equal nil result))
      (let* ((init-dir  (concat (mapconcat 'identity dirs "/")))
             (init-file (concat init-dir "/__init__.py")))
        (if (not (file-exists-p init-file))
            (setq result (concat init-dir "/" last-dir)))
        (setq last-dir (car (last dirs)))
        (nbutlast dirs 1)))

    (if result
        result
      (directory-file-name (buffer-file-name)))
    )
  )

(defun xtdmacs-code-python-project-root ()
  (let* ((module-root (xtdmacs-code-python-module-root))
         (buffer-dir  (directory-file-name (buffer-file-name))))
    (if (string= module-root buffer-dir)
        buffer-dir
      (file-name-directory (directory-file-name module-root))))
  )

(defun xtdmacs-code-python-pylint-getargs()
  (let* ((root         (xtdmacs-code-python-project-root))
         (rcpath       (concat root "/.pylintrc"))
         (globalrcpath (concat  (getenv "HOME") "/.pylintrc"))
         (args         (concat "-j4 -f parseable --reports=n")))

    (message "rcfile : %s" rcpath)
    (message "grcfile : %s" globalrcpath)

    (if (file-exists-p rcpath)
        (concat args " --rcfile=" rcpath)
      (if (file-exists-p globalrcpath)
          (concat args " --rcfile=" globalrcpath)
        args)))
  )

(defun xtdmacs-code-python-pylint-bin ()
  (let* ((sourcefile (file-truename buffer-file-name))
         (args       (funcall-or-value xtdmacs-code-python-pylint-args))
         (bin        xtdmacs-code-python-pylint-bin-path))
    (concat bin " " args " " sourcefile))
  )

(defun xtdmacs-code-python-test-bin ()
  (concat
   (if xtdmacs-code-python-test-bin-path
       xtdmacs-code-python-test-bin-path
     (concat (xtdmacs-get-install-dir) "/bin/unittests.py"))
   " "
   (funcall-or-value xtdmacs-code-python-test-args) " ")
  )

(defun xtdmacs-code-python-params (type &optional mode)
  (let* ((locaval (copy-tree xtdmacs-compile++-config-alist))
         (config (cdr (assoc type locaval)))
         (dir (cdr (assoc "dir" config)))
         (bin (cdr (assoc "bin" config)))
         (new_dir (read-directory-name  "Directory: " (funcall-or-value dir)))
         (new_bin (read-from-minibuffer "Binary: "    (funcall-or-value bin))))
    (progn
      (setcdr (assoc "dir" config) new_dir)
      (setcdr (assoc "bin" config) new_bin)
      (setq xtdmacs-compile++-config-alist locaval))
    )
  )

(defun xtdmacs-code-python-command (mode)
  (let* ((config (cdr (assoc mode xtdmacs-compile++-config-alist)))
         (dir  (cdr (assoc "dir"  config)))
         (bin  (cdr (assoc "bin"  config))))
    (format "cd %s && %s"
            (funcall-or-value dir)
            (funcall-or-value bin)))
  )

;; --------------------------------------------------------------------------- ;

(defun --xtdmacs-code-python-construct()
  (font-lock-add-keywords nil xtdmacs-code-python-keywords-alist)

  (when (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "python-mode" xtdmacs-code-python-compile-alist))

  (if xtdmacs-code-python-indent-save-auto
      (add-hook 'before-save-hook 'xtdmacs-code-format-buffer-with-ident t t))
  (if xtdmacs-code-python-indent-load-auto
      (xtdmacs-code-format-buffer-with-ident))
  (message "enabled : xtdmacs-code-python-mode")
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


;; --------------------------------------------------------------------------- ;

;;;###autoload
(put 'xtdmacs-code-python-compile-alist 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-python-keywords-alist 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-python-indent-load-auto 'safe-local-variable 'booleanp)
;;;###autoload
(put 'xtdmacs-code-python-indent-save-auto 'safe-local-variable 'booleanp)
;;;###autoload
(put 'xtdmacs-code-python-pylint-args 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-python-pylint-bin-path 'safe-local-variable 'file-exists-p)
;;;###autoload
(put 'xtdmacs-code-python-test-args 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-python-test-bin-path 'safe-local-variable 'file-exists-p)

(provide 'xtdmacs-code-python)

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
