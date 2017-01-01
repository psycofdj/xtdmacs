(require 'xtdmacs-compile++)
(require 'package)


(defcustom xtdmacs-code-sphinx-compile-alist
  '(("compile" .
     (("dir"        . xtdmacs-code-sphinx-project-root)
      ("bin"        . xtdmacs-code-sphinx-bin)
      ("get-params" . (lambda() (xtdmacs-code-sphinx-params  "compile")))
      ("command"    . (lambda() (xtdmacs-code-sphinx-command "compile")))))
    )
  "Xtdmacs-Code-sphinx  compilation configuration"
  :group 'xtdmacs-code-sphinx
  :safe '(lambda(p) t)
  :type '(alist :key-type string :value-type (alist :key-type string :value-type (choice (string) (function))))
  )



;; --------------------------------------------------------------------------- ;

(defun xtdmacs-code-sphinx-project-root ()
  (let* ((buffer-dir  (file-name-directory (buffer-file-name)))
         (conf        (concat buffer-dir "/conf.py")))
    (if (file-exists-p conf)
        buffer-dir
      nil))
  )


(defun xtdmacs-code-sphinx-bin ()
  (let* ((config        (cdr (assoc "compile" xtdmacs-compile++-config-alist)))
         (dir           (cdr (assoc "dir"     config)))
         (makefile-path (concat (funcall-or-value dir) "/Makefile")))
    (if (file-exists-p makefile-path)
        "make html"
      "sphinx-build -M html . build"))
  )


(defun xtdmacs-code-sphinx-params (type)
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

(defun xtdmacs-code-sphinx-command (mode)
  (let* ((config (cdr (assoc mode xtdmacs-compile++-config-alist)))
         (dir  (cdr (assoc "dir"  config)))
         (bin  (cdr (assoc "bin"  config))))
    (format "cd %s && %s"
            (funcall-or-value dir)
            (funcall-or-value bin)))
  )

;; --------------------------------------------------------------------------- ;

(defun --xtdmacs-code-sphinx-construct()
  (electric-indent-mode -1)
  (when (and (boundp 'xtdmacs-compile++-mode) xtdmacs-compile++-mode)
    (setcdr (assoc "compile" xtdmacs-compile++-config-alist)
            (cdr (assoc "compile" xtdmacs-code-sphinx-compile-alist)))
    (setcdr (assoc "test" xtdmacs-compile++-config-alist)
            (cdr (assoc "test" xtdmacs-code-sphinx-compile-alist)))
    )
  (message "disabled : xtdmacs-code-sphinx-mode")
  )

(defun --xtdmacs-code-sphinx-destroy()
  (message "disabled : xtdmacs-code-sphinx-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-sphinx-mode
  "Code for Sphinx documentation" nil "Code" nil
  (if xtdmacs-code-sphinx-mode
      (--xtdmacs-code-sphinx-construct)
    (--xtdmacs-code-sphinx-destroy))
  )


(provide 'xtdmacs-code-sphinx)

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
