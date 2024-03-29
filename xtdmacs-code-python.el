;; -*- lexical-binding: t -*-

(require 'xtdmacs-compile++)
(require 'package)
(require 'yasnippet)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (python-mode . lsp-deferred))

(use-package company
  :ensure t)

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(defcustom xtdmacs-code-python-compile-alist
  '((:compile .
              ((:dir        . xtdmacs-code-python-project-root)
               (:bin        . xtdmacs-code-python-pylint-bin)
               (:env        . "")
               (:get-params . xtdmacs-compile++-default-params)
               (:command    . xtdmacs-compile++-default-command)))
    (:test .
           ((:dir        . xtdmacs-code-python-project-root)
            (:bin        . xtdmacs-code-python-test-bin)
            (:env        . "")
            (:get-params . xtdmacs-compile++-default-params)
            (:command    . xtdmacs-compile++-default-command))))
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
         (distrcpath   (concat  (xtdmacs-get-install-dir)  "/vendor/pylintrc"))
         (args         (concat "-j4 -f parseable --reports=n")))

    (cond
     ((file-exists-p rcpath)       (concat args " --rcfile=" rcpath))
     ((file-exists-p globalrcpath) (concat args " --rcfile=" globalrcpath))
     ((file-exists-p distrcpath)   (concat args " --rcfile=" distrcpath))
     (t args)))
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
  (let* ((dir    (--xtdmacs-compile++-prompt-value mode type :dir "Directory"))
         (bin    (--xtdmacs-compile++-prompt-value mode type :bin "Binary")))
    (xtdmacs-compile++-query-local)
    (--xtdmacs-compile++-set-value mode type :dir dir)
    (--xtdmacs-compile++-set-value mode type :bin bin))
  )

(defun xtdmacs-code-python-command(type &optional mode)
  (let* ((dir    (--xtdmacs-compile++-get-value mode type :dir))
         (bin    (--xtdmacs-compile++-get-value mode type :bin)))
    (format "cd %s && %s"
            (funcall-or-value dir)
            (funcall-or-value bin)))
  )

(defun --xtdmacs-code-python-find-definition-other-window()
  (interactive)
  (lsp-find-definition :display-action 'window))

(defun --xtdmacs-code-python-find-references-other-window()
  (interactive)
  (lsp-find-references t :display-action 'window))

(defun --xtdmacs-code-python-find-references()
  (interactive)
  (lsp-find-references nil :display-action 'window))

;; --------------------------------------------------------------------------- ;

(defun --xtdmacs-code-python-construct()
  (yas-minor-mode t)
  (lsp)
  (font-lock-add-keywords nil xtdmacs-code-python-keywords-alist)

  (when (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "python-mode" xtdmacs-code-python-compile-alist))

  (if xtdmacs-code-python-indent-save-auto
      (progn
        (add-hook 'before-save-hook #'lsp-format-buffer t t)))

  (if xtdmacs-code-python-indent-load-auto
      (progn
        (add-hook 'before-save-hook #'lsp-format-buffer t t)))

  (if xtdmacs-code-python-indent-save-auto
      (add-hook 'before-save-hook 'xtdmacs-code-format-buffer-with-ident t t))
  (if xtdmacs-code-python-indent-load-auto
      (xtdmacs-code-format-buffer-with-ident))
  (message "enabled : xtdmacs-code-python-mode")
  )

(defun --xtdmacs-code-python-destroy()
  (when (mode-enabled 'flycheck-mode)
    (flycheck-mode nil))
  (when (mode-enabled 'auto-complete-mode)
    (auto-complete-mode nil))
  (if xtdmacs-code-python-indent-save-auto
      (remove-hook 'before-save-hook 'xtdmacs-code-format-buffer-with-ident))
  (font-lock-remove-keywords nil xtdmacs-code-python-keywords-alist)
  (message "disabled : xtdmacs-code-python-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-python-mode
  "Code for Python" nil "Code"
  `(("\M-t"           . lsp-format-region)
    ("\C-\M-t"        . lsp-format-buffer)
    ("\M-r"           . lsp-rename)
    ("\M-."           . company-complete)
    (,(kbd "<f12>")   . lsp-find-definition)
    (,(kbd "C-<f12>") . --xtdmacs-code-python-find-definition-other-window)
    (,(kbd "<f11>")   . --xtdmacs-code-python-find-references)
    (,(kbd "C-<f11>") . --xtdmacs-code-python-find-references-other-window)
    (,(kbd "C-<f10>") . lsp-ui-imenu))

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
