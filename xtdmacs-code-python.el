;;; xtdmacs-code-python.el --- Python support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for python-mode buffers: LSP, font-lock keywords,
;; key bindings, pylint-driven compile and a configurable test runner.

;;; Code:

(require 'xtdmacs-code)

(declare-function xtdmacs-compile++-register-config "xtdmacs-compile++")
(declare-function xtdmacs-compile++-query-local     "xtdmacs-compile++")
(declare-function --xtdmacs-compile++-get-value     "xtdmacs-compile++")
(declare-function --xtdmacs-compile++-set-value     "xtdmacs-compile++")
(declare-function --xtdmacs-compile++-prompt-value  "xtdmacs-compile++")

(defcustom xtdmacs-code-python-compile-alist
  '((:compile . ((:dir        . xtdmacs-code-python-project-root)
                 (:bin        . xtdmacs-code-python-pylint-bin)
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))
    (:test    . ((:dir        . xtdmacs-code-python-project-root)
                 (:bin        . xtdmacs-code-python-test-bin)
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command))))
  "Python compilation configuration."
  :group 'xtdmacs-code-python
  :safe (lambda (_) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function)))))

(defcustom xtdmacs-code-python-keywords-alist
  '(("\\<gcs_[_a-zA-Z0-9]+\\>"     . 'xtdmacs-code-face-global-variable-const-static)
    ("\\<gs_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-global-variable-static)
    ("\\<gc_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-global-variable-const)
    ("\\<g_[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-global-variable)
    ("\\<lcs_[_a-zA-Z0-9]+\\>"     . 'xtdmacs-code-face-local-variable-const-static)
    ("\\<ls_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-local-variable-static)
    ("\\<lc_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-local-variable-const)
    ("\\<l_[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-local-variable)
    ("\\<mcs_[_a-zA-Z0-9]+\\>"     . 'xtdmacs-code-face-class-member-const-static)
    ("\\<metaclass\\>"             . 'font-lock-keyword-face)
    ("\\<ms_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-class-member-static)
    ("\\<mc_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-class-member-const)
    ("\\<m_[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-class-member)
    ("\\<my_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-class-member)
    ("\\<\\(p_[_a-zA-Z0-9]+\\)\\>" . 'xtdmacs-code-face-param)
    ("\\<c_[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-counter)
    ("\\<cc_[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-counter-const))
  "Additional Python font-lock keywords."
  :group 'xtdmacs-code-python
  :safe (lambda (_) t)
  :type '(alist :key-type string :value-type sexp))

(defcustom xtdmacs-code-python-format-on-save nil
  "When non-nil, run lsp-format-buffer on save."
  :group 'xtdmacs-code-python :type 'boolean :safe #'booleanp)

(defcustom xtdmacs-code-python-pylint-args 'xtdmacs-code-python-pylint-getargs
  "Static string or function returning pylint arguments."
  :group 'xtdmacs-code-python
  :type '(choice (string :tag "string") (function :tag "function"))
  :safe (lambda (_) t))

(defcustom xtdmacs-code-python-pylint-bin-path "pylint"
  "Pylint binary path."
  :group 'xtdmacs-code-python :type 'file :safe #'file-exists-p)

(defcustom xtdmacs-code-python-test-args "-v"
  "Static string or function returning test runner arguments."
  :group 'xtdmacs-code-python
  :type '(choice (string :tag "string") (function :tag "function"))
  :safe (lambda (_) t))

(defcustom xtdmacs-code-python-test-bin-path nil
  "Test runner path (nil = use bundled xtdmacs runner)."
  :group 'xtdmacs-code-python :type 'file :safe #'file-exists-p)

;; ---------------------------------------------------------------------------
;; Helpers.

(defun xtdmacs-code-python-module-root ()
  "Return the directory of the topmost __init__.py-bearing parent."
  (let* ((origin   (buffer-file-name))
         (dir      (file-name-directory origin))
         (dirs     (split-string dir "/"))
         (last-dir (car (last dirs)))
         (result   nil))
    (while (and (> (length dirs) 1) (null result))
      (let* ((init-dir  (mapconcat #'identity dirs "/"))
             (init-file (concat init-dir "/__init__.py")))
        (unless (file-exists-p init-file)
          (setq result (concat init-dir "/" last-dir)))
        (setq last-dir (car (last dirs)))
        (nbutlast dirs 1)))
    (or result (directory-file-name (buffer-file-name)))))

(defun xtdmacs-code-python-project-root ()
  "Return the parent directory of the python module root."
  (let* ((module-root (xtdmacs-code-python-module-root))
         (buffer-dir  (directory-file-name (buffer-file-name))))
    (if (string= module-root buffer-dir)
        buffer-dir
      (file-name-directory (directory-file-name module-root)))))

(defun xtdmacs-code-python-pylint-getargs ()
  "Return pylint arguments, picking the first available .pylintrc."
  (let* ((root         (xtdmacs-code-python-project-root))
         (rcpath       (concat root "/.pylintrc"))
         (globalrcpath (concat (getenv "HOME") "/.pylintrc"))
         (distrcpath   (concat (xtdmacs-get-install-dir) "/vendor/pylintrc"))
         (args         "-j4 -f parseable --reports=n"))
    (cond ((file-exists-p rcpath)       (concat args " --rcfile=" rcpath))
          ((file-exists-p globalrcpath) (concat args " --rcfile=" globalrcpath))
          ((file-exists-p distrcpath)   (concat args " --rcfile=" distrcpath))
          (t                            args))))

(defun xtdmacs-code-python-pylint-bin ()
  "Return the full pylint command line for the current buffer."
  (concat xtdmacs-code-python-pylint-bin-path " "
          (funcall-or-value xtdmacs-code-python-pylint-args) " "
          (file-truename buffer-file-name)))

(defun xtdmacs-code-python-test-bin ()
  "Return the full test runner command line."
  (concat (or xtdmacs-code-python-test-bin-path
              (concat (xtdmacs-get-install-dir) "/bin/unittests.py"))
          " "
          (funcall-or-value xtdmacs-code-python-test-args) " "))

(defun xtdmacs-code-python-params (type &optional mode)
  "Prompt user for :dir and :bin of compile entry TYPE in MODE and store them."
  (let ((dir (--xtdmacs-compile++-prompt-value mode type :dir "Directory"))
        (bin (--xtdmacs-compile++-prompt-value mode type :bin "Binary")))
    (xtdmacs-compile++-query-local)
    (--xtdmacs-compile++-set-value mode type :dir dir)
    (--xtdmacs-compile++-set-value mode type :bin bin)))

(defun xtdmacs-code-python-command (type &optional mode)
  "Build a `cd DIR && BIN' command for compile entry TYPE in MODE."
  (let ((dir (--xtdmacs-compile++-get-value mode type :dir))
        (bin (--xtdmacs-compile++-get-value mode type :bin)))
    (format "cd %s && %s" (funcall-or-value dir) (funcall-or-value bin))))

;;;###autoload
(defun xtdmacs-code-python-setup ()
  "Configure a Python buffer with xtdmacs conventions."
  (xtdmacs-code-setup :with-lsp t)
  (font-lock-add-keywords nil xtdmacs-code-python-keywords-alist)
  (when (bound-and-true-p xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "python-mode" xtdmacs-code-python-compile-alist))
  (when xtdmacs-code-python-format-on-save
    (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

;;;###autoload
(add-hook 'python-mode-hook #'xtdmacs-code-python-setup)

;;;###autoload (put 'xtdmacs-code-python-compile-alist 'safe-local-variable (lambda (_) t))
;;;###autoload (put 'xtdmacs-code-python-keywords-alist 'safe-local-variable (lambda (_) t))
;;;###autoload (put 'xtdmacs-code-python-format-on-save 'safe-local-variable #'booleanp)
;;;###autoload (put 'xtdmacs-code-python-pylint-args 'safe-local-variable (lambda (_) t))
;;;###autoload (put 'xtdmacs-code-python-pylint-bin-path 'safe-local-variable #'file-exists-p)
;;;###autoload (put 'xtdmacs-code-python-test-args 'safe-local-variable (lambda (_) t))
;;;###autoload (put 'xtdmacs-code-python-test-bin-path 'safe-local-variable #'file-exists-p)

(provide 'xtdmacs-code-python)

;;; xtdmacs-code-python.el ends here
