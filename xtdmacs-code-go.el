;;; xtdmacs-code-go.el --- Go support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for go-mode buffers: LSP, yas, font-lock keywords,
;; key bindings, and a project-aware compile/test recipe.

;;; Code:

(require 'xtdmacs-code)

(declare-function xtdmacs-compile++-register-config "xtdmacs-compile++")
(declare-function xtdmacs-compile++-get-dir-git     "xtdmacs-compile++")
(declare-function --xtdmacs-compile++-get-value     "xtdmacs-compile++")

(defface xtdmacs-code-go-face-indent-error
  '((t (:foreground "color-124" :underline t)))
  "Indicates spaces instead of tabs for indentation."
  :group 'xtdmacs-code-go)

(defcustom xtdmacs-code-go-compile-alist
  '((:compile . ((:dir        . xtdmacs-compile++-get-dir-git)
                 (:bin        . xtdmacs-code-go-get-project-name)
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-code-go-command)))
    (:test    . ((:dir        . xtdmacs-compile++-get-dir-git)
                 (:bin        . xtdmacs-code-go-get-project-name)
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-code-go-command)))
    (:doc     . ((:dir        . xtdmacs-compile++-get-dir-buffer)
                 (:bin        . "nakedret")
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))
    (:lint    . ((:dir        . xtdmacs-compile++-get-dir-buffer)
                 (:bin        . "gometalinter.v2 -D gocyclo -D errcheck")
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))
    (:manual  . ((:dir        . xtdmacs-compile++-get-dir-git)
                 (:bin        . "true")
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command))))
  "Go compilation configuration."
  :group 'xtdmacs-code-go
  :safe (lambda (_) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function)))))

(defcustom xtdmacs-code-go-keywords-alist
  '(("\\<g_[_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-global-variable)
    ("\\<l_[_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-local-variable)
    ("\\<\\(p_[_a-zA-Z0-9]+\\)\\>"     . 'xtdmacs-code-face-param)
    ("\\<c_[_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-counter)
    ("\\<g[A-Z][_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-global-variable)
    ("\\<l[A-Z][_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-local-variable)
    ("\\<\\(p[A-Z][_a-zA-Z0-9]+\\)\\>" . 'xtdmacs-code-face-param)
    ("\\<c[A-Z][_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-counter)
    ("\\<r[A-Z][_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-return)
    ("[&*]"                            . 'font-lock-constant-face)
    ("self"                            . 'font-lock-keyword-face)
    ("^ +"                             . 'xtdmacs-code-go-face-indent-error))
  "Additional Go font-lock keywords."
  :group 'xtdmacs-code-go
  :safe (lambda (_) t)
  :type '(alist :key-type string :value-type sexp))

(defcustom xtdmacs-code-go-format-on-save nil
  "When non-nil, run lsp-organize-imports + lsp-format-buffer on save."
  :group 'xtdmacs-code-go :type 'boolean :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Helpers.

(defun xtdmacs-code-go-get-project-name ()
  "Return the basename of the current go project's git root."
  (file-name-nondirectory
   (directory-file-name (xtdmacs-compile++-get-dir-git))))

(defun xtdmacs-code-go-command (type &optional mode)
  "Build the `go build' command for compile entry TYPE in MODE."
  (let ((dir (--xtdmacs-compile++-get-value mode type :dir))
        (env (--xtdmacs-compile++-get-value mode type :env))
        (bin (--xtdmacs-compile++-get-value mode type :bin)))
    (format "cd %s && %s go build -o %s *.go"
            (funcall-or-value dir)
            (funcall-or-value env)
            (funcall-or-value bin))))

;; ---------------------------------------------------------------------------
;; Per-buffer setup.

;;;###autoload
(defun xtdmacs-code-go-setup ()
  "Configure a Go buffer with xtdmacs conventions."
  (xtdmacs-code-setup :with-lsp t)
  (font-lock-add-keywords nil xtdmacs-code-go-keywords-alist)
  (when (bound-and-true-p xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "go-mode" xtdmacs-code-go-compile-alist))
  (when xtdmacs-code-go-format-on-save
    (add-hook 'before-save-hook #'lsp-organize-imports nil t)
    (add-hook 'before-save-hook #'lsp-format-buffer    nil t))
  (add-to-list 'compilation-error-regexp-alist 'nakedret)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(nakedret "\\(.+?\\):\\([0-9]+\\).*naked returns on.*" 1 2)))

;;;###autoload
(add-hook 'go-mode-hook #'xtdmacs-code-go-setup)

;;;###autoload (put 'xtdmacs-code-go-compile-alist 'safe-local-variable (lambda (_) t))
;;;###autoload (put 'xtdmacs-code-go-keywords-alist 'safe-local-variable (lambda (_) t))
;;;###autoload (put 'xtdmacs-code-go-format-on-save 'safe-local-variable #'booleanp)

(provide 'xtdmacs-code-go)

;;; xtdmacs-code-go.el ends here
