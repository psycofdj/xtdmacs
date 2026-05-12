;;; xtdmacs-code-typescript.el --- TypeScript support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for typescript-mode buffers: LSP, yas, dap-mode
;; key bindings, font-lock keywords, and an npm-based compile recipe.

;;; Code:

(require 'xtdmacs-code)

(declare-function xtdmacs-compile++-register-config "xtdmacs-compile++")
(declare-function lsp-ui-doc-glance                 "lsp-ui-doc")
(declare-function lsp-ui-imenu                      "lsp-ui-imenu")
(declare-function dap-debug                         "dap-mode")
(declare-function dap-step-in                       "dap-mode")
(declare-function dap-step-out                      "dap-mode")
(declare-function dap-next                          "dap-mode")
(declare-function dap-continue                      "dap-mode")
(declare-function dap-debug-restart                 "dap-mode")
(declare-function dap-breakpoint-toggle             "dap-mode")

(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . yas-minor-mode))

(defface xtdmacs-code-typescript-face-indent-error
  '((t (:foreground "color-124" :underline t)))
  "Indicates spaces instead of tabs for indentation."
  :group 'xtdmacs-code-typescript)

(defcustom xtdmacs-code-typescript-compile-alist
  '((:compile . ((:dir        . xtdmacs-compile++-get-dir-git)
                 (:bin        . "npm run tsc:abs")
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))
    (:test    . ((:dir        . xtdmacs-compile++-get-dir-git)
                 (:bin        . "npm run test")
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command)))
    (:deploy  . ((:dir        . xtdmacs-compile++-get-dir-git)
                 (:bin        . "npm run lint:fix")
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command))))
  "TypeScript compilation configuration."
  :group 'xtdmacs-code-typescript
  :safe (lambda (_) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function)))))

(defcustom xtdmacs-code-typescript-keywords-alist
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
    ("self"                            . 'font-lock-keyword-face))
  "Additional TypeScript font-lock keywords."
  :group 'xtdmacs-code-typescript
  :safe (lambda (_) t)
  :type '(alist :key-type string :value-type sexp))

(defcustom xtdmacs-code-typescript-format-on-save nil
  "When non-nil, run lsp-organize-imports + lsp-format-buffer on save."
  :group 'xtdmacs-code-typescript :type 'boolean :safe #'booleanp)

(with-eval-after-load 'typescript-mode
  (let ((m typescript-mode-map))
    (define-key m (kbd "<f12>")     #'lsp-find-definition)
    (define-key m (kbd "C-<f12>")   #'--xtdmacs-lsp-find-definition-other-window)
    (define-key m (kbd "<f11>")     #'--xtdmacs-lsp-find-references)
    (define-key m (kbd "C-<f11>")   #'--xtdmacs-lsp-find-references-other-window)
    (define-key m (kbd "<f10>")     #'lsp-ui-doc-glance)
    (define-key m (kbd "C-<f10>")   #'lsp-ui-imenu)
    (define-key m (kbd "M-t")       #'lsp-format-region)
    (define-key m (kbd "C-M-t")     #'lsp-format-buffer)
    (define-key m (kbd "M-r")       #'lsp-rename)
    (define-key m (kbd "M-.")       #'company-complete)
    (define-key m (kbd "C-e <f12>") #'dap-debug)
    (define-key m (kbd "C-e s")     #'dap-step-in)
    (define-key m (kbd "C-e o")     #'dap-step-out)
    (define-key m (kbd "C-e n")     #'dap-next)
    (define-key m (kbd "C-e c")     #'dap-continue)
    (define-key m (kbd "C-e r")     #'dap-debug-restart)
    (define-key m (kbd "C-e b")     #'dap-breakpoint-toggle)))

;;;###autoload
(defun xtdmacs-code-typescript-setup ()
  "Configure a TypeScript buffer with xtdmacs conventions."
  (xtdmacs-code-setup :with-lsp t)
  (font-lock-add-keywords nil xtdmacs-code-typescript-keywords-alist)
  (when (bound-and-true-p xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "typescript-mode" xtdmacs-code-typescript-compile-alist))
  (when xtdmacs-code-typescript-format-on-save
    (add-hook 'before-save-hook #'lsp-organize-imports nil t)
    (add-hook 'before-save-hook #'lsp-format-buffer    nil t)))

;;;###autoload
(add-hook 'typescript-mode-hook #'xtdmacs-code-typescript-setup)

;;;###autoload (put 'xtdmacs-code-typescript-compile-alist 'safe-local-variable (lambda (_) t))
;;;###autoload (put 'xtdmacs-code-typescript-keywords-alist 'safe-local-variable (lambda (_) t))
;;;###autoload (put 'xtdmacs-code-typescript-format-on-save 'safe-local-variable #'booleanp)

(provide 'xtdmacs-code-typescript)

;;; xtdmacs-code-typescript.el ends here

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
