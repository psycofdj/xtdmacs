;;; xtdmacs-code.el --- Shared faces, helpers and base buffer setup  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs-code: shared faces, helpers, and base buffer setup for code editing.
;;
;; Public API (called from per-language files):
;;   - xtdmacs-code-setup      : enable base features in the current buffer
;;   - xtdmacs-code-format-buffer-with-ident
;;   - xtdmacs-code-format-buffer-without-ident
;;   - xtdmacs-code-align-vars / -align-args / -align-regexp
;;
;; Faces (xtdmacs-code-face-*) are used by per-language font-lock keywords.

;;; Code:

(require 'align)
(require 'xtdmacs-lang)

(declare-function xtdmacs-compile++-mode "xtdmacs-compile++")
(declare-function kustomize-in-kustomize-file "kustomize")
(declare-function kustomize-in-dir-strcture   "kustomize")
(declare-function kustomize-open-at-point     "kustomize")
(declare-function kustomize-open-overlay      "kustomize")
(declare-function kustomize-patch-at-point    "kustomize")
(declare-function kustomize-which-func        "kustomize")

;; ---------------------------------------------------------------------------
;; Shared external packages.  Per-language files used to redeclare these.

(use-package yafolding
  :commands yafolding-mode
  :bind (:map yafolding-mode-map
              ("M-f"   . yafolding-toggle-element)
              ("C-M-f" . yafolding-toggle-all)))

(use-package column-enforce-mode
  :commands column-enforce-mode)

(use-package display-line-numbers
  :commands display-line-numbers-mode)

(use-package lsp-mode
  :hook (prog-mode . lsp-deferred)
  :commands (lsp))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.3
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil
        lsp-ui-peek-enable t))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package company
  :hook (prog-mode . company-mode))

(use-package yasnippet
  :commands yas-minor-mode)

(with-eval-after-load 'prog-mode
  (let ((m prog-mode-map))
    (define-key m (kbd "<f12>")   #'lsp-find-definition)
    (define-key m (kbd "C-<f12>") #'--xtdmacs-lsp-find-definition-other-window)
    (define-key m (kbd "<f11>")   #'--xtdmacs-lsp-find-references)
    (define-key m (kbd "C-<f11>") #'--xtdmacs-lsp-find-references-other-window)
    (define-key m (kbd "M-r")     #'lsp-rename)
    (define-key m (kbd "M-.")     #'company-complete)
    (define-key m (kbd "<f10>")   #'lsp-ui-doc-glance)
    (define-key m (kbd "C-<f10>") #'lsp-ui-imenu)
    (define-key m (kbd "M-t")     #'lsp-format-region)
    (define-key m (kbd "C-M-t")   #'lsp-format-buffer)
    ))

;; ---------------------------------------------------------------------------
;; Faces.

(defgroup xtdmacs-code nil
  "Base face and helper definitions for xtdmacs language modes."
  :group 'tools)
(defface xtdmacs-code-face-status-error
  '((t (:foreground "#870000")))
  "Face for error status codes." :group 'xtdmacs-code)
(defface xtdmacs-code-face-status-ok
  '((t (:foreground "#af5fd7")))
  "Face for ok status codes." :group 'xtdmacs-code)
(defface xtdmacs-code-face-status-other
  '((t (:foreground "#af5fd7")))
  "Face for other status codes (timeout, notfound...)." :group 'xtdmacs-code)
(defface xtdmacs-code-face-log
  '((t (:foreground "#5f5f00")))
  "Face for logging macros/calls." :group 'xtdmacs-code)
(defface xtdmacs-code-face-global-variable
  '((t (:foreground "#ff00ff"))) "Global variable." :group 'xtdmacs-code)
(defface xtdmacs-code-face-global-variable-const
  '((t (:foreground "#ff00ff" :weight bold))) "Const global variable." :group 'xtdmacs-code)
(defface xtdmacs-code-face-global-variable-static
  '((t (:foreground "#ff00ff" :underline t))) "Static global variable." :group 'xtdmacs-code)
(defface xtdmacs-code-face-global-variable-const-static
  '((t (:foreground "#ff00ff" :underline t :weight bold))) "Const static global variable." :group 'xtdmacs-code)
(defface xtdmacs-code-face-local-variable
  '((t (:foreground "#7f7f7f"))) "Local variable." :group 'xtdmacs-code)
(defface xtdmacs-code-face-local-variable-const
  '((t (:foreground "#4e4e4e" :weight bold))) "Const local variable." :group 'xtdmacs-code)
(defface xtdmacs-code-face-local-variable-static
  '((t (:foreground "#7f7f7f" :underline t))) "Static local variable." :group 'xtdmacs-code)
(defface xtdmacs-code-face-local-variable-const-static
  '((t (:foreground "#7f7f7f" :underline t :weight bold))) "Const static local variable." :group 'xtdmacs-code)
(defface xtdmacs-code-face-param
  '((t (:foreground "#008787"))) "Parameter." :group 'xtdmacs-code)
(defface xtdmacs-code-face-param-const
  '((t (:foreground "#008787" :weight bold))) "Const parameter." :group 'xtdmacs-code)
(defface xtdmacs-code-face-macro
  '((t (:foreground "#008787" :underline t))) "Macro." :group 'xtdmacs-code)
(defface xtdmacs-code-face-class-member
  '((t (:foreground "#87afff"))) "Member variable." :group 'xtdmacs-code)
(defface xtdmacs-code-face-class-member-const
  '((t (:foreground "#87afff" :weight bold))) "Const member variable." :group 'xtdmacs-code)
(defface xtdmacs-code-face-class-member-static
  '((t (:foreground "#87afff" :underline t))) "Static member variable." :group 'xtdmacs-code)
(defface xtdmacs-code-face-class-member-const-static
  '((t (:foreground "#87afff" :underline t :weight bold))) "Static const member variable." :group 'xtdmacs-code)
(defface xtdmacs-code-face-counter
  '((t (:foreground "#875f00"))) "Iterator/counter." :group 'xtdmacs-code)
(defface xtdmacs-code-face-counter-const
  '((t (:foreground "#875f00" :weight bold))) "Const iterator/counter." :group 'xtdmacs-code)
(defface xtdmacs-code-face-return
  '((t (:foreground "#d70087"))) "Return value." :group 'xtdmacs-code)

;; ---------------------------------------------------------------------------
;; Customs.

(defcustom xtdmacs-code-indent-max-lines 2000
  "Maximum number of lines in buffer to permit auto-indentation."
  :group 'xtdmacs-code :type 'integer)

;; ---------------------------------------------------------------------------
;; Buffer formatting helpers.  Used by save hooks in per-language files.

;;;###autoload
(defun xtdmacs-code-untabify-buffer ()
  "Replace tabs by spaces in the whole buffer."
  (interactive)
  (untabify (point-min) (point-max)))

;;;###autoload
(defun xtdmacs-code-indent-buffer ()
  "Indent the whole buffer if it is not too large."
  (interactive)
  (when (< (count-lines (point-min) (point-max)) xtdmacs-code-indent-max-lines)
    (indent-region (point-min) (point-max))))

;;;###autoload
(defun xtdmacs-code-format-buffer (&optional indent untab)
  "Strip trailing whitespace; optionally INDENT and UNTAB."
  (delete-trailing-whitespace)
  (when indent (xtdmacs-code-indent-buffer))
  (when untab  (xtdmacs-code-untabify-buffer)))

;;;###autoload
(defun xtdmacs-code-format-buffer-without-ident ()
  "Strip trailing whitespace and untabify the current buffer."
  (interactive)
  (xtdmacs-code-format-buffer nil t))

;;;###autoload
(defun xtdmacs-code-format-buffer-with-ident ()
  "Strip trailing whitespace, indent, and untabify the current buffer."
  (interactive)
  (xtdmacs-code-format-buffer t t))

;; ---------------------------------------------------------------------------
;; Alignment helpers.

;;;###autoload
(defun xtdmacs-code-align-regexp (beg end regexp &optional group spacing repeat)
  "Align text in region BEG..END by REGEXP, leaving tab settings unchanged.
GROUP is the parenthesis group used for alignment (default 1, negative
to justify).  SPACING is the amount of spacing (default
`align-default-spacing', negative for absolute column).  When REPEAT is
non-nil, repeat the alignment for every match on each line."
  (interactive
   (append
    (list (region-beginning) (region-end))
    (if current-prefix-arg
        (list (read-string "Complex align using regexp: " "\\(\\s-*\\)")
              (string-to-number (read-string "Parenthesis group to modify (justify if negative): " "1"))
              (string-to-number (read-string "Amount of spacing (or column if negative): "
                                             (number-to-string align-default-spacing)))
              (y-or-n-p "Repeat throughout line? "))
      (list (concat "\\(\\s-*\\)" (read-string "Align regexp: "))
            1 align-default-spacing nil))))
  (or group   (setq group 1))
  (or spacing (setq spacing align-default-spacing))
  (let ((tab-mode indent-tabs-mode)
        (rule (list (list nil (cons 'regexp regexp)
                          (cons 'group (abs group))
                          (if (< group 0) (cons 'justify t) (cons 'bogus nil))
                          (if (>= spacing 0) (cons 'spacing spacing) (cons 'column (abs spacing)))
                          (cons 'repeat repeat)))))
    (setq indent-tabs-mode nil)
    (align-region beg end 'entire rule nil nil)
    (setq indent-tabs-mode tab-mode)))

;;;###autoload
(defun xtdmacs-code-align-vars ()
  "Align variable declarations in region."
  (interactive)
  (let ((case-fold-search nil))
    (xtdmacs-code-align-regexp (region-beginning) (region-end)
                               "\\(\\s-*\\)\\b\\(\\($\\)?[lpmcg][cs]?[cs]?\\([A-Z]\\|_\\).*\\)\\b" 1 1 0)
    (xtdmacs-code-align-regexp (region-beginning) (region-end)
                               "\\(\\s-*\\):?=" 1 1 0)))

;;;###autoload
(defun xtdmacs-code-align-args ()
  "Align argument lists in region."
  (interactive)
  (xtdmacs-code-align-regexp (region-beginning) (region-end) ",\\(\\s-*\\)"      1 1 t)
  (xtdmacs-code-align-regexp (region-beginning) (region-end) "\\(\\s-*\\)<<"     1 1 t)
  (xtdmacs-code-align-regexp (region-beginning) (region-end) "\\(\\s-*\\)=>"     1 1 t))

;; ---------------------------------------------------------------------------
;; Keys that apply to all code buffers — installed once on prog-mode-map.

(with-eval-after-load 'prog-mode
  (let ((m prog-mode-map))
    (define-key m [C-M-up]   #'backward-sexp)
    (define-key m [C-M-down] #'forward-sexp)
    (define-key m [f4]       #'indent-region)
    (define-key m [C-f4]     #'xtdmacs-code-indent-buffer)
    (define-key m [C-f1]     #'xtdmacs-code-align-vars)
    (define-key m [C-f2]     #'xtdmacs-code-align-args)
    (define-key m (kbd "M-d") #'xtdmacs-code-align-regexp)))

;; ---------------------------------------------------------------------------
;; Per-buffer base setup.  Per-language setup functions call this first.

;;;###autoload
(defun xtdmacs-code-setup ()
  "Enable base xtdmacs editing features in the current buffer.
Called from major-mode hooks (see `xtdmacs-loader-base-mode-hooks')
and from per-language setup functions."
  (display-line-numbers-mode 1)
  (yafolding-mode 1)
  (column-enforce-mode 1)
  (lsp 1)
  (lsp-ui-mode 1)
  (company-mode 1)
  (flycheck-mode 1)
  (yas-minor-mode 1)
  (when (require 'xtdmacs-compile++ nil 'noerror)
    (xtdmacs-compile++-mode 1))
  (when (require 'kustomize nil 'noerror)
    (when (kustomize-in-kustomize-file)
      (local-set-key [f12]           #'kustomize-open-at-point)
      (local-set-key (kbd "C-<f12>") (lambda () (interactive) (kustomize-open-at-point t)))
      (local-set-key (kbd "C-e")     #'kustomize-patch-at-point)
      (kustomize-which-func))
    (when (kustomize-in-dir-strcture)
      (local-set-key (kbd "C-x C-<up>") #'kustomize-open-overlay)))
  (highlight-regexp " +$" 'trailing-whitespace))

(provide 'xtdmacs-code)

;;; xtdmacs-code.el ends here
