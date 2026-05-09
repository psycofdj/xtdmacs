;;; xtdmacs-loader.el --- Wire major modes and extra setup hooks  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs-loader: wire file extensions to major modes, and attach extra
;; setup functions to selected major-mode hooks.
;;
;; Per-language setup functions (e.g. `xtdmacs-code-go-setup') are installed
;; on their language's hook by the language file itself, so this loader does
;; NOT need a "minor-mode-alist" anymore.  It only:
;;   - registers file-extension -> major-mode pairs in `auto-mode-alist'
;;   - installs cross-cutting setup functions like `xtdmacs-code-line-setup'
;;     and `xtdmacs-code-spell-prog-setup' on the hooks the user chooses.

;;; Code:

;;;###autoload
(defcustom xtdmacs-loader-auto-major-mode-alist
  '((("Dockerfile")                              dockerfile-mode)
    (("\\.php\\'")                               php-mode)
    (("\\.xml\\.erb\\'" "\\.erb\\'" "Rakefile\\'") ruby-mode)
    (("\\.html\\'" "\\.tpl\\'")                  web-mode)
    (("\\.py\\'")                                python-mode)
    (("\\.h\\'" "\\.c\\'" "\\.cc\\'" "\\.hh\\'" "\\.cpp\\'" "\\.hpp\\'" "\\.hxx\\'") c++-mode)
    (("\\.js\\'")                                js2-mode)
    (("\\.json\\'")                              json-mode)
    (("CMakeLists\\.txt\\'")                     cmake-mode)
    (("\\.groovy\\'")                            groovy-mode)
    (("Makefile")                                makefile-mode)
    (("\\.el\\'")                                emacs-lisp-mode)
    (("\\.java\\'")                              java-mode))
  "Alist of filename patterns to corresponding major modes.
See `auto-mode-alist'."
  :group 'xtdmacs-loader
  :safe (lambda (_) t)
  :type '(alist :key-type   (repeat string)
                :value-type symbol))

;;;###autoload
(defcustom xtdmacs-loader-extra-setup-alist
  '((python-mode-hook        xtdmacs-code-spell-prog-setup xtdmacs-code-line-setup)
    (c-mode-hook             xtdmacs-code-spell-prog-setup xtdmacs-code-line-setup)
    (c++-mode-hook           xtdmacs-code-spell-prog-setup xtdmacs-code-line-setup)
    (js2-mode-hook           xtdmacs-code-spell-prog-setup xtdmacs-code-line-setup)
    (js-mode-hook            xtdmacs-code-spell-prog-setup xtdmacs-code-line-setup)
    (json-mode-hook          xtdmacs-code-spell-prog-setup)
    (sh-mode-hook            xtdmacs-code-spell-prog-setup)
    (makefile-mode-hook      xtdmacs-code-spell-prog-setup xtdmacs-code-line-setup)
    (emacs-lisp-mode-hook    xtdmacs-code-spell-prog-setup xtdmacs-code-line-setup)
    (java-mode-hook          xtdmacs-code-spell-prog-setup xtdmacs-code-line-setup)
    (markdown-mode-hook      xtdmacs-code-spell-setup)
    (rst-mode-hook           xtdmacs-code-spell-setup     xtdmacs-code-line-setup)
    (go-mode-hook            xtdmacs-code-spell-prog-setup xtdmacs-code-line-setup)
    (typescript-mode-hook    xtdmacs-code-spell-prog-setup xtdmacs-code-line-setup)
    (terraform-mode-hook     xtdmacs-code-spell-prog-setup xtdmacs-code-line-setup)
    (yaml-mode-hook          xtdmacs-code-line-setup)
    (php-mode-hook           xtdmacs-code-spell-prog-setup xtdmacs-code-line-setup))
  "Alist mapping major-mode hooks to extra xtdmacs setup functions.
Each entry is (HOOK-SYMBOL FN1 FN2 ...).  The loader installs each FN
on HOOK-SYMBOL when first loaded.  Customize to change which buffer
types get the line-mode mode-line, spellcheck, etc."
  :group 'xtdmacs-loader
  :safe (lambda (_) t)
  :type '(alist :key-type   symbol
                :value-type (repeat function)))

(defun xtdmacs-loader--register-major-modes ()
  "Add `xtdmacs-loader-auto-major-mode-alist' entries to `auto-mode-alist'."
  (dolist (entry xtdmacs-loader-auto-major-mode-alist)
    (let ((patterns (car entry))
          (mode     (cadr entry)))
      (dolist (pattern patterns)
        (add-to-list 'auto-mode-alist (cons pattern mode))))))

(defun xtdmacs-loader--install-extra-hooks ()
  "Install `xtdmacs-loader-extra-setup-alist' entries via `add-hook'."
  (dolist (entry xtdmacs-loader-extra-setup-alist)
    (let ((hook (car entry))
          (fns  (cdr entry)))
      (dolist (fn fns)
        (add-hook hook fn)))))

(xtdmacs-loader--register-major-modes)
(xtdmacs-loader--install-extra-hooks)

(provide 'xtdmacs-loader)

;;; xtdmacs-loader.el ends here
