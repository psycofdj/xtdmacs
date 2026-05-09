;;; xtdmacs-code-shell.el --- Shell-script support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for sh-mode buffers: font-lock keywords, flycheck +
;; auto-complete, and a shellcheck compile recipe.

;;; Code:

(require 'xtdmacs-code)
(require 'auto-complete)

(eval-when-compile (defvar sh-mode-map))

(declare-function xtdmacs-compile++-register-config "xtdmacs-compile++")

(defcustom xtdmacs-code-shell-compile-alist
  '((:compile . ((:file       . buffer-file-name)
                 (:bin        . xtdmacs-code-shell-shellcheck-bin)
                 (:get-params . xtdmacs-compile++-current-file-params)
                 (:command    . xtdmacs-compile++-simple-file-command))))
  "Shell script compilation configuration."
  :group 'xtdmacs-code-shell
  :safe (lambda (_) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function)))))

(defcustom xtdmacs-code-shell-keywords-alist
  '(("\\<l_[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-local-variable)
    ("\\<\\(p_[_a-zA-Z0-9]+\\)\\>" . 'xtdmacs-code-face-param)
    ("\\<c_[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-counter))
  "Additional shell font-lock keywords."
  :group 'xtdmacs-code-shell
  :safe (lambda (_) t)
  :type '(alist :key-type string :value-type sexp))

(defcustom xtdmacs-code-shell-shellcheck-bin-path "/usr/bin/shellcheck"
  "Shellcheck binary path."
  :group 'xtdmacs-code-shell :type 'file :safe #'file-exists-p)

(defun xtdmacs-code-shell-shellcheck-bin ()
  "Return the full shellcheck command line including standard exclusions."
  (concat (or xtdmacs-code-shell-shellcheck-bin-path "shellcheck")
          " -f gcc -e SC2046,SC2086,SC2155 -C=never -x"))

(defun xtdmacs-code-shell-shellcheck-file ()
  "Return the path of the file shellcheck should lint (current buffer)."
  (buffer-file-name))

(with-eval-after-load 'sh-script
  (define-key sh-mode-map (kbd "M-.") #'ac-start))

;;;###autoload
(defun xtdmacs-code-shell-setup ()
  "Configure a Shell-script buffer with xtdmacs conventions."
  (xtdmacs-code-setup)
  (font-lock-add-keywords nil xtdmacs-code-shell-keywords-alist)
  (when (bound-and-true-p xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "sh-mode" xtdmacs-code-shell-compile-alist)
    (make-local-variable 'xtdmacs-compile++-config-alist))
  (auto-complete-mode 1)
  )

;;;###autoload
(add-hook 'sh-mode-hook #'xtdmacs-code-shell-setup)

;;;###autoload (put 'xtdmacs-code-shell-compile-alist 'safe-local-variable (lambda (_) t))
;;;###autoload (put 'xtdmacs-code-shell-keywords-alist 'safe-local-variable (lambda (_) t))
;;;###autoload (put 'xtdmacs-code-shell-shellcheck-bin-path 'safe-local-variable #'file-exists-p)

(provide 'xtdmacs-code-shell)

;;; xtdmacs-code-shell.el ends here

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
