;; -*- lexical-binding: t -*-

(require 'xtdmacs-compile++)
(require 'auto-complete)

(defcustom xtdmacs-code-shell-compile-alist
  '((:compile . ((:file       . buffer-file-name)
                 (:bin        . xtdmacs-code-shell-shellcheck-bin)
                 (:get-params . xtdmacs-compile++-current-file-params)
                 (:command    . xtdmacs-compile++-simple-file-command)
                 )))
  "Xtdmacs-code-shell compilation configuration"
  :group 'xtdmacs-code-shell
  :safe '(lambda(p) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function))))
  )

(defcustom xtdmacs-code-shell-keywords-alist
  '(("\\<l_[_a-zA-Z0-9]+\\>" .       'xtdmacs-code-face-local-variable)
    ("\\<\\(p_[_a-zA-Z0-9]+\\)\\>" . 'xtdmacs-code-face-param)
    ("\\<c_[_a-zA-Z0-9]+\\>" .       'xtdmacs-code-face-counter))
  "List of additional shell font-lock keywords"
  :group 'xtdmacs-code-shell
  :safe '(lambda(p) t))

(defcustom xtdmacs-code-shell-shellcheck-bin-path "/usr/bin/shellcheck"
  "shellcheck code checker file path"
  :group 'xtdmacs-code-shell
  :type 'file
  :safe 'file-exists-p)

;; --------------------------------------------------------------------------- ;

(defun xtdmacs-code-shell-shellcheck-bin()
  (concat
   (if xtdmacs-code-shell-shellcheck-bin-path
       xtdmacs-code-shell-shellcheck-bin-path
     "spellcheck")
   " -f gcc -e SC2046,SC2086,SC2155 -C=never -x")
  )

(defun xtdmacs-code-shell-shellcheck-file()
  (buffer-file-name)
  )

;; --------------------------------------------------------------------------- ;

(defun --xtdmacs-code-shell-construct()
  (font-lock-add-keywords nil xtdmacs-code-shell-keywords-alist)
  (when (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "sh-mode" xtdmacs-code-shell-compile-alist)
    (make-local-variable 'xtdmacs-compile++-config-alist)
    )
  (unless (mode-enabled 'auto-complete-mode)
    (auto-complete-mode t))
  (message "enabled : xtdmacs-code-shell-mode")
  )

(defun --xtdmacs-code-shell-destroy()
  (font-lock-remove-keywords nil xtdmacs-code-shell-keywords-alist)
  (when (mode-enabled 'auto-complete-mode)
    (auto-complete-mode nil))
  (message "disabled : xtdmacs-code-shell-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-shell-mode
  "Code for Shell" nil "Code"
  '(("\M-."    . ac-start))
  (if xtdmacs-code-shell-mode
      (--xtdmacs-code-shell-construct)
    (--xtdmacs-code-shell-destroy))
  )

;; --------------------------------------------------------------------------- ;

;;;###autoload
(put 'xtdmacs-code-shell-compile-alist 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-shell-keywords-alist 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-shell-shellcheck-bin-path 'safe-local-variable 'file-exists-p)

(provide 'xtdmacs-code-shell)

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
