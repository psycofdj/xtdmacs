;; -*- lexical-binding: t -*-

(defcustom xtdmacs-code-sphinx-compile-alist
  '((:compile . ((:dir        . xtdmacs-code-sphinx-project-root)
                 (:bin        . xtdmacs-code-sphinx-bin)
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command))))
  "Xtdmacs-Code-sphinx  compilation configuration"
  :group 'xtdmacs-code-sphinx
  :safe '(lambda(p) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function))))
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
  (let* ((dir           (--xtdmacs-compile++-get-value nil :compile :dir))
         (makefile-path (concat (funcall-or-value dir) "/Makefile")))
    (if (file-exists-p makefile-path)
        "make html"
      "sphinx-build -M html . build"))
  )

;; --------------------------------------------------------------------------- ;

(defun --xtdmacs-code-sphinx-construct()
  (electric-indent-mode nil)
  (when (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "rst-mode" xtdmacs-code-sphinx-compile-alist))
  (message "enabled : xtdmacs-code-sphinx-mode")
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
