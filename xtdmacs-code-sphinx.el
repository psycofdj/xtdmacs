;;; xtdmacs-code-sphinx.el --- rst/Sphinx support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for rst-mode buffers: disables electric indent and
;; registers a Sphinx-aware compile recipe.

;;; Code:

(require 'xtdmacs-code)

(declare-function xtdmacs-compile++-register-config "xtdmacs-compile++")
(declare-function --xtdmacs-compile++-get-value     "xtdmacs-compile++")

(defcustom xtdmacs-code-sphinx-compile-alist
  '((:compile . ((:dir        . xtdmacs-code-sphinx-project-root)
                 (:bin        . xtdmacs-code-sphinx-bin)
                 (:env        . "")
                 (:get-params . xtdmacs-compile++-default-params)
                 (:command    . xtdmacs-compile++-default-command))))
  "Sphinx compilation configuration."
  :group 'xtdmacs-code-sphinx
  :safe (lambda (_) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function)))))

(defun xtdmacs-code-sphinx-project-root ()
  "Return the buffer's directory if it contain a `conf.py', else nil."
  (let* ((buffer-dir (file-name-directory (buffer-file-name)))
         (conf       (concat buffer-dir "/conf.py")))
    (when (file-exists-p conf) buffer-dir)))

(defun xtdmacs-code-sphinx-bin ()
  "Return the Sphinx build command (make-driven if available, else sphinx-build)."
  (let* ((dir           (--xtdmacs-compile++-get-value nil :compile :dir))
         (makefile-path (concat (funcall-or-value dir) "/Makefile")))
    (if (file-exists-p makefile-path)
        "make html"
      "sphinx-build -M html . build")))

;;;###autoload
(defun xtdmacs-code-sphinx-setup ()
  "Configure an rst/Sphinx buffer with xtdmacs conventions."
  (xtdmacs-code-setup)
  (electric-indent-mode -1)
  (when (bound-and-true-p xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "rst-mode" xtdmacs-code-sphinx-compile-alist)))

;;;###autoload
(add-hook 'rst-mode-hook #'xtdmacs-code-sphinx-setup)

(provide 'xtdmacs-code-sphinx)

;;; xtdmacs-code-sphinx.el ends here

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
