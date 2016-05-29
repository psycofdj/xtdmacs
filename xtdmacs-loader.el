;;;###autoload
(defcustom xtdmacs-loader-auto-minor-mode-alist
  '((("Dockerfile") xtdmacs-code-mode dockerfile-mode)
    (("\\.php\\'") php-mode xtdmacs-code-mode xtdmacs-code-doxymacs-mode xtdmacs-code-line-mode)
    (("\\.xml\\.erb\\'" "\\.erb\\'" "Rakefile\\'") ruby-mode xtdmacs-code-mode)
    (("\\.html\\'" "\\.tpl\\'") xtdmacs-code-mode web-mode xtdmacs-code-web-mode)
    (("\\.py\\'") python-mode xtdmacs-code-mode xtdmacs-code-python-mode xtdmacs-code-line-mode)
    (("\\.h\\'" "\\.c\\'" "\\.cc\\'" "\\.hh\\'" "\\.cpp\\'" "\\.hpp\\'" "\\.hxx\\'") xtdmacs-code-mode xtdmacs-code-cpp-mode xtdmacs-code-line-mode xtdmacs-code-doxymacs-mode ac-clang-async-mode)
    (("\\.js\\'") js2-mode xtdmacs-code-js-mode xtdmacs-code-line-mode xtdmacs-code-mode)
    (("\\.json\\'") xtdmacs-code-mode json-mode xtdmacs-code-json-mode linum-mode)
    (("CMakeLists\\.txt\\'") cmake-mode xtdmacs-code-mode)
    (("\\.groovy\\'") groovy-mode xtdmacs-code-mode)
    (("Makefile") xtdmacs-code-mode makefile-mode xtdmacs-code-makefile-mode xtdmacs-code-line-mode)
    (("\\.el\\'") emacs-lisp-mode xtdmacs-code-mode xtdmacs-code-lisp-mode xtdmacs-code-line-mode)
    (("\\.java\\'") java-mode xtdmacs-code-mode xtdmacs-code-java-mode xtdmacs-code-doxymacs-mode xtdmacs-code-line-mode))
  "Alist of filename patterns vs correpsonding minor mode functions,
see `auto-mode-alist'. All elements of this alist are checked,
meaning you can enable multiple minor modes for the same
regexp."
  :group 'loader
  :safe '(lambda(p) t)
  :type '(alist :key-type (repeat string) :value-type (repeat symbol))
  )

(defun xtdmacs-loader-load-minor-modes ()
  (dolist (item xtdmacs-loader-auto-minor-mode-alist)
    (let* ((exts (car item))
           (modes (cdr item)))
      (dolist (ext exts)
        (when (string-match-p ext (buffer-file-name))
          (dolist (mode modes)
            (funcall mode))))))
  )

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

(put 'tags-file-name 'safe-local-variable 'stringp)

(add-hook 'find-file-hook 'xtdmacs-loader-load-minor-modes)

(provide 'xtdmacs-loader)
