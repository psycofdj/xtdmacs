;; -*- lexical-binding: t -*-

;;;###autoload
(defcustom xtdmacs-loader-auto-minor-mode-alist
  '((("Dockerfile") xtdmacs-code-mode)
    (("\\.php\\'") xtdmacs-code-mode  xtdmacs-code-line-mode)
    (("\\.xml\\.erb\\'" "\\.erb\\'" "Rakefile\\'") xtdmacs-code-mode)
    (("\\.html\\'" "\\.tpl\\'") xtdmacs-code-mode xtdmacs-code-web-mode)
    (("\\.py\\'") xtdmacs-code-mode xtdmacs-code-python-mode xtdmacs-code-line-mode)
    (("\\.h\\'" "\\.c\\'" "\\.cc\\'" "\\.hh\\'" "\\.cpp\\'" "\\.hpp\\'" "\\.hxx\\'") xtdmacs-code-mode xtdmacs-code-cpp-mode xtdmacs-code-line-mode  ac-clang-async-mode)
    (("\\.js\\'") xtdmacs-code-js-mode xtdmacs-code-line-mode xtdmacs-code-mode)
    (("\\.json\\'") xtdmacs-code-mode xtdmacs-code-json-mode linum-mode)
    (("CMakeLists\\.txt\\'") xtdmacs-code-mode)
    (("\\.groovy\\'") xtdmacs-code-mode)
    (("Makefile") xtdmacs-code-mode xtdmacs-code-makefile-mode xtdmacs-code-line-mode)
    (("\\.el\\'") xtdmacs-code-mode xtdmacs-code-lisp-mode xtdmacs-code-line-mode)
    (("\\.java\\'") xtdmacs-code-mode xtdmacs-code-java-mode  xtdmacs-code-line-mode))
  "Alist of filename patterns vs correpsonding minor mode functions,
see `auto-mode-alist'. All elements of this alist are checked,
meaning you can enable multiple minor modes for the same
regexp."
  :group 'loader
  :safe '(lambda(p) t)
  :type '(alist
          :key-type   (repeat string)
          :value-type (repeat symbol))
  )

;;;###autoload
(defcustom xtdmacs-loader-auto-major-mode-alist
  '((("Dockerfile") dockerfile-mode)
    (("\\.php\\'")  php-mode)
    (("\\.xml\\.erb\\'" "\\.erb\\'" "Rakefile\\'") ruby-mode)
    (("\\.html\\'" "\\.tpl\\'") web-mode)
    (("\\.py\\'") python-mode)
    (("\\.h\\'" "\\.c\\'" "\\.cc\\'" "\\.hh\\'" "\\.cpp\\'" "\\.hpp\\'" "\\.hxx\\'") c++-mode)
    (("\\.js\\'") js2-mode)
    (("\\.json\\'") json-mode)
    (("CMakeLists\\.txt\\'") cmake-mode)
    (("\\.groovy\\'") groovy-mode)
    (("Makefile") makefile-mode)
    (("\\.el\\'") emacs-lisp-mode)
    (("\\.java\\'") java-mode))
  "Alist of filename patterns vs correpsonding major modes,
see `auto-mode-alist'."
  :group 'loader
  :safe '(lambda(p) t)
  :type '(alist
          :key-type   (repeat string)
          :value-type symbol)
  )

(defun xtdmacs-loader-load-minor-modes ()
  (dolist (item xtdmacs-loader-auto-minor-mode-alist)
    (let* ((exts (car item))
           (modes (cdr item)))
      (dolist (ext exts)
        (when (string-match-p ext (buffer-file-name))
          (dolist (mode modes)
            (unless (mode-enabled mode)
              (funcall mode)
              ))))))
  )


(defun xtdmacs-loader-define-major-modes ()
  (dolist (item xtdmacs-loader-auto-major-mode-alist)
    (let* ((exts  (car item))
           (mode  (car (cdr item))))
      (dolist (ext exts)
        (add-to-list 'auto-mode-alist `(,ext . ,mode))
        )))
  )

(xtdmacs-loader-define-major-modes)
(add-hook 'find-file-hook 'xtdmacs-loader-load-minor-modes)

(provide 'xtdmacs-loader)
