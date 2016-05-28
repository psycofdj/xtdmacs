(defcustom loader-auto-minor-mode-alist
  nil
  "Alist of filename patterns vs correpsonding minor mode functions,
see `auto-mode-alist'. All elements of this alist are checked,
meaning you can enable multiple minor modes for the same
regexp."
  :group 'loader
  :safe '(lambda(p) t)
  :type '(alist :key-type (repeat string) :value-type (repeat symbol))
  )

(defun loader-load-minor-modes ()
  (dolist (item loader-auto-minor-mode-alist)
    (let* ((exts (car item))
           (modes (cdr item)))
      (dolist (ext exts)
        (when (string-match-p ext (buffer-file-name))
          (dolist (mode modes)
            (funcall mode))))))
  )

(autoload 'auto-complete                    "auto-complete"        "" t)
(autoload 'auto-complete-config             "auto-complete-config" "" t)
;; (autoload 'ac-clang-async-mode              "auto-complete-clang-async" "" t)

(autoload 'yafolding-mode                   "yafolding" "" t)
(autoload 'highlight-80+-mode               "highlight-80+" "" t)
(autoload 'linum-mode                       "linum"         "" t)
(autoload 'js2-mode                         "js2-mode"      "" t)
(autoload 'web-mode                         "web-mode"      "" t)
(autoload 'php-mode                         "pi-php-mode"   "" t)
(autoload 'json-mode                        "json-mode"     "" t)
(autoload 'groovy-mode                      "groovy-mode"   "" t)
(autoload 'cmake-mode                       "cmake-mode"    "" t)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

(put 'tags-file-name 'safe-local-variable 'stringp)


;; (autoload 'code-untabify-buffer             "code.utils"    "" t)
;; (autoload 'code-indent-buffer               "code.utils"    "" t)
;; (autoload 'code-format-buffer-without-ident "code.utils"    "" t)
;; (autoload 'code-format-buffer-with-ident    "code.utils"    "" t)
;; (autoload 'code-align-vars                  "code.utils"    "" t)
;; (autoload 'code-align-args                  "code.utils"    "" t)
;; (autoload 'code-shell-toggle                "code.utils"    "" t)
;; (autoload 'code-spell-mode                  "code.spell"    "" t)
;; (autoload 'code-java-mode                   "code.java"     "" t)
;; (autoload 'code-lisp-mode                   "code.lisp"     "" t)
;; (autoload 'code-js-mode                     "code.js"       "" t)
;; (autoload 'code-json-mode                   "code.json"     "" t)
;; (autoload 'code-web-mode                    "code.web"      "" t)
;; (autoload 'code-php-mode                    "code.php"      "" t)
;; (autoload 'code-line-mode                   "code.line"     "" t)
;; (autoload 'code-makefile-mode               "code.makefile" "" t)
;; (autoload 'compile++-mode                   "compile++"     "" t)
;; (autoload 'code-cpp-mode                    "code.cpp"      "" t)
;; (autoload 'code-doxymacs-mode               "code.doxymacs" "" t)
;; (autoload 'code-python-mode                 "code.python"   "" t)
;; (autoload 'docker-mode                      "docker"        "" t)
;; (autoload 'code-mode                        "code"          "" t)

(add-hook 'find-file-hook 'loader-load-minor-modes)

(provide 'loader)
