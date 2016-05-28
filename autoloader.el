;;; autoloader.el --- automatically extracted autoloads
;;
;;; Code:

;;;***

;;;### (autoloads (code-mode) "code" "code.el" (22341 34129 469314
;;;;;;  9000))
;;; Generated autoloads from code.el

(autoload 'code-mode "code" "\
Bunch of configuration development

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (code-cpp-mode) "code.cpp" "code.cpp.el" (22341
;;;;;;  34219 621051 345000))
;;; Generated autoloads from code.cpp.el

(autoload 'code-cpp-mode "code.cpp" "\
Code for C/C++

\(fn &optional ARG)" t nil)

(put 'code-cpp-indent-load-auto 'safe-local-variable 'booleanp)

(put 'code-cpp-indent-save-auto 'safe-local-variable 'booleanp)

(put 'code-cpp-header-extensions 'safe-local-variable 'stringp)

;;;***

;;;### (autoloads (code-doxymacs-mode) "code.doxymacs" "code.doxymacs.el"
;;;;;;  (22341 34209 476630 909000))
;;; Generated autoloads from code.doxymacs.el

(autoload 'code-doxymacs-mode "code.doxymacs" "\
Code for Doxygen

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (code-java-mode) "code.java" "code.java.el" (22341
;;;;;;  34300 76398 612000))
;;; Generated autoloads from code.java.el

(autoload 'code-java-mode "code.java" "\
Code for Java

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (code-js-mode) "code.js" "code.js.el" (22341 34282
;;;;;;  771676 18000))
;;; Generated autoloads from code.js.el

(autoload 'code-js-mode "code.js" "\
Code for Javascript

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (code-json-mode) "code.json" "code.json.el" (22341
;;;;;;  34276 495413 902000))
;;; Generated autoloads from code.json.el

(autoload 'code-json-mode "code.json" "\
Code for json

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (code-line-mode) "code.line" "code.line.el" (22341
;;;;;;  34249 170275 873000))
;;; Generated autoloads from code.line.el

(autoload 'code-line-mode "code.line" "\
Specialized modeline

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (code-lisp-mode) "code.lisp" "code.lisp.el" (22341
;;;;;;  34288 919932 766000))
;;; Generated autoloads from code.lisp.el

(autoload 'code-lisp-mode "code.lisp" "\
Code for Lisp

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (code-makefile-mode) "code.makefile" "code.makefile.el"
;;;;;;  (22341 34233 941644 825000))
;;; Generated autoloads from code.makefile.el

(autoload 'code-makefile-mode "code.makefile" "\
Code for Makefiles

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (code-php-mode) "code.php" "code.php.el" (22341
;;;;;;  34259 278694 831000))
;;; Generated autoloads from code.php.el

(autoload 'code-php-mode "code.php" "\
Code for PHP

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (code-python-mode) "code.python" "code.python.el"
;;;;;;  (22341 34193 435966 50000))
;;; Generated autoloads from code.python.el

(autoload 'code-python-mode "code.python" "\
Code for Python

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (code-spell-mode) "code.spell" "code.spell.el"
;;;;;;  (22341 34307 536710 86000))
;;; Generated autoloads from code.spell.el

(autoload 'code-spell-mode "code.spell" "\
Spell comments and strings

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (code-align-args code-align-vars code-format-buffer-with-ident
;;;;;;  code-format-buffer-without-ident code-indent-buffer code-untabify-buffer)
;;;;;;  "code.utils" "code.utils.el" (22341 34357 806808 403000))
;;; Generated autoloads from code.utils.el

(autoload 'code-untabify-buffer "code.utils" "\


\(fn)" t nil)

(autoload 'code-indent-buffer "code.utils" "\


\(fn)" t nil)

(autoload 'code-format-buffer-without-ident "code.utils" "\


\(fn)" t nil)

(autoload 'code-format-buffer-with-ident "code.utils" "\


\(fn)" t nil)

(autoload 'code-align-vars "code.utils" "\


\(fn)" t nil)

(autoload 'code-align-args "code.utils" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (code-web-mode) "code.web" "code.web.el" (22341
;;;;;;  34268 371074 597000))
;;; Generated autoloads from code.web.el

(autoload 'code-web-mode "code.web" "\
Code for web

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (compile++-mode) "compile++" "compile++.el" (22341
;;;;;;  34229 545462 640000))
;;; Generated autoloads from compile++.el

(autoload 'compile++-mode "compile++" "\
Set of function beyond compilation-mode

\(fn &optional ARG)" t nil)

(put 'compile++-buffer-height 'safe-local-variable 'integerp)

(put 'compile++-scroll-output 'safe-local-variable 'booleanp)

(put 'compile++-buffer-local 'safe-local-variable 'booleanp)

(put 'compile++-config-alist 'safe-local-variable '(lambda (p) t))

;;;***

;;;### (autoloads (docker-mode) "docker" "docker.el" (22341 34180
;;;;;;  703438 246000))
;;; Generated autoloads from docker.el

(autoload 'docker-mode "docker" "\
Docker syntax mode

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (groovy-mode groovy-mode-hook) "groovy-mode" "groovy-mode.el"
;;;;;;  (22035 52040 520025 558000))
;;; Generated autoloads from groovy-mode.el
(add-to-list 'auto-mode-alist '("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . groovy-mode))

(defvar groovy-mode-hook nil "\
*Hook called by `groovy-mode'.")

(custom-autoload 'groovy-mode-hook "groovy-mode" t)

(autoload 'groovy-mode "groovy-mode" "\
Major mode for editing Groovy code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `groovy-mode-hook'.

Key bindings:
\\{groovy-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (ac-clang-async-mode) "libs/auto-complete-clang-async"
;;;;;;  "libs/auto-complete-clang-async.el" (22341 34331 421707 202000))
;;; Generated autoloads from libs/auto-complete-clang-async.el

(autoload 'ac-clang-async-mode "libs/auto-complete-clang-async" "\
Async completion using clang

\(fn &optional ARG)" t nil)

(put 'ac-clang-complete-executable 'safe-local-variable 'stringp)

(put 'ac-clang-lang-option-function 'safe-local-variable 'functionp)

(put 'ac-clang-cflags 'safe-local-variable 'listp)

(put 'ac-clang-compile-directory-name 'safe-local-variable 'stringp)

(put 'ac-clang-compile-commands 'safe-local-variable 'functionp)

(put 'ac-clang-prefix-header 'safe-local-variable 'stringp)

(put 'ac-clang-async-do-autocompletion-automatically 'safe-local-variable 'booleanp)

(put 'ac-clang-error-buffer-name 'safe-local-variable 'stringp)

;;;***

;;;### (autoloads nil nil ("bindings.el" "config.el" "lang.el" "loader.el")
;;;;;;  (22341 34904 15615 454000))

;;;***

(provide 'autoloader)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autoload.el ends here
