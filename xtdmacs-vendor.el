;;;###autoload
(autoload 'ac-clang-async-mode "vendor/auto-complete-clang-async.el")

;;;###autoload
(put 'ac-clang-complete-executable 'safe-local-variable 'stringp)
;;;###autoload
(put 'ac-clang-lang-option-function 'safe-local-variable 'functionp)
;;;###autoload
(put 'ac-clang-cflags 'safe-local-variable 'listp)
;;;###autoload
(put 'ac-clang-compile-directory-name 'safe-local-variable 'stringp)
;;;###autoload
(put 'ac-clang-compile-commands 'safe-local-variable 'functionp)
;;;###autoload
(put 'ac-clang-prefix-header 'safe-local-variable 'stringp)
;;;###autoload
(put 'ac-clang-async-do-autocompletion-automatically 'safe-local-variable 'booleanp)
;;;###autoload
(put 'ac-clang-error-buffer-name 'safe-local-variable 'stringp)





(provide 'xtdmacs-vendor)
