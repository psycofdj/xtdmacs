(defcustom code-cpp-indent-load-auto
  t
  "Enables code auto-indentation on load."
  :group 'code-cpp
  :type 'boolean
  :safe 'booleanp)

(defcustom code-cpp-indent-save-auto
  t
  "Enables code auto-indentation on save."
  :group 'code-cpp
  :type 'boolean
  :safe 'booleanp)

(defcustom code-cpp-header-extensions "cc hh hxx"
  "List of extensions to search when cycling buffers.
   See code-cpp-header-cycle"
  :group 'code-cpp
  :type '(string :tag "Extension (no period)" "")
  :safe 'stringp
  )

(defcustom code-cpp-keywords-alist
  '(("\\<\$?gcs\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"     . 'code-face-global-variable-const-static)
    ("\\<\$?gs\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'code-face-global-variable-static)
    ("\\<\$?gc\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'code-face-global-variable-const)
    ("\\<\$?g\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"       . 'code-face-global-variable)
    ("\\<\$?lcs\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"     . 'code-face-local-variable-const-static)
    ("\\<\$?ls\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'code-face-local-variable-static)
    ("\\<\$?lc\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'code-face-local-variable-const)
    ("\\<\$?l\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"       . 'code-face-local-variable)
    ("\\<\$?t[A-Z][_a-zA-Z0-9]+\\>"                 . 'font-lock-type-face)
    ("\\<\$?mcs\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"     . 'code-face-class-member-const-static)
    ("\\<\$?ms\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'code-face-class-member-static)
    ("\\<\$?mc\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'code-face-class-member-const)
    ("\\<\$?m\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"       . 'code-face-class-member)
    ("\\<\$?_[a-zA-Z0-9][_a-zA-Z0-9]+\\>"                      . 'code-face-class-member-const-static)
    ("\\<\$?_[a-zA-Z0-9][_a-zA-Z0-9]+\\>"                      . 'code-face-class-member-static)
    ("\\<\$?_[a-zA-Z0-9][_a-zA-Z0-9]+\\>"                      . 'code-face-class-member-const)
    ("\\<\$?_[a-zA-Z0-9][_a-zA-Z0-9]+\\>"                      . 'code-face-class-member)
    ("\\<\$?my\\(_\\|[A-Z]\\)?[_a-zA-Z0-9]+\\>"     . 'code-face-class-member)


    ("\\<\$?pc\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'code-face-param-const)
    ("\\<\$?\\(p\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\)\\>" . 'code-face-param)

    ("\\<\$?c\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"       . 'code-face-counter)
    ("\\<\$?cc\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'code-face-counter-const)

    ("\\<\$?BOOST_FOREACH\\>"                       . 'font-lock-keyword-face)
    ("\\<KELOG_RELEASE\\>"                          . 'code-face-log)
    ("\\<LOG_CRIT\\>"                               . 'code-face-log)
    ("\\<LOG_ERR\\>"                                . 'code-face-log)
    ("\\<LOG_WARNING\\>"                            . 'code-face-log)
    ("\\<LOG_NOTICE\\>"                             . 'code-face-log)
    ("\\<LOG_INFO\\>"                               . 'code-face-log)
    ("\\<LOG_DEBUG\\>"                              . 'code-face-log)
    ("\\<WHERE\\>"                                  . 'code-face-log)
    ("\\<HERE\\>"                                   . 'code-face-log)
    ("\\<KEOK\\>"                                   . 'code-face-status-ok)
    ("\\<KEERROR\\>"                                . 'code-face-status-error)
    ("\\<KEWARNING\\>"                              . 'code-face-status-other)
    ("\\<KENOTFOUND\\>"                             . 'code-face-status-other)
    ("\\<KETIMEOUT\\>"                              . 'code-face-status-other))
  "List of additional font-lock rules"
  :group 'code-cpp
  :safe '(lambda(val) t)
  )

(defun --code-cpp-header-next-filename (curfile &optional create)
  (let* ((exts         (split-string code-cpp-header-extensions " "))
         (cur-ext      (file-name-extension      curfile))
         (cur-basename (file-name-sans-extension curfile))
         (result ()))
    (if (member cur-ext exts)
        (while (not (string= (nth 0 exts) cur-ext))
          (setq exts (append (cdr exts) (cons (car exts) ())))))
    (dolist (ext exts)
      (when (and (not (equal ext cur-ext))
                 (or create
                     (file-exists-p (concat cur-basename "." ext))))
        (push (concat cur-basename "." ext) result)))
    (nth 0 result))
  )

(defun code-cpp-header-cycle (&optional create)
  "Cycles buffer between existing .cc, .hh and .hxx files.
Given <bar>.cc, this function will search for <bar>.hh and <bar>.hxx file
If CREATE is non-nil, the function will show buffer even for non-existing files"
  (interactive)
  (let ((cur-file (buffer-file-name)))
    (let ((new-file (--code-cpp-header-next-filename cur-file create)))
      (if new-file
          (switch-to-buffer (find-file-noselect new-file)))))
  )

(defun code-cpp-header-cycle-create ()
  "Cycles buffer between .cc, .hh and .hxx files.
See code-cpp-header-cycle"
  (interactive)
  (code-cpp-header-cycle t)
  )


(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]*"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist '(statement-cont     . align-enum-class-closing-brace))
  )

(defun --code-cpp-save-indent()
  (if code-cpp-indent-save-auto
      (code-format-buffer-with-ident)
    ))

(defun --code-cpp-load-indent()
  (if code-cpp-indent-load-auto
      (code-format-buffer-with-ident)
    ))

(defun --code-cpp-find-tag(otherwin)
  (forward-word)
  (let ((end (point)))
    (backward-word)
    (kill-ring-save (point) end)
    (if otherwin
        (find-tag-other-window (current-kill 0))
      (find-tag (current-kill 0))))
  )

(defun code-cpp-rename-variable()
  (interactive)
  (forward-word)
  (let ((end (point)))
    (backward-word)
    (kill-ring-save (point) end)
    (let* ((word (read-from-minibuffer "label: " (current-kill 0)))
           (prefix (read-from-minibuffer "prefix: " "l"))
           (repl  (read-from-minibuffer "replacement: " (concat prefix (upcase-initials word))))
           (rword (concat "\\<" word "\\>")))
      (query-replace-regexp rword repl)))
  )

(defun --code-cpp-mode-construct()
  (font-lock-add-keywords nil code-cpp-keywords-alist)
  (add-hook 'before-save-hook '--code-cpp-save-indent t t)
  (add-hook 'hack-local-variables-hook '--code-cpp-load-indent t t)
  (fix-enum-class)
  (message "enabled : code-cpp-mode")
  )

(defun --code-cpp-mode-destroy()
  (remove-hook 'before-save-hook '--code-cpp-save-indent t)
  (remove-hook 'hack-local-variables-hook '--code-cpp-load-indent t)
  (remove-hook 'c++-mode-hook 'fix-enum-class t)
  (font-lock-remove-keywords nil code-cpp-keywords-alist)
  (message "disabled : code-cpp-mode")
  )

;;;###autoload
(define-minor-mode code-cpp-mode "Code for C/C++" nil "Code"
  '(([f12]   . code-cpp-header-cycle)
    ([C-f12] . code-cpp-header-cycle-create)
    ([f3]    . (lambda() (interactive) (--code-cpp-find-tag nil)))
    ([C-f3]  . (lambda() (interactive) (--code-cpp-find-tag t))))

  (if code-cpp-mode
      (--code-cpp-mode-construct)
    (--code-cpp-mode-destroy))
  )


;; --------------------------------------------------------------------------

;;;###autoload
(put 'code-cpp-indent-load-auto 'safe-local-variable 'booleanp)
;;;###autoload
(put 'code-cpp-indent-save-auto 'safe-local-variable 'booleanp)
;;;###autoload
(put 'code-cpp-header-extensions 'safe-local-variable 'stringp)

(provide 'code.cpp)
