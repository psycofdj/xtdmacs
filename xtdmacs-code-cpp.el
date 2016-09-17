(require 'cc-align)

(defvar xtdmacs-code-cpp-last-rename-prefix nil)

(defcustom xtdmacs-code-cpp-indent-load-auto
  t
  "Enables code auto-indentation on load."
  :group 'xtdmacs-code-cpp
  :type 'boolean
  :safe 'booleanp)

(defcustom xtdmacs-code-cpp-indent-save-auto
  t
  "Enables code auto-indentation on save."
  :group 'xtdmacs-code-cpp
  :type 'boolean
  :safe 'booleanp)

(defcustom xtdmacs-code-cpp-header-extensions "cc hh hxx"
  "List of extensions to search when cycling buffers.
   See xtdmacs-code-cpp-header-cycle"
  :group 'xtdmacs-code-cpp
  :type '(string :tag "Extension (no period)" "")
  :safe 'stringp
  )

(defcustom xtdmacs-code-cpp-keywords-alist
  '(("\\<\$?gcs\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"     . 'xtdmacs-code-face-global-variable-const-static)
    ("\\<\$?gs\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-global-variable-static)
    ("\\<\$?gc\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-global-variable-const)
    ("\\<\$?g\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-global-variable)
    ("\\<\$?lcs\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"     . 'xtdmacs-code-face-local-variable-const-static)
    ("\\<\$?ls\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-local-variable-static)
    ("\\<\$?lc\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-local-variable-const)
    ("\\<\$?l\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-local-variable)
    ("\\<\$?t[A-Z][_a-zA-Z0-9]+\\>"                 . 'font-lock-type-face)
    ("\\<\$?mcs\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"     . 'xtdmacs-code-face-class-member-const-static)
    ("\\<\$?ms\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-class-member-static)
    ("\\<\$?mc\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-class-member-const)
    ("\\<\$?m\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-class-member)
    ("\\<\$?_[a-zA-Z0-9][_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-class-member-const-static)
    ("\\<\$?_[a-zA-Z0-9][_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-class-member-static)
    ("\\<\$?_[a-zA-Z0-9][_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-class-member-const)
    ("\\<\$?_[a-zA-Z0-9][_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-class-member)
    ("\\<\$?my\\(_\\|[A-Z]\\)?[_a-zA-Z0-9]+\\>"     . 'xtdmacs-code-face-class-member)
    ("\\<\$?pc\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-param-const)
    ("\\<\$?\\(p\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\)\\>" . 'xtdmacs-code-face-param)
    ("\\<\$?c\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-counter)
    ("\\<\$?cc\\(_\\|[A-Z]\\)[_a-zA-Z0-9]+\\>"      . 'xtdmacs-code-face-counter-const)

    ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
    ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
    ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
    ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
    ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
    ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
    ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
    ("\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)  ; actual string
    ("\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter

    ("\\<\$?BOOST_FOREACH\\>"                       . 'font-lock-keyword-face)
    ("\\<KELOG_RELEASE\\>"                          . 'xtdmacs-code-face-log)
    ("\\<LOG_CRIT\\>"                               . 'xtdmacs-code-face-log)
    ("\\<LOG_ERR\\>"                                . 'xtdmacs-code-face-log)
    ("\\<LOG_WARNING\\>"                            . 'xtdmacs-code-face-log)
    ("\\<LOG_NOTICE\\>"                             . 'xtdmacs-code-face-log)
    ("\\<LOG_INFO\\>"                               . 'xtdmacs-code-face-log)
    ("\\<LOG_DEBUG\\>"                              . 'xtdmacs-code-face-log)
    ("\\<WHERE\\>"                                  . 'xtdmacs-code-face-log)
    ("\\<HERE\\>"                                   . 'xtdmacs-code-face-log)
    ("\\<KEOK\\>"                                   . 'xtdmacs-code-face-status-ok)
    ("\\<KEERROR\\>"                                . 'xtdmacs-code-face-status-error)
    ("\\<KEWARNING\\>"                              . 'xtdmacs-code-face-status-other)
    ("\\<KENOTFOUND\\>"                             . 'xtdmacs-code-face-status-other)
    ("\\<KETIMEOUT\\>"                              . 'xtdmacs-code-face-status-other))
  "List of additional font-lock rules"
  :group 'xtdmacs-code-cpp
  :safe '(lambda(val) t)
  )

(defun --xtdmacs-code-cpp-header-next-filename (curfile &optional create)
  (let* ((exts         (split-string xtdmacs-code-cpp-header-extensions " "))
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

(defun xtdmacs-code-cpp-header-cycle (&optional create)
  "Cycles buffer between existing .cc, .hh and .hxx files.
Given <bar>.cc, this function will search for <bar>.hh and <bar>.hxx file
If CREATE is non-nil, the function will show buffer even for non-existing files"
  (interactive)
  (let ((cur-file (buffer-file-name)))
    (let ((new-file (--xtdmacs-code-cpp-header-next-filename cur-file create)))
      (if new-file
          (switch-to-buffer (find-file-noselect new-file)))))
  )

(defun xtdmacs-code-cpp-header-cycle-create ()
  "Cycles buffer between .cc, .hh and .hxx files.
See xtdmacs-code-cpp-header-cycle"
  (interactive)
  (xtdmacs-code-cpp-header-cycle t)
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

(defun --xtdmacs-code-cpp-save-indent()
  (if xtdmacs-code-cpp-indent-save-auto
      (xtdmacs-code-format-buffer-with-ident)
    ))

(defun --xtdmacs-code-cpp-load-indent()
  (if xtdmacs-code-cpp-indent-load-auto
      (xtdmacs-code-format-buffer-with-ident)
    ))

(defun --xtdmacs-code-cpp-find-tag(otherwin)
  (forward-word)
  (let ((end (point)))
    (backward-word)
    (kill-ring-save (point) end)
    (if otherwin
        (find-tag-other-window (current-kill 0))
      (find-tag (current-kill 0))))
  )

(defun xtdmacs-code-cpp-rename-variable()
  (interactive)
  (forward-word)
  (let ((end (point)))
    (backward-word)
    (kill-ring-save (point) end)

    (let* ((word (read-from-minibuffer "label: " (current-kill 0)))
           (default-prefix (if xtdmacs-code-cpp-last-rename-prefix
                               xtdmacs-code-cpp-last-rename-prefix
                             "l"))
           (prefix (read-from-minibuffer "prefix: " default-prefix))
           (repl  (read-from-minibuffer "replacement: " (concat prefix (upcase-initials word))))
           (rword (concat "\\<" word "\\>")))
      (query-replace-regexp rword repl)
      (setq xtdmacs-code-cpp-last-rename-prefix prefix)))
  )

(defun --xtdmacs-code-cpp-mode-construct()
  (font-lock-add-keywords nil xtdmacs-code-cpp-keywords-alist)
  (add-hook 'before-save-hook '--xtdmacs-code-cpp-save-indent t t)
  (add-hook 'hack-local-variables-hook '--xtdmacs-code-cpp-load-indent t t)
  (fix-enum-class)
  (define-key global-map (kbd "C-e")  'xtdmacs-code-cpp-rename-variable)
  (define-key global-map (kbd "M-e")  'xtdmacs-code-cpp-rename-variable)
  (message "enabled : xtdmacs-code-cpp-mode")
  )

(defun --xtdmacs-code-cpp-mode-destroy()
  (remove-hook 'before-save-hook '--xtdmacs-code-cpp-save-indent t)
  (remove-hook 'hack-local-variables-hook '--xtdmacs-code-cpp-load-indent t)
  (remove-hook 'c++-mode-hook 'fix-enum-class t)
  (font-lock-remove-keywords nil xtdmacs-code-cpp-keywords-alist)
  (message "disabled : xtdmacs-code-cpp-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-cpp-mode "Code for C/C++" nil "Code"
  '(([f12]   . xtdmacs-code-cpp-header-cycle)
    ([C-f12] . xtdmacs-code-cpp-header-cycle-create)
    ([f3]    . (lambda() (interactive) (--xtdmacs-code-cpp-find-tag nil)))
    ([C-f3]  . (lambda() (interactive) (--xtdmacs-code-cpp-find-tag t))))

  (if xtdmacs-code-cpp-mode
      (--xtdmacs-code-cpp-mode-construct)
    (--xtdmacs-code-cpp-mode-destroy))
  )


;; --------------------------------------------------------------------------

;;;###autoload
(put 'xtdmacs-code-cpp-indent-load-auto 'safe-local-variable 'booleanp)
;;;###autoload
(put 'xtdmacs-code-cpp-indent-save-auto 'safe-local-variable 'booleanp)
;;;###autoload
(put 'xtdmacs-code-cpp-header-extensions 'safe-local-variable 'stringp)


(provide 'xtdmacs-code-cpp)
