(require 'cc-align)
(require 'irony-completion)
(require 'auto-complete)

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

(defcustom xtdmacs-code-cpp-compile-alist
  xtdmacs-compile++-default-config-alist
  "Xtdmacs-Code-Cpp compilation configuration"
  :group 'xtdmacs-code-cpp
  :safe '(lambda(p) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function))))
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


(defface xtdmacs-code-cpp-ac-irony-working-face
  '((t (:background "green")))
  "Overriding face of buffer when completion is working"
  :group 'xtdmacs-code-cpp
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

(defvar ac-source-irony
  '((cache)
    (requires   . -1)
    (limit      . nil)
    (prefix     . xtdmacs-code-cpp-ac-irony-prefix)
    (action     . xtdmacs-code-cpp-ac-irony-yas-expand)
    (candidates . xtdmacs-code-cpp-ac-irony-candidates)))

(defun xtdmacs-code-cpp-complete-irony (candidates)
  (interactive)
  (face-remap-add-relative 'mode-line-buffer-id 'nil)
  (auto-complete '(ac-source-irony)))

(defun xtdmacs-code-cpp-complete-irony-async ()
  (interactive)
  (face-remap-add-relative 'mode-line-buffer-id 'xtdmacs-code-cpp-ac-irony-working-face)
  (irony-completion-candidates-async 'xtdmacs-code-cpp-complete-irony))

(defun xtdmacs-code-cpp-ac-irony-prefix ()
  (irony-completion-beginning-of-symbol)
  )

(defun xtdmacs-code-cpp-ac-irony-yas-expand ()
  (let* ((item    (cdr ac-last-completion))
         (value   (popup-item-property item 'current))
         (str     (irony-completion-post-comp-str value))
         (placeholders (irony-completion-post-comp-placeholders value)))
    (if (and placeholders (irony-snippet-available-p))
        (irony-snippet-expand
         (irony-completion--post-complete-yas-snippet str placeholders))))
  )

(defun xtdmacs-code-cpp-ac-irony--make-candidate (candidate)
  (popup-item-propertize
   (car candidate)
   'summary (nth 2 candidate)
   'document (concat
              (irony--awhen (nth 2 candidate) ;result-type?
                            (concat it " "))
              (concat (nth 4 candidate) "\n") ;prototype
              (irony--awhen (nth 3 candidate) ;brief doc
                            (concat "\n" it "\n")))
   'current candidate
   )
  )

(defun xtdmacs-code-cpp-ac-irony-candidates ()
  (mapcar #'xtdmacs-code-cpp-ac-irony--make-candidate (irony-completion-candidates)))

(defun --xtdmacs-code-cpp-mode-construct()
  (font-lock-add-keywords nil xtdmacs-code-cpp-keywords-alist)
  (add-hook 'before-save-hook '--xtdmacs-code-cpp-save-indent t t)
  (add-hook 'hack-local-variables-hook '--xtdmacs-code-cpp-load-indent t t)
  (fix-enum-class)

  (when (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "c++-mode" xtdmacs-code-cpp-compile-alist))

  (unless (mode-enabled 'auto-complete-mode)
    (yas-minor-mode t)
    (auto-complete-mode t)
    (irony-mode t)
    (irony-cdb-autosetup-compile-options)
    (add-to-list 'ac-sources 'ac-source-irony)
    (ac-linum-workaround)
    (ac-flyspell-workaround)
    (setq popup-use-optimized-column-computation nil)
    )
  (message "enabled : xtdmacs-code-cpp-mode")
  )

(defun --xtdmacs-code-cpp-mode-destroy()
  (when (mode-enabled 'auto-complete-mode)
    (irony-mode nil)
    (auto-complete-mode nil)
    (yas-minor-mode nil)
    )
  (remove-hook 'before-save-hook '--xtdmacs-code-cpp-save-indent t)
  (remove-hook 'hack-local-variables-hook '--xtdmacs-code-cpp-load-indent t)
  (remove-hook 'c++-mode-hook 'fix-enum-class t)
  (font-lock-remove-keywords nil xtdmacs-code-cpp-keywords-alist)
  (message "disabled : xtdmacs-code-cpp-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-cpp-mode "Code for C/C++" nil "Code"
  '(([f12]      . xtdmacs-code-cpp-header-cycle)
    ([C-f12]    . xtdmacs-code-cpp-header-cycle-create)
    ("\C-e"     . irony-get-type)
    ("\M-."     . xtdmacs-code-cpp-complete-irony-async)
    ("\C-c\C-e" . xtdmacs-code-cpp-rename-variable)
    )
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
