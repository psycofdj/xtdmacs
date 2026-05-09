;;; xtdmacs-code-cpp.el --- C/C++ support  -*- lexical-binding: t -*-

;;; Commentary:

;; xtdmacs setup for c-mode and c++-mode buffers: Hungarian-style
;; font-lock keywords, header file cycling, irony+auto-complete
;; integration, an "enum class" indentation fix, and an interactive
;; variable rename helper.

;;; Code:

(require 'xtdmacs-code)
(require 'cc-align)

(eval-when-compile
  (require 'popup nil 'noerror)
  (require 'auto-complete-config nil 'noerror)
  (defvar c++-mode-map))

(declare-function xtdmacs-compile++-register-config "xtdmacs-compile++")
(declare-function ac-flyspell-workaround "auto-complete")
(declare-function ac-linum-workaround    "auto-complete")
(declare-function popup-item-property    "popup")
(declare-function popup-item-propertize  "popup")
(declare-function irony-completion-candidates              "irony-completion")
(declare-function irony-completion-candidates-async        "irony-completion")
(declare-function irony-completion-beginning-of-symbol     "irony-completion")
(declare-function irony-completion-post-comp-str           "irony-completion")
(declare-function irony-completion-post-comp-placeholders  "irony-completion")
(declare-function irony-completion--post-complete-yas-snippet "irony-completion")
(declare-function irony-snippet-available-p "irony-snippet")
(declare-function irony-snippet-expand      "irony-snippet")

(use-package irony
  :ensure t
  :commands irony-mode)

(use-package auto-complete
  :ensure t
  :commands auto-complete-mode)

(defvar xtdmacs-code-cpp-last-rename-prefix nil)

(defcustom xtdmacs-code-cpp-indent-load-auto t
  "Enables code auto-indentation on load."
  :group 'xtdmacs-code-cpp :type 'boolean :safe #'booleanp)

(defcustom xtdmacs-code-cpp-indent-save-auto t
  "Enables code auto-indentation on save."
  :group 'xtdmacs-code-cpp :type 'boolean :safe #'booleanp)

(defcustom xtdmacs-code-cpp-header-extensions "cc hh hxx"
  "Extensions cycled by `xtdmacs-code-cpp-header-cycle'."
  :group 'xtdmacs-code-cpp
  :type '(string :tag "Extensions (no period)" "")
  :safe #'stringp)

(defcustom xtdmacs-code-cpp-compile-alist
  (and (boundp 'xtdmacs-compile++-default-config-alist)
       xtdmacs-compile++-default-config-alist)
  "C/C++ compilation configuration."
  :group 'xtdmacs-code-cpp
  :safe (lambda (_) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function)))))

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
    ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t)
    ("\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)
    ("\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t)

    ("\\<\$?BOOST_FOREACH\\>" . 'font-lock-keyword-face)
    ("\\<KELOG_RELEASE\\>"    . 'xtdmacs-code-face-log)
    ("\\<LOG_CRIT\\>"         . 'xtdmacs-code-face-log)
    ("\\<LOG_ERR\\>"          . 'xtdmacs-code-face-log)
    ("\\<LOG_WARNING\\>"      . 'xtdmacs-code-face-log)
    ("\\<LOG_NOTICE\\>"       . 'xtdmacs-code-face-log)
    ("\\<LOG_INFO\\>"         . 'xtdmacs-code-face-log)
    ("\\<LOG_DEBUG\\>"        . 'xtdmacs-code-face-log)
    ("\\<WHERE\\>"            . 'xtdmacs-code-face-log)
    ("\\<HERE\\>"             . 'xtdmacs-code-face-log)
    ("\\<KEOK\\>"             . 'xtdmacs-code-face-status-ok)
    ("\\<KEERROR\\>"          . 'xtdmacs-code-face-status-error)
    ("\\<KEWARNING\\>"        . 'xtdmacs-code-face-status-other)
    ("\\<KENOTFOUND\\>"       . 'xtdmacs-code-face-status-other)
    ("\\<KETIMEOUT\\>"        . 'xtdmacs-code-face-status-other))
  "Additional C/C++ font-lock keywords."
  :group 'xtdmacs-code-cpp
  :safe (lambda (_) t)
  :type '(alist :key-type string :value-type sexp))

(defface xtdmacs-code-cpp-ac-irony-working-face
  '((t (:background "green")))
  "Mode-line buffer-id face while completion is working."
  :group 'xtdmacs-code-cpp)

;; ---------------------------------------------------------------------------
;; Header file cycling.

(defun xtdmacs-code-cpp--header-next-filename (curfile &optional create)
  "Return the next header file to cycle to from CURFILE.
With CREATE non-nil, return a name even if the file does not exist yet."
  (let* ((exts         (split-string xtdmacs-code-cpp-header-extensions " "))
         (cur-ext      (file-name-extension curfile))
         (cur-basename (file-name-sans-extension curfile))
         (result ()))
    (when (member cur-ext exts)
      (while (not (string= (car exts) cur-ext))
        (setq exts (append (cdr exts) (list (car exts))))))
    (dolist (ext exts)
      (when (and (not (equal ext cur-ext))
                 (or create (file-exists-p (concat cur-basename "." ext))))
        (push (concat cur-basename "." ext) result)))
    (car result)))

(defun xtdmacs-code-cpp-header-cycle (&optional create)
  "Cycle between .cc, .hh and .hxx files for the current buffer.
With CREATE non-nil, switch even to non-existing files."
  (interactive)
  (let* ((cur-file (buffer-file-name))
         (new-file (xtdmacs-code-cpp--header-next-filename cur-file create)))
    (when new-file
      (switch-to-buffer (find-file-noselect new-file)))))

(defun xtdmacs-code-cpp-header-cycle-create ()
  "Cycle between .cc, .hh and .hxx files, creating if needed."
  (interactive)
  (xtdmacs-code-cpp-header-cycle t))

;; ---------------------------------------------------------------------------
;; "enum class" indentation fix.

(defun xtdmacs-code-cpp--inside-enum-class-p (pos)
  "Return non-nil if POS is inside the body of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]*" nil))))

(defun xtdmacs-code-cpp--align-enum-class (langelem)
  "Indent body of an `enum class' flush with the brace; defer otherwise.
LANGELEM is the cc-mode syntactic element."
  (if (xtdmacs-code-cpp--inside-enum-class-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun xtdmacs-code-cpp--align-enum-class-closing-brace (langelem)
  "Indent the closing brace of an `enum class' (no extra offset).
LANGELEM is the cc-mode syntactic element."
  (if (xtdmacs-code-cpp--inside-enum-class-p (c-langelem-pos langelem)) '- '+))

(defun xtdmacs-code-cpp--fix-enum-class ()
  "Patch `c-offsets-alist' to indent C++ \"enum class\" sensibly."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . xtdmacs-code-cpp--align-enum-class))
  (add-to-list 'c-offsets-alist '(statement-cont     . xtdmacs-code-cpp--align-enum-class-closing-brace)))

;; ---------------------------------------------------------------------------
;; Variable rename.

(defun xtdmacs-code-cpp-rename-variable ()
  "Rename a variable interactively, applying a Hungarian-style prefix."
  (interactive)
  (forward-word)
  (let ((end (point)))
    (backward-word)
    (kill-ring-save (point) end)
    (let* ((word (read-from-minibuffer "label: " (current-kill 0)))
           (default-prefix (or xtdmacs-code-cpp-last-rename-prefix "l"))
           (prefix (read-from-minibuffer "prefix: " default-prefix))
           (repl   (read-from-minibuffer "replacement: " (concat prefix (upcase-initials word))))
           (rword  (concat "\\<" word "\\>")))
      (query-replace-regexp rword repl)
      (setq xtdmacs-code-cpp-last-rename-prefix prefix))))

;; ---------------------------------------------------------------------------
;; auto-complete + irony source.

(defvar ac-source-irony
  '((cache)
    (requires   . -1)
    (limit      . nil)
    (prefix     . xtdmacs-code-cpp-ac-irony-prefix)
    (action     . xtdmacs-code-cpp-ac-irony-yas-expand)
    (candidates . xtdmacs-code-cpp-ac-irony-candidates))
  "Auto-complete source backed by irony.")

(defun xtdmacs-code-cpp-complete-irony (_candidates)
  "Run auto-complete using the irony source.
Argument _CANDIDATES is unused; auto-complete fetches them itself."
  (interactive)
  (face-remap-add-relative 'mode-line-buffer-id nil)
  (auto-complete '(ac-source-irony)))

(defun xtdmacs-code-cpp-complete-irony-async ()
  "Trigger asynchronous irony completion at point, then run auto-complete."
  (interactive)
  (face-remap-add-relative 'mode-line-buffer-id 'xtdmacs-code-cpp-ac-irony-working-face)
  (irony-completion-candidates-async 'xtdmacs-code-cpp-complete-irony))

(defun xtdmacs-code-cpp-ac-irony-prefix ()
  "Return the position where the symbol being completed start."
  (irony-completion-beginning-of-symbol))

(defun xtdmacs-code-cpp-ac-irony-yas-expand ()
  "After auto-complete picks an irony candidate, expand its yasnippet placeholders."
  (let* ((item         (cdr ac-last-completion))
         (value        (popup-item-property item 'current))
         (str          (irony-completion-post-comp-str value))
         (placeholders (irony-completion-post-comp-placeholders value)))
    (when (and placeholders (irony-snippet-available-p))
      (irony-snippet-expand
       (irony-completion--post-complete-yas-snippet str placeholders)))))

(defun xtdmacs-code-cpp-ac-irony--make-candidate (candidate)
  "Convert an irony CANDIDATE into a popup item suitable for auto-complete."
  (popup-item-propertize
   (car candidate)
   'summary  (nth 2 candidate)
   'document (concat
              (irony--awhen (nth 2 candidate) (concat it " "))
              (concat (nth 4 candidate) "\n")
              (irony--awhen (nth 3 candidate) (concat "\n" it "\n")))
   'current  candidate))

(defun xtdmacs-code-cpp-ac-irony-candidates ()
  "Fetch irony's completion candidates and convert them to auto-complete popups."
  (mapcar #'xtdmacs-code-cpp-ac-irony--make-candidate (irony-completion-candidates)))

;; ---------------------------------------------------------------------------
;; Save/load formatting.

(defun xtdmacs-code-cpp--save-indent ()
  "Reformat the current buffer if `xtdmacs-code-cpp-indent-save-auto' is non-nil."
  (when xtdmacs-code-cpp-indent-save-auto
    (xtdmacs-code-format-buffer-with-ident)))

(defun xtdmacs-code-cpp--load-indent ()
  "Reformat the current buffer if `xtdmacs-code-cpp-indent-load-auto' is non-nil."
  (when xtdmacs-code-cpp-indent-load-auto
    (xtdmacs-code-format-buffer-with-ident)))

(with-eval-after-load 'cc-mode
  (let ((m c++-mode-map))
    (define-key m [f12]              #'xtdmacs-code-cpp-header-cycle)
    (define-key m [C-f12]            #'xtdmacs-code-cpp-header-cycle-create)
    (define-key m (kbd "C-e")        #'irony-get-type)
    (define-key m (kbd "M-.")        #'xtdmacs-code-cpp-complete-irony-async)
    (define-key m (kbd "C-c C-e")    #'xtdmacs-code-cpp-rename-variable)))

;;;###autoload
(defun xtdmacs-code-cpp-setup ()
  "Configure a C/C++ buffer with xtdmacs conventions."
  (xtdmacs-code-setup)
  (font-lock-add-keywords nil xtdmacs-code-cpp-keywords-alist)
  (add-hook 'before-save-hook         #'xtdmacs-code-cpp--save-indent nil t)
  (add-hook 'hack-local-variables-hook #'xtdmacs-code-cpp--load-indent nil t)
  (xtdmacs-code-cpp--fix-enum-class)

  (when (bound-and-true-p xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "c++-mode" xtdmacs-code-cpp-compile-alist))

  (yas-minor-mode 1)
  (auto-complete-mode 1)
  (irony-mode 1)
  (irony-cdb-autosetup-compile-options)
  (add-to-list 'ac-sources 'ac-source-irony)
  (when (or (bound-and-true-p flyspell-mode)
            (bound-and-true-p flyspell-prog-mode))
    (ac-flyspell-workaround))
  (setq popup-use-optimized-column-computation nil))

;;;###autoload
(add-hook 'c-mode-hook   #'xtdmacs-code-cpp-setup)
;;;###autoload
(add-hook 'c++-mode-hook #'xtdmacs-code-cpp-setup)

;;;###autoload (put 'xtdmacs-code-cpp-indent-load-auto 'safe-local-variable #'booleanp)
;;;###autoload (put 'xtdmacs-code-cpp-indent-save-auto 'safe-local-variable #'booleanp)
;;;###autoload (put 'xtdmacs-code-cpp-header-extensions 'safe-local-variable #'stringp)

(provide 'xtdmacs-code-cpp)

;;; xtdmacs-code-cpp.el ends here
