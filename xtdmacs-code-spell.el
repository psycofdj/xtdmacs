;;; xtdmacs-code-spell.el --- Flyspell setup helpers  -*- lexical-binding: t -*-

;;; Commentary:

;; Spell-checking helpers.  Provides two setup functions:
;;   - xtdmacs-code-spell-setup       : flyspell-mode (text)
;;   - xtdmacs-code-spell-prog-setup  : flyspell-prog-mode (code)
;; Both share a single keymap installed buffer-locally.

;;; Code:

(require 'flyspell)

(defcustom xtdmacs-code-spell-max-lines 9999999
  "Maximum number of lines in buffer to permit automatic spellcheck."
  :group 'xtdmacs-code-spell :type 'integer :safe #'integerp)

(defcustom xtdmacs-code-spell-ignore-regexp
  '("^ \\*\\* @subsubsection [^ ]+ " "@image html [^ ]+ ")
  "Patterns to ignore when spellchecking."
  :group 'xtdmacs-code-spell :type '(repeat string) :safe #'listp)

(defun xtdmacs-code-spell-next-word ()
  "Move point to the next misspelled word and offer corrections."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(defun xtdmacs-code-spell-prev-word ()
  "Move point to the previous misspelled word."
  (interactive)
  (flyspell-check-previous-highlighted-word))

(defun xtdmacs-code-spell-change-dictionary ()
  "Change the ispell dictionary, persist it as a file-local variable, and re-check."
  (interactive)
  (call-interactively #'ispell-change-dictionary)
  (add-file-local-variable 'ispell-local-dictionary ispell-local-dictionary)
  (flyspell-buffer))

(defun xtdmacs-code-spell--test-regex (content regex begin end)
  "Return non-nil if REGEX matches CONTENT and the match covers BEGIN..END."
  (let* ((match-begin (string-match regex content))
         (match-end   (match-end 0)))
    (and match-begin match-end
         (<= match-begin begin) (>= match-end end))))

(defun xtdmacs-code-spell--check-patterns (content begin end)
  "Return non-nil if any ignore pattern matches CONTENT covering BEGIN..END."
  (let ((found    nil)
        (patterns xtdmacs-code-spell-ignore-regexp))
    (while (and (not found) (car patterns))
      (setq found (xtdmacs-code-spell--test-regex content (car patterns) begin end))
      (setq patterns (cdr patterns)))
    found))

(defun xtdmacs-code-spell-ignore-patterns (begin end _type)
  "Flyspell predicate: return non-nil to ignore the misspelling at BEGIN..END.
Tested against `xtdmacs-code-spell-ignore-regexp' on the surrounding line."
  (save-excursion
    (goto-char begin)
    (let* ((start   (line-beginning-position))
           (stop    (line-end-position))
           (content (buffer-substring start stop)))
      (xtdmacs-code-spell--check-patterns content (- begin start) (- end start)))))

;; Shared keymap — bound buffer-locally by both setup functions.
(defvar xtdmacs-code-spell-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c")       #'flyspell-buffer)
    (define-key m (kbd "C-c C-<down>")  #'xtdmacs-code-spell-change-dictionary)
    (define-key m (kbd "C-c C-<right>") #'xtdmacs-code-spell-next-word)
    (define-key m (kbd "C-c C-<left>")  #'xtdmacs-code-spell-prev-word)
    m)
  "Keymap installed by `xtdmacs-code-spell-setup' / `-prog-setup'.")

(defun xtdmacs-code-spell--apply (prog-mode-p)
  "Internal: enable flyspell in the current buffer.
PROG-MODE-P non-nil means restrict checking to comments and strings."
  (if prog-mode-p
      (unless (bound-and-true-p flyspell-prog-mode)
        (flyspell-prog-mode))
    (unless (bound-and-true-p flyspell-mode)
      (flyspell-mode 1)))
  (add-hook 'flyspell-incorrect-hook #'xtdmacs-code-spell-ignore-patterns nil t)
  (when (< (count-lines (point-min) (point-max)) xtdmacs-code-spell-max-lines)
    (flyspell-buffer))
  (use-local-map (make-composed-keymap xtdmacs-code-spell-keymap (current-local-map))))

;;;###autoload
(defun xtdmacs-code-spell-prog-setup ()
  "Enable flyspell for programming buffers (comments and strings only)."
  (xtdmacs-code-spell--apply t))

;;;###autoload
(defun xtdmacs-code-spell-setup ()
  "Enable flyspell for text buffers (whole buffer)."
  (xtdmacs-code-spell--apply nil))

(provide 'xtdmacs-code-spell)

;;; xtdmacs-code-spell.el ends here

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
