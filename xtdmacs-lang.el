;;; xtdmacs-lang.el --- Shared utility functions  -*- lexical-binding: t -*-

;;; Commentary:

;; Small grab-bag of helpers used by other xtdmacs files: LSP
;; navigation wrappers, the package install-dir lookup, and a handful
;; of generic predicates and string utilities.

;;; Code:

;;;###autoload
(defun --xtdmacs-lsp-find-references ()
  "Find references to the symbol at point in the current window."
  (interactive)
  (lsp-find-references nil :display-action 'window))

;;;###autoload
(defun --xtdmacs-lsp-find-references-other-window ()
  "Find references to the symbol at point in the other window."
  (interactive)
  (lsp-find-references t :display-action 'window))

;;;###autoload
(defun --xtdmacs-lsp-find-definition-other-window ()
  "Jump to the definition of the symbol at point in the other window."
  (interactive)
  (lsp-find-definition :display-action 'window))

;;;###autoload
(defun xtdmacs-get-install-dir ()
  "Return the on-disk directory where the xtdmacs package is installed."
  (package-desc-dir (or
                     (if (package-desc-p 'xtdmacs) 'xtdmacs)
                     (cadr (assq 'xtdmacs package-alist))
                     (let ((built-in (assq 'xtdmacs package--builtins)))
                       (if built-in
                           (package--from-builtin built-in)
                         (cadr (assq 'xtdmacs package-archive-contents)))))))

;;;###autoload
(defun function-or-string-p (value)
  "Return non-nil if VALUE is either a function or a string."
  (or (functionp value) (stringp value)))

;;;###autoload
(defun funcall-or-value (value)
  "Call VALUE if it is a function, otherwise return it as-is."
  (if (functionp value)
      (funcall value)
    value))

;;;###autoload
(defun split-join (str sep)
  "Prefix every space-separated word in STR with SEP, then re-join with spaces."
  (if (string= str "")
      str
    (mapconcat
     'identity
     (mapcar
      (lambda (el) (concat sep " " el))
      (split-string str " ")) " ")))

;;;###autoload
(defun mode-enabled (symbol)
  "Return non-nil if minor-mode SYMBOL is bound and currently enabled."
  (and (boundp symbol) (symbol-value symbol)))

;;;###autoload
(defun filter (condp lst)
  "Return the elements of LST for which CONDP return non-nil."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;;;###autoload
(defun string/ends-with (string suffix)
  "Return t if STRING end with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

;;;###autoload
(defun string/starts-with (string prefix)
  "Return t if STRING start with PREFIX."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(provide 'xtdmacs-lang)

;;; xtdmacs-lang.el ends here
