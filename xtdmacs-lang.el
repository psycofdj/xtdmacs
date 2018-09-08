;; -*- lexical-binding: t -*-

;;;###autoload
(defun xtdmacs-get-install-dir()
  (package-desc-dir (or
                     (if (package-desc-p 'xtdmacs) 'xtdmacs)
                     (cadr (assq 'xtdmacs package-alist))
                     (let ((built-in (assq 'xtdmacs package--builtins)))
                       (if built-in
                           (package--from-builtin built-in)
                         (cadr (assq 'xtdmacs package-archive-contents))))))
  )


;;;###autoload
(defun function-or-string-p (value)
  (or (functionp value) (stringp value))
  )

;;;###autoload
(defun funcall-or-value (value)
  (if (functionp value)
      (funcall value)
    value)
  )

;;;###autoload
(defun split-join(str sep)
  (if (string= str "")
      str
    (mapconcat
     'identity
     (mapcar
      (lambda (el) (concat sep " " el))
      (split-string str " ")) " "))
  )

;;;###autoload
(defun mode-enabled(symbol)
  (and (boundp symbol) (symbol-value symbol))
  )

;;;###autoload
(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;;;###autoload
(defun string/ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

;;;###autoload
(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(provide 'xtdmacs-lang)

;; --------------------------------------------------------------
