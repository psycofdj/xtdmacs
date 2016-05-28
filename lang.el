(defun function-or-string-p (value)
  (or (functionp value) (stringp value))
  )

(defun funcall-or-value (value)
  (if (functionp value)
      (funcall value)
    value)
  )

(defun split-join(str sep)
  (if (string= str "")
      str
    (mapconcat
     'identity
     (mapcar
      (lambda (el) (concat sep " " el))
      (split-string str " ")) " "))
  )

(defun mode-enabled(symbol)
  (and (boundp symbol) (symbol-value symbol))
  )


(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun string/ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(provide 'lang)

;; --------------------------------------------------------------
