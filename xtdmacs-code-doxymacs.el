(require 'doxymacs)
(require 'tempo)

(defconst xtdmacs-code-doxymacs-template-doxystyle
  '((let ((next-func (doxymacs-find-next-func)))
      (if next-func
          (list
           'l
           "/**" '> 'n
           " ** @brief" '> 'n
           (xtdmacs-code-doxymacs-param-doxstyle (cdr (assoc 'args next-func)))
           (unless (string-match
                    (regexp-quote (cdr (assoc 'return next-func)))
                    doxymacs-void-types)
             '(l " ** " (doxymacs-doxygen-command-char) "return " (p "Returns: ") > n))
           " ** @details" '> 'n
           " **" '> 'n
           " */" '>)
        (progn
          (error "Can't find next function declaration.")
          nil))))
  "Doc-style template xtdmacs-code-doxymacs-mode documentation.")

(defconst xtdmacs-code-doxymacs-template-phpdoc
  '((let ((next-func (doxymacs-find-next-func)))
      (if next-func
          (list
           'l
           "/**" '> 'n
           " * <first-line-is-brief>" '> 'n
           " *" '> 'n
           " * <rest-is-multiline-details>" '> 'n
           " *" '> 'n
           (xtdmacs-code-doxymacs-param-phpdoc (cdr (assoc 'args next-func)))
           (unless (string-match
                    (regexp-quote (cdr (assoc 'return next-func)))
                    doxymacs-void-types)
             '(l " * " (doxymacs-doxygen-command-char) "return " (p "Returns: ") > n))
           " */" '>)
        (progn
          (error "Can't find next function declaration.")
          nil))))
  "Doc-style template xtdmacs-code-php-mode documentation.")

(defun xtdmacs-code-doxymacs-param-phpdoc (parms)
  "Inserts tempo elements for the given parms in the given style."
  (if parms
      (let ((prompt (concat "Parameter " (car parms) ": ")))
        (list 'l " * " (doxymacs-doxygen-command-char)
              "param " (car parms) " " (list 'p prompt) '> 'n
              (xtdmacs-code-doxymacs-param-phpdoc (cdr parms))))
    nil))

(defun xtdmacs-code-doxymacs-param-doxstyle (parms)
  "Inserts tempo elements for the given parms in the given style."
  (if parms
      (let ((prompt (concat "Parameter " (car parms) ": ")))
        (list 'l " ** " (doxymacs-doxygen-command-char)
              "param " (car parms) " " (list 'p prompt) '> 'n
              (xtdmacs-code-doxymacs-param-doxstyle (cdr parms))))
    nil))


(defun --xtdmacs-code-doxymacs-construct()
  (unless (mode-enabled 'doxymacs-mode)
    (doxymacs-mode t))
  (setq doxymacs-command-character    "@")
  (setq doxymacs-doxygen-style        "JavaDoc")
  (setq doxymacs-member-comment-end   "*/")
  (setq doxymacs-member-comment-start "/**")
  (setq doxymacs-function-comment-template xtdmacs-code-doxymacs-template-doxystyle)
  (define-key doxymacs-mode-map (kbd "C-x d") 'doxymacs-insert-function-comment)
  (define-key doxymacs-mode-map (kbd "C-x m") 'doxymacs-insert-member-comment)
  (message "enabled : xtdmacs-code-doxymacs-mode")
  )

(defun --xtdmacs-code-doxymacs-destroy()
  (when (mode-enabled 'doxymacs-mode)
    (doxymacs-mode nil))
  (message "disabled : xtdmacs-code-doxymacs")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-doxymacs-mode
  "Code for Doxygen" nil "Code" nil
  (if xtdmacs-code-doxymacs-mode
      (--xtdmacs-code-doxymacs-construct)
    (--xtdmacs-code-doxymacs-destroy))
  )

(provide 'xtdmacs-code-doxymacs)
