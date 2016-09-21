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

(defface xtdmacs-code-doxymacs-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for doxygen keywords command such as @brief, @section..."
  :group 'code
  )

(defface xtdmacs-code-doxymacs-warning-face
  '((t (:inherit xtdmacs-code-doxymacs-keyword-face)))
  "Face for doxygen @warning keyword."
  :group 'code
  )

(defface xtdmacs-code-doxymacs-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for doxygen variables of commands like @param, @tparam, @def..."
  :group 'code
  )

(defface xtdmacs-code-doxymacs-parameter-face
  '((t (:inherit font-lock-string-face)))
  "Face for doxygen parameters of commands such as @section, @ref..."
  :group 'code
  )

(defface xtdmacs-code-doxymacs-type-face
  '((t (:inherit font-lock-type-face)))
  "Face for doxygen argument applying to types, @class, @interface, @union..."
  :group 'code
  )

(defface xtdmacs-code-doxymacs-string-face
  '((t (:inherit font-lock-string-face)))
  "Face for doxygen  keyword."
  :group 'code
  )

(defface xtdmacs-code-doxymacs-bold-face
  '((t (:weight bold)))
  "Face for doxygen keyword."
  :group 'code
  )

(defface xtdmacs-code-doxymacs-underline-face
  '((t (:underline t)))
  "Face for doxygen keyword."
  :group 'code
  )

(defface xtdmacs-code-doxymacs-italic-face
  '((t (:slant italic)))
  "Face for doxygen keyword."
  :group 'code
  )

(defface xtdmacs-code-doxymacs-h1-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for doxygen keyword."
  :group 'code
  )
(defface xtdmacs-code-doxymacs-h2-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for doxygen keyword."
  :group 'code
  )
(defface xtdmacs-code-doxymacs-h3-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for doxygen keyword."
  :group 'code
  )
(defface xtdmacs-code-doxymacs-h4-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for doxygen keyword."
  :group 'code
  )


(defcustom xtdmacs-code-doxymacs-keywords-alist
  '(("\\([@\\\\]\\(brief\\|li\\|\\(end\\)?code\\|sa\\|note\\|\\(end\\)?verbatim\\|return\\|arg\\|fn\\|hideinitializer\\|showinitializer\\|\\$\\|internal\\|nosubgrouping\\|author\\|date\\|endif\\|invariant\\|post\\|pre\\|remarks\\|since\\|test\\|version\\|\\(end\\)?htmlonly\\|\\(end\\)?latexonly\\|f\\$\\|file\\|\\(end\\)?xmlonly\\|\\(end\\)?manonly\\|property\\|mainpage\\|name\\|overload\\|typedef\\|deprecated\\|par\\|addindex\\|line\\|skip\\|skipline\\|until\\|see\\|endlink\\|callgraph\\|endcond\\|else\\)\\)\\>"
     (0 'xtdmacs-code-doxymacs-keyword-face prepend))
    ("\\([@\\\\]\\(attention\\|warning\\|todo\\|bug\\)\\)\\>"
     (0 'xtdmacs-code-doxymacs-warning-face prepend))
    ("\\([@\\\\]\\(param\\(?:\\s-*\\[\\(?:in\\|out\\|in,out\\)\\]\\)?\\|a\\|tparam\\|namespace\\|relates\\(also\\)?\\|var\\|def\\)\\)\\s-+\\(\\sw+\\)"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (4 'xtdmacs-code-doxymacs-variable-face prepend))
    ("\\([@\\\\]\\(class\\|struct\\|union\\|exception\\|enum\\|throw\\|interface\\|protocol\\)\\)\\s-+\\(\\(\\sw\\|:\\)+\\)"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (3 'xtdmacs-code-doxymacs-type-face prepend))
    ("\\([@\\\\]retval\\)\\s-+\\([^  \n]+\\)"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (2 font-lock-function-name-face prepend))
    ("\\([@\\\\]b\\)\\s-+\\([^   \n]+\\)"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (2 'xtdmacs-code-doxymacs-bold-face prepend))
    ("\\([@\\\\][cp]\\)\\s-+\\([^  \n]+\\)"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (2 'xtdmacs-code-doxymacs-underline-face prepend))
    ("\\([@\\\\]e\\(m\\)?\\)\\s-+\\([^   \n]+\\)"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (3 'xtdmacs-code-doxymacs-italic-face prepend))
    ("\\([@\\\\]ingroup\\)\\s-+\\(\\(\\sw+\\s-*\\)+\\)\\s-*$"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (2 'xtdmacs-code-doxymacs-string-face prepend))
    ("\\([@\\\\]\\(link\\|copydoc\\|copybrief\\|xrefitem\\|if\\(not\\)?\\|elseif\\)\\)\\s-+\\([^ \n]+\\)"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (4 'xtdmacs-code-doxymacs-string-face prepend))
    ("\\([@\\\\]\\(cond\\|dir\\)\\(\\s-+[^   \n]+\\)?\\)"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (3 'xtdmacs-code-doxymacs-string-face prepend t))
    ("\\([@\\\\]\\(~\\)\\([^   \n]+\\)?\\)"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (3 'xtdmacs-code-doxymacs-string-face prepend t))
    ("\\([@\\\\]\\(example\\|\\(dont\\)?include\\|includelineno\\|htmlinclude\\|verbinclude\\)\\)\\s-+\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (4 'xtdmacs-code-doxymacs-string-face prepend))
    ("\\([@\\\\]dotfile\\)\\s-+\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)\\(\\s-+\"[^\"]+\"\\)?"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (2 'xtdmacs-code-doxymacs-string-face prepend)
     (3 'xtdmacs-code-doxymacs-string-face prepend t))
    ("\\([@\\\\]image\\)\\s-+\\(html\\|latex\\)\\s-+\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)\\(\\s-+\"[^\"]+\"\\)?\\(\\s-+\\sw+=[0-9]+\\sw+\\)?"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (2 'xtdmacs-code-doxymacs-string-face prepend)
     (3 'xtdmacs-code-doxymacs-string-face prepend)
     (4 'xtdmacs-code-doxymacs-string-face prepend t)
     (5 'xtdmacs-code-doxymacs-string-face prepend t))
    ("\\([@\\\\]\\(addtogroup\\|defgroup\\|weakgroup\\|page\\|anchor\\)\\)\\s-+\\([^ \n]+\\)"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (3 'xtdmacs-code-doxymacs-string-face prepend))
    ("\\([@\\\\]\\(ref\\|section\\|subsection\\|subsubsection\\)\\)\\s-+\\([^ \n]+\\)\\(\\s-+\"[^\n\"]+\"\\)"
     (4 'xtdmacs-code-doxymacs-parameter-face prepend))
    ("\\([@\\\\]\\(ref\\|section\\|subsection\\|subsubsection\\)\\)\\s-+\\([^ \n]+\\)"
     (1 'xtdmacs-code-doxymacs-keyword-face prepend)
     (3 'xtdmacs-code-doxymacs-string-face prepend))
    ("\\(^[* ]+\\)\\(#\\s-+[^\n]+\\)"
     (2 'xtdmacs-code-doxymacs-h1-face prepend))
    ("\\(^[* ]+\\)\\(##\\s-+[^\n]+\\)"
     (2 'xtdmacs-code-doxymacs-h2-face prepend))
    ("\\(^[* ]+\\)\\(###\\s-+[^\n]+\\)"
     (2 'xtdmacs-code-doxymacs-h3-face prepend))
    ("\\(^[* ]+\\)\\(####\\s-+[^\n]+\\)"
     (2 'xtdmacs-code-doxymacs-h4-face prepend))
    )
  "List of keywords for xtdmacs-code-doxymacs mode"
  :group 'xtdmacs-code-doxymacs
  :safe '(lambda(val) t)
  )

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
  (font-lock-add-keywords nil xtdmacs-code-doxymacs-keywords-alist)
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

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
