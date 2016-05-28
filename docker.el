(defface docker-mode-face-parent-url       '((t (:foreground "color-134"  :weight normal))) "Parent image url"   :group 'docker-mode)
(defface docker-mode-face-parent-tag       '((t (:foreground "color-114"  :weight normal))) "Parent image tag"   :group 'docker-mode)
(defface docker-mode-face-maintainer-name  '((t (:foreground "color-134"  :weight normal))) "Maintainer's name"  :group 'docker-mode)
(defface docker-mode-face-maintainer-email '((t (:foreground "color-114"  :weight normal))) "Maintainer's email" :group 'docker-mode)
(defvar docker-mode-keywords
  '("FROM" "RUN" "COPY" "CMD" "MAINTAINER"
    "ARG" "VOLUME" "EXPOSE" "LABEL" "ENV" "ADD"
    "ENTRYPOINT" "USER" "WORKDIR" "ONBUILD" "STOPSIGNAL")
  "Dockerfile font-lock keywords")

(defvar docker-mode-syntaxes
  '(("\\<FROM\\> +\\([^:]*\\)\\(:\\(.*\\)\\)?" 1 'docker-mode-face-parent-url)
    ("\\<FROM\\> +\\([^:]*\\)\\(:\\(.*\\)\\)?" 3 'docker-mode-face-parent-tag)
    ("\\<MAINTAINER\\> +\\(.*\\) +\\(<.*>\\)"  1 'docker-mode-face-maintainer-name)
    ("\\<MAINTAINER\\> +\\(.*\\) +\\(<.*>\\)"  2 'docker-mode-face-maintainer-email)
    ("\"[^\"]*\""                              . 'font-lock-string-face)
    ("\\(#.*\\)"                               1 'font-lock-comment-face))
  "Dockerfile font-lock syntax"
  )

(defun --docker-add-keywords ()
  (let* ((keywords (mapcar (lambda (el) (cons (concat "\\<" el "\\>") font-lock-keyword-face)) docker-mode-keywords)))
    (font-lock-add-keywords nil keywords)
    (font-lock-add-keywords nil docker-mode-syntaxes)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (when font-lock-mode
        (with-no-warnings (font-lock-fontify-buffer)))))
  )

(defun --docker-remove-keywords ()
  (let* ((keywords (mapcar (lambda (el) (cons (concat "\\<" el "\\>") font-lock-keyword-face)) docker-mode-keywords)))
    (font-lock-remove-keywords nil keywords)
    (font-lock-remove-keywords nil docker-mode-syntaxes)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (when font-lock-mode
        (with-no-warnings (font-lock-fontify-buffer)))))
  )

(defun docker-build-directory ()
  (file-name-directory (buffer-file-name)))

(defun docker-build-default-tag ()
  (concat (getenv "USER") "/" (file-name-nondirectory (directory-file-name (funcall-or-value 'docker-build-directory))))
  )

(defun docker-build-default-bin ()
  (let* ((config (cdr (assoc "compile" compile++-config-alist)))
         (args       (cdr (assoc "args" config)))
         (dockerargs (split-join args "--build-arg"))
         (tag        (cdr (assoc "tag"  config))))
    (format "docker build -t %s %s ." tag dockerargs)))

(defun docker-build-get-params ()
  (let* ((config (cdr (assoc "compile" compile++-config-alist)))
         (dir  (cdr (assoc "dir"  config)))
         (args (cdr (assoc "args" config)))
         (tag  (cdr (assoc "tag"  config)))
         (bin  (cdr (assoc "bin"  config))))
    (setcdr (assoc "dir"  config) (read-directory-name  "Directory: "  (funcall-or-value dir)))
    (setcdr (assoc "args" config) (read-from-minibuffer "Build args: " (funcall-or-value args)))
    (setcdr (assoc "tag"  config) (read-from-minibuffer "Tag: "        (funcall-or-value tag)))
    (setcdr (assoc "bin"  config) (read-from-minibuffer "Command: "    (funcall-or-value bin)))
    ))

(defun docker-build-command ()
  (let* ((config (cdr (assoc "compile" compile++-config-alist)))
         (dir  (cdr (assoc "dir"  config)))
         (bin  (cdr (assoc "bin"  config))))
    (format "cd %s && %s" dir bin))
  )

(defun --docker-construct ()
  (--docker-add-keywords)
  (when (and (boundp 'compile++-mode) compile++-mode)
    (setcdr (assoc "compile" compile++-config-alist)
            '(("dir"        . docker-build-directory)
              ("args"       . "")
              ("bin"        . docker-build-default-bin)
              ("tag"        . docker-build-default-tag)
              ("get-params" . docker-build-get-params)
              ("command"    . docker-build-command))))
  (set (make-local-variable 'comment-start) "#")
  (message "enabled : docker-mode")
  )

(defun --docker-destroy ()
  (--docker-remove-keywords)
  (message "disabled : docker-mode")
  )

;;;###autoload
(define-minor-mode docker-mode
  "Docker syntax mode" nil "Docker"
  '()
  (if docker-mode
      (--docker-construct)
    (--docker-destroy))
  )

(provide 'docker)
