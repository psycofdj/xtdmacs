(require 'xtdmacs-compile++)
(require 'package)
(require 'go-autocomplete)
(require 'auto-complete-config)
(require 'go-mode)
(require 'yasnippet)

(defface xtdmacs-code-go-face-indent-error
  '((t (:foreground "color-124" :underline t)))
  "Indicates spaces instead of tabs for indentation"
  :group 'xtdmacs-code-go
  )

(defcustom xtdmacs-code-go-compile-alist
  '(("compile" .
     (("dir"        . xtdmacs-compile++-get-dir-git)
      ("bin"        . xtdmacs-code-go-get-project-name)
      ("env"        . "")
      ("get-params" . xtdmacs-compile++-default-params)
      ("command"    . xtdmacs-code-go-command)))
    ("test" .
     (("dir"        . xtdmacs-compile++-get-dir-git)
      ("bin"        . xtdmacs-code-go-get-project-name)
      ("env"        . "")
      ("get-params" . xtdmacs-compile++-default-params)
      ("command"    . xtdmacs-code-go-command)))
    ("doc" .
     (("dir"        . xtdmacs-compile++-get-dir-buffer)
      ("bin"        . "nakedret")
      ("env"        . "")
      ("get-params" . xtdmacs-compile++-default-params)
      ("command"    . xtdmacs-compile++-default-command)))
    ("lint" .
     (("dir"        . xtdmacs-compile++-get-dir-buffer)
      ("bin"        . "gometalinter.v2 -D gocyclo -D errcheck")
      ("env"        . "")
      ("get-params" . xtdmacs-compile++-default-params)
      ("command"    . xtdmacs-compile++-default-command)))
    ("manual" .
     (("dir"        . xtdmacs-compile++-get-dir-git)
      ("bin"        . "true")
      ("env"        . "")
      ("get-params" . xtdmacs-compile++-default-params)
      ("command"    . xtdmacs-compile++-default-command))))
  "Xtdmacs-Code-go compilation configuration"
  :group 'xtdmacs-code-go
  :safe '(lambda(p) t)
  :type '(alist :key-type string
                :value-type (alist :key-type string
                                   :value-type (choice (string) (function))))
  )

(defcustom xtdmacs-code-go-keywords-alist
  '(("\\<g_[_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-global-variable)
    ("\\<l_[_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-local-variable)
    ("\\<\\(p_[_a-zA-Z0-9]+\\)\\>"     . 'xtdmacs-code-face-param)
    ("\\<c_[_a-zA-Z0-9]+\\>"           . 'xtdmacs-code-face-counter)
    ("\\<g[A-Z][_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-global-variable)
    ("\\<l[A-Z][_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-local-variable)
    ("\\<\\(p[A-Z][_a-zA-Z0-9]+\\)\\>" . 'xtdmacs-code-face-param)
    ("\\<c[A-Z][_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-counter)
    ("\\<r[A-Z][_a-zA-Z0-9]+\\>"       . 'xtdmacs-code-face-return)
    ("[&*]"                            . 'font-lock-constant-face)
    ("self"                            . 'font-lock-keyword-face)
    ("^ +"                             . 'xtdmacs-code-go-face-indent-error)
    )
  "List of additional go font-lock keywords"
  :group 'xtdmacs-code-go
  :safe '(lambda(p) t))

(defcustom xtdmacs-code-go-indent-load-auto
  nil
  "Enables go code auto-indentation on load."
  :group 'xtdmacs-code-go
  :type 'boolean
  :safe 'booleanp)

(defcustom xtdmacs-code-go-indent-save-auto
  nil
  "Enables go code auto-indentation on save."
  :group 'xtdmacs-code-go
  :type 'boolean
  :safe 'booleanp
  )

;; --------------------------------------------------------------------------- ;


(defun --xtdmacs-code-go-region-matches (line len min max)
  (and
   (<= min line)
   (>= max (+ line len))))

(defun --xtdmacs-code-go-apply-rcs-patch-region (patch-buffer beg end)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (min-line (line-number-at-pos beg))
        (max-line (line-number-at-pos end))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in --xtdmacs-code-go-apply-rcs-patch-region"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (if (not (--xtdmacs-code-go-region-matches (- from line-offset) len min-line max-line))
                    (cl-decf line-offset len)
                  (let ((text (buffer-substring start (point))))
                    (with-current-buffer target-buffer
                      (cl-decf line-offset len)
                      (goto-char (point-min))
                      (forward-line (- from len line-offset))
                      (insert text))))))
             ((equal action "d")
              (if (not (--xtdmacs-code-go-region-matches from len min-line max-line))
                  (cl-incf line-offset len)
                (with-current-buffer target-buffer
                  (go--goto-line (- from line-offset))
                  (cl-incf line-offset len)
                  (go--delete-whole-line len))))
             (t
              (error "Invalid rcs patch or internal error in --xtdmacs-code-go-apply-rcs-patch-region"))))
          )))))

(defun xtdmacs-code-go-format-region ()
  "Format the current buffer according to the formatting tool.

The tool used can be set via ‘gofmt-command` (default: gofmt) and additional
arguments can be set as a list via ‘gofmt-args`."
  (interactive)
  (let ((tmpfile (make-temp-file "gofmt" nil ".go"))
        (patchbuf (get-buffer-create "*Gofmt patch*"))
        (errbuf (if gofmt-show-errors (get-buffer-create "*Gofmt Errors*")))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        our-gofmt-args)

    (unwind-protect
        (save-restriction
          (widen)
          (if errbuf
              (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))

          (write-region nil nil tmpfile)

          (when (and (gofmt--is-goimports-p) buffer-file-name)
            (setq our-gofmt-args
                  (append our-gofmt-args
                          ;; srcdir, despite its name, supports
                          ;; accepting a full path, and some features
                          ;; of goimports rely on knowing the full
                          ;; name.
                          (list "-srcdir" (file-truename buffer-file-name)))))
          (setq our-gofmt-args (append our-gofmt-args
                                       gofmt-args
                                       (list "-w" tmpfile)))
          (message "Calling gofmt: %s %s" gofmt-command our-gofmt-args)
          ;; We're using errbuf for the mixed stdout and stderr output. This
          ;; is not an issue because gofmt -w does not produce any stdout
          ;; output in case of success.
          (if (zerop (apply #'call-process gofmt-command nil errbuf nil our-gofmt-args))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                    (message "Buffer is already gofmted")
                  (--xtdmacs-code-go-apply-rcs-patch-region patchbuf (region-beginning) (region-end))
                  (message "Applied gofmt"))
                (if errbuf (gofmt--kill-error-buffer errbuf)))
            (message "Could not apply gofmt")
            (if errbuf (gofmt--process-errors (buffer-file-name) tmpfile errbuf))))

      (kill-buffer patchbuf)
      (delete-file tmpfile)
      )))

(defun xtdmacs-code-go-get-project-name ()
  (file-name-nondirectory
   (directory-file-name
    (xtdmacs-compile++-get-dir-git)))
  )

(defun xtdmacs-code-go-command (type &optional mode)
  (let* ((dir    (--xtdmacs-compile++-get-value mode type "dir"))
         (env    (--xtdmacs-compile++-get-value mode type "env"))
         (bin    (--xtdmacs-compile++-get-value mode type "bin")))
    (format "cd %s && %s go build -o %s *.go"
            (funcall-or-value dir)
            (funcall-or-value env)
            (funcall-or-value bin)))
  )

;; --------------------------------------------------------------------------- ;

(defun --xtdmacs-code-go-construct()
  (font-lock-add-keywords nil xtdmacs-code-go-keywords-alist)

  (ac-config-default)
  (setq ac-sources '(ac-source-go))
  (go-eldoc-setup)
  (define-key go-mode-map (kbd "<f12>")   'godef-jump)
  (define-key go-mode-map (kbd "C-<f12>") 'godef-jump-other-window)
  (yas-minor-mode)
  (go-snippets-initialize)
  (unless (mode-enabled 'yas-minor-mode)
    (yas-minor-mode t))

  (when (mode-enabled 'xtdmacs-compile++-mode)
    (xtdmacs-compile++-register-config "go-mode" xtdmacs-code-go-compile-alist))

  ;; (if xtdmacs-code-go-indent-save-auto
  ;;     (add-hook 'before-save-hook '(lambda() (xtdmacs-code-format-buffer t nil)) t t))
  (if xtdmacs-code-go-indent-save-auto
      (add-hook 'before-save-hook #'gofmt-before-save))
  (if xtdmacs-code-go-indent-load-auto
      (xtdmacs-code-format-buffer t nil))


  (add-to-list 'compilation-error-regexp-alist 'nakedret)
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(nakedret
     "\\(.+?\\):\\([0-9]+\\).*naked returns on.*" 1 2))

  (message "enabled : xtdmacs-code-go-mode")
  )

(defun --xtdmacs-code-go-destroy()
  (if xtdmacs-code-go-indent-save-auto
      (remove-hook 'before-save-hook '(lambda() (xtdmacs-code-format-buffer t nil))))
  (when (mode-enabled 'yas-minor-mode)
    (yas-minor-mode nil))
  (font-lock-remove-keywords nil xtdmacs-code-go-keywords-alist)
  (message "disabled : xtdmacs-code-go-mode")
  )

;;;###autoload
(define-minor-mode xtdmacs-code-go-mode
  "Code for Go" nil "Code"
  '(("\M-t"    . xtdmacs-code-go-format-region)
    ("\C-\M-t" . gofmt)
    ("\M-."    . ac-start)
    ("\C-e"    . godoc-at-point)
    ("\M-e"    . godoc))
  (if xtdmacs-code-go-mode
      (--xtdmacs-code-go-construct)
    (--xtdmacs-code-go-destroy))
  )


;; --------------------------------------------------------------------------- ;

;;;###autoload
(put 'xtdmacs-code-go-compile-alist 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-go-keywords-alist 'safe-local-variable '(lambda(val) t))
;;;###autoload
(put 'xtdmacs-code-go-indent-load-auto 'safe-local-variable 'booleanp)
;;;###autoload
(put 'xtdmacs-code-go-indent-save-auto 'safe-local-variable 'booleanp)

(provide 'xtdmacs-code-go)

;; Local Variables:
;; ispell-local-dictionary: "american"
;; End:
