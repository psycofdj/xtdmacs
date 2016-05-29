;;;###autoload
(defcustom xtdmacs-loader-auto-minor-mode-alist
  nil
  "Alist of filename patterns vs correpsonding minor mode functions,
see `auto-mode-alist'. All elements of this alist are checked,
meaning you can enable multiple minor modes for the same
regexp."
  :group 'loader
  :safe '(lambda(p) t)
  :type '(alist :key-type (repeat string) :value-type (repeat symbol))
  )

(defun xtdmacs-loader-load-minor-modes ()
  (dolist (item xtdmacs-loader-auto-minor-mode-alist)
    (let* ((exts (car item))
           (modes (cdr item)))
      (dolist (ext exts)
        (when (string-match-p ext (buffer-file-name))
          (dolist (mode modes)
            (funcall mode))))))
  )

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

(put 'tags-file-name 'safe-local-variable 'stringp)

(add-hook 'find-file-hook 'xtdmacs-loader-load-minor-modes)

(provide 'xtdmacs-loader)
