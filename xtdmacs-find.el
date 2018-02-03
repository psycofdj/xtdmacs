;; Open files and goto lines like we see from g++ etc. i.e. file:line#
(defadvice find-file-noselect
    (around find-file-noselect-at-line
            (filename &optional nowarn rawfile wildcards)
            activate)
  "Turn files like file.cpp:14:5 into file.cpp and going to the 14-th line and 5th col"

  (save-match-data
    (let* ((matched (string-match "^\\(.*?\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?:?$" filename))
           (line-number
            (and matched
                 (match-string 2 filename)
                 (string-to-number (match-string 2 filename))))
           (column-number
            (and matched
                 (match-string 4 filename)
                 (string-to-number (match-string 4 filename))))
           (filename
            (if (and matched (file-exists-p (match-string 1 filename)))
                (match-string 1 filename)
              filename))
           (buffer-name ad-do-it))

      (when line-number
        (with-current-buffer buffer-name
          (goto-char (point-min))
          (forward-line (1- line-number))))
      (when column-number
        (with-current-buffer buffer-name
          (move-to-column column-number)))
      ))
  )

(provide 'xtdmacs-find)
