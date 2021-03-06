;;
;; url: http://hacks-galore.org/aleix/blog/archives/2013/01/08/install-emacs-packages-from-command-line
;; Install package from command line. Example:
;;
;;   $ emacs --batch --expr "(define pkg-to-install 'smex)" -l emacs-pkg-install.el
;;

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" .        "https://melpa.org/packages/")        t)
(package-initialize)
(when pkg-refresh
  (package-refresh-contents))

;; Fix HTTP1/1.1 problems
(setq url-http-attempt-keepalives nil)
;;(package-refresh-contents)
(package-install-file pkg-to-install)
