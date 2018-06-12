;; -*- mode: emacs-lisp; -*-

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("melpa"        . "http://melpa.milkbox.net/packages/"))
  ;;(add-to-list 'package-archives '("marmalade"    . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("marmalade"    . "https://ojab.ru/marmalade/"))
  (package-initialize))
(fset 'xterm-color-unfontify-region 'font-lock-default-unfontify-region)
;; Custom file definition


(setq custom-file "~/.emacs-custom")
(load custom-file)

;; configuration de l'encodage
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Desactive le systeme de creation de fichiers de sauvegarde emacs (file.cc~)
(setq backup-inhibited  t)

;; Diviser la fenetre en deux buffers
(split-window-horizontally)

(if window-system
    (setq frame-background-mode 'light)
  (setq frame-background-mode 'dark))

(require 'flyspell)
(require 'xtdmacs-find)
(require 'xtdmacs-loader)
(require 'go-autocomplete)
(require 'auto-complete-config)
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/elpa/go-snippets-20170831.2302/go-snippets.el")

(defun testload()
  (interactive)
  (ac-config-default)
  (setq ac-sources '(ac-source-go))

  )

