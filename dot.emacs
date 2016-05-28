;; -*- mode: emacs-lisp -*-
;; ----------------------
;; -  User config file  -
;; ----------------------
(message "loading .emacs...")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq load-path (cons (file-name-directory (file-truename "~/.emacs")) load-path))

;; Custom file definition
(setq custom-file "~/.emacs-custom")
(load custom-file)

;; Adresse email
(setq user-mail-adress "xavier.marcelet@orange.com")
;; Nom de l'utilisateur
(setq user-full-name "Xavier MARCELET")

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; conf generale
(require 'config)
