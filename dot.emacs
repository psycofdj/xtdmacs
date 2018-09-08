;; -*- mode: emacs-lisp; -*-
;; -*- lexical-binding: t -*-

;; configure repositories
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("marmalade"    . "https://ojab.ru/marmalade/"))
  (package-initialize))

(fset 'xterm-color-unfontify-region 'font-lock-default-unfontify-region)

;; custom file definition
(setq custom-file "~/.emacs-custom")
(load custom-file)

;; set utf-8 encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; disable automatic backup files
(setq backup-inhibited  t)

;; split window by default
(split-window-horizontally)

(if window-system
    (setq frame-background-mode 'light)
  (setq frame-background-mode 'dark))

(require 'xtdmacs-find)
(require 'xtdmacs-loader)
(require 'go-autocomplete)
(require 'auto-complete-config)
(require 'yasnippet)
