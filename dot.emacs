;; -*- mode: emacs-lisp; -*-
;; -*- lexical-binding: t -*-

;; configure repositories
(require 'package)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(fset 'xterm-color-unfontify-region 'font-lock-default-unfontify-region)

;; custom file definition
(setq custom-file "~/.emacs-custom")
(load custom-file)
;(load "~/.emacs.d/dash.el")
;(load "~/.emacs.d/dash-functional.el")

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

;; Per-language setup functions (xtdmacs-code-shell-setup, xtdmacs-code-python-setup
;; etc.) are auto-attached to their major-mode hooks by the language files via
;; ;;;###autoload cookies.  No manual add-hook calls needed here.

(require 'xtdmacs-find)
(require 'xtdmacs-loader)
(require 'auto-complete-config)
(require 'yasnippet)
