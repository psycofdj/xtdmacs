;; -*- mode: emacs-lisp; -*-
;; -*- lexical-binding: t -*-

;; configure repositories
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives
        '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
          ("MELPA Stable" . "https://stable.melpa.org/packages/")
          ("MELPA" . "https://melpa.org/packages/"))
        package-archive-priorities
        '(("MELPA Stable" . 10)
          ("MELPA" . 5)
          ("GNU ELPA" . 0)))
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

(add-hook 'sh-mode-hook 'xtdmacs-code-shell-mode)
(add-hook 'sh-mode-hook 'xtdmacs-code-mode)
(add-hook 'python-mode-hook 'xtdmacs-code-python-mode)
(add-hook 'python-mode-hook 'xtdmacs-code-mode)

(require 'xtdmacs-find)
(require 'xtdmacs-loader)
(require 'auto-complete-config)
(require 'yasnippet)
