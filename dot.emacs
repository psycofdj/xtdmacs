;;; dot.emacs --- Default ~/.emacs configuration file  -*- mode: emacs-lisp; lexical-binding: t -*-

;;; Commentary:

;; Create symlink from this file to ~/.emacs

;;; Code:


(fset 'xterm-color-unfontify-region 'font-lock-default-unfontify-region)

;; install packages from source
(unless (package-installed-p 'yaml-lsp)
  (package-vc-install
   '(yaml-lsp
     :vc-backend Git
     :url "https://github.com/psycofdj/yaml-lsp"
     :lisp-dir "emacs"
     :main-file "yaml-lsp.el")))

;; configure package repositories
(require 'package)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


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
(require 'auto-complete-config)
(require 'yasnippet)
