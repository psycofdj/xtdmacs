;;; xtdmacs.el --- Set of developing tools  -*- lexical-binding: t -*-

;; Author: Xavier Marcelet
;; URL: https://github.com/psycofdj/xtdmacs
;; Version: 0.8
;; Package-Requires: ((emacs "27.1")
;;                    (auto-complete             "20250101.843")
;;                    (company                   "20260424.2111")
;;                    (lsp-mode                  "20260508.1656")
;;                    (lsp-ui                    "20260507.1740")
;;                    (flycheck                  "20260320.1715")
;;                    (cmake-mode                "20250114.1444")
;;                    (column-enforce-mode       "20200605.1933")
;;                    (dockerfile-mode           "20240914.1549")
;;                    (groovy-mode               "20230318.533")
;;                    (iflipb                    "20220612.858")
;;                    (irony                     "20231018.1915")
;;                    (js2-mode                  "20241205.140")
;;                    (json-mode                 "20240427.1245")
;;                    (markdown-mode             "20260425.954")
;;                    (markdown-toc              "20260131.1444")
;;                    (php-mode                  "20250602.1308")
;;                    (pkg-info                  "20150517.1143")
;;                    (popup                     "20250101.843")
;;                    (smarty-mode               "20100703.1158")
;;                    (use-package               "2.4.5")
;;                    (web-mode                  "20241227.530")
;;                    (xterm-color               "20251128.1842")
;;                    (yafolding                 "20250601.2133")
;;                    (yaml-mode                 "20260420.156")
;;                    (yasnippet                 "20250602.1342"))

;;; Commentary:

;; xtdmacs is a collection of code-editing helpers for Emacs covering
;; multiple languages (C/C++, Go, Python, TypeScript, Terraform, YAML,
;; PHP, JavaScript, JSON, Java, Lisp, Shell, and more).
;;
;; The entry points are the per-language setup functions, which are
;; auto-attached to their major-mode hooks; users typically just need
;; to `(require 'xtdmacs-loader)' from their init file.

;;; Code:

(provide 'xtdmacs)

;;; xtdmacs.el ends here
