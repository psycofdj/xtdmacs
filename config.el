;; include du repertoire /libs
(setq load-path (cons (concat (file-name-directory load-file-name) "/libs/") load-path))

;; -------------------------
;; - Comportement general -
;; -------------------------

;; garder les messages verbeux dans le buffer *Messages*
(setq message-log-max t)

;; Ne pas activier la coloration de la zone entre la mark
;; et le cursor
(setq transient-mark-mode 'nil)

;; taille de l'indentation par defaut
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; affiche les espaces en fin de ligne
(setq-default show-trailing-whitespace  t)

;; desactivation des gestionaires de source-control
(setq vc-handled-backends    nil)

;; configuration de l'encodage
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Emacs ne demande pas de confirmation avant de quitter. Emacs demandera si on veut
;; sauvegarder les fichiers en cours d'edition
(setq confirm-kill-emacs nil)

;; Toujours ajouter un \n en fin de fichier a la sauvegarde
(setq require-final-newline t)

;; Ne pas montrer l'ecran de bienvenu emacs au demarrage
(setq inhibit-splash-screen t)

;; Desactive le systeme de creation de fichiers de sauvegarde emacs (file.cc~)
(setq backup-inhibited  t)
(setq auto-save-default nil)

;; Diviser la fenetre en deux buffers
(split-window-horizontally)

(scroll-bar-mode -1)
;; ------------------
;; - Mode Changelog -
;; ------------------
;; Adresse email pour le mode changelog-mode
(setq add-log-mailing-address user-mail-adress)

;; Activation du rechargement automatique des fichiers qui ont changés sur disque
(global-auto-revert-mode 1)
(setq auto-revert-interval 0.5)

(if window-system
    (setq frame-background-mode 'light)
  (setq frame-background-mode 'dark))

;; ------------------------------------
;; - Appel aux bibliotheques externes -
;; ------------------------------------
(require 'lang)
(require 'bindings)
(require 'autoloader)
(require 'loader)

(provide 'config)
