;;; init.el --- user configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;; This file loads the greater part of my Emacs configuration.

;;; Code:

;;;; load modules
(p/mods
 (m editing)    ;; evil, avy, rg and stuff
 (m completion) ;; orderless, marginalia, consult, embark, company
 (m parens)     ;; smartparens
 (m project)    ;; projectile
 (m vcs)        ;; magit
 (m snippets)   ;; yasnippet
 (m rainbow)    ;; rainbow, delimeters, indent-guides and stuff
 (m paredit)    ;; paredit
 (m ibuffer)
 (m dashboard)
 (m ranger)
 (m checkers)  ;; fly[check/spell]
 (l ru-dvorak) ;; my dvorak-based russian IM
 (m eshell)    ;; aweshell
 (m writeroom)
 (m exwm)

 (lang conf)
 (lang lsp)
 (lang python)
 (lang haskell)
 (lang markdown)
 (lang org)
 (lang cl))

;;;; add user info
(setq user-full-name "Pavel Stepanov"
      user-mail-address "paulkreuzmann@gmail.com")

;;;; set input method
(setq default-input-method 'ru-dvorak)

;;;; change window using mouse
(setq focus-follows-mouse t
      mouse-autoselect-window t)

;;;; load favourite font
(when (and (display-graphic-p)
	   (member "Fira Code" (font-family-list)))
  (set-frame-font "Fira Code-10" t t))

;;;; load a beautiful theme
(use-package acme-theme
  :disabled
  :config
  (setq acme-theme-black-fg t)
  (load-theme 'acme t))

(use-package modus-themes
  :config
  (load-theme 'modus-operandi t))

;;;; cl
(setq inferior-lisp-program "ros run")
