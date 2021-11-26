;;; init.el --- user configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;; This file loads the greater part of my Emacs configuration.

;;; Code:

;;;; load modules
(p/mods
;;;;; stuff
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
 (m vterm)
 (m writeroom)
 (m exwm)
 (m telega)
 (m pdf)
;;;;; languages
 (lang treesitter)
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
(setq default-input-method 'russian-computer)

;;;;; also change some keys for dvorak

(defun extra-keys ()
  (keyboard-translate ?\C-t ?\C-x)
  (keyboard-translate ?\C-x ?\C-t))

;; (add-hook 'dashboard-mode-hook #'extra-keys)

;;;; change window using mouse
(setq focus-follows-mouse t
      mouse-autoselect-window t)

;;;; load favourite font
(when (and (display-graphic-p)
	   (member "FiraCode Nerd Font" (font-family-list)))
  (set-frame-font "FiraCode Nerd Font-10" t t))

;;;; load a beautiful theme
(use-package acme-theme
  :disabled
  :config
  (setq acme-theme-black-fg t)
  (load-theme 'acme t))

(use-package modus-themes
  :hook (after-init . (lambda ()
                        (load-theme 'modus-operandi t))))


;;;; cl
(setq inferior-lisp-program "ros run")

;;;; helpful C source
(setq find-function-C-source-directory
      (format "/usr/share/emacs/%s/src" emacs-version))

;;;; google translate
(use-package google-translate
  :commands (google-translate-translate)
  :bind ("C-c t" . google-translate-smooth-translate)
  :custom
  (google-translate-translation-directions-alist
   '(("en" . "ru") ("ru" . "en")))
  (google-translate-backend-method 'wget)
  :init
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))
  (defun google-translate-from-selection (text)
    (interactive (list (gui-get-primary-selection)))
    (let ((google-translate-output-destination 'kill-ring))
      (google-translate-translate "en" "ru" text)
      (gui-set-selection nil (pop kill-ring))))
  :config
  (require 'google-translate-smooth-ui))

;;;; kill ring selection integration
(setq save-interprogram-paste-before-kill t
      mouse-drag-copy-region t)

;;;; exwm monitor setup
(with-eval-after-load 'exwm-randr
  (setq exwm-randr-workspace-monitor-plist '(0 "DP-2")))
