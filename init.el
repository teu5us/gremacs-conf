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
 (m folds)
;;;;; languages
 (lang treesitter)
 (lang conf)
 (lang lsp)
 (lang python)
 (lang haskell)
 (lang markdown)
 (lang org)
 (lang latex)
 (lang cl)
 (lang nix))

;;;; add user info
(setq user-full-name "Pavel Stepanov"
      user-mail-address "paulkreuzmann@gmail.com")

;;;; set input method
(add-hook 'after-init-hook
          #'(lambda ()
              (setq default-input-method 'russian-computer))
          90)

;;;;; also change some keys for dvorak

(defun extra-keys ()
  (keyboard-translate ?\C-t ?\C-x)
  (keyboard-translate ?\C-x ?\C-t))

;; (add-hook 'dashboard-mode-hook #'extra-keys)

;;;; change window using mouse
(setq focus-follows-mouse t
      mouse-autoselect-window t)

;;;; load favourite font
(setq p/main-font "FiraCode NF"
      p/main-font-size 14
      p/variable-pitch-font "Fira Sans"
      p/variable-pitch-font-size 14)

;;;; load a beautiful theme
(use-package acme-theme
  :custom
  (acme-theme-black-fg t))

(use-package modus-themes
  :custom
  (modus-themes-variable-pitch-ui t))

(use-package dracula-theme
  :custom
  (dracula-enlarge-headings nil)
  (dracula-height-title-1 1.0)
  (dracula-height-title-2 1.0)
  (dracula-height-title-3 1.0))

(add-hook 'after-init-hook
          #'(lambda ()
              (load-theme 'dracula t)))

;;;; cl
(setq inferior-lisp-program "ros run")

;;;; helpful C source
(setq find-function-C-source-directory
      (let ((standard-directory (format "/usr/share/emacs/%s/src" emacs-version))
            (nix-directory (format "%s../share/emacs/%s/src"
                                   (file-name-directory (file-truename (executable-find "emacs")))
                                   emacs-version)))
        (cond
         ((file-directory-p standard-directory)
          standard-directory)
         ((file-directory-p nix-directory)
          nix-directory)
         (t nil))))

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

;;;; dimmer
(use-package dimmer
  :hook (after-init . dimmer-mode)
  :custom
  (dimmer-adjustment-mode :both)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-hydra)
  (dimmer-configure-org))

;;;; sushi preview in ranger
(with-eval-after-load 'ranger
  (defun p/sushi-preview ()
    (interactive)
    (let ((marked-files (dired-get-marked-files)))
      (mapc (lambda (f)
              (let ((process-connection-type nil))
                (start-process "" nil "sushi" f)))
            marked-files)))

  (define-key ranger-normal-mode-map (kbd "w p") #'p/sushi-preview)
  (define-key ranger-emacs-mode-map (kbd "C-x w p") #'p/sushi-preview))

;;;; splits configuration
(setq split-width-threshold 80
      split-height-threshold 40)
