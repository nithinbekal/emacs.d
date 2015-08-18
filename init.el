
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(defvar my-packages
  '(
     alchemist
     elixir-mode
     enh-ruby-mode
     evil
     evil-leader
     evil-rails
     helm
     magit
     projectile
     relative-line-numbers
     robe
     smartparens
     textmate
  ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(global-relative-line-numbers-mode)

(require 'evil)
(evil-mode t)

(require 'evil-rails)

(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "," 'helm-find-file
  "e" 'find-file
  "q" 'kill-buffer)

(require 'elixir-mode)

(require 'helm-config)
(setq helm-autoresize-mode t)
(setq helm-buffers-fuzzy-matching t)

(require 'projectile)
(projectile-global-mode)

(require 'enh-ruby-mode)
(add-hook 'ruby-mode-hook 'robe-mode)

(require 'smartparens)
(smartparens-global-mode 1)

(require 'textmate)
(textmate-mode)
