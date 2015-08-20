
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(defvar my-packages
  '(
     alchemist
     coffee-mode
     company
     elixir-mode
     evil
     evil-leader
     evil-nerd-commenter
     evil-rails
     evil-surround
     evil-visualstar
     flx-ido
     haml-mode
     helm
     ido-vertical-mode
     key-chord
     magit
     mmm-mode
     pbcopy
     powerline
     projectile
     relative-line-numbers
     ruby-mode
     robe
     saveplace
     smartparens
     smex
     smooth-scrolling
     textmate
     ujelly-theme
  ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load "~/.emacs.d/custom/config.el")
