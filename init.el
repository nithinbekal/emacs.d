
; Set GC threshold to 100MB (default is 800KB) to improve start up time.
; For my current emacs.d setup, this has improved startup time has gone from
; 510ms to 270ms - a 47% improvement!
; The downside is that GCing 100MB could cause GC pauses while you're working,
; so it's a good idea to set the limit lower once init has completed.
; I haven't reset the threshold here yet.
; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold 100000000)


; package.el is the built-in package manager included in emacs.
; By default it uses the GNU ELPA repository to look for packages.
; GNU ELPA contains a very small number of packages, so we add MELPA, where
; most of the popular packages are available.

(require 'package)
(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))


; UI related stuff

(set-default-font "Monaco-12")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq make-backup-files nil)
(setq auto-save-default nil)

; Start GUI emacs maximized
(set-frame-parameter nil 'fullscreen 'maximized)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Don't litter project directories with #autosave_files#
(setq make-backup-files nil)

;; Auto insert closing parenthesis/brackets/braces
(electric-pair-mode)

;; Answer yes/no prompts with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Move GUI customizations to separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


; use-package allows you to load packages lazily, and speeds up the initial
; load time of emacs.

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

; use-package isn't needed at runtime, so this can reduce load time.
(eval-when-compile
  (require 'use-package))


;; Elixir tooling support
(use-package alchemist
  :init (add-hook 'elixir-mode-hook 'alchemist-mode)
  :defer t
  :ensure t)


(use-package clojure-mode
  :init
  (use-package cider :defer t :ensure t)

  :defer t
  :ensure t)


;; Coffeescript syntax highlighting
(use-package coffee-mode :defer t :ensure t)


;; Hide some minor modes in the mode line.
;; use-package supports diminish, so you can hide modes by adding
;; :diminish 'mode-name to the use-package form.
(use-package diminish :ensure t)


;; Opens Dash.app to look up documentation for the word at point on <leader>dd
(use-package dash-at-point :ensure t)


;; Syntax highlighting and navigation for Elixir.
(use-package elixir-mode :defer t :ensure t)


;; Powerful vim emulation inside emacs.
;; Evil specific packages:
;; - evil-mc: Multiple cursor support
;;   C-n/C-p to create cursors, C-t to skip cursor, gru to undo all cursors
;; - evil-leader: Configure leader keys.
;; - evil-surround: Emacs port of vim-surround. Allows adding/removing/changing
;;   surround characters. eg. `cs"'` in normal mode changes surround from
;;   double to single quotes.
(use-package evil
  :init
  (use-package evil-mc
    :init (global-evil-mc-mode 1)
    :diminish 'evil-mc-mode
    :ensure t)

  (use-package evil-leader
    ; (global-evil-leader-mode) should be loaded before (evil-mode 1).
    :init (global-evil-leader-mode)

    :config
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      ","  'helm-projectile-find-file
      "bb" 'helm-buffers-list
      "bi" (lambda () (interactive) (load-file "~/.emacs.d/init.el"))
      "dd" 'dash-at-point
      "f"  'helm-projectile-ag
      "gs" 'magit-status
      "gg" (lambda () (interactive) (find-file "~/Dropbox/notes/todo.org"))
      "gt" (lambda () (interactive) (find-file "~/Dropbox/todo/gtd.md"))
      "q"  'kill-buffer-and-window
      "s"  'projectile-toggle-between-implementation-and-test
      "vs" 'split-window-horizontally
      "vi" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
      "yb" (kbd "gg v G y") ; Yank buffer
      "yt" 'yafolding-toggle-element
      "yy" 'yafolding-toggle-all
      )

    :ensure t)

  (use-package evil-surround
    :config (global-evil-surround-mode 1)
    :ensure t)

  (evil-mode 1)

  ; Use C-s to save files
  (define-key evil-normal-state-map "\C-s" 'save-buffer)
  (define-key evil-insert-state-map "\C-s" 'save-buffer)

  ; Use C-u to delete line backwards
  ; Ref: https://github.com/purcell/emacs.d/blob/485a3af948db4671baf73f14bced123bae3112f3/init-editing-utils.el#L147
  (define-key evil-insert-state-map "\C-u"
    (lambda ()
      (interactive)
      (let ((prev-pos (point)))
          (back-to-indentation)
          (kill-region (point) prev-pos))))

  ;; (evil-define-key 'normal clojure-mode-map (kbd "C-x") 'cider-eval-last-sexp)

  ;; (evil-define-key 'insert cider-mode-map (kbd "M-up") )

  ; Use C-] to jump to definition in ruby
  (evil-define-key 'normal ruby-mode-map (kbd "C-]") 'robe-jump)

  (define-key evil-normal-state-map "gh" 'windmove-left)
  (define-key evil-normal-state-map "gj" 'windmove-down)
  (define-key evil-normal-state-map "gk" 'windmove-up)
  (define-key evil-normal-state-map "gl" 'windmove-right)

  ; Suspend emacs on C-z in normal mode (this is mapped to C-x C-z by default)
  (define-key evil-normal-state-map "\C-z" 'suspend-frame)

  ; Unimpaired - Adding some of the keybindings from vim-unimpaired
  ; TODO: Accept count so you can do "5 [ SPC" to insert 5 empty lines

  (define-key evil-normal-state-map (kbd "[ SPC") (lambda() (interactive)(evil-insert-newline-above) (forward-line)))
  (define-key evil-normal-state-map (kbd "] SPC") (lambda() (interactive)(evil-insert-newline-below) (forward-line -1)))

  (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "] b") 'next-buffer)

  ;; escape should quit everywhere
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  :ensure t)


;; HAML syntax highlighting
(use-package haml-mode :defer t :ensure t)


;; Helm allows fuzzy autocomplete for interactions requiring selecting an item
;; from many possible choices.
;; Detailed tutorial: http://tuhdo.github.io/helm-intro.html
;; Related packages:
;; - helm-ag package allows use of helm-projectile-ag for project wide search.
;;   <leader>f is used for the project search.
(use-package helm
  :init
  ; Tramp sometimes messes with helm while it tries to figure out SSH/DNS
  ; settings. This will avoid the issues.
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  (setq helm-M-x-fuzzy-match t)

  (use-package helm-ag :defer t :ensure t)

  :config
  (require 'helm-config)

  (global-set-key (kbd "M-x") 'helm-M-x)

  :defer t
  :ensure t)


(use-package idris-mode
  :defer t
  :ensure t)


;; Set relative numbering for line numbers. The actual line number is shown
;; for the current line, but all other lines are shown relative to current.
(use-package linum-relative
  :init
  (setq
    linum-relative-current-symbol ""
    linum-relative-format "%3s "
    linum-delay t)

  :config
  (linum-relative-global-mode)
  (linum-relative-in-helm-p) ; Disable linum in helm

  :diminish 'linum-relative-mode
  :ensure t)


;; Awesome git interface inside emacs.
;; <leader>gs - Git status
(use-package magit :defer t :ensure t)


;; Markdown support
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :init (setq markdown-command "multimarkdown")
  :ensure t)


;; macOS clipboard integration - Makes yanked text available in system
;; clipboard and vice versa. This works automatically in GUI emacs, but this
;; package makes it available in terminal emacs too.
(use-package pbcopy :ensure t)


;; Project interaction library for emacs. Allows doing things like jump to
;; file in project or project-wide search.
;; Related packages:
;; - helm-projectile: adds helm integration for projectile.
;; - projectile-rails: Provides useful Ex commands like find model/controller etc.
;;   Supports projectile-completion-system, allowing use of evil-leader
;;   keybinding <leader>s for toggling between code and test.
(use-package projectile
  :diminish projectile-mode

  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (setq projectile-create-missing-test-files t)
    (helm-projectile-on)
    :ensure t)

  (use-package projectile-rails :ensure t)

  (projectile-mode t)

  ; TODO: Figure out a better way to make projectile load quickly.
  ; projectile takes almost 500ms to load, so defer loading until 10ms after
  ; start up so emacs feels snappier. Deferring for 1s occassionally leads to
  ; projectile not being available when I start using it immediately after
  ; opening emacs.
  :defer 0.01
  :ensure t)


;; Allows powerful jump-to-method and other conveniences for Ruby.
(use-package robe
  :init (add-hook 'ruby-mode-hook 'robe-mode)
  :defer t
  :ensure t)


;; When you visit a file, point goes to the last place where it was when you
;; previously visited the same file.
(use-package saveplace
  :init (setq save-place-file "~/.emacs.d/saved-places")
  :config (setq-default save-place t)
  :ensure t)


;; Balance parentheses and easily navigate S-expressions.
(use-package paredit
  :init
  (use-package evil-paredit
    :init (add-hook 'paredit-mode-hook 'evil-paredit-mode)
    :ensure t)

  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

  :diminish "(p)"
  :ensure t)

;; Highlights delimiters such as parentheses, brackets or braces according to
;; their depth.
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :defer t
  :ensure t)


;; Standard ML
(use-package sml-mode
  :defer t
  :ensure t)


;; Always keep a few lines visible for context when scrolling
(use-package smooth-scrolling
  :config (smooth-scrolling-mode 1)
  :ensure t)


;; Enable ujelly-theme - I use jellybean theme with vim, and ujelly is the
;; closest one I've found for emacs.
(use-package ujelly-theme :ensure t)


;; undo-tree-mode is available by deefault. This explicit mention is to hide the
;; mode from the modeline using diminish.
(use-package undo-tree :diminish undo-tree-mode)


;; Supports highlighting for HTML, CSS, erb etc.
(use-package web-mode :defer t :ensure t)


;; Distraction free writing mode
(use-package writeroom-mode :defer t :ensure t)


;; Code folding
;; <leader>yy toggles all elements
;; <leader>yt toggles current element
(use-package yafolding
  :init (add-hook 'prog-mode-hook 'yafolding-mode)
  :defer t
  :ensure t)

;; Insert snippets with TAB.
(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'markdown-mode-hook #'yas-minor-mode)

  :config
  (yas-reload-all)

  :diminish 'yas-minor-mode
  :defer t
  :ensure t)


; Now that emacs has started up, we can set GC threshold to 50MB.
(setq gc-cons-threshold 50000000)
