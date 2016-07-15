

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

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; Start GUI emacs maximized
(set-frame-parameter nil 'fullscreen 'maximized)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)


; use-package allows you to load packages lazily, and speeds up the initial
; load time of emacs.

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

; use-package isn't needed at runtime, so this can reduce load time.
(eval-when-compile
  (require 'use-package))


(use-package clojure-mode
  :config
  (use-package cider
    :defer t
    :ensure t)

  :defer t
  :ensure t)


(use-package coffee-mode
  :defer t
  :ensure t)


(use-package dash-at-point
  ; Opens Dash.app to look up documentation for the word at point.
  ; I map <leader>dd using evil-leader to be able to easily access it.
  :ensure t)


(use-package elixir-mode
  ; Syntax highlighting and navigation for Elixir.

  :config
  (use-package alchemist
    ; Elixir tooling support.
    :defer t
    :ensure t)

  :defer t
  :ensure t)


(use-package evil
  ; Powerful vim emulation inside emacs.

  :init

  (use-package evil-mc
    ; Multiple cursor support
    ; C-n/C-p to create cursors
    ; C-t to skip cursor
    ; gru to undo all cursors

    :init
    (global-evil-mc-mode 1)

    :ensure t)

  (use-package evil-leader
    ; Configure leader keys for evil mode.
    ; (global-evil-leader-mode) should be loaded before (evil-mode 1).

    :init (global-evil-leader-mode)

    :config
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      ","  'projectile-find-file
      "dd" 'dash-at-point
      "f"  'helm-projectile-ag
      "gs" 'magit-status
      "q"  'kill-buffer-and-window
      "s"  'projectile-toggle-between-implementation-and-test
      "vs"  'split-window-right

      "vi" (lambda ()
             (interactive)
             (find-file "~/.emacs.d/init.el"))

      "yb" (kbd "gg v G y")     ; Yank buffer
      "yt" 'yafolding-toggle-element
      "yy" 'yafolding-toggle-all

      "gg" (lambda ()
             (interactive)
             (find-file "~/Dropbox/todo/work.md"))

      "gt" (lambda ()
             (interactive)
             (find-file "~/Dropbox/todo/gtd.md"))
      )

    :ensure t)

  (use-package evil-surround
    ; Emacs port of vim-surround.
    ; Allows adding/removing/changing surround characters.
    ; eg. `cs"'` in normal mode changes surround from double to single quotes.

    :config
    (global-evil-surround-mode 1)

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

  :ensure t)


(use-package haml-mode
  ; HAML syntax highlighting
  :defer t
  :ensure t)


(use-package helm
  ; Helm allows fuzzy autocomplete for interactions requiring selecting an item
  ; from many possible choices.
  ; Detailed tutorial: http://tuhdo.github.io/helm-intro.html

  :init
  ; Tramp sometimes messes with helm while it tries to figure out SSH/DNS
  ; settings. This will avoid the issues.
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  (setq helm-M-x-fuzzy-match t)
  (setq helm-candidate-number-limit 100)

  :config
  (require 'helm-config)

  (global-set-key (kbd "M-x") 'helm-M-x)

  (use-package helm-ag
    ; helm-ag package allows use of helm-projectile-ag for project wide search.
    ; <leader>f is used for the project search.
    :defer t
    :ensure t)

  :defer t
  :ensure t)


(use-package linum-relative
  ; Set relative numbering for line numbers. The actual line number is shown
  ; for the current line, but all other lines are shown relative to current.

  :init
  (setq
    linum-relative-current-symbol ""
    linum-relative-format "%3s "
    linum-delay t)

  :config
  (linum-relative-global-mode)
  (linum-relative-in-helm-p) ; Disable linum in helm

  :ensure t)


(use-package magit
  ; Awesome git interface inside emacs.
  ; <leader>gs - Git status
  :defer t
  :ensure t)


(use-package markdown-mode
  ; Markdown support

  :commands (markdown-mode gfm-mode)

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :init
  (setq markdown-command "multimarkdown")

  :ensure t)


(use-package pbcopy
  ; macOS clipboard integration - Makes yanked text available in system
  ; clipboard and vice versa. This works automatically in GUI emacs, but this
  ; package makes it available in terminal emacs too.
  :ensure t)


(use-package projectile
  ; Project interaction library for emacs. Allows doing things like jump to
  ; file in project or project-wide search.

  :config
  (use-package helm-projectile
    ; Helm integration for projectile.

    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)

    :ensure t)

  (use-package projectile-rails
    ; Provides useful Ex commands like find model/controller etc.
    ; Supports projectile-completion-system, allowing use of evil-leader
    ; keybinding <leader>s for toggling between code and test.
    :ensure t)

  (projectile-mode t)

  ; TODO: Figure out a better way to make projectile load quickly.
  ; projectile takes almost 500ms to load, so defer loading until 10ms after
  ; start up so emacs feels snappier. Deferring for 1s occassionally leads to
  ; projectile not being available when I start using it immediately after
  ; opening emacs.
  :defer 0.01
  :ensure t)


(use-package robe
  ; Allows powerful jump-to-method and other conveniences for Ruby.

  :init
  (add-hook 'ruby-mode-hook 'robe-mode)

  :defer t
  :ensure t)


(use-package saveplace
  ; When you visit a file, point goes to the last place where it was when you
  ; previously visited the same file.

  :init
  (setq save-place-file "~/.emacs.d/saved-places")

  :config
  (setq-default save-place t)

  :ensure t)


(use-package smartparens-config
  ; Managing paired characters like parentheses, braces, brackets, quotes, etc.
  ; Tutorial: https://ebzzry.github.io/emacs-pairs.html

  :init
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)

  :config
  (show-smartparens-global-mode t)

  :defer t
  :ensure smartparens)


(use-package evil-smartparens
  :init
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

  :defer t
  :ensure t)


(use-package smooth-scrolling
  ; Always keep a few lines visible for context when scrolling

  :config
  (smooth-scrolling-mode 1)

  :ensure t)


(use-package spaceline-config
  ; Modeline that is used in spacemacs

  :init
  (setq spaceline-window-numbers-unicode t)

  :config
  (spaceline-spacemacs-theme)

  :ensure spaceline)


(use-package ujelly-theme
  ; Enable ujelly-theme - I use jellybean theme with vim, and ujelly is the
  ; closest one I've found for emacs.
  :ensure t)


(use-package web-mode
  ; Supports highlighting for HTML, CSS, erb etc.
  :defer t
  :ensure t)


(use-package yafolding
  ; Code folding
  ; <leader>yy toggles all elements
  ; <leader>yt toggles current element

  :init
  (add-hook 'prog-mode-hook 'yafolding-mode)

  :defer t
  :ensure t)


(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'markdown-mode-hook #'yas-minor-mode)

  :config
  (yas-reload-all)

  :defer t
  :ensure t)


; Now that emacs has started up, we can set GC threshold to 50MB.
(setq gc-cons-threshold 50000000)

