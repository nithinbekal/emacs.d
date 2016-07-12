
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


; use-package allows you to load packages lazily, and speeds up the initial
; load time of emacs.

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

; use-package isn't needed at runtime, so this can reduce load time.
(eval-when-compile
  (require 'use-package))


(use-package evil
  ; Powerfule vim emulation inside emacs.

  :init

  (use-package evil-leader
    ; Configure leader keys for evil mode.
    ; (global-evil-leader-mode) should be loaded before (evil-mode), which is
    ; why evil-leader is in the init section.

    :init (global-evil-leader-mode)

    :config
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "t"  'projectile-find-file
      "v"  'split-window-right
      "yb" (kbd "gg v G y")     ; Yank buffer
      )

    :ensure t)

  (evil-mode 1)

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

  (projectile-global-mode)

  :ensure t)


(use-package ujelly-theme
  ; Enable ujelly-theme - I use jellybean theme with vim, and ujelly is the
  ; closest one I've found for emacs.
  :ensure t)

