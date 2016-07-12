
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

(require 'use-package)


(use-package evil
  ; Powerfule vim emulation inside emacs.

  :ensure t
  :init

  (use-package evil-leader
    ; Configure leader keys for evil mode.

    :ensure t
    :init (global-evil-leader-mode)

    :config
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "t"  'helm-find-files
      "v"  'split-window-right
      "yb" (kbd "gg v G y")     ; Yank buffer
    ))

  (evil-mode 1))


(use-package helm
  ; Helm allows fuzzy autocomplete for interactions requiring selecting an item
  ; from many possible choices.
  ; Detailed tutorial: http://tuhdo.github.io/helm-intro.html

  :ensure t
  :init
  ; Tramp sometimes messes with helm while it tries to figure out SSH/DNS
  ; settings. This will avoid the issues.
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  (require 'helm-config)
  (helm-mode))


(use-package ujelly-theme
  ; Enable ujelly-theme - I use jellybean theme with vim, and ujelly is the
  ; closest one I've found for emacs.
  :ensure t)

