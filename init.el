

; package.el is the built-in package manager included in emacs.
; By default it uses the GNU ELPA repository to look for packages.
; GNU ELPA contains a very small number of packages, so we add MELPA, where
; most of the popular packages are available.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

; Add all the packages you want to install in the packages-list variable.
; When emacs starts, package.el will look for packages that are not installed
; yet and install them.

(defvar packages-list
  '(
    evil
    helm
    ujelly-theme
    )
  "List of packages that are installed on first startup.")

(dolist (p packages-list)
  (unless (package-installed-p p)
    (package-install p)))

; Enable ujelly-theme - I use jellybean theme with vim, and ujelly is the
; closest one I've found for emacs.

(require 'ujelly-theme)

; Disable the menu bar at the top.
(menu-bar-mode -1)

; Enable evil-mode
(require 'evil)
(evil-mode 1)

; Helm allows fuzzy autocomplete for interactions requiring selecting an item
; from many possible choices.
; Detailed tutorial: http://tuhdo.github.io/helm-intro.html

; Tramp sometimes messes with helm while it tries to figure out SSH/DNS
; settings. This will avoid the issues.
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(require 'helm)
(require 'helm-config)
(helm-mode 1)

