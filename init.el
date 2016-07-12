

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

(defvar packages-list '(ujelly-theme)
  "List of packages that are installed on first startup.")

(dolist (p packages-list)
  (unless (package-installed-p p)
    (package-install p)))

