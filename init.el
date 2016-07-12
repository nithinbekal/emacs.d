

; package.el is the built-in package manager included in emacs.
; By default it uses the GNU ELPA repository to look for packages.
; GNU ELPA contains a very small number of packages, so we add MELPA, where
; most of the popular packages are available.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

