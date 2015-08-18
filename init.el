
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(defvar my-packages
  '(
     evil
     evil-leader
     evil-rails
     magit
     relative-line-numbers
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
  "e" 'find-file
  "q" 'kill-buffer)
