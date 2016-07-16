
;; Emacs lisp script to start Jekyll similar to my tmux blogging setup. This is
;; not part of my emacs init file, but needs to be loaded separately. Opens a
;; shell buffer running jekyll server in the background.

;; Usage:
;;   $ emacs -nw --load ~/.emacs.d/jekyll.el

(interactive)
(cd "~/Dropbox/blog")
(pop-to-buffer (get-buffer-create (generate-new-buffer-name "jekyll")))
(shell (current-buffer))
(process-send-string nil "cd ~/Dropbox/blog\n")
(process-send-string nil "jekyll serve\n")
(delete-window)
(projectile-find-file)
