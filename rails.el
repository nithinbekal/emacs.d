
;; Emacs lisp script to start rails server in the background from within emacs.
;; Usage:
;; Run the following command from the root of your rails project.
;;   $ emacs --load ~/.emacs.d/rails.el

(interactive)

(pop-to-buffer (get-buffer-create (generate-new-buffer-name "rails-server")))
(shell (current-buffer))
(process-send-string nil "bundle exec rails s\n")
(delete-window)

(pop-to-buffer (get-buffer-create (generate-new-buffer-name "rails-console")))
(shell (current-buffer))
(process-send-string nil "bundle exec rails console\n")
(delete-window)
