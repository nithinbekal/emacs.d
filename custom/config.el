
;;; Code:

(setq make-backup-files nil)
(menu-bar-mode -1)

(smex-initialize)

(projectile-mode t)

;; Show projectile lists by most recently active
(setq projectile-sort-order (quote recently-active))

(setq ido-decorations (quote ("\n↪ "     "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(require 'flx-ido)
(require 'ido-vertical-mode)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)

;; Parens handling
;; Show and create matching parens automatically
(show-paren-mode t)
(smartparens-global-mode t)
(show-smartparens-global-mode nil)
(setq sp-autoescape-string-quote nil)
;; Do not highlight paren area
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)
;; Do not use default slight delay
(setq show-paren-delay 0)

(defadvice save-buffer (before save-buffer-always activate)
  "always save buffer"
  (set-buffer-modified-p t))

;; =============================================================================
;; Evil
;; =============================================================================

(setq evil-toggle-key "")

(require 'evil)
(evil-mode 1)

(global-evil-visualstar-mode 1)
(progn (setq evil-default-state 'normal)
       (setq evil-auto-indent t)
       (setq evil-shift-width 2)
       (setq evil-search-wrap t)
       (setq evil-find-skip-newlines t)
       (setq evil-move-cursor-back nil)
       (setq evil-mode-line-format 'before)
       (setq evil-esc-delay 0.001)
       (setq evil-cross-lines t))

(setq evil-overriding-maps nil)
(setq evil-intercept-maps nil)

;; Don't wait for any other keys after escape is pressed.
(setq evil-esc-delay 0)

;; Make HJKL keys work in special buffers
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings occur-mode 'emacs)

(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "." 'find-tag
  "t" 'projectile-find-file
  "b" 'ido-switch-buffer
  "cc" 'evilnc-comment-or-uncomment-lines
  "ag" 'projectile-ag
  "," 'switch-to-previous-buffer
  "gb" 'mo-git-blame-current
  "gL" 'magit-log
  "gs" 'magit-status
  "w"  'kill-buffer
  "nn" 'neotree-toggle
  "nf" 'neotree-find
  "gk" 'windmove-up
  "gj" 'windmove-down
  "gl" 'windmove-right
  "gh" 'windmove-left
  "rv" 'projectile-rails-find-current-view
  "vs" 'split-window-right
  "hs" 'split-window-below
  "x" 'smex)

;; =============================================================================
;; Evil Packages
;; =============================================================================

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-visualstar)

(defun fix-underscore-word ()
  (modify-syntax-entry ?_ "w"))

(defun buffer-exists (bufname)   (not (eq nil (get-buffer bufname))))
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  ;; Don't switch back to the ibuffer!!!
  (if (buffer-exists "*Ibuffer*")  (kill-buffer "*Ibuffer*"))
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; =============================================================================
;; Evil Bindings
;; =============================================================================
(define-key evil-normal-state-map (kbd "C-j") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-scroll-up)

;; Make ";" behave like ":" in normal mode
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-motion-state-map (kbd ";") 'evil-ex)

;; Yank whole buffer
(define-key evil-normal-state-map (kbd "gy") (kbd "gg v G y"))

(setq key-chord-two-keys-delay 0.075)
(key-chord-mode 1)
;; Hack to fix eldoc errors when `jk` is triggered
(eldoc-mode 1)
(eldoc-mode 0)
;; end hack
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "JK" 'evil-normal-state)
(key-chord-define evil-insert-state-map "Jk" 'evil-normal-state)

(define-key evil-insert-state-map "j" #'cofi/maybe-exit-j)
(define-key evil-insert-state-map "J" #'cofi/maybe-exit-J)
(evil-define-command cofi/maybe-exit-j ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "" ?k)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
         (delete-char -1)
         (set-buffer-modified-p modified)
         (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

(evil-define-command cofi/maybe-exit-J ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "J")
    (let ((evt (read-event (format "" ?k)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
         (delete-char -1)
         (set-buffer-modified-p modified)
         (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

(define-key evil-normal-state-map "gh" 'windmove-left)
(define-key evil-normal-state-map "gj" 'windmove-down)
(define-key evil-normal-state-map "gk" 'windmove-up)
(define-key evil-normal-state-map "gl" 'windmove-right)

(add-hook 'neotree-mode-hook
 (lambda ()
   (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
   (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
   (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
   (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
         (define-key evil-normal-state-local-map (kbd "ma") 'neotree-create-node)
         (define-key evil-normal-state-local-map (kbd "md") 'neotree-delete-node)
         (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
         (define-key evil-normal-state-local-map (kbd "mm") 'neotree-rename-node)
))

;; Map ctrl-j/k to up down in ido selections
(add-hook 'ido-setup-hook
  (lambda ()
    (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
))


;; =============================================================================
;; UI
;; =============================================================================

(global-relative-line-numbers-mode)

(defun padded-line-numbers (offset)
    (format "%2d " (abs offset)))

(setq relative-line-numbers-format #'padded-line-numbers)

(setq-default truncate-lines t)

(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd " w) line) 'face 'linum)))

(setq linum-format 'linum-format-func)
;; use customized linum-format: add a addition space after the line number

;; Remember the cursor position of files when reopening them
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; show the column number in the status bar
(column-number-mode t)

;; Powerline
(require 'powerline)
(powerline-vim-theme)

;; Highlight cursor line
(global-hl-line-mode t)
(set-face-background hl-line-face "gray10")

;; Make lines longer than 80 highlighted
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode t)

(add-hook 'prog-mode-hook 'whitespace-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'smooth-scrolling)
(setq smooth-scroll-margin 3)
;; Delay updates to give Emacs a chance for other changes
(setq linum-delay t)
(setq redisplay-dont-pause t)

; Auto-indent with the Return key
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Fix cursor
(defun my-send-string-to-terminal (string)
   (unless (display-graphic-p) (send-string-to-terminal string)))

(defun my-evil-terminal-cursor-change ()
   (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
        (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\e]50;CursorShape=1\x7")))
           (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
     (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
          (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
              (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

  (add-hook 'after-make-frame-functions (lambda (frame) (my-evil-terminal-cursor-change)))
  (my-evil-terminal-cursor-change)

(setq inhibit-startup-screen t)

;; =============================================================================
;; Custom Packages
;; =============================================================================

(require 'ujelly-theme)

(add-to-list 'load-path "~/.emacs.d/vendor/longlines/")
(require 'longlines)

(require 'elixir-mode)
(add-to-list 'load-path "~/.emacs.d/vendor/alchemist.el")
(require 'alchemist)

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)

(defun author-mode ()
  (interactive)
  (linum-mode -1)
  (writeroom-mode t)
  (longlines-mode t)
  (flyspell-mode t)
  (turn-off-smartparens-mode)
)

;; I want underscores as part of word in all modes
(modify-syntax-entry (string-to-char "_") "w" (standard-syntax-table))
(modify-syntax-entry (string-to-char "_") "w" text-mode-syntax-table)
(modify-syntax-entry (string-to-char "_") "w" lisp-mode-syntax-table)
(modify-syntax-entry (string-to-char "_") "w" emacs-lisp-mode-syntax-table)
;; (require 'enh-ruby-mode)
(require 'ruby-mode)
(require 'coffee-mode)
(modify-syntax-entry (string-to-char "_") "w" ruby-mode-syntax-table)
(modify-syntax-entry (string-to-char "_") "w" elixir-mode-syntax-table)
(modify-syntax-entry (string-to-char "_") "w" coffee-mode-syntax-table)

;; File handling
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Space indentation - I want tab as two spaces everywhere
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 2)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default css-indent-offset 2)
(setq-default lisp-indent-offset 2)
(setq-default sgml-basic-offset 2)
(setq-default nxml-child-indent 2)

;; (add-hook 'enh-ruby-mode-hook (lambda () (setq evil-shift-width 2)))
(add-hook 'ruby-mode-hook (lambda ()
                            (setq evil-shift-width 2)
                            (setq tab-width 2)))

(add-hook 'elixir-mode-hook (lambda ()
                            (setq evil-shift-width 2)
                            (setq tab-width 2)))

(add-hook 'coffee-mode-hook (lambda ()
                            (setq evil-shift-width 2)
                            (setq tab-width 2)))

(add-hook 'haml-mode-hook (lambda ()
                            (setq evil-shift-width 2)
                            (setq tab-width 2)))

(add-hook 'html-mode-hook (lambda ()
                            (emmet-mode t)
                            (sgml-mode 0)
                            (setq evil-shift-width 2)
                            (setq tab-width 2)))

(add-hook 'css-mode-hook (lambda ()
                            (setq evil-shift-width 2)
                            (setq tab-width 2)))

;; Play nice with evil-mode in compilation-mode, ie project-ag results
(add-hook 'compilation-mode-hook '(lambda ()
                                    (local-unset-key "g")
                                    (local-unset-key "h")
                                    (local-unset-key "k")))

;;==============================================================================
;; Hack "*" to hightlight, but not jump to first match
(defun my-evil-prepare-word-search (forward symbol)
  "Prepare word search, but do not move yet."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (let ((string (car-safe regexp-search-ring))
        (move (if forward #'forward-char #'backward-char))
        (end (if forward #'eobp #'bobp)))
    (setq isearch-forward forward)
    (setq string (evil-find-thing forward (if symbol 'symbol 'word)))
    (cond
     ((null string)
      (error "No word under point"))
     (t
      (setq string
            (format (if symbol "\\_<%s\\_>" "\\<%s\\>")
                    (regexp-quote string)))))
    (evil-push-search-history string forward)
    (my-evil-search string forward t)))

(defun my-evil-search (string forward &optional regexp-p start)
  "Highlight STRING matches.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, STRING is taken to be a regular expression.
START is the position to search from; if unspecified, it is
one more than the current position."
  (when (and (stringp string)
             (not (string= string "")))
    (let* ((orig (point))
           (start (or start
                      (if forward
                          (min (point-max) (1+ orig))
                        orig)))
           (isearch-regexp regexp-p)
           (isearch-forward forward)
           (case-fold-search
            (unless (and search-upper-case
                         (not (isearch-no-upper-case-p string nil)))
              case-fold-search)))
      ;; no text properties, thank you very much
      (set-text-properties 0 (length string) nil string)
      (setq isearch-string string)
      (isearch-update-ring string regexp-p)
      ;; handle opening and closing of invisible area
      (cond
       ((boundp 'isearch-filter-predicates)
        (dolist (pred isearch-filter-predicates)
          (funcall pred (match-beginning 0) (match-end 0))))
       ((boundp 'isearch-filter-predicate)
        (funcall isearch-filter-predicate (match-beginning 0) (match-end 0))))
      (evil-flash-search-pattern string t))))

(define-key evil-motion-state-map "*" 'my-evil-prepare-word-search)
(define-key evil-motion-state-map (kbd "*") 'my-evil-prepare-word-search)
;; end highlight hack
;;==============================================================================


;; Enable syntax highlighting in markdown
(require 'mmm-mode)
  (mmm-add-classes
    '((markdown-rubyp
      :submode ruby-mode
      :face mmm-declaration-submode-face
      :front "^\{:language=\"ruby\"\}[\n\r]+~~~"
      :back "^~~~$")))

  (mmm-add-classes
    '((markdown-elixirp
      :submode elixir-mode
      :face mmm-declaration-submode-face
      :front "^\{:language=\"elixir\"\}[\n\r]+~~~"
      :back "^~~~$")))

  (mmm-add-classes
    '((markdown-elixirp
      :submode elixir-mode
      :face mmm-declaration-submode-face
      :front "^```elixir$"
      :back "^```$")))

  (mmm-add-classes
    '((markdown-jsp
      :submode js-mode
      :face mmm-declaration-submode-face
      :front "^\{:language=\"javascript\"\}[\n\r]+~~~"
      :back "^~~~$")))

  (mmm-add-classes
    '((markdown-ruby
      :submode ruby-mode
      :face mmm-declaration-submode-face
      :front "^~~~\s?ruby[\n\r]"
      :back "^~~~$")))

  (mmm-add-classes
    '((markdown-elixir
      :submode elixir-mode
      :face mmm-declaration-submode-face
      :front "^~~~\s?elixir[\n\r]"
      :back "^~~~$")))

  (mmm-add-classes
    '((markdown-js
      :submode js-mode
      :face mmm-declaration-submode-face
      :front "^~~~\s?javascript[\n\r]"
      :back "^~~~$")))

(setq mmm-submode-decoration-level 0)

(add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-rubyp))
(add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-elixirp))
(add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-jsp))
(add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-ruby))
(add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-elixir))
(add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-js))

(require 'haml-mode)

(provide 'anything-bundle)
