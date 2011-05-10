(setq load-path (append load-path (list "~/dotfiles/emacs/.emacs.d/site-lisp")))

(column-number-mode)
(delete-selection-mode)

(ido-mode t)
;; from http://emacsblog.org/2008/05/19/giving-ido-mode-a-second-chance/
(setq ido-enable-flex-matching t)

;; ;; Make sure all backup files only live in one place
;; (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Make sure all backup files only live in one place
;; http://amitp.blogspot.com/2007/03/emacs-move-autosave-and-backup-files.html
(defvar user-temporary-file-directory
  "~/.emacs-backup")
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; Global line numbers
(global-linum-mode)

;; -----------------------------
;; KEY BINDINGS and HELPER FUNCTIONS
;; -----------------------------

;; line movement, from http://www.schuerig.de/michael/blog/index.php/2009/01/16/line-movement-for-emacs/
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines 1))
    (next-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key "\M-n" 'move-line-down)
(global-set-key "\M-p" 'move-line-up)

; Comment region
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-v") 'uncomment-region)

;; Org-mode !!!
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "C-c C-a") 'org-agenda)
(global-set-key (kbd "C-c C-b") 'org-iswitchb)

; Make Emacs use "newline-and-indent" when you hit the Enter key so
; that you don't need to keep using TAB to align yourself when coding.
; from http://www-cs-students.stanford.edu/~manku/dotemacs.html
(global-set-key "\C-m" 'newline-and-indent)

;; http://peter.peca.dk/art_Emacs_Ruby_Mode.html
;; Indent when pressing Enter in Ruby-Mode
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))


(global-set-key "\M-/" 'hippie-expand)
(global-set-key (kbd "C-l") 'kill-whole-line)

; Easier underscores, idea courtesy of Snider
(global-set-key (kbd "S-SPC") "_")

; Django template tag macros
(global-set-key (kbd "C-c b") (lambda () (interactive) (insert "{%  %}") (backward-char 3)))
(global-set-key (kbd "C-c v") (lambda () (interactive) (insert "{{  }}") (backward-char 3)))

; Rails view tag macros
;; (global-set-key (kbd "C-c b") (lambda () (interactive) (insert "<%  %>") (backward-char 3)))
;; (global-set-key (kbd "C-c v") (lambda () (interactive) (insert "<%=  %>") (backward-char 3)))

; from http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs/88828#88828
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-;") 'duplicate-line)

;; -----------------------------
;; LOAD OTHER PEOPLE'S STUFF
;; -----------------------------

;; load nXhtml
;; (load "~/.emacs.d/site-lisp/nxhtml/autostart.el")
;; (setq mumamo-background-colors nil) 
;; (add-to-list 'auto-mode-alist '("\\.html$" . html-mumamo-mode))

;; YASNIPPET

(add-to-list 'load-path
             "~/dotfiles/emacs/.emacs.d/site-lisp/yasnippet-0.6.1b")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/dotfiles/emacs/.emacs.d/site-lisp/yasnippet-0.6.1b/snippets")

;; THEME_STUFF

(require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-hober)))

(color-theme-initialize)
(require 'zenburn)
(require 'color-theme-gruber-darker)
(require 'color-theme-tangotango)
(require 'color-theme-subdued)
(color-theme-vim-colors)

(setq my-color-themes (list 'color-theme-vim-colors
                            'color-theme-tangotango
                            'color-theme-zenburn
                            'color-theme-gruber-darker
                            'color-theme-subdued
                            'color-theme-vim-colors
                            ))
;; http://www.emacswiki.org/cgi-bin/wiki/ColorTheme#toc7
;; Hit a key to cycle through my-color-themes
(defun my-theme-set-default () ; Set the first row
  (interactive)
  (setq theme-current my-color-themes)
  (funcall (car theme-current)))

(defun my-describe-theme () ; Show the current theme
  (interactive)
  (message "%s" (car theme-current)))

;; Set the next theme (fixed by Chris Webber - tanks)
(defun my-theme-cycle ()		
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current my-color-themes))
  (funcall (car theme-current))
  (message "%S" (car theme-current)))

(setq theme-current my-color-themes)
;; (setq color-theme-is-global nil) ; Initialization
(my-theme-set-default)
(global-set-key [f12] 'my-theme-cycle)

;; http://rinari.rubyforge.org/Basic-Setup.html#Basic-Setup
;; Rinari
;; (add-to-list 'load-path "~/dotfiles/emacs/.emacs.d/site-lisp/rinari")
;; (require 'rinari)

;; http://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer/3669681#3669681
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [f5] 'show-file-name)

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers