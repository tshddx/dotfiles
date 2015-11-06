(setq load-path (append load-path (list "~/dotfiles/emacs/.emacs.d/site-lisp")))

(column-number-mode)
(delete-selection-mode)

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

(when (eq system-type 'darwin) ;; mac specific settings
  ;; default font
;;  (set-face-attribute 'default nil :font "Andale Mono")
  (set-default-font "Andale Mono")
  ;; key bindings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
          )

(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Save sessions
(desktop-save-mode 1)

(global-hl-line-mode)
(set-face-background hl-line-face "gray23")

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-markup-indent-offset 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-engines-alist
      '(
        ("ctemplate" . "\\.hbs.erb\\'")
        )
      )




;; -----------------------------
;; KEY BINDINGS and HELPER FUNCTIONS
;; -----------------------------

;; line movement, from http://www.schuerig.de/michael/blog/index.php/2009/01/16/line-movement-for-emacs/
;; (defun move-line-down ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (next-line)
;;       (transpose-lines 1))
;;     (next-line)
;;     (move-to-column col)))

;; (defun move-line-up ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (next-line)
;;       (transpose-lines -1))
;;     (move-to-column col)))

;; (global-set-key "\M-n" 'move-line-down)
;; (global-set-key "\M-p" 'move-line-up)

;; Potentially better move-lines, from https://github.com/targzeta/move-lines
(require 'move-lines)
(move-lines-binding)

; Comment region
;; (global-set-key (kbd "C-c C-c") 'comment-region)
;; (global-set-key (kbd "C-c C-v") 'uncomment-region)

;; Org-mode !!!
;; The following lines are always needed.  Choose your own keys.
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c C-l") 'org-insert-link)
;; (global-set-key (kbd "C-c C-a") 'org-agenda)
;; (global-set-key (kbd "C-c C-b") 'org-iswitchb)

; Make Emacs use "newline-and-indent" when you hit the Enter key so
; that you don't need to keep using TAB to align yourself when coding.
; from http://www-cs-students.stanford.edu/~manku/dotemacs.html
(global-set-key "\C-m" 'newline-and-indent)

;; http://peter.peca.dk/art_Emacs_Ruby_Mode.html
;; Indent when pressing Enter in Ruby-Mode
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))
(setq ruby-deep-indent-paren nil)

(global-set-key "\M-/" 'hippie-expand)
(global-set-key (kbd "C-l") 'kill-whole-line)

(global-set-key (kbd "C-d") 'delete-forward-char); instead of just delete-char, which doesn't delete active region

; Easier underscores, idea courtesy of Snider
;; (global-set-key (kbd "S-SPC") "_")

; Django template tag macros
;; (global-set-key (kbd "C-c b") (lambda () (interactive) (insert "{%  %}") (backward-char 3)))
;; (global-set-key (kbd "C-c v") (lambda () (interactive) (insert "{{  }}") (backward-char 3)))

; Rails view tag macros
(global-set-key (kbd "C-c b") (lambda () (interactive) (insert "<%  %>") (backward-char 3)))
(global-set-key (kbd "C-c v") (lambda () (interactive) (insert "<%=  %>") (backward-char 3)))

; from http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs/88828#88828
;; (defun duplicate-line()
;;   (interactive)
;;   (move-beginning-of-line 1)
;;   (kill-line)
;;   (yank)
;;   (open-line 1)
;;   (next-line 1)
;;   (yank)
;; )
;; (global-set-key (kbd "C-;") 'duplicate-line)

; Potentially better duplicate line/region, from http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(require 'duplicate-current-line-or-region)
(global-set-key (kbd "C-;") 'duplicate-current-line-or-region)

(defun close-element-and-indent ()
  (interactive)
  (web-mode-element-close)
  (indent-for-tab-command)
  )

(add-hook 'web-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c /") 'close-element-and-indent)
            (local-set-key (kbd "C-;") 'duplicate-current-line-or-region) ;; Because web-mode tries to steal this.
            ))

;; Preserve TAB binding and allow C-i, http://stackoverflow.com/a/1792482
;; Seems to screw up things, like tab completion in minibuffer
;; (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
;; (global-set-key (kbd "<tab>") 'indent-for-tab-command)

(global-set-key (kbd "C-S-O") 'previous-multiframe-window)
(global-set-key (kbd "C-o") 'next-multiframe-window)

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

;; define my own plist-to-alist function, since emacs 24 doesn't have it and it's required for color-themes
;; from http://stackoverflow.com/a/14183331
(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))

  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist)))
  alist))


(require 'color-theme)
(require 'color-theme-tangotango)
(eval-after-load "color-theme"
  '(progn
     (eval-after-load "color-theme"
       '(color-theme-tangotango))))

;; (require 'zenburn)
;; (require 'color-theme-gruber-darker)
;; (require 'color-theme-subdued)
;; (require 'color-theme-tomorrow)

;; disabled the color-theme stuff, since emacs 24 has built-in theme support. what follows is the built-in stuff

;; (add-to-list 'custom-theme-load-path "~/dotfiles/emacs/.emacs.d/themes/")
;; (load-theme 'solarized-dark t)

;; http://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer/3669681#3669681
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [f5] 'show-file-name)

;; (require 'autopair)
;; (autopair-global-mode) ;; to enable in all buffers

;; Uniquify buffer names http://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;
;; BEGIN JORGEN, from http://www.emacswiki.org/emacs/JorgenSchaefersEmacsConfig
;;

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

(display-time)

;;
;; END JORGEN
;;

;; https://github.com/ajvargo/ruby-refactor
(require 'ruby-refactor)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

;; https://github.com/nschum/highlight-symbol.el
(require 'highlight-symbol)
(highlight-symbol-mode)

;; Shortcuts for navigating through symbols
(global-set-key (kbd "C-S-S") 'highlight-symbol-next)
(global-set-key (kbd "C-S-R") 'highlight-symbol-prev)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
             ("marmalade" . "http://marmalade-repo.org/packages/")
             ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(define-key projectile-mode-map (kbd "C-x C-f") 'projectile-find-file)
(global-set-key (kbd "A-x A-f") 'ido-find-file)

(ido-mode 1)
(ido-everywhere 1)
(require 'flx-ido)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)


;;
;; JavaScript stuff
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;

;; use web-mode for .jsx files
;(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
;(require 'flycheck)

;; turn on flychecking globally
;(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
;(setq-default flycheck-disabled-checkers
;(append flycheck-disabled-checkers
;    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
;(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
;(setq-default flycheck-disabled-checkers
;  (append flycheck-disabled-checkers
;    '(json-jsonlist)))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable default ruby checker since we prefer rubocop
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(ruby)))

;; use rubocop with ruby-mode for ruby files
(flycheck-add-mode 'ruby-rubocop 'ruby-mode)

(global-set-key (kbd "A-n") 'flycheck-next-error)
(global-set-key (kbd "A-p") 'flycheck-previous-error)

;; make helm window full width at bottom with 40% height
;; https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
(add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.4)))

(defvar grep-find-ignored-files (list))
(defvar grep-find-ignored-directories (list))

;; helm completion for running functions
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
