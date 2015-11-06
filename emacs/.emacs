(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(ansi-term-color-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(cursor-in-non-selected-windows nil)
 '(custom-safe-themes
   (quote
    ("36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(desktop-path (quote ("~/.emacs.d/" "~")))
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(fci-rule-color "#383838")
 '(flycheck-ruby-rubocop-executable "/Users/shaddox/.rbenv/shims/rubocop")
 '(highlight-symbol-idle-delay 0)
 '(highlight-symbol-on-navigation-p t)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(mumamo-chunk-coloring 1)
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(scroll-step 16)
 '(show-paren-mode t)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tabkey2-mode nil)
 '(tool-bar-mode nil)
 '(web-mode-comment-style 2)
 '(yas-global-mode t))
;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default ((t (:inherit nil :stipple nil :background "#2e3434" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "microsoft" :family "Consolas")))))

(add-to-list 'load-path "~/dotfiles/emacs/.emacs.d")

(load "baddox_emacs.el")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-symbol-face ((t (:background "selectedMenuItemColor")))))
(put 'downcase-region 'disabled nil)
