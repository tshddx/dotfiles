
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(ansi-term-color-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(cursor-in-non-selected-windows nil)
 '(custom-enabled-themes (quote (tangotango)))
 '(custom-safe-themes
   (quote
    ("cdfc5c44f19211cfff5994221078d7d5549eeb9feda4f595a2fd8ca40467776c" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(desktop-path (quote ("~/.emacs.d/" "~")))
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(enh-ruby-add-encoding-comment-on-save nil)
 '(enh-ruby-deep-indent-construct nil)
 '(enh-ruby-deep-indent-paren nil)
 '(enh-ruby-encoding-map
   (quote
    ((us-ascii)
     (utf-8)
     (shift-jis . cp932)
     (shift_jis . cp932)
     (japanese-cp932 . cp932))))
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin")))
 '(fci-rule-color "#383838")
 '(flycheck-ruby-rubocop-executable "/Users/shaddox/.rbenv/shims/rubocop")
 '(highlight-symbol-colors (quote ("dark blue")))
 '(highlight-symbol-idle-delay 0)
 '(highlight-symbol-on-navigation-p t)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-strict-missing-semi-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(mumamo-chunk-coloring 1)
 '(package-selected-packages
   (quote
    (zenburn-theme ruby-block enh-ruby-mode less-css-mode rjsx-mode ag helm helm-ag haml-mode tangotango-theme web-mode s projectile highlight-symbol flycheck flx-ido)))
 '(ruby-block-highlight-face (quote show-paren-match))
 '(ruby-block-highlight-toggle (quote overlay))
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(scroll-step 16)
 '(show-paren-mode t)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tabkey2-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-delay 0.4)
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
 '(enh-ruby-op-face ((t (:foreground "#729fcf"))))
 '(erm-syn-errline ((t (:background "#700101"))))
 '(erm-syn-warnline ((t (:background "#504b20"))))
 '(highlight-symbol-face ((t (:background "gray30")))))
(put 'downcase-region 'disabled nil)
