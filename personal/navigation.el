;;; navigation-persona --- nav personal preference

;;; Commentary:

;;; Code:
(require 'spaces-as-tab-mode)

(setq prelude-guru nil) ; Re-enable arrow keys

;; Code: Scroll Mode
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ; two line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ; scroll window under mouse

(setq scroll-step 1)               ; keyboard scroll one line at a time
(setq scroll-conservatively 10000) 

(setq scroll-margin 1
      scroll-conservatively 0)

(setq-default scroll-up-aggressively 0.01
			  scroll-down-aggressively 0.01)

; (setq delete-selection-mode 0)

;; Code: Key Binding
(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill) ; Shift + click adjust selection

(define-key global-map (kbd "<right>") 'forward-char-to-tab-stop) ; Hack of spaces-as-tab-mode
(define-key global-map (kbd "<left>") 'backward-char-to-tab-stop) ; Hack of spaces-as-tab-mode

(define-key global-map (kbd "C-x C-b") 'buffer-menu) ; In frame buffer-list

(define-key global-map (kbd "<f12>") 'tool-bar-mode) ; Toolbar toggle appearance

(provide 'personal-navigation)
;;; navigation.el ends here
