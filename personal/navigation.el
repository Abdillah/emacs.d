;;;; Navigation editor

;; Re-enable arrow keys
(setq prelude-guru nil)
(require 'spaces-as-tab-mode)

;; Code: Scroll Mode
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ; one line at a time    
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
(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)

(define-key global-map (kbd "<right>") 'forward-char-to-tab-stop)
(define-key global-map (kbd "<left>") 'backward-char-to-tab-stop)
