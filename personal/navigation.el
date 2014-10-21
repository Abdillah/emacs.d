;;; navigation-persona --- nav personal preference

;;; Commentary:

;;; Code:
(require 'spaces-as-tab-mode)

;;; Function definition
(defun minor-mode-toggle-prefix ()
  "Minor mode toggle prefix."
  nil)

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

(defun scroll-left-one (&rest rest)
  ""
  (interactive)
  (scroll-left 1)
  nil)

(defun scroll-right-one (&rest rest)
  ""
  (interactive)
  (scroll-right 1)
  nil)

(setq prelude-guru nil) ; Re-enable arrow keys

;; Code: Scroll Mode
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ; two line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ; scroll window under mouse

(setq scroll-step 1)               ; keyboard scroll one line at a time
(setq scroll-conservatively 10)

(setq scroll-margin 1)

(setq-default scroll-up-aggressively 0.01
			  scroll-down-aggressively 0.01)

; (setq delete-selection-mode 0)

;; Key Binding
(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill) ; Shift + click adjust selection
;(define-key global-map (kbd "C-S-x") 'buffer-menu) ; CUA Big Alphabet
;(define-key global-map (kbd "C-S-c") 'copy-region-as-kill) ; CUA Big Alphabet
;(define-key global-map (kbd "C-S-k") 'buffer-menu) ; CUA Big Alphabet


(define-key global-map (kbd "<right>") 'forward-char-to-tab-stop) ; Hack of spaces-as-tab-mode
(define-key global-map (kbd "<left>") 'backward-char-to-tab-stop) ; Hack of spaces-as-tab-mode
;; (define-key global-map (kbd "<mouse-6>") 'scroll-right-one)
;; (define-key global-map (kbd "<mouse-7>") 'scroll-left-one)

(define-key global-map (kbd "C-x C-b") 'ibuffer) ; In frame buffer-list

(define-prefix-command 'minor-mode-toggle-prefix)
(global-set-key (kbd "C-x C-i") 'minor-mode-toggle-prefix)
(global-set-key (kbd "C-x C-i c") 'company-mode)
(global-set-key (kbd "C-x C-i p") 'projectile-mode)
(global-set-key (kbd "C-x C-i y") 'yas-minor-mode)
(global-set-key (kbd "C-x C-i s") 'sr-speedbar-launch)

(define-key global-map (kbd "C-S-e") 'comment-or-uncomment-region-or-line) ; In frame buffer-list

(define-key global-map (kbd "<f12>") 'tool-bar-mode) ; Toolbar toggle appearance


;; Web-mode navigation enhanchement
(defun web-mode-activate-callback ()
  "Called when web-mode activated."
  (local-set-key (kbd "C-c c") 'web-mode-element-close))

(add-hook 'web-mode-hook 'web-mode-activate-callback)

(provide 'personal-navigation)
;;; navigation.el ends here
