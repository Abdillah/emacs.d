;;;; Prelude editor custom property

;; Set spellcheck off
(setq prelude-flyspell nil)

(defun cursor-shape-hook ()
  (if (equal (thing-at-point 'line) "\n") ;; condition
      (setq-default cursor-type 'bar)))

(add-hook 'post-command-hook 
          'cursor-shape-hook)


(setq prelude-whitespace nil) ; turn off whitespace visualization

;; Code: Cursor Mode
; (set-default cursor-type 'bar)

;; Code: Programming Adjustment
(cua-mode 1)
; (cua-selection-mode t)
(setq cua-keep-region-after-copy t)

; Indent using spaces
(setq-default indent-tabs-mode nil)

(global-linum-mode 1)
(column-number-mode 1)
; (transient-mark-mode 1)
(setq-default fill-column 100)
(setq-default tab-width 4)
; (electric-pair-mode 1)
(show-paren-mode 1)
