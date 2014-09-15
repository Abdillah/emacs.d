;;; editor-persona --- editor personal preference

;;; Commentary:

;;; Code:
; (require 'fill-column-indicator)

(setq prelude-flyspell nil) ; Set spellcheck off

(defun cursor-shape-hook ()
  (if (equal (thing-at-point 'line) "\n") ; condition
      (setq-default cursor-type 'bar)))

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(add-hook 'font-lock-mode-hook 
          'my-doxymacs-font-lock-hook)

(add-hook 'post-command-hook 
          'cursor-shape-hook)

; (add-hook 'post-command-hook 
;           'turn-on-fci-mode)

(setq prelude-whitespace nil) ; turn off whitespace visualization
(whitespace-mode -1)
(global-whitespace-mode -1)

;; Code: Cursor Mode
; (set-default cursor-type 'bar)

;; Code: Programming Adjustment
(cua-mode 1)
; (cua-selection-mode t)
; (setq cua-keep-region-after-copy t)

; (fci-mode 1)

; Indent using spaces
(setq-default indent-tabs-mode nil)

(global-linum-mode 1)
(column-number-mode 1)
; (transient-mark-mode 1)
(setq-default fill-column 80)
(setq-default tab-width 4)
; (electric-pair-mode 1)
(show-paren-mode 1)

; (add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete)
; (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(require 'auto-complete-config)
(ac-config-default)

;; Code: Smartparens setting
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
(setq sp-autoescape-string-quote nil)

;; Code: Buffer name
(setq uniquify-buffer-name-style 'forward)

(provide 'personal-editor)
;;; editor.el ends here
