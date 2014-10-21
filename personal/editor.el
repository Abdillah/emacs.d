;;; editor-persona --- editor personal preference

;;; Commentary:

;;; Code:

;; (require 'fill-column-indicator)
;; (require 'auto-complete)
;; (require 'auto-complete-config)

;; (setq prelude-flyspell nil) ; Turn spellcheck off
;; (setq prelude-whitespace nil) ; turn off whitespace visualization

(defun doxymacs-font-lock-fontify ()
  "Doxynacs font lock activation; assigned to hooks."
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(defun font-lock-refresh-when-idle (&rest arg)
  "Refresh font-lock when idle."
  (if (not font-lock-fontified)
      (progn (run-with-idle-timer 5 nil 'font-lock-fontify-buffer)
             (setq-local font-lock-fontified t))
    nil)
  )

;; Editor Hooks
(add-hook 'font-lock-mode-hook
          'doxymacs-font-lock-fontify)

(add-hook 'after-change-functions
          'font-lock-refresh-when-idle)

; (add-hook 'post-command-hook
;           'turn-on-fci-mode)

(whitespace-mode -1)         ; turn off whitespace visualization
(global-whitespace-mode -1)  ; turn off global whitespace visualization

;; Cursor
(setq-default cursor-type 'bar)

;; Programming Adjustment
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
(electric-pair-mode 1)
(show-paren-mode 1)

; (ac-config-default) ; auto-complete default setting

;; Smartparens setting
; (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)  ; SP disable tick (') pairing
; (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)  ; SP disable btick (`) pairing
; (setq sp-autoescape-string-quote nil)                  ; SP turn in-string escaper off

;; Buffer
(setq uniquify-buffer-name-style 'forward)

(provide 'personal-editor)
;;; editor.el ends here
