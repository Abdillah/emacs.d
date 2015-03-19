;;;; Global Section
;;;; -- Config of saving, loading, file backup, buffer, window operation

(setq inhibit-startup-message t
      config-changed-for-menu nil)

(setq frame-title-format "Emacs - %b")

(tool-bar-mode 0) ; Hide toolbar

(setq backup-by-copying t                                  ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/auto-save-list")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)                                   ; use versioned backups

(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; see the doc

;; Native (built-in) hooks configuration
(add-hook 'before-save-hook 'delete-trailing-whitespace-except-current-line)

;;;; Editor Section
;;;; -- Config of file editing operation

(whitespace-mode -1)            ; turn off whitespace visualization
(global-whitespace-mode -1)     ; turn off global whitespace visualization
(column-number-mode 1)          ;
(setq-default cursor-type 'bar) ; set cursor shape
(setq-default fill-column 80)

;; Yank (paste) replace mode
(defadvice yank (before delete-before-yank (&rest arg))
  "A `delete-selection-mode' alternative as it doesn't work now.
It delete text before yank (paste)."
  (if (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end))))
(ad-activate 'yank)

;; Search automatically selected word
(defun isearch-yank-region ()
  "Pull next character, subword or word from buffer into search string.
Subword is used when `subword-mode' is activated. "
  (interactive)
  (if (region-active-p)
      (progn (deactivate-mark)           ; Remove existing mark
             (isearch-yank-internal
              (lambda ()
                (goto-char (region-end))
                (region-beginning))))))
(add-hook 'isearch-mode-hook 'isearch-yank-region)

;;;; Navigation Section
;;;; -- Config of keys and ergonomics enhancement operation

;; Scroll Preference
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ; two line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ; scroll window under mouse

(setq scroll-step 1)               ; keyboard scroll one line at a time
(setq scroll-margin 1)
(setq scroll-conservatively 10)

(setq-default scroll-up-aggressively 0.01
			  scroll-down-aggressively 0.01)

;; Custom word navigation
(defun count-occurences (regex string)
  (recursive-count regex string 0))

(defun recursive-count (regex string start)
  (if (string-match regex string start)
      (+ 1 (recursive-count regex string (match-end 0)))
    0))

(defadvice forward-word (around forward-word-stop-before-word activate)
  "Navigate to the nearest word, stop before it."
  (setq first-point (point))
  ad-do-it
  (setq count (count-occurences "[^A-z]" (buffer-substring first-point (point))))
  (if (> count 2)
      (backward-word 1))
  ;; (message (number-to-string count))
  )
(ad-activate 'forward-word)

(defadvice backward-word (around backward-word-stop-before-word activate)
  "Navigate to the nearest word, stop before it."
  (setq first-point (point))
  ad-do-it
  (setq count (count-occurences "[^A-z]" (buffer-substring first-point (point))))
  (if (> count 2)
      (forward-word 1))
  ;; (message (number-to-string count))
  )
(ad-activate 'backward-word)

;; Comment custom behavior
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

(define-key global-map (kbd "C-S-e") 'comment-or-uncomment-region-or-line) ; In frame buffer-list

;;; #key
;;; Keys assignment in the end to utilize prior function
;; Change native-interfered window keys
(defun window-cmd-prefix ()
  "Prefix in window related command."
  nil)
(define-prefix-command 'window-cmd-prefix)
(global-set-key (kbd "C-x C-w") 'window-cmd-prefix)

;; Minor mode toggle keys
(defun minor-mode-toggle-prefix ()
  "Minor mode toggle prefix."
  nil)
(define-prefix-command 'minor-mode-toggle-prefix)
(global-set-key (kbd "C-x C-i") 'minor-mode-toggle-prefix)

(global-set-key (kbd "C-x C-i c") 'company-mode)
(global-set-key (kbd "C-x C-i p") 'projectile-mode)
(global-set-key (kbd "C-x C-i y") 'yas-minor-mode)
(global-set-key (kbd "C-x C-i s") 'sr-speedbar-launch)

(define-key global-map (kbd "C-x C-b") 'ibuffer)     ; In frame buffer-list
(define-key global-map (kbd "<f12>") 'tool-bar-mode) ; Toolbar toggle appearance

(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill) ; Shift + click adjust selection


(provide 'personal-config)
