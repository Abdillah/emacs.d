;;; Uncomment the modules you'd like to use and restart Prelude afterwards

;; Official module
(require 'ibuffer)
(defun ibuffer-group-buffer ()
  "Personal `ibuffer` grouping function."
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("c/c++" (or (mode . c-mode)
                              (mode . c++-mode)))
                 ("web" (mode . web-mode))
                 ("emacs-lisp" (mode . emacs-lisp-mode))
                 ("terminal" (mode . shell-mode))
;;                 ("emacs.d" (filename . (expand-file-name "~/.emacs.d/")))
                 ("Help" (or (mode . Man-mode)
                             (mode . woman-mode)
                             (mode . Info-mode)
                             (mode . Help-mode)
                             (mode . help-mode)))
                 ("Emacs Internal" (or (name . "^\\*scratch\\*$")
                                       (name . "^\\*Messages\\*$")
                                       (name . "^\\*Backtrace\\*$")
                                       (name . "^\\*Compile-Log\\*$")
                                       (name . "^\\*SPEEDBAR\\*$")
                                       (name . "^\\*Ibuffer\\*$")
                                       (name . "^\\*Buffer List\\*$")
                                       (name . "^\\*Completions\\*$")))
                 ("Setting" (or (name . "^\\*Customize Option*\\*$")))
                 ("erc" (mode . erc-mode))
                 ("dired" (mode . dired-mode))
                 ("perl" (mode . cperl-mode))
                 ("planner" (or
                             (name . "^\\*Calendar\\*$")
                             (name . "^diary$")
                             (mode . muse-mode)))
                 ("gnus" (or
                          (mode . message-mode)
                          (mode . bbdb-mode)
                          (mode . mail-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)
                          (name . "^\\.bbdb$")
                          (name . "^\\.newsrc-dribble")))))))
  (ibuffer-switch-to-saved-filter-groups "default")

  (ibuffer-auto-mode 1))

(add-hook 'ibuffer-mode-hook 'ibuffer-group-buffer) ; Add above grouping function

(setq ibuffer-show-empty-filter-groups nil)         ; Hide empty group

;; (require 'desktop-mode)
;; (setq desktop-buffers-not-to-save
;;       (concat "\\("
;;               "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;;               "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
;;               "\\)$"))
;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
;; (add-to-list 'desktop-modes-not-to-save 'Info-mode)
;; (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;; (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; Personal modules
(require 'prelude-modeline)
(require 'files-mod)
(require 'uniquify)


;; Man-made modules
(package-initialize)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-css-colorization t)

(setq web-mode-ac-sources-alist
      '(("php"  . (ac-source-words-in-buffer
                   ac-source-words-in-same-mode-buffers
                   ac-source-dictionary))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))
        ("css"  . (ac-source-words-in-buffer ac-source-css-property))))

;; (add-hook 'web-mode-before-auto-complete-hooks
;;           '(lambda ()
;;              (let ((web-mode-cur-language (web-mode-language-at-pos)))
;;                (if (string= web-mode-cur-language "php")
;;                    (yas-activate-extra-mode 'php-mode)
;;                  (yas-deactivate-extra-mode 'php-mode))
;;                (if (string= web-mode-cur-language "css")
;;                    (setq emmet-use-css-transform t)
;;                  (setq emmet-use-css-transform nil)))))

(require 'multi-term)

(require 'ergoemacs-mode)
(global-set-key (kbd "M-4") 'split-window-right)
(global-set-key (kbd "M-$") 'split-window-below)

(ergoemacs-mode 1)

(require 'hl-line+)
(toggle-hl-line-when-idle 1)

(require 'strip-trailing-space+)

(require 'sr-speedbar)

(require 'workgroups2)

(setq wg-session-file "~/.emacs.d/config/workgroups")
(setq wg-prefix-key (kbd "C-c z"))

;; (add-hook 'wg-before-switch-to-workgroup-hook 'sr-speedbar-close)
;; (add-hook 'wg-after-switch-to-workgroup-hook 'sr-speedbar-open)
;; (add-hook 'wg-before-switch-to-workgroup-hook 'ibuffer)
;; (setq wg-emacs-exit-save-behavior 'save)
; (setq wg-mode-line-on nil)

(workgroups-mode 1)

;(require 'persp-mode)
;(add-to-list 'speedbar-frame-parameters (cons 'persp-ignore-wconf t))
;(global-set-key (kbd "C-x b") #'(lambda (arg)
;                                  (interactive "P")
;                                  (with-persp-buffer-list () (ibuffer))))

;; (require 'auto-complete-config)
;; (ac-config-default)

;; (defun ac-web-mode-candidates ()
;;   "Pick the right set of candidates based on position of point context."
;;   (let (cur-web-mode-lang (web-mode-language-at-pos))
;;     (cond ((string= cur-web-mode-lang "php")
;;            (ac-mode-dictionary 'php-mode))
;;           ((string= cur-web-mode-lang "css")
;;            css-property-ids)
;;           ((string= cur-web-mode-lang "html")
;;            '("div" "script" "testing")))
;;     )
;;   )

;; (ac-define-source web-mode
;;   '((candidates . ac-web-mode-candidates)))

;; (add-to-list 'ac-modes 'web-mode)
