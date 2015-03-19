;;; Uncomment the modules you'd like to use and restart Prelude afterwards

;;
;; Official module
;;
(require 'delsel)
(delete-selection-mode 1)

(require 'ibuffer)
(defun ibuffer-group-buffer ()
  "Personal `ibuffer` grouping function."
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("c/c++" (or (mode . c-mode)
                              (mode . c++-mode)))
                 ("vala" (mode . vala-mode))
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

;;; Tabbar Mode
(require 'tabbar)
(tabbar-mode 1)
(setq tabbar-cycle-scope 'tab)
(global-set-key (kbd "C-x C-<next>") 'tabbar-forward-group)
(global-set-key (kbd "C-x C-<prior>") 'tabbar-backward-group)
(global-set-key (kbd "C-x C-<right>") 'tabbar-forward-tab)
(global-set-key (kbd "C-x C-<left>") 'tabbar-backward-tab)
(global-set-key (kbd "C-x <right>") 'tabbar-forward-tab)      ; alias
(global-set-key (kbd "C-x <left>") 'tabbar-backward-tab)      ; alias

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
     Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ;; ((member (buffer-name)
    ;;          '("*scratch*" "*Messages*" "*Help*"))
    ;;  "Common"
    ;;  )
    ((member (buffer-name)
             '("xyz" "day" "m3" "abi" "for" "nws" "eng" "f_g" "tim" "tmp"))
     "Main"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Emacs Internal"
     )
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Internal"
     )
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail"
     )
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the header line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

(defun tabbar-add-tab (tabset object &optional append_ignored)
 "Add to TABSET a tab with value OBJECT if there isn't one there yet.
If the tab is added, it is added at the beginning of the tab list,
unless the optional argument APPEND is non-nil, in which case it is
added at the end."
 (let ((tabs (tabbar-tabs tabset)))
   (if (tabbar-get-tab object tabset)
       tabs
     (let ((tab (tabbar-make-tab object tabset)))
       (tabbar-set-template tabset nil)
       (set tabset (sort (cons tab tabs)
                         (lambda (a b) (string< (buffer-name (car a)) (buffer-name (car b))))))))))

;;; Auto-complete Mode
(require 'auto-complete)

;; Used only for C like mode
(defun docu-args (symbol)
  ""
  (setq docs (semantic-ia-show-summary (point)))
  docs)

(defvar ac-source-function-arg
  '((candidate . (list "alinea" "bemiu" "xaimky"))
;;    (prefix . "(")
    (document . docu-args)
    (symbol . "k")
    ))

(add-hook 'c-mode-common-hook '(lambda ()

          ;; ac-omni-completion-sources is made buffer local so
          ;; you need to add it to a mode hook to activate on
          ;; whatever buffer you want to use it with.  This
          ;; example uses C mode (as you probably surmised).

          ;; auto-complete.el expects ac-omni-completion-sources to be
          ;; a list of cons cells where each cell's car is a regex
          ;; that describes the syntactical bits you want AutoComplete
          ;; to be aware of. The cdr of each cell is the source that will
          ;; supply the completion data.  The following tells autocomplete
          ;; to begin completion when you type in a . or a ->

          (add-to-list 'ac-omni-completion-sources
                       (cons "\\." '(ac-source-semantic)))
          (add-to-list 'ac-omni-completion-sources
                       (cons "->" '(ac-source-semantic)))

          ;; ac-sources was also made buffer local in new versions of
          ;; autocomplete.  In my case, I want AutoComplete to use
          ;; semantic and yasnippet (order matters, if reversed snippets
          ;; will appear before semantic tag completions).

          (add-to-list 'ac-sources ac-source-semantic)
          (add-to-list 'ac-sources ac-source-yasnippet)
          ))

(ac-config-default)
(auto-complete-mode 1)


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

;;
;; Personal modules
;;
(require 'soft-tab-nav)
(define-key global-map (kbd "<right>") 'forward-char-skip-indent-group) ; Hack of spaces-as-tab-mode
(define-key global-map (kbd "<left>") 'backward-char-skip-indent-group) ; Hack of spaces-as-tab-mode
(add-hook 'after-change-major-mode-hook
          'detect-block-indentation)

(require 'prelude-modeline)
(require 'files-mod)
(require 'uniquify)

;;
;; Man-made modules
;;
(package-initialize)

(require 'multi-term)

(require 'ergoemacs-mode)
(global-set-key (kbd "M-4") 'split-window-right)
(global-set-key (kbd "M-$") 'split-window-below)
(global-set-key (kbd "C-S-f") 'isearch-backward)
(global-set-key (kbd "M-Y") 'occur)

(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-J"))

(define-key isearch-mode-map [remap isearch-occur] 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-M-y") 'isearch-occur)

(ergoemacs-mode 1)

(require 'nlinum)
(set-face-attribute 'linum nil :height 100)
(nlinum-mode 1)

(require 'hl-line+)
(toggle-hl-line-when-idle 1)

(require 'strip-trailing-space+)

(require 'sr-speedbar)

(require 'workgroups2)
(setq wg-session-file "~/.emacs.d/config/workgroups")
(setq wg-prefix-key (kbd "C-c z"))
(setq wg-emacs-exit-save-behavior nil)
;; (add-hook 'wg-before-switch-to-workgroup-hook 'sr-speedbar-close)
;; (add-hook 'wg-after-switch-to-workgroup-hook 'sr-speedbar-open)
;; (add-hook 'wg-before-switch-to-workgroup-hook 'ibuffer)
;; (setq wg-emacs-exit-save-behavior 'save)
; (setq wg-mode-line-on nil)

(workgroups-mode 1)

;;; Smartparens setting
(require 'smartparens)
(smartparens-global-mode 1)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)  ; SP disable tick (') pairing
(sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)  ; SP disable tick (') pairing
(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)  ; SP disable btick (`) pairing
(sp-local-pair 'lisp-interaction-mode "`" nil :actions nil)  ; SP disable btick (`) pairing
(setq sp-autoescape-string-quote nil)                  ; SP turn in-string escaper off

;; Show paren pair
(show-smartparens-mode 1)

;; Disable electricity
(electric-pair-mode 0)

(cua-mode 1)
(unless (fboundp 'cua-replace-region)
  (defun cua-replace-region ()
    "Replace the active region with the character you type."
    (interactive)
    (let ((not-empty (and cua-delete-selection (cua-delete-region))))
      (unless (eq this-original-command this-command)
        (let ((overwrite-mode
               (and overwrite-mode
                    not-empty
                    (not (eq this-original-command 'self-insert-command)))))
          (cua--fallback))))))


;;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;; Yasnippets
(require 'yasnippet)
(add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'html-mode)))
(add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'js-mode)))
(add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'css-mode)))
(yas-global-mode 1)

(require 'vala-mode)
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

;;; Web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))

;; (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; (setq web-mode-enable-auto-pairing t)
;; (setq web-mode-enable-comment-keywords t)
;; (setq web-mode-enable-css-colorization t)

;; (setq web-mode-ac-sources-alist
;;       '(("php"  . (ac-source-words-in-buffer
;;                    ac-source-words-in-same-mode-buffers
;;                    ac-source-dictionary))
;;         ("html" . (ac-source-words-in-buffer ac-source-abbrev))
;;         ("css"  . (ac-source-words-in-buffer ac-source-css-property))))

;; (defadvice previous-line (after web-mode-update-standard-indent (&rest arg))
;;   "Insert an empty line when moving up from the top line."
;;   (progn (setplist 'plist (web-mode-point-context (point)))
;;          (setq standard-indent (get 'plist ':indent-offset))))

;; (defadvice next-line (after web-mode-update-standard-indent (&rest arg))
;;   "Insert an empty line when moving up from the top line."
;;   (progn (setplist 'plist (web-mode-point-context (point)))
;;          (setq standard-indent (get 'plist ':indent-offset))))

;; (defun web-mode-line-nav-advice ()
;;   "Activate advice for previous-line and next-line function."
;;   (ad-activate 'previous-line)
;;   (ad-activate 'next-line))

;; (defun web-mode-modify-key ()
;;   "Change key sequence while in `web-mode'"
;;   (define-key global-map (kbd "C-S-e") 'web-mode-comment-or-uncomment))

;; (add-hook 'web-mode-hook
;;           'web-mode-line-nav-advice)
;; (add-hook 'web-mode-hook
;;           'web-mode-modify-key)

;; (add-hook 'web-mode-before-auto-complete-hooks
;;           '(lambda ()
;;              (let ((web-mode-cur-language (web-mode-language-at-pos)))
;;                (if (string= web-mode-cur-language "php")
;;                    (yas-activate-extra-mode 'php-mode)
;;                  (yas-deactivate-extra-mode 'php-mode))
;;                (if (string= web-mode-cur-language "css")
;;                    (setq emmet-use-css-transform t)
;;                  (setq emmet-use-css-transform nil)))))

;;; Js2-mode IDE
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun html-mode-autoclose-tag (&rest r)
  "Add `html-mode-tag-close' function into insertion `post-self-insert-hook'"
  (setq sgml-quick-keys 'close))

(add-hook 'html-mode-hook 'html-mode-autoclose-tag)

;;; Multi-web-mode
;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;                   (js-mode "<script ?\\(type=\"text/javascript\"\\|language=\"javascript\"\\)?[^>]*>" "</script>")
;;                   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)

;; MMM mode
(require 'mmm-auto)
(require 'mmm-html-css-js)
(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)
(mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-js)
(mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-css)
(mmm-add-mode-ext-class 'html-mode "\\.x?html?\\'" 'html-js)
(mmm-add-mode-ext-class 'html-mode "\\.x?html?\\'" 'html-css)
(mmm-add-mode-ext-class 'html-mode "\\.x?html?\\'" 'html-php)

;;; PHP mode
(require 'php-mode)

;; To use abbrev-mode, add lines like this:
(add-hook 'php-mode-hook
          '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))










;; [TRASH]
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
