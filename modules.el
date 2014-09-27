;;; Uncomment the modules you'd like to use and restart Prelude afterwards

;; Personal modules
(require 'prelude-modeline)
(require 'uniquify)


;; Man-made modules
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-ac-sources-alist
      '(("php"  . (ac-source-words-in-buffer
                   ac-source-words-in-same-mode-buffers
                   ac-source-dictionary))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))
        ("css"  . (ac-source-words-in-buffer ac-source-css-property))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

(require 'multi-term)

(package-initialize)
(require 'ergoemacs-mode)

(require 'auto-complete-config)
(ac-config-default)

(defun ac-web-mode-candidates ()
  "Pick the right set of candidates based on position of point context."
  (let (cur-web-mode-lang (web-mode-language-at-pos))
    (cond ((string= cur-web-mode-lang "php")
           (ac-mode-dictionary 'php-mode))
          ((string= cur-web-mode-lang "css")
           css-property-ids)
          ((string= cur-web-mode-lang "html")
           '("div" "script" "testing")))
    )
  )

(ac-define-source web-mode
  '((candidates . ac-web-mode-candidates)))

(add-to-list 'ac-modes 'web-mode)
