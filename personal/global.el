
;;; global-persona --- global personal preference

;;; Commentary:

;;; Code:

(setq inhibit-startup-message t)
(setq config-changed-for-menu nil)

(tool-bar-mode 0)

(setq backup-by-copying t                             ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)                              ; use versioned backups

(defun config-change-indirect-callback ()
  "Set `config-change` variable to on, allowing callbacks."
  (setq config-changed t)
  nil)

(defun refresh-mode-menu ()
  "Refresh menu by toggling on and off-ed it."
  (if config-changed-for-menu
      (progn (reactivate-menu)
             (setq config-changed-for-menu nil)))
  nil)

(defun reactivate-menu (&rest loom)
  "Refresh menu by toggling on and off-ed it."
    (set-frame-parameter (selected-frame) 'menu-bar-lines 0)
    (set-frame-parameter (selected-frame) 'menu-bar-lines 1)
  nil)

(defun create-intermediate-dir ()
  "When a buffer saved in deep uncreated dirs, it will try to `mkdir` and construct path to it."
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p dir))
                 (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
        (make-directory dir t))))
  nil)

(add-hook 'write-file-functions 'create-intermediate-dir)

(refresh-mode-menu)

;; Hooks setting
(add-hook 'before-save-hook 'delete-trailing-whitespace-except-current-line)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'emacs-startup-hook 'reactivate-menu) ; Refresh Buffer custom menu
(add-hook 'after-init-hook 'reactivate-menu)    ; Alternative (worked one) of above

(add-hook 'window-configuration-change-hook 'config-change-indirect-callback)
(add-hook 'menu-bar-update-hook 'refresh-mode-menu)

(add-hook 'first-change-hook 'reactivate-menu)
(add-hook 'mouse-leave-buffer-hook 'reactivate-menu)

(add-hook 'start-shift-selection-hook 'reactivate-menu)  ; Alternative (worked one) of above
(add-hook 'end-shift-selection-hook 'reactivate-menu)    ; Alternative (worked one) of above

(add-hook 'change-major-mode-hook 'reactivate-menu)
(add-hook 'post-command-hook 'config-change-indirect-callback)

(defun window-cmd-prefix ()
  "Prefix in window related command."
  nil)

(define-prefix-command 'window-cmd-prefix)
(global-set-key (kbd "C-x C-w") 'window-cmd-prefix)
;; (ergoemacs-key "C-x C-w" 'window-cmd-prefix "Window cmd prefix")
(setq ergoemacs-ignore-prev-global nil)


(provide 'personal-global)
;;; global.el ends here
