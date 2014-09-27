;;; global-persona --- global personal preference

;;; Commentary:

;;; Code:

(setq inhibit-startup-message t)
(setq config-changed-for-menu nil)

(tool-bar-mode 0)
(ergoemacs-mode 1)

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

(refresh-mode-menu)

;; Hooks setting
(add-hook 'emacs-startup-hook 'reactivate-menu) ; Refresh Buffer custom menu
(add-hook 'after-init-hook 'reactivate-menu)    ; Alternative (worked one) of above

(add-hook 'start-shift-selection-hook 'reactivate-menu)  ; Alternative (worked one) of above
(add-hook 'end-shift-selection-hook 'reactivate-menu)    ; Alternative (worked one) of above

(add-hook 'change-major-mode-hook 'reactivate-menu)


(add-hook 'window-configuration-change-hook 'config-change-indirect-callback)
(add-hook 'menu-bar-update-hook 'refresh-mode-menu)
(add-hook 'first-change-hook 'reactivate-menu)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'personal-global)
;;; global.el ends here
