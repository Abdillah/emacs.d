;;; global-persona --- global personal preference

;;; Commentary:

;;; Code:
(setq inhibit-startup-message t)

(tool-bar-mode 0)

(setq config-changed-for-menu nil)

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
  (menu-bar-mode -1)
  (menu-bar-mode 1)
  nil)

(refresh-mode-menu)

(add-hook 'emacs-startup-hook (lambda () (reactivate-menu))) ; Refresh Buffer custom menu
(add-hook 'after-init-hook (lambda () (reactivate-menu)))    ; Alternative (worked one) of above

(add-hook 'window-configuration-change-hook 'config-change-indirect-callback)
(add-hook 'menu-bar-update-hook 'refresh-mode-menu)
(add-hook 'first-change-hook 'reactivate-menu)

(provide 'personal-global)
;;; global.el ends here
