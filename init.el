;; Init emacs file

(require 'server)
(unless (server-running-p)
  (server-start))

(defun add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (add-subfolders-to-load-path name)))))


(defvar personal-dir "~/.emacs.d/personal")
(defvar module-dir "~/.emacs.d/modules")

(add-to-list 'load-path module-dir)

; Execute necessary (listed) modules
(load "~/.emacs.d/modules")

(add-to-list 'load-path personal-dir)
(add-subfolders-to-load-path personal-dir)

;; preload the personal settings
(when (file-exists-p personal-dir)
  (message "Loading personal configuration files in %s..." personal-dir)
  (mapc 'load (directory-files personal-dir 't "^[^#].*el$")))


(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))
