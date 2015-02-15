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

(defvar module-dir "~/.emacs.d/modules")

(add-to-list 'load-path module-dir)
(add-subfolders-to-load-path module-dir) ; Execute necessary (listed) modules
(load "~/.emacs.d/modules")      ; preload all modules necessary settings
(load "~/.emacs.d/configure")    ; preload the personal settings

(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))
