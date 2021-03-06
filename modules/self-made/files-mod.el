;;; files-mod.el --- file input and output commands (enhanced) for Emacs

;; Copyright (C) 1985-1987, 1992-2013 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Package: emacs

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines most of Emacs's file- and directory-handling functions,
;; including basic file visiting, backup generation, link handling,
;; ITS-id version control, load- and write-hook handling, and the like.

;;; Code:

(defun basic-save-buffer ()
  "Save the current buffer in its visited file, if it has been modified.
The hooks `write-contents-functions' and `write-file-functions' get a chance
to do the job of saving; if they do not, then the buffer is saved in
the visited file in the usual way.
Before and after saving the buffer, this function runs
`before-save-hook' and `after-save-hook', respectively."
  (interactive)
  (save-current-buffer
    ;; In an indirect buffer, save its base buffer instead.
    (if (buffer-base-buffer)
        (set-buffer (buffer-base-buffer)))
    (if (or (buffer-modified-p)
            ;; handle the case when no modification has been made but
            ;; the file disappeared since visited
            (and buffer-file-name
                 (not (file-exists-p buffer-file-name))))
        (let ((recent-save (recent-auto-save-p))
              setmodes)
          ;; If buffer has no file name, ask user for one.
          (or buffer-file-name
              (let ((filename
                     (expand-file-name
                      (read-file-name "File to save in: "
                                      nil (expand-file-name (buffer-name))))))
                (if (file-exists-p filename)
                    (if (file-directory-p filename)
                        ;; Signal an error if the user specified the name of an
                        ;; existing directory.
                        (error "%s is a directory" filename)
                      (unless (y-or-n-p (format "File `%s' exists; overwrite? "
                                                filename))
                        (error "Canceled")))
                  ;; Signal an error if the specified name refers to a
                  ;; non-existing directory.
                  (let ((dir (file-name-directory filename)))
                    (unless (file-directory-p dir)
                      (catch 'prompt-create
                        (if (when (y-or-n-p (format "Directory %s does not exist. Create it?" dir))
                                   (make-directory dir t))
                            (throw 'prompt-create
                                   (error "Dir %s does not exist (not created also)" dir))))

                      )
                    )
                  )
                (set-visited-file-name filename)))
          (or (verify-visited-file-modtime (current-buffer))
              (not (file-exists-p buffer-file-name))
              (yes-or-no-p
               (format
                "%s has changed since visited or saved.  Save anyway? "
                (file-name-nondirectory buffer-file-name)))
              (user-error "Save not confirmed"))
          (save-restriction
            (widen)
            (save-excursion
              (and (> (point-max) (point-min))
                   (not find-file-literally)
                   (/= (char-after (1- (point-max))) ?\n)
                   (not (and (eq selective-display t)
                             (= (char-after (1- (point-max))) ?\r)))
                   (or (eq require-final-newline t)
                       (eq require-final-newline 'visit-save)
                       (and require-final-newline
                            (y-or-n-p
                             (format "Buffer %s does not end in newline.  Add one? "
                                     (buffer-name)))))
                   (save-excursion
                     (goto-char (point-max))
                     (insert ?\n))))
            ;; Support VC version backups.
            (vc-before-save)
            (run-hooks 'before-save-hook)
            (or (run-hook-with-args-until-success 'write-contents-functions)
                (run-hook-with-args-until-success 'local-write-file-hooks)
                (run-hook-with-args-until-success 'write-file-functions)
                ;; If a hook returned t, file is already "written".
                ;; Otherwise, write it the usual way now.
                (setq setmodes (basic-save-buffer-1)))
            ;; Now we have saved the current buffer.  Let's make sure
            ;; that buffer-file-coding-system is fixed to what
            ;; actually used for saving by binding it locally.
            (if save-buffer-coding-system
                (setq save-buffer-coding-system last-coding-system-used)
              (setq buffer-file-coding-system last-coding-system-used))
            (setq buffer-file-number
                  (nthcdr 10 (file-attributes buffer-file-name)))
            (if setmodes
                (condition-case ()
                    (progn
                      (set-file-modes buffer-file-name (car setmodes))
                      (set-file-selinux-context buffer-file-name (nth 1 setmodes)))
                  (error nil))))
          ;; If the auto-save file was recent before this command,
          ;; delete it now.
          (delete-auto-save-file-if-necessary recent-save)
          ;; Support VC `implicit' locking.
          (vc-after-save)
          (run-hooks 'after-save-hook))
      (message "(No changes need to be saved)"))))

(provide 'files-mod)
