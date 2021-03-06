;;;; Mode Line style modules

;; Mode-line custom setup
(setq separator ?\s)   ; (:propertize " " face mode-line-separator)

(setq-default mode-line-format '(
  ;; Position, including warning for reach fill-column length
 "-: "
  (:propertize " " face mode-line-padding)
  (:propertize "[%4l,"
               face mode-line-position-face)
  (:eval (propertize " %3c]"
                     'face
                     (if (>= (current-column) fill-column)
                         'mode-line-max-col-face
                       'mode-line-position-face)))
  (:propertize " " face mode-line-padding)
  ;; emacsclient [default -- keep?]

  " "

  (:propertize " " face mode-line-padding)
  (:propertize "%b"
               face 'mode-line-filename-face
			   local-map
               (keymap
                (header-line keymap
                             (mouse-3 . mode-line-next-buffer)
                             (down-mouse-3 . ignore)
                             (mouse-1 . mode-line-previous-buffer)
                             (down-mouse-1 . ignore))
                (mode-line keymap
                           (mouse-3 . mode-line-next-buffer)
                           (mouse-1 . mode-line-previous-buffer)))
               mouse-face mode-line-highlight help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer" face mode-line-buffer-id)
  ;; (:propertize " " face mode-line-padding)

  ;; " "

  (:propertize " (" face mode-line-padding)
  (:propertize
   (:eval (cond ((not buffer-read-only)
                 (propertize "RW" 'face 'mode-line-file-status-face))
                (buffer-read-only
                 (propertize "R-" 'face 'mode-line-file-status-face))
                (t "    ")))
   mouse-face mode-line-highlight
   local-map
   (keymap (mode-line keymap
                      (mouse-1 . mode-line-toggle-read-only))))

  (:propertize
   (:eval
    (cond ((buffer-modified-p)
           (propertize "M" 'face 'mode-line-file-status-face))
          ((not (buffer-modified-p))
           (propertize "-" 'face 'mode-line-file-status-face))
          (t "    ")))
   mouse-face mode-line-highlight
   local-map
   (keymap (mode-line keymap
                      (mouse-1 . mode-line-toggle-modified))))
  (:propertize ") in " face mode-line-padding)

                                        ; directory and buffer/file name
  (:propertize (:eval (concat "[" (shorten-directory default-directory 20) "]"))
               help-echo (shorten-directory default-directory 100)
               face mode-line-folder-face)
  (:propertize " " face mode-line-padding)


  (:propertize " " face mode-line-padding)
  (:propertize "%3p"
               help-echo "Size indication mode\nmouse-1: Display Line and Column Mode Menu"
               face 'mode-line-buf-position-face
               mouse-face mode-line-highlight
               local-map
               (keymap
                (mode-line keymap
                           (down-mouse-1 keymap
                                         (column-number-mode menu-item "Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button
                                                             (:toggle . column-number-mode))
                                         (line-number-mode menu-item "Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button
 														   (:toggle . line-number-mode))
                                         "Toggle Line and Column Number Display"))))

  (:propertize " " face mode-line-padding)

  " "

  (:propertize " %[" face mode-line-padding)
  ;; narrow [default -- keep?]
  ;; " %n "
  ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
  (:propertize mode-name face mode-line-mode-face)
  (:propertize "%] " face mode-line-padding)

  " "

  (:propertize " " face mode-line-padding)
  (:eval (propertize (format-mode-line minor-mode-alist)
                     'face 'mode-line-minor-mode-face))
  (:propertize " " face mode-line-padding)

  " "

  (:propertize " " face mode-line-padding)
  (:propertize (vc-mode vc-mode) face 'mode-line-base-container)
  (:propertize mode-line-misc-info face 'mode-line-base-container)
  (:propertize " " face mode-line-padding)

  " "

  (:propertize " " face mode-line-padding)
  (:propertize mode-line-process
               face 'mode-line-process-face)
  (:propertize (global-mode-string global-mode-string)
               face 'mode-line-base-container)
  (:propertize " " face mode-line-padding)

  " "

  ; nyan-mode uses nyan cat as an alternative to %p
  ;(:eval (when nyan-mode (list (nyan-create))))

  (:propertize mode-line-client face 'mode-line-base-container)
  "%-"
  ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-base-container)
; :inherit 'mode-line-base-container
(make-face 'mode-line-padding)
(make-face 'mode-line-separator)
(make-face 'mode-line-symbol-face)
(make-face 'mode-line-file-status-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-buf-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-max-col-face)


(set-face-attribute 'mode-line nil
                    :foreground "#fff" :background "#343434"
                    :inverse-video nil
                    :height 100
                    :box '(:line-width 3 :color "#343434" :style nil))

(set-face-attribute 'mode-line-highlight nil
                    :background "#343434"
                    :foreground "white"
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :foreground "#eee" :background "#075"
                    :inverse-video nil
                    :height 100
                    :box '(:line-width 3 :color "#075" :style nil))

(set-face-attribute 'mode-line-base-container nil
                    :height 100
                    :foreground "#005a00"
                    :background "#b0ff80"
                    :box '(:line-width 3 :color "#b0ff80" :style nil))

(set-face-attribute 'mode-line-padding nil
                    :foreground "#333"
                    :background "#b0ff80"
                    :height 100
                    :box '(:line-width 3 :color "#b0ff80" :style nil))

(set-face-attribute 'mode-line-separator nil
                    :foreground "#333"
                    :background "#296"
                    :height 100
                    :box '(:line-width 3 :color "#296" :style nil))

(set-face-attribute 'mode-line-symbol-face nil
                    :foreground "#000")

(set-face-attribute 'mode-line-file-status-face nil
                    :inherit 'mode-line-base-container
                    :foreground "#4271ae"
                    :weight 'bold
                    :height 100
                    :box '(:line-width 1 :color "#4271ae"))

(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-base-container
                    :foreground "#c82829" :background "#ffffff"
                    :box '(:line-width 3 :color "#b0ff80" :style nil))

(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-base-container
                    :foreground "#000")

(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-base-container
                    :foreground "#ff001a"
                    :weight 'bold
                    :box '(:line-width 3 :color "#b0ff80" :style nil))

(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-base-container
                    :family "Ubuntu Mono"
                    :foreground "#0000ff"
                    :height 100
                    :box '(:line-width 3 :color "#b0ff80" :style nil))

(set-face-attribute 'mode-line-buf-position-face nil
                    :inherit 'mode-line-base-container
                    :foreground "blue")

(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-base-container
                    :foreground "#005a00"
                    :box nil)

(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :inherit 'mode-line-base-container
                    :foreground "#005a00"
                    :height 100)

(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-mode-face
                    :inherit 'mode-line-base-container
                    :foreground "#9900ff")

(set-face-attribute 'mode-line-max-col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "#00f" :background "#b0ff80"
                    :weight 'bold)

(provide 'prelude-modeline)
