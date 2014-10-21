;;;; Mode Line style modules

;; Mode-line custom setup
;
(setq-default mode-line-format '(
  ;mode-line-front-space

  " "

  ; Position, including warning for reach fill-column length
  (:propertize "cur(%4l," face mode-line-position-face)
  (:eval (propertize "%3c)" 'face
           (if (>= (current-column) fill-column)
               'mode-line-max-col-face
               'mode-line-position-face)))
  ; emacsclient [default -- keep?]
  mode-line-client

  " | "

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

  " "

  (:propertize "(" face 'mode-line-symbol-face)
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

  (:propertize ")" face mode-line-symbol-face)

  (:propertize " in " face 'mode-line-symbol-face)

  ; directory and buffer/file name
  (:propertize (:eval (concat "[" (shorten-directory default-directory 20) "]"))
               face mode-line-folder-face)

  " | "

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

  " | "

  ; narrow [default -- keep?]
  ;" %n "
  ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
  (:propertize "%[" face 'mode-line-symbol-face)
  (:propertize mode-name face mode-line-mode-face)
  (:propertize "%]" face 'mode-line-symbol-face)

  " |"

  (:eval (propertize (format-mode-line minor-mode-alist)
                     'face 'mode-line-minor-mode-face))

  " | "

  (vc-mode vc-mode)

  mode-line-misc-info 

  (:propertize mode-line-process
               face 'mode-line-process-face)
  (global-mode-string global-mode-string)
  "    "
  ; nyan-mode uses nyan cat as an alternative to %p
  ;(:eval (when nyan-mode (list (nyan-create))))

  " ;%-"
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
    :foreground "#999" :background "#b0ff80"
    :inverse-video nil
    :height 110
    :box '(:line-width 3 :color "#b0ff80" :style nil))

(set-face-attribute 'mode-line-highlight nil
    :background "#c0ee90"
	:box nil
)

(set-face-attribute 'mode-line-inactive nil
    :foreground "#999" :background "#05d090"
    :inverse-video nil
    :height 110
    :box '(:line-width 3 :color "#05d090" :style nil))

(set-face-attribute 'mode-line-symbol-face nil
    :foreground "#000")

(set-face-attribute 'mode-line-file-status-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :weight 'bold
    :height 110
    :box '(:line-width 1 :color "#4271ae"))

(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829" :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))

(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "#000")

(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#ff001a"
    :weight 'bold)

(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Ubuntu Mono"
    :foreground "#0000ff"
    :height 110)

(set-face-attribute 'mode-line-buf-position-face nil
    :inherit 'mode-line-position-face)


(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "#005a00")

(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "#005a00"
    :height 110)

(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#9900ff")

(set-face-attribute 'mode-line-max-col-face nil
    :inherit 'mode-line-position-face
    :foreground "#00f" :background "#b0ff80"
    :weight 'bold)

(provide 'prelude-modeline)
