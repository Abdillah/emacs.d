;;; spaces-morpher.el --- Support for the Foo programming language

;; Copyright (C) 2013 Hernawan Fa'iz Abdillah

;; Author: Hernawan Fa'iz Abdillah <abdillah96.bu@gmail.com>
;; Maintainer: Hernawan Fa'iz Abdillah <abdillah96.bu@gmail.com>
;; Version: 1.0
;; Package-Version: 1.0
;; Created: 4 Sep 2013
;; Keywords: convenience, navigation, style

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: This software make navidation on emacs easier by bypassing
;;;             `tab-width` of spaces in forward and backward navigation.
;;;             Install it

(defun count-specific-char (char &optional start-reg end-reg)
  "Count provided char from `start-region` (or start-buffer if nil) to `end-region` (or end-buffer if nil)."
  (setq char-pos ())
  (setq char-count 0)
  (if (not (and start-reg end-reg))
    (progn (setq start-reg (point-min))
           (setq end-reg (point-max))))

  (setq count start-reg)
  (while (< count end-reg)
    (if (equal (string (char-after count)) (string char))
	  (progn (setq char-count (1+ char-count))
			 (setq char-pos (cons count char-pos))))
	(setq count (1+ count)))

  ; Return
  char-count)

(defun current-column-char-based ()
    "Convert current-column position into column with char-based position."
	; (current-column) => (current-column-char-based)
	(- (point) (line-beginning-position)))

(defun column-to-char-based (column-pos)
    "Convert column position into column with char-based position."
    (interactive "^")
	(setq first-pos (current-column))
	(move-to-column column-pos)
	(setq char-based-column-pos (current-column-char-based))
	(move-to-column first-pos)

	; Return
	char-based-column-pos)

(setq is-shift-translated-before nil)
(setq last-shifted-position 0)

(defun forward-char-to-tab-stop (&optional N)
    "Move forward skipping char or to nearest tab-stop if in space-indented region."
    (interactive "^p")

	(if (not N)
	  (setq N 1))

	(setq is-space-indented-region t)

    (if (and this-command-keys-shift-translated (not is-shift-translated-before))
      (progn (if (not (region-active-p)) ; Detection of active region
               (push-mark (point) t t))
             (setq last-shifted-position (point))
             (right-char)
             (setq is-shift-translated-before t))
      ; else
      (if (and this-command-keys-shift-translated is-shift-translated-before)
        (progn (if (not (region-active-p)) ; Detection of active region
                 (push-mark (point) t t))
               (setq last-shifted-position (point))
               (forward-char N))
        ; else
        (if (not this-command-keys-shift-translated)
            (progn (setq is-shift-translated-before nil)
                   (deactivate-mark)
                   (pop-mark)))))

    (if (not this-command-keys-shift-translated)
        (dotimes (i N)
          (setq first-pos (current-column))
          (setq nearest-left-tab-stop (- first-pos (% first-pos tab-width)))
          (setq nearest-right-tab-stop (+ first-pos (- tab-width (% first-pos tab-width)))) 

          (if (equal first-pos nearest-left-tab-stop)
              (setq is-space-indented-region t))

          (setq count nearest-left-tab-stop)

          (setq space-count (count-specific-char ?\s 
                                                 (+ (line-beginning-position) 
                                                    (column-to-char-based nearest-left-tab-stop))
                                                 (+ (line-beginning-position) 
                                                    (column-to-char-based nearest-right-tab-stop))))

          (if (equal space-count tab-width)
              (move-to-column nearest-right-tab-stop)
            (right-char)))))

(defun backward-char-to-tab-stop (&optional N)
    "Move backward skipping char or to nearest tab-stop if in space-indented region."
    (interactive "^p")

	(if (not N)
	  (setq N 1))

	(setq is-space-indented-region t)

    (if (and this-command-keys-shift-translated (not is-shift-translated-before))
        (progn (if (not (region-active-p)) ; Detection of active region
                 (push-mark (point) t t))
               (setq is-shift-translated-before t)
               (setq last-shifted-position (point))
               (backward-char))
      ; else
      (if (and this-command-keys-shift-translated is-shift-translated-before)
          (progn (if (not (region-active-p)) ; Detection of active region
                   (push-mark (point) t t))
                 (setq last-shifted-position (point))
                 (backward-char N))
        ; else
        (if (not this-command-keys-shift-translated)
            (progn (setq is-shift-translated-before nil)
                   (deactivate-mark)
                   (pop-mark)))))
       
    (if (not this-command-keys-shift-translated)
        (dotimes (i N)
          (setq first-pos (current-column))
          (setq nearest-left-tab-stop (- first-pos (% first-pos tab-width)))
          (setq nearest-right-tab-stop (+ first-pos (- tab-width (% first-pos tab-width))))
          
          (if (equal first-pos nearest-right-tab-stop)
              (setq is-space-indented-region t))

          (if (equal first-pos nearest-left-tab-stop)
              (progn (setq nearest-left-tab-stop (- nearest-left-tab-stop tab-width))
                     (setq nearest-right-tab-stop (- nearest-right-tab-stop tab-width))))

          (if (< nearest-left-tab-stop 0)
              (setq nearest-left-tab-stop 0))

	  	  (setq space-count (count-specific-char ?\s 
                                                 (+ (line-beginning-position)
                                                    (column-to-char-based nearest-left-tab-stop))
                                                 (+ (line-beginning-position) 
                                                    (column-to-char-based nearest-right-tab-stop))))

          (if (equal space-count tab-width)
              (move-to-column nearest-left-tab-stop)
            (backward-char)))))

(provide 'spaces-as-tab-mode)
