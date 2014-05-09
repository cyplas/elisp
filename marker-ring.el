;;; marker-ring.el --- maintain and navigate a ring of markers in a buffer

;; Copyright (C) 2001 Cyprian Laskowski

;; Author: Cyprian Laskowski <swagbelly@yahoo.com>
;; Created: 21 May 2001
;; Version: 0.1
;; Keywords: convenience

;; This file is NOT currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.


;;; Commentary:

;; There are already several useful features for navigating through the old
;; marks in a buffer, including the built-in C-u C-SPACE feature, as well as
;; Benjamin Rutt's recent marker-visit.el package (thanks to him for inspiring
;; this package).  However, the mark is most often set (at least by me) when
;; cutting or copying a region, and I find that typically these are not
;; interesting buffer positions to have as bookmarks to come back to.

;; Hence this package provides a SEPARATE marker ring that the user explicitly
;; manipulates and navigates, and the optional feature of having this ring
;; updated when the mark is set.


;;; Installation:

;; Get the most recent version at http://www.swagbelly.net/elisp/lib/.

;; To install, put this file in your Emacs load-path and the following line in
;; your .emacs file:

;; (require 'marker-ring)

;; If you like, you can customize the ring size, a prefix key for marker-ring
;; commands (you might want to set this to ', for example), and whether or
;; not to have new marks automatically added to the marker ring by default; type
;; M-x customize-group marker-ring RETURN to customize these.

;; Then load this file or restart Emacs.


;;; History:

;; 20 May 2001: First revision


;;; Code:

(require 'cl)
(require 'ring)


;;; Customization variables

(defgroup marker-ring nil
  "Convenient business for moving between buffer markers"
  :group 'convenience)

(defcustom marker-ring-size 16
  "Size of buffer marker rings."
  :type 'integer
  :group 'marker-ring)

(defcustom marker-ring-key-prefix nil
  "Key to which assign keymap."
  :type '(choice string (const nil))
  :group 'marker-ring)

(defcustom marker-ring-auto-add-mark-flag nil
  "Non-nil means to add mark to marker ring whenever new mark is set."
  :type 'boolean
  :group 'marker-ring)


;;; Internal variables

(defvar marker-ring nil
  "The buffer's marker ring.")
(make-variable-buffer-local 'marker-ring)

(defvar marker-ring-index nil
  "The current index in the marker ring of the buffer.")
(make-variable-buffer-local 'marker-ring)

(when (and (not (boundp 'marker-ring-map)) marker-ring-key-prefix)
  (define-prefix-command 'marker-ring-map)
  (define-key global-map marker-ring-key-prefix 'marker-ring-map)
  (define-key marker-ring-map "a" 'marker-ring-add)
  (define-key marker-ring-map "d" 'marker-ring-remove)
  (define-key marker-ring-map "n" 'marker-ring-next)
  (define-key marker-ring-map "p" 'marker-ring-prev)
  (define-key marker-ring-map "c" 'marker-ring-current)
  (define-key marker-ring-map "k" 'marker-ring-clear)
  (define-key marker-ring-map "m" 'marker-ring-add-mark)
  (define-key marker-ring-map "t" 'marker-ring-toggle-auto-add-mark))


;;; Commands

;;;###autoload
(defun marker-ring-add ()
  "Add the position of point to the marker ring."
  (interactive)
  (marker-ring-add-internal (set-marker (make-marker) (point))))

;;;###autoload
(defun marker-ring-remove ()
  "Remove the current marker from the marker ring."
  (interactive)
  (if (not (ring-empty-p marker-ring))
      (ring-remove marker-ring marker-ring-index)
    (message "Marker ring is empty.")))

;;;###autoload
(defun marker-ring-next (arg)
  "Cycle forward to the next position in the marker ring.
With numeric prefix ARG, cycle forward that many positions."
  (interactive "p")
  (while (> arg 0)
    (setq marker-ring-index
          (ring-plus1 marker-ring-index (ring-length marker-ring))
          arg (1- arg)))
  (marker-ring-current))

;;;###autoload
(defun marker-ring-prev (arg)
  "Cycle backward to the previous position in the marker ring.
With numeric prefix ARG, cycle backward that many positions."
  (interactive "p")
  (while (> arg 0)
    (setq marker-ring-index
          (ring-minus1 marker-ring-index (ring-length marker-ring))
          arg (1- arg)))
  (marker-ring-current))

;;;###autoload
(defun marker-ring-current ()
  "Go to the current index in the marker ring."
  (interactive)
  (if (not (ring-empty-p marker-ring))
      (goto-char (ring-ref marker-ring marker-ring-index))
    (message "Marker ring is empty.")))

;;;###autoload
(defun marker-ring-clear ()
  "Remove all items from the marker ring."
  (interactive)
  (setq marker-ring-index 0)
  (while (not (ring-empty-p marker-ring))
    (ring-remove marker-ring)))

;;;###autoload
(defun marker-ring-add-mark ()
  "Add the mark to the marker ring."
  (interactive)
  (if mark-active
      (marker-ring-add-internal (mark-marker))
    (message "Couldn't add mark: mark is not set in this buffer.")))

;;;###autoload
(defun marker-ring-toggle-auto-add-mark ()
  "Toggle whether or not setting the mark will add the mark to the marker ring."
  (interactive)
  (if (member 'marker-ring-auto-add-mark
              (mapcar 'car (ad-get-enabled-advices 'set-mark 'after)))
      (ad-disable-advice 'set-mark 'after 'marker-ring-auto-add-mark)
    (ad-enable-advice 'set-mark 'after 'marker-ring-auto-add-mark))
  (ad-activate 'set-mark))


;;; Internal functions

;;;###autoload
(defun marker-ring-create ()
  "Create a new marker ring of length `marker-ring-size'.
Also set `marker-ring-index' to 0."
  (setq marker-ring (make-ring marker-ring-size)
        marker-ring-index 0))

;;;###autoload
(defun marker-ring-add-internal (marker)
  "Add MARKER to `marker-ring'."
  (unless marker-ring (marker-ring-create))
  (ring-insert-at-beginning marker-ring marker))

(defadvice set-mark (after marker-ring-auto-add-mark disable)
  "Add mark to `marker-ring' whenever mark is set with `set-mark'."
  (marker-ring-add-mark))

(when marker-ring-auto-add-mark-flag
  (marker-ring-toggle-auto-add-mark))

(provide 'marker-ring)

;;; marker-ring.el ends here
