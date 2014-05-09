;;; move-mode.el --- minor mode for rapid read-only movement around buffer

;; Copyright (C) 2000 Cyprian Laskowski

;; Author: Cyprian Laskowski <swagbelly@yahoo.com>
;; Created: 10 Dec 2000
;; Version: 0.1
;; Keywords: convenience, read-only, motion

;; This file is NOT currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; move-mode.el is a minor mode which provides lots of Emacs' movement
;; functions at single key presses in read-only mode, while also
;; making it extremely finger-efficient to save and return to points
;; in a buffer (with a mark ring) and define and use keyboard macros.

;;; Change Log:

;; Still in infancy stage.

;;; Bugs:

;; Search functions don't exit right with C-g.  

;;; Code:

(require 'cl)

;;  Customization variables

(defgroup move-mode nil
  "Minor mode for moving around buffer quickly"
  :group 'convenience)

(defcustom move-mode-conflict-modes
  '(view-mode)
  "*Minor modes which are disabled when move-mode is enabled, and
reenabled when move-mode is disabled." 
  :tag "Potentially conflicting minor modes"
  :type '(repeat symbol)
  :group 'move-mode)

(defcustom move-mode-major-mode-functions 
  '(((emacs-lisp-mode lisp-interaction-mode)
     forward-sexp backward-sexp end-of-defun beginning-of-defun)
    ((c-mode c++-mode java-mode jde-mode)
     c-end-of-statement c-beginning-of-statement c-end-of-defun c-beginning-of-defun))
  "*Classes of modes and their members which move-mode uses to
determine which functions to use when moving by \"paragraphs\" and
\"sentences\"."
  :tag "Major mode movement functions"
  :type '(repeat
	  (list
	   (repeat (symbol :tag "Major mode"))
	   (function :tag "forward sentence function")
	   (function :tag "backward sentence function")
	   (function :tag "forward paragraph function")
	   (function :tag "backward paragraph function")))
  :group 'move-mode)

(defcustom move-mode-default-functions
  '(forward-sentence backward-sentence forward-paragraph backward-paragraph)
  "*Default paragraph and sentence movement functions"
  :tag "Default movement functions"
  :type '(list 
	  (function :tag "forward sentence function")
	  (function :tag "backward sentence function")
	  (function :tag "forward paragraph function")
	  (function :tag "backward paragraph function"))
  :group 'move-mode)

(defcustom move-mode-use-read-only t
  "*Non-nil means move-mode puts buffer in read-only mode while the mode is active."
  :tag "Use read only" 
  :type 'boolean
  :group 'move-mode)

(defcustom move-mode-scroll-tenths 5
  "*Tenths of window height by which window will be scrolled for move-mode part of window scroll commands.  0 means scroll one line at a time."
  :tag "Window scroll tenths"
  :type 'integer
  :group 'move-mode)

(make-variable-buffer-local 'move-mode-scroll-tenths)   

(defcustom move-mode-mark-ring-size 16
  "*Maximum size of move-mode mark ring" 
  :tag "Mark ring size"
  :type 'integer
  :group 'move-mode)

(defcustom move-mode-forward-on-right t
  "*Non-nil means keys for forward motion commands are to the right of backward motion commands."
  :tag "Forward motion keys on right"
  :type 'boolean
  :group 'move-mode)

;; Internal variables

(defvar move-mode-save-conflict-modes nil
  "A sublist of move-mode-conflict-modes including which were enabled when move-mode was entered.")
(make-variable-buffer-local 'move-mode-save-conflict-modes)

(defvar move-mode-save-read-only nil
  "Non-nil means current buffer was in read-only state when move-mode was entered.")  
(make-variable-buffer-local 'move-mode-save-read-only) 

(defvar move-mode nil
  "Non-nil if move-mode is enabled.
Don't change this variable directly.") 
(make-variable-buffer-local 'move-mode) 

(defvar move-mode-isearch-regexp-string nil
  "The last regexp searched for with move-mode-isearch-forward-regexp (or backward).")

(defvar move-mode-isearch-string nil
  "The last string searched for with move-mode-isearch-forward (or backward).")

(defvar move-mode-mark-ring nil
  "Ring of user-saved marks.")
(make-variable-buffer-local 'move-mode-mark-ring)
  
(defvar move-mode-mark-index 0)

(define-prefix-command 'move-mode-personal-map)

(defvar move-mode-personal-map nil
  "Personal Keymap for move-mode.")
(unless move-mode-personal-map
  (setq move-mode-personal-map (make-sparse-keymap)))

(defvar move-mode-map nil
  "Keymap for move-mode.")
(unless move-mode-map
  (setq move-mode-map (make-sparse-keymap))
  (flet ((bind (lst)
	       (mapc '(lambda (elt)
			(define-key
			  move-mode-map
			  (car elt)
			  (car (cdr elt))))
		     lst)))
    (bind
     '(("Q" font-lock-mode)
       ("W" move-mode-toggle-follow)
       ("E" setnu-mode)
       ("R" move-mode-navi)
       ("U" move-mode-undo)
       ("{" dired-jump-other-window)
       ("M" diff-buffer-file)
       ("P" move-mode-bypass)
       ("O" call-last-kbd-macro)
       ("u" universal-argument)
       ("i" exchange-point-and-mark)
       ("I" kill-ring-save)
       ("'" move-mode-personal-command)
       ("\"" move-mode-set-personal-command)
       ("\\" repeat)
       ("Z" delete-window)
       ("X" delete-other-windows)
       ("C" split-window-vertically)
       ("V" other-window)
       ("G" move-mode-cycle-other-window)
       ("A" move-mode-mark-push) 
       ("S" move-mode-mark-pop)
       ("J" start-kbd-macro) 
       ("K" end-kbd-macro)
       (":" exit-recursive-edit)
       ("L" move-mode-kbd-macro-query)
       ("T" eval-expression)
       ("Y" shell-command-with-completion)
       ("-" negative-argument)
       ("0" digit-argument)
       ("9" digit-argument)
       ("8" digit-argument)
       ("7" digit-argument)
       ("6" digit-argument)
       ("5" digit-argument)      
       ("4" digit-argument)
       ("3" digit-argument)
       ("2" digit-argument)
       ("1" digit-argument)
       ("t" goto-char)
       ("y" goto-line)
       (" " set-mark-command)
       (">" end-of-buffer)
       ("<" beginning-of-buffer)
       ("=" move-mode-set-scroll-tenths)
       ("\e\e" move-mode)
       ("\C-m" move-mode)
       ("?" move-mode-describe-bindings)))
    (if move-mode-forward-on-right
	(bind	 
	 '(("[" backward-list)
	   ("]" forward-list)
	   ("D" move-mode-mark-previous)
	   ("F" move-mode-mark-next)
	   ("N" scroll-other-window)
	   ("B" scroll-other-window-down)
	   ("h" end-of-line) 
	   ("g" beginning-of-line)
	   ("b" scroll-down)
	   ("n" scroll-up)
	   ("o" backward-to-indentation)
	   ("p" forward-to-indentation)
	   ("m" move-mode-isearch-backward-last-regexp)
	   ("," move-mode-isearch-forward-last-regexp)
	   ("." move-mode-isearch-backward-regexp)
	   ("/" move-mode-isearch-forward-regexp)
	   ("j" move-mode-isearch-backward-last)
	   ("k" move-mode-isearch-forward-last)
	   ("l" move-mode-isearch-backward)
	   (";" move-mode-isearch-forward)
	   ("w" move-mode-forward-sentence-function)
	   ("q" move-mode-backward-sentence-function)
	   ("r" move-mode-forward-paragraph-function)
	   ("e" move-mode-backward-paragraph-function)
	   ("z" move-mode-scroll-down-part)
	   ("x" move-mode-scroll-up-part)
	   ("v" forward-word)
	   ("c" backward-word)
	   ("f" next-line)
	   ("d" previous-line)
	   ("s" forward-char)
	   ("a" backward-char)))
      (bind
       '(("[" forward-list)
	 ("]" backward-list)
	 ("F" move-mode-mark-previous)
	 ("D" move-mode-mark-next)
	 ("B" scroll-other-window-down)
	 ("N" scroll-other-window)
	 ("g" beginning-of-line)
	 ("h" end-of-line)
	 ("n" scroll-up)
	 ("b" scroll-down)
	 ("p" forward-to-indentation)
	 ("o" backward-to-indentation)
	 ("," move-mode-isearch-forward-last-regexp)
	 ("m" move-mode-isearch-backward-last-regexp)
	 ("/" move-mode-isearch-forward-regexp)
	 ("." move-mode-isearch-backward-regexp)
	 ("k" move-mode-isearch-forward-last)
	 ("j" move-mode-isearch-backward-last)
	 (";" move-mode-isearch-forward)
	 ("l" move-mode-isearch-backward)
	 ("q" move-mode-backward-sentence-function)
	 ("w" move-mode-forward-sentence-function)
	 ("e" move-mode-backward-paragraph-function)
	 ("r" move-mode-forward-paragraph-function)
	 ("x" move-mode-scroll-up-part)
	 ("z" move-mode-scroll-down-part)
	 ("v" forward-word)
	 ("c" backward-word)
	 ("f" next-line)
	 ("d" previous-line)
	 ("s" forward-char)
	 ("a" backward-char))))))
	   
(unless (assq 'move-mode minor-mode-alist)
  (setq minor-mode-alist
	(cons '(move-mode " Move") minor-mode-alist)))

(unless (assq 'move-mode minor-mode-map-alist)
  (setq minor-mode-map-alist
	(cons (cons 'move-mode move-mode-map) minor-mode-map-alist)))


;; commands

(defun move-mode (&optional arg)
  "Toggle move-mode, a minor for moving around a buffer quickly, but not editing it.  
With arg, turn move-mode on iff arg is positive.

\\<move-mode-map>

I can only imagine this mode being of interest to key binding fanatics
like myself.

This mode can be used to quickly move around a buffer by logical
quantities, using the various forward-x and backward-x commands of
Emacs, including searches.  In a couple of cases (i.e., paragraph and
sentence motion), movement is dependent on the major-mode type (e.g.,
movement in emacs-lisp-mode is by sexp rather than sentence and defun
rather than paragraph), and these are easily customizable by mode.

The key behind this mode is the keymap (whose bindings can be viewed
within move-mode by typing `\\[describe-bindings]').  (In fact, this
mode does very little else: it uses almost exclusively standard emacs
functions.)  It is completely non-intuitive for the memory, and
foregoes almost all Emacs keybinding conventions.  But the intention
is that it be intuitive to the fingers rather than the memory, and the
hope is that the mind will eventually come around when it sees how
pleased the fingers are.  This mode is motivated primarily by VI's
\"hjkl\" bindings, Emacs' view-mode, and my desire to (eventually) be
able to move between any two points in a buffer in a nanosecond.

The central bindings in this mode are of two types: forward and
backward movement types (currently, by char, line, word, paragraph,
sentence, window, part-window, indentation, line beginning and end),
and forward and backward search movement types (isearch, isearch by
regexp, both new searches and last searches).  The key bindings were
decided upon by ranking the types in terms of probable frequency, and
mapping the most frequent ones to keys that were closest for the
fingers from standard typing position.  The search bindings were
placed in the right hand's domain, and the other movement bindings in
the left hand's domain.  In every backward-forward pair, the backward
movement key is immediately to the left of the forward movement key.

Since key bindings and their rationale are at the core of this mode, here's a
brief geographically-oriented synopsis:

asdf - mvmt by chars, lines
qwer - mvmt by paragraphs, sentences
zxcv - mvmt by part-screens, words
jkl; - isearches
m,./ - regexp isearches
u - prefix
i - exchange point/mark
op - mvmt by indentation
[] - mvmt by list
bn - mvmt by screens
gh - beg/end of line
ty - goto char/line
<> - beg/end of buffer
space - set mark
return - exit move-mode
0-9 - numeric prefixes
JKL: - kbd macros
ASDF - marker ring
ZXCV - window operations
QWER - toggling of favourite minor modes

\\{move-mode-map}"

  (interactive "P")
  (unless (and arg
	       (if (> (prefix-numeric-value arg) 0)
		   move-mode
		 (not move-mode)))
    (if move-mode (move-mode-disable)
      (move-mode-enable))
    (force-mode-line-update)))

(defun move-mode-find-file ()
  "Opens file (using find-file) and enables move-mode."
  (interactive)
  (call-interactively 'find-file)
  (move-mode))

(defun move-mode-isearch-forward (count)
  (interactive "p")
  (isearch-forward)
  (while (> count 1)
    (isearch-repeat-forward)
    (setq count (1- count)))
  (setq move-mode-isearch-string isearch-string))

(defun move-mode-isearch-forward-regexp (count)
  (interactive "p")
  (isearch-forward-regexp)
  (while (> count 1)
    (isearch-repeat-forward)
    (setq count (1- count)))
  (setq move-mode-isearch-regexp-string isearch-string))

(defun move-mode-isearch-backward (count)
  (interactive "p")
  (isearch-backward)
  (while (> count 1)
    (isearch-repeat-backward)
    (setq count (1- count)))
  (setq move-mode-isearch-string isearch-string))

(defun move-mode-isearch-backward-regexp (count)
  (interactive "p")
  (isearch-backward-regexp)
  (while (> count 1)
    (isearch-repeat-backward)
    (setq count (1- count)))
  (setq move-mode-isearch-regexp-string isearch-string))

(defun move-mode-isearch-forward-last (count)
  (interactive "p")
  (search-forward move-mode-isearch-string nil nil count))

(defun move-mode-isearch-forward-last-regexp (count)
  (interactive "p")
  (search-forward-regexp move-mode-isearch-regexp-string nil nil count))

(defun move-mode-isearch-backward-last (count)
  (interactive "p")
  (search-backward move-mode-isearch-string nil nil count))

(defun move-mode-isearch-backward-last-regexp (count)
  (interactive "p")
  (search-backward-regexp move-mode-isearch-regexp-string nil nil count))

(defun move-mode-scroll-down-part (count)
  "Scroll down by half the window's height."
  (interactive "p")
  (if (eq move-mode-scroll-tenths 0)
      (scroll-down count)
    (scroll-down (ceiling (* (window-height) move-mode-scroll-tenths count 0.1)))))

(defun move-mode-scroll-up-part (count)
  "Scroll up by half the window's height."
  (interactive "p")
  (if (eq move-mode-scroll-tenths 0)
      (scroll-up count)
    (scroll-up (ceiling (* (window-height) move-mode-scroll-tenths count 0.1)))))
	       
(defun move-mode-set-scroll-tenths (tenths)
  "1 through 9 mean 10 through 90 percent of window is scrolled with part window scroll functions.  0 means 1 line at a time."
  (interactive "cScroll tenths: ")
  (setq move-mode-scroll-tenths (char-to-int tenths)))

(defun move-mode-kbd-macro-query ()
  (interactive)
  (kbd-macro-query 1))

(defun move-mode-navi ()
  (interactive)
  (navi (current-buffer)))

;; internal functions

(defun move-mode-setup-mode-specific-functions ()
  "Sets up the paragraph and sentence functions according to major
mode; functions are determined by identifying the current mode,
identifying its move-mode class (see move-mode-major-modes), and
finding the corresponding paragraph and sentence functions according
to the definitions for the class." 
  (let ((func-list (or 
		    (assoc-default major-mode
				   move-mode-major-mode-functions
				   (lambda (a b) (member b a)))
		    move-mode-default-functions)))
    (fset 'move-mode-forward-sentence-function (nth 0 func-list))
    (fset 'move-mode-backward-sentence-function (nth 1 func-list))
    (fset 'move-mode-forward-paragraph-function (nth 2 func-list))
    (fset 'move-mode-backward-paragraph-function (nth 3 func-list))))

(defun move-mode-enable ()
  "Switches move-mode on; should only be called by move-mode"
  (setq move-mode t
	move-mode-save-read-only buffer-read-only
	move-mode-save-conflict-modes
	(remove-if-not '(lambda (x)
			  (and (boundp x) (eval x)))
		       move-mode-conflict-modes))
  (mapc '(lambda (x) (funcall x -1)) move-mode-save-conflict-modes)
  (when move-mode-use-read-only (setq buffer-read-only t))
  (unless move-mode-mark-ring (setq move-mode-mark-ring (make-ring move-mode-mark-ring-size)))
  (move-mode-setup-mode-specific-functions))

(defun move-mode-disable ()
  "Switches move-mode off; should only be called by move-mode"
  (mapc '(lambda (x) (funcall x 1)) move-mode-save-conflict-modes)
  (setq move-mode nil
	buffer-read-only move-mode-save-read-only
	move-mode-save-conflict-modes nil))

(defun move-mode-bypass (keyseq)
  "Executes command which keyseq is bound to in local maps with
move-mode-map disabled."
  (interactive "kKey sequence: ")
  (let ((move-mode nil) (buffer-read-only nil) (last-command-char (string-to-char keyseq)))
    (call-interactively (key-binding keyseq))))

(defvar move-mode-bindings nil
  "move-mode personal bindings (prefix:)
\\{move-mode-personal-map}
move-mode main map:
\\{move-mode-map}")

(defun move-mode-describe-bindings ()
  (interactive)
  (describe-variable 'move-mode-bindings))

(defun move-mode-set-personal-command (prefix keyseq)
  "Setup or get docs on command in move-mode personal keymap."
  (interactive "P\nkKey sequence: ")
  (if prefix
      (let ((comm (lookup-key move-mode-personal-map keyseq)))
	(if (arrayp comm)
	    (describe-function comm)
	  (message "Macro: %s" (format-kbd-macro comm))))
    (let ((comm (read-command "Command to bind: ")))
      (if (commandp comm)
	  (define-key move-mode-personal-map keyseq comm)
	(let ((macro (intern (concat "move-mode-macro-" keyseq))))
	  (fset macro last-kbd-macro)
	  (define-key move-mode-personal-map keyseq macro))))))

(defun move-mode-personal-command (prefix keyseq)
  "Executes command in move-mode personal keymap."
  (interactive "p\nkKey sequence: ")
  (let ((buffer-read-only nil) (comm (lookup-key move-mode-personal-map keyseq)))
    (while (> prefix 0) 
      (command-execute comm)
      (setq prefix (1- prefix)))))

(defun move-mode-cycle-other-window ()
  (interactive)
  (other-window 1)
  (bury-buffer) 
  (other-window 1))
    
(defun move-mode-mark-push ()
  (interactive)
  (ring-insert move-mode-mark-ring (point-marker)))

(defun move-mode-mark-pop ()
  (interactive)
  (ring-remove move-mode-mark-ring move-mode-mark-index))

(defun move-mode-mark-previous (prefix)
  (interactive "p")
  (setq move-mode-mark-index (- move-mode-mark-index prefix))
  (goto-char (ring-ref move-mode-mark-ring move-mode-mark-index)))

(defun move-mode-mark-next (prefix)
  (interactive "p")
  (setq move-mode-mark-index (+ move-mode-mark-index prefix))
  (goto-char (ring-ref move-mode-mark-ring move-mode-mark-index)))

(defun move-mode-toggle-follow ()
  (interactive)
  (if (and (boundp 'follow-mode) follow-mode) 
      (turn-off-follow-mode)
    (follow-delete-other-windows-and-split)))

(defun move-mode-undo (prefix)
  (interactive "p")
  (let ((buffer-read-only nil))
    (undo prefix)))

(provide 'move-mode)

;;; move-mode.el ends here
