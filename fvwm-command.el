;;; fvwm-command.el --- Interface to FvwmCommand (and hence fvwm)

;; Copyright (C) 2001, 2002 Cyprian Laskowski <swagbelly@yahoo.com>

;; Filename: fvwm-command.el
;; Version: 1.0
;; Keywords: unix
;; Author: Cyprian Laskowski <swagbelly@yahoo.com>
;; URL: http://24.71.17.22/elisp/lib/fvwm-command.el

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.


;;; Commentary:

;; This is a little interface to FvwmCommand, an FVWM module which
;; allows you to execute FVWM commands from the command line.  This
;; package provides emacs commands to execute an FVWM command (using
;; FvwmCommand), visit an FVWM script file (in your FVWM script file
;; directory), or load an FVWM script file.  It also provides simple
;; macros `fvwm-command-defun' and `fvwm-command-define-key', which
;; are similar to defun and define-key, except they deal with FVWM
;; commands, not Emacs commands.

;; One thing that might be interesting to do with this package is to
;; map FVWM functions to keymaps for specific emacs modes.  For
;; example, you could do:
;;
;; (fvwm-command-define-key tex-mode-map "\C-cg" "Next (GV) WarpToWindow 0 0")
;;
;; Or, if you have an FVWM configuration that befuddles anyone else
;; that tries to use it, but you sometimes want to be able to have
;; someone sit down in front of your computer in a confortable
;; environment, you could do something like:
;;
;; (defvar fvwm-command-my-setup t)
;;
;; (defun fvwm-command-toggle-setups ()
;;   (interactive)
;;   (if fvwm-command-my-setup
;;         (fvwm-command-load-script
;;           "Restart fvwm2 -f /home/cyp/fvwm/normal_setup.fvwm")
;;      (fvwm-command-load-script
;;        "Restart fvwm2"))
;;   (setq fvwm-command-my-setup (not fvwm-command-my-setup)))
;;
;; (define-key my-favourite-map "f" 'fvwm-command-toggle-setups)
;;
;; Or, if you really spend most of your computer time in emacs like
;; me, and you're a bit crazy about key bindings, you could have an
;; FVWM script which maps lots of FVWM commands to convenient keys,
;; like:

;; (fvwm-command-defun walk-around
;;   "Read fvwm/walk_around.fvwm"
;;   "Cycle through X windows.")
;;
;; (define-key global-map "\C-cf" 'fvwm-command-walk-around)
;;
;; And in ~/fvwm/walk_around.fvwm, you might have:

;; DestroyFunc ReturnToEmacs
;; AddToFunc ReturnToEmacs
;; + "I" Prev (emacs) WarpToWindow 50 50
;; + "I" Key h A A -
;; + "I" Key j A A -
;; + "I" Key k A A -
;; + "I" Key l A A -
;; + "I" Key n A A -
;; + "I" Key p A A -
;; + "I" Key Return A A -
;;
;; Key h A A Scroll -100 0
;; Key j A A Scroll 0 +100
;; Key k A A Scroll 0 -100
;; Key l A A Scroll +100 0
;; Key n A A Next (!Iconic) WarpToWindow 50 50
;; Key p A A Prev (!Iconic) WarpToWindow 50 50
;; Key Return A A ReturnToEmacs


;;; Installation:

;; To install, put this file in your load-path, and the following in
;; your .emacs file:

;; (load "fvwm-command")

;; Of course, you also need to be running FVWM with FvwmCommand, if
;; not doing so already, from your .fvwm2rc file.  You can do this by
;; adding the line
;;
;; Module FvwmCommandS
;;
;; to that file.

;; You should also customize the `fvwm-command-path' and
;; `fvwm-command-script-directory' variables.


;;; Commands and functions:

;; - `fvwm-command': specify an FVWM command to execute immediately
;; - `fvwm-command-find-script': like `find-file', but with completion
;; in your FVWM script directory
;; - `fvwm-command-load-script': load a FVWM script from your FVWM script
;; directory
;; - `fvwm-command-defun': simplify binding an FVWM command to an emacs
;; function
;; - `fvwm-command-define-key': simplify binding an FVWM command to an
;; emacs key
;;
;; Examples of Usage:
;; M-x fvwm-command RET Key F1 A A - RET
;; (fvwm-command-defun fvwm-move-to-window "MoveToWindow 1 1" "Raises X window")
;; (fvwm-command-define-key my-favourite-map "b" "Beep")
;; (fvwm-command-define-key tex-mode-map "\C-cg" "Next (gv) WarpToWindow 50 50")


;;; ChangeLog:

;; Revision 1.0 (2002/10/25)
;; * renamed many functions and variables, including a general
;;   switch from the short prefix `fvwm-' to the full prefix
;;   `fvwm-command', the location of the FvwmCommand script to
;;   `fvwm-command-path', and the name of the main command to
;;   simply `fvwm-command'.
;; * removed all the "recapture" business, under the assumption
;;   that anyone who chooses to use this package must like FVWM
;;   sufficiently to have a reasonably recent version, in which case
;;   this old feature is obsolete (and it makes the code so ugly).
;; * added another example to the Commentary.
;; * allowed for the `fvwm-command-*-buffer' variables to be nil, in
;;   which case the corresponding buffers are not created and the
;;   information is thrown away; changed the default of
;;   `fvwm-command-output-buffer' to nil.
;; * got rid of `fvwm-exec-command-internal'.
;; * improved the documentation a bit.
;; * cleaned up the mess in general.
;; * updated URL.



;;; Code:

;;; Customization variables:

(defgroup fvwm-command nil
  "Options affecting the fvwm-command package, an emacs interface to
FvwmCommand, which is in turn a command-line interface to FVWM.")

(defcustom fvwm-command-path "/usr/X11R6/bin/FvwmCommand"
  "Full path to the FvwmCommand executable.
This must be set properly, or nothing will work here."
  :type '(file :must-match t)
  :group 'fvwm-command)

(defcustom fvwm-command-script-directory "~/fvwm"
  "Location of your FVWM script directory.
It is not essential that this be set, but without it, fvwm-find-file and
fvwm-load-script will not work."
  :type 'directory
  :group 'fvwm-command)

(defcustom fvwm-command-log-buffer "*fvwm-log*"
  "Buffer in which to log the FvwmCommand calls.
If nil, FvwmCommand calls will not be recorded in a buffer."
  :type '(choice (string :tag "Buffer name")
                 (const :tag "none" nil))
  :group 'fvwm-command)

(defcustom fvwm-command-error-buffer "*fvwm-error*"
  "Buffer in which to log errors from FvwmCommand.
If nil, errors will not be logged in a buffer."
  :type '(choice (string :tag "Buffer name")
                 (const :tag "none" nil))
  :group 'fvwm-command)

(defcustom fvwm-command-output-buffer nil
  "Name of buffer to which to send standard output from FvwmCommand.
If nil, standard output will not be recorded in a buffer.

I don't see why this would ever be used.  But just in case, for the
sake of completeness, here it is."
  :type '(choice (string :tag "Buffer name")
                 (const :tag "none" nil))
  :group 'fvwm-command)


;;; Commands:

(defun fvwm-command (command)
  "Run a FVWM command using FvwmCommand."
  (interactive "sFVWM Command: ")
  (shell-command (concat fvwm-command-path " '" command "'")
                 fvwm-command-output-buffer
                 fvwm-command-error-buffer)
  (when fvwm-command-log-buffer
    (set-buffer (get-buffer-create fvwm-command-log-buffer))
    (insert command "\n")))

(defun fvwm-command-find-script (file)
  "Edit FILE in `fvwm-command-script-directory'."
  (interactive (list (fvwm-command-get-script)))
  (find-file file))

(defun fvwm-command-load-script (file)
  "Read FILE in `fvwm-command-script-directory' using FvwmCommand."
  (interactive (list (fvwm-command-get-script)))
  (fvwm-command (concat "Read " (expand-file-name file))))


;;; Macros:

(defmacro fvwm-command-defun (symbol command &optional docstring)
  "Define an emacs function which, when called, executes COMMAND.
The new function is that of SYMBOL, preceded with \"fvwm-command-\".
DOCSTRING is optional documentation for the new function."
  (let ((sym (intern (concat "fvwm-command-"
                             (symbol-name symbol)))))
    `(defun ,sym ()
       ,docstring
       (interactive)
       (fvwm-command ,command))))

(defmacro fvwm-command-define-key (keymap key command)
  "Define an emacs key binding to an FVWM command.
Bind KEY in KEYMAP to execute the FVWM command COMMAND.

Note that this is a quick and dirty way, because the command that you
define doesn't get a name or documentation string, only a keybinding
that invokes it from emacs."

`(define-key ,keymap ,key (lambda () (interactive)
                           (fvwm-command ,command))))


;;; Internal functions:

(defun fvwm-command-get-script ()
  "Ask the user to choose an FVWM script, and return the full path to it.
Possible choices are the existing scripts in
`fvwm-command-script-directory', but user can type something else as
well."
  (concat
   fvwm-command-script-directory
   "/"
   (completing-read "FVWM script: "
                    (mapcar
                     (lambda (x) (cons x nil))
                     (directory-files
                      fvwm-command-script-directory)))))


(provide 'fvwm-command)
