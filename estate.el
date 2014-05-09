;;; estate.el --- log parts of Emacs state 

;; Copyright (C) 2001 Cyprian Laskowski

;; Author: Cyprian Laskowski <swagbelly@yahoo.com>
;; Created: 11 June 2001
;; Version: 0.1
;; Keywords: help

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

;;; Installation:

;; Get the most recent version at http://www.swagbelly.net/elisp/lib/.

;; To install, put this file in your Emacs load-path and the following lines in
;; your .emacs file:

;; (require 'estate)

;; Next, customize it with "M-x customize-group estate".  You must at least
;; configure the `estate-directory' variable to a directory in which you want
;; the Emacs state logs to be saved.  However, you will probably want to
;; customize the other variables as well.

;;; Code:

(require 'cl)

;;; Customization variables

(defgroup estate nil
  "Emacs state library customization group."
  :group 'help)

(defcustom estate-directory "~/estate"
  "Directory where to store the estate log files."
  :type 'file
  :group 'estate)

(defcustom estate-log-variables '(features)
  "Important variables whose values should be logged by estate-log."
  :type '(repeat variable) 
  :group 'estate)

(defcustom estate-log-functions '(emacs-version)
  "Useful functions to run which estate-log should run and log the return values from."
  :type '(repeat function) 
  :group 'estate)

(defcustom estate-log-interval 300
  "Number of seconds before log attempts."
  :type 'number
  :group 'estate)

;;; Internal variables

(defvar estate-file nil)
(defvar estate-timer nil)

;;; Commands

(defun estate-daemon ()
  "Set an Emacs timer to update estate log every `estate-log-interval' seconds.
The timer object created is stored in `estate-timer'; if `estate-timer' is
non-nil, the timer is not created, and an error message is given instead."
  (interactive)
  (if estate-timer
      (error "Estate daemon already running. Type M-x estate-cancel to stop it first.")
    (setq estate-timer (run-at-time nil estate-log-interval 'estate-log))))
 
(defun estate-cancel ()
  "Cancel the `estate-timer' timer and set `estate-timer' to nil." 
  (interactive)
  (cancel-timer estate-timer)
  (setq estate-timer nil))

;;; Internal functions

(defun estate-log ()
  (interactive)
  (unless estate-file
    (setq estate-file (concat (file-name-as-directory estate-directory)
                              (generate-new-filename estate-directory "log"))))
  (with-temp-file estate-file
    (insert
     "Date:  " (format-time-string "%H:%M:%S - %D") "\n\n"
     "Variables:\n"
     "**********\n\n"
     (mapconcat (lambda (x) (estate-variable-value-string x))
                estate-log-variables
                "\n\n")
     "\n\n\n"
     "Functions:\n"
     "**********\n\n"
     (mapconcat (lambda (x) (estate-function-value-string x))
                estate-log-functions
                "\n\n"))))
  
(defun estate-function-value-string (func)
  (concat "(" (symbol-name func) ")\n\n"
          "=> " (prin1-to-string (funcall func)) "\n"))

(defun estate-variable-value-string (var)
  (concat (symbol-name var) "\n\n"
          "=> " (prin1-to-string (symbol-value var)) "\n"))

(defun generate-new-filename (dir ext)
  "Generate next filename in directory DIR with extension EXT.
The sequence is 1.EXT, 2.EXT, 3.EXT, ...  Does not actually create the file,
just returns the new name (with no path)."
  (concat
   (number-to-string
    (1+ (length (directory-files dir nil (concat "^[0-9]+\\." ext "$")))))
   "."
   ext))


(provide 'estate)

;;; estate.el ends here
