;;; vocab-test.el --- foreign language vocabulary-testing program

;; Copyright (C) 2001 Cyprian Laskowski

;; Author: Cyprian Laskowski <swagbelly@yahoo.com>
;; Created: 21 Apr 2001
;; Version: 0.5
;; Keywords: language, international

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

;; Vocab Test is an Emacs alternative to flash cards for learning
;; vocabulary lists of foreign languages, although it could be used
;; for other purposes as well, like learning those intolerable
;; trigonometric formulas or some essential Emacs key bindings.  A new
;; reason never to leave Emacs.

;; There is a fair bit of customization that can optionally be done,
;; especially with respect to display formats, but to get a feel for
;; the purpose, setup and default configuration of Vocab Test, please
;; read through the "Quick Start" section here.


;;; Quick Start:

;; Scenario: Say you are a French speaker, and are currently taking a
;; course in Japanese.  You have a textbook with chapters numbered 1
;; through 10, each with its own vocabulary list.  You can use this
;; library to help learn this vocabulary.  To set up, put this file in
;; your emacs load-path, and the following in your .emacs file:

;; (autoload 'vocab-test "Test vocabulary of a foreign language." t)
;; (autoload 'vocab-test-edit "Edit a datafile for Vocab Test." t)

;; Then load this file or restart Emacs, and customize a few variables:
;; Type M-x customize-group vocab-test

;; Add two entries to vocab-test-languages:
;; ("French" . "latin-1-prefix")
;; ("Japanese" .  "japanese-hiragana")

;; Add an entry to vocab-test-labels:
;; ("Japanese-Course" "French" "Japanese" japanese-iso-8bit-with-esc-unix)

;; Create directories "~/vocab" and "~/vocab/Japanese-Course".

;; Type M-x vocab-test-edit, supplying as arguments "Japanese-Course" and
;; "test.txt" (completion works here).  Note that the buffer gets put into a
;; Japanese input method; you can toggle between the Japanese and French input
;; methods with C-c C-t (and, of course, type C-\ to toggle being in and out of
;; input methods altogether).

;; Now create some entries:

;; (You can get a bogus sample (bogus since I don't know any Japanese) at
;; http://www.swagbelly.net/elisp/lib/vocab-test-sample.txt.  Make sure that
;; you get it with an appropriate coding system, like
;; japanese-iso-8bit-with-esc-unix; doing this through Netscape seems to work.
;; There's also an unrelated but more realistic vocab-test-sample2.txt available
;; there.)

;; Lines beginning with `#' are comments.  Otherwise, the line is a word entry.
;; A word entry has the form:
;; meta-info; question word; answer word

;; (meta-info must have at least one character, but can be anything for now,
;; unless you look into `vocab-test-forms-all-numbers': this is mostly a
;; provision to allow for word type information being stored in the future.)

;; Once you have set up a few entries or downloaded my sample file, save it, and
;; type M-x vocab-test, providing the same arguments as before.

;; You're being tested!  Note that the input method is set to
;; "japanese-hiragana", which is what you specified above, and both the Japanese
;; and French characters appear properly (although this may depend on whether
;; you have appropriate fonts: there's a huge and wonderful set designed for
;; Emacs at ftp://ftp.gnu.org/pub/gnu/intlfonts/intlfonts-1.2.tar.gz; watch your
;; disk space!).

;; When you have completed the test, a log of the results is appended to the
;; new file ~/vocab/Japanese-Course/vocab-test.log.


;;; Features
;;
;; - automatic setting up of appropriate coding systems and input methods to
;; handle other languages properly, provided you've set some simple preferences
;; once.  See `vocab-test-languages' and `vocab-test-labels'.
;;
;; - randomized word selection, and recursive asking of incorrectly
;; answered questions until all words originally asked are answered correctly.
;; See `vocab-test-repeat-method' and `vocab-test-selection-method'
;;
;; - logging of test results.   See `vocab-test-log-method' and
;; `vocab-test-log-format'.
;;
;; - flexible and easily customizable format specifications for just about
;; anything that appears in Vocab Test buffers or logs.  See the various
;; `...-format' variables, and `vocab-test-process-format'.
;;
;; - many hooks, including test-type-specific ones.  See the various `...-hook'
;; variables.
;;
;; - togglable answer filtering mechanisms, so that you can have, for example,
;; long answer entries in your datafile, and gradually increase the amounts that
;; are actually tested as you learn.  See `vocab-test-forms-strict-flag',
;; `vocab-test-forms-strict-regexp', `vocab-test-forms-all-flag', and
;; `vocab-test-forms-all-numbers'.
;;
;; - a corresponding (albeit, not very developed) mode for easier editing of
;; VocabTest datafiles.  See `vocab-test-edit-mode'.
;;
;; - a frame parameters setting, which can be used to have Vocab Test open up a
;; new frame with the given set of parameters (such as a font) when it starts up
;; a new test.  See `vocab-test-frame-properties'.

;;; Test-specific hooks

;; I couldn't fit this documentation in elsewhere, so here it is.
;;
;; Each label you define for your tests (in `vocab-test-labels') is automatically
;; associated with a couple of hooks: `vocab-test-label-<label>-hook' and
;; `vocab-test-edit-<label>-hook', which are run after `vocab-test-start-hook'
;; and `vocab-test-edit-hook', respectively.  These allow you to have different
;; settings for these variables if you use Vocab Test for more than one kind of
;; test.  For example, if you wanted tests involving the label 'My-Label' to be
;; put into a separate frame, but not for other kinds of tests, you could put
;; the following into your .emacs file:
;;
;; (add-hook 'vocab-test-label-My-Label-hook
;;           (lambda ()
;;             (make-variable-buffer-local 'vocab-test-frame-properties)
;;             (setq vocab-test-frame-properties '((title . "My Label Test")))))

;;; Code:

(require 'cl)
;(require 'time-date)

;;; Custom type for customization format variables

(defconst vocab-test-format-type
  '(repeat :tag "Format" (choice
                          (string :tag "Literal string: ")
                          (list (const :tag "Date" date)
                                (string :tag "Format" format)
                                (boolean :tag "Universal" universal))
                          (const :tag "Answer position" marker)
                          (const :tag "Test label" label)
                          (const :tag "Source language" source)
                          (const :tag "Target language" target)
                          (const :tag "Chapter datafile" chapter)
                          (const :tag "Test size" test-size)
                          (const :tag "Round number" round)
                          (const :tag "Round size" round-size)
                          (const :tag "Round results" results)
                          (const :tag "Strict forms" strict)
                          (const :tag "All forms" all)
                          (const :tag "Number asked" asked)
                          (const :tag "Number remaining" remaining)
                          (const :tag "Number correct" correct)
                          (const :tag "Number wrong" wrong)
                          (const :tag "Percent correct" percent)
                          (const :tag "Question word" question)
                          (const :tag "Correct answer" answer)
                          (const :tag "Correct answer (filtered)" answer-filtered)
                          (const :tag "User's answer" yours)
                          (const :tag "User's answer (filtered)" yours-filtered)
                          (const :tag "newline" newline)
                          (function :tag "Custom function" special)
                          ))
  "Vocab Test customization type for 'vocab-test-...-format' variables.
Note that not all of these will make sense for every format, but I've used this
universal scheme partly out of laziness, and partly so that users can decide for
themselves what makes sense and what doesn't.  See `vocab-test-process-format'
for a description of the components.")

;;; Customization variables

(defgroup vocab-test nil
  "Foreign language vocabulary testing program"
  :group 'applications)

(defcustom vocab-test-directory "~/vocab/"
  "Root directory for Vocab Test datafiles."
  :type 'directory
  :group 'vocab-test)

(defcustom vocab-test-labels nil
  "A list of testing labels of languages.
Each element has the form (LABEL SOURCE TARGET CODING-SYSTEM).  LABEL is a
string identifying a testing scheme between two languages; its characters should
be valid both in a filename and in a Lisp variable name.  SOURCE and TARGET
should be names of languages taken from `vocab-test-languages': SOURCE is the
language to test from, and TARGET the language to test to.  CODING-SYSTEM is the
Emacs coding system to be used when reading or writing a Vocab Test datafile
pertaining to LABEL."
  :type '(repeat (list (string :tag "Label")
                       (string :tag "Source")
                       (string :tag "Target")
                       (symbol :tag "Coding system" :value iso-latin-1)))
  :group 'vocab-test)

(defcustom vocab-test-languages '(("English" . nil))
  "A list of languages for Vocab Test, with corresponding input methods.
Each element has the form (LANGUAGE . INPUT-METHOD).  LANGUAGE is an arbitrary
string identifying a language, and INPUT-METHOD should be a valid Emacs input
method.   When Vocab Test testing or editing is occurring, these methods are
automatically invoked as appropriate.  If INPUT-METHOD is nil, no input method
is used."
  :type '(alist :key-type (string :tag "Language")
                :value-type (choice (const nil)
                                    (string :tag "Input method")))
  :group 'vocab-test)

(defcustom vocab-test-frame-properties nil
  "Frame properties used when `vocab-test' is invoked.
If nil, the test occurs in the current frame.  If non-nil, a new frame is
created with these properties for the test.  Should be a valid association list,
passable to `make-frame'."
  :type '(alist :key-type string :tag "parameter"
                :value-type sexp :tag "value")
  :group 'vocab-test)

(defcustom vocab-test-buffer-name "*vocab-test*"
  "Name used for Vocab Test buffers."
  :type 'string
  :group 'vocab-test)

(defcustom vocab-test-test-size 50
  "Number of questions that a Vocab Test consists of.
This is only the size of the first round of a test, not subsequent rounds."
  :type 'integer
  :group 'vocab-test)

(defcustom vocab-test-word-selection-method 'random-unique
  "Manner by which the next word is chosen in a test.
linear means to go through the words in the order used in the datafile.
random means to pick words randomly.
random-unique means to pick words randomly, but uniquely, so that each word is
asked at most once per round."
  :type '(choice (const :tag "Linear" linear)
                 (const :tag "Random" random)
                 (const :tag "Random and unique" random-unique))
  :group 'vocab-test)

(defcustom vocab-test-repeat-method 'until-all
  "The method used to determine how many rounds a vocab test should go on for.
The value can be either the symbol until-all, or an integer.  If until-all,
Vocab Test will continue testing (starting new rounds) until all words asked in
the first round have been answered correctly.  If an integer, places a limit on
the number of rounds to be tested.  The test will stop at the latest at that
number of rounds, even if incorrectly answered questions still persist."
  :type '(choice :value until-all
                 (const :tag "Until all answered" until-all)
                 (integer :tag "Max number of rounds" :value 1))
  :group 'vocab-test)

(defcustom vocab-test-forms-strict-flag t
  "Non-nil means not to strip parts of an answer before checking it.
All occurrences of the regexp `vocab-test-forms-strict-regexp' are stripped.
This is a buffer-local variable."
  :type 'boolean
  :group 'vocab-test)

(make-variable-buffer-local 'vocab-test-forms-strict-flag)

(defcustom vocab-test-forms-strict-regexp "([^()]*)\\|_"
  "Regexp to filter out if `vocab-test-forms-strict-flag' is non-nil.
All occurrences of this regexp are stripped from both the user's answer and the
correct answer are removed before a comparison is made."
  :type 'regexp
  :group 'vocab-test)

(defcustom vocab-test-forms-all-flag t
  "Non-nil means to require all forms of a word when checking an answer.
`vocab-test-forms-all-numbers' determines how many forms to test for.  This is a
buffer-local variable."
  :type 'boolean
  :group 'vocab-test)

(make-variable-buffer-local 'vocab-test-forms-all-flag)

(defcustom vocab-test-forms-all-numbers nil
  "Correspondence between grammatical categories and numbers of forms to test.
Forms in the answer are delimited by `vocab-test-forms-separator'.  Only applies
if `vocab-test-forms-all-flag' is non-nil.  The value is a list, with each
element having the form (CATEGORY . NUMBER).  CATEGORY is the grammatical
category, determined by the first character in a datafile line.  NUMBER is the
number of forms to check.  If this variable applies, only the first NUMBER forms
will be verified during a comparison between user's answer and the correct
answer."
  :type '(alist :key-type (character :tag "Letter")
                :value-type (integer :tag "Number of forms"))
  :group 'vocab-test)

(defcustom vocab-test-comment-regexp "^#"
  "Regular expression used to recognize comments in a datafile line."
  :type 'string
  :group 'vocab-test)

(defcustom vocab-test-language-separator ";"
  "String used to separate entries in a Vocab Test datafile."
  :type 'string
  :group 'vocab-test)

(defcustom vocab-test-forms-separator "/"
  "String used to separate individual forms of an answer in a Vocab Test datafile."
  :type 'string
  :group 'vocab-test)

(defcustom vocab-test-source-language-first t
  "Non-nil if source words precede target words in datafiles.
Datafile entries are interpreted as using the sequence
(meta-info source-word target-word), or (meta-info target-word source-word),
depending on the value of this variable."
  :type 'boolean
  :group 'vocab-test)

(defcustom vocab-test-log-filename "vocab-test.log"
  "Name of logfile to use in a test's directory under `vocab-test-directory'."
  :type 'file
  :group 'vocab-test)

(defcustom vocab-test-log-method 'always
  "Degree of automatic logging of test results for Vocab Tests.
- 'always' means always log at the end of a test
- 'never' means to never log
- 'ask' means to prompt the user upon completion of a test and ask him whether
to log that particular test."
  :type '(choice (const :tag "Always" always)
                 (const :tag "Ask" ask)
                 (const :tag "Never" never))
  :group 'vocab-test)

(defcustom vocab-test-start-format
  '("Welcome to Vocab Test" newline newline
    "This is a test for " source " into " target ", chapter " chapter "."
    newline newline
    "Press any key to begin." newline newline)
  "Format used when a test is begun."
  :type vocab-test-format-type
  :group 'vocab-test)

(defcustom vocab-test-ask-question-format
  '(source ": " question newline
           target ": " marker newline
           "(" correct "/" asked ") <" round-size ">" newline
           "<" strict "/" all ">" newline newline)
  "Format used when a question is asked."
  :type vocab-test-format-type
  :group 'vocab-test)

(defcustom vocab-test-answer-correct-format
  '(newline newline "Ka'plah!" newline newline)
  "Format used when a question has been answered correctly."
  :type vocab-test-format-type
  :group 'vocab-test)

(defcustom vocab-test-answer-wrong-format
  '(newline newline "Shaka, when the walls fell." newline newline
            "The correct answer is: " answer-filtered newline
            "Your answer was:       " yours-filtered newline newline)
  "Format used when a question has been answered incorrectly."
  :type vocab-test-format-type
  :group 'vocab-test)

(defcustom vocab-test-round-format
  '(newline newline "This Round's Results: " newline newline
            "Correct: " correct newline
            "Total: " asked newline
            "Round results so far:" results newline newline)
  "Format used when a round comes to an end."
  :type vocab-test-format-type
  :group 'vocab-test)

(defcustom vocab-test-finish-format
  '(newline newline "Goodbye!" newline newline)
  "Format used when a test comes to an end."
  :type vocab-test-format-type
  :group 'vocab-test)

(defcustom vocab-test-log-format
  '((date "%B %d" nil) ": " label "Round results: " results newline)
  "Format used in log entries."
  :type vocab-test-format-type
  :group 'vocab-test)

(defcustom vocab-test-snapshot-format
  '("Language label: " label newline
    "Source language: " source newline
    "Target language: " target newline
    "Test size: " test-size newline
    "Round results: " results newline
    "Strict/lax: " strict newline
    "All/some: " all newline
    "----------" newline
    "This Round:" newline
    "Answered correctly: " correct newline
    "Questions asked: " asked newline
    "Round size: " round-size newline
    "Percent correct: " percent newline
    "----------" newline
    "This Question:" newline
    "Question: " question newline
    "Your answer: " yours newline
    "Your answer, filtered: " yours-filtered newline)
  "Format used for showing snapshots of current state and parameters of test."
  :type vocab-test-format-type
  :group 'vocab-test)

(defcustom vocab-test-load-hook nil
  "Functions to run after `vocab-test' is loaded."
  :type 'hook
  :group 'vocab-test)

(defcustom vocab-test-edit-mode-hook nil
  "Functions to run when `vocab-test-edit-mode' is invoked."
  :type 'hook
  :group 'vocab-test)

(defcustom vocab-test-mode-hook nil
  "Functions to run when `vocab-test-mode' is invoked."
  :type 'hook
  :group 'vocab-test)

(defcustom vocab-test-start-hook nil
  "Functions to run just before a test is begun."
  :type 'hook
  :group 'vocab-test)

(defcustom vocab-test-finish-hook nil
  "Functions to run right after a test is completed."
  :type 'hook
  :group 'vocab-test)

;;; Internal variables

(defvar vocab-test-source-language nil
  "Language being currently tested from.")

(make-variable-buffer-local 'vocab-test-source-language)

(defvar vocab-test-target-language nil
  "Language being currently tested into.")

(make-variable-buffer-local 'vocab-test-target-language)

(defvar vocab-test-label nil
  "Test (language label) of the current buffer.")

(make-variable-buffer-local 'vocab-test-label)

(defvar vocab-test-round nil
  "Current round number.")

(make-variable-buffer-local 'vocab-test-round)

(defvar vocab-test-questions-correct nil
  "Number of correct responses made in current round.")

(make-variable-buffer-local 'vocab-test-questions-correct)

(defvar vocab-test-questions-asked nil
  "Number of questions already asked in current round.")

(make-variable-buffer-local 'vocab-test-questions-asked)

(defvar vocab-test-source-files nil
  "List of datafiles from which current test was constructed.
Currently only the first entry is ever set or looked at, but I've made this into
a list because in the future I would like to be able to have tests using
multiple datafiles in one test.")

(make-variable-buffer-local 'vocab-test-source-files)

(defvar vocab-test-data-all nil
  "The data entries: an alist of source and target words for current round.")

(make-variable-buffer-local 'vocab-test-data-all)

(defvar vocab-test-data-round nil
  "The sequence of words being asked in the current round.")

(make-variable-buffer-local 'vocab-test-data-round)

(defvar vocab-test-data-incorrect nil
  "Words answered incorrectly in current round.
This is a sublist of `vocab-test-data-all'.")

(make-variable-buffer-local 'vocab-test-data-incorrect)

(defvar vocab-test-marker nil
  "Marker denoting starting position in buffer of expected answer.")

(make-variable-buffer-local 'vocab-test-marker)

(defvar vocab-test-test-in-progress nil
  "Whether or not a test is currently in progress.")

(make-variable-buffer-local 'vocab-test-test-in-progress)

(defvar vocab-test-round-size nil
  "Size of current round.")

(make-variable-buffer-local 'vocab-test-round-size)

(defvar vocab-test-frame nil
  "Frame of testing buffer.
nil if no special frame has been assigned.")

(make-variable-buffer-local 'vocab-test-frame)

(defvar vocab-test-word nil
  "Current word being tested.")

(make-variable-buffer-local 'vocab-test-word)

(defvar vocab-test-response nil
  "Current answer made by user.")

(make-variable-buffer-local 'vocab-test-response)

(defvar vocab-test-round-results nil
  "Round by Round results of last/current test.
This is simply a list of integers, of how many questions were answered correctly
in each round.")

(make-variable-buffer-local 'vocab-test-round-results)

(defvar vocab-test-start-time nil
  "Time at which test was begun.")

(make-variable-buffer-local 'vocab-test-start-time)

(put 'vocab-test-mode 'mode-class 'special)

(defvar vocab-test-mode-hook nil
  "Normal hook run when entering `vocab-test' mode.")

(defvar vocab-test-mode-syntax-table nil
  "Syntax table used while in `vocab-test-mode'.")
(unless vocab-test-mode-syntax-table
  (setq vocab-test-mode-syntax-table (make-syntax-table)))

(defvar vocab-test-mode-abbrev-table nil
  "Abbrev table used while in `vocab-test-mode'.")
(unless vocab-test-mode-abbrev-table
  (define-abbrev-table 'vocab-test-mode-abbrev-table ()))

(defvar vocab-test-mode-map nil
  "Keymap for `vocab-test-mode'.")
(unless vocab-test-mode-map
  (setq vocab-test-mode-map (make-sparse-keymap))
  (define-key vocab-test-mode-map "\C-c\C-n" 'vocab-test)
  (define-key vocab-test-mode-map "\C-a" 'vocab-test-beginning-of-line)
  (define-key vocab-test-mode-map "\C-m" 'vocab-test-submit-answer)
  (define-key vocab-test-mode-map "\C-c\C-c" 'vocab-test-chapter)
  (define-key vocab-test-mode-map "\C-c\C-r" 'vocab-test-retest)
  (define-key vocab-test-mode-map "\C-c\C-s" 'vocab-test-toggle-strict)
  (define-key vocab-test-mode-map "\C-c\C-d" 'vocab-test-display-snapshot)
  (define-key vocab-test-mode-map "\C-c\C-a" 'vocab-test-toggle-all-forms))

(defvar vocab-test-edit-mode-hook nil
  "Normal hook run when entering `vocab-test-mode'.")

(defvar vocab-test-edit-mode-syntax-table nil
  "Syntax table used while in `vocab-test-mode'.")
(unless vocab-test-edit-mode-syntax-table
  (setq vocab-test-edit-mode-syntax-table (make-syntax-table)))

(defvar vocab-test-edit-mode-abbrev-table nil
  "Abbrev table used while in `vocab-test-mode'.")
(unless vocab-test-edit-mode-abbrev-table
  (define-abbrev-table 'vocab-test-edit-mode-abbrev-table ()))

(defvar vocab-test-edit-mode-map nil
  "Keymap for `vocab-test-edit-mode'.")
(unless vocab-test-edit-mode-map
  (setq vocab-test-edit-mode-map (make-sparse-keymap))
  (define-key vocab-test-edit-mode-map "\C-c\C-t" 'vocab-test-toggle-input-methods)
  (define-key vocab-test-edit-mode-map "\C-c\C-i" 'vocab-test-edit-next-item)
  (define-key vocab-test-edit-mode-map "\C-ci" 'vocab-test-edit-prev-item))


;;; Commands

;;;###autoload
(defun vocab-test (label chapter)
  "Start a new vocab test using the language label LABEL and its datafile CHAPTER."
  (interactive
   (let* ((p (completing-read
              "Test label: "
              (mapcar (lambda (x) (cons (car x) nil)) vocab-test-labels)
              nil))
          (c (read-file-name
              "Chapter: "
              (concat (directory-file-name vocab-test-directory) "/" p "/")
              nil t)))
     (list p c)))
  (let* ((label-entry (assoc label vocab-test-labels))
         (source-language (nth 1 label-entry))
         (target-language (nth 2 label-entry))
         (coding-system (nth 3 label-entry))
         (input-method (cdr (assoc target-language vocab-test-languages)))
         (coding-system-for-read coding-system)
         (coding-system-for-write coding-system)
         (vocab-buffer (generate-new-buffer vocab-test-buffer-name))
         )
    (set-buffer vocab-buffer)
    (switch-to-buffer vocab-buffer)
    (vocab-test-mode)
    (run-hooks 'vocab-test-start-hook)
    (run-hooks (intern (concat "vocab-test-label-" label "-hook")))
    (when vocab-test-frame-properties
      (select-frame (make-frame vocab-test-frame-properties)))
    (setq vocab-test-label label
          vocab-test-source-language source-language
          vocab-test-target-language target-language
          vocab-test-source-files (list (file-name-nondirectory chapter))
          vocab-test-round 1
          vocab-test-questions-correct 0
          vocab-test-questions-asked 0
          vocab-test-test-in-progress t
          vocab-test-word nil
          vocab-test-response nil
          vocab-test-round-results nil
          vocab-test-data-all
          (vocab-test-datafile-read (file-name-nondirectory chapter))
          vocab-test-test-size
          (if
              (eq vocab-test-word-selection-method 'random)
              vocab-test-test-size
            (min vocab-test-test-size
                   (length vocab-test-data-all)))
          vocab-test-round-size vocab-test-test-size
          vocab-test-data-incorrect nil
          )
    (vocab-test-make-round-data-list)
    (insert (vocab-test-process-format vocab-test-start-format))
    (set-input-method input-method)
    (read-char "Press any key to begin")
    (setq vocab-test-start-time (current-time))
    (vocab-test-ask-question)))

;;;###autoload
(defun vocab-test-chapter (chapter)
  "Start a new test using the current label and datafile CHAPTER."
  (interactive
   (list (read-file-name "Chapter: "
                         (vocab-test-get-directory) nil t)))
  (vocab-test vocab-test-label chapter))

;;;###autoload
(defun vocab-test-retest ()
  "Restart the same vocab test currently in play.
The same language label and datafile are used, but not the same sequence of questions."
  (interactive)
  (vocab-test vocab-test-label (concat (vocab-test-get-directory)
                                      (car vocab-test-source-files))))

;;;###autoload
(defun vocab-test-mode ()
  "Mode for learning vocabulary of foreign languages."
  (interactive)
  (kill-all-local-variables)
  (use-local-map vocab-test-mode-map)
  (setq local-abbrev-table vocab-test-mode-abbrev-table)
  (set-syntax-table vocab-test-mode-syntax-table)
  (setq mode-name "Vocab-Test")
  (setq major-mode 'vocab-test-mode)
  (setq vocab-test-marker (make-marker))
  (run-hooks 'vocab-test-mode-hook))

;;;###autoload
(defun vocab-test-ask-question ()
  "Ask a new question during a Vocab Test.
The manner of selecting the question depends on
`vocab-test-word-selection-method'."
  (interactive)
  (if (not vocab-test-test-in-progress)
      (error "No vocabulary test in progress!")
    (setq vocab-test-word (nth vocab-test-questions-asked vocab-test-data-round))
    (narrow-to-region (point-max) (point-max))
    (insert (vocab-test-process-format vocab-test-ask-question-format))
    (add-text-properties (point-min) (1- vocab-test-marker) '(read-only t))
    (add-text-properties vocab-test-marker (1- (point-max)) '(read-only t))
    (goto-char vocab-test-marker)))

;;;###autoload
(defun vocab-test-submit-answer ()
  "Verify user's answer to the current question, and prepare for next question.
End the test if appropriate."
  (interactive)
  (if (not vocab-test-test-in-progress)
      (error "No vocab test in progress")
    (setq vocab-test-response (buffer-substring vocab-test-marker (point)))
    (let ((correct-answer (nth 2 vocab-test-word))
          (category (aref (nth 0 vocab-test-word) 0)))
      (goto-char (point-max))
      (if (string= (vocab-test-filter vocab-test-response category)
                   (vocab-test-filter correct-answer category))
          (progn
            (insert
            (vocab-test-process-format vocab-test-answer-correct-format))
            (incf vocab-test-questions-correct))
        (insert (vocab-test-process-format vocab-test-answer-wrong-format))
        (add-to-list 'vocab-test-data-incorrect vocab-test-word))
      (incf vocab-test-questions-asked)
      (if (= vocab-test-questions-asked vocab-test-round-size)
          (progn
            (setq vocab-test-round-results
                  (append vocab-test-round-results
                          (cons vocab-test-questions-correct nil)))
            (insert (vocab-test-process-format vocab-test-round-format))
            (if (or (null vocab-test-data-incorrect)
                    (and
                     (not (eq vocab-test-repeat-method 'until-all))
                     (<= vocab-test-repeat-method vocab-test-round)))
                (progn
;                  (message "Test took %d seconds." (seconds-elapsed (current-time) vocab-test-start-time))
                  (message "Test took %d seconds." (- (nth 2 (current-time)) (nth 2 vocab-test-start-time)))
                  (insert (vocab-test-process-format vocab-test-finish-format))
                  (if (or (eq vocab-test-log-method 'always)
                          (and (eq vocab-test-log-method 'ask)
                               (y-or-n-p "Log the test? ")))
                      (let ((log (vocab-test-process-format
                                  vocab-test-log-format))
                            (file (concat (vocab-test-get-directory)
                                          vocab-test-log-filename)))
                        (with-temp-buffer
                          (insert log)
                          (append-to-file (point-min) (point-max) file))))
                  (setq vocab-test-test-in-progress nil)
                  (run-hooks 'vocab-test-finish-hook))
              (setq vocab-test-questions-asked 0
                    vocab-test-questions-correct 0
                    vocab-test-round-size (length vocab-test-data-incorrect)
                    vocab-test-round (1+ vocab-test-round))
	      (vocab-test-make-round-data-list)
	      (setq vocab-test-data-incorrect nil)
              (read-char "Type any key for next round")
              (vocab-test-ask-question)))
        (when (not (string= (vocab-test-filter vocab-test-response category)
                            (vocab-test-filter correct-answer category)))
          (read-char "Type any key for next question"))
        (vocab-test-ask-question)))))

;;;###autoload
(defun vocab-test-toggle-strict ()
  "Toggle \"strict/lax\" testing.
`vocab-test-forms-strict-flag' is toggled; if nil, any matches of regexp
`vocab-test-forms-strict-regexp' are removed from both user's answer and correct
answer before they are compared."
  (interactive)
  (setq vocab-test-forms-strict-flag (not vocab-test-forms-strict-flag)))

;;;###autoload
(defun vocab-test-toggle-all-forms ()
  "Toggle \"all/some\" testing.
`vocab-test-forms-all-flag' is toggled; if nil, only the number of forms
corresponding to the grammatical category of `vocab-test-word' are used in both
the user's answer and correct answer during comparison.  This correspondence is
defined by `vocab-test-forms-all-numbers'."
  (interactive)
  (setq vocab-test-forms-all-flag (not vocab-test-forms-all-flag)))

;;;###autoload
(defun vocab-test-display-snapshot ()
  "Display a snapshot of testing parameters and information."
  (interactive)
  (save-window-excursion
    (let ((msg (vocab-test-process-format vocab-test-snapshot-format)))
      (switch-to-buffer-other-window
       (generate-new-buffer "*vocab-test-snapshot*"))
      (insert msg)
      (read-char "Press any key to continue: ")
      (kill-buffer buff))))

;;;###autoload
(defun vocab-test-beginning-of-line ()
  "Go to the beginning of the region in which the answer is given."
  (interactive)
  (goto-char vocab-test-marker))

;;;###autoload
(defun vocab-test-edit (label chapter)
  "Find datafile CHAPTER of language label LABEL.
Set up file for editing with vocab-test-edit-mode."
  (interactive
   (let* ((p (completing-read
              "Test label: "
              (mapcar (lambda (x) (cons (car x) nil)) vocab-test-labels)))
          (c (read-file-name
              "Chapter: "
              (concat (directory-file-name vocab-test-directory) "/" p "/"))))
     (list p c)))
  (let* ((label-entry (assoc label vocab-test-labels))
         (source-language (nth 1 label-entry))
         (target-language (nth 2 label-entry))
         (coding-system (nth 3 label-entry))
         (coding-system-for-read coding-system)
         (coding-system-for-write coding-system)
         (input-method (cdr (assoc target-language vocab-test-languages))))
    (find-file chapter)
    (vocab-test-edit-mode)
    (setq vocab-test-label label
          vocab-test-source-language source-language
          vocab-test-target-language target-language)
    (set-input-method input-method)
    (run-hooks (intern (concat "vocab-test-edit-" vocab-test-label "-hook")))))

;;;###autoload
(defun vocab-test-edit-mode ()
  "Mode for editing datafiles for Vocab Test.
This doesn't do much: sets up convenient toggling feature between input methods
for two languages, and a few movement-related keybindings."
  (interactive)
  (kill-all-local-variables)
  (use-local-map vocab-test-edit-mode-map)
  (setq local-abbrev-table vocab-test-edit-mode-abbrev-table)
  (set-syntax-table vocab-test-edit-mode-syntax-table)
  (setq mode-name "Vocab-Edit")
  (setq major-mode 'vocab-test-edit-mode)
  (run-hooks 'vocab-test-edit-mode-hook))

;;;###autoload
(defun vocab-test-edit-next-item ()
  "Go to the next item in a Vocab Test datafile."
  (interactive)
  (forward-char 1)
  (re-search-forward (concat "^\\|" vocab-test-language-separator "\\s-*\\S-"))
  (unless (looking-at "^") (backward-char 1)))

;;;###autoload
(defun vocab-test-edit-prev-item ()
  "Go to the previous item in a Vocab Test datafile.
This function doesn't always work quite right, and is a mess."
  (interactive)
  (let ((count
         (save-excursion
           (backward-char 1)
           (if (looking-at
                (concat "\\s-\\|" vocab-test-language-separator)) 2 1))))
    (backward-char 1)
    (re-search-backward
     (concat "^\\|" vocab-test-language-separator) nil t count)
    (unless (looking-at "^")
      (forward-char 1)
      (re-search-forward "\\s-*"))))

;;; Internal functions

;;;###autoload
(defun vocab-test-filter (word category)
  "Filter WORD with grammatical category CATEGORY.
How much filtering occurs depends on the settings of the trict/lax and all/some
testing features.  Settings are defined by `vocab-test-forms-strict-flag',
`vocab-test-forms-strict-regexp', `vocab-test-forms-all-flag', and
`vocab-test-forms-all-numbers'."
  (if (null word)
      ""
    (let ((strict-use vocab-test-forms-strict-flag)
          (strict-regexp vocab-test-forms-strict-regexp)
          (all-use vocab-test-forms-all-flag)
          (all-numbers vocab-test-forms-all-numbers)
          (separator vocab-test-forms-separator))
      (with-temp-buffer
        (insert (trim word))
        (goto-char (point-min))
        (when (not all-use)
          (let ((num (cdr (assoc category all-numbers))))
            (if (and num (re-search-forward separator nil t num))
                (delete-region (1- (point)) (point-max)))))
        (goto-char (point-min))
        (when (not strict-use)
          (while (re-search-forward strict-regexp nil t)
            (replace-match "")))
        (buffer-string)))))

;;;###autoload
(defun vocab-test-process-format (format)
  "Return a Vocab Test status string generated from FORMAT.
FORMAT is a list, the elements of which, when processed, generate a string.  The
strings are concatenated together, and the resultant string is returned.  Valid
elements for FORMAT are:

- a string: literal
- a list of 3 elements, whose car is the symbol 'date': the second element is a
valid time format of the type expected by `format-time-string', and the third
element, if non-nil, indicates that universal time is used, rather than local
time.  The resultant date string is used
- a function, which takes no arguments and returns a string; you're on your own
here, but if you want to dig into doing things yourself, here's a way.
- 'marker': this is a special symbol, it always results just in \"\"; but it
sets `vocab-test-marker' to the current position along in the string; this will
only work if the return to this function call is inserted in the current
position in the buffer
- 'label': the label corresponding to the test
- 'source': the name of the source language
- 'target': the name of the target language
- 'chapter': the filename (without directory) of the datafile being used for the
test
- 'test-size': the size of tests (i.e., the number of questions in the first
round)
- 'round': the current round number
- 'round-size': the total number of questions in the current round
- 'results': a space-delimited string of the number of correct answers in the
rounds of the test
- 'strict': whether the 'strict' feature is enabled (\"strict\") or disabled (\"lax\")
- 'all': whether the 'all' feature is enabled (\"al\") or disabled (\"some\")
- 'asked': the number of questions asked so far in the current round
- 'remaining': the number of questions yet to be asked in the current round
- 'correct': the number of questions answered correctly in the current round
- 'wrong': the number of questions ansered incorrectly in the current round
- 'percent': the percentage of questions answered correctly in the current round
- 'question': the current question being tested
- 'answer': the correct answer to the current question
- 'answer-filtered': like 'answer', but appropriately filtered with
'vocab-test-filter'
- 'yours': the user's answer to the current question
- 'yours-filtered': like 'yours', but appropriately filtered with
'vocab-test-filter'
- 'newline': just a newline, \"\\n\"; this is just here because I don't like the
way newlines normally appear in customization buffers

Of course, not all of these possibilities will make sense in every context."
  (let ((len 0))
    (mapconcat
     (lambda (elt)
       (let* ((raw
               (cond ((stringp elt) elt)
                     ((and (listp elt) (eq (car elt) 'date))
                      (format-time-string (nth 1 elt) nil (nth 2 elt)))
                     ((eq elt 'results)
                      (mapconcat (lambda (x) (number-to-string x)) vocab-test-round-results " "))
                     ((eq elt 'question) (nth 1 vocab-test-word))
                     ((eq elt 'answer) (nth 2 vocab-test-word))
                     ((eq elt 'answer-filtered)
                      (vocab-test-filter (nth 2 vocab-test-word)
                                         (aref (nth 0 vocab-test-word) 0)))
                     ((eq elt 'yours) vocab-test-response)
                     ((eq elt 'yours-filtered)
                      (vocab-test-filter vocab-test-response
                                         (aref (nth 0 vocab-test-word) 0)))
                     ((eq elt 'marker)
                      (progn (setq vocab-test-marker (+ (point) len)) ""))
                     ((eq elt 'label) vocab-test-label)
                     ((eq elt 'source) vocab-test-source-language)
                     ((eq elt 'target) vocab-test-target-language)
                     ((eq elt 'round) vocab-test-round)
                     ((eq elt 'strict)
                      (if vocab-test-forms-strict-flag "strict" "lax"))
                     ((eq elt 'all) (if vocab-test-forms-all-flag "all" "some"))
                     ((eq elt 'correct) vocab-test-questions-correct)
                     ((eq elt 'asked) vocab-test-questions-asked)
                     ((eq elt 'wrong)
                      (- vocab-test-questions-asked
                         vocab-test-questions-correct))
                     ((eq elt 'percent)
                      (format "%d" (* 100
                                      (/ (float vocab-test-questions-correct)
                                         vocab-test-questions-asked))))
                     ((eq elt 'remaining)
                      (- vocab-test-test-size vocab-test-questions-asked))
                     ((eq elt 'test-size) vocab-test-test-size)
                     ((eq elt 'round-size) vocab-test-round-size)
                     ((eq elt 'chapter) (car vocab-test-source-files))
                     ((eq elt 'newline) "\n")
                     ((functionp elt) (funcall elt))
                     (t (error "Invalid element for `vocab-test-process-format'"))))
              (result (cond ((stringp raw) raw)
                             ((numberp raw) (number-to-string raw))
                             ((null raw) "")
                             (t (error "Invalid type for `vocab-test-process-format'"))))
              (this-len (length result)))
         (setq len (+ len this-len))
         result))
     format
     "")))

;;;###autoload
(defun vocab-test-datafile-read (chapter)
  "Parse datafile CHAPTER, returning an alist of source and target words."
  (let ((curr-buff (current-buffer))
        (data-buff
         (find-file-noselect (concat (vocab-test-get-directory) chapter)))
        data)
    (set-buffer data-buff)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))
        (if (not (or (string-match vocab-test-comment-regexp line)
                     (string-match "^[\t ]*$" line)))
            (let* ((line-tokens
                    (split-string line (concat
                                        "[\t ]*"
                                        vocab-test-language-separator
                                        "[\t ]*")))
                   (meta-info (nth 0 line-tokens))
                   (word1 (nth 1 line-tokens))
                   (word2 (nth 2 line-tokens))
                   (source-word
                    (if vocab-test-source-language-first word1 word2))
                   (target-word
                    (if vocab-test-source-language-first word2 word1)))
              (setq data (cons (list meta-info source-word target-word) data))))
        (forward-line)))
    (kill-buffer data-buff)
    (set-buffer curr-buff)
    data))

;;;###autoload
(defun vocab-test-make-round-data-list ()
  (let* ((method vocab-test-word-selection-method)
	 (round1 (= vocab-test-round 1))
	 (data (if round1
		   vocab-test-data-all
		 vocab-test-data-incorrect)))
    (setq vocab-test-data-round
	  (cond ((eq method 'linear) data)
		((eq method 'random-unique) (randomize-list data))
		((eq method 'random) (random-list data))))))

;;;###autoload
(defun vocab-test-get-directory ()
  "Return the absolute path to the current testing directory."
  (interactive)
  (concat (directory-file-name vocab-test-directory) "/" vocab-test-label "/"))

;;; Some helpful functions, if not yet defined

(if (fboundp 'line-end-position)
    (message "WARNING: `line-end-position' is being redefined.")
  (defsubst line-end-position (&optional N)
    (save-excursion (end-of-line N) (point))))

(if (fboundp 'line-beginning-position)
    (message "WARNING: `line-beginning-position' is being redefined.")
  (defsubst line-beginning-position (&optional N)
    (save-excursion (beginning-of-line N) (point))))

(if (fboundp 'trim)
    (message "WARNING: `trim' is being redefined.")
  (defun trim (str)
    (if (not (string-match "\\S-" str))
        ""
      (string-match "\\(\\s-*\\)\\S-.*" str)
      (let ((s (replace-match "" nil nil str 1)))
        (string-match ".*\\S-\\(\\s-*\\)" s)
        (replace-match "" nil nil s 1)))))

(if (fboundp 'splice-nth-from-list)
    (message "WARNING: `splice-nth-from-list' is being redefined.")
  (defun splice-nth-from-list (index lst)
    (if (= index 0)
	(cdr lst)
      (cons (car lst) (splice-nth-from-list (1- index) (cdr lst))))))

(if (fboundp 'randomize-list)
    (message "WARNING: `randomize-list' is being redefined.")
  (defun randomize-list (lst)
    (unless (null lst)
      (let* ((index (random (length lst)))
	     (elt (nth index lst)))
	(cons elt (randomize-list (splice-nth-from-list index lst)))))))

(if (fboundp 'random-list)
    (message "WARNING: `random-list' is being redefined.")
  (defun random-list (lst n)
    (unless (= n 0)
      (let* ((index (random (length lst)))
	     (elt (nth index lst)))
	(cons elt (random-list lst (1- n)))))))

(defun seconds-elapsed (t1 t2)
  (time-to-seconds (subtract-time t1 t2)))

(run-hooks 'vocab-test-load-hook)

(provide 'vocab-test)

;;; vocab-test.el ends here
