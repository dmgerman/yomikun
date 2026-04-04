;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel M German (dmg@turingmachine.org)

;; Author: Daniel M German (dmg@turingmachine.org)
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.2
;; Homepage: https://github.com/dmgerman/yomikun
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Dictionary lookup and display for yomikun.  Provides tooltip-based
;; definition display, kanji information, and a persistent lookup log.

;;; Code:

(require 'pos-tip)

;;; --- Configuration ---

(defvar yk-dict-command '("myougiden" "--human")
  "Command and arguments for dictionary lookup.  Term is appended.")

(defvar yk-kanji-dict-command '("kanji-dict.py")
  "Command and arguments for kanji lookup.  Term is appended.")

(defvar yk-tango-buffer-name "*yk-tango*"
  "Buffer name for the lookup log.")

(defvar yk-tango-entry-format "\n\n\n----Term: %s\n%s"
  "Format for log entries.  First %s is the term, second is the definition.")

(defvar yk-tooltip-timeout 10
  "How long to display tooltips, in seconds.")

;;; --- Tooltip Display ---

(defun yk-pad-first-line (str)
  "Pad first line of STR to the width of the longest line.
On macOS, pos-tip tooltips are as wide as the first line,
so shorter first lines would truncate longer subsequent lines."
  (let* ((lines (split-string str "\n"))
         (max-width (apply #'max (mapcar #'string-width lines)))
         (first-width (string-width (car lines)))
         (padding (max 0 (- max-width first-width))))
    (if (cdr lines)
        (concat (car lines)
                (make-string padding ?\s)
                "\n"
                (mapconcat #'identity (cdr lines) "\n"))
      str)))

(defun yk-tip-show (msg)
  "Display MSG in a tooltip near point."
  (pos-tip-show (yk-pad-first-line msg)
                nil nil nil
                yk-tooltip-timeout))

;;; --- External Command Runner ---

(defun yk-run-external-command (command term)
  "Run COMMAND with TERM appended as final argument.
COMMAND is a list of (program arg1 arg2 ...).
Returns the command output as a string."
  (if (and term (> (length term) 0))
      (with-temp-buffer
        (let ((exit-code (apply #'call-process
                                (car command) nil t nil
                                (append (cdr command) (list term)))))
          (if (= exit-code 0)
              (buffer-string)
            (format "Command failed (exit %d): %s"
                    exit-code (buffer-string)))))
    "no term given"))

(defun yk-run-dictionary (term)
  "Run dictionary lookup for TERM, return output string."
  (yk-run-external-command yk-dict-command term))

(defun yk-run-kanji-dictionary (term)
  "Run kanji dictionary lookup for TERM, return output string."
  (yk-run-external-command yk-kanji-dict-command term))

;;; --- Display ---

(defun yk-show-definition (term definition)
  "Show DEFINITION for TERM as tooltip, message, and in tango buffer."
  (yk-tip-show definition)
  (message "%s" definition)
  (with-current-buffer (get-buffer-create yk-tango-buffer-name)
    (goto-char (point-max))
    (insert (format yk-tango-entry-format term definition))
    (when-let ((win (get-buffer-window)))
      (set-window-point win (point-max)))))

;;; --- Text Extraction ---

(defun yk-extract-word-at-point ()
  "Extract the ASCII word at point."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun yk-extract-text ()
  "Extract text near point: Japanese text to next punctuation, or ASCII word."
  (save-excursion
    (let ((loc (point))
          (cur-char (char-to-string (char-after))))
      (if (yk-has-japanese-characters-p cur-char)
          (progn
            (re-search-forward "[[:punct:]]" nil t)
            (buffer-substring-no-properties loc (1- (match-beginning 0))))
        (yk-extract-word-at-point)))))

;;; --- Interactive Lookup Commands ---

(defun yk-define-at-point ()
  "Show definition of the morph under point in a tooltip.
If the text under point is not a morph, prompts for a term.
Keeps a log of searched words in the tango buffer."
  (interactive)
  (save-excursion
    (let* ((term (yk-extract-term-at-point))
           (definition (yk-run-dictionary term)))
      (if (> (length definition) 0)
          (yk-show-definition term definition)
        (message "Term [%s] not found" term)))))

(defun yk-kanji-at-point ()
  "Show kanji information of the morph under point in a tooltip.
If the text under point is not a morph, prompts for a term.
Keeps a log of searched words in the tango buffer."
  (interactive)
  (save-excursion
    (let* ((term (yk-extract-term-at-point))
           (definition (yk-run-kanji-dictionary term)))
      (if (> (length definition) 0)
          (yk-show-definition term definition)
        (message "Term [%s] not found" term)))))

(provide 'yomikun-dict)
;;; yomikun-dict.el ends here
