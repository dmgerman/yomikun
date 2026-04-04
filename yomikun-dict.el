;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 daniel german (dmg@turingmachine.org)

;; Author: Daniel M German (dmg@turingmachine.org)
;; Created: May 1, 2023
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.2
;; Homepage: https://github.com/dmgerman/yomikun
;; Package-Requires: ((emacs "27.1") )

;;; Commentary:


;;; Code:

;;;(require 'popup)
(require 'pos-tip)

;; Dictionary command: list of (program arg1 arg2 ...)
;; The lookup term is appended as the final argument.
(defvar yk-dict-command '("myougiden" "--human")
  "Command and arguments for dictionary lookup. Term is appended.")
(defvar yk-kanji-dict-command '("kanji-dict.py")
  "Command and arguments for kanji lookup. Term is appended.")

(defvar yk-tango-buffer-name "*yk-tango*")

;; format to log the entry. first is the looked-up term, second definition
(defvar   yk-tango-entry-format "\n\n\n----Term: %s\n%s")

;; how long to show the tooltip
(defvar   yk-tooltip-timeout 10)


(defun yk-longest-line-len (str)
  (let ((longest 0)
        (start 0))
    (while (string-match "\n" str start)
      (let ((end (match-end 0))
            (length (- (match-end 0) start 1)))
        (when (> length longest)
          (setq longest length))
        (setq start end)))
    (when (> (length str) start)
      (let ((length (- (length str) start)))
        (when (> length longest)
          (setq longest length))))
    longest))


(defun yk-pad-first-line (st)
  "this function pads the first line to match the length of the longest line

we need this because in osx the tooltips are as wide as the first line of text
thus, it would truncate longer lines

TODO: ideally, this should measure the width of the text, considering the
typeface to be used and wide/narrow chars width.
" 
  (let (
        (first-line-len (string-match "\n" st))
        )
    (if first-line-len
        (let*
            (
             (max-len (yk-longest-line-len st))
             (padding (if (> max-len first-line-len)
                          (- max-len first-line-len)
                        0))
             (rest (substring st first-line-len))
             )
;          (message "st [%s] first [%s] max [%s] padding [%s]" st first-line-len max-len padding)
          (if rest
              (format "%s%s\n%s\n"
                      (substring st 0 first-line-len)
                      (make-string padding ?-)
                      rest)
            st
            )
          )
      ;; else: return it unchanged
      st
      )
    )
  )

(defun yk-tip-show (msg)
  (pos-tip-show
  ;; if the tooltip is too small
  ;;     the tooltip will be as wide as the first line.
  ;;   so we need to pad the first line so the tooltip is wide enough for the definition
   (yk-pad-first-line msg)   ; string
   nil
   nil   ; pos
   nil   ; window
   10     ; timeout
   ))


(defun yk-run-external-command (command term)
  "Run COMMAND with TERM appended as final argument.
COMMAND is a list of (program arg1 arg2 ...).
Returns the command output as a string, or nil on error."
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


(defun yk-show-definition (term definition)
  "Show DEFINITION for TERM as tooltip, message, and in tango buffer."
  (yk-tip-show definition)
  (message definition)
  (with-current-buffer (get-buffer-create yk-tango-buffer-name)
    (goto-char (point-max))
    (insert (format yk-tango-entry-format term definition))
    (when-let ((win (get-buffer-window)))
      (set-window-point win (point-max)))))

(defun yk-extract-word-at-point ()
  "Extract the word at point and return it as a string. Only useful for non jp words"
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))


(defun yk-extract-text ()
  ;; extracts text under point.
  ;; if the current char is non-jp, it extracts the current "word"
  ;; if the text is japanese, extract to the next punctuation
  (save-excursion
    (let (
          (loc  (point))
          (cur-char (char-to-string (char-after)))
          )
      ;; if it is a
      (if (yk-has-japanese-characters-p cur-char)
          (progn ;;if
            (re-search-forward "[[:punct:]]" nil t)
            (buffer-substring-no-properties loc (- (match-beginning 0) 1))
            )
        (progn;; else
          (yk-extract-word-at-point)
          )
        )
      )
    )
  )
                                        
(defun yk-define-at-point ()
  "show definition of the morph under point in a tooltip and a message. If the text
under the point is not a morph, extracts most meaningful option and asks user for
confirmation.

Keeps a log  of searched words in a transient buffer too"
  (interactive)
  (save-excursion
    (let* (
           (term (yk-extract-term-at-point))
           (definition (yk-run-dictionary term))
           )
      (if (> (length definition) 0)
          (yk-show-definition term definition)
        ;; else
        (message (format "Term [%s] not found" term))
        )
      )
    )
  )
             
(defun yk-kanji-at-point ()
  "show kanji information of the morph under point in a tooltip and a message. If the text
under the point is not a morph, extracts most meaningful option and asks user for
confirmation.

Keeps a log  of searched words in a transient buffer too"
  (interactive)
  (save-excursion
    (let* (
           (term (yk-extract-term-at-point))
           (definition (yk-run-kanji-dictionary term))
           )
      (if (> (length definition) 0)
          (yk-show-definition term definition)
        ;; else
        (message (format "Term [%s] not found" term))
        )
      )
    )
  )



(provide 'yomikun-dict)
