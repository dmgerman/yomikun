;;; -*- lexical-binding: t; -*-

(require 'popup)
(require 'pos-tip)

(defvar yk-dict-command "myougiden --human '%s'")

(defvar yk-tango-buffer-name "*tango*")

;; format to log the entry. first is the looked-up term, second definition
(defvar   yk-tango-entry-format "\n\n\n----Term: %s\n%s")

;; how long to show the tooltip
(defvar   yk-tooltip-timeout 10)


(defun yk-longest-line-len (str)
  (let ((longest 0)
        (start 0))
    (while (string-match "\n" str start)
      (let ((end (match-end 0))
            (length (- (match-end 0) start 1) ) )
        (message "current match [%s][%s]" end length)
        (when (> length longest)
          (setq longest length))
        (setq start end))
      )
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
  (let*
      (
       (first-line-len (string-match "\n" st))
       (max-len (yk-longest-line-len st))
       (padding (if (> max-len first-line-len)
                    (- max-len first-line-len)
                  0))
       (rest (substring st first-line-len))
       )
    (message "st [%s] first [%s] max [%s] padding [%s]" st first-line-len max-len padding)
    (if rest
        (format "%s%s\n%s\n"
                (substring st 0 first-line-len)
                (make-string padding ?-)
                rest)
      st
      )
    
    ))

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


(defun yk-run-dictionary (term)
  (if (> (length term) 0)
      (shell-command-to-string (format  yk-dict-command term))
    "no term given"
    ))


(defun yk-show-definition (term definition)
  (yk-tip-show definition)
  (message definition)
  (get-buffer-create yk-tango-buffer-name)
  (with-current-buffer  yk-tango-buffer-name
    (let (
          (cur-window (get-buffer-window))
          )
      (goto-char (point-max))
      (insert (format yk-tango-entry-format term definition))
      (set-window-point cur-window (point-max))
      )
    ))

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
  "show definition of the currently selected word in a tooltip and a message. Keeps a log
   of searched words in a buffer too. Uses myougiden."
  (interactive)
  (save-excursion
    (let* (
           (term (yk-extract-term-at-point))
           (definition (yk-run-dictionary term))
           )
      (if (> (length definition) 0)
          (yk-show-definition term definition)
                                        ; else
        (message (format "Term [%s] not found" term))
        )
      )
    )
  )
             
