(require 'popup)
(require 'pos-tip)

(defvar yk-dict-command "myougiden --human '%s'")

(defvar yk-tango-buffer-name "*tango*")

;; format to log the entry. first is the looked-up term, second definition
(defvar   yk-tango-entry-format "\n\n\n----Term: %s\n%s")

;; how long to show the tooltip
(defvar   yk-tooltip-timeout 10)


(defun yk-pad-first-line (st)
  "this function pads the first line to match the length of the second and an extra end of line" 
  (let*
      (
       (from  (string-match "\n" st))
       (pos2  (if from (string-match "\n" st (+ from 1)) nil))
       (len (if pos2 (- pos2 1) nil))
       )
    (if len
        (format "%s%s\n%s\n"
                (substring st 0 from)
                (make-string len ?-)
                (substring st from))
      st
      )))

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
  (dmg-jp-tip-show definition)
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
      (if (my-has-japanese-characters-p cur-char)
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
             
