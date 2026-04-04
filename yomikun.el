;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel M German (dmg@turingmachine.org)

;; overlay code based on https://github.com/katspaugh/kuromoji.el
;; used with permission

;; Author: Daniel M German (dmg@turingmachine.org)
;; Created: May 1, 2023
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.2
;; Homepage: https://github.com/dmgerman/yomikun
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Yomikun brings yomichan/migaku-style features to Emacs for learning
;; Japanese.  It tokenizes Japanese text using mecab, tracks reading
;; status of words (known/unknown/learning/ignored), provides overlay
;; information about current tokens, and integrates dictionary support.

;;; Code:

(require 'pos-tip)
(require 'cl-lib)
(require 'yomikun-db)
(require 'yomikun-mecab)

;;; --- Customization Group ---

(defgroup yomikun nil
  "Japanese reading assistant using mecab."
  :group 'text
  :prefix "yk-")

;;; --- Configuration ---

(defcustom yk-debug nil
  "Enable debug messages when non-nil."
  :type 'boolean
  :group 'yomikun)

(defmacro yk-debug-message (format-string &rest args)
  "Print debug message if `yk-debug' is non-nil."
  `(when yk-debug
     (message ,format-string ,@args)))

(defcustom yk-max-tokens-to-process 10000
  "Maximum number of tokens to process per invocation.
Safety limit to prevent runaway processing on huge buffers."
  :type 'integer
  :group 'yomikun)



(defun yk-katakana-to-hiragana (str)
  "Convert katakana to hiragana. Very rough, but it works"
  (mapconcat
   (lambda (c)
     (if (and (>= c #x30a1) (<= c #x30f6))
         (char-to-string (+ c (- #x3041 #x30a1)))
       (char-to-string c)))
   str ""))

;;;###autoload
(defun yk-do-region (beg end &optional callback)
  "Process the region through mecab, applying morphological overlays.
When CALLBACK is non-nil, it is called after processing completes."
  (interactive "r")
  (yk-remove-props-and-overlays beg end)
  (yk-process-region beg end callback))

;;;###autoload
(defun yk-do-buffer (&optional callback)
  "Process the entire buffer through mecab.
When CALLBACK is non-nil, it is called after processing completes."
  (interactive)
  (yk-do-region (point-min) (point-max) callback))


;;;;;;;;;;;;;faces


(defface yk-face-unknown
  '((t ( :background "misty rose"
                )))
  "Face for default unknown text"
  :group 'yomikun)

(defface yk-face-learning
  '((t ( :background "#EAFFEA"
         )))
  "Face for default learning text"
  :group 'yomikun)

(defface yk-face-ignore
  '((t ( :background "gray95"
         )))
  "Face for default learning text"
  :group 'yomikun)

(defface yk-face-compound
  '((t ( :underline (:color "red" :style wave :position 3)
         )))
  "Face for default learning text"
  :group 'yomikun)

;; for grammatical entities

(defface yk-face-noun
  '((t (:inherit font-lock-string-face
                 )))
  "Face for noun unknown"
  :group 'yomikun
  )

(defface yk-face-noun-alt
  `((((class color) (background light))
     (:foreground  "dark blue"))
    (((class color) (background dark))
     (:foreground  "dark blue")))
  "Face for nouns alt."
  :group 'yomikun)

(defface yk-face-verb
  `((((class color) (background light))
     (:foreground  "SteelBlue3"))
    (((class color) (background dark))
     (:foreground  "blue")))
  "Face for verb"
  :group 'yomikun
  )

(defface yk-face-morpheme
  `((((class color) (background light))
     (:foreground  "magenta"))
    (((class color) (background dark))
     (:foreground  "darkgreen")))
  "Face for verbs."
  :group 'yomikun)

(defface yk-face-adverb
  `((((class color) (background light))
     (:foreground  "purple"))
    (((class color) (background dark))
     (:foreground  "purple")))
  "Face for adverbs."
  :group 'yomikun)

(defface yk-face-adjective
  `((((class color) (background light))
     (:foreground  "orange"))
    (((class color) (background dark))
     (:foreground  "orange")))
  "Face for adjectives."
  :group 'yomikun)

(defface yk-face-particle
  `((((class color) (background light))
     (:foreground  "darkgrey"))
    (((class color) (background dark))
     (:foreground  "darkgrey")))
  "Face for particles."
  :group 'yomikun)

(defface yk-face-punctuation
  `((((class color) (background light))
     (:foreground  "black"))
    (((class color) (background dark))
     (:foreground  "black")))
  "Face for particles."
  :group 'yomikun)

;; combined unknown and grammar point
(defface yk-face-noun-unknown
  '((t (:inherit ( yk-face-noun  yk-face-unknown))))
  "Face for noun unknown")

(defface yk-face-particle-unknown
  '((t (:inherit ( yk-face-particle  yk-face-unknown))))
  "Face for particle unknown")

(defface yk-face-verb-unknown
  '((t (:inherit ( yk-face-verb  yk-face-unknown))))
  "Face for verb unknown")

(defface yk-face-adverb-unknown
  '((t (:inherit ( yk-face-adverb  yk-face-unknown))))
  "Face for Adverb unknown")

(defface yk-face-punctuation-unknown
  '((t (:inherit ( yk-face-punctuation  yk-face-unknown))))
  "Face for punctuation unknown")

(defface yk-face-morpheme-unknown
  '((t (:inherit ( yk-face-morpheme  yk-face-unknown))))
  "Face for morpheme unknown")

(defface yk-face-adjective-unknown
  '((t (:inherit (yk-face-adjective yk-face-unknown)                
                 )))
  "Face for adjective unknown")

;; learning

(defface yk-face-noun-learning
  '((t (:inherit ( yk-face-noun  yk-face-learning))))
  "Face for noun learning")

(defface yk-face-particle-learning
  '((t (:inherit ( yk-face-particle  yk-face-learning))))
  "Face for particle learning")

(defface yk-face-verb-learning
  '((t (:inherit ( yk-face-verb  yk-face-learning))))
  "Face for verb learning")

(defface yk-face-adverb-learning
  '((t (:inherit ( yk-face-adverb  yk-face-learning))))
  "Face for Adverb learning")

(defface yk-face-punctuation-learning
  '((t (:inherit ( yk-face-punctuation  yk-face-learning))))
  "Face for punctuation learning")

(defface yk-face-morpheme-learning
  '((t (:inherit ( yk-face-morpheme  yk-face-learning))))
  "Face for morpheme learning")

(defface yk-face-adjective-learning
  '((t (:inherit (yk-face-adjective yk-face-learning)                
                 )))
  "Face for adjective learning")

;; the lookup tables

(defvar yk-wtype-table '(
                         ("名詞" . yk-face-noun)
                         ("助詞" . yk-face-particle)
                         ("動詞" . yk-face-verb)
                         ("副詞" . yk-face-adverb)
                         ("記号" . yk-face-punctuation)
                         ("助動詞" . yk-face-morpheme)
                         ("形容詞" . yk-face-adjective)
                         ))

(defvar yk-wtype-table-status-unknown '(
                         ("名詞" . yk-face-noun-unknown)
                         ("助詞" . yk-face-particle-unknown)
                         ("動詞" . yk-face-verb-unknown)
                         ("副詞" . yk-face-adverb-unknown)
                         ("記号" . yk-face-punctuation-unknown)
                         ("助動詞" . yk-face-morpheme-unknown)
                         ("形容詞"   . yk-face-adjective-unknown)
                         ))


(defvar yk-wtype-table-status-learning'(
                                        ("名詞" . yk-face-noun-learning)
                                        ("助詞" . yk-face-particle-learning)
                                        ("動詞" . yk-face-verb-learning)
                                        ("副詞" . yk-face-adverb-learning)
                                        ("記号" . yk-face-punctuation-learning)
                                        ("助動詞" . yk-face-morpheme-learning)
                                        ("形容詞" . yk-face-adjective-learning)
                                        ))


(defvar yk-font-table-default-status '(
;; known  does not have an entry, since it is not fontified
                                       ("unknown" . yk-face-unknown)
                                       ("learning" . yk-face-learning)
                                       ("ignore" .   yk-face-ignore)
                                       ))

(defun yk-font-table-wtype-to-use (status)
  "This is the core of the fontification.
   Depending on the status, return a given table.

   For ignore, we don't fonti

   Defaults to the unknown table

"
  (cond
   ((string-equal "known" status)     yk-wtype-table)
   ((string-equal "learning" status)  yk-wtype-table-status-learning)
   ((string-equal "ignore" status)    nil)
   (t yk-wtype-table-status-unknown)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some high order functions to
;;    process using pfun phrases and morphs in the current buffer
;;    whe the cmpfun returns tru

;; a phrase is any japanese text between punctuation

(defun yk-morph-do-phrases (cmpfun pfun)
  "process each phrase (pfun begin end) that satisfies (cmpfun begin)"
  (let ((pos (point-min)))
    (while pos
      (when (and (get-text-property pos 'yk-morph)
                 (funcall cmpfun pos))
        (funcall pfun pos (next-single-property-change pos 'yk-morph)))
      (setq pos (next-single-property-change pos 'yk-morph))
      )))


(defun yk-morph-do-morphs-in-region (beg end cmpfun pfun)
  "process each morph (pfun begin end) that satisfies (cmpfun begin)"
  (let ((pos beg))
    (while (and pos
            (< pos end))
;      (message "just  making sure [%s]" pos)
      (when (and (get-text-property pos 'begin)
                 (funcall cmpfun pos))
        (funcall pfun pos (next-single-property-change pos 'begin)))
      (setq pos (next-single-property-change pos 'begin))
      )))

(defun yk-get-tokens-region (beg end)
  "return a list of all the tokens in the region"
  (let(
       (lst (list))
       )
    (yk-morph-do-morphs-in-region beg end
                                  (lambda (pos) t)
                                  (lambda (pos end)
                                    (setq lst (cons
                                               (list pos
                                                     (get-text-property pos 'surface)
                                                     (get-text-property pos 'seen)
                                                     )
                                               lst))
                                    )
                                  )
    (nreverse lst)
    )
  )


(defun yk-has-japanese-characters-p (str)
  "return t if str has any Japanese characters."
  (not (string-match-p "\\`[[:ascii:]]*\\'" str))
  
  )



(defun yk-build-potential-candidates (lst len)
  ;; lst is a list of pairs (surface seen)
;  (message "build [%s] [%d]" lst len)
  (if (>= (length lst) len)
      ;;
      (let*  (
             (prefix  (string-join
                       ;; use seen  to build prefix
                       (mapcar (lambda (el) (nth 2 el))
                               (cl-subseq lst 0 (- len 1)))
                       ""
                       )
                      )
             (suffix   (nth (- len 1) lst))
;             (patito (message "suffix [%s]" suffix))
             (suffixes (list (nth 1 suffix) (nth 2 suffix)))
             )
;        (message ">candidate prefix [%d] [%s] [suffixes]" len prefix suffixes)
                                        ;prefix
        ;; in case surface is same as root
;;        (delete-dups suffixes)
        (mapcar
         (lambda (el) (concat prefix el))
         suffixes
         )
        )
    ;; else
    nil
      )
  )

(defun yk-find-compound (lst candidates)
  ;; see what is the longest substring
  ;; starting from position 0
  ;; that is found in candidates
  ;; return nil or (compound number-of-tokens)
  (let* (
         (n (length lst))
;;         (longest (longest-string candidates))
         (max  (apply 'max (mapcar 'length candidates)))
         (min  (apply 'min (mapcar 'length candidates)))
         (compound nil)
        )
    (while (and
            (not compound)
            (> n 1))
      (let* (
            (current (yk-build-potential-candidates lst n))
            (cur-surface (nth 0 current))
            (cur-seen    (nth 1 current))
            )
;        (message "           tryin [%s][%s] current [%s]" cur-surface cur-seen current)
;        (message "      candidates [%s]" candidates)
;;      avoid unnecessary tests
        (when (or (and (<= (length cur-surface) max) (>= (length cur-surface) min))
                  (and (<= (length cur-seen) max)   (>= (length cur-seen) min))
                  )
          ;; see if one matches
          (cond
           ((member cur-seen candidates)
            (progn
              (setq compound (list cur-seen n))
;              (message "Seen Foooooooooooooooooooooo [%s]" compound)
              )
            )
           ((member cur-surface candidates)
            (progn
              (setq compound (list cur-surface n))
;              (message "Sir Foooooooooooooooooooooo [%s]" compound)
              )
            )
           )
          )
        )
      (setq n (- n 1))
      )
    ;; return value
;;    (if compound
;;        (message "Found compound [%s] " compound)
;;    )
    compound
    
    )
  )

(defvar yk-compound-occurences 0)

(defun yk-find-compound-matches (lst)
  
  
  (let (
        (len (length lst))
        (not-done t)
        (offset 0)
        (result nil)
        ;; experimenting... remove later if unused
        (max-cur-compound-len 0)
        )
    ;; add dummy element to we can do the while loop
    ;; simplifies logic
    (setq lst (cons t lst))
    (while (and
            not-done
            (setq lst (cdr lst)) ;; consume the list
            
            )
      (let* (
             (candidates        (yk-build-potential-candidates lst 2))
             ;;            TODO this is slowing down things... but there might be situations
             ;; where a root ending of a compound of two might match
             ;; but maybe it is too small to worry about it?
             ;; but it might affect verbs primarily
;;           (surface-matches-p (yk-db-compound-exists (nth 0 candidates)))
             (db-candidates     (yk-db-compound-prefix-candidates (nth 1 candidates)))
             (compound          (and db-candidates
                                     (yk-find-compound lst db-candidates) 
                                     ))
             (compound-len (and compound
                                     (nth 1 compound)))
             (compound-st      (and compound
                                     (nth 0 compound)))
             )
;        (message "---------------matches %s" surface-matches-p)
;        (message "candidates %s" candidates)
;        (message "db exist %s" db-candidates)
        (when compound
;;          (message "Found match offset [%d][%s] [%s] len list [%d]"
;;                   offset
;;                   compound compound-len (length lst))
;;          (message "start [%s] last [%s]" (car lst) (nth (+ compound-len -1) lst))
          ;;; save starting point, end point and compound
          (push (list (car lst) (nth (+ compound-len -1 ) lst)
                      compound-st) result)
          (if (> compound-len max-cur-compound-len)
              (setq max-cur-compound-len compound-len)
              )
          (if (= compound-len (length lst))
              ;; no point on continuing. we have a match to the end of the list
              ;; there are other conditions where it is not possible to find matches
              ;; but it is already complex enough and the gains might be marginal
              (setq not-done nil)
              )
          )
        )
      (setq offset (+ offset 1))
;      (setq lst (cdr lst))
      )
    
;    (if result (message "Compounds: [%s]" result))
    result
    )
  )

(defun yk-mark-as-compound (lst)
  "Mark LST as a compound term with overlay and text properties."
  (setq yk-compound-occurences (+ 1 yk-compound-occurences))
  (let* ((beg-compound (car (nth 0 lst)))
         (end-token-pos (car (nth 1 lst)))
         (end-compound (+ (get-text-property end-token-pos 'end) -1))
         (st (nth 2 lst)))
    (yk-debug-message "beg [%s] end [%s] st [%s] len [%d]"
                      beg-compound end-compound st (length st))
    (yk-set-overlay-compound-at-pos beg-compound end-compound)
    ;; Each character may belong to multiple overlapping compounds,
    ;; so we must cons onto the existing property per-character.
    (let ((pos beg-compound))
      (while (<= pos end-compound)
        (put-text-property pos (1+ pos) 'compound
                           (cons st (get-text-property pos 'compound)))
        (setq pos (1+ pos))))))

(defun yk-process-compounds-in-phrase (beg end)
;  (message "sentence [%s]" (buffer-substring beg end))
  (let* (
         (p-tokens (yk-get-tokens-region beg end))
         (matches (yk-find-compound-matches p-tokens))
        )
    (yk-debug-message "tokens [%s]" p-tokens)
    (yk-debug-message "matches [%s]" matches)
    (when matches
      ;; reset overlay for compound
      (remove-text-properties beg end '(compound nil ))
      ;yk-face-compound          
;      (message "matches [%s]" matches)
      (mapc
       'yk-mark-as-compound
       matches
       )
      )
    matches
    )  
  )

(defun yk-do-all-compounds ()
  "Find and mark all compound terms in the buffer.
Requires the dictionary database to be configured."
  (interactive)
  (if (not yk-db-dict-file)
      (message "Yomikun: `yk-db-dict-file' not configured, skipping compound detection.")
    (setq yk-compound-occurences 0)
    (yk-morphs-delete-overlays-at-pos 'yomikun-comp (point-min) (point-max))
    (yk-db-dict-open)
    (with-silent-modifications
      (yk-morph-do-phrases
       (lambda (pos) t)
       'yk-process-compounds-in-phrase))
    (message "Done. Found [%s] compounds" yk-compound-occurences)))



(defun yk-morph-do-morphs (cmpfun pfun)
  "process each moph(beg end) that satisfies cmpfun(beg)
and call pfun on it"
  (let ((pos (point-min)))
    (while pos
      (when (and (get-text-property pos 'begin)
             (funcall cmpfun pos))
        (funcall pfun pos (next-single-property-change pos 'begin)))
      (setq pos (next-single-property-change pos 'begin))
      )))

(defun yk-morph-matches-at (morphProps pos)
  (let ((props (text-properties-at pos)))
     (and (string-equal (plist-get morphProps 'root)
                        (plist-get props 'root))
          (string-equal (plist-get morphProps 'wtype)
                        (plist-get props 'wtype))
          (string-equal (plist-get morphProps 'surface)
                        (plist-get props 'surface)))))

(defun yk-morphs-delete-overlays-at-pos (name beg end)
  "delete overlays between beg and end that have property name equal t"
  (yk-debug-message "deleting... overlays name [%s:%s ][%s]" beg end name)
  (remove-overlays beg end name t)
  )


(defun yk-remove-props-and-overlays (beg end)
  (interactive "r")
  ;; find all props and remove them
  ;; find all overlays and remove them
  (with-silent-modifications
   (let ((inhibit-read-only t)) ; allow modifying read-only text
    (remove-text-properties beg end '(root nil
                                           wtype nil
                                           surface nil
                                           pronun  nil
                                           begin   nil
                                           yk-morph nil
                                           end     nil
                                           status  nil
                                           ))
    (remove-overlays beg end 'yomikun t)
    (remove-overlays beg end 'yomikun-comp t)
  )))

(defun yk-morph-get-morph-from-props (props)
  "get the morph from the properties"
  (list
   (plist-get props 'root)
   (plist-get props 'wtype)
   (plist-get props 'surface)
   )
  )

(defun yk-morph-get-from-props (props attr)
  "get specific attribute from props
this might be redundant, but it is trying to create a layer between
properties and data
"
  (plist-get props attr)
  )

(defun yk-sort-hash-table (hash-table compare-func)
  "Return a sorted list of key-value pairs from HASH-TABLE.
The list is sorted using COMPARE-FUNC to compare elements."
  (let (pairs)
    (maphash (lambda (key value)
               (push (cons key value) pairs))
             hash-table)
    (sort pairs compare-func)))
                                        ;

(defun yk-extract-all-morphs ()
  "return a hashtable where the key is the morph, and the value the frequency"
  (let
      (
       (all-morphs (make-hash-table :test 'equal) )
       )
    (yk-morph-do-morphs
     (lambda (beg) (plist-get (text-properties-at beg) 'root))   ;; process all morphs
     (lambda (beg end)  ;; add morphs to the hashtable
       (let*
           (
            (props (text-properties-at beg))
            (morph (yk-morph-get-morph-from-props props))
            )
         (yk-debug-message "%s" props)
         ;; increase their counter by 1
         (puthash morph
                  (+ (gethash morph all-morphs 0) 1)
                  all-morphs)
         ))
     )
;    (yk-debug-message "[%s]" all-morphs)
    (yk-sort-hash-table all-morphs
                        (lambda (a b) (> (cdr a) (cdr b)))
                        )
    )
  )

(defun yk-report-all-morphs ()
  "Report the frequency of morphs in the current buffer."
  (interactive)
  (let ((morphs (yk-extract-all-morphs)))
    (mapc
     (lambda (m) (message "morphs [%s]" m))
     morphs)
    (message "total: %d" (length morphs))))

(defun yk-update-status-at-position (beg end wtype newstatus)
  "Update the overlay and status text property at BEG..END."
  (yk-set-overlay-wtype-at-pos beg end wtype newstatus)
  (with-silent-modifications
    (put-text-property beg end 'status newstatus)))


(defun yk-replace-all-overlays (props newstatus)
  ;;                                      ;
  ;; replace all the overlays of the given morph in all the document
  ;; according to the new status
  ;; 
  (yk-morph-do-morphs
   (lambda (beg)      (yk-morph-matches-at props beg  )););
   (lambda (beg end)  (yk-update-status-at-position beg
                                                    end
                                                    (plist-get props 'wtype)    
                                                    newstatus
                                                   )
     )
   )

  )

(defun yk-morph-new-status (props new-status)
  "Update the status of the morph described by PROPS to NEW-STATUS.
Updates the database, memoization cache, and all overlays in the buffer."
  (let* ((root (plist-get props 'root))
         (wtype (plist-get props 'wtype))
         (old-status (plist-get props 'status))
         (surface (plist-get props 'surface)))
    (if (not (string-equal old-status new-status))
        (progn
          (yk-debug-message "Setting status [%s] → [%s] for %s/%s/%s"
                            old-status new-status root wtype surface)
          (yk-morph-status-set root wtype surface new-status)
          (yk-replace-all-overlays props new-status))
      (yk-debug-message "Morph %s/%s/%s already has status [%s]"
                        root wtype surface new-status))))

(defun yk-wtype-status-to-face (wtype status)
  "Return the face for WTYPE and STATUS, or nil if none applies."
  (let* ((font-table (yk-font-table-wtype-to-use status))
         (face (assoc wtype font-table)))
    (if face
        (cdr face)
      (cdr-safe (assoc status yk-font-table-default-status)))))

(defun yk-set-overlay (name beg end face)
  (let ((ovl (make-overlay beg end)))
    (overlay-put ovl 'font-lock-face face)
    (overlay-put ovl name t)))

(defun yk-set-overlay-compound-at-pos (beg end)
  (yk-set-overlay 'yomikun-comp beg end 'yk-face-compound) 
  )

(defun yk-set-overlay-wtype-at-pos (beg end wtype status)
  (yk-morphs-delete-overlays-at-pos 'yomikun beg end)
  (let (
        (face (yk-wtype-status-to-face wtype status))
        )
    (if face
        (yk-set-overlay 'yomikun beg end face)
      )
    )
  )



(defun yk-set-text-prop-token (token)
  ;(message "setting prope [%s]" token)
  (let*
      (
       (beg (plist-get token 'begin))
       (root (plist-get token 'root))
       (wtype (plist-get token 'wtype))
       (seen (plist-get token 'seen))
       (surface (plist-get token 'surface))
       (status (yk-morph-status-get root wtype surface))
       (end (+ (plist-get token 'end) 1)) ;; ahh, it should 
;       (face (yk-wtype-status-to-face wtype status))
       )

    (yk-set-overlay-wtype-at-pos beg end wtype status)
    (when status
      (put-text-property beg end 'status status)
      (yk-debug-message ">>>>>>>>>status [%s]" status)
      (when (string-equal status "unknown")
        (put-text-property beg (+ 1 beg) 'cursor-sensor-functions (list #'yk-auto-help-at-point));
        )
      )

    (put-text-property beg end 'begin beg)
    (put-text-property beg end 'end end)
    (put-text-property beg end 'wtype  wtype)
    (put-text-property beg end 'root root)
    (put-text-property beg end 'surface surface)
    (put-text-property beg end 'yk-morph t)
    (put-text-property beg end 'seen seen)
  ;; i prefer hiragana to katanana
    (put-text-property beg end 'pronun
                       (yk-katakana-to-hiragana (plist-get token 'pronun)))))

(defun yk-process-wtype-p (token)
  (and
   (assoc (plist-get token 'wtype) yk-wtype-table)
;   (yk-has-japanese-characters-p (plist-get token 'surface))
   )  
  )

(defun yk-process-tokens(jpTokens)
  ;; make sure database is open
  (yk-db-status-open)
  (with-silent-modifications
    (dolist (token jpTokens)
      
      (if (yk-has-japanese-characters-p (plist-get token 'seen))
;;          (if (yk-process-wtype-p token)
              (yk-set-text-prop-token token)            
;;            )
        
        )    
      )
    )
  )

(defun yk-extract-term-at-point ()
    (interactive)
    (let* ((pos (point))
           (props (text-properties-at pos))
           (comp (and props;
                      (nth 0 (plist-get props 'compound))))
           (term (and props
                      (or
                       comp
                       (plist-get props 'root)
                       )
                      ))
           )
      (when (or (not term )
                (string-equal term ""))
        (setq term (read-string
                    "No japanese parsed text under point. Term to search: "
                    (yk-extract-text)) )
        )
      (when (or (not term )
                (string-equal term ""))
        (error "no term found or provided")
        )
      (yk-debug-message "[%s]" term)
      term
      )
    )
    
(defun yk-prop-at-point ()
  (interactive)
  (let* ((pos (point))
         (props (text-properties-at pos)))
    (message "Properties at position %d: %s" pos props)))

(defun yk-quick-dict-at (pos)
  "Show a quick dictionary tooltip for the morph at POS."
  (when yk-db-dict
    (let* ((props (text-properties-at pos))
           (root (plist-get props 'root)))
      (when root
        (let ((def (yk-dict-def root
                                (plist-get props 'pronun)
                                (plist-get props 'wtype))))
          (when def
            (yk-tip-show (format "%s" def))))))))

(defun yk-quick-dict-at-point ()
  (interactive)
  (yk-quick-dict-at (point))
)


(defun yk-auto-help-at-point (window oldpos dir)
  (if (equal dir 'entered)
;      (message "entering [%s] [%s] [%s] [%s]" window oldpos dir
;               (text-properties-at oldpos)
                                        ;               )
      (yk-quick-dict-at (point))
    )
  
)


(defun yk-properties-at-point ()
  (let* ((pos (point))
         (props (text-properties-at pos)))
    (if (plist-get props 'yk-morph)
        props
      nil
        )))
  
  
(defun yk-mark-at-point-as-ignored ()
  (interactive)
  (let* (
         (props (yk-properties-at-point)))
    (if props
        (yk-morph-new-status props "ignore")
        )
    )
  )

(defun yk-mark-at-point-as-known ()
  (interactive)
  (let* (
         (props (yk-properties-at-point)))
    (if props
        (yk-morph-new-status props "known")
      )
    )
  )

(defun yk-mark-at-point-as-unknown ()
(interactive)
(let* (
       (props (yk-properties-at-point)))
  (if props
      (yk-morph-new-status props "unknown")
    )
  )
)

(defun yk-mark-at-point-as-learning ()
  (interactive)
  (let* (
         (props (yk-properties-at-point)))
    (if props
        (yk-morph-new-status props "learning")
      )
    )
  )

(defun yk-buffer-parsed-p ()
  "Return non-nil if the current buffer has been parsed by yomikun."
  (text-property-not-all (point-min) (point-max) 'yk-morph nil))

(defun yk-process-region (beg end &optional callback)
  "Process the region BEG to END through mecab and apply token overlays.
Text is split into chunks to handle mecab's 8KB byte-offset limit.
When CALLBACK is non-nil, it is called with no arguments after completion."
  (yk-mecab--validate-binary)
  (yk-mecab--validate-dict-dir)
  (if (= beg end)
      (progn
        (message "Yomikun: empty region, nothing to process.")
        (when callback (funcall callback)))
    (let ((input-text (buffer-substring-no-properties beg end)))
      (let ((tokens (yk-mecab--parse-output-chunked input-text beg)))
        (yk-debug-message "Parsed %d tokens" (length tokens))
        (yk-process-tokens tokens)
        (when callback
          (funcall callback))))))

(defun yk-visit-site-with-param (base-url parm)
  (let ((url (format base-url (url-hexify-string parm))))
    (browse-url url)))

(defun yk-jisho-at-point ()
  (interactive)
  (save-excursion
    (let* (
           (term (yk-extract-term-at-point))
           )
      (if term
          (yk-visit-site-with-param "https://jisho.org/search/%s" term)
          )
      )
    )
  )

(defun yk-kanji-damage-at-point ()
  (interactive)
  (save-excursion
    (let* (
           (kanji (char-to-string (char-after)))
           )
      ;; todo, check that it is a kanji
      (if kanji
          (yk-visit-site-with-param "http://www.kanjidamage.com/kanji/search?utf8=✓&q=%s" kanji)
        )
      )
    )
  )



(defun yk-mark-sentence-at-point ()
  "Select the sentence around the point delimited by newline and/or 。."
    (interactive)
    (let ((beg (save-excursion
;;                 (backward-char) ;; move one char back to start selection at beginning of sentence
                 (skip-chars-backward "^。\n") ;; move back to beginning of sentence
                 (point)))
          (end (save-excursion
 ;;                (forward-char) ;; move one char forward to end selection at end of sentence
                 (skip-chars-forward "^。\n") ;; move forward to end of sentence
                 (point))))
      (if (eq (char-after end) ?。)
          (setq end (+ end 1)))
      (set-mark beg)
      (goto-char end)))



(defvar yk-minor-map (make-sparse-keymap)
  "Keymap used yk-minor mode")

(defun yk-disable-mode ()
  (interactive)
  (message "Exiting yk-minor-mode")
  (cursor-sensor-mode -1)
  (yk-minor-mode -1)
  )

(define-key yk-minor-map (kbd "=") 'yk-mark-sentence-at-point)
(define-key yk-minor-map (kbd "p") 'yk-prop-at-point)
(define-key yk-minor-map (kbd "i") 'yk-mark-at-point-as-ignored)
(define-key yk-minor-map (kbd "u") 'yk-mark-at-point-as-unknown)
(define-key yk-minor-map (kbd "k") 'yk-mark-at-point-as-known)
(define-key yk-minor-map (kbd "l") 'yk-mark-at-point-as-learning)
(define-key yk-minor-map (kbd "RET") 'yk-define-at-point)
(define-key yk-minor-map (kbd "t") 'yk-extract-term-at-point)
(define-key yk-minor-map (kbd "j") 'yk-jisho-at-point)
(define-key yk-minor-map (kbd "n") 'yk-kanji-at-point)
(define-key yk-minor-map (kbd "m") 'yk-kanji-damage-at-point)
(define-key yk-minor-map (kbd "x") 'yk-disable-mode)

;;;###autoload
(define-minor-mode yk-minor-mode
  "Minor mode for interacting with yomikun-processed Japanese text.
Provides keybindings for marking word status, looking up definitions,
and navigating Japanese text.

When activated, automatically parses the buffer and finds compounds
if the buffer has not already been processed."
  :global nil
  :lighter " yk"
  :keymap yk-minor-map
  (if yk-minor-mode
      (if (yk-buffer-parsed-p)
          (cursor-sensor-mode 1)
        (message "Yomikun: parsing buffer...")
        (yk-do-buffer
         (lambda ()
           (yk-do-all-compounds)
           (cursor-sensor-mode 1)
           (message "Yomikun: ready."))))
    (cursor-sensor-mode -1)))

(provide 'yomikun)
;;; yomikun.el ends here
