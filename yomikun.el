;;; -*- lexical-binding: t; -*-
;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 daniel german (dmg@turingmachine.org)

;; overlay code based on https://github.com/katspaugh/kuromoji.el
;; used with permission

;; Author: Daniel M German (dmg@turingmachine.org)
;; Created: May 1, 2023
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.2
;; Homepage: https://github.com/dmgerman/yomikun
;; Package-Requires: ((emacs "27.1") )




(require 'pos-tip)
(require 'emacsql)

(defvar yk-mecab-command "echo" "Shell command to run on buffer contents")
(defvar yk-process-name "jp-process")

;; string used by emacs to delimit end of process
(defvar yk-process-end-st "\nProcess")

(defvar yk-debug nil "Enable debug messages when non-nil")

(defmacro yk-debug-message (format-string &rest args)
  "Print debug message if yk-debug is non-nil."
  `(when yk-debug
     (message ,format-string ,@args)))

(defvar yk-db-status-file "~/yk-status.db")
(defvar yk-db-dict-file nil) 

(defvar yk-max-tokens-to-process 10000)


(defvar yk-process-buffer "*yk-process*" "Name of buffer for shell command process")
(defvar yk-report-buffer "*yk-report*" "Name of buffer for report")


(defvar yk-date-format "%Y-%m-%d %H:%M" "Format to use when logging to the status database")

(defvar yk-db-dict nil)   ;; instance of the quick dictionary db
(defvar yk-db-status nil) ;; instance of the status db

;; TODO move my own configuration somewhere else

(setq yk-mecab-command  "/Users/dmg/bin/osx/m-mecab.sh")
(setq yk-max-tokens-to-process 100000)
(setq yk-db-status-file "~/jp-status.db")
(setq yk-db-dict-file "~/dictionary.db")


(defun yk-db-status-open ()
  (interactive)
    (when (not (file-exists-p yk-db-status-file))
      (signal 'file-error (format "the file does not exist [%s]" yk-db-status-file))
      )
    ;; only open it if it is not open
    (when (not yk-db-status)
     (setq yk-db-status (emacsql-sqlite-open yk-db-status-file)))
    )

(defun yk-db-dict-open ()
  (interactive)
  (when (not (file-exists-p yk-db-dict-file))
    (signal 'file-error (format "the dictionary file does not exist [%s]" yk-db-dict-file))
    )
  ;; only open it if it is not open
  (when (not yk-db-dict)
    (setq yk-db-dict (emacsql-sqlite-open yk-db-dict-file)))
  )

(defun yk-db-dict-close ()
  (emacsql-close yk-db-dict)
  )

(defun yk-db-status-close()
  (emacsql-close yk-db-status)
  )

(defun yk-get-time-date ()
  (format-time-string yk-date-format (current-time))
  )

(defun yk-db-status-create ()
  "Create status database"
  (interactive)
  (message "creating status database at [%s]" yk-db-status-file)
  (when (file-exists-p yk-db-status-file) 
    (signal 'file-error (format "the file already exist [%s]" yk-db-status-file))
    )
  (setq yk-db-status (emacsql-sqlite-open yk-db-status-file))
  (emacsql yk-db-status [:create-table words ([morph mtype surface status date]
                                              (:primary-key [morph mtype surface])
                                              )])
  )

(defun yk-db-dict-def (root pronun wtype)
  ;; try to fin all markers... if not, match 
  (let* (
         ;; search first with wtype
         (with-all (nth 0
                          (emacsql yk-db-dict [:select [gloss pos root reading wtype]
                                                       :from entries
                                                       :where (and (= root $s1) (= reading $s2)
                                                                   (= wtype $s3))
                                                       :limit 1
                                                       ] root pronun wtype) ))
         ;; if not found search without it
         (without-type (or with-all
                   (nth 0
                        (emacsql yk-db-dict [:select [gloss pos root reading wtype]
                                                     :from entries
                                                     :where (and (= root $s1) (= reading $s2) )
                                                     :limit 1
                                                     ] root pronun) )))
         (result (or without-type
                     (nth 0
                          (emacsql yk-db-dict [:select [gloss pos root reading wtype]
                                                       :from entries
                                                       :where (= root $s1)
                                                       :limit 1
                                                       ] root ) )))
         )
    result
    )
  
   )

;; this is a memoization of the compounds queries
;; it does not seem to make an impact worth the memory it uses
;; currently unused
(setq yk-compound-candidates-table (make-hash-table :test 'equal))

(defun yk-compound-prefix-candidates (st)
  (or (gethash st yk-compound-candidates-table)
      (let
          (
           (comps (yk-db-compound-prefix-candidates st))
           )
        (puthash st
                 comps
                 yk-compound-candidates-table)        
        )        
      )
  )

(defun yk-db-compound-prefix-candidates (st)
  "Return a list of compounds that have as prefix the string st"
  (when (> (length st) 2)
    (setq st (concat st "%"))
    ;;    (message "                  searhing for candidates [%s]" st)
    ;; remove the list wrapping each string
    (mapcar
     (lambda (ls) (nth 0 ls))
     (emacsql yk-db-dict [:select [compound]
                                  :from compounds
                                  :where (like compound $s1)
                                  :order-by [(desc (length compound))]
                                  ] st) 
     )
    
    )
  )

(defun yk-db-compound-exists (st)
  "Check if a given compound exists"
  (and
   (emacsql yk-db-dict [:select [compound]
                                :from compounds
                                :where (= compound $s1)
                                ] st) 
   
   )
  )


;; hash used for memoization of dictionary lookups
;; currently unused
(setq yk-dict-table (make-hash-table :test 'equal))

(defun yk-dict-def (root pronun wtype)
  (or (gethash (list root pronun wtype) yk-dict-table)
      (let
          (
           (def (yk-db-dict-def root pronun wtype))
           )
        (puthash (list root pronun wtype)
                 def
                 yk-dict-table)        
        )        
      )
  )

(defun yk-db-morph-status-get (root wtype surface)
  "return the tuple that matches the given root wtype and
surface. The three attribuetes are the primary key, so
return match as a list or nil if not found
"
  ;; since it is a singleton, remove wrapping list
  ;; nth returns nil if the list is empty
  ;;  (message "get db [%s %s %s]" root wtype surface)
  
  (or (nth 0;
       (emacsql yk-db-status [:select [morph mtype status date]
                                      :from words
                                      :where (and (= morph $s1) (= mtype $s2) (= surface $s3))
                                      ] root wtype surface) 
       )
      (list nil nil "unknown" nil))
  )

(defun yk-db-morph-status-delete (root wtype surface)
  "delete given morph, if it exists"
  (emacsql yk-db-status [:delete 
                            :from words
                            :where (and (= morph $s1) (= mtype $s2) (= surface $s3))
                          ] root wtype surface)
  )

(defun yk-db-morph-status-insert (root wtype surface status)
  "insert new tuple"
  (let (
        (today (yk-get-time-date))
        )
                                        ;
     (emacsql yk-db-status [:insert :into words
                              :values ([$s1 $s2 $s3 $s4 $s5])] root wtype surface status today)
     ))

(defun yk-db-morph-status-update (root wtype surface status)
  "update new tuple by deleting it, then inserting"
  (yk-db-morph-status-delete root wtype surface)
  
  (if (not (string-equal status "unknown"))
   (yk-db-morph-status-insert root wtype surface status))
  )

;; initialize hash table
(setq yk-status-table (make-hash-table :test 'equal))

;; counter for profiling purposes
(setq yk-repeat-counter 0)

(defun yk-morph-status-get (root wtype surface)
  "memoized function that checks for the status of a given morph."
  ;; memoize the status of the given morph
  (or (gethash (list root wtype surface) yk-status-table)
      (let
          (
           (status (nth 2
                        (yk-db-morph-status-get root wtype surface)))
           )
        (setq yk-repeat-counter (+ yk-repeat-counter 1))
;        (message "gone to the db [%s]" status)
        (puthash (list root wtype surface)
                 status
                 yk-status-table)        
        )        
    ))

(defun yk-morph-status-set (root wtype surface status)
  "stores new status in db if needed, updating memoization table"
;  (message "entering")
  (let(
       (curval (yk-morph-status-get root wtype surface))
       )
    (unless (and curval
                 (string-equal curval status)
             )
      ;; cache does not have it or it is different
      (yk-db-morph-status-update root wtype surface status)
      (puthash (list root wtype surface) status yk-status-table)
      )
    ))


(defun yk-katakana-to-hiragana (str)
  "Convert katakana to hiragana. Very rough, but it works"
  (mapconcat
   (lambda (c)
     (if (and (>= c #x30a1) (<= c #x30f6))
         (char-to-string (+ c (- #x3041 #x30a1)))
       (char-to-string c)))
   str ""))

(defun yk-do-region (beg end)
  "jk-ize the region"
  (interactive "r")
  ;; remove current info in region
  (yk-remove-props-and-overlays beg end)
  
  (if (bufferp yk-process-buffer)
      (kill-buffer yk-process-buffer))

  ;; do the work
  (yk-process-region beg end)
  )

(defun yk-do-buffer ()
  "jk-ize the region"
  (interactive)
  (yk-do-region (point-min) (point-max))
  )


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
  :group 'my
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
  :group 'my
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
   (yk-wtype-table-status-unknown)
   )
  )
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
                               (subseq lst 0 (- len 1)))
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
  ;; this function is probably slower than it can be
  ;; but it not executed a lot
  (setq yk-compound-occurences (+ 1 yk-compound-occurences))

  (let*(
       (beg-compound (car (nth 0 lst)))
       (end-token-pos   (car (nth 1 lst)) )
       (end-compound    (+ (get-text-property end-token-pos 'end) -1))
       (st  (nth 2 lst))
       )
    (yk-debug-message "beg [%s] end [%s] st [%s] len [%d]" beg-compound end-compound st (length st))
    (yk-set-overlay-compound-at-pos beg-compound end-compound)
    (dolist (pos (number-sequence beg-compound end-compound))
      (let (
            (cur-prop (get-text-property pos 'compound))
            )
        (yk-debug-message "setting [%d] with [%s] current [%s]" pos st cur-prop)
        (put-text-property pos (+ 1 pos) 'compound (cons st cur-prop ))
        )
      )
    )
  )

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
  (interactive)
  (setq yk-compound-occurences 0)
  (yk-morphs-delete-overlays-at-pos 'yomikun-comp (point-min) (point-max))
  (yk-db-dict-open)
  (with-silent-modifications 
    (yk-morph-do-phrases
     (lambda (pos) t)
     'yk-process-compounds-in-phrase
     ))
  (message "Done. Found [%s] compounds" yk-compound-occurences)
  )



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
  (let ((pos (point))
         (props (text-properties-at pos))
         )
;    (progn
     (and (string-equal (plist-get morphProps 'root)
                        (plist-get props 'root)
                        )
          (string-equal (plist-get morphProps 'wtype)
                        (plist-get props 'wtype)
                        )
          (string-equal (plist-get morphProps 'surface)
                        (plist-get props 'surface)
                        )
          )
     )
;    )
  
  )

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

(setq yk-test-all-morphs (make-hash-table :test 'equal));(list))
;;(setq yk-status-table (make-hash-table :test 'equal))

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
  "report the frequency of morphs"
  (interactive)
  (let (
        (morphs (yk-extract-all-morphs))
;        (buffer (create... yk-report-buffer))
        )
    
    (mapc 
     (lambda (m) (message "morphs [%s]" m))
     morphs
     )
    (message "total: %d" (length morphs))
    )
  )

(defun yk-update-status-at-position (beg end wtype newstatus)
  ;;
  ;;  replace the overlays at the given position
  ;;  according to the new status
  ;;
  (yk-set-overlay-wtype-at-pos beg end wtype newstatus )
)


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
  ;; mark the morph in props as new status
  ;; in the database and in the database
  ;; 
  (let* (
         (root (plist-get props   'root))
         (wtype (plist-get props  'wtype))
         (old-status (plist-get props 'status))
         (surface (plist-get props 'surface))
         )
;    (message "after let [%s] [%s] [%s]-> [%s] " root wtype status new-status)
;    (message "   begin end [%s] [%s]" beg end)
    (if (not (string-equal old-status new-status))
        (progn
          (message "setting new status [%s] old [%s]" new-status old-status)
          (message "   root [%s] wtype [%s] surface [%s]" root wtype surface)
          ; we need to set the status of that word everywhere...
          (yk-morph-status-set root wtype surface new-status)
          ;; now we have to process the document to find instances of the word
          ;; ... there is a need for a redo buffer
          ;; but that is expensive, soo simply scan the document
          ;; and find any instances of the morph
          ;; and change its property
          ;; first remove all overlays in the file
          (yk-replace-all-overlays props new-status)
          )
      (message "morph (%s %s %s)already has [%s] status" root wtype surface new-status)
      )
    ))

;; (defun yk-mark-morph-as-known (beg end)
  
;;   )

;; (defun yk-morph-set-known ()
;;   (interactive)
;;   (let* (
;;          (pos (point))
;;          (props (text-properties-at pos))
;;          )
;;     (if (and props 
;;              (plist-get props 'root)
;;              )
;;         (progn ;
;;           (yk-morph-new-status props "known")

;;           (yk-morph-do-morphs
;;            (lambda (beg) (yk-morph-matches-at props beg  )););
;;            'yk-mark-as-known
;;            )
;;           )
;;       )
;;     )
;;   )

(defun yk-wtype-status-to-face (wtype status)
                                        ;  (and
;  (message "inside yk-wtype-status-to-face [%s][%s]" wtype status)
  (let* (
        (font-table (yk-font-table-wtype-to-use status))
        (face (assoc wtype font-table))
        )
;    (message "inside let [%s]" face)
;    (message "table used [%s]" font-table)
    (if face
        (cdr face)
      ;; else
      (let (
            (face (cdr-safe (assoc status yk-font-table-default-status)))
            )
        (message "> no face for wtype. [%s] status [%s].. using [%s]" wtype status face)
        face
        )
      
      )
    )   
  )

(defun yk-set-overlay (name beg end face)
  (let (
        (ovl (make-overlay beg end));)
        )
    (overlay-put ovl 'font-lock-face face)
    (overlay-put ovl 'yomikun t)
    )
  )

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
  (let* (
         (props (text-properties-at pos))
         (def  (yk-dict-def
                (plist-get props 'root)
                (plist-get props 'pronun)
                (plist-get props 'wtype)
                )
               )
         )
    (yk-debug-message "%s %s" def props)
    (yk-tip-show (format "%s" def))
    )
  
  )

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

(defun yk-process-filter (beg end output)
  "Process OUTPUT from mecab one line at a time using jp-process."
  (yk-debug-message "Starting [%s]" (yk-until-eoln output))
  (let* (
         (lines (split-string output "\n" t))
         (tokens  (mapcar #'yk-mecab-process-line lines))         ;
         )
    (yk-sync-list-to-st
     tokens
     (buffer-substring-no-properties beg end)
     beg
     )
    ))

(defun yk-process-root-for-kana (morph)
  "When the morph is a Kana word, mecab returns the word and
its romagi equivalent. For example: the root of ドーム
is ドーム-dome. This function removes the romagi if it exists"
  (if (and morph (cl-position ?- morph))
      ;; if we have a morph with -
    (let* (
           (pos  (cl-position ?- morph))
           (romagi (and pos (substring morph (+ 1 pos)))))
      (if (and romagi (string-match-p "^[a-zA-Z]+$"romagi ) )
          (substring morph 0 pos) ;; kana part
        morph)) ;; else of if
    morph)) ;; else of if

(defun yk-mecab-process-line (line)
  "maps a mecab output line into a pair (surface properties)
Properties is a property-list with information about the 
"
  (let* (
       (textpair (and line (split-string line "\t")))
       (seen     (nth 0 textpair))
       (info     (nth 1 textpair))
       (infolist (and info (split-string info ",")))
       (wtype   (and infolist (nth 0 infolist)))
       (root    (and infolist (nth 7 infolist)))
       (root-clean (yk-process-root-for-kana root))
       (pronun  (and infolist (nth 9 infolist)))
       (surface (and infolist (nth 10 infolist)))
       )
  (progn
    (if (string-equal "EOS" seen)  ; EOS is a line end
        (setq seen "\n"))
;    (message "Line [%s] surface [%s]" line surface)
    (list 'seen seen 'surface surface 'wtype wtype 'root root-clean 'pronun pronun)
    ))
)

(defun yk-list-add-running-length (pairs)
  "Convert a list of pairs of strings to a list of lists of three elements,
    Each list has 3 elements: the accumulative sum of the length of the first
  string of the input pairs, the first string in the pair, and the third string
  in the pair."
  (let ((running-lengths ())
        (accumulative-length 0))
    (dolist (pair pairs)
      (setq accumulative-length (+ accumulative-length (string-width (car pair))))
      (setq running-lengths (append running-lengths
                                    (list (list
                                           (- accumulative-length -1 (length (car pair)))
                                           accumulative-length (car pair) (nth 1 pair))))))
    running-lengths))

(defun yk-sync-list-to-st (lst st regionOffset)
  "Unfortunately mecab does not output all characters (e.g. spaces). This means
  we need to find out where in the text each token is.

  This function returns a set of tokens that include the position (beg, end) where
  they occur.

"
  (yk-debug-message "entering [%s]" (yk-until-eoln st))
  (yk-debug-message "%s " (car lst))
  (let (
        (output (list))
        (counter 0)
        (offset 0)
        (lenSt (length st))
      )
    (progn
      (while (and (> lenSt offset)
                  (not (null lst)))
        (let* (
               (nextToken (nth 0 lst))
               (next      (plist-get nextToken 'seen))
               (wtype (plist-get nextToken 'wtype))
               (nextLen   (length next))
               (prefix    (substring st offset (+ nextLen offset)))
               )
;          (message "entering iteration %d offset %d lenSt %d len tokens [%d] next [%s]" counter offset lenSt (length lst) (nth 0 lst))
          (if (< counter yk-max-tokens-to-process)
            ;; just in case we get into an infinite loop, or the input is humongous
              (progn
                (setq counter (+ counter 1))
  ;              (message "[%d]current string [%d] [%s]" counter offset (yk-until-eoln (substring st offset)))
;                (message "    next token [%s]" nextToken)
;                (message "    prefix [%s] -> next [%s] nextLen [%d]" prefix next nextLen)
                (if (string-equal next prefix ) ; test matches
                    (let ((endpos (+ offset nextLen ))
                          ) 
                      (progn
                        ;; ignore punctuation
                        (when (not (equal wtype "補助記号"))
                            (plist-put nextToken 'begin (+ offset regionOffset))
                          (plist-put nextToken 'end   (+ endpos regionOffset -1))
                          (push nextToken output))
                        ;;(add-to-list 'output nextToken t)
;                        (message "    + it matches!! offset %d [%s]" offset nextToken)
                        (setq lst (cdr lst))
                        (setq offset (+ offset nextLen))
                        ))
                  ;;; does not match
                  (let* (
                         ;; search from offset for the next token
                         ;; TODO: do not add a new token, it is not needed.
                         ;;     simply advance the character being inspected
                         (skip (cl-search next (substring st offset))) ;; text that is skipped
                         ;;(newSeen (substring st offset (+ skip offset)))
                         ;;(endpos (+ position skip -1))
                         ;; the next line is not really necessary
                         ;; unless we require that the list of tokens
                         ;; represent ALL the text (i.e. the text
                         ;; can be regenerated from the tokens)
;                         (newToken (list 'surface newSeen 'seen newSeen 'begin position 'end endpos 'wtype "other"))
                         )
                    (progn
                      (if (not skip)
                          (error "something went wrong. Unable to match mecab to text") 
                          )
                      (setq offset (+ offset skip))
;                      (message "   > does not match. skip [%d] offset after [%d]" skip offset)
                      )))
                )
           
 ;           (message ">>>> to start another iteration [%d] [%s]" offset (nth 0 lst))
          ))
        )
      ;; left over string... 
      ;; but if there is text left, we don't care for it
      (nreverse output)
      )))

(defun yk-process-mecab (beg end mecabBuffer)
  "Process mecab output mecab on region.

  buffer is the process output from mecab

  1. synchronize text and mecab
  2. Process mecab records
     for reach line in mecab, create a token
  3. process each token
     - add properties and overlay

"
  (let*
      (
       ;; get raw mecab output
       (output (with-current-buffer mecabBuffer
                 (progn
                   (buffer-substring-no-properties (point-min) (point-max)))
                 )
               )
       (yk-debug-message "done processing raw mecab")
       ;; save mecab output, remove end-of-process message
       (outputMecab (substring output 0 (string-match yk-process-end-st output)))
       ;; separate tokens
       (jpTokens (yk-process-filter beg end outputMecab))
       (yk-debug-message "done processing processing tokens")

       )
    (yk-debug-message "finished mecab processing %d tokens" (length jpTokens))
;    (message "[%s]" jpTokens )
    ;; process the tokens
    (yk-process-tokens jpTokens)
    )
  )

(defun yk-create-temp-file-from-string (string)
  (let ((temp-file (make-temp-file "tmp-string-")))
    (with-temp-file temp-file
      (insert string))
    temp-file))

(defun yk-create-simple-buffer (name)
  (let ((buffer (generate-new-buffer name)))
    (buffer-disable-undo buffer)
;    (with-current-buffer buffer
;      (progn
;        (setq buffer-undo-list nil)
;        (mapc (lambda (x) (funcall x -1))
;              (yk-active-minor-modes))
;
;        )
;      )
    buffer
    )
  )

(defun yk-process-region (beg end)
  ;; it run more way faster using an external file as input
  ;; than piping the text to the process
  (setq process-adaptive-read-buffering t)
  (let* (
         (proc-buffer (yk-create-simple-buffer yk-process-buffer))
        (temp-file (yk-create-temp-file-from-string (buffer-substring-no-properties beg end)))
        (process (start-process yk-process-name proc-buffer yk-mecab-command temp-file)
                 )
        )
    (progn
      ;; async input is buffered and we do not want to process
      ;; incomplete lines. So wait until all output is created and process it
      (yk-debug-message "created temp file [%s]" temp-file)
      (yk-debug-message "process starting")

;      (process-send-region process beg end)
      (process-send-eof process)
      (yk-debug-message "process sent")
      (while (accept-process-output process))
      (yk-debug-message "mecab Done")
      (yk-process-mecab beg end proc-buffer)
      (yk-debug-message "finished processing buffer")
      (kill-buffer yk-process-buffer)
      (delete-file temp-file)
      )      
    )
  )

(defun yk-process-region2 (beg end)
  (let (
        
        (process (start-process-shell-command yk-process-name yk-process-buffer yk-mecab-command)
                 )
        )
    (progn
      ;; async input is buffered and we do not want to process
      ;; incomplete lines. So wait until all output is created and process it
      (setq process-adaptive-read-buffering t)
      (yk-debug-message "process starting")
                                        ;      (with-current-buffer yk-process-buffer
                                        ;        (mapc (lambda (x) (funcall x -1))
                                        ;              (yk-active-minor-modes))
                                        ;        )

      (process-send-region process beg end)
      (process-send-eof process)
      (yk-debug-message "process sent")
      (while (accept-process-output process))
      (yk-debug-message "mecab Done")
      (yk-process-mecab beg end yk-process-buffer)
      (yk-debug-message "finished processing buffer")
      (kill-buffer yk-process-buffer)
      )      
    )
  )


;; these two functions were supposed to be a way to make it faster... but it ended
;; being slower. Wayyyyyyyyyyy slower
;; go figure

(defun yk-process-mecab-output (beg end outputMecab)
  "Process mecab output mecab on region.

  buffer is the process output from mecab

  1. synchronize text and mecab
  2. Process mecab records
     for reach line in mecab, create a token
  3. process each token
     - add properties and overlay

"
  (let*
      (
       (jpTokens (yk-process-filter beg end outputMecab))
       )
    (yk-debug-message "finished mecab processing %d tokens" (length jpTokens))
                                        ;    (message "[%s]" jpTokens )
    ;; process the tokens
    (yk-process-tokens jpTokens)
    )
  )



(defun yk-process-region-test (beg end)
  ;; execute command (list of strings)
  ;; return the output of the command
  (interactive "r")
  (setq process-adaptive-read-buffering t)
  (let (
        (mecabOutput "")
        (output (list))
        (process (make-process
                  :name "yk-process"
                  :buffer nil
                  :command (list yk-mecab-command)
                  :filter 'yk-filter
                  :sentinel 'yk-sentinel
                  )
                 )
        )
    (defun yk-filter (proc string)
   ;;(message "from [%s] process [%s] string [%s]" yk-output proc string)
      (setq output (cons string output))
      
      )
    (defun yk-sentinel (process event)
      (yk-debug-message "Process: [%s] had event [%s]" process event)
      (setq mecabOutput
            (mapconcat 'identity (reverse output) "")
            )
      (yk-process-mecab-output beg end mecabOutput)
      (yk-debug-message "finally finishing with length of output [%d] " (length output))
      )
    (process-send-region process beg end)
    (process-send-eof process)
    )
  )

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

(define-minor-mode yk-minor-mode
  "my help"

  :global nil
  :lighter   "_yk_"    ; lighter
  :keymap yk-minor-map             ; keymap
  :after-hook (progn
                (cursor-sensor-mode -1)
                )
  ;; this is where the code goes
  )


;; some utility functions

(defun yk-pos ()
  (interactive)
  (message "%s" (point))
  )

(defun yk-until-eoln (st)
  (substring st 0 (string-match "\n" st))
  )

(defun yk-abc ()
  (interactive)
  (find-file "/tmp/alice.txt")
  )


(defun yk-test ()
  (interactive)
  (kill-buffer "*Messages*")
  (kill-buffer "alice.txt")
  (find-file "/tmp/alice.txt")
  (load-file "/Users/dmg/.emacs.d/dmg-jp.el")
  (setq patito32 (yk-mecab-command-on-buffer))
  )

;;(setq inhibit-point-motion-hooks t)

(provide 'yomikun)
