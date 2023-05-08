;;; -*- lexical-binding: t; -*-

;; overlay code based on https://github.com/katspaugh/kuromoji.el


(require 'emacsql)

(defvar my-command "echo" "Shell command to run on buffer contents")
(defvar my-process-name "jp-process")

(defvar my-process-end-st "\nProcess")
(defvar my-status-db-file "~/jp-status.db")

(setq my-command  "/Users/dmg/bin/osx/m-mecab.sh")

(defvar my-max-tokens-to-process 10000)

(setq my-max-tokens-to-process 100000)

(defvar my-process-buffer "*jp-process*" "Name of buffer for shell command process")

(defvar my-report-buffer "*jp-report*" "Name of buffer for report")

(defvar my-dict-db nil) ;; instance of the quick dictionary db
(defvar my-status-db nil) ;; instance of the status db

(defun my-db-open ()
  (interactive)
    (when (not (file-exists-p my-status-db-file))
      (signal 'file-error (format "the file does not exist [%s]" my-status-db-file))
      )
    ;; only open it if it is not open
    (when (not my-status-db)
     (setq my-status-db (emacsql-sqlite my-status-db-file)))
    )


(defvar my-status-db-file "~/jp-status.db")

(defvar my-dict-db nil) ;; instance of the db
(defvar my-dict-db-file nil) ;; instance of the db
(setq my-dict-db-file "~/dictionary.db")

(defun my-db-dict-open ()
  (interactive)
  (when (not (file-exists-p my-dict-db-file))
    (signal 'file-error (format "the dictionary file does not exist [%s]" my-dict-db-file))
    )
  ;; only open it if it is not open
  (when (not my-dict-db)
    (setq my-dict-db (emacsql-sqlite my-dict-db-file)))
  )

(defun my-db-dict-close ()
  (emacsql-close my-dict-db)
  )

(defun my-db-status-close()
  (emacsql-close my-status-db)
  )

(defun my-get-time-date ()
  (format-time-string "%Y-%m-%d %H:%M" (current-time))
  
  )

(defun my-db-create ()
  (interactive)
  (when (file-exists-p my-status-db-file) 
    (signal 'file-error (format "the file already exist [%s]" my-status-db-file))
    )
  (setq my-status-db (emacsql-sqlite my-status-db-file))
  (emacsql my-status-db [:create-table words ([morph mtype surface status date]
                                              (:primary-key [morph mtype surface])
                                              )])
  )

(defun my-db-dict-def (root pronun)
  (emacsql my-dict-db [:select [def pos rank]
                                 :from dict
                                 :where (and (= root $s1) (= pronun $s2) )
                                 :order-by [(asc rank)]
                                 ] root pronun) 
  
  )

(setq my-dict-table (make-hash-table :test 'equal))


(defun my-dict-def (root pronun)
  (or (gethash (list root pronun) my-dict-table)
      (let
          (
           (def (my-db-dict-def root pronun))
           )
        (puthash (list root pronun)
                 def
                 my-dict-table)        
        )        
      )
  )


(defun my-db-morph-status-get (root wtype surface)
  ;; since it is a singleton, remove wrapping list
  ;; nth returns nil if the list is empty
  ;;  (message "get db [%s %s %s]" root wtype surface)
  
  (or (nth 0;
       (emacsql my-status-db [:select [morph mtype status date]
                                      :from words
                                      :where (and (= morph $s1) (= mtype $s2) (= surface $s3))
                                      ] root wtype surface) 
       )
      (list nil nil "unknown" nil))
  )

(defun my-db-morph-status-delete (root wtype surface)
  (emacsql my-status-db [:delete 
                            :from words
                            :where (and (= morph $s1) (= mtype $s2) (= surface $s3))
                          ] root wtype surface)
  )

(defun my-db-morph-status-insert (root wtype surface status)
  (let (
        (today (my-get-time-date))
        )
                                        ;
     (emacsql my-status-db [:insert :into words
                              :values ([$s1 $s2 $s3 $s4 $s5])] root wtype surface status today)
     ))

(defun my-db-morph-status-update (root wtype surface status)
  (my-db-morph-status-delete root wtype surface)
  
  (if (not (string-equal status "unknown"))
   (my-db-morph-status-insert root wtype surface status))
  )

(setq my-status-table (make-hash-table :test 'equal))

(setq my-repeat-counter 0)

(defun my-morph-status-get (root wtype surface)
  ;; memoize the status of the given morph
  (or (gethash (list root wtype surface) my-status-table)
      (let
          (
           (status (nth 2
                        (my-db-morph-status-get root wtype surface)))
           )
        (setq my-repeat-counter (+ my-repeat-counter 1))
;        (message "gone to the db [%s]" status)
        (puthash (list root wtype surface)
                 status
                 my-status-table)        
        )        
    ))

(defun my-morph-status-set (root wtype surface status)
;  (message "entering")
  (let(
       (curval (my-morph-status-get root wtype surface))
       )
    (unless (and curval
                 (string-equal curval status)
             )
      ;; cache does not have it or it is different
      (my-db-morph-status-update root wtype surface status)
      (puthash (list root wtype surface) status my-status-table)
      )
    ))


(defun my-katakana-to-hiragana (str)
  "Convert katakana to hiragana."
  (mapconcat
   (lambda (c)
     (if (and (>= c #x30a1) (<= c #x30f6))
         (char-to-string (+ c (- #x3041 #x30a1)))
       (char-to-string c)))
   str ""))

(defun my-do-region (beg end)
  "jp-ize the buffer

     1. Run mecab command on the current buffer asynchronously,
     2. Process the output.
     3. kill output buffer
"
  (interactive "r")

  (my-remove-props-and-overlays beg end)
  ;; kill the buffer if it exists
  (if (bufferp my-process-buffer)
      (kill-buffer my-process-buffer))

  (my-process-region beg end)
  )

(defun my-do-buffer ()
  (interactive)
  (my-do-region (point-min) (point-max))
  )


(defun my-pos ()
  (interactive)
  (message "%s" (point))
  )
              
(defun my-until-eoln (st)
  (substring st 0 (string-match "\n" st))
  )

(defun my-abc ()
  (interactive)
  (find-file "/tmp/alice.txt")
  )


(defun my-test ()
  (interactive)
  (kill-buffer "*Messages*")
  (kill-buffer "alice.txt")
  (find-file "/tmp/alice.txt")
  (load-file "/Users/dmg/.emacs.d/dmg-jp.el")
  (setq patito32 (my-command-on-buffer))
  )

(defface my-face-unknown
  '((t ( :background "LightYellow2"
                )))
  "Face for default unknown text"
  :group 'my)

(defface my-face-learning
  '((t ( :background "PaleGreen1"
         )))
  "Face for default learning text"
  :group 'my)

(defface my-face-ignore
  '((t ( :background "gray80"
         )))
  "Face for default learning text"
  :group 'my)


(defface my-face-noun
  '((t (:inherit font-lock-string-face
                 )))
  "Face for noun unknown"
  :group 'my
  )

(defface my-face-noun-alt
  `((((class color) (background light))
     (:foreground  "darkblue"))
    (((class color) (background dark))
     (:foreground  "darkblue")))
  "Face for nouns alt."
  :group 'my)

;;font-lock-keyword-face
;; (defface my-face-verb
;;   `((((class color) (background light))
;;      (:foreground  "darkgreen"))
;;     (((class color) (background dark))
;;      (:foreground  "red")))
;;   "Face for verbs."
;;   :group 'my)

(defface my-face-verb
  '((t (:inherit font-lock-keyword-face
                 )))
  "Face for verb"
  :group 'my
  )

(defface my-face-morpheme
  `((((class color) (background light))
     (:foreground  "magenta"))
    (((class color) (background dark))
     (:foreground  "darkgreen")))
  "Face for verbs."
  :group 'my)

(defface my-face-adverb
  `((((class color) (background light))
     (:foreground  "purple"))
    (((class color) (background dark))
     (:foreground  "purple")))
  "Face for adverbs."
  :group 'my)

(defface my-face-adjective
  `((((class color) (background light))
     (:foreground  "orange"))
    (((class color) (background dark))
     (:foreground  "orange")))
  "Face for adjectives."
  :group 'my)

(defface my-face-particle
  `((((class color) (background light))
     (:foreground  "darkgrey"))
    (((class color) (background dark))
     (:foreground  "darkgrey")))
  "Face for particles."
  :group 'my)

(defface my-face-punctuation
  `((((class color) (background light))
     (:foreground  "black"))
    (((class color) (background dark))
     (:foreground  "black")))
  "Face for particles."
  :group 'my)

(defface my-face-noun-unknown
  '((t (:inherit ( my-face-noun  my-face-unknown))))
  "Face for noun unknown")

(defface my-face-particle-unknown
  '((t (:inherit ( my-face-particle  my-face-unknown))))
  "Face for particle unknown")

(defface my-face-verb-unknown
  '((t (:inherit ( my-face-verb  my-face-unknown))))
  "Face for verb unknown")

(defface my-face-adverb-unknown
  '((t (:inherit ( my-face-adverb  my-face-unknown))))
  "Face for Adverb unknown")

(defface my-face-punctuation-unknown
  '((t (:inherit ( my-face-punctuation  my-face-unknown))))
  "Face for punctuation unknown")

(defface my-face-morpheme-unknown
  '((t (:inherit ( my-face-morpheme  my-face-unknown))))
  "Face for morpheme unknown")

(defface my-face-adjective-unknown
  '((t (:inherit (my-face-adjective my-face-unknown)                
                 )))
  "Face for adjective unknown")


(defface my-face-noun-learning
  '((t (:inherit ( my-face-noun  my-face-learning))))
  "Face for noun learning")

(defface my-face-particle-learning
  '((t (:inherit ( my-face-particle  my-face-learning))))
  "Face for particle learning")

(defface my-face-verb-learning
  '((t (:inherit ( my-face-verb  my-face-learning))))
  "Face for verb learning")

(defface my-face-adverb-learning
  '((t (:inherit ( my-face-adverb  my-face-learning))))
  "Face for Adverb learning")

(defface my-face-punctuation-learning
  '((t (:inherit ( my-face-punctuation  my-face-learning))))
  "Face for punctuation learning")

(defface my-face-morpheme-learning
  '((t (:inherit ( my-face-morpheme  my-face-learning))))
  "Face for morpheme learning")

(defface my-face-adjective-learning
  '((t (:inherit (my-face-adjective my-face-learning)                
                 )))
  "Face for adjective learning")


(defvar my-wtype-table '(
                         ("名詞" . my-face-noun)
                         ("助詞" . my-face-particle)
                         ("動詞" . my-face-verb)
                         ("副詞" . my-face-adverb)
                         ("記号" . my-face-punctuation)
                         ("助動詞" . my-face-morpheme)
                         ("形容詞" . my-face-adjective)
                         ))

(defvar my-wtype-table-status-unknown '(
                         ("名詞" . my-face-noun-unknown)
                         ("助詞" . my-face-particle-unknown)
                         ("動詞" . my-face-verb-unknown)
                         ("副詞" . my-face-adverb-unknown)
                         ("記号" . my-face-punctuation-unknown)
                         ("助動詞" . my-face-morpheme-unknown)
                         ("形容詞"   . my-face-adjective-unknown)
                         ("default" . my-face-unknown)
                         ))


(defvar my-wtype-table-status-learning'(
                                        ("名詞" . my-face-noun-learning)
                                        ("助詞" . my-face-particle-learning)
                                        ("動詞" . my-face-verb-learning)
                                        ("副詞" . my-face-adverb-learning)
                                        ("記号" . my-face-punctuation-learning)
                                        ("助動詞" . my-face-morpheme-learning)
                                        ("形容詞" . my-face-adjective-learning)
                                        ))


(defvar my-font-table-default-status '(
                                       ("unknown" . my-face-unknown)
                                       ("learning" . my-face-learning)
                                       ("ignore" .   my-face-ignore)
                                       ))

(defun my-has-japanese-characters-p (str)
  "Check if STR has any Japanese characters."
  (not (string-match-p "\\`[[:ascii:]]*\\'" str))
  
  )

(defun my-font-table-to-use (status)
  (cond 
   ((string-equal "known" status)     my-wtype-table)
   ((string-equal "learning" status)  my-wtype-table-status-learning)
   (my-wtype-table-status-unknown)
   )
  
  )

(defun my-morph-do-morphs (cmpfun pfun)
  "process each moph(beg end) that satisfies cmpfun(beg)
and call pfun on it"
  (let ((pos (point-min)))
    (while (setq pos (next-single-property-change pos 'begin))
      (when (funcall cmpfun pos)
        (funcall pfun pos (next-single-property-change pos 'begin))))))

(defun my-morph-matches-at (morphProps pos)
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

(defun my-morphs-delete-overlays-at-pos (beg end)
  "delete overlays between beg and end"
  (dolist (overlay (overlays-in beg end))
    (message "deleting... overlay [%s]" overlay)
    (delete-overlay overlay))  
  )


(defun my-test-all ()
  (interactive)

  (let* ((pos (point))
         (props (text-properties-at pos))
         )
    ;; process the morphs that match this one
    ;; and replace the overlays
    (my-morph-do-morphs
     (lambda (beg) (my-morph-matches-at props beg  )););
      )
    'my-morphs-delete-overlays-at-pos
    )
  )

(defun my-remove-props-and-overlays (beg end)
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
                                           my-morph nil
                                           end     nil
                                           status  nil
                                           ))
    (remove-overlays beg end 'my-name t)
  )))

(setq my-test-all-morphs (make-hash-table :test 'equal));(list))
;;(setq my-status-table (make-hash-table :test 'equal))

(defun my-morph-get-morph-from-props (props)
  "get the morph from the properties"
  (list
   (plist-get props 'root)
   (plist-get props 'wtype)
   (plist-get props 'surface)
   )
  )

(defun my-morph-get-from-props (props attr)
  "get specific attribute from props
this might be redundant, but it is trying to create a layer between
properties and data
"
  (plist-get props attr)
  )

(defun my-sort-hash-table (hash-table compare-func)
  "Return a sorted list of key-value pairs from HASH-TABLE.
The list is sorted using COMPARE-FUNC to compare elements."
  (let (pairs)
    (maphash (lambda (key value)
               (push (cons key value) pairs))
             hash-table)
    (sort pairs compare-func)))
                                        ;

(defun my-extract-all-morphs ()
  "return a hashtable where the key is the morph, and the value the frequency"
  (let
      (
       (all-morphs (make-hash-table :test 'equal) )
       )
    (my-morph-do-morphs
     (lambda (beg) (plist-get (text-properties-at beg) 'root))   ;; process all morphs
     (lambda (beg end)  ;; add morphs to the hashtable
       (let*
           (
            (props (text-properties-at beg))
            (morph (my-morph-get-morph-from-props props))
            )
         (message "%s" props)
         ;; increase their counter by 1
         (puthash morph
                  (+ (gethash morph all-morphs 0) 1)
                  all-morphs)
         ))
     )
;    (message "[%s]" all-morphs)
    (my-sort-hash-table all-morphs
                        (lambda (a b) (> (cdr a) (cdr b)))
                        )
    )
  )

(defun my-report-all-morphs ()
  "report the frequency of morphs"
  (interactive)
  (let (
        (morphs (my-extract-all-morphs))
;        (buffer (create... my-report-buffer))
        )
    
    (mapc 
     (lambda (m) (message "morphs [%s]" m))
     morphs
     )
    (message "total: %d" (length morphs))
    )
  )

(defun my-update-status-at-position (beg end wtype newstatus)
  ;;
  ;;  replace the overlays at the given position
  ;;  according to the new status
  ;;
  (my-morphs-delete-overlays-at-pos beg end)
  (my-set-overlay-at-pos beg end wtype newstatus)
)


(defun my-replace-all-overlays (props newstatus)
  ;;                                      ;
  ;; replace all the overlays of the given morph in all the document
  ;; according to the new status
  ;; 
  (my-morph-do-morphs
   (lambda (beg)      (my-morph-matches-at props beg  )););
   (lambda (beg end)  (my-update-status-at-position beg
                                                    end
                                                    (plist-get props 'wtype)    
                                                    newstatus
                                                   )
     )
   )

  )

(defun my-morph-new-status (props new-status)
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
          (my-morph-status-set root wtype surface new-status)
          ;; now we have to process the document to find instances of the word
          ;; ... there is a need for a redo buffer
          ;; but that is expensive, soo simply scan the document
          ;; and find any instances of the morph
          ;; and change its property
          ;; first remove all overlays in the file
          (my-replace-all-overlays props new-status)
          )
      (message "morph (%s %s %s)already has [%s] status" root wtype surface new-status)
      )
    ))

;; (defun my-mark-morph-as-known (beg end)
  
;;   )

;; (defun my-morph-set-known ()
;;   (interactive)
;;   (let* (
;;          (pos (point))
;;          (props (text-properties-at pos))
;;          )
;;     (if (and props 
;;              (plist-get props 'root)
;;              )
;;         (progn ;
;;           (my-morph-new-status props "known")

;;           (my-morph-do-morphs
;;            (lambda (beg) (my-morph-matches-at props beg  )););
;;            'my-mark-as-known
;;            )
;;           )
;;       )
;;     )
;;   )

(defun my-wtype-status-to-face (wtype status)
                                        ;  (and
;  (message "inside my-wtype-status-to-face [%s][%s]" wtype status)
  (let* (
        (font-table (my-font-table-to-use status))
        (face (assoc wtype font-table))
        )
;    (message "inside let [%s]" face)
;    (message "table used [%s]" font-table)
    (if face
        (cdr face)
      (assoc status my-font-table-default-status)
        )
    )   
  )


(defun my-set-overlay-at-pos (beg end wtype status)
  (let (
        (face (my-wtype-status-to-face wtype status))
        )

    (if face
        (let (
              (ovl (make-overlay beg end));)
              )
          (overlay-put ovl 'font-lock-face face)
          (overlay-put ovl 'my-name t)
          )
        )
    )
  )


(defun my-set-text-prop-token (token)
  ;(message "setting prope [%s]" token)
  (let*
      (
       (beg (plist-get token 'begin))
       (root (plist-get token 'root))
       (wtype (plist-get token 'wtype))
       (surface (plist-get token 'surface))
       (status (my-morph-status-get root wtype surface))
       (end (+ (plist-get token 'end) 1)) ;; ahh, it should 
;       (face (my-wtype-status-to-face wtype status))
       )
    (my-set-overlay-at-pos beg end wtype status)
    (if status
        (put-text-property beg end 'status status)
      )
    (put-text-property beg end 'begin beg)
    (put-text-property beg end 'end end)
    (put-text-property beg end 'wtype  wtype)
    (put-text-property beg end 'root root)
    (put-text-property beg end 'surface surface)
    (put-text-property beg end 'my-morph t)
    ;; i prefer hiragana to katanana
    (put-text-property beg end 'pronun
                       (my-katakana-to-hiragana (plist-get token 'pronun)))
  ))


(defun my-process-wtype-p (token)
  (and
   (assoc (plist-get token 'wtype) my-wtype-table)
;   (my-has-japanese-characters-p (plist-get token 'surface))
   )  
  )

(defun my-process-tokens(jpTokens)
  ;; make sure database is open
  (my-db-open)
  (with-silent-modifications
    (dolist (token jpTokens)
      
      (if (my-has-japanese-characters-p (plist-get token 'seen))
;;          (if (my-process-wtype-p token)
              (my-set-text-prop-token token)            
;;            )
        
        )    
      )
    )
  )

(defun my-extract-term-at-point ()
    (interactive)
    (let* ((pos (point))
           (props (text-properties-at pos))
           (term (and props
                      (plist-get props 'root)
                      ))
           )
      (if term
          (message "%s" term)
        (error "no japanese text under point (perhaps it has not been parsed)")
        )
      )
    )


(defun my-define-at-point ()
  "show definition of the currently selected word in a tooltip and a message. Keeps a log
   of searched words in a buffer too. Uses myougiden."
  (interactive)
  (save-excursion
    (let* (
           (term (my-extract-term-at-point))
           (definition (dmg-run-dictionary term))
           )
      (if (> (length definition) 0)
          (dmg-show-definition term definition)
                                        ; else
        (message (format "Term [%s] not found" term))
        )
      )
    )
  )


(defun my-prop-at-point ()
  (interactive)
  (let* ((pos (point))
         (props (text-properties-at pos)))
    (message "Properties at position %d: %s" pos props)))


(defun my-properties-at-point ()
  (let* ((pos (point))
         (props (text-properties-at pos)))
    (if (plist-get props 'my-morph)
        props
      nil
        )))
  
  
(defun my-mark-at-point-as-ignored ()
  (interactive)
  (let* (
         (props (my-properties-at-point)))
    (if props
        (my-morph-new-status props "ignore")
        )
    )
  )

(defun my-mark-at-point-as-known ()
  (interactive)
  (let* (
         (props (my-properties-at-point)))
    (if props
        (my-morph-new-status props "known")
      )
    )
  )

(defun my-mark-at-point-as-unknown ()
(interactive)
(let* (
       (props (my-properties-at-point)))
  (if props
      (my-morph-new-status props "unknown")
    )
  )
)

(defun my-mark-at-point-as-learning ()
  (interactive)
  (let* (
         (props (my-properties-at-point)))
    (if props
        (my-morph-new-status props "learning")
      )
    )
  )

(defun my-process-filter (beg end output)
  "Process OUTPUT from mecab one line at a time using jp-process."
  (message "Starting [%s]" (my-until-eoln output))
  (let* (
         (lines (split-string output "\n" t))
         (tokens  (mapcar #'my-mecab-process-line lines))         ;
         )
    (my-sync-list-to-st
     tokens
     (buffer-substring beg end)
     beg
     )
    ))


(defun my-mecab-process-line (line)
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
       (pronun  (and infolist (nth 9 infolist)))
       (surface (and infolist (nth 10 infolist)))
       )
  (progn
                                        
    (if (string-equal "EOS" seen)  ; EOS is a line end
        (setq seen "\n"))
;    (message "Line [%s] surface [%s]" line surface)
    (list 'seen seen 'surface surface 'wtype wtype 'root root 'pronun pronun)
    ))
)

(defun my-list-add-running-length (pairs)
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

(defun my-sync-list-to-st (lst st regionOffset)
  "Unfortunately mecab does not output all characters (e.g. spaces). This means
  we need to find out where in the text each token is.

  This function returns a set of tokens that include the position (beg, end) where
  they occur.

"
  (message "entering [%s]" (my-until-eoln st))
  (message "%s " (car lst))
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
               (nextLen   (length next))
               (prefix    (substring st offset (+ nextLen offset)))
               )
;          (message "entering iteration %d offset %d lenSt %d len tokens [%d] next [%s]" counter offset lenSt (length lst) (nth 0 lst))
          (if (< counter my-max-tokens-to-process)
            ;; just in case we get into an infinite loop, or the input is humongous
              (progn
                (setq counter (+ counter 1))
  ;              (message "[%d]current string [%d] [%s]" counter offset (my-until-eoln (substring st offset)))
;                (message "    next token [%s]" nextToken)
;                (message "    prefix [%s] -> next [%s] nextLen [%d]" prefix next nextLen)
                (if (string-equal next prefix ) ; test matches
                    (let ((endpos (+ offset nextLen ))
                          ) 
                      (progn
                                        ;                    (setq st (substring st nextLen))
                        (plist-put nextToken 'begin (+ offset regionOffset))
                        (plist-put nextToken 'end   (+ endpos regionOffset -1))
                        (push nextToken output)
                        ;;(add-to-list 'output nextToken t)
;                        (message "    + it matches!! offset %d [%s]" offset nextToken)
                        (setq lst (cdr lst))
                        (setq offset (+ offset nextLen))
                        ))
                                        ; does not match
                  (let* (
                         ;; search from offset for the next token
                         ;; TODO: do not add a new token, it is not needed.
                         ;;     simply advance the caracter being inspected
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
      ;; but if there is text left, we dont' care for it
      (nreverse output)
      )))

(defun my-process-mecab (beg end mecabBuffer)
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
       (message "done processing raw mecab")
       ;; save mecab ouptut, remove end-of-process message
       (outputMecab (substring output 0 (string-match my-process-end-st output)))
       ;; separate tokens
       (jpTokens (my-process-filter beg end outputMecab))
       (message "done processing processing tokens")

       )
    (message "finished mecab processng %d tokens" (length jpTokens))
;    (message "[%s]" jpTokens )
    ;; process the tokens
    (my-process-tokens jpTokens)
    )
  )

(defun my-create-temp-file-from-string (string)
  (let ((temp-file (make-temp-file "tmp-string-")))
    (with-temp-file temp-file
      (insert string))
    temp-file))

(defun my-create-simple-buffer (name)
  (let ((buffer (generate-new-buffer name)))
    (buffer-disable-undo buffer)
;    (with-current-buffer buffer
;      (progn
;        (setq buffer-undo-list nil)
;        (mapc (lambda (x) (funcall x -1))
;              (my-active-minor-modes))
;
;        )
;      )
    buffer
    )
  )

(defun my-process-region (beg end)
  ;; it run more way faster using an external file as input
  ;; than piping the text to the process
  (setq process-adaptive-read-buffering t)
  (let* (
         (proc-buffer (my-create-simple-buffer my-process-buffer))
        (temp-file (my-create-temp-file-from-string (buffer-substring beg end)))
        (process (start-process my-process-name proc-buffer my-command temp-file)
                 )
        )
    (progn
      ;; async input is buffered and we do not want to process
      ;; incomplete lines. So wait until all output is created and process it
      (message "created temp file [%s]" temp-file)
      (message "process starting")

;      (process-send-region process beg end)
      (process-send-eof process)
      (message "process sent")
      (while (accept-process-output process))
      (message "mecab Done")
      (my-process-mecab beg end proc-buffer)
      (message "finisheb pressing buffer")
      (kill-buffer my-process-buffer)
      (delete-file temp-file)
      )      
    )
  )

(defun my-process-region2 (beg end)
  (let (
        
        (process (start-process-shell-command my-process-name my-process-buffer my-command)
                 )
        )
    (progn
      ;; async input is buffered and we do not want to process
      ;; incomplete lines. So wait until all output is created and process it
      (setq process-adaptive-read-buffering t)
      (message "process starting")
                                        ;      (with-current-buffer my-process-buffer
                                        ;        (mapc (lambda (x) (funcall x -1))
                                        ;              (my-active-minor-modes))
                                        ;        )

      (process-send-region process beg end)
      (process-send-eof process)
      (message "process sent")
      (while (accept-process-output process))
      (message "mecab Done")
      (my-process-mecab beg end my-process-buffer)
      (message "finisheb pressing buffer")
      (kill-buffer my-process-buffer)
      )      
    )
  )


;; these two functions were supposed to be a way to make it faster... but it ended
;; being slower. Wayyyyyyyyyyy slower
;; go figure

(defun my-process-mecab-output (beg end outputMecab)
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
       (jpTokens (my-process-filter beg end outputMecab))
       )
    (message "finished mecab processng %d tokens" (length jpTokens))
                                        ;    (message "[%s]" jpTokens )
    ;; process the tokens
    (my-process-tokens jpTokens)
    )
  )



(defun my-process-region-test (beg end)
  ;; execute command (list of strings)
  ;; return the output of the command
  (interactive "r")
  (setq process-adaptive-read-buffering t)
  (let (
        (mecabOutput "")
        (output (list))
        (process (make-process
                  :name "my-process"
                  :buffer nil
                  :command (list my-command)
                  :filter 'my-filter
                  :sentinel 'my-sentinel
                  )
                 )
        )
    (defun my-filter (proc string)
   ;;(message "from [%s] process [%s] string [%s]" my-output proc string)
      (setq output (cons string output))
      
      )
    (defun my-sentinel (process event)
      (message "Process: [%s] had event [%s]" process event)
      (setq mecabOutput
            (mapconcat 'identity (reverse output) "")
            )
      (my-process-mecab-output beg end mecabOutput)
      (message "finally finishing with length of output [%d] " (length output))
      )
    (process-send-region process beg end)
    (process-send-eof process)
    )
  )

(defun my-visit-site-with-param (base-url parm)
  (let ((url (format base-url (url-hexify-string parm))))
    (browse-url url)))

(defun my-jisho-at-point ()
  (interactive)
  (save-excursion
    (let* (
           (term (my-extract-term-at-point))
           )
      (if term
          (my-visit-site-with-param "https://jisho.org/search/%s" term)
          )
      )
    )
  )

(defun my-kanji-damage-at-point ()
  (interactive)
  (save-excursion
    (let* (
           (kanji (char-to-string (char-after)))
           )
      ;; todo, check that it is a kanji
      (if kanji
          (my-visit-site-with-param "http://www.kanjidamage.com/kanji/search?utf8=✓&q=%s" kanji)
        )
      )
    )
  )



(defun my-mark-sentence-at-point ()
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



(defvar my-minor-map (make-sparse-keymap)
  "Keymap used my-minor mode")

(defun my-disable-mode ()
  (interactive)
  (message "Exiting my-minor-mode")
  (my-minor-mode -1)
  )

(define-key my-minor-map (kbd "=") 'my-mark-sentence-at-point)
(define-key my-minor-map (kbd "p") 'my-prop-at-point)
(define-key my-minor-map (kbd "i") 'my-mark-at-point-as-ignored)
(define-key my-minor-map (kbd "k") 'my-mark-at-point-as-known)
(define-key my-minor-map (kbd "l") 'my-mark-at-point-as-learning)
(define-key my-minor-map (kbd "RET") 'my-define-at-point)
(define-key my-minor-map (kbd "j") 'my-jisho-at-point)
(define-key my-minor-map (kbd "m") 'my-kanji-damage-at-point)
(define-key my-minor-map (kbd "x") 'my-disable-mode)

(define-minor-mode my-minor-mode
  "my help"

  :global nil
  :lighter   "_yk_"    ; lighter
  :keymap my-minor-map             ; keymap

  ;; this is where the code goes
  )


