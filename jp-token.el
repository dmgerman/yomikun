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

(defvar my-process-buffer "*jp-process-buffer*" "Name of buffer for shell command process")

(defvar my-status-db nil) ;; instance of the db

(defun my-db-open ()
  (interactive)
    (when (not (file-exists-p my-status-db-file))
      (signal 'file-error (format "the file does not exist [%s]" my-status-db-file))
      )
    ;; only open it if it is not open
    (when (not my-status-db)
     (setq my-status-db (emacsql-sqlite my-status-db-file)))
    )

(defun my-db-close()
  (emacsql-close my-status-db)
  )

(defun my-get-time-date ()
  (format-time-string "%Y-%m-%d %H:%M" (current-time))
  
  )

(defun my-db-create ()
  (when (file-exists-p my-status-db-file) 
    (signal 'file-error (format "the file already exist [%s]" my-status-db-file))
    )
  (setq my-status-db (emacsql-sqlite my-status-db-file))
  (emacsql my-status-db [:create-table words ([morph mtype surface status date]
                                              (:primary-key [morph mtype surface])
                                              )])
  )


(defun my-db-morph-status-get (root wtype surface)
  ;; since it is a singleton, remove wrapping list
  ;; nth returns nil if the list is empty
  ;;  (message "get db [%s %s %s]" root wtype surface)
  
  (or (nth 0;
       (emacsql my-status-db [:select [morph mtype status date]
                                      :from words
                                      :where (and (= morph $s1) (= mtype $s2) (=surface $s3))
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

(defun my-process-mecab ()
  "retrieves the output from mecab, parses and adds
  the properties and overlays
  to the current text based on it"
  (interactive)
  (let*
      (
       ;; get raw mecab output
       (output (with-current-buffer my-process-buffer
                 (progn
                   (buffer-substring-no-properties (point-min) (point-max)))
                 )
               )
       ;; remove end marker
       (message "done processing raw mecab")
       (outputMecab (substring output 0 (string-match my-process-end-st output)))
       ;; separate tokens
       (jpTokens (my-process-filter outputMecab))
       (message "done processing processing tokens")

       )
    (message "finished mecab processng %d tokens" (length jpTokens))
    ;; process the tokens
    (my-process-tokens jpTokens)
    )
  
  )

(defun my-do-buffer ()
  "Run a command on the current buffer asynchronously,
   reusing an existing shell command process buffer if one exists.
   When it ends, process the output."
  (interactive)
  ;; kill the buffer if it exists
  (if (bufferp my-process-buffer)
      (kill-buffer my-process-buffer))
  ;; create buffer and remove undo
;;    (with-current-buffer (get-buffer-create my-process-buffer)
;;    (setq buffer-undo-list nil);
;;    )

  (let (
        
        (process (or (get-process my-process-name)
                      (start-process-shell-command my-process-name my-process-buffer my-command)
                      )                 
                 )
        )
    (progn
      ;; async input is buffered and we do not want to process
      ;; incomplete lines. So wait until all output is created and process it
      (setq process-adaptive-read-buffering t)
      (message "process starting")
      (process-send-region process (point-min) (point-max))
      (message "process sent")
      (process-send-eof process)
      (while (accept-process-output process))
      (message "mecab Done")
      (my-process-mecab)
      (message "finisheb pressing buffer")
      (kill-buffer my-process-buffer)
      )      
    )
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
  '((t (:bold t)))
  "Face for bold text."
  :group 'my
  )

(defface my-face-learning
  '((t (:underline t)))
  "Face for bold text."
  :group 'my
  )
  
(defface my-face-noun
  `((((class color) (background light))
     (:foreground  "blue"))
    (((class color) (background dark))
     (:foreground  "blue")))
  "Face for nouns."
  :group 'my)


(defface my-face-noun-alt
  `((((class color) (background light))
     (:foreground  "darkblue"))
    (((class color) (background dark))
     (:foreground  "darkblue")))
  "Face for nouns."
  :group 'my)

(defface my-face-verb
  `((((class color) (background light))
     (:foreground  "darkgreen"))
    (((class color) (background dark))
     (:foreground  "red")))
  "Face for verbs."
  :group 'my)

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
  '((t (:inherit my-face-noun :bold t)))
  "Face for noun unknown")

(defface my-face-particle-unknown
  '((t (:inherit my-face-particle :bold t)))
  "Face for particle unknown")

(defface my-face-verb-unknown
  '((t (:inherit my-face-verb :bold t)))
  "Face for verb unknown")

(defface my-face-adverb-unknown
  '((t (:inherit my-face-adverb :bold t)))
  "Face for Adverb unknown")

(defface my-face-punctuation-unknown
  '((t (:inherit my-face-punctuation :bold t)))
  "Face for punctuation unknown")

(defface my-face-morpheme-unknown
  '((t (:inherit my-face-morpheme :bold t)))
  "Face for morpheme unknown")

(defface my-face-adjective-unknown
  '((t (:inherit my-face-adjective :bold t)))
  "Face for adjective unknown")


(defvar my-wtype-table '(
                         ("名詞" . my-face-noun)
                         ("助詞" . my-face-particle)
                         ("動詞" . my-face-verb)
                         ("副詞" . my-face-adverb)
                         ("記号" . my-face-punctuation)
                         ("助動詞" . my-face-morpheme)
                         ("形容詞" . my-face-adjective)
                         ))

(defvar my-wtype-table-status-unknown'(
                         ("名詞" . my-face-noun-unknown)
                         ("助詞" . my-face-particle-unknown)
                         ("動詞" . my-face-verb-unknown)
                         ("副詞" . my-face-adverb-unknown)
                         ("記号" . my-face-punctuation-unknown)
                         ("助動詞" . my-face-morpheme-unknown)
                         ("形容詞" . my-face-adjective-unknown)
                         ))

(defun my-has-japanese-characters-p (str)
  "Check if STR has any Japanese characters."
  (not (string-match-p "\\`[[:ascii:]]*\\'" str))
  
  )

(defun my-font-table-to-use (status)
  (cond 
   ((string-equal "known" status)  my-wtype-table)
   (my-wtype-table-status-unknown)
   )
  
  )

(defun my-morph-do-morphs (pfun cmpfun)
  ""
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

(defun my-morphs-delete-overlays (beg end)
  (message "Found at [%d:%d]" beg end)
  (dolist (overlay (overlays-in beg end))
    (message "deleting... overlay [%s]" overlay)
    (delete-overlay overlay))
  
  )


(defun my-test-all ()
  (interactive)

  (let* ((pos (point))
         (props (text-properties-at pos))
         )
    (my-morph-do-morphs
     'my-morphs-delete-overlays
     (lambda (beg) (my-morph-matches-at props beg  )););
      )
     )
  )


(defun my-replace-all-overlays (root wtype surface newstatus)
                                        ;
  ;; we need to walk the entire document, because we do not know
  ;; 
  )

(defun my-morph-new-status (props new-status)
  "mark morph at point as new status"
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
          (my-replace-all-overlays root wtype surface new-status)
          )
      )
    ))

(defun my-morph-set-known ()
  (interactive)
  (let* (
         (pos (point))
         (props (text-properties-at pos))
         )
    (if (and props)
        (plist-get props 'root)
        )
    (my-morph-new-status props "known")
    )
  
  
  )

(defun my-wtype-status-to-face (wtype status)
                                        ;  (and
;  (message "inside my-wtype-status-to-face [%s][%s]" wtype status)
  (let* (
        (font-table (my-font-table-to-use status))
        (face (assoc wtype font-table))
        )
;    (message "inside let [%s]" face)
    (and face
        (cdr face)
        )
    )   
  )


(defun my-set-text-prop-token (token)
;  (message "setting prope [%s]" token)
  (let*
      (
       (beg (plist-get token 'begin))
       (root (plist-get token 'root))
       (wtype (plist-get token 'wtype))
       (surface (plist-get token 'surface))
       (status (my-morph-status-get root wtype surface))
       (end (+ (plist-get token 'end) 1)) ;; ahh, it should 
       (ovl (make-overlay beg end))
       (face (my-wtype-status-to-face wtype status))
       )
    (overlay-put ovl 'my t)
    (if face
        (progn
;          (message "setting face %s" face)
          (overlay-put ovl 'font-lock-face face)
          (overlay-put ovl 'my-name t)
          )
      )
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
          (if (my-process-wtype-p token)
              (my-set-text-prop-token token)            
            )
        
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
          (dmg-show-definition definition)
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
    (if (plist-get props 'my-morph t)
        props
      nil
        )))
  
  
(defun my-mark-as-ignored-at-point ()
  (interactive)
  (let* (
         (props (my-properties-at-point)))
    (if props
        (my-morph-new-status "ignore")
        )
    )
  )




(defun my-process-filter (output)
  "Process OUTPUT from mecab one line at a time using jp-process."
  (message "Starting [%s]" (my-until-eoln output))
  (let* (
         (lines (split-string output "\n" t))
         (tokens  (mapcar #'my-mecab-process-line lines))         ;
         )
    (my-sync-list-to-st
     tokens
     (buffer-substring (point-min) (point-max))
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
;    (message "Line [%s] surface [%s] [%d]" line surface (string-width surface))
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

(defun my-sync-list-to-st (lst st)
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
                        (plist-put nextToken 'begin (+ offset 1))
                        (plist-put nextToken 'end   endpos)
;                        (setq position (+ endpos 1))
                        (push nextToken output)
                        ;;(add-to-list 'output nextToken t)
 ;                       (message "    + it matches!! offset %d [%s]" offset nextToken)
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

(defvar my-minor-map (make-sparse-keymap)
  "Keymap used my-minor mode")

(define-key my-minor-map (kbd "i") 'my-prop-at-point)
(define-key my-minor-map (kbd "d") 'my-define-at-point)

(define-minor-mode my-minor-mode
  "my help"

  :global nil
  :lighter   "_jp_"    ; lighter
  :keymap my-minor-map             ; keymap

  ;; this is where the code goes
  )


