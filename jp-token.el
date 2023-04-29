(defvar my-command "echo" "Shell command to run on buffer contents")
(defvar my-process-name "jp-process")

(defvar my-process-end-st "\nProcess")
(defvar my-status-db-file "~/jp-status.db")

(setq my-command  "/Users/dmg/bin/osx/m-mecab.sh")

(defvar my-max-tokens-to-process 10000)

(defvar my-process-buffer "*jp-process-buffer*" "Name of buffer for shell command process")

(defvar my-status-db nil) ;; instance of the db

(defun my-db-open ()
    (when (not (file-exists-p my-status-db-file))
      (signal 'file-error (format "the file does not exist [%s]" my-status-db-file))
      )
    (setq my-status-db (emacs-sqlite my-status-db-file))
    )

(defun my-db-close()
  (emacsql-close my-status-db)
  )

(defun my-db-create ()
  (when (file-exists-p my-status-db-file) 
    (signal 'file-error (format "the file already exist [%s]" my-status-db-file))
    )
  (setq my-status-db (emacsql-sqlite my-status-db-file))
  (emacsql my-status-db [:create-table words ([morph mtype status date]
                                              (:primary-key [morph mtype])
                                              )])
  )


(defun my-db-morph-status-get (root wtype)
  ;; since it is a singleton, remove wrapping list
  ;; nth returns nil if the list is empty
  (message "[%s %s]" root wtype)
  (nth 0;
       (emacsql my-status-db [:select [morph mtype status date]
                                      :from words
                                      :where (and (= morph $s1) (= mtype $s2))
                                      ] root wtype)
  ))

(defun my-db-morph-status-delete (root wtype)
  (emacsql my-status-db [:delete 
                            :from words
                            :where (and (= morph $s1) (= mtype $s2))
                          ] root wtype)
  )

(defun my-db-morph-status-insert (root wtype status)
  (let (
        (today "today")
        )
                                        ;
     (emacsql my-status-db [:insert :into words
                              :values ([$s1 $s2 $s3 $s4])] root wtype status today)
     ))

(defun my-db-morph-status-update (root wtype status)
  (my-db-morph-status-delete root wtype)
  (my-db-morph-status-insert root wtype status)
  )

(setq my-status-table (make-hash-table :test 'equal))

(defun my-morph-status-get (root wtype)
  ;; memoize the status of the given morph
  (or (gethash (list root wtype) my-status-table)
      (let
          (
           (status (nth 2
                        (my-db-morph-status-get root wtype))))
        (message "gone to the db [%s]" status)
        (puthash (list root wtype)
                 status
                 my-status-table)        
        )        
    ))

(defun my-morph-status-set (root wtype status)
  (message "entering")
  (let(
       (curval (my-morph-status-get root wtype))
       )
    (unless (and curval
                 (string-equal curval status)
             )
      ;; cache does not have it or it is different
      (message "here")
      (my-db-morph-status-update root wtype status)
      (puthash (list root wtype) status my-status-table)
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
                 (buffer-substring-no-properties (point-min) (point-max)))
               )
       ;; remove end marker
       (outputMecab (substring output 0 (string-match my-process-end-st output)))
       ;; separate tokens
       (jpTokens (my-process-filter outputMecab))
       
       )
    (message "finished %d tokens" (length jpTokens))
    ;; process the tokens
    (my-process-tokens jpTokens)
    )
  
  )

(defun my-do-buffer ()
  "Run a command on the current buffer asynchronously,
   reusing an existing shell command process buffer if one exists.
   When it ends, process the output."
  (interactive)
  (message "entering")
  ;; kill the buffer if it exists
  (kill-buffer my-process-buffer)
  (let (
        (process (or (get-process my-process-name)
                      (start-process-shell-command my-process-name my-process-buffer my-command)
                      )                 
                 )
        )
    (progn
      ;; async input is buffered and we do not want to process
      ;; incomplete lines. So wait until all output is created and process it
      (setq process-adaptive-read-buffering nil)
      (message "process starting")
      (process-send-region process (point-min) (point-max))
      (message "process sent")
      (process-send-eof process)
      (while (accept-process-output process))
      (message "mecab Done")
      (my-process-mecab)
      )      
    )
  )

(defun my-pos ()
  (interactive)
  (message "%s" (point))
  )
              
;(my-sync-list-to-st
;         (buffer-substring (point-min) (point-max))
;         )

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



(defvar my-wtype-table '(
                         ("名詞" . my-face-noun)
                         ("助詞" . my-face-particle)
                         ("動詞" . my-face-verb)
                         ("副詞" . my-face-adverb)
                         ("記号" . my-face-punctuation)
                         ("助動詞" . my-face-morpheme)
                         ("形容詞" . my-face-adjective)
                         ))


(defun my-has-japanese-characters-p (str)
  "Check if STR has any Japanese characters."
  (not (string-match-p "\\`[[:ascii:]]*\\'" str))
  
  )


(defun my-set-text-prop (token face)
  (let*
      (
       (beg (plist-get token 'begin))
       (end (+ (plist-get token 'end) 1)) ;; ahh, it should 
       (ovl (make-overlay beg end))
       )
    (message "setting face %s" token)
    (overlay-put ovl 'my t)
    (overlay-put ovl 'font-lock-face face)
    (put-text-property beg end 'root (plist-get token 'root))
    (put-text-property beg end 'surface (plist-get token 'surface))
    ;; i prefer hiragana to katanana
    (put-text-property beg end 'pronun
                       (my-katakana-to-hiragana (plist-get token 'pronun)))
;    (put-text-property beg end 'face  face)
  ))

(defun my-token-wtype-to-face (token)
  (and
   (assoc (plist-get token 'wtype) my-wtype-table)
;   (my-has-japanese-characters-p (plist-get token 'surface))
   )  
  )

(defun my-process-tokens(jpTokens)
  (with-silent-modifications
    (dolist (token jpTokens)
      
      (if (my-has-japanese-characters-p (plist-get token 'surface))
          (let(
               (face (my-token-wtype-to-face token))
               )
            (if face
                (my-set-text-prop token face)
              
              )
            )
        )    
      )
    )
  )


(defun my-prop-at-point ()
  (interactive)
  (let* ((pos (point))
         (props (text-properties-at pos)))
    (message "Properties at position %d: %s" pos props)))


(defun my-process-filter (output)
  "Process OUTPUT from mecab one line at a time using jp-process."
  (message "Starting [%s]" (my-until-eoln output))
  (let* (
         (lines (split-string output "\n" t))
         (tokens  (mapcar #'my-mecab-process-line lines))         ;
         )
    (message "hello world %s" (car tokens))
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
       (surface  (nth 0 textpair))
       (info     (nth 1 textpair))
       (infolist (and info (split-string info ",")))
       (wtype   (and infolist (nth 0 infolist)))
       (root    (and infolist (nth 7 infolist)))
       (pronun  (and infolist (nth 9 infolist)))
       )
  (progn
                                        
    (if (string-equal "EOS" surface)  ; EOS is a line end
        (setq surface "\n"))
    (message "Line [%s] surface [%s] [%d]" line surface (string-width surface))
    (list 'surface surface 'wtype wtype 'root root 'pronun pronun)
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
;  (message "entering [%s]" (my-until-eoln st))
;  (message "%s " (car lst))
  (let (
        (output (list))
        (counter 0)
        (position 1)
      )
    (progn
      (while (and (> (length st) 0)
                  (not (null lst)))
        (let* (
               (nextToken (nth 0 lst))
               (next (plist-get nextToken 'surface ))
               (nextLen  (length next))
               (prefix (substring st 0 nextLen))
               )
          (progn
            ;; just in case we get into an infinite loop, or the input is humongous
            (if (> counter my-max-tokens-to-process)
                (aborort))
            (setq counter (+ counter 1))
            (message "current next [%s]" (my-until-eoln st))
            (message "    next token [%s]" nextToken)
            (message "    prefix [%s] -> next [%s] nextLen [%d]" prefix next nextLen)
            
            (if (string-equal next prefix ) ; test matches
                (let ((endpos (+ position nextLen -1))
                      ) 
                  (progn
                    (setq st (substring st nextLen))
                    (plist-put nextToken 'begin position)
                    (plist-put nextToken 'end   endpos)
                    (setq position (+ endpos 1))
                    (add-to-list 'output nextToken t)
                    (message "    + it matches!! remaining chars %d [%s]" (length st) nextToken)
                    (setq lst (cdr lst))
                ))
              ; does not match
              (let* (
                     (skip (cl-search next st)) ;; text that is skipped
                     (newSurface (substring st 0 skip))
                     (endpos (+ position skip -1))
                     ;; the next line is not really necessary
                     ;; unless we require that the list of tokens
                     ;; represent ALL the text (i.e. the text
                     ;; can be regenerated from the tokens)
                     (newToken (list 'surface newSurface 'begin position 'end endpos 'wtype "other"))
                    )
                (progn
                  (setq st (substring st skip))
                  (add-to-list 'output newToken  t)
                  (message "   > does not match. new token [%s]" newToken)
                  (setq position (+ endpos 1))
                  )))
            
;            (message ">>>> to start another iteration [%s] [%s]" st (nth 0 lst))
          ))
        )
      ; left over string... 
      (if (> (length st) 0)
          (add-to-list 'output (list newSurface (list 'wtype "other"))  t)        
        )
      output
      )))
