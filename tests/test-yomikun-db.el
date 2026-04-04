;;; -*- lexical-binding: t; -*-
;;; test-yomikun-db.el --- Tests for yomikun-db module

(require 'buttercup)

;;; --- Database tests using temp SQLite ---

(describe "yomikun-db"
  :var (temp-db-file saved-status-file saved-db-status)

  (before-each
    (setq saved-status-file yk-db-status-file)
    (setq saved-db-status yk-db-status)
    (setq temp-db-file (make-temp-file "yomikun-test-" nil ".db"))
    ;; Delete the file so yk-db-status-create can create it fresh
    (delete-file temp-db-file)
    (setq yk-db-status-file temp-db-file)
    (setq yk-db-status nil)
    (yk-db-status-create))

  (after-each
    (yk-db-status-close)
    (when (file-exists-p temp-db-file)
      (delete-file temp-db-file))
    (setq yk-db-status-file saved-status-file)
    (setq yk-db-status saved-db-status)
    ;; Clear memoization cache between tests
    (clrhash yk-status-table))

  (describe "yk-db-status-create"
    (it "creates a database file"
      ;; The file was created in before-each
      (expect (file-exists-p temp-db-file) :to-be-truthy)))

  (describe "yk-db-morph-status-insert and get"
    (it "roundtrips a morph record"
      (yk-db-morph-status-insert "猫" "名詞" "猫" "known")
      (let ((result (yk-db-morph-status-get "猫" "名詞" "猫")))
        (expect (nth 0 result) :to-equal "猫")     ;; morph
        (expect (nth 1 result) :to-equal "名詞")    ;; mtype
        (expect (nth 2 result) :to-equal "known"))) ;; status

    (it "returns default unknown record for missing morph"
      (let ((result (yk-db-morph-status-get "nonexistent" "名詞" "none")))
        (expect (nth 2 result) :to-equal "unknown"))))

  (describe "yk-db-morph-status-delete"
    (it "removes an existing record"
      (yk-db-morph-status-insert "犬" "名詞" "犬" "known")
      (yk-db-morph-status-delete "犬" "名詞" "犬")
      (let ((result (yk-db-morph-status-get "犬" "名詞" "犬")))
        (expect (nth 2 result) :to-equal "unknown"))))

  (describe "yk-db-morph-status-update"
    (it "changes status of existing record"
      (yk-db-morph-status-insert "食べる" "動詞" "食べる" "unknown")
      (yk-db-morph-status-update "食べる" "動詞" "食べる" "known")
      (let ((result (yk-db-morph-status-get "食べる" "動詞" "食べる")))
        (expect (nth 2 result) :to-equal "known")))

    (it "deletes record when updating to unknown"
      (yk-db-morph-status-insert "走る" "動詞" "走る" "known")
      (yk-db-morph-status-update "走る" "動詞" "走る" "unknown")
      (let ((result (yk-db-morph-status-get "走る" "動詞" "走る")))
        ;; Should return default record since it was deleted
        (expect (nth 0 result) :to-be nil))))

  (describe "yk-db-status-open and close"
    (it "nils out connection on close"
      (yk-db-status-close)
      (expect yk-db-status :to-be nil))

    (it "reopens after close"
      (yk-db-status-close)
      (yk-db-status-open)
      (expect yk-db-status :not :to-be nil))))

;;; --- Memoization tests ---

(describe "yk-morph-status-get (memoized)"
  :var (temp-db-file saved-status-file saved-db-status)

  (before-each
    (setq saved-status-file yk-db-status-file)
    (setq saved-db-status yk-db-status)
    (setq temp-db-file (make-temp-file "yomikun-test-" nil ".db"))
    (delete-file temp-db-file)
    (setq yk-db-status-file temp-db-file)
    (setq yk-db-status nil)
    (yk-db-status-create)
    (clrhash yk-status-table))

  (after-each
    (yk-db-status-close)
    (when (file-exists-p temp-db-file)
      (delete-file temp-db-file))
    (setq yk-db-status-file saved-status-file)
    (setq yk-db-status saved-db-status)
    (clrhash yk-status-table))

  (it "returns status from database"
    (yk-db-morph-status-insert "猫" "名詞" "猫" "learning")
    (expect (yk-morph-status-get "猫" "名詞" "猫")
            :to-equal "learning"))

  (it "caches result after first call"
    (yk-db-morph-status-insert "猫" "名詞" "猫" "learning")
    (yk-morph-status-get "猫" "名詞" "猫")
    ;; Verify it's in the cache
    (expect (gethash '("猫" "名詞" "猫") yk-status-table)
            :to-equal "learning"))

  (it "returns unknown for missing morph"
    (expect (yk-morph-status-get "nonexistent" "名詞" "x")
            :to-equal "unknown")))

(describe "yk-morph-status-set (memoized)"
  :var (temp-db-file saved-status-file saved-db-status)

  (before-each
    (setq saved-status-file yk-db-status-file)
    (setq saved-db-status yk-db-status)
    (setq temp-db-file (make-temp-file "yomikun-test-" nil ".db"))
    (delete-file temp-db-file)
    (setq yk-db-status-file temp-db-file)
    (setq yk-db-status nil)
    (yk-db-status-create)
    (clrhash yk-status-table))

  (after-each
    (yk-db-status-close)
    (when (file-exists-p temp-db-file)
      (delete-file temp-db-file))
    (setq yk-db-status-file saved-status-file)
    (setq yk-db-status saved-db-status)
    (clrhash yk-status-table))

  (it "updates both cache and database"
    (yk-morph-status-set "猫" "名詞" "猫" "known")
    ;; Check cache
    (expect (gethash '("猫" "名詞" "猫") yk-status-table)
            :to-equal "known")
    ;; Check database directly
    (let ((result (yk-db-morph-status-get "猫" "名詞" "猫")))
      (expect (nth 2 result) :to-equal "known")))

  (it "skips DB write when status unchanged"
    (yk-morph-status-set "猫" "名詞" "猫" "known")
    (spy-on 'yk-db-morph-status-update)
    (yk-morph-status-set "猫" "名詞" "猫" "known")
    (expect 'yk-db-morph-status-update :not :to-have-been-called)))

;;; --- Dictionary DB tests ---

(describe "yomikun dict-db"
  :var (temp-dict-file saved-dict-file saved-db-dict)

  (before-each
    (setq saved-dict-file yk-db-dict-file)
    (setq saved-db-dict yk-db-dict)
    (setq temp-dict-file (make-temp-file "yomikun-dict-test-" nil ".db"))
    (delete-file temp-dict-file)
    (setq yk-db-dict-file temp-dict-file)
    (setq yk-db-dict nil)
    ;; Create and populate a test dictionary database
    (setq yk-db-dict (emacsql-sqlite-open temp-dict-file))
    (emacsql yk-db-dict [:create-table entries
                          ([gloss pos root reading wtype])])
    (emacsql yk-db-dict [:create-table compounds
                          ([compound])])
    ;; Insert test entries
    (emacsql yk-db-dict [:insert :into entries :values ([$s1 $s2 $s3 $s4 $s5])]
             "cat" "noun" "猫" "ねこ" "名詞")
    (emacsql yk-db-dict [:insert :into entries :values ([$s1 $s2 $s3 $s4 $s5])]
             "dog" "noun" "犬" "いぬ" "名詞")
    (emacsql yk-db-dict [:insert :into entries :values ([$s1 $s2 $s3 $s4 $s5])]
             "eat (generic)" "verb" "食べる" "たべる" "動詞")
    ;; Insert test compounds
    (emacsql yk-db-dict [:insert :into compounds :values ([$s1])] "東京都")
    (emacsql yk-db-dict [:insert :into compounds :values ([$s1])] "東京")
    (emacsql yk-db-dict [:insert :into compounds :values ([$s1])] "東京駅")
    ;; Clear memoization caches
    (clrhash yk-dict-table)
    (clrhash yk-compound-candidates-table))

  (after-each
    (yk-db-dict-close)
    (when (file-exists-p temp-dict-file)
      (delete-file temp-dict-file))
    (setq yk-db-dict-file saved-dict-file)
    (setq yk-db-dict saved-db-dict)
    (clrhash yk-dict-table)
    (clrhash yk-compound-candidates-table))

  (describe "yk-db-dict-def"
    (it "finds entry with all three fields matching"
      (let ((result (yk-db-dict-def "猫" "ねこ" "名詞")))
        (expect result :to-be-truthy)
        (expect (nth 0 result) :to-equal "cat")))

    (it "falls back to root+reading when wtype doesn't match"
      (let ((result (yk-db-dict-def "猫" "ねこ" "bogus-type")))
        (expect result :to-be-truthy)
        (expect (nth 0 result) :to-equal "cat")))

    (it "falls back to root-only when reading doesn't match"
      (let ((result (yk-db-dict-def "猫" "bogus-reading" "bogus-type")))
        (expect result :to-be-truthy)
        (expect (nth 0 result) :to-equal "cat")))

    (it "returns nil when nothing matches"
      (let ((result (yk-db-dict-def "鳥" "とり" "名詞")))
        (expect result :to-be nil))))

  (describe "yk-dict-def (memoized)"
    (it "caches results after first lookup"
      (yk-dict-def "猫" "ねこ" "名詞")
      (expect (gethash '("猫" "ねこ" "名詞") yk-dict-table)
              :to-be-truthy))

    (it "returns cached result on second call"
      (let ((first (yk-dict-def "猫" "ねこ" "名詞"))
            (second (yk-dict-def "猫" "ねこ" "名詞")))
        (expect first :to-equal second))))

  (describe "yk-db-compound-prefix-candidates"
    (it "returns compounds matching prefix"
      ;; Prefix must be > 2 characters for the query to fire
      (let ((result (yk-db-compound-prefix-candidates "東京都")))
        (expect result :to-be-truthy)
        (expect (member "東京都" result) :to-be-truthy)))

    (it "returns nil for short prefix (<=2 chars)"
      (expect (yk-db-compound-prefix-candidates "東") :to-be nil)
      (expect (yk-db-compound-prefix-candidates "東京") :to-be nil))

    (it "returns nil for prefix with no matches"
      (expect (yk-db-compound-prefix-candidates "大阪府大") :to-be nil)))

  (describe "yk-db-compound-exists"
    (it "returns truthy for existing compound"
      (expect (yk-db-compound-exists "東京都") :to-be-truthy))

    (it "returns nil for non-existing compound"
      (expect (yk-db-compound-exists "大阪府") :to-be nil)))

  (describe "yk-db-dict-open and close"
    (it "nils out connection on close"
      (yk-db-dict-close)
      (expect yk-db-dict :to-be nil))

    (it "reopens after close"
      (yk-db-dict-close)
      (yk-db-dict-open)
      (expect yk-db-dict :not :to-be nil))))

;;; test-yomikun-db.el ends here
