;;; -*- lexical-binding: t; -*-
;;; test-yomikun.el --- Tests for yomikun main module

(require 'buttercup)

;;; --- yk-katakana-to-hiragana ---

(describe "yk-katakana-to-hiragana"
  (it "converts katakana to hiragana"
    (expect (yk-katakana-to-hiragana "アイウ") :to-equal "あいう"))

  (it "leaves hiragana unchanged"
    (expect (yk-katakana-to-hiragana "あいう") :to-equal "あいう"))

  (it "converts mixed katakana/ASCII"
    (expect (yk-katakana-to-hiragana "テストtest") :to-equal "てすとtest"))

  (it "returns empty string for empty input"
    (expect (yk-katakana-to-hiragana "") :to-equal ""))

  (it "leaves ASCII unchanged"
    (expect (yk-katakana-to-hiragana "hello") :to-equal "hello"))

  (it "handles mixed hiragana and katakana"
    (expect (yk-katakana-to-hiragana "あイう") :to-equal "あいう")))

;;; --- yk-has-japanese-characters-p ---

(describe "yk-has-japanese-characters-p"
  (it "returns nil for ASCII-only text"
    (expect (yk-has-japanese-characters-p "hello world") :to-be nil))

  (it "returns t for Japanese text"
    (expect (yk-has-japanese-characters-p "東京") :to-be-truthy))

  (it "returns t for mixed ASCII/Japanese"
    (expect (yk-has-japanese-characters-p "hello東京") :to-be-truthy))

  (it "returns nil for empty string"
    (expect (yk-has-japanese-characters-p "") :to-be nil)))

;;; --- yk-font-table-wtype-to-use ---

(describe "yk-font-table-wtype-to-use"
  (it "returns base table for known status"
    (expect (yk-font-table-wtype-to-use "known") :to-equal yk-wtype-table))

  (it "returns learning table for learning status"
    (expect (yk-font-table-wtype-to-use "learning")
            :to-equal yk-wtype-table-status-learning))

  (it "returns nil for ignore status"
    (expect (yk-font-table-wtype-to-use "ignore") :to-be nil))

  (it "returns unknown table for unrecognized status"
    (expect (yk-font-table-wtype-to-use "something-else")
            :to-equal yk-wtype-table-status-unknown))

  (it "returns unknown table for unknown status"
    (expect (yk-font-table-wtype-to-use "unknown")
            :to-equal yk-wtype-table-status-unknown)))

;;; --- yk-wtype-status-to-face ---

(describe "yk-wtype-status-to-face"
  (it "returns correct face for known noun"
    (expect (yk-wtype-status-to-face "名詞" "known")
            :to-equal 'yk-face-noun))

  (it "returns correct face for unknown verb"
    (expect (yk-wtype-status-to-face "動詞" "unknown")
            :to-equal 'yk-face-verb-unknown))

  (it "returns correct face for learning adjective"
    (expect (yk-wtype-status-to-face "形容詞" "learning")
            :to-equal 'yk-face-adjective-learning))

  (it "returns ignore face for ignored words"
    (expect (yk-wtype-status-to-face "名詞" "ignore")
            :to-equal 'yk-face-ignore))

  (it "falls back to status face for unknown wtype"
    (expect (yk-wtype-status-to-face "未知" "unknown")
            :to-equal 'yk-face-unknown)))

;;; --- yk-sort-hash-table ---

(describe "yk-sort-hash-table"
  (it "sorts entries by provided comparison function"
    (let ((ht (make-hash-table :test 'equal)))
      (puthash "a" 3 ht)
      (puthash "b" 1 ht)
      (puthash "c" 2 ht)
      (let ((sorted (yk-sort-hash-table ht
                      (lambda (a b) (< (cdr a) (cdr b))))))
        (expect (cdar sorted) :to-equal 1)
        (expect (cdadr sorted) :to-equal 2)
        (expect (cdaddr sorted) :to-equal 3))))

  (it "returns empty list for empty hash table"
    (let ((ht (make-hash-table :test 'equal)))
      (expect (yk-sort-hash-table ht (lambda (a b) t))
              :to-equal nil))))

;;; --- Buffer-based tests ---

(describe "yk-set-overlay"
  (it "creates overlay with the given name property"
    (with-temp-buffer
      (insert "東京")
      (yk-set-overlay 'yomikun 1 3 'yk-face-noun)
      (let ((ovs (overlays-in 1 3)))
        (expect (length ovs) :to-equal 1)
        (expect (overlay-get (car ovs) 'yomikun) :to-be t)
        (expect (overlay-get (car ovs) 'font-lock-face) :to-equal 'yk-face-noun))))

  (it "uses compound name when specified"
    (with-temp-buffer
      (insert "東京")
      (yk-set-overlay 'yomikun-comp 1 3 'yk-face-compound)
      (let ((ovs (overlays-in 1 3)))
        (expect (overlay-get (car ovs) 'yomikun-comp) :to-be t)
        ;; Should NOT have 'yomikun property
        (expect (overlay-get (car ovs) 'yomikun) :to-be nil)))))

(describe "yk-remove-props-and-overlays"
  (it "removes all yomikun text properties"
    (with-temp-buffer
      (insert "東京")
      (with-silent-modifications
        (put-text-property 1 3 'root "東京")
        (put-text-property 1 3 'wtype "名詞")
        (put-text-property 1 3 'yk-morph t))
      (yk-remove-props-and-overlays 1 3)
      (expect (get-text-property 1 'root) :to-be nil)
      (expect (get-text-property 1 'wtype) :to-be nil)
      (expect (get-text-property 1 'yk-morph) :to-be nil)))

  (it "removes both yomikun and yomikun-comp overlays"
    (with-temp-buffer
      (insert "東京")
      (yk-set-overlay 'yomikun 1 3 'yk-face-noun)
      (yk-set-overlay 'yomikun-comp 1 3 'yk-face-compound)
      (expect (length (overlays-in 1 3)) :to-equal 2)
      (yk-remove-props-and-overlays 1 3)
      (expect (length (overlays-in 1 3)) :to-equal 0))))

(describe "yk-morph-matches-at"
  (it "returns t when properties match at position"
    (with-temp-buffer
      (insert "猫は")
      (with-silent-modifications
        (put-text-property 1 2 'root "猫")
        (put-text-property 1 2 'wtype "名詞")
        (put-text-property 1 2 'surface "猫"))
      (let ((props '(root "猫" wtype "名詞" surface "猫")))
        (expect (yk-morph-matches-at props 1) :to-be-truthy))))

  (it "returns nil when properties don't match"
    (with-temp-buffer
      (insert "猫は")
      (with-silent-modifications
        (put-text-property 1 2 'root "猫")
        (put-text-property 1 2 'wtype "名詞")
        (put-text-property 1 2 'surface "猫"))
      (let ((props '(root "犬" wtype "名詞" surface "犬")))
        (expect (yk-morph-matches-at props 1) :to-be nil))))

  (it "uses the passed position, not (point)"
    (with-temp-buffer
      (insert "猫は犬")
      (with-silent-modifications
        (put-text-property 1 2 'root "猫")
        (put-text-property 1 2 'wtype "名詞")
        (put-text-property 1 2 'surface "猫")
        (put-text-property 3 4 'root "犬")
        (put-text-property 3 4 'wtype "名詞")
        (put-text-property 3 4 'surface "犬"))
      ;; Point is at position 1, but we ask about position 3
      (goto-char 1)
      (let ((props '(root "犬" wtype "名詞" surface "犬")))
        (expect (yk-morph-matches-at props 3) :to-be-truthy)))))

(describe "yk-mark-sentence-at-point"
  (it "selects sentence delimited by 。"
    (with-temp-buffer
      (insert "猫は好き。犬も好き。")
      (goto-char 3) ;; inside first sentence
      (yk-mark-sentence-at-point)
      (expect (region-beginning) :to-equal 1)
      (expect (region-end) :to-equal 6))) ;; includes 。

  (it "selects sentence delimited by newline"
    (with-temp-buffer
      (insert "猫は好き\n犬も好き\n")
      (goto-char 3)
      (yk-mark-sentence-at-point)
      (expect (region-beginning) :to-equal 1)
      (expect (region-end) :to-equal 5))))

;;; --- Token-to-buffer pipeline ---

(defun yk-test--make-token (seen wtype root surface pronun begin end)
  "Helper: create a token plist matching what yk-mecab--parse-output returns."
  (list 'seen seen 'surface surface 'wtype wtype
        'root root 'pronun pronun 'begin begin 'end end))

(describe "yk-set-text-prop-token"
  :var (temp-db saved-status-file saved-db-status)

  (before-each
    (setq saved-status-file yk-db-status-file)
    (setq saved-db-status yk-db-status)
    (setq temp-db (make-temp-file "yomikun-test-" nil ".db"))
    (delete-file temp-db)
    (setq yk-db-status-file temp-db)
    (setq yk-db-status nil)
    (yk-db-status-create)
    (clrhash yk-status-table))

  (after-each
    (yk-db-status-close)
    (when (file-exists-p temp-db) (delete-file temp-db))
    (setq yk-db-status-file saved-status-file)
    (setq yk-db-status saved-db-status)
    (clrhash yk-status-table))

  (it "sets all expected text properties on a token"
    (with-temp-buffer
      (insert "猫が好き")
      (with-silent-modifications
        (let ((token (yk-test--make-token "猫" "名詞" "猫" "猫" "ネコ" 1 1)))
          (yk-set-text-prop-token token)
          (expect (get-text-property 1 'root) :to-equal "猫")
          (expect (get-text-property 1 'wtype) :to-equal "名詞")
          (expect (get-text-property 1 'surface) :to-equal "猫")
          (expect (get-text-property 1 'seen) :to-equal "猫")
          (expect (get-text-property 1 'yk-morph) :to-be t)
          (expect (get-text-property 1 'pronun) :to-equal "ねこ")  ;; katakana→hiragana
          (expect (get-text-property 1 'begin) :to-equal 1)
          (expect (get-text-property 1 'status) :to-be-truthy)))))

  (it "creates an overlay with correct face for unknown word"
    (with-temp-buffer
      (insert "猫が好き")
      (with-silent-modifications
        (let ((token (yk-test--make-token "猫" "名詞" "猫" "猫" "ネコ" 1 1)))
          (yk-set-text-prop-token token)
          (let ((ovs (overlays-in 1 2)))
            (expect (length ovs) :to-be-greater-than 0)
            (expect (overlay-get (car ovs) 'yomikun) :to-be t))))))

  (it "sets cursor-sensor for unknown words"
    (with-temp-buffer
      (insert "猫が好き")
      (with-silent-modifications
        (let ((token (yk-test--make-token "猫" "名詞" "猫" "猫" "ネコ" 1 1)))
          (yk-set-text-prop-token token)
          (expect (get-text-property 1 'cursor-sensor-functions)
                  :to-be-truthy)))))

  (it "converts pronunciation from katakana to hiragana"
    (with-temp-buffer
      (insert "東京です")
      (with-silent-modifications
        (let ((token (yk-test--make-token "東京" "名詞" "東京" "東京" "トウキョウ" 1 2)))
          (yk-set-text-prop-token token)
          (expect (get-text-property 1 'pronun) :to-equal "とうきょう"))))))

;;; --- yk-process-tokens ---

(describe "yk-process-tokens"
  :var (temp-db saved-status-file saved-db-status)

  (before-each
    (setq saved-status-file yk-db-status-file)
    (setq saved-db-status yk-db-status)
    (setq temp-db (make-temp-file "yomikun-test-" nil ".db"))
    (delete-file temp-db)
    (setq yk-db-status-file temp-db)
    (setq yk-db-status nil)
    (yk-db-status-create)
    (clrhash yk-status-table))

  (after-each
    (yk-db-status-close)
    (when (file-exists-p temp-db) (delete-file temp-db))
    (setq yk-db-status-file saved-status-file)
    (setq yk-db-status saved-db-status)
    (clrhash yk-status-table))

  (it "applies properties to all tokens in the list"
    (with-temp-buffer
      (insert "猫が好き")
      (let ((tokens (list
                     (yk-test--make-token "猫" "名詞" "猫" "猫" "ネコ" 1 1)
                     (yk-test--make-token "が" "助詞" "が" "が" "ガ" 2 2)
                     (yk-test--make-token "好き" "形容詞" "好き" "好き" "スキ" 3 4))))
        (yk-process-tokens tokens)
        (expect (get-text-property 1 'root) :to-equal "猫")
        (expect (get-text-property 2 'root) :to-equal "が")
        (expect (get-text-property 3 'root) :to-equal "好き"))))

  (it "skips non-Japanese tokens"
    (with-temp-buffer
      (insert "I am 猫")
      (let ((tokens (list
                     (yk-test--make-token "I" "名詞" "I" "I" "" 1 1)
                     (yk-test--make-token "猫" "名詞" "猫" "猫" "ネコ" 5 5))))
        (yk-process-tokens tokens)
        ;; ASCII "I" should not be processed
        (expect (get-text-property 1 'root) :to-be nil)
        ;; Japanese "猫" should be processed
        (expect (get-text-property 5 'root) :to-equal "猫")))))

;;; --- yk-set-overlay-wtype-at-pos ---

(describe "yk-set-overlay-wtype-at-pos"
  (it "creates overlay with face matching wtype and status"
    (with-temp-buffer
      (insert "猫")
      (yk-set-overlay-wtype-at-pos 1 2 "名詞" "unknown")
      (let ((ovs (overlays-in 1 2)))
        (expect (length ovs) :to-equal 1)
        (expect (overlay-get (car ovs) 'font-lock-face)
                :to-equal 'yk-face-noun-unknown))))

  (it "replaces existing overlay on second call"
    (with-temp-buffer
      (insert "猫")
      (yk-set-overlay-wtype-at-pos 1 2 "名詞" "unknown")
      (yk-set-overlay-wtype-at-pos 1 2 "名詞" "known")
      (let ((ovs (overlays-in 1 2)))
        (expect (length ovs) :to-equal 1)
        (expect (overlay-get (car ovs) 'font-lock-face)
                :to-equal 'yk-face-noun))))

  (it "creates no overlay for ignored words"
    (with-temp-buffer
      (insert "猫")
      (yk-set-overlay-wtype-at-pos 1 2 "名詞" "ignore")
      ;; ignore falls back to yk-face-ignore via yk-font-table-default-status
      (let ((ovs (overlays-in 1 2)))
        (expect (length ovs) :to-equal 1)
        (expect (overlay-get (car ovs) 'font-lock-face)
                :to-equal 'yk-face-ignore)))))

;;; --- Morph iteration ---

(defun yk-test--setup-morph-buffer ()
  "Set up a buffer with morph properties on '猫が好き'.
Returns the buffer. Caller must kill it."
  (let ((buf (generate-new-buffer " *yk-test*")))
    (with-current-buffer buf
      (insert "猫が好き")
      (with-silent-modifications
        ;; 猫 at pos 1
        (put-text-property 1 2 'begin 1)
        (put-text-property 1 2 'end 2)
        (put-text-property 1 2 'root "猫")
        (put-text-property 1 2 'wtype "名詞")
        (put-text-property 1 2 'surface "猫")
        (put-text-property 1 2 'yk-morph t)
        ;; が at pos 2
        (put-text-property 2 3 'begin 2)
        (put-text-property 2 3 'end 3)
        (put-text-property 2 3 'root "が")
        (put-text-property 2 3 'wtype "助詞")
        (put-text-property 2 3 'surface "が")
        (put-text-property 2 3 'yk-morph t)
        ;; 好き at pos 3-4
        (put-text-property 3 5 'begin 3)
        (put-text-property 3 5 'end 5)
        (put-text-property 3 5 'root "好き")
        (put-text-property 3 5 'wtype "形容詞")
        (put-text-property 3 5 'surface "好き")
        (put-text-property 3 5 'yk-morph t)))
    buf))

(describe "yk-morph-do-morphs"
  (it "visits all morphs in buffer"
    (let ((buf (yk-test--setup-morph-buffer)))
      (unwind-protect
          (with-current-buffer buf
            (let ((visited nil))
              (yk-morph-do-morphs
               (lambda (_pos) t)
               (lambda (beg _end)
                 (push (get-text-property beg 'root) visited)))
              (setq visited (nreverse visited))
              (expect visited :to-equal '("猫" "が" "好き"))))
        (kill-buffer buf))))

  (it "filters morphs by predicate"
    (let ((buf (yk-test--setup-morph-buffer)))
      (unwind-protect
          (with-current-buffer buf
            (let ((visited nil))
              (yk-morph-do-morphs
               (lambda (pos)
                 (string-equal (get-text-property pos 'wtype) "名詞"))
               (lambda (beg _end)
                 (push (get-text-property beg 'root) visited)))
              (expect visited :to-equal '("猫"))))
        (kill-buffer buf)))))

(describe "yk-morph-do-morphs-in-region"
  (it "only visits morphs within the specified region"
    (let ((buf (yk-test--setup-morph-buffer)))
      (unwind-protect
          (with-current-buffer buf
            (let ((visited nil))
              (yk-morph-do-morphs-in-region 2 5
               (lambda (_pos) t)
               (lambda (beg _end)
                 (push (get-text-property beg 'root) visited)))
              (setq visited (nreverse visited))
              (expect visited :to-equal '("が" "好き"))))
        (kill-buffer buf)))))

(describe "yk-get-tokens-region"
  (it "returns token list for the region"
    (let ((buf (yk-test--setup-morph-buffer)))
      (unwind-protect
          (with-current-buffer buf
            (let ((tokens (yk-get-tokens-region 1 5)))
              (expect (length tokens) :to-equal 3)
              ;; Each token is (pos surface seen)
              (expect (nth 1 (car tokens)) :to-equal "猫")))
        (kill-buffer buf))))

  (it "returns empty list for region with no morphs"
    (with-temp-buffer
      (insert "hello world")
      (let ((tokens (yk-get-tokens-region 1 12)))
        (expect tokens :to-equal nil)))))

;;; --- Status change lifecycle ---

(describe "yk-morph-new-status"
  :var (temp-db saved-status-file saved-db-status)

  (before-each
    (setq saved-status-file yk-db-status-file)
    (setq saved-db-status yk-db-status)
    (setq temp-db (make-temp-file "yomikun-test-" nil ".db"))
    (delete-file temp-db)
    (setq yk-db-status-file temp-db)
    (setq yk-db-status nil)
    (yk-db-status-create)
    (clrhash yk-status-table))

  (after-each
    (yk-db-status-close)
    (when (file-exists-p temp-db) (delete-file temp-db))
    (setq yk-db-status-file saved-status-file)
    (setq yk-db-status saved-db-status)
    (clrhash yk-status-table))

  (it "updates status in database and overlays across the buffer"
    (with-temp-buffer
      (insert "猫が猫が") ;; 猫 appears twice, with trailing char for property boundary
      (with-silent-modifications
        ;; Set up both 猫 tokens with "unknown" status
        (put-text-property 1 2 'root "猫")
        (put-text-property 1 2 'wtype "名詞")
        (put-text-property 1 2 'surface "猫")
        (put-text-property 1 2 'status "unknown")
        (put-text-property 1 2 'begin 1)
        (put-text-property 1 2 'yk-morph t)
        (yk-set-overlay 'yomikun 1 2 'yk-face-noun-unknown)

        (put-text-property 2 3 'root "が")
        (put-text-property 2 3 'wtype "助詞")
        (put-text-property 2 3 'surface "が")
        (put-text-property 2 3 'begin 2)
        (put-text-property 2 3 'yk-morph t)

        (put-text-property 3 4 'root "猫")
        (put-text-property 3 4 'wtype "名詞")
        (put-text-property 3 4 'surface "猫")
        (put-text-property 3 4 'status "unknown")
        (put-text-property 3 4 'begin 3)
        (put-text-property 3 4 'yk-morph t)
        (yk-set-overlay 'yomikun 3 4 'yk-face-noun-unknown)

        (put-text-property 4 5 'root "が")
        (put-text-property 4 5 'wtype "助詞")
        (put-text-property 4 5 'surface "が")
        (put-text-property 4 5 'begin 4)
        (put-text-property 4 5 'yk-morph t))

      ;; Mark first 猫 as known
      (let ((props (text-properties-at 1)))
        (yk-morph-new-status props "known"))

      ;; Database should reflect the change
      (expect (yk-morph-status-get "猫" "名詞" "猫") :to-equal "known")))

  (it "does nothing when status is already the same"
    (with-temp-buffer
      (insert "猫")
      (with-silent-modifications
        (put-text-property 1 2 'root "猫")
        (put-text-property 1 2 'wtype "名詞")
        (put-text-property 1 2 'surface "猫")
        (put-text-property 1 2 'status "unknown")
        (put-text-property 1 2 'begin 1)
        (put-text-property 1 2 'yk-morph t))
      (spy-on 'yk-morph-status-set)
      (let ((props (text-properties-at 1)))
        (yk-morph-new-status props "unknown"))
      (expect 'yk-morph-status-set :not :to-have-been-called))))

;;; --- yk-replace-all-overlays ---

(describe "yk-replace-all-overlays"
  (it "updates overlays for all matching morphs"
    (with-temp-buffer
      (insert "猫が猫が")  ;; 4 chars so last 猫 has room
      (with-silent-modifications
        ;; First 猫 at pos 1
        (put-text-property 1 2 'root "猫")
        (put-text-property 1 2 'wtype "名詞")
        (put-text-property 1 2 'surface "猫")
        (put-text-property 1 2 'begin 1)
        (put-text-property 1 2 'yk-morph t)
        (yk-set-overlay 'yomikun 1 2 'yk-face-noun-unknown)

        ;; Second 猫 at pos 3
        (put-text-property 3 4 'root "猫")
        (put-text-property 3 4 'wtype "名詞")
        (put-text-property 3 4 'surface "猫")
        (put-text-property 3 4 'begin 3)
        (put-text-property 3 4 'yk-morph t)
        (yk-set-overlay 'yomikun 3 4 'yk-face-noun-unknown)

        ;; が at pos 2 and 4 should not be affected
        (put-text-property 2 3 'root "が")
        (put-text-property 2 3 'wtype "助詞")
        (put-text-property 2 3 'surface "が")
        (put-text-property 2 3 'begin 2)
        (put-text-property 2 3 'yk-morph t)
        (yk-set-overlay 'yomikun 2 3 'yk-face-particle-unknown)

        (put-text-property 4 5 'root "が")
        (put-text-property 4 5 'wtype "助詞")
        (put-text-property 4 5 'surface "が")
        (put-text-property 4 5 'begin 4)
        (put-text-property 4 5 'yk-morph t)
        (yk-set-overlay 'yomikun 4 5 'yk-face-particle-unknown))

      (let ((props '(root "猫" wtype "名詞" surface "猫")))
        (yk-replace-all-overlays props "known"))

      ;; Both 猫 overlays should be updated to known face
      (let ((ov1 (car (overlays-at 1)))
            (ov2 (car (overlays-at 3))))
        (expect (overlay-get ov1 'font-lock-face) :to-equal 'yk-face-noun)
        (expect (overlay-get ov2 'font-lock-face) :to-equal 'yk-face-noun))

      ;; が should still have unknown face
      (let ((ov-ga (car (overlays-at 2))))
        (expect (overlay-get ov-ga 'font-lock-face)
                :to-equal 'yk-face-particle-unknown)))))

;;; --- Compound detection (pure functions) ---

(describe "yk-build-potential-candidates"
  ;; lst items are (pos surface seen) — matching yk-get-tokens-region format
  (it "builds 2-token candidates from a token list"
    (let* ((lst '((1 "東" "東") (2 "京" "京") (3 "都" "都")))
           (result (yk-build-potential-candidates lst 2)))
      ;; prefix = seen of token 0 = "東"
      ;; suffix = (surface, seen) of token 1 = ("京" "京")
      (expect result :to-be-truthy)
      (expect (member "東京" result) :to-be-truthy)))

  (it "builds 3-token candidates"
    (let* ((lst '((1 "東" "東") (2 "京" "京") (3 "都" "都")))
           (result (yk-build-potential-candidates lst 3)))
      ;; prefix = seen of tokens 0-1 = "東京"
      ;; suffix = (surface, seen) of token 2 = ("都" "都")
      (expect result :to-be-truthy)
      (expect (member "東京都" result) :to-be-truthy)))

  (it "returns nil when list is shorter than requested length"
    (let* ((lst '((1 "東" "東")))
           (result (yk-build-potential-candidates lst 2)))
      (expect result :to-be nil)))

  (it "returns nil for length 1"
    (let* ((lst '((1 "東" "東") (2 "京" "京")))
           (result (yk-build-potential-candidates lst 1)))
      ;; length 1 means (- len 1) = 0, prefix is empty, suffix is first token
      ;; This produces ("東" "東") — concatenation of "" + each suffix
      (expect result :to-be-truthy))))

(describe "yk-find-compound"
  (it "finds the longest matching compound"
    (let* ((lst '((1 "東" "東") (2 "京" "京") (3 "都" "都")))
           (candidates '("東京" "東京都")))
      (let ((result (yk-find-compound lst candidates)))
        (expect result :to-be-truthy)
        ;; Should find "東京都" (3 tokens) as the longest match
        (expect (nth 0 result) :to-equal "東京都")
        (expect (nth 1 result) :to-equal 3))))

  (it "returns nil when no compound matches"
    (let* ((lst '((1 "猫" "猫") (2 "が" "が")))
           (candidates '("東京都")))
      (expect (yk-find-compound lst candidates) :to-be nil)))

  (it "finds 2-token compound when 3-token doesn't match"
    (let* ((lst '((1 "東" "東") (2 "京" "京") (3 "猫" "猫")))
           (candidates '("東京")))
      (let ((result (yk-find-compound lst candidates)))
        (expect (nth 0 result) :to-equal "東京")
        (expect (nth 1 result) :to-equal 2)))))

;;; --- yk-process-wtype-p ---

(describe "yk-process-wtype-p"
  (it "returns truthy for known word types"
    (expect (yk-process-wtype-p '(wtype "名詞")) :to-be-truthy)
    (expect (yk-process-wtype-p '(wtype "動詞")) :to-be-truthy)
    (expect (yk-process-wtype-p '(wtype "助詞")) :to-be-truthy))

  (it "returns nil for unknown word types"
    (expect (yk-process-wtype-p '(wtype "接頭辞")) :to-be nil)
    (expect (yk-process-wtype-p '(wtype "other")) :to-be nil)))

;;; --- yk-extract-text ---

(describe "yk-extract-text"
  (it "extracts Japanese text up to punctuation"
    (with-temp-buffer
      (insert "猫が好き。犬も好き。")
      (goto-char 1)
      ;; Extracts from point to one char before the 。
      (expect (yk-extract-text) :to-equal "猫が好")))

  (it "extracts ASCII word at point"
    (with-temp-buffer
      (insert "hello world")
      (goto-char 2)
      (expect (yk-extract-text) :to-equal "hello"))))

;;; --- yk-properties-at-point ---

(describe "yk-properties-at-point"
  (it "returns properties when on a morph"
    (with-temp-buffer
      (insert "猫")
      (with-silent-modifications
        (put-text-property 1 2 'yk-morph t)
        (put-text-property 1 2 'root "猫"))
      (goto-char 1)
      (let ((props (yk-properties-at-point)))
        (expect props :to-be-truthy)
        (expect (plist-get props 'root) :to-equal "猫"))))

  (it "returns nil when not on a morph"
    (with-temp-buffer
      (insert "hello")
      (goto-char 1)
      (expect (yk-properties-at-point) :to-be nil))))

;;; test-yomikun.el ends here
