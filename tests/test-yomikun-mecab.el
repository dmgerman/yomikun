;;; -*- lexical-binding: t; -*-
;;; test-yomikun-mecab.el --- Tests for yomikun-mecab module

(require 'buttercup)

;;; --- yk-mecab--process-root-for-kana ---

(describe "yk-mecab--process-root-for-kana"
  (it "strips romaji suffix from kana words"
    (expect (yk-mecab--process-root-for-kana "ドーム-dome")
            :to-equal "ドーム"))

  (it "keeps non-romaji suffix after hyphen"
    (expect (yk-mecab--process-root-for-kana "東京-東京")
            :to-equal "東京-東京"))

  (it "returns nil for nil input"
    (expect (yk-mecab--process-root-for-kana nil)
            :to-equal nil))

  (it "returns word unchanged when no hyphen"
    (expect (yk-mecab--process-root-for-kana "猫")
            :to-equal "猫"))

  (it "handles word with only romaji after hyphen"
    (expect (yk-mecab--process-root-for-kana "テスト-test")
            :to-equal "テスト"))

  (it "keeps hyphen when suffix is mixed"
    (expect (yk-mecab--process-root-for-kana "foo-bar1")
            :to-equal "foo-bar1")))

;;; --- yk-mecab--build-byte-to-char-table ---

(describe "yk-mecab--build-byte-to-char-table"
  (it "maps ASCII text correctly (1 byte per char)"
    (let ((table (yk-mecab--build-byte-to-char-table "abc")))
      (expect (aref table 0) :to-equal 0)
      (expect (aref table 1) :to-equal 1)
      (expect (aref table 2) :to-equal 2)
      (expect (aref table 3) :to-equal 3))) ;; sentinel

  (it "maps Japanese text correctly (3 bytes per char in UTF-8)"
    (let ((table (yk-mecab--build-byte-to-char-table "東京")))
      (expect (aref table 0) :to-equal 0)  ;; 東 starts at byte 0
      (expect (aref table 3) :to-equal 1)  ;; 京 starts at byte 3
      (expect (aref table 6) :to-equal 2))) ;; sentinel

  (it "maps mixed ASCII/Japanese text"
    (let ((table (yk-mecab--build-byte-to-char-table "a東b")))
      (expect (aref table 0) :to-equal 0)  ;; a at byte 0
      (expect (aref table 1) :to-equal 1)  ;; 東 at byte 1
      (expect (aref table 4) :to-equal 2)  ;; b at byte 4
      (expect (aref table 5) :to-equal 3))) ;; sentinel

  (it "returns nil for intermediate byte positions"
    (let ((table (yk-mecab--build-byte-to-char-table "東")))
      (expect (aref table 1) :to-equal nil)
      (expect (aref table 2) :to-equal nil)))

  (it "handles empty string"
    (let ((table (yk-mecab--build-byte-to-char-table "")))
      (expect (aref table 0) :to-equal 0)
      (expect (length table) :to-equal 1))))

;;; --- yk-mecab--parse-line ---

(describe "yk-mecab--parse-line"
  (let ((byte-to-char (yk-mecab--build-byte-to-char-table "東京都に住んでいる")))

    (it "parses a valid 7-field line"
      (let ((result (yk-mecab--parse-line
                     "東京\t名詞\t東京\tトウキョウ\t東京\t0\t6"
                     byte-to-char 1)))
        (expect result :not :to-be nil)
        (expect (plist-get result 'seen) :to-equal "東京")
        (expect (plist-get result 'wtype) :to-equal "名詞")
        (expect (plist-get result 'root) :to-equal "東京")
        (expect (plist-get result 'pronun) :to-equal "トウキョウ")
        (expect (plist-get result 'begin) :to-equal 1)   ;; char 0 + offset 1
        (expect (plist-get result 'end) :to-equal 2)))    ;; char 2 + offset 1 - 1

    (it "returns nil for empty line"
      (expect (yk-mecab--parse-line "" byte-to-char 1)
              :to-be nil))

    (it "returns nil for nil line"
      (expect (yk-mecab--parse-line nil byte-to-char 1)
              :to-be nil))

    (it "returns nil for line with fewer than 7 fields"
      (expect (yk-mecab--parse-line "東京\t名詞\t東京" byte-to-char 1)
              :to-be nil))

    (it "applies region-offset to positions"
      (let ((result (yk-mecab--parse-line
                     "東京\t名詞\t東京\tトウキョウ\t東京\t0\t6"
                     byte-to-char 100)))
        (expect (plist-get result 'begin) :to-equal 100)
        (expect (plist-get result 'end) :to-equal 101)))))

;;; --- yk-mecab--parse-output ---

(describe "yk-mecab--parse-output"
  (it "parses multiple lines into token list"
    (let ((tokens (yk-mecab--parse-output
                   yk-test-mecab-output-tokyo
                   yk-test-input-tokyo
                   1)))
      (expect (length tokens) :to-be-greater-than 0)
      ;; First token should be 東京
      (expect (plist-get (car tokens) 'seen) :to-equal "東京")
      (expect (plist-get (car tokens) 'wtype) :to-equal "名詞")))

  (it "returns empty list for empty output"
    (expect (yk-mecab--parse-output "" "test" 1)
            :to-equal nil))

  (it "filters out empty/unparseable lines"
    (let ((tokens (yk-mecab--parse-output
                   "\n\n東京\t名詞\t東京\tトウキョウ\t東京\t0\t6\n\n"
                   "東京"
                   1)))
      (expect (length tokens) :to-equal 1)))

  (it "correctly handles mixed ASCII/Japanese byte offsets"
    (let ((tokens (yk-mecab--parse-output
                   yk-test-mecab-output-mixed
                   yk-test-input-mixed
                   1)))
      ;; "猫" is at char position 5 in "I am 猫"
      (let ((neko (cl-find "猫" tokens
                           :key (lambda (t) (plist-get t 'seen))
                           :test #'string-equal)))
        (expect neko :not :to-be nil)
        (expect (plist-get neko 'begin) :to-equal 6)     ;; char 5 + offset 1
        (expect (plist-get neko 'end) :to-equal 6)))))   ;; single char, inclusive

;;; --- yk-mecab--build-args ---

(describe "yk-mecab--build-args"
  (before-each
    (setq yk-mecab-dict-type 'unidic)
    (setq yk-mecab-dict-dir nil)
    ;; Bypass validation since we test arg construction, not config
    (spy-on 'yk-mecab--validate-binary)
    (spy-on 'yk-mecab--validate-dict-dir)
    (spy-on 'file-exists-p :and-return-value t))

  (after-each
    (setq yk-mecab-dict-type nil)
    (setq yk-mecab-dict-dir nil))

  (it "includes --node-format flag"
    (let ((args (yk-mecab--build-args "/tmp/test.txt")))
      (expect (cl-some (lambda (a) (string-prefix-p "--node-format=" a)) args)
              :to-be-truthy)))

  (it "includes --unk-format flag"
    (let ((args (yk-mecab--build-args "/tmp/test.txt")))
      (expect (cl-some (lambda (a) (string-prefix-p "--unk-format=" a)) args)
              :to-be-truthy)))

  (it "includes input file as last argument"
    (let ((args (yk-mecab--build-args "/tmp/test.txt")))
      (expect (car (last args)) :to-equal "/tmp/test.txt")))

  (it "includes -d flag when dict-dir is set"
    (setq yk-mecab-dict-dir "/usr/local/lib/mecab/dic/ipadic")
    (let ((args (yk-mecab--build-args "/tmp/test.txt")))
      (expect (member "-d" args) :to-be-truthy)))

  (it "omits -d flag when dict-dir is nil"
    (let ((args (yk-mecab--build-args "/tmp/test.txt")))
      (expect (member "-d" args) :to-be nil))))

;;; --- yk-mecab--get-dict-config ---

(describe "yk-mecab--get-dict-config"
  (it "returns unidic config when type is unidic"
    (let ((yk-mecab-dict-type 'unidic))
      (let ((config (yk-mecab--get-dict-config)))
        (expect (cdr (assq 'name config)) :to-equal "UniDic"))))

  (it "returns ipadic config when type is ipadic"
    (let ((yk-mecab-dict-type 'ipadic))
      (let ((config (yk-mecab--get-dict-config)))
        (expect (cdr (assq 'name config)) :to-equal "IPAdic"))))

  (it "errors on unknown dictionary type"
    (let ((yk-mecab-dict-type 'bogus))
      (expect (yk-mecab--get-dict-config) :to-throw 'error))))

;;; --- yk-mecab--write-temp-file ---

(describe "yk-mecab--write-temp-file"
  (it "creates a file with correct UTF-8 content"
    (let ((temp-file (yk-mecab--write-temp-file "東京都")))
      (unwind-protect
          (let ((content (with-temp-buffer
                           (insert-file-contents temp-file)
                           (buffer-string))))
            (expect content :to-equal "東京都"))
        (delete-file temp-file))))

  (it "returns a path that exists"
    (let ((temp-file (yk-mecab--write-temp-file "test")))
      (unwind-protect
          (expect (file-exists-p temp-file) :to-be-truthy)
        (delete-file temp-file)))))

;;; --- Integration Tests (require mecab) ---

(describe "mecab integration"
  :var (mecab-works)

  (before-all
    ;; Check that mecab is available AND has a working dictionary
    (setq mecab-works
          (and (yk-test-mecab-available-p)
               (condition-case nil
                   (progn (yk-mecab--detect-dict-type) t)
                 (error nil)))))

  (it "detects dictionary type"
    (assume mecab-works "mecab not available or not configured")
    (let ((dict-type (yk-mecab--detect-dict-type)))
      (expect dict-type :to-be-truthy)
      (expect (assq dict-type yk-mecab-dictionary-registry) :to-be-truthy)))

  (it "tokenizes Japanese text end-to-end"
    (assume mecab-works "mecab not available or not configured")
    (let* ((input "東京都に住んでいる")
           (temp-file (yk-mecab--write-temp-file input)))
      (unwind-protect
          (let* ((args (yk-mecab--build-args temp-file))
                 (output (yk-mecab--run-command (cons yk-mecab-binary args)))
                 (tokens (yk-mecab--parse-output output input 1)))
            (expect (length tokens) :to-be-greater-than 0)
            ;; Verify positions are within text range
            (dolist (tok tokens)
              (expect (plist-get tok 'begin) :to-be-greater-than 0)
              (expect (plist-get tok 'end) :to-be-less-than
                      (+ 1 (length input) 1))))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

;;; test-yomikun-mecab.el ends here
