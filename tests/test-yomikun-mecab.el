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

  (it "replaces newlines with spaces to preserve byte offsets"
    (let ((temp-file (yk-mecab--write-temp-file "東京\n大阪")))
      (unwind-protect
          (let ((content (with-temp-buffer
                           (insert-file-contents temp-file)
                           (buffer-string))))
            (expect content :to-equal "東京 大阪"))
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

;;; --- Chunking tests ---

(describe "yk-mecab--split-into-chunks"
  (it "returns a single chunk for small text"
    (let ((chunks (yk-mecab--split-into-chunks "東京都")))
      (expect (length chunks) :to-equal 1)
      (expect (car (car chunks)) :to-equal "東京都")
      (expect (cdr (car chunks)) :to-equal 0)))

  (it "splits large text into multiple chunks"
    ;; Build a string that exceeds 7000 bytes (Japanese chars = 3 bytes each)
    ;; 2400 chars * 3 bytes = 7200 bytes > 7000
    (let* ((base "東京都に住んでいる。")  ;; 10 chars, 28 bytes (9 JP + 1 punct)
           (repeated (apply #'concat (make-list 250 base)))  ;; 2500 chars
           (chunks (yk-mecab--split-into-chunks repeated)))
      (expect (length chunks) :to-be-greater-than 1)
      ;; Verify offsets are sequential and non-overlapping
      (let ((prev-end 0))
        (dolist (chunk chunks)
          (expect (cdr chunk) :to-equal prev-end)
          (setq prev-end (+ (cdr chunk) (length (car chunk))))))
      ;; Verify concatenating all chunks reconstructs the original
      (let ((reconstructed (mapconcat #'car chunks "")))
        (expect reconstructed :to-equal repeated))))

  (it "prefers paragraph boundaries over sentence boundaries"
    (let* ((para1 (apply #'concat (make-list 80 "東京都に住む。")))  ;; ~80 sentences
           (para2 (apply #'concat (make-list 80 "大阪府で暮らす。")))
           (text (concat para1 "\n\n" para2))
           (chunks (yk-mecab--split-into-chunks text)))
      ;; If paragraph boundary is within range, should split there
      (when (> (length chunks) 1)
        (let ((first-chunk (car (car chunks))))
          ;; First chunk should end at the paragraph boundary
          (expect (string-suffix-p "\n\n" first-chunk) :to-be-truthy)))))

  (it "splits at sentence boundaries when possible"
    (let* ((sentence "東京都に住んでいる。")
           ;; ~240 sentences * ~28 bytes = ~6720 bytes per chunk
           (repeated (apply #'concat (make-list 500 sentence)))
           (chunks (yk-mecab--split-into-chunks repeated)))
      ;; Each chunk should end at a 。boundary (except possibly the last)
      (dolist (chunk (butlast chunks))
        (let ((text (car chunk)))
          (expect (string-suffix-p "。" text) :to-be-truthy))))))

;;; --- Full file integration test ---

(defvar yk-test--fixture-dir
  (expand-file-name "fixtures/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing test fixture files.")

(describe "full file parsing"
  :var (mecab-works)

  (before-all
    (setq mecab-works
          (and (yk-test-mecab-available-p)
               (condition-case nil
                   (progn (yk-mecab--detect-dict-type) t)
                 (error nil)))))

  (it "parses kumo_no_ito.txt with correct overlay alignment"
    (assume mecab-works "mecab not available or not configured")
    (let* ((fixture-file (expand-file-name "kumo_no_ito.txt" yk-test--fixture-dir))
           ;; Read and duplicate the file content to ensure > 8KB
           (base-text (with-temp-buffer
                        (insert-file-contents fixture-file)
                        (buffer-string)))
           (text (concat base-text "\n" base-text))
           (temp-db (make-temp-file "yomikun-test-" nil ".db"))
           (saved-status-file yk-db-status-file)
           (saved-db-status yk-db-status))
      (unwind-protect
          (progn
            ;; Set up temp DB
            (delete-file temp-db)
            (setq yk-db-status-file temp-db)
            (setq yk-db-status nil)
            (yk-db-status-create)
            (clrhash yk-status-table)

            (with-temp-buffer
              (insert text)
              (let ((buf-size (point-max)))
                ;; Process the buffer
                (yk-do-region (point-min) (point-max))

                ;; 1. Verify parsing covers the full buffer
                (let ((last-morph nil))
                  (let ((pos (1- buf-size)))
                    (while (and (> pos 0) (not last-morph))
                      (when (get-text-property pos 'yk-morph)
                        (setq last-morph pos))
                      (setq pos (1- pos))))
                  (expect last-morph :to-be-truthy)
                  ;; Last morph should be within 20 chars of end
                  (expect last-morph :to-be-greater-than (- buf-size 20)))

                ;; 2. Verify overlays exist and cover parsed regions
                (let ((overlay-count (length (overlays-in (point-min) (point-max)))))
                  (expect overlay-count :to-be-greater-than 100))

                ;; 3. Verify the 'seen' property matches the actual buffer text
                ;;    at every morph position (this catches byte-offset misalignment)
                (let ((mismatches 0)
                      (checked 0))
                  (let ((pos (point-min)))
                    (while pos
                      (when (get-text-property pos 'seen)
                        (let* ((seen (get-text-property pos 'seen))
                               (end-prop (get-text-property pos 'end))
                               (actual (buffer-substring-no-properties
                                        pos (min (+ pos (length seen)) buf-size))))
                          (setq checked (1+ checked))
                          (unless (string-equal seen actual)
                            (setq mismatches (1+ mismatches)))))
                      (setq pos (next-single-property-change pos 'begin))))
                  (expect checked :to-be-greater-than 100)
                  (expect mismatches :to-equal 0))

                ;; 4. Verify second half has morphs (tests chunking beyond 8KB)
                (let ((midpoint (/ buf-size 2))
                      (found-after-mid nil))
                  (let ((pos midpoint))
                    (while (and (< pos buf-size) (not found-after-mid))
                      (when (get-text-property pos 'yk-morph)
                        (setq found-after-mid pos))
                      (setq pos (1+ pos))))
                  (expect found-after-mid :to-be-truthy)))))

        ;; Cleanup
        (yk-db-status-close)
        (when (file-exists-p temp-db) (delete-file temp-db))
        (setq yk-db-status-file saved-status-file)
        (setq yk-db-status saved-db-status)
        (clrhash yk-status-table)))))

;;; test-yomikun-mecab.el ends here
