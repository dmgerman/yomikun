;;; -*- lexical-binding: t; -*-
;;; test-yomikun-dict.el --- Tests for yomikun-dict module

(require 'buttercup)

;;; --- yk-pad-first-line ---

(describe "yk-pad-first-line"
  (it "pads first line to width of longest line"
    (let* ((result (yk-pad-first-line "hi\nlong line here"))
           (first-line (car (split-string result "\n"))))
      (expect (string-width first-line)
              :to-equal (string-width "long line here"))))

  (it "returns single-line string unchanged"
    (expect (yk-pad-first-line "hello") :to-equal "hello"))

  (it "appends two trailing blank lines"
    (let ((result (yk-pad-first-line "line1\nline2")))
      (expect (string-suffix-p "\n \n " result) :to-be-truthy)))

  (it "trims trailing whitespace from input"
    (let* ((result (yk-pad-first-line "line1\nline2\n\n"))
           (lines (split-string result "\n")))
      (expect (nth 0 lines) :to-match "^line1")
      (expect (nth 1 lines) :to-equal "line2"))))

;;; --- yk-run-external-command ---

(describe "yk-run-external-command"
  (it "returns 'no term given' for empty term"
    (expect (yk-run-external-command '("echo") "")
            :to-equal "no term given"))

  (it "returns 'no term given' for nil term"
    (expect (yk-run-external-command '("echo") nil)
            :to-equal "no term given"))

  (it "runs command and returns output"
    (let ((result (yk-run-external-command '("echo") "hello")))
      (expect (string-trim result) :to-equal "hello")))

  (it "passes all arguments correctly"
    (let ((result (yk-run-external-command '("echo" "-n") "hello")))
      (expect result :to-equal "hello")))

  (it "returns no-results message for non-zero exit with no output"
    (let ((result (yk-run-external-command '("false") "anything")))
      (expect result :to-match "No results"))))

;;; --- yk-extract-word-at-point ---

(describe "yk-extract-word-at-point"
  (it "extracts ASCII word at point"
    (with-temp-buffer
      (insert "hello world")
      (goto-char 3) ;; inside "hello"
      (expect (yk-extract-word-at-point) :to-equal "hello")))

  (it "returns nil when not on a word"
    (with-temp-buffer
      (insert "  ")
      (goto-char 1)
      (expect (yk-extract-word-at-point) :to-be nil))))

;;; --- yk-run-external-command: type validation ---

(describe "yk-run-external-command type validation"
  (it "errors with helpful message when command is a string"
    (expect (yk-run-external-command "myougiden --human '%s'" "猫")
            :to-throw 'error))

  (it "works with list command"
    (let ((result (yk-run-external-command '("echo") "hello")))
      (expect (string-trim result) :to-equal "hello"))))

;;; --- yk-show-definition ---

(describe "yk-show-definition"
  (before-each
    (spy-on 'yk-tip-show))

  (it "shows tooltip and logs to tango buffer"
    (yk-show-definition "猫" "cat, feline")
    (expect 'yk-tip-show :to-have-been-called-with "cat, feline")
    (expect (get-buffer yk-tango-buffer-name) :to-be-truthy)
    (with-current-buffer yk-tango-buffer-name
      (expect (buffer-string) :to-match "猫")
      (expect (buffer-string) :to-match "cat, feline")))

  (after-each
    (when (get-buffer yk-tango-buffer-name)
      (kill-buffer yk-tango-buffer-name))))

;;; --- yk-tip-show error handling ---

(describe "yk-tip-show"
  (it "does not signal when pos-tip-show errors"
    (spy-on 'pos-tip-show :and-throw-error 'error)
    ;; Should not throw — catches the error and falls back to message
    (expect (yk-tip-show "test message") :not :to-throw)))

;;; --- yk-define-at-point ---

(describe "yk-define-at-point"
  :var (temp-db saved-status-file saved-db-status)

  (before-each
    (setq saved-status-file yk-db-status-file)
    (setq saved-db-status yk-db-status)
    (setq temp-db (make-temp-file "yomikun-test-" nil ".db"))
    (delete-file temp-db)
    (setq yk-db-status-file temp-db)
    (setq yk-db-status nil)
    (yk-db-status-create)
    (clrhash yk-status-table)
    (spy-on 'yk-tip-show)
    (spy-on 'yk-run-dictionary :and-return-value "cat, feline"))

  (after-each
    (yk-db-status-close)
    (when (file-exists-p temp-db) (delete-file temp-db))
    (setq yk-db-status-file saved-status-file)
    (setq yk-db-status saved-db-status)
    (clrhash yk-status-table)
    (when (get-buffer yk-tango-buffer-name)
      (kill-buffer yk-tango-buffer-name)))

  (it "looks up the root of the morph at point"
    (with-temp-buffer
      (insert "猫が好き")
      (with-silent-modifications
        (put-text-property 1 2 'root "猫")
        (put-text-property 1 2 'yk-morph t))
      (goto-char 1)
      (yk-define-at-point)
      (expect 'yk-run-dictionary :to-have-been-called-with "猫")))

  (it "shows definition in tooltip"
    (with-temp-buffer
      (insert "猫が好き")
      (with-silent-modifications
        (put-text-property 1 2 'root "猫")
        (put-text-property 1 2 'yk-morph t))
      (goto-char 1)
      (yk-define-at-point)
      (expect 'yk-tip-show :to-have-been-called))))

;;; --- yk-extract-term-at-point ---

(describe "yk-extract-term-at-point"
  (it "extracts root from morph at point"
    (with-temp-buffer
      (insert "猫が好き")
      (with-silent-modifications
        (put-text-property 1 2 'root "猫")
        (put-text-property 1 2 'yk-morph t))
      (goto-char 1)
      (expect (yk-extract-term-at-point) :to-equal "猫")))

  (it "prefers compound over root"
    (with-temp-buffer
      (insert "東京都")
      (with-silent-modifications
        (put-text-property 1 2 'root "東")
        (put-text-property 1 2 'compound '("東京都"))
        (put-text-property 1 2 'yk-morph t))
      (goto-char 1)
      (expect (yk-extract-term-at-point) :to-equal "東京都"))))

;;; test-yomikun-dict.el ends here
