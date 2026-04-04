;;; -*- lexical-binding: t; -*-
;;; test-yomikun-dict.el --- Tests for yomikun-dict module

(require 'buttercup)

;;; --- yk-pad-first-line ---

(describe "yk-pad-first-line"
  (it "pads short first line to match longest"
    (let ((result (yk-pad-first-line "hi\nlong line here")))
      ;; First line "hi" (width 2) padded to match "long line here" (width 14)
      (expect (string-width (car (split-string result "\n")))
              :to-be-greater-than 2)))

  (it "returns single-line string unchanged"
    (expect (yk-pad-first-line "hello") :to-equal "hello"))

  (it "handles CJK characters with double width"
    (let ((result (yk-pad-first-line "ab\n東京都")))
      ;; "ab" has width 2, "東京都" has width 6
      ;; First line should be padded to width 6
      (let ((first-line (car (split-string result "\n"))))
        (expect (string-width first-line) :to-equal 6))))

  (it "does not pad when first line is already longest"
    (let ((result (yk-pad-first-line "long first line\nhi")))
      (expect (car (split-string result "\n"))
              :to-equal "long first line"))))

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

  (it "returns error message for non-zero exit code"
    (let ((result (yk-run-external-command '("false") "anything")))
      (expect result :to-match "Command failed"))))

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

;;; test-yomikun-dict.el ends here
