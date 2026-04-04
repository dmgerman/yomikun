;;; -*- lexical-binding: t; -*-
;;; test-helper.el --- Test helper for yomikun tests

;;; Commentary:
;; Setup file loaded before all buttercup test files.

;;; Code:

;; Add the parent directory (yomikun module root) to load-path
(let ((yomikun-dir (file-name-directory
                    (directory-file-name
                     (file-name-directory
                      (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path yomikun-dir))

;; Add straight.el build directories for dependencies (batch mode only)
(when noninteractive
  (let ((straight-build-dir
         (cl-some
          (lambda (arch)
            (let ((dir (expand-file-name
                        (format "%s/%s/straight/build" arch emacs-version)
                        user-emacs-directory)))
              (when (file-directory-p dir) dir)))
          ;; Try multiple arch names since system-configuration may differ
          (list (car (split-string system-configuration "-"))
                "arm64" "aarch64" "x86_64"))))
    (when straight-build-dir
      (dolist (dir (directory-files straight-build-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))))

(require 'yomikun-mecab)
(require 'yomikun-db)
(require 'yomikun-dict)
(require 'yomikun)

;;; --- Test Utilities ---

(defun yk-test-mecab-available-p ()
  "Return non-nil if mecab is available for integration tests."
  (executable-find yk-mecab-binary))

(defun yk-test-make-temp-db ()
  "Create a temporary status database for testing.
Returns the file path.  Caller must delete it."
  (let* ((temp-file (make-temp-file "yomikun-test-" nil ".db"))
         (yk-db-status-file temp-file)
         (yk-db-status nil))
    (yk-db-status-create)
    (yk-db-status-close)
    temp-file))

;;; --- Sample Mecab Output ---
;; These represent the canonical --node-format output:
;; surface \t pos \t lemma \t pronunciation \t written-form \t byte-start \t byte-end

(defvar yk-test-mecab-output-tokyo
  "東京\t名詞\t東京\tトウキョウ\t東京\t0\t6
都\t名詞\t都\tト\t都\t6\t9
に\t助詞\tに\tニ\tに\t9\t12
住ん\t動詞\t住む\tスン\t住ん\t12\t18
で\t助詞\tで\tデ\tで\t18\t21
いる\t動詞\tいる\tイル\tいる\t21\t27
"
  "Sample mecab output for \"東京都に住んでいる\".")

(defvar yk-test-input-tokyo "東京都に住んでいる"
  "Test input string for Tokyo sentence.")

(defvar yk-test-mecab-output-mixed
  "I\t名詞\tI\t\tI\t0\t1
am\t名詞\tam\t\tam\t2\t4
猫\t名詞\t猫\tネコ\t猫\t5\t8
"
  "Sample mecab output for mixed ASCII/Japanese text \"I am 猫\".")

(defvar yk-test-input-mixed "I am 猫"
  "Test input for mixed ASCII/Japanese.")

;;; test-helper.el ends here
