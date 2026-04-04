;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel M German (dmg@turingmachine.org)

;; Author: Daniel M German (dmg@turingmachine.org)
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://github.com/dmgerman/yomikun

;;; Commentary:

;; Mecab abstraction layer for yomikun.  Handles dictionary detection,
;; command construction with --node-format, output parsing, and
;; byte-offset to character-position conversion.
;;
;; The key design principle: each dictionary entry in the registry
;; specifies a --node-format string that produces a CANONICAL column
;; order, making the parser completely dictionary-independent.
;;
;; Canonical output format (tab-separated):
;;   surface \t pos \t lemma \t pronunciation \t written-form \t byte-start \t byte-end

;;; Code:

(require 'cl-lib)

;;; --- Configuration ---

(defvar yk-mecab-binary "mecab"
  "Path to the mecab executable.
Can be an absolute path or a command name found via PATH.")

(defvar yk-mecab-dict-dir nil
  "Path to mecab dictionary directory.
When nil, mecab uses its compiled-in default dictionary.")

(defvar yk-mecab-dict-type nil
  "Dictionary type symbol: `unidic', `ipadic', or nil for auto-detection.
When nil, `yk-mecab--detect-dict-type' is called to determine the type.")

;;; --- Dictionary Registry ---

(defvar yk-mecab-dictionary-registry
  '((unidic
     . ((name . "UniDic")
        (detect-pattern . "unidic\\|UniDic\\|UNIDIC")
        ;; UniDic fields: %f[0]=pos1, %f[7]=lemma, %f[9]=pronunciation, %f[10]=written-form
        (node-format . "%m\t%f[0]\t%f[7]\t%f[9]\t%f[10]\t%ps\t%pe\n")
        (unk-format  . "%m\t%f[0]\t%m\t\t%m\t%ps\t%pe\n")
        (eos-format  . "\n")))
    (ipadic
     . ((name . "IPAdic")
        (detect-pattern . "ipadic\\|IPAdic\\|IPADIC")
        ;; IPAdic fields: %f[0]=pos1, %f[6]=base-form, %f[7]=reading
        ;; IPAdic has no separate written-form, so we use %m (surface)
        (node-format . "%m\t%f[0]\t%f[6]\t%f[7]\t%m\t%ps\t%pe\n")
        (unk-format  . "%m\t%f[0]\t%m\t\t%m\t%ps\t%pe\n")
        (eos-format  . "\n"))))
  "Registry of mecab dictionary configurations.
Each entry maps a dictionary type symbol to an alist with:
  name          - human-readable name
  detect-pattern - regex matched against `mecab -D' output
  node-format   - --node-format string producing canonical columns
  unk-format    - --unk-format for unknown words
  eos-format    - --eos-format for end-of-sentence markers

Adding a new dictionary requires only adding an entry here.")

;;; --- Dictionary Detection ---

(defun yk-mecab--detect-dict-type ()
  "Detect which dictionary mecab is configured to use.
Runs `mecab -D' and matches the output against registry patterns.
Returns the dictionary type symbol (e.g., `unidic') or signals an error."
  (let ((output (yk-mecab--run-command
                 (append (list yk-mecab-binary "-D")
                         (when yk-mecab-dict-dir
                           (list "-d" yk-mecab-dict-dir))))))
    (or (cl-some
         (lambda (entry)
           (let ((pattern (cdr (assq 'detect-pattern (cdr entry)))))
             (when (string-match-p pattern output)
               (car entry))))
         yk-mecab-dictionary-registry)
        (error "Could not detect mecab dictionary type from:\n%s" output))))

(defun yk-mecab--get-dict-config ()
  "Return the dictionary configuration alist for the current setup.
Auto-detects dictionary type if `yk-mecab-dict-type' is nil."
  (let ((dict-type (or yk-mecab-dict-type
                       (yk-mecab--detect-dict-type))))
    (or (cdr (assq dict-type yk-mecab-dictionary-registry))
        (error "Unknown dictionary type: %s" dict-type))))

;;; --- Command Construction ---

(defun yk-mecab--build-args (input-file)
  "Build the argument list for mecab processing INPUT-FILE.
Returns a list of strings suitable for `call-process' or `make-process'."
  (let ((config (yk-mecab--get-dict-config)))
    (append
     (list (format "--node-format=%s" (cdr (assq 'node-format config)))
           (format "--unk-format=%s"  (cdr (assq 'unk-format config)))
           (format "--eos-format=%s"  (cdr (assq 'eos-format config)))
           "--bos-format=")
     (when yk-mecab-dict-dir
       (list "-d" yk-mecab-dict-dir))
     (list input-file))))

;;; --- Byte-Offset to Character-Position Conversion ---

(defun yk-mecab--build-byte-to-char-table (text)
  "Build a vector mapping byte offsets to character positions in TEXT.
Returns a vector V where (aref V byte-offset) gives the character position.
Only populated at character boundaries; intermediate byte positions are nil.
TEXT is assumed to be encoded as UTF-8 (mecab's default encoding)."
  (let* ((encoded (encode-coding-string text 'utf-8))
         (byte-len (length encoded))
         (table (make-vector (1+ byte-len) nil))
         (byte-pos 0)
         (char-pos 0))
    ;; Walk through the text character by character, recording
    ;; the byte offset where each character starts.
    (while (< char-pos (length text))
      (aset table byte-pos char-pos)
      (let ((char-byte-len (string-bytes
                            (substring text char-pos (1+ char-pos)))))
        (setq byte-pos (+ byte-pos char-byte-len)
              char-pos (1+ char-pos))))
    ;; Sentinel entry for end-of-text
    (aset table byte-pos char-pos)
    table))

;;; --- Output Parsing ---

(defun yk-mecab--parse-line (line byte-to-char region-offset)
  "Parse a single mecab output LINE into a token plist.
BYTE-TO-CHAR is the byte-to-character mapping vector.
REGION-OFFSET is the buffer position where the input text starts.
Returns a plist with keys: begin, end, seen, wtype, root, pronun, surface.
Returns nil for empty lines or lines that cannot be parsed."
  (when (and line (> (length line) 0))
    (let* ((fields (split-string line "\t"))
           (nfields (length fields)))
      (when (>= nfields 7)
        (let* ((seen        (nth 0 fields))
               (wtype       (nth 1 fields))
               (raw-root    (nth 2 fields))
               (pronun      (nth 3 fields))
               (surface     (nth 4 fields))
               (byte-start  (string-to-number (nth 5 fields)))
               (byte-end    (string-to-number (nth 6 fields)))
               (char-start  (aref byte-to-char byte-start))
               (char-end    (aref byte-to-char byte-end)))
          ;; Skip empty or unparseable entries
          (when (and char-start char-end (> (length seen) 0))
            (list 'seen    seen
                  'surface (if (and surface (> (length surface) 0)) surface seen)
                  'wtype   wtype
                  'root    (yk-mecab--process-root-for-kana
                            (if (and raw-root (> (length raw-root) 0)) raw-root seen))
                  'pronun  (or pronun "")
                  ;; begin is inclusive, end is inclusive (last char position)
                  'begin   (+ char-start region-offset)
                  'end     (+ char-end region-offset -1))))))))

(defun yk-mecab--process-root-for-kana (morph)
  "Strip romaji suffix from kana morphemes.
Mecab UniDic returns kana words with romaji equivalents, e.g.,
\"ドーム-dome\".  This function removes the romaji portion."
  (if (and morph (cl-position ?- morph))
      (let* ((pos (cl-position ?- morph))
             (romaji (substring morph (1+ pos))))
        (if (string-match-p "\\`[a-zA-Z]+\\'" romaji)
            (substring morph 0 pos)
          morph))
    morph))

(defun yk-mecab--parse-output (mecab-output input-text region-offset)
  "Parse MECAB-OUTPUT into a list of token plists.
INPUT-TEXT is the original string that was sent to mecab.
REGION-OFFSET is the buffer position where INPUT-TEXT starts.
Returns a list of token plists ready for `yk-process-tokens'."
  (let* ((byte-to-char (yk-mecab--build-byte-to-char-table input-text))
         (lines (split-string mecab-output "\n" t)))
    (delq nil
          (mapcar
           (lambda (line)
             (yk-mecab--parse-line line byte-to-char region-offset))
           lines))))

;;; --- Temp File Management ---

(defun yk-mecab--write-temp-file (text)
  "Write TEXT to a temporary file with UTF-8 encoding.
Returns the temp file path.  Caller is responsible for deletion."
  (let ((temp-file (make-temp-file "yomikun-"))
        (coding-system-for-write 'utf-8))
    (with-temp-file temp-file
      (insert text))
    temp-file))

;;; --- Process Helpers ---

(defun yk-mecab--run-command (args)
  "Run a mecab command with ARGS and return stdout as a string.
ARGS is a list where the first element is the program."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process (car args) nil t nil (cdr args))))
      (unless (= exit-code 0)
        (error "mecab command failed (exit %d): %s\nOutput: %s"
               exit-code (mapconcat #'identity args " ") (buffer-string)))
      (buffer-string))))

;;; --- Diagnostics ---

(defun yk-doctor ()
  "Run diagnostic checks on the yomikun/mecab configuration.
Opens a *yomikun-doctor* buffer with results."
  (interactive)
  (let ((buf (get-buffer-create "*yomikun-doctor*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Yomikun Doctor ===\n\n")
        (yk-doctor--check-binary)
        (insert "\n")
        (yk-doctor--check-dictionary)
        (insert "\n")
        (yk-doctor--test-tokenize)
        (insert "\n")
        (yk-doctor--check-databases)
        (insert "\n--- Done ---\n")
        (goto-char (point-min))))
    (display-buffer buf)))

(defun yk-doctor--check-binary ()
  "Check if the mecab binary exists and is executable."
  (insert "1. Mecab Binary\n")
  (let ((found (executable-find yk-mecab-binary)))
    (if found
        (progn
          (insert (format "   OK: Found at %s\n" found))
          (condition-case err
              (let ((version (string-trim
                              (yk-mecab--run-command (list found "--version")))))
                (insert (format "   Version: %s\n" version)))
            (error (insert (format "   WARNING: Could not get version: %s\n"
                                   (error-message-string err))))))
      (insert (format "   FAIL: '%s' not found in PATH\n" yk-mecab-binary)))))

(defun yk-doctor--check-dictionary ()
  "Check dictionary detection."
  (insert "2. Dictionary\n")
  (when yk-mecab-dict-dir
    (insert (format "   Dict dir: %s\n" yk-mecab-dict-dir))
    (unless (file-directory-p yk-mecab-dict-dir)
      (insert "   WARNING: Directory does not exist\n")))
  (condition-case err
      (let ((dict-type (yk-mecab--detect-dict-type)))
        (let ((config (cdr (assq dict-type yk-mecab-dictionary-registry))))
          (insert (format "   OK: Detected %s (%s)\n"
                          dict-type (cdr (assq 'name config))))))
    (error (insert (format "   FAIL: %s\n" (error-message-string err))))))

(defun yk-doctor--test-tokenize ()
  "Run a test tokenization to verify the full pipeline."
  (insert "3. Tokenization Test\n")
  (let ((test-input "東京都に住んでいる"))
    (condition-case err
        (let* ((temp-file (yk-mecab--write-temp-file test-input))
               (args (yk-mecab--build-args temp-file))
               (output (unwind-protect
                           (yk-mecab--run-command (cons yk-mecab-binary args))
                         (delete-file temp-file)))
               (tokens (yk-mecab--parse-output output test-input 1)))
          (insert (format "   Input: %s\n" test-input))
          (insert (format "   OK: Parsed %d tokens\n" (length tokens)))
          (dolist (tok (cl-subseq tokens 0 (min 5 (length tokens))))
            (insert (format "     [%d-%d] %s (%s) root=%s\n"
                            (plist-get tok 'begin)
                            (plist-get tok 'end)
                            (plist-get tok 'seen)
                            (plist-get tok 'wtype)
                            (plist-get tok 'root)))))
      (error (insert (format "   FAIL: %s\n" (error-message-string err)))))))

(defun yk-doctor--check-databases ()
  "Check database file accessibility."
  (insert "4. Databases\n")
  (dolist (db-spec (list (cons "Status DB" (bound-and-true-p yk-db-status-file))
                         (cons "Dict DB"   (bound-and-true-p yk-db-dict-file))))
    (let ((label (car db-spec))
          (path  (cdr db-spec)))
      (cond
       ((not path)
        (insert (format "   %s: Not configured\n" label)))
       ((file-exists-p path)
        (insert (format "   %s: OK (%s)\n" label path)))
       (t
        (insert (format "   %s: MISSING (%s)\n" label path)))))))

(provide 'yomikun-mecab)
;;; yomikun-mecab.el ends here
