;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Daniel M German (dmg@turingmachine.org)

;; Author: Daniel M German (dmg@turingmachine.org)
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://github.com/dmgerman/yomikun

;;; Commentary:

;; Database layer for yomikun.  Manages the status database (tracks
;; known/unknown/learning/ignored words) and the dictionary database
;; (quick definition lookups and compound detection).  All queries are
;; wrapped with memoization caches for interactive performance.

;;; Code:

(require 'emacsql)
(require 'emacsql-sqlite)

;;; --- Configuration ---

(defcustom yk-db-status-file "~/yk-status.db"
  "Path to the SQLite status database file."
  :type '(choice (const :tag "Not configured" nil)
                 (file :tag "Database file"))
  :group 'yomikun)

(defcustom yk-db-dict-file nil
  "Path to the SQLite dictionary database file."
  :type '(choice (const :tag "Not configured" nil)
                 (file :tag "Database file"))
  :group 'yomikun)

(defvar yk-date-format "%Y-%m-%d %H:%M"
  "Format to use when logging timestamps to the status database.")

;;; --- Connection State ---

(defvar yk-db-status nil
  "Active emacsql connection to the status database.")

(defvar yk-db-dict nil
  "Active emacsql connection to the dictionary database.")

;;; --- Memoization Caches ---

(defvar yk-status-table (make-hash-table :test 'equal)
  "Memoization cache for morph status lookups.")

(defvar yk-dict-table (make-hash-table :test 'equal)
  "Memoization cache for dictionary lookups.")

(defvar yk-compound-candidates-table (make-hash-table :test 'equal)
  "Memoization cache for compound query results.")

;;; --- Connection Management ---

(defun yk-db-status-open ()
  "Open the status database, creating a connection if needed."
  (interactive)
  (unless yk-db-status-file
    (error "Yomikun: `yk-db-status-file' is not configured"))
  (unless (file-exists-p yk-db-status-file)
    (signal 'file-error (list "Status database does not exist" yk-db-status-file)))
  (unless yk-db-status
    (setq yk-db-status (emacsql-sqlite-open yk-db-status-file))))

(defun yk-db-dict-open ()
  "Open the dictionary database, creating a connection if needed."
  (interactive)
  (unless yk-db-dict-file
    (error "Yomikun: `yk-db-dict-file' is not configured"))
  (unless (file-exists-p yk-db-dict-file)
    (signal 'file-error (list "Dictionary database does not exist" yk-db-dict-file)))
  (unless yk-db-dict
    (setq yk-db-dict (emacsql-sqlite-open yk-db-dict-file))))

(defun yk-db-status-close ()
  "Close the status database connection."
  (when yk-db-status
    (emacsql-close yk-db-status)
    (setq yk-db-status nil)))

(defun yk-db-dict-close ()
  "Close the dictionary database connection."
  (when yk-db-dict
    (emacsql-close yk-db-dict)
    (setq yk-db-dict nil)))

(defun yk-db-status-create ()
  "Create a new empty status database at `yk-db-status-file'."
  (interactive)
  (when (file-exists-p yk-db-status-file)
    (signal 'file-error (list "Status database already exists" yk-db-status-file)))
  (setq yk-db-status (emacsql-sqlite-open yk-db-status-file))
  (emacsql yk-db-status [:create-table words
                          ([morph mtype surface status date]
                           (:primary-key [morph mtype surface]))]))

;;; --- Status Database: Low-Level ---

(defun yk-db--ensure-status-open ()
  "Ensure the status database connection is open, signaling if not."
  (unless yk-db-status
    (error "Yomikun: status database is not open.  Call `yk-db-status-open' first")))

(defun yk-db--ensure-dict-open ()
  "Ensure the dictionary database connection is open, signaling if not."
  (unless yk-db-dict
    (error "Yomikun: dictionary database is not open.  Call `yk-db-dict-open' first")))

(defun yk-get-time-date ()
  "Return the current time formatted per `yk-date-format'."
  (format-time-string yk-date-format (current-time)))

(defun yk-db-morph-status-get (root wtype surface)
  "Return the status record matching ROOT, WTYPE, SURFACE.
Returns a list (morph mtype status date) or a default unknown record."
  (yk-db--ensure-status-open)
  (or (nth 0
       (emacsql yk-db-status [:select [morph mtype status date]
                                      :from words
                                      :where (and (= morph $s1) (= mtype $s2) (= surface $s3))]
                root wtype surface))
      (list nil nil "unknown" nil)))

(defun yk-db-morph-status-delete (root wtype surface)
  "Delete the morph record matching ROOT, WTYPE, SURFACE."
  (yk-db--ensure-status-open)
  (emacsql yk-db-status [:delete
                          :from words
                          :where (and (= morph $s1) (= mtype $s2) (= surface $s3))]
           root wtype surface))

(defun yk-db-morph-status-insert (root wtype surface status)
  "Insert a new morph record with ROOT, WTYPE, SURFACE, STATUS."
  (yk-db--ensure-status-open)
  (let ((today (yk-get-time-date)))
    (emacsql yk-db-status [:insert :into words
                            :values ([$s1 $s2 $s3 $s4 $s5])]
             root wtype surface status today)))

(defun yk-db-morph-status-update (root wtype surface status)
  "Update morph status by deleting and re-inserting.
If STATUS is \"unknown\", the record is simply deleted (default state)."
  (yk-db-morph-status-delete root wtype surface)
  (unless (string-equal status "unknown")
    (yk-db-morph-status-insert root wtype surface status)))

;;; --- Status Database: Memoized ---

(defun yk-morph-status-get (root wtype surface)
  "Return the status string for the morph (ROOT WTYPE SURFACE).
Results are memoized in `yk-status-table'."
  (or (gethash (list root wtype surface) yk-status-table)
      (let ((status (nth 2 (yk-db-morph-status-get root wtype surface))))
        (puthash (list root wtype surface) status yk-status-table))))

(defun yk-morph-status-set (root wtype surface status)
  "Set STATUS for morph (ROOT WTYPE SURFACE) in DB and cache.
Only writes to the database if the status actually changed."
  (let ((curval (yk-morph-status-get root wtype surface)))
    (unless (and curval (string-equal curval status))
      (yk-db-morph-status-update root wtype surface status)
      (puthash (list root wtype surface) status yk-status-table))))

;;; --- Dictionary Database ---

(defun yk-db-dict-def (root pronun wtype)
  "Look up a dictionary definition for ROOT, PRONUN, WTYPE.
Tries progressively broader searches: all three fields, then root+pronun,
then root alone."
  (yk-db--ensure-dict-open)
  (let* ((with-all (nth 0
                    (emacsql yk-db-dict [:select [gloss pos root reading wtype]
                                                 :from entries
                                                 :where (and (= root $s1) (= reading $s2)
                                                             (= wtype $s3))
                                                 :limit 1]
                             root pronun wtype)))
         (without-type (or with-all
                           (nth 0
                            (emacsql yk-db-dict [:select [gloss pos root reading wtype]
                                                         :from entries
                                                         :where (and (= root $s1) (= reading $s2))
                                                         :limit 1]
                                     root pronun))))
         (result (or without-type
                     (nth 0
                      (emacsql yk-db-dict [:select [gloss pos root reading wtype]
                                                    :from entries
                                                    :where (= root $s1)
                                                    :limit 1]
                               root)))))
    result))

(defun yk-dict-def (root pronun wtype)
  "Memoized dictionary lookup for ROOT, PRONUN, WTYPE."
  (or (gethash (list root pronun wtype) yk-dict-table)
      (let ((def (yk-db-dict-def root pronun wtype)))
        (puthash (list root pronun wtype) def yk-dict-table))))

;;; --- Compound Detection ---

(defun yk-db-compound-prefix-candidates (st)
  "Return a list of compounds that have ST as a prefix."
  (yk-db--ensure-dict-open)
  (when (> (length st) 2)
    (let ((pattern (concat st "%")))
      (mapcar
       (lambda (ls) (nth 0 ls))
       (emacsql yk-db-dict [:select [compound]
                                    :from compounds
                                    :where (like compound $s1)
                                    :order-by [(desc (length compound))]]
                pattern)))))

(defun yk-compound-prefix-candidates (st)
  "Memoized wrapper around `yk-db-compound-prefix-candidates'."
  (or (gethash st yk-compound-candidates-table)
      (let ((comps (yk-db-compound-prefix-candidates st)))
        (puthash st comps yk-compound-candidates-table))))

(defun yk-db-compound-exists (st)
  "Return non-nil if compound ST exists in the dictionary."
  (yk-db--ensure-dict-open)
  (and
   (emacsql yk-db-dict [:select [compound]
                                :from compounds
                                :where (= compound $s1)]
            st)
   t))

(provide 'yomikun-db)
;;; yomikun-db.el ends here
