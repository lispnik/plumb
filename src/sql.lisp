;;;; sql.lisp -- rows in and rows out, on cl-dbi.
;;;;
;;;; The third tabular source next to plumb/json and plumb/csv, and the only one
;;;; that can filter, join and order before anything crosses a channel.  cl-dbi
;;;; is the portable layer, so this is SQLite, PostgreSQL and MySQL rather than
;;;; one engine.
;;;;
;;;; Loaded by the PLUMB/SQL system, defining into the PLUMB package and
;;;; exporting at load time, exactly as src/csv.lisp and src/json.lisp do.
;;;;
;;;; Two things about cl-dbi shape this file.
;;;;
;;;; FETCH already returns a plist, which is plumb's object shape -- but with
;;;; case-preserving lowercase keys: (:|id| 1 :|Name| "alpha").  FIELD matches
;;;; keywords with EQ against an upcased name, so `.name` would silently miss
;;;; every column.  The keys are re-interned here by the same rule JSON-KEY and
;;;; CSV-KEY use.  Same lesson as jzon's hash tables: a library's natural shape
;;;; is not plumb's, and bridging it is this file's job rather than the
;;;; caller's.
;;;;
;;;; FETCH is row-at-a-time and returns NIL at the end, so FROM-SQL STREAMS.  It
;;;; is the first of these readers that is not a barrier: `from-sql "..." |
;;;; take 5` stops the query part-way, the way `commits | take 5` stops git log.

(in-package #:plumb)

(defun sql-key (name)
  "A column name as the keyword FIELD will match -- upcased, because FIELD
compares field names case-insensitively everywhere else, so `.name` has to
reach a column spelled name, Name or NAME."
  (intern (string-upcase (string name)) :keyword))

(defun sql-row (plist)
  "One cl-dbi row with its keys re-interned."
  (loop for (key value) on plist by #'cddr
        append (list (sql-key key) value)))

(defun sql-connect (driver database connect)
  (apply #'dbi:connect driver
         (append (when database (list :database-name database)) connect)))

;;; ------------------------------------------------------------------ writing

(defun sql-value (value)
  "VALUE as something a driver will bind.

Driven by nothing but the value's type, and needed for the same reason JSONABLE
is: FIELDS and FIELD make every plumb object serialisable, so a FILE-ENTRY or a
COMMIT reaches here carrying keywords, pathnames and conditions that no driver
knows what to do with."
  (typecase value
    ((or null number string) value)
    ((member t) 1)                      ; SQL has no boolean everywhere; 1/NULL
    (keyword (string-downcase (symbol-name value)))
    (symbol (string-downcase (symbol-name value)))
    (pathname (sb-ext:native-namestring value))
    (condition (princ-to-string value))
    (t (present value))))

(defun sql-column-type (rows column)
  "INTEGER, REAL or TEXT, from what the column actually holds.

Only used by :CREATE.  A column of nothing but integers is INTEGER, one that
mixes integers and reals is REAL, and anything else is TEXT -- which is also
where an all-NULL column lands, since there is nothing to infer from."
  (let ((seen '()))
    (dolist (row rows)
      (let ((v (sql-value (table-value row column))))
        (cond ((null v))
              ((integerp v) (pushnew :integer seen))
              ((realp v) (pushnew :real seen))
              (t (pushnew :text seen)))))
    (cond ((member :text seen) "TEXT")
          ((member :real seen) "REAL")
          ((member :integer seen) "INTEGER")
          (t "TEXT"))))

(defun sql-identifier (name)
  "A column or table name, double-quoted.

Standard SQL, and what SQLite and PostgreSQL want.  MySQL wants backticks
unless it is in ANSI_QUOTES mode -- so a MySQL user with a column named `order`
needs that mode, which is worth knowing and not worth a dialect table here."
  (format nil "\"~a\"" (substitute #\Space #\" (string-downcase (string name)))))

;;; ------------------------------------------------------------------- stages

(defstage from-sql ((query string) &key (driver :sqlite3) database connect params)
  "Run QUERY and emit a row per result.

  from-sql \"select name, size from files where size > ?\" :params (list 1024)
    | where {(search \".lisp\" .name)} | sort-by .size :desc | table

  from-sql \"select * from users\" :driver :postgres
    :connect (list :username \"me\" :host \"db.internal\")

Rows arrive as plists with upcased keyword keys, so `.name` reaches a column
spelled name, Name or NAME, and TABLE finds its columns.  Values keep the types
the driver gives them: integers, reals, strings, NIL for NULL, and byte vectors
for BLOBs.

STREAMS, unlike FROM-JSON and FROM-CSV -- cl-dbi fetches a row at a time, so
`from-sql \"select * from big\" | take 5` stops the query rather than reading
the table first.  The connection closes on every exit path, including that one.

:PARAMS binds the ? placeholders.  That is not a convenience: interpolating a
value into the query string is how injection happens, and a value containing a
quote breaks the query even when nobody is being hostile.

:DRIVER is :sqlite3 unless you say otherwise, :DATABASE is the file or database
name, and :CONNECT is any further plist handed straight to DBI:CONNECT -- which
is where a host, username and password go."
  (:consumes nil) (:produces :objects)
  (let ((connection (sql-connect driver database connect)))
    (unwind-protect
         (let ((result (dbi:execute (dbi:prepare connection query) params)))
           (loop for row = (dbi:fetch result)
                 while row
                 do (emit (sql-row row))))
      ;; Runs on EOF, on (finish), and on the CHANNEL-CLOSED a downstream TAKE
      ;; causes -- which is the whole reason a query can be abandoned early.
      (ignore-errors (dbi:disconnect connection)))))

(defstage to-sql ((table string) &key (driver :sqlite3) database connect create)
  "Insert every object into TABLE, one row each.

  ls \"src/*.lisp\" | to-sql \"files\" :database \"/tmp/x.db\" :create
  ps | where {(> .rss 500mb)} | to-sql \"heavy\" :database \"/tmp/x.db\"

Columns are the union of FIELDS across every row, in first-seen order -- the
same rule TABLE and TO-CSV use, which is why this works on a FILE-ENTRY, a
PROCESS or a type you add later without any of them knowing about SQL.  A row
missing a column gets NULL rather than a shifted insert.

A barrier for that reason: the column list is not known until the last row has
arrived.

WITHOUT :CREATE the table must already exist, and a missing one is an error
naming it.  That is deliberate -- a mistyped table name should not silently
become a second table that accepts the insert.  With :CREATE the table is made
if absent, with each column typed from what it actually holds: INTEGER, REAL,
or TEXT for everything else.

Everything is inserted in ONE transaction.  On SQLite per-row autocommit is
about a disk sync per row, so this is the difference between usable and not."
  (:consumes :objects) (:produces nil) (:barrier t)
  (let ((rows (make-array 16 :adjustable t :fill-pointer 0)))
    (do-input (x) (vector-push-extend x rows))
    (let* ((rows (coerce rows 'list))
           (columns (table-columns rows)))
      (when rows
        (let ((connection (sql-connect driver database connect)))
          (unwind-protect
               (progn
                 (when create
                   (dbi:do-sql
                    connection
                    (format nil "CREATE TABLE IF NOT EXISTS ~a (~{~a~^, ~})"
                            (sql-identifier table)
                            (mapcar (lambda (c)
                                      (format nil "~a ~a" (sql-identifier c)
                                              (sql-column-type rows c)))
                                    columns))))
                 (let ((sql (format nil "INSERT INTO ~a (~{~a~^, ~}) VALUES (~{~a~^, ~})"
                                    (sql-identifier table)
                                    (mapcar #'sql-identifier columns)
                                    (make-list (length columns)
                                               :initial-element "?"))))
                   (dbi:with-transaction connection
                     (let ((statement (dbi:prepare connection sql)))
                       (dolist (row rows)
                         (dbi:execute statement
                                      (mapcar (lambda (c)
                                                (sql-value (table-value row c)))
                                              columns)))))))
            (ignore-errors (dbi:disconnect connection))))))))

;;; Exported here rather than in package.lisp: these symbols only name anything
;;; once this system is loaded, and HELP lists exported symbols that are FBOUND.

(export '(from-sql to-sql sql-value) '#:plumb)
