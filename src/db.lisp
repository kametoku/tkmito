(in-package :cl-user)
(defpackage tkmito.db
  (:use :cl)
  (:export #:*config*
           #:sql-logger
           #:connect
           #:disconnect
           #:ensure-connection
           #:with-transaction
           #:rollback
           #:execute
           #:fetch
           #:fetch-all))
(in-package :tkmito.db)

(defvar *config* nil)

(defun sql-logger (sql params row-count took-ms prev-stack)
  (let* ((mito:*mito-logger-stream* t)
         (sql-log (with-output-to-string (*standard-output*)
                    (mito.logger:mito-sql-logger
                     sql params row-count took-ms prev-stack))))
    (log:debug "[SQL] ~A" sql-log)))

(defun connect (&optional (config *config*))
  (apply #'mito:connect-toplevel config))

(defun disconnect ()
  (mito:disconnect-toplevel))

(defun connected-p ()
  (let ((conn (and (boundp 'mito:*connection*)
                   mito:*connection*)))
    (and conn (tkutil:ignore-errors-log (dbi:ping conn)) conn)))

(defun ensure-connection ()
  (or (connected-p)
      (connect)))

(defmacro with-transaction (&body body)
  `(dbi:with-transaction (ensure-connection)
     (progn ,@body)))

(defun rollback ()
  (dbi:rollback (connected-p)))

(defgeneric execute (sql &optional params)
  (:method ((sql string) &optional params)
    (let ((query (dbi:prepare mito:*connection* sql)))
      (dbi:execute query params)))
  (:method ((sql sxql.statement:select-statement) &optional params)
    (declare (ignore params))
    (apply #'execute (multiple-value-list (sxql:yield sql)))))

(defun fetch (result)
  (dbi:fetch result))

(defun fetch-all (result)
  (dbi:fetch-all result))

