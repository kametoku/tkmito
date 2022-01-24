(in-package :cl-user)
(defpackage tkmito.search
  (:use :cl)
  (:export #:columns
           #:query-columns
           #:query
           #:where
           #:where-date
           #:order-by
           #:includes
           #:select
           #:with-paginate
           #:id-column
           #:sortable-columns
           #:date-columns
           #:with-search-parameters))
(in-package :tkmito.search)

(defun table-name (class)
  (mito.class:table-name (find-class class)))

(defun %table-column-name (table-name column-name)
  (let ((column (format nil "~A.~A" table-name column-name)))
    (tkutil:string-to-keyword column)))

(defun table-column-name (table-name table-column)
  (let ((type (mito.class:table-column-type table-column)))
    (when (or (keywordp type)
              (consp type))
      (%table-column-name table-name
                          (mito.class:table-column-name table-column)))))
  
(defun columns (class)
  (let* ((class (find-class class))
         (table-name (mito.class:table-name class)))
    (remove nil (mapcar (lambda (table-column)
                          (table-column-name table-name table-column))
                        (mito.class:table-column-slots class)))))

(defgeneric query-columns (class)
  (:documentation "Returns list of columns in keyword that can be
searched for.")
  (:method ((class t))
    (columns class)))

(defun query (query &rest classes)
  (when (and (not (tkutil:blankp query))
             classes)
    (let* ((query (concatenate 'string "%" query "%"))
           (columns (loop for class in classes
                          append (query-columns class))))
      (sxql:where `(:or ,@(mapcar #'(lambda (column)
                                      (list :like column query))
                                  columns))))))

(defun expression (column value &key op)
  (cond ((not value) nil)
        (op (list op column value))
        ((listp value)
         (list :in column value))
        (t (list := column value))))

(defun where (column value &rest args &key op)
  (declare (ignorable op))
  (let ((expression (apply #'expression column value args)))
    (when expression
      (sxql:where expression))))

(defgeneric midnight (date)
  (:method ((date local-time:timestamp))
    (local-time:encode-timestamp 0 0 0 0
                                 (local-time:timestamp-day date)
                                 (local-time:timestamp-month date)
                                 (local-time:timestamp-year date)))
  (:method ((date string))
    (midnight (local-time:parse-timestring date))))

(defun next-day (date &key (offset 1))
  (local-time:adjust-timestamp (midnight date) (offset :day offset)))

(defun where-date (date-column start-date end-date)
  (when (tkutil:blankp date-column)
    (return-from where-date))
  (let ((start (unless (tkutil:blankp start-date)
                 (midnight start-date)))
        (end (unless (tkutil:blankp end-date)
               (next-day (midnight end-date)))))
    (cond ((and start end)
           (sxql:where (:and (:>= date-column start)
                             (:< date-column end))))
          (start
           (sxql:where (:>= date-column start)))
          (end
           (sxql:where (:< date-column end))))))

(defun order-by (sort-column &optional sort-direction)
  (when sort-column
    (unless sort-direction (setf sort-direction :asc))
    (sxql:order-by (list sort-direction sort-column))))

(defun includes (&rest classes)
;;   (cons :includes (mapcar #'mito.util:ensure-class classes)))
  (cons :includes classes))

(defun all-fields (object-type &rest additonal-fields)
  (let* ((table-name (table-name object-type))
         (fields `((:distinct ,(%table-column-name table-name "*")))))
    ;; fields: ((:distinct :{table-name}.*))
    (nconc fields additonal-fields)))

(defun select (class &optional fields &rest clauses)
  (let ((sql (sxql:select (or fields (all-fields class))
               (sxql:from (sxql:make-sql-symbol (table-name class)))))
        (include-classes '()))
;;     (setf clauses (append (narrow-user class) clauses))
    (loop for clause in clauses
          if (and (consp clause) (eql (car clause) :includes))
            do (setf include-classes (append include-classes (cdr clause)))
          else
            if clause
              do (sxql:add-child sql clause))
    (let ((results (mito.dao:select-by-sql class sql)))
      (dolist (foreign-class include-classes)
        (if (consp foreign-class)
            (mito:include-foreign-objects
             (mito.util:ensure-class (car foreign-class))
             (mapcar (cadr foreign-class) results))
            (mito:include-foreign-objects
             (mito.util:ensure-class foreign-class) results)))
      (values results sql))))

(defun count-all (class sql)
  "Count the number of records witout offset and limit restrictins from sql."
  (let* ((column (if (keywordp class)
                     class
                     (tkutil:string-to-keyword
                      (concatenate 'string (table-name class) ".id"))))
         (count-sql (sxql:select `((:as (:count (:distinct ,column)) :count)))))
    (loop for clause in (slot-value sql 'sxql.statement::children)
          for clause-type = (type-of clause)
          unless (member clause-type '(sxql.clause::fields-clause
                                       sxql.clause::limit-clause
                                       sxql.clause::offset-clause))
            do (sxql:add-child count-sql clause))
    (let* ((result (mito:retrieve-by-sql count-sql))
           (count (getf (first result) :count)))
      (values count count-sql))))

(defmacro with-paginate (class &body body)
  `(multiple-value-bind (result sql)
       (progn ,@body)
     (multiple-value-bind (total count-sql)
         (count-all ,class sql)
       (values result total sql count-sql))))

(defun id-column (object-type)
  (tkutil:string-to-keyword (format nil "~A.id" (table-name object-type))))

(defgeneric sortable-columns (object-type)
  (:documentation "columns that are sortable with OBJECT-TYPE when
calling select-* and search-* functions."))

(defmethod sortable-columns ((object-type t))
  (columns object-type))

(defgeneric date-columns (object-type)
  (:documentation "date columns that are filterd with OBJECT-TYPE when
calling select-* and search-* functions."))

(defun %filter-where (parameter &optional column func op)
  (when column
    (let* ((parameter (if (listp parameter) (car parameter) parameter))
           (value (if func `(funcall ,func ,parameter) parameter)))
      `(when ,parameter
         (where ,column ,value :op ,op)))))

(defun filter-where (filter)
  (apply #'%filter-where filter))

(defmacro with-search-parameters ((fields clauses object-type args &rest filters)
                                  &body body)
  "(with-search-parameters (fields-variable clauses-variable object-type args filter*) form*)
filter ::= variable | (variable [filter-option])
filter-option ::= column [func [op]]"
  (let* ((filters (mapcar #'tkutil:ensure-list filters))
         (filter-parameters (mapcar #'car filters))
         (filter-wheres (mapcar #'filter-where filters)))
    `(destructuring-bind (&key query
                            ,@filter-parameters
                            date-column start-date end-date
                            sort-column sort-direction
                            (page 1) (per-page 100)
                            (offset (if page (* (1- page) per-page) 0))
                            (limit (or per-page 100)))
         ,args
       (let ((,fields (all-fields ,object-type sort-column))
             (,clauses (list (sxql:offset offset) (sxql:limit limit)
                             (query query ,object-type)
                             ,@filter-wheres
                             (where-date date-column start-date end-date)
                             (order-by sort-column sort-direction))))
         (progn ,@body)))))

;; (defun search-users (&key id name email is-admin
;;                        query date-column start-date end-date
;;                        sort-column sort-direction page per-page offset limit
;;                        &rest args)
;;   (with-search-parameters clauses 'eboshi.model:user args
;;     (apply #'select-users
;;            (where :users.id id)
;;            (where :users.name name)
;;            (where :users.email email)
;;            (where :users.is_admin is-admin)
;;            clauses)))
;; (defun search-users (&key id name email is-admin
;;                      &rest args &allow-other-keys)
;;   (setf args
;;         (alexandria:remove-from-plist args :id :name :email :is-admin))
;;   (with-search-parameters clauses 'eboshi.model:user args
;;     (apply #'select-users
;;            (where :users.id id)
;;            (where :users.name name)
;;            (where :users.email email)
;;            (where :users.is_admin is-admin)
;;            clauses)))

;; #|
;; (defun search-users (&rest args)
;;   (tkmito.search:with-search-parameters
;;       (clauses 'eboshi.model:user args
;;                (id :users.id)
;;                (name :users.name)
;;                (email :users.email)
;;                is-admin)
;;     (apply #'select-users
;;            (tkmito.serach:where :users.is_admin (if is-admin 1 0))
;;            clauses)))
;; #|
