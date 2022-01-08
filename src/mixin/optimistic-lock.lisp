(in-package #:tkmito.mixin)

(defparameter *lock-optimistically-p* t)

(defclass optimistic-lock ()
  ((lock-version :col-type :integer
                 :initarg :lock-version
                 :initform 0
                 :accessor lock-version))
  (:metaclass mito:dao-table-mixin))

(defmethod check-conflict ((object optimistic-lock))
  (when *lock-optimistically-p*
    (let ((current (mito:find-dao (class-of object) :id (mito:object-id object))))
      (log:debug object current (lock-version object) (lock-version current))
      (unless (= (lock-version object) (lock-version current))
        (error 'tkmito.exception:stale-object-error :object object)))))

(defmethod mito:update-dao :before ((object optimistic-lock))
  (when *lock-optimistically-p*
    (mito.connection:check-connected)
    (check-conflict object)
    (incf (slot-value object 'lock-version))))

;; XXX: what if the transaction is aborted?
;; lock-version becomes incorrect by rollback.

;; (defmethod dbi:rollback :after ((conn dbi.driver:dbi-connection))
