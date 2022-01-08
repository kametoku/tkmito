(in-package #:tkmito.mixin)

(defclass has-code ()
  ((code :col-type (:varchar 64)
         :initarg :code
         :initform (lack.util:generate-random-id)
         :accessor code))
  (:metaclass mito:dao-table-mixin)
  (:unique-keys code))
