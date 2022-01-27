(in-package #:tkmito.mixin)

(defclass has-code ()
  ((code :col-type (:varchar 64)
         :initarg :code
         :initform (uuid:make-v4-uuid)
         :accessor code))
  (:metaclass mito:dao-table-mixin)
  (:unique-keys code))
