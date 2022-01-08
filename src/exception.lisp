(in-package :cl-user)
(defpackage tkmito.exception
  (:use :cl)
  (:export #:stale-object-error
           ))
(in-package :tkmito.exception)

(define-condition stale-object-error (error)
  ((object :initarg :object :initform nil :reader object))
  (:report (lambda (condition stream)
             (format stream "Optimistic Lock: object ~:[~;~:*~S ~]is stale."
                     (object condition)))))
