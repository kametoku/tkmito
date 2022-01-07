(in-package :cl-user)
(defpackage tkmito.model
  (:use :cl)
  (:export #:make-instance-save
           #:resource-not-found
           #:find-instance
           #:find-instance-or-make-save
           #:retrieve-instances
           #:include-foreign-fields
           #:with-foreign-fields-included
           #:inflate-string
           #:validate
           #:update
           #:to-plist
           #:chain
           ))
(in-package :tkmito.model)

(defun make-instance-save (class &rest args)
  (let ((object (apply #'make-instance class args)))
    (mito:save-dao object)
    object))

(define-condition resource-not-found (error)
  ((resource :initarg :resource :initform nil :reader resource)
   (indicator :initarg :indicator :initform nil :reader indicator))
  (:report (lambda (condition stream)
             (format stream "~:[~;~:*~A ~]~:[~;~:*~S ~]not found."
                     (resource condition) (indicator condition)))))

(defun find-instance (class &rest args)
  (or (apply #'mito:find-dao class args)
      (error 'resource-not-found :resource class :indicator args)))

(defun find-instance-or-make-save (class &rest args)
  (or (apply #'mito:find-dao class args)
      (apply #'make-instance-save class args)))

(defun retrieve-instances (class &rest args)
  (or (apply #'mito:retrieve-dao class args)
      (error resource-not-found :resource class :indicator args)))

(defun include-foreign-fields (object &rest field-value-list)
  (let ((class (class-of object)))
    (loop for (field value) on field-value-list by #'cddr
          for slot = (mito.class:find-slot-by-name class field :test #'string=)
          unless slot
            do (error "Class ~S does not have a slot named ~S" class field)
          do (setf (slot-value object (c2mop:slot-definition-name slot)) value))
    object))

(defmacro with-foreign-fields-included (foreign-fields args &body body)
  `(destructuring-bind (&rest foreign-fields &key ,@foreign-fields
                        &allow-other-keys)
       ,args
     (declare (ignorable ,@foreign-fields))
     (let ((objects (progn ,@body)))
       (cond ((not foreign-fields) objects)
             ((listp objects)
              (loop for object in objects
                    collect (apply #'include-foreign-fields object foreign-fields)))
             (t (apply #'include-foreign-fields objects foreign-fields))))))

#|
(defun retrieve-custom-images (&rest args)
  (with-foreign-fields-included (design) args
    (apply #'retrieve-instances 'custom-image args)))

(let ((design ...))
  (retrieve-custom-images :design design))
|#

(defun inflate-string (value)
  ;; workaround for mito's issue related to varchar "".
  ;; if the column type is varchar and a colume value is "" (empty string),
  ;; mito set the nil to the corresponding class slot.
  (or value ""))


(defgeneric validate (object &rest args)
  (:documentation "Throw an error condition if any of ARGS is invalid for OBJECT.")
  (:method (object &key &allow-other-keys) t))

(defgeneric update (object &rest args)
  (:documentation "Update the slots of OBJECT with ARGS.
ARGS is a plist each of which consist of slot name in keyword and new value.
Before updating the slots, `validate' will be called to validate ARGS.
If OBJECT is a mito dao-class and and any slots are changed, OBJECT
will be saved.
If there is no slot to be updated or the argument DONT-SAVE is non-nil, the
object will be not saved to the database.

Example: (update item :quantity 100)"))

(defmethod update (object &rest args)
  (apply #'validate object args)
  (let ((slots (mapcar #'c2mop:slot-definition-name
                       (c2mop:class-slots (class-of object)))))
    (loop for slot in slots
          for key = (intern (symbol-name slot) :keyword)
          for value = (getf args key 'value-not-specified)
          unless (eql value 'value-not-specified)
            nconc (list key (setf (slot-value object slot) value)))))

(defparameter *dao-slots* '(:created-at :updated-at :synced :id))

(defmethod update :around ((object mito:dao-class) &rest args &key dont-save
                           &allow-other-keys)
  (apply #'alexandria:remove-from-plist args :dont-save *dao-slots*) ; XXX
  (let ((changed-p (apply #'call-next-method object args)))
    (when (and changed-p (not dont-save))
      (mito:save-dao object))
    changed-p))

(defgeneric to-plist (object)
  (:method (object) object)
  (:method ((object cons))
    (mapcar #'to-plist object))
  (:method ((object mito:dao-class))
    (let ((slots (mapcar #'c2mop:slot-definition-name
                         (c2mop:class-slots (class-of object)))))
      (loop for slot in slots
            for value = (and (slot-boundp object slot)
                             (slot-value object slot))
            when value
              nconc (list (intern (symbol-name slot) :keyword)
                          (to-plist (slot-value object slot)))))))

(defmacro chain (&rest args)
  "Call functions until it returns nil or reach to the last function call.
This macro can be used to chain together accessors and fucntion calls.
If any of functions in chain returns nil, it stops succeeding function calls
and return nil.
Example: (chain user #'user-address #'address-postal-code)"
  (if (cdr args)
      `(and ,(car args)
            (let ((object (funcall ,(cadr args) ,(car args))))
              (chain object ,@(cddr args))))
      (car args)))
