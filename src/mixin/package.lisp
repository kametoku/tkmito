(in-package #:cl-user)

(defpackage #:tkmito.mixin
  (:use #:cl)
  (:export #:optimistic-lock
           #:lock-version
           #:check-conflict
           #:*lock-optimistically-p*

           #:has-code
           #:code
           ))
