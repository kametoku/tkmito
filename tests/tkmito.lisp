(defpackage cl-cpj/tests/main
  (:use :cl
        :cl-cpj
        :rove))
(in-package :cl-cpj/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-cpj)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
