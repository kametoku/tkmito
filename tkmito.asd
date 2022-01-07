(defsystem "tkmito"
  :version "0.1.0"
  :author "Tokuya Kameshima"
  :license "LLGPL"
  :depends-on ("alexandria"
               "cl-ppcre"
               "cl-reexport"
               "local-time"
               "log4cl"
               "mito"
               "sxql"
               "tkutil")
  :components ((:module "src"
                :components ((:file "db")
                             (:file "model")
                             (:file "search"))))
  :description "Tiny Common Lisp Wrapper for Mito."
  :in-order-to ((test-op (test-op "tkmito/tests"))))

(defsystem "tkmito/tests"
  :author ""
  :license ""
  :depends-on ("tkmito"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "tkmito"))))
  :description "Test system for tkmito"
  :perform (test-op (op c) (symbol-call :rove :run c)))
