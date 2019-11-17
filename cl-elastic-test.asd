#|
  This file is a part of cl-elastic.
  (c) 2019 Finn Völkel 
  Author: Finn Völkel  (firstname.lastname@gmail.com)
|#

(asdf:defsystem cl-elastic-test
  :name "cl-elastic-test"
  :version "1.0.0"
  :author "Finn Völkel  (firstname.lastname@gmail.com)"
  :license ""
  :description "Test system for cl-elastic"
  :depends-on (:cl-elastic
               :named-readtables
               :parachute)
  :components ((:file "cl-elastic-test"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test
                                                  :cl-elastic-test)))
