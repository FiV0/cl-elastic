#|
  This file is a part of cl-elasticsearch.
  (c) 2019 Finn Völkel 
  Author: Finn Völkel  (firstname.lastname@gmail.com)
|#

(asdf:defsystem cl-elasticsearch-test
  :name "cl-elasticsearch-test"
  :version "1.0.0"
  :author "Finn Völkel  (firstname.lastname@gmail.com)"
  :license ""
  :description "Test system for cl-elasticsearch"
  :depends-on (:cl-elasticsearch
               :parachute)
  :components ((:file "cl-elasticsearch-test"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test
                                                  :cl-elasticsearch-test)))
