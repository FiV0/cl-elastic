#|
  This file is a part of cl-elastic.
  (c) 2019 Finn Völkel 
  Author: Finn Völkel  (firstname.lastname@gmail.com)
|#

(asdf:defsystem cl-elastic
  :version "0.0.1"
  :author "Finn Völkel"
  :license "MIT"
  :serial t
  :depends-on (:drakma
               :named-readtables
               :yason)
  :components ((:file "cl-elastic"))
  :description "Elasticsearch client for Common Lisp"
  :in-order-to ((test-op (test-op "cl-elastic-test"))))
