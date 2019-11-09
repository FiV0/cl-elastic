#|
  This file is a part of cl-elasticsearch.
  (c) 2019 Finn Völkel 
  Author: Finn Völkel  (firstname.lastname@gmail.com)
|#

(asdf:defsystem cl-elasticsearch
  :version "0.0.1"
  :author "Finn Völkel"
  :license ""
  :serial t
  :depends-on ("bordeaux-threads"
               "cl-json"
               "drakma"
               "flexi-streams")
  :components ((:file "cl-elasticsearch"))
  :description "Elasticsearch client for Common Lisp"
  :in-order-to ((test-op (test-op "cl-elasticsearch-test"))))
