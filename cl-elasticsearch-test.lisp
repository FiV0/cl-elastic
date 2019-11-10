(defpackage cl-elasticsearch-test
  (:use :cl
        :cl-elasticsearch
        :parachute)
  (:shadow #:run)
  (:export cl-elasticsearch-test))
(in-package :cl-elasticsearch-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-elasticsearch)' in your Lisp.

(defvar client (make-instance '<client>))

(send-request client "")


(define-test text-target-1 
  (is-values (values 0 "1")
    (= 0)
    (equal "1")))
