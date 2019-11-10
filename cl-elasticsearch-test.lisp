(defpackage cl-elasticsearch-test
  (:use :cl
        :cl-elasticsearch
        :parachute)
  (:shadow #:run)
  (:export cl-elasticsearch-test))
(in-package :cl-elasticsearch-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-elasticsearch)' in your Lisp.

(defvar *client* (make-instance '<client>))

(define-test simple-es-test 
  (multiple-value-bind (res status) (send-request *client* "")
    (of-type hash-table res)
    (is = 200 status)))

(defun get-version ()
  (let ((res (send-request *client* "")))
    (nth-value 0 (gethash "number" (gethash "version" res)))))

(defun major-version ()
  (let* ((version (get-version))
        (pos (position #\. version)))
    (nth-value 0 (parse-integer (subseq version 0 pos)))))


;; TODO check if index exists
(define-test create-and-delete-index-test
  :depends-on (simple-es-test)
  (multiple-value-bind (res status)
      (send-request *client* '(:elasticsearch-test) :method :put)
    (true (gethash "acknowledged" res))
    (is = 200 status))
  (multiple-value-bind (res status)
      (send-request *client* '(:elasticsearch-test) :method :delete)
    (true (gethash "acknowledged" res))
    (is = 200 status)))

(defun create-index (data)
  (send-request *client* '(:elasticsearch-test) :method :put :data data))

(defun delete-index ()
  (send-request *client* '(:elasticsearch-test) :method :delete))

(enable-hashtable-syntax)

(setq cl-elasticsearch:*enable-keywords* t)



(defvar index-settings #{:settings #{:number_of_shards 1}
                              :mapping #{:test_doc
                              #{:properties #{:test #{:type "text"}}}}})

(cl-elasticsearch::keywords-to-strings index-settings)

(create-index index-settings)

(delete-index)

(if (> 6 (major-version))
    (progn)
    (progn
      (let ((index-settings #{:settings #{:number_of_shards 1}
                              :mapping #{:test_doc
                              #{:properties #{:test #{:type "text"}}}}}
                              ))
        (create-index index-settings)
        (define-test index-document-test
          (multiple-value-bind (res status)
              (send-request *client* '(:elasticsearch-test :test_doc 3) :method :put
                            #{:test "toto" })
            )
          ))))


(disable-hashtable-syntax)
