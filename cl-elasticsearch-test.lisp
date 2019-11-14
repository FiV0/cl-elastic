(defpackage cl-elasticsearch-test
  (:use :cl
        :cl-elasticsearch
        :parachute)
  (:import-from :named-readtables
                :in-readtable)
  (:shadow #:run)
  (:export cl-elasticsearch-test))
(in-package :cl-elasticsearch-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-elasticsearch)' in your Lisp.

(in-readtable hashtable-syntax)

(define-test enable-keywords-test
  (let* ((*enable-keywords* t)
         (res (cl-elasticsearch::keywords-to-strings #{:settings #{:foo "bar"}})))
    (true (nth-value 1 (gethash "settings" res)))
    (true (nth-value 1 (gethash "foo" (gethash "settings" res))))
    (is equal "bar" (gethash "foo" (gethash "settings" res)))))

(in-readtable :standard)

(defvar *client* (make-instance '<client>))

(define-test simple-es-test 
  (multiple-value-bind (res status) (send-request *client* "")
    (of-type hash-table res)
    (is = 200 status)))

(defun get-version ()
  (let* ((*enable-keywords* t)
         (res (send-request *client* "")))
    (nth-value 0 (gethash :number (gethash :version res)))))

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

(in-readtable hashtable-syntax)

(define-test index-find-delete-document-test
  (if (< 6 (major-version))
      (let ((index-settings #{:settings #{:number_of_shards 1}
                            :mappings #{:properties #{:test #{:type "text"}}}})
            (*enable-keywords* t))
        (create-index index-settings)
        (multiple-value-bind (res status)
            (send-request *client* '(:elasticsearch-test :_doc 3) :method :put
                          :data #{:test "toto"})
          (declare (ignore res))
          (is = 201 status))
        (multiple-value-bind (res status)
            (send-request *client* '(:elasticsearch-test :_doc 3) :method :get)
          (is = 200 status)
          (is equal "toto" (gethash :test (gethash :_source res))))
        (multiple-value-bind (res status)
            (send-request *client* '(:elasticsearch-test :_doc 3) :method :delete)
          (declare (ignore res))
          (is = 200 status))
        (multiple-value-bind (res status)
            (send-request *client* '(:elasticsearch-test :_doc 3) :method :delete)
          (declare (ignore res))
          (is = 404 status))
        (delete-index))
      (progn)))

(define-test index-and-search-document-test
  (if (< 6 (major-version))
      (let ((index-settings #{:settings #{:number_of_shards 1}
                            :mappings #{:properties #{:test #{:type "text"}}}})
            (*enable-keywords* t))
        (create-index index-settings)
        (multiple-value-bind (res status)
            (send-request *client* '(:elasticsearch-test :_doc 3) :method :put
                          :data #{:test "toto" })
          (declare (ignore res))
          (is = 201 status))
        ;; necessary for indexing to finish
        (sleep 1)
        (multiple-value-bind (res status)
            (send-request *client* '(:elasticsearch-test :_search) :method :get
                          :data #{:query #{:term #{:test "toto"}}})
          (is = 200 status)
          (is equal
              "toto"
              (gethash :test (gethash :_source
                                      (car (gethash :hits (gethash :hits res)))))))
        (delete-index))
      (progn)))

(define-test encode-json-test
  (let* ((*enable-keywords* t)
         (res (cl-elasticsearch::encode-json (list #{:foo 1} #{:bar 2}))))
    (is equal (format nil "{\"foo\":1}~%{\"bar\":2}~%") res)))

(define-test bulk-indexing-test
  :depends-on (encode-json-test)
  (if (< 6 (major-version))
      (let ((index-settings #{:settings #{:number_of_shards 1}
                            :mappings #{:properties #{:test #{:type "text"}}}})
            (*enable-keywords* t))
        (create-index index-settings)
        (multiple-value-bind (res status)
            (send-request *client* '(:_bulk) :method :post
                          :data (list
                                  #{:index #{:_index "elasticsearch-test" :_id 3}}
                                  #{:test "foo"}
                                  #{:index #{:_index "elasticsearch-test" :_id 2}}
                                  #{:test "bar"}
                                  #{:delete #{:_index "elasticsearch-test" :_id 3}}))
          (declare (ignore res))
          (is = 200 status))
        ;; necessary for indexing to finish
        (sleep 1)
        (multiple-value-bind (res status)
            (send-request *client* '(:elasticsearch-test :_search) :method :get
                          :data #{:query #{:term #{:test "bar"}}})
          (is = 200 status)
          (is equal
              "bar"
              (gethash :test (gethash :_source
                                      (car (gethash :hits (gethash :hits res)))))))
        (delete-index))
      (progn)))

(in-readtable :standard)
