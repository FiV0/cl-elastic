# Cl-Elasticsearch - Elasticsearch client for Common Lisp

### Motivation

This project is supposed to be a simple interface for 
[Elasticsearch](https://www.elastic.co/products/elasticsearch). 
The emphasis is on *simple*, i.e. this is not supposed to be a DSL on top of 
another DSL. You should be able go to the Elasticsearch docs and translate
that immediatly into working code. If you are looking for a DSL checkout
the section about other work.

### Installation

Currently the library is not in quicklisp. You need to add the repo to your 
local quicklisp repos.

### Usage

```cl
(ql:quicklisp :cl-elasticsearch)
(use :cl-elasticsearch)

(defvar *client* (make-instance '<client> :endpoint "http://localhost:9200"))

;; creates an index named `elasticsearch-test`
(send-request *client* '("elasticsearch-test") :method :put)
```
You can enable `keyword` arguments which lets you use keywords as keys in 
hashtable and parameter plists as well as uri construction. 
The transformation is always to lowercase.

```cl
(setq *enable-keywords* t)
(send-request *client* '(:elasticsearch-test) :method :put)
```
The library uses the [yason](https://github.com/phmarek/yason) library under 
the hood to map between lisp objects and JSON. As hashtables are therefore 
ubiquitous for JSON construction the library also exports a simple reader syntax
for literal hashmap construction. The syntax is `#{key1 value1 key2 value2 ...}`.
For the enabling and disabling of the syntax the library uses 
[named-readtables](https://github.com/melisgl/named-readtables).

```cl
(in-readtable hashtable-syntax)

(defvar foo "bar")
;; creates a hashmap with ("bar" 1) and ("foo" 2) as key/value pairs
#{foo 1 "foo" 2}

(in-readtable :standard)
```
For the remaining examples we are assuming that `*enable-keywords*` is set to
true and the above hashtable syntax is enabled. The following examples
assume that you are using Elasticsearch version 7.0.0 or above,
otherwise you might need to adapt the index settings.

```cl
;; create a index with a test field of type text
(send-request *client* '("elasticsearch-test") :method :put 
              :data #{:settings #{:number_of_shards 1}
                      :mappings #{:properties #{:test #{:type "text"}}}})

;; create a document with id 3
(send-request *client* '(:elasticsearch-test :_doc 3) :method :put
              :data #{:test "toto"})
              
;; find a document by id
(send-request *client* '(:elasticsearch-test :_doc 3) :method :get)

;; search for something
(send-request *client* '(:elasticsearch-test :_search) :method :get
              :data #{:query #{:term #{:test "toto"}}})

;; delete the document
(send-request *client* '(:elasticsearch-test :_doc 3) :method :delete)

;; for bulk requests you can pass a list of operations as data
(send-request *client* '(:_bulk) :method :post
              :data (list
                     #{:index #{:_index "elasticsearch-test" :_id 3}}
                     #{:test "foo"}
                     #{:index #{:_index "elasticsearch-test" :_id 2}}
                     #{:test "bar"}
                     #{:delete #{:_index "elasticsearch-test" :_id 3}}))
```

### Other work

There are a couple of other clients, although non of them are in quicklisp:  
[clesc](https://github.com/own-pt/clesc)  
[cl-elasticsearch](https://github.com/kraison/cl-elasticsearch)  
[eclastic](https://github.com/gschjetne/eclastic)  

### Todo

- Add authentication
- Add async indexing with `cl-async`.

### Author

* Finn Völkel (firstname.lastname@gmail.com)

### Licence

MIT Licence

### Copyright

Copyright (c) 2019 Finn Völkel (firstname.lastname@gmail.com)
