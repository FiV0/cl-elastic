# Cl-Elasticsearch - Elasticsearch client for Common Lisp

### Motivation

This project is supposed to be a simple interface for Elasticsearch. The
emphasis is on *simple*, i.e. this is not supposed to be a DSL on top of 
another DSL. You should be able go to the Elasticsearch docs and translate
that immediatly into working code. If you are looking for a DSL, checkout
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
for literal hashmap construction. 

```cl
(enable-hashtable-syntax)

(defvar foo "bar")
;; creates a hashmap with ("bar" 1) and ("foo" 2) key/value pairs
#{foo 1 "foo" 2}

(disable-hashtable-syntax)
```

### Other work

There are a couple of other clients, although non of them are in quicklisp:  
[clesc](https://github.com/own-pt/clesc)  
[cl-elasticsearch](https://github.com/kraison/cl-elasticsearch)  
[eclastic](https://github.com/gschjetne/eclastic)  

### Todo

- Add async indexing with `bordeaux-thread`.

### Author

* Finn Völkel (firstname.lastname@gmail.com)

### Licence

MIT Licence

### Copyright

Copyright (c) 2019 Finn Völkel (firstname.lastname@gmail.com)
