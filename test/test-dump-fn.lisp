;;;; cl-plus/test/test-dump-fn.lisp

;; For making function at READ TIME, e.g., '#[1..] => #[1..]
;; This file is to check whether cl implementaion is able to dump
;; functions into compiled file.

;; cf.
;; http://objectmix.com/lisp/382390-objects-type-function-cant-dumped-into-fasl-files.html
;; https://groups.google.com/forum/#!topic/comp.lang.lisp/8uob-i07BEc
;; http://john.freml.in/clozurecl-head-finalization-vs-compile-load-bug
;; http://ircarchive.info/lisp/2007/5/11/34.html

;; CLHS:
;;  > Functions are not externalizable objects.
;;     cf. http://clhs.lisp.se/Body/03_bdbb.htm
;;  > externalizable object n. an object that can be used as a literal
;;  > object in code to be processed by the file compiler.
;;     cf. http://clhs.lisp.se/Body/26_glo_e.htm#externalizable_object
;;     cf. http://clhs.lisp.se/Body/03_bda.htm


;;====================================================================
;; Test, dump function into compiled file
;;====================================================================
;; Usage
;; -----
;; 
;;  If the followings runs at repl with no error, cl system is
;;  able to dump functions into compiled files.
;;
;;  * (compile-file "./test/test-dump-fn.lisp")
;;  * (load "./test/dump-fn-test")
;;  * (funcall dump-fn:cons-fn 1 '(2)) => (1 2)
;;  * (funcall dump-fn:closure 1) => 43

(in-package :cl-user)
(defpackage :dump-fn (:use :cl))
(in-package :dump-fn)

(export 'cons-fn)
(defvar cons-fn #.#'cons)

(export 'closure)
(defvar closure #.(let ((a 42)) (lambda (x) (+ x a))))


;;====================================================================
