;;;; Last modified: 2014-06-29 10:27:26 tkych

;; cl-plus/test/prelude.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Prelude for CL-PLUS-TEST
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus-test
  (:nicknames #:cl+t)
  (:export #:run-tests
           #:all
           #:is-=
           #:is-equal
           #:is-equalp)
  (:use #:cl #:fiveam))

(in-package #:cl-plus-test)

(def-suite all)


(defmacro is-= (got expected &rest reason-args)
  `(is (= ,got ,expected) ,@reason-args))

(defmacro is-eql (got expected &rest reason-args)
  `(is (eql ,got ,expected) ,@reason-args))

(defmacro is-equal (got expected &rest reason-args)
  `(is (equal ,got ,expected) ,@reason-args))

(defmacro is-equalp (got expected &rest reason-args)
  `(is (equalp ,got ,expected) ,@reason-args))


;;====================================================================
