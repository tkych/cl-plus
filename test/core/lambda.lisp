;;;; cl-plus/test/core/lambda.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for Lambda 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.lambda
  (:export #:?lambda)
  (:use #:cl #:fiveam)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:cl-plus-test
                #:all)
  (:import-from #:cl-plus.src.readtables
                #:cl+caret)
  (:import-from #:cl-plus.src.core.lambda
                #:*shorthand-lambda-parameters-limit*
                #:*shorthand-lambda-parameter-prefix*))

(in-package #:cl-plus.test.core.lambda)

(def-suite ?lambda :in all)
(in-suite ?lambda)

(in-readtable cl+caret)

;;--------------------------------------------------------------------

(test ?caret-reader.0.Church
  (is (= 4 (^x(+ x x) 2)))
  (is (= 5 (^xy(+ x y) 2 3)))
  (is (= 9 (^xyz(+ x y z) 2 3 4)))
  (is (= 5 (^x.y(+ x y) 2 3)))
  (is (= 9 (^x.y.z(+ x y z) 2 3 4)))
  (is (= 9 (^x0.y0.z0(+ x0 y0 z0) 2 3 4)))
  (is (= 9 (^val.(+ val) 9))))


(test ?caret-reader.1.McCarthy
  (is (= 5 ((^ (a b) (+ a b)) 2 3))))


(test ?caret-reader.2.Hickey
  (is (eq nil (^() 1)))
  (is (= 0  (^(+ @) 0 1 2 3 4 5)))
  (is (= 0  (^(+ @ @) 0 1 2 3 4 5)))
  (is (= 2  (^(+ @1 @1) 0 1 2 3 4 5)))
  (is (= 10 (^(+ @0 @1 @2 @3 @4) 0 1 2 3 4 5)))
  (is (equal (^(list @2 @REST) 0 1 2 3 4 5)
             '(2 (3 4 5))))
  (is (equal (^(list @2 @R) 0 1 2 3 4 5)
             '(2 (3 4 5))))
  (is (equal (^(cons @4 @ALL) 0 1 2 3 4 5)
             '(4 0 1 2 3 4 5)))
  (is (equal (^(cons @4 @A) 0 1 2 3 4 5)
             '(4 0 1 2 3 4 5)))
  (is (equal (^(list @4 @REST @ALL) 0 1 2 3 4 5)
             '(4 (5) (0 1 2 3 4 5))))
  (is (equal (^(list @4 @ALL @REST) 0 1 2 3 4 5)
             '(4 (0 1 2 3 4 5) (5))))
  
  (let ((@0 100) (@1 100) (@2 100) (@3 100) (@4 100))
    (declare (ignorable @0 @1 @2 @3 @4))
    (is (= 10 (^(+ @0 @1 @2 @3 @4) 0 1 2 3 4 5))))
  
  ;; (signals SIMPLE-ERROR ^(+ @10000)) ; read time error

  (is (= (funcall ^(if (<= @ 1)
                       1
                       (* @ (@ME (1- @))))
                  5)
         120))
  (is (= (funcall ^(if (<= @ 1)
                       1
                       (* @ (@M (1- @))))
                  5)
         120)))


(test ?caret-reader.3.nest
  ;; Church
  (is (= 45 (^x(^x(+ x 3) x)
               42)))
  (is (= 35 (^x(^x(+ x 3 (^x(- 10) 10000))
                  x)
               42)))
  ;; McCarthy
  (is (= 45 ((^ (x) ((^ (x) (+ x 3))
                     x))
             42)))
  (is (= 35 ((^ (x) ((^ (x) (+ x 3 ((^ (x) (declare (ignore x))
                                       (- 10))
                                    10000)))
                     x))
             42)))
  ;; Hickey
  (is (= 45 (^(^(+ @ 3)
                @)
              42)))
  (is (= 35 (^(^(+ @ 3 (^(- 10) 10000))
                @)
              42))))


(test ?caret-reader.4.composite
  (is (= 35 (^x((^ (x) (+ x 3 (^(- 10) 10000)))
                x)
               42)))
  (is (= 35 (^((^ (x) (+ x 3 (^x(- 10) 10000)))
               @)
              42)))
  (is (= 35 ((^ (x) (^(+ @ 3 (^x(- 10) 10000))
                      x))
             42))))


;;====================================================================
