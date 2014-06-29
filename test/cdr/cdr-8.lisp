;;;; Last modified: 2014-06-29 10:24:38 tkych

;; cl-plus/test/cdr/cdr-8.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for CDR-8 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.cdr.cdr-8
  (:export #:?cdr-8)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:all)
  (:import-from #:cl-plus.src.cdr.cdr-8
                #:*epsilon-for-comparing-float*
                #:equals
                #:compare
                #:uncomparable-objects
                #:lt
                #:lte
                #:gt
                #:gte
                #:lessp
                #:not-greaterp
                #:greaterp
                #:not-lessp
                #:hash-code))

(in-package #:cl-plus.test.cdr.cdr-8)

(def-suite ?cdr-8 :in all)
(in-suite ?cdr-8)

;;--------------------------------------------------------------------
;; EQUALS
;;--------------------------------------------------------------------

(test ?equals.numbers
  ;; integers
  (is-true  (equals 42 42))
  (is-false (equals 42 10))
  (is-false (equals 42 'a))
  ;; float
  (let ((*epsilon-for-comparing-float* 1d-10))
    (is-true (equals (- 1.0d0 0.9d0)
                     0.1d0))
    (is-true (equals (+ 0.15d0 0.15d0)
                     (+ 0.1d0 0.2d0)))
    (is-true (equals (- 1.0d0 0.9d0)
                     0.1d0))
    (is-true (equals 0.0d0
                     1d-11))
    (let ((a 1d6) (b 1d6))
      (dotimes (_ 10) (incf a 0.1d0))
      (incf b 1.0d0)
      (is-true (equals a b)))
    (is-true  (equals 42.0 42.0))
    (is-false (equals 42.000001d0
                      42.000002d0
                      :epsilon
                      00.00000001d0))
    (is-true  (equals 42.00000000000001d0
                      42.00000000000002d0
                      :epsilon
                      00.00000000001d0)))
  ;; rational
  (is-true  (equals 42/3 42/3))
  (is-true  (equals 42/3 84/6))
  (is-false (equals 42/3 10/3))
  (is-false (equals 42/3 'a))
  ;; complex
  (is-true  (equals #c(1 1) #c(1 1)))
  (is-true  (equals #c(1 1)
                    #c(1 1.000000002d0)
                    :epsilon
                         0.001d0))
  (is-false (equals #c(1 1) #c(1 1.1)))
  (is-false (equals #c(1 1) 'a)))

(test ?equals.conses
  (is-false (equals (cons 1 1) (cons 1 1)))
  (is-true  (equals (cons 1 1) (cons 1 1)
                    :recursive t))
  (is-false (equals '(0 #\1 two "three")
                    '(0 #\1 two "THREE")))
  (is-false (equals '(0 #\1 two "three")
                    '(0 #\1 two "THREE")
                    :recursive t))
  (is-true  (equals '(0 #\1 two "three")
                    '(0 #\1 two "THREE")
                    :recursive t
                    :case-sensitive nil)))

(test ?equals.characters
  (is-true  (equals #\a #\a))
  (is-false (equals #\a #\A))
  (is-true  (equals #\a #\A :case-sensitive nil)))

(test ?equals.strings
  (is-true  (equals "abc" "abc"))
  (is-false (equals "FOO" "foo"))
  (is-true  (equals "FOO" "foo" :case-sensitive nil)))

(test ?equals.vectors
  (let ((vec (make-array 3 :initial-contents '(0 1 2))))
    (is-true  (equals vec (vector 0 1 2)))
    (is-true  (equals vec #(0 1 2)))
    (is-false (equals vec (vector 2 1 0)))
    (is-false (equals vec :foo))))

(test ?equals.arrays
  (let ((ary (make-array '(3 3) :initial-contents '((0 1 2)
                                                    (3 4 5)
                                                    (6 7 8)))))
    (is-true  (equals ary #2A((0 1 2) (3 4 5) (6 7 8))))
    (is-false (equals ary #2A((6 7 8) (0 1 2) (3 4 5))))
    (is-false (equals ary (vector 2 1 0)))))

(test ?equals.hash-tables
  (let ((ht1 (make-hash-table :test 'eq))
        (ht2 (make-hash-table :test 'equalp)))
    (loop :for (k v) :in '((:foo 0) (:bar 1) (:baz 2))
          :do (setf (gethash k ht1) v
                    (gethash k ht2) v))
    (is-true  (equals ht1 ht2))
    (is-false (equals ht1 ht2 :check-properties t))

    (setf (gethash :foo ht1) 42)
    (is-false (equals ht1 ht2))
    (is-true  (equals ht1 ht2 :by-value nil))

    (remhash :foo ht1)
    (setf (gethash :quux ht1) 0)
    (is-false (equals ht1 ht2))
    (is-true  (equals ht1 ht2 :by-key nil))

    (setf (gethash :foo ht1) 0)
    (is-false (equals ht1 ht2 :by-key nil))))

(defstruct foo a s d)

(test ?equals.structures
  (handler-case 
      (remove-method #'equals
                     (find-method #'equals () (mapcar #'find-class '(foo foo))))
    (error () nil))
  (is-false (equals (make-foo :a 42 :d "a string")
                    (make-foo :a 42 :d "a string")))
  (is-false (equals (make-foo :a 42 :d "a bar")
                    (make-foo :a 42 :d "a baz")))
  (defmethod equals ((a foo) (b foo) &key (recursive t) &allow-other-keys)
    (declare (ignore recursive))
    (or (eq a b)
        (= (foo-a a) (foo-a b))))

  (is-true (equals (make-foo :a 42 :d "a string")
                   (make-foo :a 42 :d "a string")))
  (is-true (equals (make-foo :a 42 :d "a string")
                   (make-foo :a 42 :d "a String")
                   :case-sensitive t))
  (handler-case 
     (remove-method #'equals
                    (find-method #'equals () (mapcar #'find-class '(foo foo))))
   (error () nil)))


;;--------------------------------------------------------------------
;; COMPARE
;;--------------------------------------------------------------------

(test ?compare.numbers
  ;; integer
  (is (eq (compare 42 0)
          '>))
  (is (eq (compare 42 1024)
          '<))
  ;; rational
  (is (eq (compare 42/5 0/5)
          '>))
  (is (eq (compare 42/3 1024/3)
          '<))
  ;; float
  (is (eq (compare pi pi)
          '=))
  (is (eq (compare pi 3.0s0)
          '>))
  (is (eq (compare pi (+ 1d-5 pi)
                   :epsilon 1d-6)
          '<))
  (is (eq (compare pi (+ 1d-5 pi)
                   :epsilon 1d-3)
          '=))
  ;; complex
  (is (eq (compare #c(1 0) 1)
          '=))
  (is (eq (compare #c(1 0) #c(1 0))
          '=))
  (is (eq (compare #c(1 0) #c(2 0))
          '<))
  (is (eq (compare #c(1 0) #c(1 1))
          '/=)))

(test ?compare.symbols
  (is (eq (compare 'this-symbol 'this-symbol)
          '=))
  (is (eq (compare 'this-symbol 'that-symbol)
          '/=)))

(test ?compare.characters
  (is (eq (compare #\a #\a)
          '=))
  (is (eq (compare #\a #\A)
          '>))
  (is (eq (compare #\a #\A :case-sensitive nil)
          '=)))

(test ?compare.strings
  (is (eq (compare "asd" "asd")
          '=))
  (is (eq (compare "asd" "ASD")
          '>))
  (is (eq (compare "asd" "ASD" :case-sensitive nil)
          '=)))

;; (defstruct foo a s d)

(test ?compare.structures
  (handler-case 
      (remove-method #'compare
                     (find-method #'compare () (mapcar #'find-class '(foo foo))))
    (error () nil))
  
  (is (eq (compare (make-foo :a 42) (make-foo :a 42))
          '/=))

  (defmethod compare ((a foo) (b foo) &rest keys &key recursive &allow-other-keys)
    (declare (ignore recursive))
    (let ((d-r (apply #'compare (foo-d a) (foo-d b) keys))
          (a-r (apply #'compare (foo-a a) (foo-a b) keys)))
      (if (eq d-r a-r)
          d-r
          '/=)))

  ;; FIXME: ??? Probably, CDR-8 might have typo or I'm in PANIC ??? Modified: /= -> < .
  (is (eq (compare (make-foo :a  0 :d "I am a FOO")
                   (make-foo :a 42 :d "I am a foo"))
          '<))
  ;; FIXME: ??? Probably, CDR-8 might have typo or I'm in PANIC ??? Modified: < -> /= .
  (is (eq (compare (make-foo :a  0 :d "I am a FOO")
                   (make-foo :a 42 :d "I am a foo")
                   :case-sensitive nil)
          '/=))
  
  (handler-case 
      (remove-method #'compare
                     (find-method #'compare () (mapcar #'find-class '(foo foo))))
    (error () nil)))


(test ?compare.arrays
  (is (eq (compare #(q w e r t y) #(q w e r t y 42))
          '/=))
  (is (eq (compare (make-array 3 :initial-element 0)
                   (vector 1 2 42))
          '/=)))

(test ?compare.conses
  (is (eq (compare '(q w e r t y) '(q w e r t y))
          '=))
  (is (eq (compare '(q w e r t y) '(q w e r t y) :recursive t)
          '=))
  (is (eq (compare '(1 1) '(2 2))
          '/=))
  (is (eq (compare '(1 1) '(2 2) :recursive t)
          '<))
  (is (eq (compare '(1 1) '(2 2 2) :recursive t)
          '/=))
  (is (eq (compare '(1 1 1 1) '(2 2 2) :recursive t)
          '/=))
  (is (eq (compare '(1 1 1 1) '("2" 2 2) :recursive t)
          '/=))
  (is (eq (compare '("1") '("2") :recursive t)
          '<))
  (is (eq (compare '(a) '(a) :recursive t)
          '=))
  (is (eq (compare '(nil) '(nil) :recursive t)
          '=))
  (is (eq (compare '() '() :recursive t)
          '=))
  (is (eq (compare '() '())
          '=)))


;;--------------------------------------------------------------------
;; LT, LTE, GT, GTE
;;--------------------------------------------------------------------

;; TODO:

(test ?lt
  (is-false (lt 42 0))
  (is-true  (lt 42 1024))
  (is-false (lt "asd" "asd")))

(test ?lte
  (is-false (lte "asd" "ASD"))
  (is-true  (lte "asd" "ASD" :case-sensitive nil)))

(test ?gt
  (is-true (gt pi 3.0s0)))

(test ?gte
  (is-true (gte pi pi)))


;; (defstruct foo a s d)

(test ?lte.structure

  (defmethod compare ((a foo) (b foo) &rest keys &key recursive &allow-other-keys)
    (declare (ignore recursive))
    (let ((d-r (apply #'compare (foo-d a) (foo-d b) keys))
          (a-r (apply #'compare (foo-a a) (foo-a b) keys)))
      (if (eq d-r a-r)
          d-r
          '/=)))
  
  ;; FIXME: ??? Probably, CDR-8 might have typo ???
  (signals uncomparable-objects
    (lte (make-foo :a 0 :d "I am a FOO")
         (make-foo :a 42 :d "I am a foo")
         :case-sensitive nil))
  
  (is-true (lte (make-foo :a 0 :d "I am a FOO")
                (make-foo :a 42 :d "I am a foo")))

  (handler-case 
      (remove-method #'compare
                     (find-method #'compare () (mapcar #'find-class '(foo foo))))
    (error () nil)))

(test ?uncomparable-objects
  (signals uncomparable-objects
    (lte (make-array 3 :initial-element 0)
         (vector 1 2 42))))


;;--------------------------------------------------------------------
;; HASH-CODE
;;--------------------------------------------------------------------

(test ?hash-code
  (let ((ht (make-hash-table)))
    (is (= (hash-code ht)
           (sxhash ht)))))


;;====================================================================
