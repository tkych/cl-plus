;;;; Last modified: 2014-06-29 10:02:21 tkych

;; cl-plus/src/cdr/cdr-8.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; CDR-8: Generic Equality and Comparison for Common Lisp
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.cdr.cdr-8
  (:documentation "
CDR-8
=====


Generic Functions
-----------------

 * EQUALS
 * COMPARE
 * HASH-CODE


Functions
---------

 * LT
 * LTE
 * GT
 * GTE


Condition
---------

 * UNCOMPARABLE-OBJECTS


Reference
---------

 [0] Marco Antoniotti,
     CDR 8: Generic Equality and Comparison for Common Lisp.
     http://cdr.eurolisp.org/document/8/
")
  (:nicknames #:cl+cdr-8)
  (:export #:*epsilon-for-comparing-float*
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
           #:hash-code)
  (:use #:cl))

(in-package #:cl-plus.src.cdr.cdr-8)

;;--------------------------------------------------------------------
;; EPSILON for floating number comparsion
;;--------------------------------------------------------------------
;; TBD: default value of *epsilon-for-comparing-float*

(defparameter *epsilon-for-comparing-float*
  #.(* (expt 10 6) (max double-float-epsilon double-float-negative-epsilon))
  "epsilon for floating number comparsion.")


;;--------------------------------------------------------------------
;; EQUALS
;;--------------------------------------------------------------------

(defgeneric equals (a b &rest keys &key &allow-other-keys)
  (:documentation "
EQUALS a b &rest keys &key &allow-other-keys => result


Arguments and Values
--------------------

 a, b             -- common lisp objects.
 recursive        -- a generalized boolean; default is NIL.
 result           -- a boolean.
 keys             -- a list (as per the usual behavior).
 by-key           -- a generalized boolean; default is T.
 by-values        -- a generalized boolean; default is T.
 check-properties -- a generalized boolean; default is NIL.
 case-sensitive   -- a generalized boolean; default is T. 


Description
-----------

The EQUALS generic functions defines methods to test for \"equality\"
of two objects a and b. When two objects a and b are EQUALS under an
appropriate and context-dependent notion of \"equality\", then the
function returns T as result; otherwise EQUALS returns NIL as result.

If the argument recursive is T, then EQUALS may recurse down the
\"structure\" of a and b. The description of each known method
contains the relevant information about its recursive dependent
behavior.

EQUALS provides some default behavior, but it is intended mostly as a
hook for users. As such, it is allowed to add keyword arguments to
user-defined EQUALS methods, as the &key and &allow-other-keys
lambda-list markers imply.


Reference
---------

 [0] Marco Antoniotti,
     CDR 8: Generic Equality and Comparison for Common Lisp.
     http://cdr.eurolisp.org/document/8/
"))

(defmethod equals ((a t) (b t) &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (equalp a b))

(defmethod equals ((a number) (b number) &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (= a b))

(defmethod equals ((a complex) (b number) &rest keys &key &allow-other-keys)
  (and (apply #'equals (realpart a) (realpart b) keys)
       (apply #'equals (imagpart a) (imagpart b) keys)))

(defmethod equals ((a number) (b complex) &rest keys &key &allow-other-keys)
  (apply #'equals b a keys))

(defmethod equals ((a float) (b number) &rest keys
                   &key (epsilon *epsilon-for-comparing-float*) &allow-other-keys)
  (declare (ignore keys))
  ;; References:
  ;; -----------
  ;;  * Bruce Dawson, Comparing Floating Point Numbers, 2012 Edition.
  ;;    http://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
  ;; 
  ;;  * Kazuhiro Fujieda, 浮動小数点数の等値比較 Ruby
  ;;    http://qiita.com/fujieda/items/90d5465c887f2607e21a
  ;;
  ;;  * Michael Borgwardt (a.k.a. brazzy), The Floating-Point Guide, Comparison.
  ;;    http://floating-point-gui.de/errors/comparison/
  ;;    https://github.com/brazzy/floating-point-gui.de
  ;;    http://stackoverflow.com/questions/4915462/how-should-i-do-floating-point-comparison
  (if (= a b)
      ;; 0. Shortcut.
      T
      (let ((diff (abs (- a b))))
        (or
         ;; 1. Absolute error.
         ;;    * If a and b are large numbers, returns NIL.
         (< diff epsilon)
         ;; 2. Relative error.
         ;;    * If a and b close to zero, returns NIL.
         (< diff (* epsilon (max (abs a) (abs b))))))))

(defmethod equals ((a number) (b float) &rest keys &key &allow-other-keys)
  (apply #'equals b a keys))

(defmethod equals ((a cons) (b cons) &rest keys &key recursive &allow-other-keys)
  (if recursive
      (tree-equal a b :test (lambda (x y) (apply #'equals x y keys)))
      (eq a b)))

(defmethod equals ((a character) (b character) &rest keys
                    &key (case-sensitive t) &allow-other-keys)
  (declare (ignore keys))
  (if case-sensitive
      (char= a b)
      (char-equal a b)))

(defmethod equals ((a string) (b string) &rest keys
                    &key (case-sensitive t) &allow-other-keys)
  (declare (ignore keys))
  (if case-sensitive
      (string= a b)
      (string-equal a b)))

;; ADD
(defmethod equals ((a vector) (b vector) &rest keys &key &allow-other-keys)
  (or (eq a b)
      (and (= (length a) (length b))
           (every (lambda (x y) (apply #'equals x y keys))
                  a b))))

(defmethod equals ((a array) (b array) &rest keys &key &allow-other-keys)
  (and (equal (array-dimensions a) (array-dimensions b))
       (loop :for i :from 0 :below (array-total-size a)
             :always (apply #'equals (row-major-aref a i) (row-major-aref b i)
                            keys))))

(defmethod equals ((a structure-object) (b structure-object)
                   &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (eq a b))

(defmethod equals ((a standard-object) (b standard-object)
                   &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (eq a b))

(defun get-sorted-keys (ht)
  (let ((keys (make-array (hash-table-count ht) :element-type t)))
    (loop :for i :from 0
          :for k :being :the :hash-keys :of ht
          :do (setf (svref keys i) k))
    (sort keys #'string< :key #'write-to-string)))

(defun get-sorted-values (ht)
  (let ((vals (make-array (hash-table-count ht) :element-type t)))
    (loop :for i :from 0
          :for v :being :the :hash-values :of ht
          :do (setf (svref vals i) v))
    (sort vals #'string< :key #'write-to-string)))

(defmethod equals ((a hash-table) (b hash-table) &rest keys
                   &key (by-key t) (by-value t) check-properties
                   &allow-other-keys)
  (or (eq a b)
      (and (= (hash-table-count a) (hash-table-count b))
           (if check-properties
               (and (eql (hash-table-test a)
                         (hash-table-test b))
                    (= (hash-table-rehash-size a)
                       (hash-table-rehash-size b))
                    (= (hash-table-rehash-threshold a)
                       (hash-table-rehash-threshold b)))
               T)
           (if by-key
               (every (lambda (x y) (apply #'equals x y keys))
                      (get-sorted-keys a) (get-sorted-keys b))
               T)
           (if by-value
               (every (lambda (x y) (apply #'equals x y keys))
                      (get-sorted-values a) (get-sorted-values b))
               T))))


;;--------------------------------------------------------------------
;; COMPARE
;;--------------------------------------------------------------------

(defgeneric compare (a b &rest keys &key &allow-other-keys)
  (:documentation "
COMPARE a b &rest keys &key recursive &allow-other-keys => result


Arguments and Values
--------------------

 a, b           -- common lisp objects.
 recursive      -- a generalized boolean; default is NIL.
 result         -- a symbol of type (member < > = /=).
 keys           -- a list (as per the usual behavior).
 case-sensitive -- a generalized boolean; default is T.


Description
-----------

The generic function COMPARE defines methods to test the ordering of
two objects a and b, if such order exists. The result value returned
by COMPARE is one of the four symbols: <, >, = or /=. The COMPARE
function returns /= as result by default; thus it can represent
partial orders among objects. The equality tests should be coherent
with what the generic function EQUALS does.

If the argument recursive is T, then COMPARE may recurse down the
\"structure\" of a and b. The description of each known method
contains the relevant information about its recursive dependent
behavior.


Reference
---------

 [0] Marco Antoniotti,
     CDR 8: Generic Equality and Comparison for Common Lisp.
     http://cdr.eurolisp.org/document/8/
"))


(defmethod compare ((a T) (b T) &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  'cl:/=)

;; NOTE:
;;  When a or b is a complex numbers:
;;    1. If a = b, then it returns = .
;;    2. If a /= b, then it returns /= .  <- Modified!
;;  (The function < and > are for real numbers. If a or b is complex, then it raises type-error)
(defmethod compare ((a complex) (b number) &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (if (= a b)
      'cl:=
      'cl:/=))

(defmethod compare ((a number) (b complex) &rest keys &key &allow-other-keys)
  (apply #'compare b a keys))

;; TODO:
;; compare float number
(defmethod compare ((a number) (b number) &rest keys
                    &key (epsilon *epsilon-for-comparing-float*) &allow-other-keys)
  (declare (ignorable epsilon))
  (cond ((apply #'equals a b keys) 'cl:=)
        ((< a b)                   'cl:<)
        (t                         'cl:>)))

(defmethod compare ((a rational) (b rational) &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (cond ((= a b) 'cl:=)
        ((< a b) 'cl:<)
        (t       'cl:>)))

;; NOTE:
;;  * case-sensitive nil -> t, (CDR-8 is inconsistent for case-sensitive default).
(defmethod compare ((a character) (b character) &rest keys
                    &key (case-sensitive t) &allow-other-keys)
  (declare (ignore keys))
  (if case-sensitive
      (cond ((char= a b) 'cl:=)
            ((char< a b) 'cl:<)
            (t           'cl:>))
      (cond ((char-equal a b) 'cl:=)
            ((char-lessp a b) 'cl:<)
            (t                'cl:>))))

;; NOTE:
;;  * case-sensitive nil -> t, (CDR-8 is inconsistent for case-sensitive default).
(defmethod compare ((a string) (b string) &rest keys
                    &key (case-sensitive t) &allow-other-keys)
  (declare (ignore keys))
  (if case-sensitive
      (cond ((string= a b) 'cl:=)
            ((string< a b) 'cl:<)
            (t             'cl:>))
      (cond ((string-equal a b) 'cl:=)
            ((string-lessp a b) 'cl:<)
            (t                  'cl:>))))

(defmethod compare ((a symbol) (b symbol) &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (if (eq a b)
      'cl:=
      'cl:/=))

;; ADD
(defmethod compare ((a cons) (b cons) &rest keys &key recursive &allow-other-keys)
  (if (not recursive)
      (if (equal a b) 'cl:= 'cl:/=)
      (if (or (cdr a) (cdr b)) ; <-> (not (and (endp (cdr a)) (endp (cdr b))))
          (let ((car-comp (apply #'compare (car a) (car b) keys))
                (cdr-comp (apply #'compare (cdr a) (cdr b) keys)))
            (if (eq car-comp cdr-comp)
                car-comp
                'cl:/=))
          (apply #'compare (car a) (car b) keys))))

;; ADD
(defmethod compare ((a array) (b array) &rest keys &key recursive &allow-other-keys)
  (if (or (not recursive)
          (zerop (array-total-size a)))
      (if (equalp a b) 'cl:= 'cl:/=)
      (if (= (array-total-size a) (array-total-size b))
          (or (loop :with order := (apply #'compare
                                          (row-major-aref a 0)
                                          (row-major-aref b 0)
                                          keys)
                    :for i :from 1 :below (array-total-size a)
                    :always (eq order (apply #'compare
                                             (row-major-aref a i)
                                             (row-major-aref b i)
                                             keys))
                    :finally (return order))
              'cl:/=)
          'cl:/=)))


;;--------------------------------------------------------------------
;; LT, LTE, GT, GTE
;;--------------------------------------------------------------------

(define-condition uncomparable-objects (error)
  ((a :initarg :a)
   (b :initarg :b))
  (:report (lambda (c s)
             (with-slots (a b) c
               (format s "There is no ordering relation between ~S and ~S." a b)))))

(defun lt (a b &rest keys &key &allow-other-keys)
  (case (apply #'compare a b keys)
    (cl:<  t)
    (cl:/= (error 'uncomparable-objects :a a :b b))
    (t     nil)))

(defun lte (a b &rest keys &key &allow-other-keys)
  (case (apply #'compare a b keys)
    (cl:>  nil)
    (cl:/= (error 'uncomparable-objects :a a :b b))
    (t     t)))

(defun gt (a b &rest keys &key &allow-other-keys)
  (case (apply #'compare a b keys)
    (cl:>  t)
    (cl:/= (error 'uncomparable-objects :a a :b b))
    (t     nil)))

(defun gte (a b &rest keys &key &allow-other-keys)
  (case (apply #'compare a b keys)
    (cl:<  nil)
    (cl:/= (error 'uncomparable-objects :a a :b b))
    (t  t)))

(setf (fdefinition 'lessp)        (fdefinition 'lt)
      (fdefinition 'not-greaterp) (fdefinition 'lte)
      (fdefinition 'greaterp)     (fdefinition 'gt)
      (fdefinition 'not-lessp)    (fdefinition 'gte))


;;--------------------------------------------------------------------
;; HASH-CODE
;;--------------------------------------------------------------------

(defgeneric hash-code (hash-table)
  (:documentation "
HASH-CODE a => result

Arguments and Values
--------------------

 a      -- a common lisp object.
 result -- a positive fixnum in the range (mod array-total-size-limit).


Description
-----------

The HASH-CODE generic function is provided as a companion to EQUALS
for the benefit of those Common Lisp implementations that provide a
handle on the inner working of hash tables (usually in the form of an
extra :sxhash or :hash-function keyword argument to make-hash-table),
or for bottom-up hash table implementations.

HASH-CODE is modeled after the Java hashCode() method of
java.lang.Object. The same description applies almost unchanged.

The general contract of HASH-CODE is the following.

 * Whenever it is invoked on the same object more than once during an
   a Common Lisp session, the HASH-CODE generic function must
   consistently return the same fixnum, provided no information used
   in EQUALS comparisons on the object a is modified. This integer
   need not remain consistent from one Common Lisp session to another.

 * If two objects are equal according to the EQUALS generic predicate,
   then calling the HASH-CODE generic function on each of the two
   objects must produce the same integer result.

 * It is not required that if two objects are unequal according to the
   EQUALS generic predicate, then calling the HASH-CODE generic
   function on each of the two objects must produce distinct integer
   results. However, the programmer should be aware that producing
   distinct integer results for unequal objects may improve the
   performance of hashtables.


Reference
---------

 [0] Marco Antoniotti,
     CDR 8: Generic Equality and Comparison for Common Lisp.
     http://cdr.eurolisp.org/document/8/
"))


(defmethod hash-code ((ht hash-table))
  (sxhash ht))


;;====================================================================
