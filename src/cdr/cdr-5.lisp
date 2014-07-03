;;;; cl-plus/src/cdr/cdr-5.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;; The following code is implemented from CDR-5 documentation by a
;; very straightforward way, except for ARRAY-INDEX type.  It is
;; slightly modified to be compatible with alexandria:ARRAY-INDEX.

;;====================================================================
;; CDR-5: Sub-interval Numerical Types for Common Lisp
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.cdr.cdr-5
  (:documentation "
CDR-5
=====

Types & Predicates
------------------

 * See CDR-5.


Note
----

 - Alexandria also implements CDR-5. cf. alexandria/types.lisp


Reference
---------

 [0] Marco Antoniotti (2008),
     CDR 5: Sub-interval Numerical Types for Common Lisp.
     http://cdr.eurolisp.org/document/5/
")
  (:nicknames #:cl+cdr-5)
  (:export #:negative-fixnum
           #:negative-fixnum-p
           #:non-positive-fixnum
           #:non-positive-fixnum-p
           #:non-negative-fixnum
           #:non-negative-fixnum-p
           #:positive-fixnum
           #:positive-fixnum-p

           #:negative-integer
           #:negative-integer-p
           #:non-positive-integer
           #:non-positive-integer-p
           #:non-negative-integer
           #:non-negative-integer-p
           #:positive-integer
           #:positive-integer-p

           #:negative-rational
           #:negative-rational-p
           #:non-positive-rational
           #:non-positive-rational-p
           #:non-negative-rational
           #:non-negative-rational-p
           #:positive-rational
           #:positive-rational-p

           #:negative-ratio
           #:negative-ratio-p
           #:non-positive-ratio
           #:non-positive-ratio-p
           #:non-negative-ratio
           #:non-negative-ratio-p
           #:positive-ratio
           #:positive-ratio-p

           #:negative-real
           #:negative-real-p
           #:non-positive-real
           #:non-positive-real-p
           #:non-negative-real
           #:non-negative-real-p
           #:positive-real
           #:positive-real-p

           #:negative-float
           #:negative-float-p
           #:non-positive-float
           #:non-positive-float-p
           #:non-negative-float
           #:non-negative-float-p
           #:positive-float
           #:positive-float-p

           #:negative-short-float
           #:negative-short-float-p
           #:non-positive-short-float
           #:non-positive-short-float-p
           #:non-negative-short-float
           #:non-negative-short-float-p
           #:positive-short-float
           #:positive-short-float-p

           #:negative-single-float
           #:negative-single-float-p
           #:non-positive-single-float
           #:non-positive-single-float-p
           #:non-negative-single-float
           #:non-negative-single-float-p
           #:positive-single-float
           #:positive-single-float-p

           #:negative-double-float
           #:negative-double-float-p
           #:non-positive-double-float
           #:non-positive-double-float-p
           #:non-negative-double-float
           #:non-negative-double-float-p
           #:positive-double-float
           #:positive-double-float-p

           #:negative-long-float
           #:negative-long-float-p
           #:non-positive-long-float
           #:non-positive-long-float-p
           #:non-negative-long-float
           #:non-negative-long-float-p
           #:positive-long-float
           #:positive-long-float-p

           #:array-index
           #:array-index-p)
  (:use #:cl))

(in-package #:cl-plus.src.cdr.cdr-5)




;;--------------------------------------------------------------------
;; FIXNUM Sub-Interval Types
;;--------------------------------------------------------------------

(declaim (inline negative-fixnum-p
                 non-positive-fixnum-p
                 non-negative-fixnum-p
                 positive-fixnum-p))

(deftype negative-fixnum     () `(integer ,most-negative-fixnum -1))
(deftype non-positive-fixnum () `(integer ,most-negative-fixnum 0))
(deftype non-negative-fixnum () `(integer 0 ,most-positive-fixnum))
(deftype positive-fixnum     () `(integer 1 ,most-positive-fixnum))

(defun negative-fixnum-p     (x) (typep x 'negative-fixnum))
(defun non-positive-fixnum-p (x) (typep x 'non-positive-fixnum))
(defun non-negative-fixnum-p (x) (typep x 'non-negative-fixnum))
(defun positive-fixnum-p     (x) (typep x 'positive-fixnum))


;;--------------------------------------------------------------------
;; INTEGER Sub-Interval Types
;;--------------------------------------------------------------------

(declaim (inline negative-integer-p
                 non-positive-integer-p
                 non-negative-integer-p
                 positive-integer-p))

(deftype negative-integer     () `(integer * -1))
(deftype non-positive-integer () `(integer * 0))
(deftype non-negative-integer () `(integer 0 *))
(deftype positive-integer     () `(integer 1 *))

(defun negative-integer-p     (x) (typep x 'negative-integer))
(defun non-positive-integer-p (x) (typep x 'non-positive-integer))
(defun non-negative-integer-p (x) (typep x 'non-negative-integer))
(defun positive-integer-p     (x) (typep x 'positive-integer))


;;--------------------------------------------------------------------
;; RATIONAL Sub-Interval Types
;;--------------------------------------------------------------------

(declaim (inline negative-rational-p
                 non-positive-rational-p
                 non-negative-rational-p
                 positive-rational-p))

(deftype negative-rational     () '(rational * (0)))
(deftype non-positive-rational () '(rational * 0))
(deftype non-negative-rational () '(rational 0 *))
(deftype positive-rational     () '(rational (0) *))

(defun negative-rational-p     (x) (typep x 'negative-rational))
(defun non-positive-rational-p (x) (typep x 'non-positive-rational))
(defun non-negative-rational-p (x) (typep x 'non-negative-rational))
(defun positive-rational-p     (x) (typep x 'positive-rational))


;;--------------------------------------------------------------------
;; RATIO Sub-Interval Types
;;--------------------------------------------------------------------

(declaim (inline ratiop
                 ratio-plusp
                 ratio-minusp
                 negative-ratio-p
                 non-positive-ratio-p
                 non-negative-ratio-p
                 positive-ratio-p))

(defun ratiop       (x) (and (rationalp x) (> (denominator x) 1)))
(defun ratio-plusp  (x) (and (ratiop x) (plusp x)))
(defun ratio-minusp (x) (and (ratiop x) (minusp x)))

(deftype negative-ratio     () '(satisfies ratio-minusp))
(deftype non-positive-ratio () 'negative-ratio)
(deftype non-negative-ratio () 'positive-ratio)
(deftype positive-ratio     () '(satisfies ratio-plusp))

(defun negative-ratio-p     (x) (typep x 'negative-ratio))
(defun non-positive-ratio-p (x) (typep x 'non-positive-ratio))
(defun non-negative-ratio-p (x) (typep x 'non-negative-ratio))
(defun positive-ratio-p     (x) (typep x 'positive-ratio))


;;--------------------------------------------------------------------
;; REAL Sub-Interval Types
;;--------------------------------------------------------------------

(declaim (inline negative-real-p
                 non-positive-real-p
                 non-negative-real-p
                 positive-real-p))

(deftype negative-real     () '(real * (0)))
(deftype non-positive-real () '(real * 0))
(deftype non-negative-real () '(real 0 *))
(deftype positive-real     () '(real (0) *))

(defun negative-real-p     (x) (typep x 'negative-real))
(defun non-positive-real-p (x) (typep x 'non-positive-real))
(defun non-negative-real-p (x) (typep x 'non-negative-real))
(defun positive-real-p     (x) (typep x 'positive-real))


;;--------------------------------------------------------------------
;; FLOAT Sub-Interval Types
;;--------------------------------------------------------------------
;; float, short-float, single-float, double-float, and long-float.

(declaim (inline negative-float-p
                 non-positive-float-p
                 non-negative-float-p
                 positive-float-p

                 negative-short-float-p
                 non-positive-short-float-p
                 non-negative-short-float-p
                 positive-short-float-p

                 negative-single-float-p
                 non-positive-single-float-p
                 non-negative-single-float-p
                 positive-single-float-p

                 negative-double-float-p
                 non-positive-double-float-p
                 non-negative-double-float-p
                 positive-double-float-p

                 negative-long-float-p
                 non-positive-long-float-p
                 non-negative-long-float-p
                 positive-long-float-p))

;; float
(deftype negative-float     () '(float * (0.0E0)))
(deftype non-positive-float () '(float * 0.0E0))
(deftype non-negative-float () '(float 0.0E0 *))
(deftype positive-float     () '(float (0.0E0) *))

(defun negative-float-p     (x) (typep x 'negative-float))
(defun non-positive-float-p (x) (typep x 'non-positive-float))
(defun non-negative-float-p (x) (typep x 'non-negative-float))
(defun positive-float-p     (x) (typep x 'positive-float))

;; short-float
(deftype negative-short-float     () '(short-float * (0.0S0)))
(deftype non-positive-short-float () '(short-float * 0.0S0))
(deftype non-negative-short-float () '(short-float 0.0S0 *))
(deftype positive-short-float     () '(short-float (0.0S0) *))

(defun negative-short-float-p     (x) (typep x 'negative-short-float))
(defun non-positive-short-float-p (x) (typep x 'non-positive-short-float))
(defun non-negative-short-float-p (x) (typep x 'non-negative-short-float))
(defun positive-short-float-p     (x) (typep x 'positive-short-float))

;; single-float
(deftype negative-single-float     () '(single-float * (0.0F0)))
(deftype non-positive-single-float () '(single-float * 0.0F0))
(deftype non-negative-single-float () '(single-float 0.0F0 *))
(deftype positive-single-float     () '(single-float (0.0F0) *))

(defun negative-single-float-p     (x) (typep x 'negative-single-float))
(defun non-positive-single-float-p (x) (typep x 'non-positive-single-float))
(defun non-negative-single-float-p (x) (typep x 'non-negative-single-float))
(defun positive-single-float-p     (x) (typep x 'positive-single-float))

;; double-float
(deftype negative-double-float     () '(double-float * (0.0D0)))
(deftype non-positive-double-float () '(double-float * 0.0D0))
(deftype non-negative-double-float () '(double-float 0.0D0 *))
(deftype positive-double-float     () '(double-float (0.0D0) *))

(defun negative-double-float-p     (x) (typep x 'negative-double-float))
(defun non-positive-double-float-p (x) (typep x 'non-positive-double-float))
(defun non-negative-double-float-p (x) (typep x 'non-negative-double-float))
(defun positive-double-float-p     (x) (typep x 'positive-double-float))

;; long-float
(deftype negative-long-float     () '(long-float * (0.0L0)))
(deftype non-positive-long-float () '(long-float * 0.0L0))
(deftype non-negative-long-float () '(long-float 0.0L0 *))
(deftype positive-long-float     () '(long-float (0.0L0) *))

(defun negative-long-float-p     (x) (typep x 'negative-long-float))
(defun non-positive-long-float-p (x) (typep x 'non-positive-long-float))
(defun non-negative-long-float-p (x) (typep x 'non-negative-long-float))
(defun positive-long-float-p     (x) (typep x 'positive-long-float))


;;--------------------------------------------------------------------
;; ARRAY-INDEX Sub-Interval Type
;;--------------------------------------------------------------------

(declaim (inline array-index-p))

;; CDR-5, original
;; (deftype array-index () `(integer 0 (,array-dimension-limit)))

;; From alexandria
(deftype array-index (&optional (length array-dimension-limit))
  "Type designator for an index into array of LENGTH: an integer
between 0 (inclusive) and LENGTH (exclusive). LENGTH defaults to
ARRAY-DIMENSION-LIMIT."
  `(integer 0 (,length)))

(defun array-index-p (x) (typep x 'array-index))


;;====================================================================
