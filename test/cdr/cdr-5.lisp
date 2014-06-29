;;;; Last modified: 2014-06-29 10:24:33 tkych

;; cl-plus/test/cdr/cdr-5.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for CDR-5 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.cdr.cdr-5
  (:export #:?cdr-5)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:all)
  (:import-from #:cl-plus.src.cdr.cdr-5
                #:negative-fixnum
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
                #:array-index-p))

(in-package #:cl-plus.test.cdr.cdr-5)

(def-suite ?cdr-5 :in all)
(in-suite ?cdr-5)


;;--------------------------------------------------------------------
;; FIXNUM Sub-Interval Types
;;--------------------------------------------------------------------


;;--------------------------------------------------------------------
;; INTEGER Sub-Interval Types
;;--------------------------------------------------------------------

;;--------------------------------------------------------------------
;; RATIONAL Sub-Interval Types
;;--------------------------------------------------------------------


;;--------------------------------------------------------------------
;; RATIO Sub-interval Types
;;--------------------------------------------------------------------

;;--------------------------------------------------------------------
;; REAL Sub-Interval Types
;;--------------------------------------------------------------------


;;--------------------------------------------------------------------
;; FLOAT Sub-Interval Types
;;--------------------------------------------------------------------
;; float, short-float, single-float, double-float, and long-float.



;;--------------------------------------------------------------------
;; ARRAY-INDEX Sub-Interval Type
;;--------------------------------------------------------------------


(test ?array-index
  (for-all ((i (gen-integer :min 0 :max array-total-size-limit)))
    (is-true (typep i 'array-index)))
  (for-all ((i (gen-integer :max -1)))
    (is-false (typep i 'array-index))))


;;====================================================================
