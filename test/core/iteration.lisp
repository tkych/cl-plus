;;;; cl-plus/test/core/iteration.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for Iteration 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.iteration
  (:export #:?iteration)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:all)
  (:import-from #:cl-plus.src.core.iteration
                #:enable-loop-unrolling
                #:disable-loop-unrolling
                #:dolist2))

(in-package #:cl-plus.test.core.iteration)

(def-suite ?iteration :in all)
(in-suite ?iteration)

;;--------------------------------------------------------------------
;; dolist2
;;--------------------------------------------------------------------

;; TODO:
;; ECL:
;; Running test ?DOLIST2 
;;; OPTIMIZE levels: Safety=0, Space=0, Speed=3, Debug=0
;;;
;;; Warning:
;;;   ! Declaration of type
;;; LIST
;;; was found for not bound variable #:LIST11.
;;; Warning:
;;;   ! Declaration of type
;;; LIST
;;; was found for not bound variable #:LIST36.
;;; End of Pass 1...


(test ?dolist2
  (is (= (reduce #'+ '(1 2 3 4 5))
         (let ((result 0))
           (dolist2 (i v '(1 2 3 4 5))
             (declare (ignore i))
             (incf result v))
           result)))
  (is (= (reduce #'+ (subseq '(1 2 3 4 5) 0 3))
         (let ((result 0))
           (dolist2 (i v '(1 2 3 4 5))
             (if (>= i 3)
                 (return)
                 (incf result v)))
           result))))


;;====================================================================
