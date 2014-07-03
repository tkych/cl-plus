;;;; cl-plus/test/srfi/srfi-45.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;--------------------------------------------------------------------
;; Copyright (C) Philip L. Bewig (2007). All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;--------------------------------------------------------------------

;; The following code is a Common Lisp translation from reference
;; implementation of SRFI-41.

;; PASSING THE TESTS DOES NOT MEAN A CORRECT IMPLEMENTATION.

;;====================================================================
;; Test for SRFI-45
;;====================================================================

(in-package :cl-user)

(defpackage #:cl-plus.test.srfi.srfi-45
  (:documentation "
Test for SRFI-45
================


References
----------

 [0] André van Tonder.
     Scheme Request for Implementation 45:
     Primitives for Expressing Iterative Lazy Algorithms.
     http://srfi.schemers.org/srfi-45

 [1] g000001.
     srfi-41.
     https://github.com/g000001/srfi-45
     (Scheme style Common Lisp translation)
")
  (:export #:?srfi-45)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:all)
  #+ccl
  (:import-from #:trivial-timeout
                #:with-timeout
                #:timeout-error)
  #-ccl
  (:import-from #:bordeaux-threads
                #:with-timeout
                #:timeout)
  (:import-from #:alexandria
                #:once-only)
  (:import-from #:cl-plus.src.srfi.srfi-45
                #:promisep
                #:copy-promise
                #:lazy
                #:eager
                #:delay
                #:force))

(in-package #:cl-plus.test.srfi.srfi-45)

(def-suite ?srfi-45 :in all)
(in-suite ?srfi-45)


;;--------------------------------------------------------------------
;; Feature
;;--------------------------------------------------------------------

(test ?srfi-45-feature
  (is-true (find :srfi-45 *features*)))


;;====================================================================
;; TESTS AND BENCHMARKS:
;;====================================================================
;; PASSING THE TESTS DOES NOT MEAN A CORRECT IMPLEMENTATION.

;;--------------------------------------------------------------------
;; Memorization Tests
;;--------------------------------------------------------------------

(test ?memorization.1
  (let* ((out (make-string-output-stream))
         (s (delay (progn
                     (princ 'hello out)
                     1))))
    (unwind-protect
         (progn
           (is (equal "" (get-output-stream-string out)))
           (is (= 1 (force s)))
           (is (equal "HELLO" (get-output-stream-string out)))
           (is (= 1 (force s)))
           (is (equal "" (get-output-stream-string out))))
      (close out))))
;;===> Should display 'hello once


(test ?memorization.2
  (let* ((out (make-string-output-stream))
         (s (delay (progn
                     (princ 'bonjour out)
                     2))))
    (unwind-protect
         (progn
           (is (equal "" (get-output-stream-string out)))
           (is (= 4 (+ (force s) (force s))))
           (is (equal "BONJOUR" (get-output-stream-string out)))
           (is (= 2 (force s)))
           (is (equal "" (get-output-stream-string out))))
      (close out))))
;;===> Should display 'bonjour once


(test ?memorization.3
  "pointed out by Alejandro Forero Cuervo"
  (let* ((out (make-string-output-stream))
         (r (delay (progn
                     (princ 'hi out)
                     1)))
         (s (lazy r))
         (u (lazy s)))
    (unwind-protect
         (progn
           (is (equal "" (get-output-stream-string out)))
           (is (= 1 (force u)))
           (is (equal "HI" (get-output-stream-string out)))
           (is (= 1 (force r)))
           (is (equal "" (get-output-stream-string out))))
      (close out))))
;;===> Should display 'hi once


(test ?memorization.4
  "Stream (pipe) memoization"
  (let* ((out (make-string-output-stream)))
    (unwind-protect
         (labels ((pipe-drop (s index)
                    (lazy
                     (if (zerop index)
                         s
                         (pipe-drop (cdr (force s))
                                    (1- index)))))
                  (ones ()
                    (delay (progn
                             (princ 'ho out)
                             (cons 1 (ones))))))
           (let ((s (ones)))
             (is (equal "" (get-output-stream-string out)))
             (is (= 1 (car (force (pipe-drop s 4)))))
             (is (= 1 (car (force (pipe-drop s 4)))))
             (is (equal "HOHOHOHOHO" (get-output-stream-string out)))
             (is (equal "" (get-output-stream-string out)))))
      (close out))))
;;===> Should display 'ho five times


;;--------------------------------------------------------------------
;; Reentrancy Tests
;;--------------------------------------------------------------------

(test ?reentrancy.1
  "from R5RS"
  (let ((x 5) p (count 0))
    (setf p (delay (progn
                     (incf count)
                     (if (> count x)
                         count
                         (force p)))))
    (is (= 6 (force p)))    ;===>  6
    (setf x 10)
    (is (= 6 (force p)))))  ;===>  6


(test ?reentrancy.2
  "from SRFI 40"
  (let (f)
    (setf f (let ((first? t))
              (delay (if first?
                         (progn
                           (setf first? nil)
                           (force f))
                         'second))))
    (is (eq 'second (force f)))))
;;===> 'second


(test ?reentrancy.3
  "due to John Shutt"
  (let (p q)
    (setf q (let ((count 5))
              (labels ((get-count () count))
                (setf p (delay (if (<= count 0)
                                   count
                                   (progn
                                     (decf count)
                                     (force p)
                                     (incf count 2)
                                     count))))
                (list #'get-count p))))
    (destructuring-bind (get-count p) q
      (is (= 5 (funcall get-count)))     ; ===>   5
      (is (= 0 (force p)))               ; ===>   0
      (is (= 10 (funcall get-count)))))) ; ===>   10


;;--------------------------------------------------------------------
;; Test leaks:  All the leak tests should run in bounded space.
;;--------------------------------------------------------------------

;; The implementation idea of STACK-OVERFLOW-P is due to g000001.
;; cf. g000001, srfi-45, macro UNTIL-STACK-EXHAUSTED-OR-TIMEOUT
(defmacro stack-overflow-p (&body body)
  (declare (ignorable body))

  #+(or sbcl clozure abcl ecl)
  `(handler-case
       #-ccl (bordeaux-threads:with-timeout (1) ,@body)
       #+ccl (trivial-timeout:with-timeout (1) ,@body)
     
     #+sbcl (sb-kernel::control-stack-exhausted () t)
     #+ccl  (ccl::stack-overflow-condition      () t)
     #+abcl (storage-condition (c)
              (and (search "STACK OVERFLOW"
                           (slot-value c 'system::format-control) ;=> "Stack overflow."
                           :test 'string-equal)
                   t))
     #+ecl  (ext:stack-overflow () t)
     
     #-ccl (bordeaux-threads:timeout () nil)
     #+ccl (trivial-timeout:timeout-error () nil)
     (:no-error (&rest args) (declare (ignore args)) nil))
  
  #-(or sbcl clozure abcl ecl)
  `(progn
     (warn "Sorry, STACK-OVERFLOW-P doesn't work properly for your cl system.")
     t)
  )


(defun %loop () (lazy (%loop)))

(test ?leak.1
  "Infinite loop in bounded space."
  (is-false (stack-overflow-p (force (%loop)))))

(test ?leak.2
  "Pending memos should not accumulate in shared structures."
  (let ((s (%loop)))
   (is-false (stack-overflow-p (force s)))))

(defun from (n)
  (delay (cons n (from (+ n 1)))))

(defun traverse (s)
  (lazy (traverse (cdr (force s)))))

(test ?leak.3
  "Safely traversing infinite stream."
  (is-false (stack-overflow-p (force (traverse (from 0))))))

(test ?leak.4
  "Safely traversing infinite stream while pointer to head of result exists."
  (let ((s (traverse (from 0))))
    (is-false (stack-overflow-p (force s)))))


(defmacro match (exp &body clauses)
  "Convenient list deconstructor used below."
  (once-only (exp)
    `(cond ((null ,exp) ,(second (first clauses)))
           ((consp ,exp) (let ((,(car (first (second clauses)))
                                 (first ,exp))
                               (,(cdr (first (second clauses)))
                                 (rest ,exp)))
                           ,(second (second clauses))))
           (t 'match-error))))


(defun pipe-filter (pred p)
  (lazy (match (force p)
          (()            (delay '()))
          ((head . tail) (if (funcall pred head)
                             (delay (cons head (pipe-filter pred tail)))
                             (pipe-filter pred tail))))))

(test ?leak.5
  "Naive stream-filter should run in bounded space. Simplest case."
  (is-false (stack-overflow-p
              (force (pipe-filter (lambda (n) (= n 100000000000))
                                  (from 0))))))


(defun pipe-ref (p index)
  (lazy
   (match (force p)
     (()            'error)
     ((head . tail) (if (zerop index)
                        (delay head)
                        (pipe-ref tail (1- index)))))))

(test ?leak.6
  "Another long traversal should run in bounded space."
  ;; The pipe-ref procedure above does not strictly need to be lazy.
  ;; It is defined lazy for the purpose of testing safe compostion of
  ;; lazy procedures in the times3 benchmark below (previous
  ;; candidate solutions had failed this).

  ;; Check that evenness is correctly implemented - should terminate:
  (is (= 0 (force (pipe-ref (pipe-filter #'zerop (from 0))
                            0))))
  (let ((p (pipe-ref (from 0) 100000000)))
    (is-false (stack-overflow-p (force p)))))


(defun times3 (n)
  (pipe-ref (pipe-filter
             (lambda (x) (zerop (mod x n)))
             (from 0))
            3))

(test ?leak.7
  "Infamous example from SRFI 40."
  (is (= 21 (force (times3 7))))
  (is-false (stack-overflow-p (force (times3 100000000)))))


;;====================================================================
