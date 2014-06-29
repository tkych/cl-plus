;;;; Last modified: 2014-06-29 10:27:47 tkych

;; cl-plus/test/srfi/srfi-19.lisp

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
;; Test for SRFI-19
;;====================================================================

(in-package :cl-user)

(defpackage #:cl-plus.test.srfi.srfi-19
  (:documentation "
Test for SRFI-19
================

References
----------

 [0] Will Fitzgerald (2004),
     Scheme Request for Implementation 19: Time Data Types and Procedures.
     http://srfi.schemers.org/srfi-19
     http://srfi.schemers.org/srfi-19/srfi-19.scm
     http://srfi.schemers.org/srfi-19/srfi-19-test-suite.scm

 [1] g000001, srfi-19.
     https://github.com/g000001/srfi-19
     (scheme style common lisp translation)
")
  (:export #:?srfi-19)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:all)
  (:import-from #:cl-plus.src.srfi.srfi-19
                ;; Current time and clock resolution
                #:current-date
                #:current-julian-day
                #:current-modified-julian-day
                #:current-time
                #:time-resolution

                ;; Time object and accessors
                #:make-time
                #:timep
                #:time-type
                #:time-nanosecond
                #:time-second
                #:copy-time
  
                ;; Time comparison procedures
                #:time<=
                #:time<
                #:time=
                #:time>=
                #:time>

                ;; Time arithmetic procedures
                #:time-difference
                #:ntime-difference
                #:add-duration
                #:nadd-duration
                #:subtract-duration
                #:nsubtract-duration

                ;; Date object and accessors
                #:make-date
                #:datep
                #:date-nanosecond
                #:date-second
                #:date-minute
                #:date-hour
                #:date-day
                #:date-month
                #:date-year
                #:date-zone-offset
                #:date-year-day
                #:date-week-day
                #:date-week-number

                ;; Time/Date/Julian Day/Modified Julian Day Converters   
                #:date->julian-day
                #:date->modified-julian-day
                #:date->time-monotonic
                #:date->time-tai
                #:date->time-utc
                #:julian-day->date
                #:julian-day->time-monotonic
                #:julian-day->time-tai
                #:julian-day->time-utc
                #:modified-julian-day->date
                #:modified-julian-day->time-monotonic
                #:modified-julian-day->time-tai
                #:modified-julian-day->time-utc
                #:time-monotonic->date
                #:time-monotonic->julian-day
                #:time-monotonic->modified-julian-day
                #:time-monotonic->time-tai
                #:ntime-monotonic->time-tai
                #:time-monotonic->time-utc
                #:ntime-monotonic->time-utc
                #:time-tai->date
                #:time-tai->julian-day
                #:time-tai->modified-julian-day
                #:time-tai->time-monotonic
                #:ntime-tai->time-monotonic
                #:time-tai->time-utc
                #:ntime-tai->time-utc
                #:time-utc->date
                #:time-utc->julian-day
                #:time-utc->modified-julian-day
                #:time-utc->time-monotonic
                #:ntime-utc->time-monotonic
                #:time-utc->time-tai
                #:ntime-utc->time-tai
   
                ;; Date to String/String to Date Converters
                #:date->string
                #:string->date))

(in-package #:cl-plus.test.srfi.srfi-19)

(def-suite ?srfi-19 :in all)
(in-suite ?srfi-19)


;;--------------------------------------------------------------------
;; Feature
;;--------------------------------------------------------------------

(test ?srfi-19-feature
  (is-true (find :srfi-19 *features*)))


;;--------------------------------------------------------------------
;; Creating time structures
;;--------------------------------------------------------------------

(test ?time-stractures
  (is-true (timep (current-time :time-tai)))
  (is-true (timep (current-time :time-utc)))
  (is-true (timep (current-time :time-monotonic)))
  (is-true (timep (current-time :time-thread)))
  (is-true (timep (current-time :time-process))))


;;--------------------------------------------------------------------
;; Testing time resolutions
;;--------------------------------------------------------------------

(test ?time-resolution
  (is-true (time-resolution :time-tai))
  (is-true (time-resolution :time-utc))
  (is-true (time-resolution :time-monotonic))
  (is-true (time-resolution :time-thread))
  (is-true (time-resolution :time-process)))


;;--------------------------------------------------------------------
;; Time comparisons (time=, etc.)
;;--------------------------------------------------------------------

(test ?time-comparsion
 (let ((t1  (make-time :time-utc 0 1))
       (t2  (make-time :time-utc 0 1))
       (t3  (make-time :time-utc 0 2))
       (t11 (make-time :time-utc 1001 1))
       (t12 (make-time :time-utc 1001 1))
       (t13 (make-time :time-utc 1001 2)))
   (is-true (time=  t1 t2))
   (is-true (time>  t3 t2))
   (is-true (time<  t2 t3))
   (is-true (time>= t1 t2))
   (is-true (time>= t3 t2))
   (is-true (time<= t1 t2))
   (is-true (time<= t2 t3))
   (is-true (time=  t11 t12))
   (is-true (time>  t13 t12))
   (is-true (time<  t12 t13))
   (is-true (time>= t11 t12))
   (is-true (time>= t13 t12))
   (is-true (time<= t11 t12))
   (is-true (time<= t12 t13))))


;;--------------------------------------------------------------------
;; Time difference
;;--------------------------------------------------------------------

(test ?time-difference
  (let ((t1 (make-time :time-utc      0  3000))
        (t2 (make-time :time-utc      0  1000))
        (t3 (make-time :time-duration 0  2000))
        (t4 (make-time :time-duration 0 -2000)))
    (is-true (time= t3 (time-difference t1 t2)))
    (is-true (time= t4 (time-difference t2 t1)))))


;;--------------------------------------------------------------------
;; TAI <-> UTC Conversions
;;--------------------------------------------------------------------

(defun test-one-utc-tai-edge (utc tai-diff tai-last-diff)
  (let* (;; right on the edge they should be the same
         (utc-basic (make-time :time-utc 0 utc))
         (tai-basic (make-time :time-tai 0 (+ utc tai-diff)))
         (utc->tai-basic (time-utc->time-tai utc-basic))
         (tai->utc-basic (time-tai->time-utc tai-basic))
         ;; a second before they should be the old diff
         (utc-basic-1 (make-time :time-utc 0 (- utc 1)))
         (tai-basic-1 (make-time :time-tai 0 (- (+ utc tai-last-diff) 1)))
         (utc->tai-basic-1 (time-utc->time-tai utc-basic-1))
         (tai->utc-basic-1 (time-tai->time-utc tai-basic-1))
         ;; a second later they should be the new diff
         (utc-basic+1 (make-time :time-utc 0 (+ utc 1)))
         (tai-basic+1 (make-time :time-tai 0 (+ (+ utc tai-diff) 1)))
         (utc->tai-basic+1 (time-utc->time-tai utc-basic+1))
         (tai->utc-basic+1 (time-tai->time-utc tai-basic+1))
         ;; ok, let:s move the clock half a month or so plus half a second
         (shy (* 15 24 60 60))
         (hs (/ (expt 10 9) 2))
         ;; a second later they should be the new diff
         (utc-basic+2 (make-time :time-utc hs (+ utc shy)))
         (tai-basic+2 (make-time :time-tai hs (+ (+ utc tai-diff) shy)))
         (utc->tai-basic+2 (time-utc->time-tai utc-basic+2))
         (tai->utc-basic+2 (time-tai->time-utc tai-basic+2)))
    (and (time= utc-basic tai->utc-basic)
         (time= tai-basic utc->tai-basic)
         (time= utc-basic-1 tai->utc-basic-1)
         (time= tai-basic-1 utc->tai-basic-1)
         (time= utc-basic+1 tai->utc-basic+1)
         (time= tai-basic+1 utc->tai-basic+1)
         (time= utc-basic+2 tai->utc-basic+2)
         (time= tai-basic+2 utc->tai-basic+2))))

(test ?tai<->utc-conversions
  (is-true (test-one-utc-tai-edge 915148800  32 31))
  (is-true (test-one-utc-tai-edge 867715200  31 30))
  (is-true (test-one-utc-tai-edge 820454400  30 29))
  (is-true (test-one-utc-tai-edge 773020800  29 28))
  (is-true (test-one-utc-tai-edge 741484800  28 27))
  (is-true (test-one-utc-tai-edge 709948800  27 26))
  (is-true (test-one-utc-tai-edge 662688000  26 25))
  (is-true (test-one-utc-tai-edge 631152000  25 24))
  (is-true (test-one-utc-tai-edge 567993600  24 23))
  (is-true (test-one-utc-tai-edge 489024000  23 22))
  (is-true (test-one-utc-tai-edge 425865600  22 21))
  (is-true (test-one-utc-tai-edge 394329600  21 20))
  (is-true (test-one-utc-tai-edge 362793600  20 19))
  (is-true (test-one-utc-tai-edge 315532800  19 18))
  (is-true (test-one-utc-tai-edge 283996800  18 17))
  (is-true (test-one-utc-tai-edge 252460800  17 16))
  (is-true (test-one-utc-tai-edge 220924800  16 15))
  (is-true (test-one-utc-tai-edge 189302400  15 14))
  (is-true (test-one-utc-tai-edge 157766400  14 13))
  (is-true (test-one-utc-tai-edge 126230400  13 12))
  (is-true (test-one-utc-tai-edge  94694400  12 11))
  (is-true (test-one-utc-tai-edge  78796800  11 10))
  (is-true (test-one-utc-tai-edge  63072000  10  0))
  (is-true (test-one-utc-tai-edge         0   0  0)) ; at the epoch
  (is-true (test-one-utc-tai-edge        10   0  0)) ; close to it ...
  (is-true (test-one-utc-tai-edge 1045789645 32 32)) ; about now ...
  )

;;--------------------------------------------------------------------
;; TAI <-> Date Conversions
;;--------------------------------------------------------------------

(defun date== (d1 d2)
  (and (= (date-year d1) (date-year d2))
       (= (date-month d1) (date-month d2))
       (= (date-day d1) (date-day d2))
       (= (date-hour d1) (date-hour d2))
       (= (date-second d1) (date-second d2))
       (= (date-nanosecond d1) (date-nanosecond d2))
       (= (date-zone-offset d1) (date-zone-offset d2))))


(test ?tai<->date-conversions
  (is-true (date== (time-tai->date (make-time time-tai 0 (+ 915148800 29)) 0)
                   (make-date 0 58 59 23 31 12 1998 0)))
  (is-true (date== (time-tai->date (make-time time-tai 0 (+ 915148800 30)) 0)
                   (make-date 0 59 59 23 31 12 1998 0)))
  (is-true (date== (time-tai->date (make-time time-tai 0 (+ 915148800 31)) 0)
                   (make-date 0 60 59 23 31 12 1998 0)))
  (is-true (date== (time-tai->date (make-time time-tai 0 (+ 915148800 32)) 0)
                   (make-date 0 0 0 0 1 1 1999 0))))


;;--------------------------------------------------------------------
;; Date <-> UTC Conversions
;;--------------------------------------------------------------------

(defun date<->utc-conversions
  (is-true (time= (make-time time-utc 0 (- 915148800 2))
                  (date->time-utc (make-date 0 58 59 23 31 12 1998 0))))
  (is-true (time= (make-time time-utc 0 (- 915148800 1))
                  (date->time-utc (make-date 0 59 59 23 31 12 1998 0))))
  ;; yes, I think this is acutally right.
  (is-true (time= (make-time time-utc 0 (- 915148800 0))
                  (date->time-utc (make-date 0 60 59 23 31 12 1998 0))))
  (is-true (time= (make-time time-utc 0 (- 915148800 0))
                  (date->time-utc (make-date 0 0 0 0 1 1 1999 0))))
  (is-true (time= (make-time time-utc 0 (+ 915148800 1))
                  (date->time-utc (make-date 0 1 0 0 1 1 1999 0)))))


;;--------------------------------------------------------------------
;; TZ Offset conversions
;;--------------------------------------------------------------------

(test tz<->offset-conversions
  (let ((ct-utc (make-time :time-utc 6320000 1045944859))
        (ct-tai (make-time :time-tai 6320000 1045944891))
        (cd     (make-date 6320000 19 14 15 22 2 2003 -18000)))
    (is-true (time= ct-utc (date->time-utc cd)))
    (is-true (time= ct-tai (date->time-tai cd)))))


;;====================================================================
