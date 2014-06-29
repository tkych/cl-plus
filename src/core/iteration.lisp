;;;; Last modified: 2014-06-29 10:05:12 tkych

;; cl-plus/src/core/iteration.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Iteration
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.core.iteration
  (:documentation
   "
Iteration
=========

")
  (:nicknames #:cl+iteration)
  (:export #:enable-loop-unrolling
           #:disable-loop-unrolling
           #:dolist2)
  (:use #:cl)
  (:import-from #:alexandria
                #:once-only
                #:with-gensyms
                #:parse-body))

(in-package #:cl-plus.src.core.iteration)

;;--------------------------------------------------------------------
;; Loop Unrolling (((EXPERIMANTAL)))
;;--------------------------------------------------------------------
;; cf. Tomohiro Matsuyama's blog, http://cx4a.blogspot.com/2011_11_01_archive.html

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type (or null (integer 1 *)) *loop-unrolling-threshold*))
  (defvar *loop-unrolling-threshold* nil "
Threshold for loop unrolling. Default is `NIL'.
"))

(defmacro enable-loop-unrolling (threshold)
  (once-only (threshold)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (check-type ,threshold (integer 0 *))
       (setf *loop-unrolling-threshold* ,threshold))))

(setf (documentation 'enable-loop-unrolling 'function) "
   (((EXPERIMANTAL)))
")

(defmacro disable-loop-unrolling ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *loop-unrolling-threshold* nil)))

(setf (documentation 'disable-loop-unrolling 'function) "
   (((EXPERIMANTAL)))
")


;;--------------------------------------------------------------------
;; dolist2
;;--------------------------------------------------------------------
;; TODO: loop-unrolling

(defmacro dolist2 ((index value list &optional result) &body body)
  (multiple-value-bind (tag-statements declarations) (parse-body body)
    (with-gensyms (i tail LOOP)
      (once-only (list)
        `(block nil
           (let ((,i 0)
                 (,tail ,list))
             (declare (type (integer 0 *) ,i)
                      (type list ,list))
             (tagbody ,LOOP
                (unless (endp ,tail)
                  (let ((,index ,i)
                        (,value (first ,tail)))
                    ,@declarations
                    (tagbody ,@tag-statements))
                  (incf ,i)
                  (setf ,tail (rest ,tail))
                  (GO ,LOOP))))
           ,(when result
              `(let (,index ,value)
                 (declare (ignorable ,index ,value))
                 ,result)))))))

(setf (documentation 'dohash 'function) "
DOLIST2 (index value array &optional result) &body body => result
")


;;====================================================================
