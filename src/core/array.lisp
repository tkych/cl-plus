;;;; Last modified: 2014-06-29 10:04:03 tkych

;; cl-plus/src/core/array.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Array
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.core.array
  (:documentation "
Array
=====

 - array-subscripts
 - doary
 - doary2
")
  (:export #:array-subscripts
           #:doary
           #:doary2)
  (:nicknames #:cl+array)
  (:use #:cl)
  (:import-from #:alexandria
                #:once-only
                #:with-gensyms
                #:parse-body)
  (:import-from #:cl-plus.src.cdr.cdr-5
                #:array-index))

(in-package #:cl-plus.src.core.array)


;;--------------------------------------------------------------------
;; array-subscripts
;;--------------------------------------------------------------------

(defun array-subscripts (array row-major-index)
  (check-type array           array)
  (check-type row-major-index array-index)
  (if (<= (array-total-size array) row-major-index)
      (error "Row-major-index ~S out of bounds for ~S."
             row-major-index (type-of array))
      ;; Ugly!
      (let ((subscripts '()))
        (declare (type list subscripts))
        (loop :for d :of-type array-index
                :in (maplist (lambda (dims) (reduce #'* dims))
                             (rest (array-dimensions array)))
              :with (s i) := `(nil ,row-major-index)
              :do (multiple-value-setq (s i) (floor i d))
                  (push s subscripts)
              :finally (push i subscripts))
        (nreverse subscripts))))

(setf (documentation 'array-subscripts 'function) "
ARRAY-SUBSCRIPTS array row-major-index => subscripts

Note
----
 - ARRAY-SUBSCRIPTS is the duality function for ARRAY-ROW-MAJOR-INDEX.
   For example,
   * (apply #'array-row-major-index
            array (array-subscripts array index)) => index

   * (array-subscripts
       array (apply #'array-row-major-index array subscripts)) => subscripts
")


;;--------------------------------------------------------------------
;; doary
;;--------------------------------------------------------------------

(defmacro doary ((var array &optional result) &body body)
  (multiple-value-bind (tag-statements declarations) (parse-body body)
    (with-gensyms (size i LOOP)
      (once-only (array)
        `(block nil
           (let ((,size (array-total-size ,array))
                 (,i 0))
             (declare (type array-index ,size ,i))
             (tagbody ,LOOP
                (unless (<= ,size ,i)
                  (let ((,var (row-major-aref ,array ,i)))
                    ,@declarations
                    (tagbody ,@tag-statements))
                  (incf ,i)
                  (GO ,LOOP))))
           ,(when result
              `(let (,var)
                 (declare (ignorable ,var))
                 ,result)))))))

(setf (documentation 'dohash 'function) "
DOARY (var array &optional result) &body body => result
")

(define-compiler-macro doary (&whole form (var array &optional result) &body body)
  (if (and (integerp cl-plus.src.core.iteration::*loop-unrolling-threshold*)
           (arrayp array)
           (<= (array-total-size array)
               cl-plus.src.core.iteration::*loop-unrolling-threshold*))
      `(block nil
         ,@(loop :for i :from 0 :below (array-total-size array)
                 :collect `(let ((,var ,(row-major-aref array i))) ,@body))
         ,(when result
            `(let (,var)
               (declare (ignorable ,var))
               ,result)))
      form))


;;--------------------------------------------------------------------
;; doary2
;;--------------------------------------------------------------------
;; TODO: loop-unrolling
;;   cl-plus.src.core.iteration::*loop-unrolling-threshold*

(defmacro doary2 ((index value array &optional result) &body body)
  (multiple-value-bind (tag-statements declarations) (parse-body body)
    (with-gensyms (size i LOOP)
      (once-only (array)
        `(block nil
           (let ((,size (array-total-size ,array))
                 (,i 0))
             (declare (type array-index ,size ,i))
             (tagbody ,LOOP
                (unless (<= ,size ,i)
                  (let ((,index ,i)
                        (,value (row-major-aref ,array ,i)))
                    ,@declarations
                    (tagbody ,@tag-statements))
                  (incf ,i)
                  (GO ,LOOP))))
           ,(when result
              `(let (,value)
                 (declare (ignorable ,value))
                 ,result)))))))


(setf (documentation 'dohash 'function) "
DOARY2 (index value array &optional result) &body body => result
")


;;====================================================================
