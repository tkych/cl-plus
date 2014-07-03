;;;; cl-plus/test/core/array.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for Array 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.array
  (:export #:?array)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:all
                #:is-=
                #:is-eql
                #:is-equal
                #:is-equalp)
  (:import-from #:cl-plus.src.core.array
                #:array-subscripts
                #:doary
                #:doary2))

(in-package #:cl-plus.test.core.array)

(def-suite ?array :in all)
(in-suite ?array)

;;--------------------------------------------------------------------
;; array-subscripts
;;--------------------------------------------------------------------

(test ?array-subscripts
  (signals type-error (array-subscripts '(0 1 2 3) 1))
  (signals type-error (array-subscripts #(0 1 2 3) -1))
  (let* ((dim 9)
         (ary (make-array (list dim dim dim))))
    (for-all ((i (gen-integer :min 0 :max (1- dim)))
              (j (gen-integer :min 0 :max (1- dim)))
              (k (gen-integer :min 0 :max (1- dim))))
      (is (equal (list i j k)
                 (array-subscripts ary (array-row-major-index ary i j k)))))
    (for-all ((i (gen-integer :min 0 :max (1- (array-total-size ary)))))
      (is (= i (apply #'array-row-major-index ary
                      (array-subscripts ary i)))))))


;;--------------------------------------------------------------------
;; doary
;;--------------------------------------------------------------------

(defparameter ary  #(1 2 3 4 5))
(defparameter ary2 #2A(( 1  2  3  4  5)
                       (11 12 13 14 15)))
(defparameter ary3 #3A((( 1  2  3) ( 4  5  6) ( 7  8  9))
                       ((10 11 12) (13 14 15) (16 17 18))
                       ((19 20 21) (22 23 24) (25 26 27))))

(test ?doary
  (is (= (reduce #'+ ary)
         (let ((result 0))
           (doary (v ary)
             (incf result v))
           result)))

  (is (= (reduce #'+ #(1 2 3))
         (let ((result 0))
           (doary (v #(1 2 3 4 5))
             (if (> v 3)
                 (return)
                 (incf result v)))
           result)))

  (is (= (loop :for i :from 0 :below (array-dimension ary2 0)
               :sum (loop :for j :from 0 :below (array-dimension ary2 1)
                          :sum (aref ary2 i j)))
         (let ((result 0))
           (doary (v ary2)
             (incf result v))
           result)))
  
  (is (= (loop :for i :from 0 :below (array-dimension ary2 0)
               :sum (loop :for j :from 0 :below (array-dimension ary2 1)
                          :if (< 11 (aref ary2 i j))
                            :do (loop-finish)
                          :else :sum (aref ary2 i j)))
         (let ((result 0))
           (doary (v ary2)
             (if (< 11 v)
                 (return)
                 (incf result v)))
           result)))

  (is (= (loop :for i :from 0 :below (array-dimension ary3 0)
               :sum (loop :for j :from 0 :below (array-dimension ary3 1)
                          :sum (loop :for k :from 0 :below (array-dimension ary3 2)
                                     :sum (aref ary3 i j k))))
         (let ((result 0))
           (doary (v ary3)
             (incf result v))
           result)))

  (is (= (loop :for i :from 0 :below (array-dimension ary3 0)
               :sum (loop :for j :from 0 :below (array-dimension ary3 1)
                          :sum (loop :for k :from 0 :below (array-dimension ary3 2)
                                     :if (< 15 (aref ary3 i j k))
                                       :do (loop-finish)
                                     :else :sum (aref ary3 i j k))))
         (let ((result 0))
           (doary (v ary3)
             (if (< 15 v)
                 (return)
                 (incf result v)))
           result)))
  )


;;--------------------------------------------------------------------
;; doary2
;;--------------------------------------------------------------------

(test ?doary2
  (is (= (reduce #'+ ary)
         (let ((result 0))
           (doary2 (i v ary)
             (declare (ignore i))
             (incf result v))
           result)))

  (is (= (reduce #'+ (subseq ary 0 3))
         (let ((result 0))
           (doary2 (i v ary)
             (if (<= 3 i)
                 (return)
                 (incf result v)))
           result)))

  (is (= (loop :for i :from 0 :below (array-dimension ary2 0)
               :sum (loop :for j :from 0 :below (array-dimension ary2 1)
                          :sum (aref ary2 i j)))
         (let ((result 0))
           (doary2 (i v ary2)
             (declare (ignore i))
             (incf result v))
           result)))

  (is (= (loop :for i :from 0 :below (array-dimension ary2 0)
               :sum (loop :for j :from 0 :below (array-dimension ary2 1)
                          :if (<= 11 (array-row-major-index ary2 i j))
                            :do (loop-finish)
                          :else :sum (aref ary2 i j)))
         (let ((result 0))
           (doary2 (i v ary2)
             (if (<= 11 i)
                 (return)
                 (incf result v)))
           result)))

  (is (= (loop :for i :from 0 :below (array-dimension ary3 0)
               :sum (loop :for j :from 0 :below (array-dimension ary3 1)
                          :sum (loop :for k :from 0 :below (array-dimension ary3 2)
                                     :sum (aref ary3 i j k))))
         (let ((result 0))
           (doary2 (i v ary3)
             (declare (ignore i))
             (incf result v))
           result)))

  (is (= (loop :for i :from 0 :below (array-dimension ary3 0)
               :sum (loop :for j :from 0 :below (array-dimension ary3 1)
                          :sum (loop :for k :from 0 :below (array-dimension ary3 2)
                                     :if (<= 15 (array-row-major-index ary3 i j k))
                                       :do (loop-finish)
                                     :else :sum (aref ary3 i j k))))
         (let ((result 0))
           (doary2 (i v ary3)
             (if (<= 15 i)
                 (return)
                 (incf result v)))
           result)))
  )


;;====================================================================
