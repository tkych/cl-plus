;;;; cl-plus/test/core/sequens.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for Sequens 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.sequens
  (:export #:?sequens)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:all
                #:is-eql
                #:is-equal
                #:is-equalp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:cl-plus.src.readtables
                #:cl+sharp-bracket)
  (:import-from #:cl-plus.src.core.sequens
                #:sequens
                #:sequensp
                #:seq->lseq
                #:doseq
                #:take   #:take-until   #:take-while
                #:drop   #:drop-until   #:drop-while
                #:zip    #:interleave   #:interpose)
  (:import-from #:cl-plus.src.srfi.srfi-41
                #:+empty-pipe+
                #:pipe)
  (:import-from #:cl-plus.src.core.lazy-sequence
                #:lazy-sequence-p
                #:make-lazy-seq
                #:induce
                #:make-lazy-flow
                #:induce-flow))

(in-package #:cl-plus.test.core.sequens)

(def-suite ?sequens :in all)
(in-suite ?sequens)

(in-readtable cl+sharp-bracket)

;;--------------------------------------------------------------------
;; sequens, sequensp
;;--------------------------------------------------------------------

(test ?sequens.type
  (is-true (typep (list 0 1 2)   'sequens))
  (is-true (typep (vector 0 1 2) 'sequens))
  (is-true (typep #*1010101010   'sequens))
  (is-true (typep "string"       'sequens))
  (is-true (typep (make-lazy-seq '() +empty-pipe+) 'sequens))
  (is-false (typep (make-hash-table) 'sequens))
  (is-false (typep :foo 'sequens))
  (is-false (typep 'foo 'sequens))
  (is-false (typep (pipe 0 1 2) 'sequens))
  (is-false (typep +empty-pipe+ 'sequens)))

(test ?sequensp
  (is-true (sequensp (list 0 1 2)))
  (is-true (sequensp (vector 0 1 2)))
  (is-true (sequensp #*1010101010))
  (is-true (sequensp "string"))
  (is-true (sequensp (make-lazy-seq '() +empty-pipe+)))
  (is-false (sequensp (make-hash-table)))
  (is-false (sequensp :foo))
  (is-false (sequensp 'foo))
  (is-false (sequensp (pipe 0 1 2)))
  (is-false (sequensp +empty-pipe+)))


;;--------------------------------------------------------------------
;; seq->lseq
;;--------------------------------------------------------------------

(test ?seq->lseq
  )


;;--------------------------------------------------------------------
;; doseq
;;--------------------------------------------------------------------
;; TODO:

(test ?doseq
  )


;;--------------------------------------------------------------------
;; take
;;--------------------------------------------------------------------
;; TODO:
;; :all

(test ?take.error
  (signals type-error (take :foo '(0 1 2)))
  (signals type-error (take 10 :foo)))


(test ?take.list
  (is-equal (take 0 '(0 1 2 3 4 5))
            '())
  (is-equal (take 3 '(0 1 2 3 4 5))
            '(0 1 2))
  (is-equal (take 30 '(0 1 2 3 4 5))
            '(0 1 2 3 4 5))
  (is-equal (take 3 '(0 1 2 3 4 5) 'sequens)
            '(0 1 2))
  (is-equal (take 3 '(0 1 2 3 4 5) 'sequence)
            '(0 1 2))
  (is-equal (take 3 '(0 1 2 3 4 5) 'list)
            '(0 1 2))
  (is-equalp (take 3 '(0 1 2 3 4 5) 'vector)
             #(0 1 2))
  (is-equal (take 2 '(0 1 2 3 4 5) 'bit-vector)
            #*01)
  (is-equal (take 2 '(#\0 #\1 #\2 #\3 #\4 #\5) 'string)
            "01")
  
  (for-all ((lst (gen-list))
            (len (gen-integer :min 0 :max 15)))
    (let ((end (min len (length lst))))
      (is-equal (take len lst)
                (subseq lst 0 end))
      (is-equal (take len lst 'sequens)
                (subseq lst 0 end))
      (is-equal (take len lst 'sequence)
                (subseq lst 0 end))
      (is-equal (take len lst 'list)
                (subseq lst 0 end))
      (is (equalp (take len lst 'vector)
                  (coerce (subseq lst 0 end) 'vector)))))
  
  (for-all ((str (gen-string))
            (len (gen-integer :min 0 :max 15)))
    (is-equal (take len (coerce str 'list) 'string)
              (subseq str 0 (min len (length str)))))
  
  (for-all ((bits (gen-list :elements (lambda () (random 2))))
            (len  (gen-integer :min 0 :max 15)))
    (is-equal (take len bits 'bit-vector)
              (coerce (subseq bits 0 (min len (length bits)))
                      'bit-vector))))


(test ?take.vector
  (is-equal (take 0 #(0 1 2 3 4 5))
            '())
  (is-equal (take 3 #(0 1 2 3 4 5))
            '(0 1 2))
  (is-equal (take 30 #(0 1 2 3 4 5))
            '(0 1 2 3 4 5))
  (is-equalp (take 3 #(0 1 2 3 4 5) 'sequens)
             #(0 1 2))
  (is-equalp (take 3 #(0 1 2 3 4 5) 'sequence)
             #(0 1 2))
  (is-equal (take 3 #(0 1 2 3 4 5) 'list)
            '(0 1 2))
  (is-equalp (take 3 #(0 1 2 3 4 5) 'vector)
             #(0 1 2))
  (is-equal (take 2 #(0 1 2 3 4 5) 'bit-vector)
            #*01)
  (is-equal (take 2 #(#\0 #\1 #\2 #\3 #\4 #\5) 'string)
            "01")
  
  (for-all ((lst (gen-list))
            (len (gen-integer :min 0 :max 15)))
    (let ((vec (coerce lst 'vector))
          (end (min len (length lst))))
      (is-equal (take len vec)
                (coerce (subseq vec 0 end) 'list))
      (is-equalp (take len vec 'sequens)
                 (subseq vec 0 end))
      (is-equalp (take len vec 'sequence)
                 (subseq vec 0 end))
      (is-equal (take len vec 'list)
                (coerce (subseq vec 0 end) 'list))
      (is-equalp (take len vec 'vector)
                 (coerce (subseq vec 0 end) 'vector))))
  
  (for-all ((str (gen-string))
            (len (gen-integer :min 0 :max 15)))
    (is-equal (take len (coerce str 'vector) 'string)
              (subseq str 0 (min len (length str)))))
  
  (for-all ((bits (gen-list :elements (lambda () (random 2))))
            (len  (gen-integer :min 0 :max 15)))
    (is-equal (take len (coerce bits 'vector) 'bit-vector)
              (coerce (subseq bits 0 (min len (length bits)))
                      'bit-vector))))


(test ?take.string
  (is-equal (take 0 "012345")
            '())
  (is-equal (take 3 "012345")
            '(#\0 #\1 #\2))
  (is-equal (take 30 "012345")
            '(#\0 #\1 #\2 #\3 #\4 #\5))
  (is-equal (take 2 "012345" 'sequens)
            "01")
  (is-equal (take 2 "012345" 'sequence)
            "01")
  (is-equal (take 3 "012345" 'list)
            '(#\0 #\1 #\2))
  (is-equalp (take 3 "012345" 'vector)
             #(#\0 #\1 #\2))
  (is-equal (take 2 "012345" 'string)
            "01")
  
  (for-all ((str (gen-string))
            (len (gen-integer :min 0 :max 15)))
    (let ((end (min len (length str))))
      (is-equal (take len str)
                (coerce (subseq str 0 end) 'list))
      (is-equal (take len str 'sequens)
                (subseq str 0 end))
      (is-equal (take len str 'sequence)
                (subseq str 0 end))
      (is-equal (take len str 'list)
                (coerce (subseq str 0 end) 'list))
      (is-equalp (take len str 'vector)
                 (coerce (subseq str 0 end) 'vector))
      (is-equal (take len str 'string)
                (subseq str 0 end)))))


(test ?take.bit-vector
  (is-equal (take 3 #*10101)
            '(1 0 1))
  (is-equal (take 0 #*10101)
            '())
  (is-equal (take 30 #*10101)
            '(1 0 1 0 1))
  (is-equal (take 3 #*10101 'sequens)
            #*101)
  (is-equal (take 3 #*10101 'sequence)
            #*101)
  (is-equal (take 3 #*10101 'list)
            '(1 0 1))
  (is-equalp (take 3 #*10101 'vector)
             #(1 0 1))
  (is-equalp (take 3 #*10101 'bit-vector)
             #*101)
  
  (for-all ((bits (gen-list :elements (lambda () (random 2))))
            (len (gen-integer :min 0 :max 15)))
    (let* ((bvec (coerce bits 'bit-vector))
           (end (min len (length bvec))))
      (is-equal (take len bvec)
                (coerce (subseq bvec 0 end) 'list))
      (is-equal (take len bvec 'sequens)
                (subseq bvec 0 end))
      (is-equal (take len bvec 'sequence)
                (subseq bvec 0 end))
      (is-equal (take len bvec 'list)
                (coerce (subseq bvec 0 end) 'list))
      (is-equalp (take len bvec 'vector)
                 (coerce (subseq bvec 0 end) 'vector))
      (is-equal (take len (coerce bvec 'vector) 'bit-vector)
                (coerce (subseq bvec 0 end) 'bit-vector)))))


(test ?take.lazy-sequence
  (is-equal (take 0 #[0 1 2 3 4 5])
            '())
  (is-equal (take 10 #[0 1 ..])
            '(0 1 2 3 4 5 6 7 8 9))
  (is-equal (take 30 #[0 1 2])
            '(0 1 2))
  (is-equal (take :all (take 3 #[0 1 2 3 4 5] 'sequens))
            '(0 1 2))
  (is-equal (take 3 #[0 1 2 3 4 5] 'list)
            '(0 1 2))
  (is-equalp (take 3 #[0 1 2 3 4 5] 'vector)
             #(0 1 2))
  (is-equal (take 2 #[0 1 2 3 4 5] 'bit-vector)
            #*01)
  (is-equal (take 2 #[#\0 #\1 #\2 #\3 #\4 #\5] 'string)
            "01")
  
  (for-all ((lst (gen-list))
            (len (gen-integer :min 0 :max 15)))
    (let ((lseq (make-lazy-seq lst))
          (end  (min len (length lst))))
      (is-equal (take len lseq)
                (subseq lst 0 end))
      (is-equal (take :all (take len lseq 'sequens))
                (subseq lst 0 end))
      (is-equal (take :all (take len lseq 'sequence))
                (subseq lst 0 end))
      (is-equal (take len lseq 'list)
                (subseq lst 0 end))
      (is-equalp (take len lseq 'vector)
                 (coerce (subseq lst 0 end) 'vector))))
  
  (for-all ((str (gen-string))
            (len (gen-integer :min 0 :max 15)))
    (is-equal (take len (make-lazy-seq (coerce str 'list)) 'string)
              (subseq str 0 (min len (length str)))))
  
  (for-all ((bits (gen-list :elements (lambda () (random 2))))
            (len  (gen-integer :min 0 :max 15)))
    (is-equal (take len (make-lazy-seq bits) 'bit-vector)
              (coerce (subseq bits 0 (min len (length bits)))
                      'bit-vector))))


;;--------------------------------------------------------------------
;; take-until
;;--------------------------------------------------------------------
;; TODO: sequence, sequens
(test ?take-until.list
  (is (equal (take-until #'plusp '(-3 -2 -1 0 1 2 3))
             '(-3 -2 -1 0)))
  (is (equal (take-until #'minusp '(-3 -2 -1 0 1 2 3))
             '()))
  (is (equal (take-until #'plusp '(-3 -2 -1 0 1 2 3) 'list)
             '(-3 -2 -1 0)))
  (is (equal (take-until #'minusp '(-3 -2 -1 0 1 2 3) 'list)
             '()))
  (is (equalp (take-until #'plusp '(-3 -2 -1 0 1 2 3) 'vector)
              #(-3 -2 -1 0)))
  (is (equalp (take-until #'minusp '(-3 -2 -1 0 1 2 3) 'vector)
              #()))
  (is (equal (take-until #'minusp '(-3 -2 -1 0 1 2 3) 'string)
             ""))
  (is (equal (take-until #'integerp '(#\0 #\1 #\2 3 4 5) 'string)
             "012"))
  (is (equal (take-until #'minusp '(-3 -2 -1 0 1 2 3) 'bit-vector)
             #*))
  (is (equal (take-until #'zerop '(1 1 0 0) 'bit-vector)
             #*11)))

(test ?take-until.vector
  (is (equal (take-until #'plusp #(-3 -2 -1 0 1 2 3))
             '(-3 -2 -1 0)))
  (is (equal (take-until #'minusp #(-3 -2 -1 0 1 2 3))
             '()))
  (is (equal (take-until #'plusp #(-3 -2 -1 0 1 2 3) 'list)
             '(-3 -2 -1 0)))
  (is (equal (take-until #'minusp #(-3 -2 -1 0 1 2 3) 'list)
             '()))
  (is (equalp (take-until #'plusp #(-3 -2 -1 0 1 2 3) 'vector)
              #(-3 -2 -1 0)))
  (is (equalp (take-until #'minusp #(-3 -2 -1 0 1 2 3) 'vector)
              #()))
  (is (equal (take-until #'minusp #(-3 -2 -1 0 1 2 3) 'string)
             ""))
  (is (equal (take-until #'integerp #(#\0 #\1 #\2 3 4 5) 'string)
             "012"))
  (is (equal (take-until #'minusp #(-3 -2 -1 0 1 2 3) 'bit-vector)
             #*))
  (is (equal (take-until #'zerop #(1 1 0 0) 'bit-vector)
             #*11)))

(test ?take-until.string
  (is (equal (take-until #'digit-char-p "012abc")
             '()))
  (is (equal (take-until #'alpha-char-p "012abc")
             '(#\0 #\1 #\2)))
  (is (equal (take-until #'digit-char-p "012abc" 'list)
             '()))
  (is (equal (take-until #'alpha-char-p "012abc" 'list)
             '(#\0 #\1 #\2)))
  (is (equalp (take-until #'digit-char-p "012abc" 'vector)
              #()))
  (is (equalp (take-until #'alpha-char-p "012abc" 'vector)
              #(#\0 #\1 #\2)))
  (is (equal (take-until #'digit-char-p "012abc" 'string)
             ""))
  (is (equal (take-until #'alpha-char-p "012abc" 'string)
             "012")))

(test ?take-until.bit-vector  
  (is (equal (take-until #'zerop #*000111)
             '()))
  (is (equal (take-until (complement #'zerop) #*000111)
             '(0 0 0)))
  (is (equal (take-until #'zerop #*000111 'list)
             '()))
  (is (equal (take-until (complement #'zerop) #*000111 'list)
             '(0 0 0)))
  (is (equalp (take-until #'zerop #*000111 'vector)
              #()))
  (is (equalp (take-until (complement #'zerop) #*000111 'vector)
              #(0 0 0)))
  (is (equalp (take-until #'zerop #*000111 'bit-vector)
              #*))
  (is (equalp (take-until (complement #'zerop) #*000111 'bit-vector)
              #*000)))

(test ?take-until.lazy-sequence
  (is (equal (take-until #'plusp #[-3 -2 -1 0 1 2 3])
             '(-3 -2 -1 0)))
  (is (equal (take-until #'minusp #[-3 -2 -1 0 1 2 3])
             '()))
  (is (equal (take-until #'plusp #[-3 -2 -1 0 1 2 3] 'list)
             '(-3 -2 -1 0)))
  (is (equal (take-until #'minusp #[-3 -2 -1 0 1 2 3] 'list)
             '()))
  (is (equalp (take-until #'plusp #[-3 -2 -1 0 1 2 3] 'vector)
              #(-3 -2 -1 0)))
  (is (equalp (take-until #'minusp #[-3 -2 -1 0 1 2 3] 'vector)
              #()))
  (is (equal (take-until #'minusp #[-3 -2 -1 0 1 2 3] 'string)
             ""))
  (is (equal (take-until #'integerp #[#\0 #\1 #\2 3 4 5] 'string)
             "012"))
  (is (equal (take-until #'minusp #[-3 -2 -1 0 1 2 3] 'bit-vector)
             #*))
  (is (equal (take-until #'zerop #[1 1 0 0] 'bit-vector)
             #*11)))


;;--------------------------------------------------------------------
;; take-while
;;--------------------------------------------------------------------

(test ?take-while.list
  (is (equal (take-while #'plusp '(-3 -2 -1 0 1 2 3))
             '()))
  (is (equal (take-while #'minusp '(-3 -2 -1 0 1 2 3))
             '(-3 -2 -1)))
  (is (equal (take-while #'plusp '(-3 -2 -1 0 1 2 3) 'list)
             '()))
  (is (equal (take-while #'minusp '(-3 -2 -1 0 1 2 3) 'list)
             '(-3 -2 -1)))
  (is (equalp (take-while #'plusp '(-3 -2 -1 0 1 2 3) 'vector)
              #()))
  (is (equalp (take-while #'minusp '(-3 -2 -1 0 1 2 3) 'vector)
              #(-3 -2 -1)))
  (is (equal (take-while #'plusp '(-3 -2 -1 0 1 2 3) 'string)
             ""))
  (is (equal (take-while #'characterp '(#\0 #\1 #\2 3 4 5) 'string)
             "012"))
  (is (equal (take-while #'plusp '(-3 -2 -1 0 1 2 3) 'bit-vector)
             #*))
  (is (equal (take-while #'plusp '(1 1 0 0) 'bit-vector)
             #*11)))

(test ?take-while.vector
  (is (equal (take-while #'plusp #(-3 -2 -1 0 1 2 3))
             '()))
  (is (equal (take-while #'minusp #(-3 -2 -1 0 1 2 3))
             '(-3 -2 -1)))
  (is (equal (take-while #'plusp #(-3 -2 -1 0 1 2 3) 'list)
             '()))
  (is (equal (take-while #'minusp #(-3 -2 -1 0 1 2 3) 'list)
             '(-3 -2 -1)))
  (is (equalp (take-while #'plusp #(-3 -2 -1 0 1 2 3) 'vector)
              #()))
  (is (equalp (take-while #'minusp #(-3 -2 -1 0 1 2 3) 'vector)
              #(-3 -2 -1)))
  (is (equal (take-while #'plusp #(-3 -2 -1 0 1 2 3) 'string)
             ""))
  (is (equal (take-while #'characterp #(#\0 #\1 #\2 3 4 5) 'string)
             "012"))
  (is (equal (take-while #'plusp #(-3 -2 -1 0 1 2 3) 'bit-vector)
             #*))
  (is (equal (take-while #'plusp #(1 1 0 0) 'bit-vector)
             #*11)))

(test ?take-while.string
  (is (equal (take-while #'digit-char-p "012abc")
             '(#\0 #\1 #\2)))
  (is (equal (take-while #'alpha-char-p "012abc")
             '()))
  (is (equal (take-while #'digit-char-p "012abc" 'list)
             '(#\0 #\1 #\2)))
  (is (equal (take-while #'alpha-char-p "012abc" 'list)
             '()))
  (is (equalp (take-while #'digit-char-p "012abc" 'vector)
              #(#\0 #\1 #\2)))
  (is (equalp (take-while #'alpha-char-p "012abc" 'vector)
              #()))
  (is (equal (take-while #'digit-char-p "012abc" 'string)
             "012"))
  (is (equal (take-while #'alpha-char-p "012abc" 'string)
             "")))

(test ?take-while.bit-vector  
  (is (equal (take-while #'zerop #*000111)
             '(0 0 0)))
  (is (equal (take-while (complement #'zerop) #*000111)
             '()))
  (is (equal (take-while #'zerop #*000111 'list)
             '(0 0 0)))
  (is (equal (take-while (complement #'zerop) #*000111 'list)
             '()))
  (is (equalp (take-while #'zerop #*000111 'vector)
              #(0 0 0)))
  (is (equalp (take-while (complement #'zerop) #*000111 'vector)
              #()))
  (is (equalp (take-while #'zerop #*000111 'bit-vector)
              #*000))
  (is (equalp (take-while (complement #'zerop) #*000111 'bit-vector)
              #*)))

(test ?take-while.lazy-sequence
  (is (equal (take-while #'minusp #[-3 -2 -1 0 1 2 3])
             '(-3 -2 -1)))
  (is (equal (take-while #'plusp #[-3 -2 -1 0 1 2 3])
             '()))
  (is (equal (take-while #'minusp #[-3 -2 -1 0 1 2 3] 'list)
             '(-3 -2 -1)))
  (is (equal (take-while #'plusp #[-3 -2 -1 0 1 2 3] 'list)
             '()))
  (is (equalp (take-while #'minusp #[-3 -2 -1 0 1 2 3] 'vector)
              #(-3 -2 -1)))
  (is (equalp (take-while #'plusp #[-3 -2 -1 0 1 2 3] 'vector)
              #()))
  (is (equal (take-while #'plusp #[-3 -2 -1 0 1 2 3] 'string)
             ""))
  (is (equal (take-while #'characterp #[#\0 #\1 #\2 3 4 5] 'string)
             "012"))
  (is (equal (take-while #'plusp #[-3 -2 -1 0 1 2 3] 'bit-vector)
             #*))
  (is (equal (take-while #'zerop #[0 0 1 1] 'bit-vector)
             #*00)))


;;--------------------------------------------------------------------
;; drop
;;--------------------------------------------------------------------
;; TODO:
;; :all


(test ?drop.list.0
  (is (equal (drop 0 '(0 1 2 3 4 5))
             '(0 1 2 3 4 5)))
  (is (equal (drop 3 '(0 1 2 3 4 5))
             '(3 4 5)))
  (is (equal (drop 30 '(0 1 2 3 4 5))
             '()))
  (is (equalp (drop 3 '(0 1 2 3 4 5) 'list)
              '(3 4 5)))
  (is (equalp (drop 3 '(0 1 2 3 4 5) 'vector)
              #(3 4 5)))
  (is (equal (drop 2 '(0 0 1 1) 'bit-vector)
             #*11))
  (is (equal (drop 2 '(#\0 #\1 #\2 #\3 #\4 #\5) 'string)
             "2345")))

(test ?drop.list.1
  (for-all ((lst (gen-list))
            (len (gen-integer :min 0 :max 15)))
    (let ((end (min len (length lst))))
      (is (equal (drop len lst)
                 (subseq lst end)))
      (is (equal (drop len lst 'list)
                 (subseq lst end)))
      (is (equalp (drop len lst 'vector)
                  (coerce (subseq lst end) 'vector)))))
  (for-all ((str (gen-string))
            (len (gen-integer :min 0 :max 15)))
    (is (equal (drop len (coerce str 'list) 'string)
               (subseq str (min len (length str))))))
  (for-all ((bits (gen-list :elements (lambda () (random 2))))
            (len  (gen-integer :min 0 :max 15)))
    (is (equal (drop len bits 'bit-vector)
               (coerce (subseq bits (min len (length bits)))
                       'bit-vector)))))

(test ?drop.vector.0
  (is (equalp (drop 0 #(0 1 2 3 4 5))
              #(0 1 2 3 4 5)))
  (is (equalp (drop 3 #(0 1 2 3 4 5))
              #(3 4 5)))
  (is (equalp (drop 30 #(0 1 2 3 4 5))
              #()))
  (is (equalp (drop 3 #(0 1 2 3 4 5) 'list)
              '(3 4 5)))
  (is (equalp (drop 3 #(0 1 2 3 4 5) 'vector)
              #(3 4 5)))
  (is (equal (drop 2 #(0 0 1 1) 'bit-vector)
             #*11))
  (is (equal (drop 2 #(#\0 #\1 #\2 #\3 #\4 #\5) 'string)
             "2345")))

(test ?drop.vector.1
  (for-all ((lst (gen-list))
            (len (gen-integer :min 0 :max 15)))
    (let ((vec (coerce lst 'vector))
          (end (min len (length lst))))
      ;; Note: drop's default result-type is SEQUENS.
      (is (equalp (drop len vec)
                  (subseq vec end)))
      (is (equal (drop len vec 'list)
                 (coerce (subseq vec end) 'list)))
      (is (equalp (drop len vec 'vector)
                  (coerce (subseq vec end) 'vector)))))
  (for-all ((str (gen-string))
            (len (gen-integer :min 0 :max 15)))
    (is (equal (drop len (coerce str 'vector) 'string)
               (subseq str (min len (length str))))))
  (for-all ((bits (gen-list :elements (lambda () (random 2))))
            (len  (gen-integer :min 0 :max 15)))
    (is (equal (drop len (coerce bits 'vector) 'bit-vector)
               (coerce (subseq bits (min len (length bits)))
                       'bit-vector)))))

(test ?drop.string.0
  (is (equal (drop 0 "012345")
             "012345"))
  (is (equal (drop 3 "012345")
             "345"))
  (is (equal (drop 30 "012345")
             ""))
  (is (equal (drop 3 "012345" 'list)
             '(#\3 #\4 #\5)))
  (is (equalp (drop 3 "012345" 'vector)
              "345"))
  (is (equal (drop 3 "012345" 'string)
             "345")))

(test ?drop.string.1
  (for-all ((str (gen-string))
            (len (gen-integer :min 0 :max 15)))
    (let ((end (min len (length str))))
      (is (equal (drop len str)
                 (subseq str end)))
      (is (equal (drop len str 'list)
                 (coerce (subseq str end) 'list)))
      (is (equalp (drop len str 'vector)
                  (coerce (subseq str end) 'vector)))
      (is (equal (drop len str 'string)
                 (subseq str end))))))

(test ?drop.bit-vector.0
  (is (equal (drop 3 #*000111)
             #*111))
  (is (equal (drop 0 #*000111)
             #*000111))
  (is (equal (drop 30 #*000111)
             #*))
  (is (equal (drop 3 #*000111 'list)
             '(1 1 1)))
  (is (equalp (drop 3 #*000111 'vector)
              #(1 1 1)))
  (is (equal (drop 3 #*000111 'bit-vector)
             #*111)))

(test ?drop.bit-vector.1
  (for-all ((bits (gen-list :elements (lambda () (random 2))))
            (len (gen-integer :min 0 :max 15)))
    (let* ((bvec (coerce bits 'bit-vector))
           (end (min len (length bvec))))
      (is (equal (drop len bvec)
                 (subseq bvec end)))
      (is (equal (drop len bvec 'list)
                 (coerce (subseq bvec end) 'list)))
      (is (equalp (drop len bvec 'vector)
                  (coerce (subseq bvec end) 'vector)))
      (is (equal (drop len (coerce bvec 'vector) 'bit-vector)
                 (coerce (subseq bvec end) 'bit-vector))))))

(test ?drop.lazy-sequence.0
  (is (equal (take :all (drop 0 #[0 1 2 3 4 5]))
             '(0 1 2 3 4 5)))
  (is (equal (take :all (drop 3 #[0 1 2 3 4 5]))
             '(3 4 5)))
  (is (equal (take :all (drop 30 #[0 1 2 3 4 5]))
             '()))
  (is (equal (drop 3 #[0 1 2 3 4 5] 'list)
             '(3 4 5)))
  (is (equalp (drop 3 #[0 1 2 3 4 5] 'vector)
              #(3 4 5)))
  (is (equal (drop 2 #[0 0 1 1] 'bit-vector)
             #*11))
  (is (equal (drop 2 #[#\0 #\1 #\2 #\3 #\4 #\5] 'string)
             "2345")))

(test ?drop.lazy-sequence.1
  (for-all ((lst (gen-list))
            (len (gen-integer :min 0 :max 15)))
    (let ((end  (min len (length lst))))
      (is (equal (take :all (drop len (make-lazy-seq lst)))
                 (subseq lst end)))
      (is (equal (drop len (make-lazy-seq lst) 'list)
                 (subseq lst end)))
      (is (equalp (drop len (make-lazy-seq lst) 'vector)
                  (coerce (subseq lst end) 'vector)))))
  (for-all ((str (gen-string))
            (len (gen-integer :min 0 :max 15)))
    (let ((lseq (make-lazy-seq (coerce str 'list))))
      (is (equal (drop len lseq 'string)
                 (subseq str (min len (length str)))))))
  (for-all ((bits (gen-list :elements (lambda () (random 2))))
            (len  (gen-integer :min 0 :max 15)))
    (let ((lseq (make-lazy-seq bits)))
      (is (equal (drop len lseq 'bit-vector)
                 (coerce (subseq bits (min len (length bits)))
                         'bit-vector))))))


;;--------------------------------------------------------------------
;; drop-until
;;--------------------------------------------------------------------

;; TODO: sequens, sequence
(test ?drop-until.list
  (is (equal (drop-until #'plusp '(-3 -2 -1 0 1 2 3))
             '(1 2 3)))
  (is (equal (drop-until #'minusp '(-3 -2 -1 0 1 2 3))
             '(-3 -2 -1 0 1 2 3)))
  (is (equal (drop-until #'plusp '(-3 -2 -1 0 1 2 3) 'list)
             '(1 2 3)))
  (is (equal (drop-until #'minusp '(-3 -2 -1 0 1 2 3) 'list)
             '(-3 -2 -1 0 1 2 3)))
  (is (equalp (drop-until #'plusp '(-3 -2 -1 0 1 2 3) 'vector)
              #(1 2 3)))
  (is (equalp (drop-until #'minusp '(-3 -2 -1 0 1 2 3) 'vector)
              #(-3 -2 -1 0 1 2 3)))
  (is (equal (drop-until #'characterp '(-3 -2 -1 0 1 2 3) 'string)
             ""))
  (is (equal (drop-until #'characterp '(0 1 2 #\3 #\4 #\5) 'string)
             "345"))
  (is (equal (drop-until #'zerop '(-3 -2 -1 0 1) 'bit-vector)
             #*01))
  (is (equal (drop-until #'minusp '(1 1 0 0) 'bit-vector)
             #*)))

(test ?drop-until.vector
  (is (equalp (drop-until #'plusp #(-3 -2 -1 0 1 2 3))
              #(1 2 3)))
  (is (equalp (drop-until #'minusp #(-3 -2 -1 0 1 2 3))
              #(-3 -2 -1 0 1 2 3)))
  (is (equal (drop-until #'plusp #(-3 -2 -1 0 1 2 3) 'list)
             '(1 2 3)))
  (is (equal (drop-until #'minusp #(-3 -2 -1 0 1 2 3) 'list)
             '(-3 -2 -1 0 1 2 3)))
  (is (equalp (drop-until #'plusp #(-3 -2 -1 0 1 2 3) 'vector)
              #(1 2 3)))
  (is (equalp (drop-until #'minusp #(-3 -2 -1 0 1 2 3) 'vector)
              #(-3 -2 -1 0 1 2 3)))
  (is (equal (drop-until #'characterp #(-3 -2 -1 0 1 2 3) 'string)
             ""))
  (is (equal (drop-until #'characterp #(0 1 2 #\3 #\4 #\5) 'string)
             "345"))
  (is (equal (drop-until #'zerop #(-3 -2 -1 0 1) 'bit-vector)
             #*01))
  (is (equal (drop-until #'minusp #(1 1 0 0) 'bit-vector)
             #*)))

(test ?drop-until.string
  (is (equal (drop-until #'digit-char-p "abc")
             ""))
  (is (equal (drop-until #'digit-char-p "012abc")
             "012abc"))
  (is (equal (drop-until #'alpha-char-p "012abc")
             "abc"))
  (is (equal (drop-until #'digit-char-p "012abc" 'list)
             '(#\0 #\1 #\2 #\a #\b #\c)))
  (is (equal (drop-until #'alpha-char-p "012abc" 'list)
             '(#\a #\b #\c)))
  (is (equalp (drop-until #'digit-char-p "012abc" 'vector)
              #(#\0 #\1 #\2 #\a #\b #\c)))
  (is (equalp (drop-until #'alpha-char-p "012abc" 'vector)
              #(#\a #\b #\c)))
  (is (equal (drop-until #'digit-char-p "012abc" 'string)
             "012abc"))
  (is (equal (drop-until #'alpha-char-p "012abc" 'string)
             "abc")))

(test ?drop-until.bit-vector  
  (is (equal (drop-until #'zerop #*000111)
             #*000111))
  (is (equal (drop-until (complement #'zerop) #*000111)
             #*111))
  (is (equal (drop-until #'zerop #*000111 'list)
             '(0 0 0 1 1 1)))
  (is (equal (drop-until (complement #'zerop) #*000111 'list)
             '(1 1 1)))
  (is (equalp (drop-until #'zerop #*000111 'vector)
              #(0 0 0 1 1 1)))
  (is (equalp (drop-until (complement #'zerop) #*000111 'vector)
              #(1 1 1)))
  (is (equalp (drop-until #'characterp #*000111 'bit-vector)
              #*))
  (is (equalp (drop-until (complement #'zerop) #*000111 'bit-vector)
              #*111)))

(test ?drop-until.lazy-sequence
  (is (equal (take :all (drop-until #'plusp #[-3 -2 -1 0 1 2 3]))
             '(1 2 3)))
  (is (equal (take :all (drop-until #'minusp #[-3 -2 -1 0 1 2 3]))
             '(-3 -2 -1 0 1 2 3)))
  (is (equal (drop-until #'plusp #[-3 -2 -1 0 1 2 3] 'list)
             '(1 2 3)))
  (is (equal (drop-until #'minusp #[-3 -2 -1 0 1 2 3] 'list)
             '(-3 -2 -1 0 1 2 3)))
  (is (equalp (drop-until #'plusp #[-3 -2 -1 0 1 2 3] 'vector)
              #(1 2 3)))
  (is (equalp (drop-until #'minusp #[-3 -2 -1 0 1 2 3] 'vector)
              #(-3 -2 -1 0 1 2 3)))
  (is (equal (drop-until #'characterp #[-3 -2 -1 0 1 2 3] 'string)
             ""))
  (is (equal (drop-until #'characterp #[0 1 2 #\3 #\4 #\5] 'string)
             "345"))
  (is (equal (drop-until #'zerop #[-3 -2 -1 0 1] 'bit-vector)
             #*01))
  (is (equal (drop-until #'minusp '(1 1 0 0) 'bit-vector)
             #*)))

;;--------------------------------------------------------------------
;; drop-while
;;--------------------------------------------------------------------

(test ?drop-while.list
  (is (equal (drop-while #'plusp '(-3 -2 -1 0 1 2 3))
             '(-3 -2 -1 0 1 2 3)))
  (is (equal (drop-while #'minusp '(-3 -2 -1 0 1 2 3))
             '(0 1 2 3)))
  (is (equal (drop-while #'plusp '(-3 -2 -1 0 1 2 3) 'list)
             '(-3 -2 -1 0 1 2 3)))
  (is (equal (drop-while #'minusp '(-3 -2 -1 0 1 2 3) 'list)
             '(0 1 2 3)))
  (is (equalp (drop-while #'plusp '(-3 -2 -1 0 1 2 3) 'vector)
              #(-3 -2 -1 0 1 2 3)))
  (is (equalp (drop-while #'minusp '(-3 -2 -1 0 1 2 3) 'vector)
              #(0 1 2 3)))
  (is (equal (drop-while #'plusp '(1 2 3) 'string)
             ""))
  (is (equal (drop-while #'integerp '(0 1 2 #\3 #\4 #\5) 'string)
             "345"))
  (is (equal (drop-while #'minusp '(-3 -2 -1) 'bit-vector)
             #*))
  (is (equal (drop-while #'plusp '(1 1 0 0) 'bit-vector)
             #*00)))


(test ?drop-while.vector
  (is (equalp (drop-while #'plusp #(-3 -2 -1 0 1 2 3))
              #(-3 -2 -1 0 1 2 3)))
  (is (equalp (drop-while #'minusp #(-3 -2 -1 0 1 2 3))
              #(0 1 2 3)))
  (is (equal (drop-while #'plusp #(-3 -2 -1 0 1 2 3) 'list)
             '(-3 -2 -1 0 1 2 3)))
  (is (equal (drop-while #'minusp #(-3 -2 -1 0 1 2 3) 'list)
             '(0 1 2 3)))
  (is (equalp (drop-while #'plusp #(-3 -2 -1 0 1 2 3) 'vector)
              #(-3 -2 -1 0 1 2 3)))
  (is (equalp (drop-while #'minusp #(-3 -2 -1 0 1 2 3) 'vector)
              #(0 1 2 3)))
  (is (equal (drop-while #'plusp #(1 2 3) 'string)
             ""))
  (is (equal (drop-while #'integerp #(0 1 2 #\3 #\4 #\5) 'string)
             "345"))
  (is (equal (drop-while #'minusp #(-3 -2 -1) 'bit-vector)
             #*))
  (is (equal (drop-while #'plusp #(1 1 0 0) 'bit-vector)
             #*00)))

(test ?drop-while.string
  (is (equal (drop-while #'digit-char-p "012")
             ""))
  (is (equal (drop-while #'digit-char-p "012abc")
             "abc"))
  (is (equal (drop-while #'alpha-char-p "012abc")
             "012abc"))
  (is (equal (drop-while #'digit-char-p "012abc" 'list)
             '(#\a #\b #\c)))
  (is (equal (drop-while #'alpha-char-p "012abc" 'list)
             '(#\0 #\1 #\2 #\a #\b #\c)))
  (is (equalp (drop-while #'digit-char-p "012abc" 'vector)
              #(#\a #\b #\c)))
  (is (equalp (drop-while #'alpha-char-p "012abc" 'vector)
              #(#\0 #\1 #\2 #\a #\b #\c)))
  (is (equal (drop-while #'digit-char-p "012abc" 'string)
             "abc"))
  (is (equal (drop-while #'alpha-char-p "012abc" 'string)
             "012abc")))


(test ?drop-while.bit-vector  
  (is (equal (drop-while #'zerop #*000111)
             #*111))
  (is (equal (drop-while (complement #'zerop) #10*1)
             #*))
  (is (equal (drop-while #'zerop #*000111 'list)
             '(1 1 1)))
  (is (equal (drop-while (complement #'zerop) #10*1 'list)
             '()))
  (is (equalp (drop-while #'zerop #*000111 'vector)
              #(1 1 1)))
  (is (equalp (drop-while (complement #'zerop) #10*1 'vector)
              #()))
  (is (equalp (drop-while #'zerop #*000111 'bit-vector)
              #*111))
  (is (equalp (drop-while (complement #'zerop) #10*1 'bit-vector)
              #*)))

(test ?drop-while.lazy-sequence
  (is (equal (take :all (drop-while #'plusp #[-3 -2 -1 0 1 2 3]))
             '(-3 -2 -1 0 1 2 3)))
  (is (equal (take :all (drop-while #'minusp #[-3 -2 -1 0 1 2 3]))
             '(0 1 2 3)))
  (is (equal (drop-while #'plusp #[-3 -2 -1 0 1 2 3] 'list)
             '(-3 -2 -1 0 1 2 3)))
  (is (equal (drop-while #'minusp #[-3 -2 -1 0 1 2 3] 'list)
             '(0 1 2 3)))
  (is (equalp (drop-while #'plusp #[-3 -2 -1 0 1 2 3] 'vector)
              #(-3 -2 -1 0 1 2 3)))
  (is (equalp (drop-while #'minusp #[-3 -2 -1 0 1 2 3] 'vector)
              #(0 1 2 3)))
  (is (equal (drop-while #'plusp #[1 2 3] 'string)
             ""))
  (is (equal (drop-while #'integerp #[0 1 2 #\3 #\4 #\5] 'string)
             "345"))
  (is (equal (drop-while #'minusp #[-3 -2 -1] 'bit-vector)
             #*))
  (is (equal (drop-while #'plusp #[1 1 0 0] 'bit-vector)
             #*00)))


;;--------------------------------------------------------------------
;; zip
;;--------------------------------------------------------------------

(test ?zip.list
  (is (equal (zip '() '(1 2 3 4))
             '()))
  (is (equal (zip '(a b c) '())
             '()))
  (is (equal (zip '(a b c) '(1 2 3 4))
             '((a 1) (b 2) (c 3))))
  (is (equal (zip '(a b c) '(1 2 3 4) '(:foo :bar :baz))
             '((a 1 :foo) (b 2 :bar) (c 3 :baz))))
  (is (equal (zip '(a b c) '(1 2 3 4) '(:foo :bar :baz) '(one))
             '((a 1 :foo one))))
  (is (equal (zip '(a b c) #(1 2 3 4))
             '((a 1) (b 2) (c 3))))

  (is (equal (zip '(a b c) '(1 2 3 4) #(:foo :bar :baz))
             '((a 1 :foo) (b 2 :bar) (c 3 :baz))))
  (is (equal (zip '(a b c) '(1 2 3 4) '(:foo :bar :baz) #(one))
             '((a 1 :foo one))))
  (is (equal (zip '(a b c) #*1111 "string" #(one))
             '((a 1 #\s one)))))

(test ?zip.vector
  (is (equal (zip #() '(1 2 3 4))
             '()))
  (is (equal (zip #(a b c) '())
             '()))
  (is (equal (zip #(a b c) '(1 2 3 4))
             '((a 1) (b 2) (c 3))))
  (is (equal (zip #(a b c) '(1 2 3 4) '(:foo :bar :baz))
             '((a 1 :foo) (b 2 :bar) (c 3 :baz))))
  (is (equal (zip #(a b c) '(1 2 3 4) '(:foo :bar :baz) '(one))
             '((a 1 :foo one))))
  (is (equal (zip #(a b c) #(1 2 3 4))
             '((a 1) (b 2) (c 3))))
  (is (equal (zip #(a b c) '(1 2 3 4) #(:foo :bar :baz))
             '((a 1 :foo) (b 2 :bar) (c 3 :baz))))
  (is (equal (zip #(a b c) '(1 2 3 4) '(:foo :bar :baz) #(one))
             '((a 1 :foo one))))
  (is (equal (zip #(a b c) #*1111 "string" #(one))
             '((a 1 #\s one)))))

(test ?zip.bit-vector
  (is (equal (zip #* '(1 2 3 4))
             '()))
  (is (equal (zip #*111 '())
             '()))
  (is (equal (zip #*111 '(0 0 0 0))
             '((1 0) (1 0) (1 0))))
  )


;;--------------------------------------------------------------------
;; interleave
;;--------------------------------------------------------------------

(test ?interleave.list
  (is (equal (interleave '() '(1 2 3 4))
             '()))
  (is (equal (interleave '(a b c) '())
             '()))
  (is (equal (interleave '(a b c) '(1 2 3 4))
             '(a 1 b 2 c 3)))
  (is (equal (interleave '(a b c) '(1 2 3 4) '(:foo :bar :baz))
             '(a 1 :foo b 2 :bar c 3 :baz)))
  (is (equal (interleave '(a b c) '(1 2 3 4) '(:foo :bar :baz) '(one))
             '(a 1 :foo one)))

  (is (equal (interleave '(a b c) #(1 2 3 4))
             '(a 1 b 2 c 3)))
  (is (equal (interleave '(a b c) '(1 2 3 4) #(:foo :bar :baz))
             '(a 1 :foo b 2 :bar c 3 :baz)))
  (is (equal (interleave '(a b c) '(1 2 3 4) '(:foo :bar :baz) #(one))
             '(a 1 :foo one)))
  )

;; (interleave '(:a :b) (induce #'1+ 1)) => (:a 1 :b 2)



;;--------------------------------------------------------------------
;; interpose
;;--------------------------------------------------------------------

(test ?interpose.list
  (is (equal (interpose :sep '())
             '()))
  (is (equal (interpose :sep '(0))
             '(0)))
  (is (equal (interpose :sep '(0 1 2 3))
             '(0 :sep 1 :sep 2 :sep 3))))

(test ?interpose.vector
  (is (equalp (interpose :sep #())
              #()))
  (is (equalp (interpose :sep #(0))
              #(0)))
  (is (equalp (interpose :sep #(0 1 2 3))
              #(0 :sep 1 :sep 2 :sep 3))))

(test ?interpose.string
  (is (string= (interpose #\0 "")
               ""))
  (is (string= (interpose #\0 "s")
               "s"))
  (is (string= (interpose #\0 "string")
               "s0t0r0i0n0g")))

(test ?interpose.bit-vector
  (is (equal (interpose 0 #*)
             #*))
  (is (equal (interpose 0 #*1)
             #*1))
  (is (equal (interpose 0 #*1111)
             #*1010101)))

(test ?interpose.lazy-sequence
  (is-true (lazy-sequence-p (interpose :sep #[0 1 2])))
  (is (equal (take :all (interpose :sep #[]))
             '()))
  (is (equal (take :all (interpose :sep #[0 1 2]))
             '(0 :sep 1 :sep 2)))
  (is (equal (take 10 (interpose :sep #[1 2 ..]))
             '(1 :sep 2 :sep 3 :sep 4 :sep 5 :sep)))
  (is (equal (take 10 (interpose :sep #[1 2 4 ..]))
             '(1 :sep 2 :sep 4 :sep 8 :sep 16 :sep))))


;;====================================================================
