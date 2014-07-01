;;;; cl-plus/test/core/buxis.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Tests for Buxis 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.buxis
  (:export #:?buxis)
  (:use #:cl #:fiveam)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:cl-plus.src.readtables
                #:cl+)
  (:import-from #:alexandria
                #:with-gensyms
                #:copy-hash-table)
  #-ccl
  (:import-from #:bordeaux-threads
                #:with-timeout
                #:timeout)
  #+ccl
  (:import-from #:trivial-timeout
                #:with-timeout
                #:timeout-error)
  (:import-from #:cl-plus-test
                #:all
                #:is-=
                #:is-eql
                #:is-equal
                #:is-equalp)
  (:import-from #:alexandria
                #:copy-array)
  (:import-from #:cl-plus.src.core.lazy-sequence
                #:lfor
                #:accessed-length
                #:lazy-take)
  (:import-from #:cl-plus.src.core.array
                #:array-subscripts)
  (:import-from #:cl-plus.src.core.buxis
                #:buxis       #:buxisp
                #:dobux       #:dobux2
                #:with-iterator
                #:with-iterators
                #:every*      #:notevery*
                #:some*       #:notany*
                #:map*        #:reduce*
                #:count*      #:count-if*      #:count-if-not*
                #:find*       #:find-if*       #:find-if-not*
                #:position*   #:position-if*   #:position-if-not*
                #:remove*     #:remove-if*     #:remove-if-not*
                #:collect-if*
                #:substitute* #:substitute-if* #:substitute-if-not*
                #:fill*))

(in-package #:cl-plus.test.core.buxis)

(def-suite ?buxis :in all)
(in-suite ?buxis)

(in-readtable cl+)


;;--------------------------------------------------------------------
;; utils for test
;;--------------------------------------------------------------------

(defmacro with-time-limit ((sec &optional (if-timeout :timeout))
                           &body body)
  (declare (ignorable sec body))
  (check-type sec (integer 1 *))
  #+thread-support
  (progn
    #-ccl `(handler-case
               (bordeaux-threads:with-timeout (,sec) ,@body)
             (bordeaux-threads:timeout () ,if-timeout))
    #+ccl `(handler-case
               (trivial-timeout:with-timeout (,sec) ,@body)
             (trivial-timeout:timeout-error () ,if-timeout)))
  #-thread-support
  (progn
    (warn "Sorry, WITH-TIME-LIMIT doesn't work properly for your CL system.")
    if-timeout)
  )


(defmacro with-muffle-warnings (&body body)
  (with-gensyms (c)
   `(handler-bind ((warning (lambda (,c)
                              (declare (ignore ,c))
                              (muffle-warning))))
      ,@body)))


;;--------------------------------------------------------------------
;; with-iterator, with-iterators
;;--------------------------------------------------------------------

(test ?with-iterator.list
  (with-iterator (next '(1 2 3))
    (is (equal (multiple-value-list (next))
               '(T 0 1)))
    (is (equal (multiple-value-list (next))
               '(T 1 2)))
    (is (equal (multiple-value-list (next))
               '(T 2 3)))
    (is (equal (multiple-value-list (next))
               '(NIL)))
    (is (equal (multiple-value-list (next))
               '(NIL)))))

(test ?with-iterator.vector
  (with-iterator (next #(1 2 3))
    (is (equal (multiple-value-list (next))
               '(T 0 1)))
    (is (equal (multiple-value-list (next))
               '(T 1 2)))
    (is (equal (multiple-value-list (next))
               '(T 2 3)))
    (is (equal (multiple-value-list (next))
               '(NIL)))
    (is (equal (multiple-value-list (next))
               '(NIL)))))

(test ?with-iterator.bit-vector
  (with-iterator (next #*101)
    (is (equal (multiple-value-list (next))
               '(T 0 1)))
    (is (equal (multiple-value-list (next))
               '(T 1 0)))
    (is (equal (multiple-value-list (next))
               '(T 2 1)))
    (is (equal (multiple-value-list (next))
               '(NIL)))
    (is (equal (multiple-value-list (next))
               '(NIL)))))

(test ?with-iterator.string
  (with-iterator (next "str")
    (is (equal (multiple-value-list (next))
               '(T 0 #\s)))
    (is (equal (multiple-value-list (next))
               '(T 1 #\t)))
    (is (equal (multiple-value-list (next))
               '(T 2 #\r)))
    (is (equal (multiple-value-list (next))
               '(NIL)))
    (is (equal (multiple-value-list (next))
               '(NIL)))))

(test ?with-iterator.lazy-sequence
  ;; finite
  (with-iterator (next #[1 2 3])
    (is (equal (multiple-value-list (next))
               '(T 0 1)))
    (is (equal (multiple-value-list (next))
               '(T 1 2)))
    (is (equal (multiple-value-list (next))
               '(T 2 3)))
    (is (equal (multiple-value-list (next))
               '(NIL 3 NIL)))
    (is (equal (multiple-value-list (next))
               '(NIL 4 NIL))))
  ;; infinite
  (let ((lseq #4[1 ..]))
    (is (= 1 (accessed-length lseq)))
    (with-iterator (next lseq)
      (is (equal (multiple-value-list (next))
                 '(T 0 1)))
      (is (equal (multiple-value-list (next))
                 '(T 1 2)))
      (is (equal (multiple-value-list (next))
                 '(T 2 3)))
      (is (equal (multiple-value-list (next))
                 '(T 3 4)))
      (is (equal (multiple-value-list (next))
                 '(T 4 5))))
    (is (= 5 (accessed-length lseq)))))

(test ?with-iterator.hash-table
  (let ((ht #{:foo 0 :bar 1 :baz 2}))
    (with-muffle-warnings
      (with-iterator (next ht)
        (multiple-value-bind (more? key val) (next)
          (is-true more?)
          (is (= val (gethash key ht))))
        (multiple-value-bind (more? key val) (next)
          (is-true more?)
          (is (= val (gethash key ht))))
        (multiple-value-bind (more? key val) (next)
          (is-true more?)
          (is (= val (gethash key ht))))
        (multiple-value-bind (more? key val) (next)
          (declare (ignore val))
          (is-false more?)
          (is (eq :nothing (gethash key ht :nothing))))))))

(test ?with-iterator.array
  (with-iterator (next #2A((1 2) (3 4)))
    (is (equal (multiple-value-list (next))
               '(T 0 1)))
    (is (equal (multiple-value-list (next))
               '(T 1 2)))
    (is (equal (multiple-value-list (next))
               '(T 2 3)))
    (is (equal (multiple-value-list (next))
               '(T 3 4)))
    (is (equal (multiple-value-list (next))
               '(NIL)))
    (is (equal (multiple-value-list (next))
               '(NIL)))))

(test ?with-iterators.list
  (let ((result '()))
    (with-iterators (next '((1 2 3) (a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 A) (T 2 B) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; vector
  (let ((result '()))
    (with-iterators (next '((1 2 3) #(a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 A) (T 2 B) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; bit-vector
  (let ((result '()))
    (with-iterators (next '((1 2 3) #*10))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 1) (T 2 0) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; string
  (let ((result '()))
    (with-iterators (next '((1 2 3) "string"))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 #\s) (T 2 #\t) (T 3 #\r)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; lazy-sequence
  (let ((result '()))
    (with-iterators (next (list '(1 2 3) #[:foo :bar]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 :foo) (T 2 :bar) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  (let ((result '()))
    (with-iterators (next (list '(1 2 3) #[42 ..]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 42) (T 2 43) (T 3 44)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; hash-table
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '((1 2 3) #{:foo 42}))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 1 42) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; array
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '((1 2 3) #2A((a b) (c d))))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 1 a) (T 2 b) (T 3 c)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL))))))

(test ?with-iterators.vector
  (let ((result '()))
    (with-iterators (next '(#(1 2 3) (a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 A) (T 2 B) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; vector
  (let ((result '()))
    (with-iterators (next '(#(1 2 3) #(a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 A) (T 2 B) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; bit-vector
  (let ((result '()))
    (with-iterators (next '(#(1 2 3) #*10))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 1) (T 2 0) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; string
  (let ((result '()))
    (with-iterators (next '(#(1 2 3) "string"))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 #\s) (T 2 #\t) (T 3 #\r)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; lazy-sequence
  (let ((result '()))
    (with-iterators (next (list #(1 2 3) #[:foo :bar]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 :foo) (T 2 :bar) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  (let ((result '()))
    (with-iterators (next (list #(1 2 3) #[42 ..]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 42) (T 2 43) (T 3 44)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; hash-table
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '(#(1 2 3) #{:foo 42}))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 1 42) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; array
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '(#(1 2 3) #2A((a b) (c d))))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 1 A) (T 2 B) (T 3 C)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL))))))

(test ?with-iterators.bit-vector
  (let ((result '()))
    (with-iterators (next '(#*111 (a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 A) (T 1 B) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; vector
  (let ((result '()))
    (with-iterators (next '(#*111 #(a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 A) (T 1 B) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; bit-vector
  (let ((result '()))
    (with-iterators (next '(#*111 #*10))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 1) (T 1 0) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; string
  (let ((result '()))
    (with-iterators (next '(#*111 "string"))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 #\s) (T 1 #\t) (T 1 #\r)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; lazy-sequence
  (let ((result '()))
    (with-iterators (next (list #*111 #[:foo :bar]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 :foo) (T 1 :bar) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  (let ((result '()))
    (with-iterators (next (list #*111 #[42 ..]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 42) (T 1 43) (T 1 44)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; hash-table
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '(#*111 #{:foo 42}))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 1 42) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; array
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '(#*111 #2A((a b) (c d))))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 1 A) (T 1 B) (T 1 C)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL))))))

(test ?with-iterators.string
  (let ((result '()))
    (with-iterators (next '("str" (a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T #\s A) (T #\t B) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; vector
  (let ((result '()))
    (with-iterators (next '("str" #(a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T #\s A) (T #\t B) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; bit-vector
  (let ((result '()))
    (with-iterators (next '("str" #*10))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T #\s 1) (T #\t 0) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; string
  (let ((result '()))
    (with-iterators (next '("str" "string"))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T #\s #\s) (T #\t #\t) (T #\r #\r)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; lazy-sequence
  (let ((result '()))
    (with-iterators (next (list "str" #[:foo :bar]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T #\s :FOO) (T #\t :BAR) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  (let ((result '()))
    (with-iterators (next (list "str" #[42 ..]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T #\s 42) (T #\t 43) (T #\r 44)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; hash-table
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '("str" #{:foo 42}))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T #\s 42) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; array
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '("str" #2A((a b) (c d))))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T #\s a) (T #\t b) (T #\r c)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL))))))

(test ?with-iterators.lazy-sequence.finite
  ;; list
  (let ((result '()))
    (with-iterators (next (list #[1 2 3] '(a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 a) (T 2 b) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; vector
  (let ((result '()))
    (with-iterators (next (list #[1 2 3] #(a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 a) (T 2 b) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; bit-vector
  (let ((result '()))
    (with-iterators (next (list #[1 2 3] #*11))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 1) (T 2 1) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; string
  (let ((result '()))
    (with-iterators (next (list #[1 2 3] "string"))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 #\s) (T 2 #\t) (T 3 #\r)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; lazy-sequence
  (let ((result '()))
    (with-iterators (next (list #[1 2 3] #[:foo :bar]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 :foo) (T 2 :bar) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  (let ((result '()))
    (with-iterators (next (list #[1 2 3] #[:foo :bar ..]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 :foo) (T 2 :bar) (T 3 :foo)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; hash-table
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next (list #[1 2 3] #{:foo 42}))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 1 42) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))  
  ;; array
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next (list #[1 2 3] #2A((a b) (c d))))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 1 a) (T 2 b) (T 3 c)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL))))))

(test ?with-iterators.lazy-sequence.infinite
  ;; list
  (let ((result '()))
    (with-iterators (next (list #[42..] '(a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 42 a) (T 43 b) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; vector
  (let ((result '()))
    (with-iterators (next (list #[42..] #(a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 42 a) (T 43 b) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; bit-vector
  (let ((result '()))
    (with-iterators (next (list #[42..] #*11))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 42 1) (T 43 1) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; string
  (let ((result '()))
    (with-iterators (next (list #[42..] "string"))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 42 #\s) (T 43 #\t) (T 44 #\r)
                 (T 45 #\i) (T 46 #\n) (T 47 #\g)
                 (NIL) (NIL) (NIL) (NIL)))))
  ;; lazy-sequence
  (let ((result '()))
    (with-iterators (next (list #[42..] #[:foo :bar]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 42 :foo) (T 43 :bar) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  (let ((result '()))
    (with-iterators (next (list #[42..] #[:foo :bar :baz ..]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 42 :FOO) (T 43 :BAR) (T 44 :BAZ) (T 45 :FOO)
                 (T 46 :BAR) (T 47 :BAZ) (T 48 :FOO) (T 49 :BAR)
                 (T 50 :BAZ) (T 51 :FOO)))))
  ;; hash-table
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next (list #[42..] #{:foo 42}))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 42 42) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))  
  ;; array
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next (list #[42..] #2A((a b) (c d))))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 42 a) (T 43 b) (T 44 c) (T 45 d)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL))))))

(test ?with-iterators.hash-table
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '(#{:foo 42} (a b)))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 42 A) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; vector
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '(#{:foo 42} #(a b)))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 42 A) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; bit-vector
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '(#{:foo 42} #*10))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 42 1) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; string
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '(#{:foo 42} "string"))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 42 #\s) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; lazy-sequence
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next (list #{:foo 42} #[1 2]))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 42 1) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next (list #{:foo 42} #[42 ..]))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 42 42) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; hash-table
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '(#{:foo 42} #{:foo 42}))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 42 42) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; array
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '(#{:foo 42} #2A((a b) (c d))))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 42 A) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL))))))

(test ?with-iterators.array
  (let ((result '()))
    (with-iterators (next '(#2A((1 2) (3 4)) (a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 A) (T 2 B) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; vector
  (let ((result '()))
    (with-iterators (next '(#2A((1 2) (3 4)) #(a b)))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 A) (T 2 B) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; bit-vector
  (let ((result '()))
    (with-iterators (next '(#2A((1 2) (3 4)) #*10))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 1) (T 2 0) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; string
  (let ((result '()))
    (with-iterators (next '(#2A((1 2) (3 4)) "string"))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 #\s) (T 2 #\t) (T 3 #\r) (T 4 #\i)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; lazy-sequence
  (let ((result '()))
    (with-iterators (next (list #2A((1 2) (3 4)) #[1 2]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 1) (T 2 2) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  (let ((result '()))
    (with-iterators (next (list #2A((1 2) (3 4)) #[42 ..]))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T 1 42) (T 2 43) (T 3 44) (T 4 45)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; hash-table
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '(#2A((1 2) (3 4)) #{:foo 42}))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 1 42) (NIL) (NIL)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL)))))
  ;; array
  (let ((result '()))
    (with-muffle-warnings
      (with-iterators (next '(#2A((1 2) (3 4)) #2A((a b) (c d))))
        (dotimes (i 10)
          (push (multiple-value-list (next)) result))))
    (is (equal (nreverse result)
               '((T 1 a) (T 2 b) (T 3 c) (T 4 d)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL))))))

(test ?with-iterators.composit
  (let ((result '()))
    (with-iterators (next (list
                           '(a b c) #(i j k) #*111
                           "string" #[:foo :bar :baz] #[42..]
                           #2A((1 2) (3 4))))
      (dotimes (i 10)
        (push (multiple-value-list (next)) result)))
    (is (equal (nreverse result)
               '((T A I 1 #\s :FOO 42 1)
                 (T B J 1 #\t :BAR 43 2)
                 (T C K 1 #\r :BAZ 44 3)
                 (NIL) (NIL) (NIL) (NIL) (NIL) (NIL) (NIL))))))


;;--------------------------------------------------------------------
;; buxis, buxisp
;;--------------------------------------------------------------------

(test ?buxis.type
  (is-true  (typep #{:foo 0} 'buxis))
  (is-true  (typep '(0 1 2)  'buxis))
  (is-true  (typep #(0 1 2)  'buxis))
  (is-true  (typep #*1010    'buxis))
  (is-true  (typep "string"  'buxis))
  (is-true  (typep #[0 1 2]  'buxis))
  (is-true  (typep #2A((0 0)) 'buxis))
  (is-false (typep 42 'buxis))
  (is-false (typep #\a 'buxis))
  (is-false (typep 'symbol 'buxis)))


(test ?buxisp
  (is-true  (buxisp #{:foo 0}))
  (is-true  (buxisp '(0 1 2)))
  (is-true  (buxisp #(0 1 2)))
  (is-true  (buxisp #*1010))
  (is-true  (buxisp "string"))
  (is-true  (buxisp #[0 1 2]))
  (is-true  (buxisp #2A((0 0))))
  (is-false (buxisp 42))
  (is-false (buxisp #\a))
  (is-false (buxisp 'symbol)))


;;--------------------------------------------------------------------
;; dobux
;;--------------------------------------------------------------------

(test ?dobux.list
  (for-all ((lst (gen-list)))
    (let ((vals '()))
      (dobux (v lst)
        (push v vals))
      (is (equal (nreverse vals) lst))
      (is-true (dobux (v lst t) (declare (ignore v)))))))


(test ?dobux.vector
  (for-all ((lst (gen-list)))
    (let ((vec (coerce lst 'vector))
          (vals '()))
      (dobux (v vec)
        (push v vals))
      (is (equal (nreverse vals) lst))
      (is-true (dobux (v vec t) (declare (ignore v)))))))


(test ?dobux.lazy-sequence
  (for-all ((lst (gen-list)))
    (let ((lseq (lfor x :in lst))
          (vals '()))
      (dobux (v lseq)
        (push v vals))
      (is (equal (nreverse vals)
                 lst))
      (is-true (dobux (v lseq t) (declare (ignore v)))))))


(test ?dobux.array
  (for-all ((lst (gen-list)))
    (let ((ary (make-array (list 2 (length lst))
                           :initial-contents (list lst lst)))
          (vals '()))
      (dobux (v ary)
        (push v vals))
      (is (equal (nreverse vals)
                 (append lst lst)))
      (is-true (dobux (v ary t) (declare (ignore v)))))))


(test ?dobux.hash-table
  (for-all ((lst (gen-list)))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :from 0
                    :for v :in lst
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (vals '()))
      (with-muffle-warnings
        (dobux (v ht)
          (push v vals)))
      (is (equal (sort vals #'<)
                 (sort (copy-list lst) #'<)))
      (is-true (with-muffle-warnings
                 (dobux (v ht t) (declare (ignore v))))))))


;;--------------------------------------------------------------------
;; dobux2
;;--------------------------------------------------------------------

(test ?dobux2.list
  (for-all ((lst (gen-list)))
    (let ((tags '())
          (vals '()))
      (dobux2 (i v lst)
        (push i tags)
        (push v vals))
      (when lst
        (is (= (1+ (first tags))
               (length lst))))
      (is (equal (nreverse vals) lst))
      (is-true (dobux2 (i v lst t) (declare (ignore i v)))))))


(test ?dobux2.vector
  (for-all ((lst (gen-list)))
    (let ((vec (coerce lst 'vector))
          (tags '())
          (vals '()))
      (dobux2 (i v vec)
        (push i tags)
        (push v vals))
      (when lst
        (is (= (1+ (first tags))
               (length lst))))
      (is (equal (nreverse vals) lst))
      (is-true (dobux2 (i v vec t) (declare (ignore i v)))))))


(test ?dobux2.lazy-sequence
  (for-all ((lst (gen-list)))
    (let ((lseq (lfor x :in lst))
          (tags '())
          (vals '()))
      (dobux2 (i v lseq)
        (push i tags)
        (push v vals))
      (when lst
        (is (= (1+ (first tags))
               (length lst))))
      (is (equal (nreverse vals)
                 lst))
      (is-true (dobux2 (i v lseq t) (declare (ignore i v)))))))


(test ?dobux2.array
  (for-all ((lst (gen-list)))
    (let ((ary (make-array (list 2 (length lst))
                           :initial-contents (list lst lst)))
          (tags '())
          (vals '()))
      (dobux2 (i v ary)
        (push i tags)
        (push v vals))
      (when lst
        (is (= (1+ (first tags))
               (* 2 (length lst)))))
      (is (equal (nreverse vals)
                 (append lst lst)))
      (is-true (dobux2 (i v ary t) (declare (ignore i v)))))))


(test ?dobux2.hash-table
  (for-all ((lst (gen-list)))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :from 0
                    :for v :in lst
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (tags '())
          (vals '()))
      (with-muffle-warnings
        (dobux2 (k v ht)
          (push k tags)
          (push v vals)))
      (when lst
        (is (= (1+ (apply #'max tags))
               (length lst))))
      (is (equal (sort vals #'<)
                 (sort (copy-list lst) #'<)))
      (is-true (with-muffle-warnings
                 (dobux2 (k v ht t) (declare (ignore k v))))))))


;;--------------------------------------------------------------------
;; every*
;;--------------------------------------------------------------------

(test ?every*.list
  (is-true  (every* #'oddp  '(1 3 5)))
  (is-false (every* #'evenp '(1 3 5)))
  ;; list
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    '(1 3 5)
                    '(2 4 6)))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    '(1 3 5)
                    '(2 4 99999999)))
  ;; vector
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    '(1 3 5)
                    #(2 4 6)))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    '(1 3 5)
                    #(2 4 99999999)))
  ;; bit-vector
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    '(1 3 5)
                    #*000))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    '(1 3 5)
                    #*0001))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    '(1 3 5)
                    #*001))
  ;; string
  (is-true  (every* ^xy(and (oddp x) (digit-char-p y))
                    '(1 3 5)
                    "123"))
  (is-true  (every* ^xy(and (oddp x) (digit-char-p y))
                    '(1 3 5)
                    "123A"))
  (is-false (every* ^xy(and (oddp x) (digit-char-p y))
                    '(1 3 5)
                    "12A"))
  ;; lazy-sequence
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    '(1 3 5)
                    #[2 4 6]))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    '(1 3 5)
                    #[2 4 99999999]))
  
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (oddp x) (evenp y))
                     '(1 3 5)
                     #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (oddp x) (evenp y))
                     '(1 3 5)
                     #[2 3 ..]))
           nil))
  ;; array
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    '(1 3 5 7)
                    #2A((2 4) (6 8))))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    '(1 3 5)
                    #2A((2 4) (6 99999999))))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    '(1 3 5 7)
                    #2A((2 4) (6 99999999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      '(1 3 5)
                      #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      '(1 3 5)
                      #{:foo 0 :bar 2 :baz 9999999}))))


(test ?every*.vector
  (is-true  (every* #'oddp  #(1 3 5)))
  (is-false (every* #'evenp #(1 3 5)))
  ;; list
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #(1 3 5)
                    '(2 4 6)))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #(1 3 5)
                    '(2 4 99999999)))
  ;; vector
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #(1 3 5)
                    #(2 4 6)))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #(1 3 5)
                    #(2 4 99999999)))
  ;; bit-vector
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #(1 3 5)
                    #*000))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #(1 3 5)
                    #*0001))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #(1 3 5)
                    #*001))
  ;; string
  (is-true  (every* ^xy(and (oddp x) (digit-char-p y))
                    #(1 3 5)
                    "123"))
  (is-true  (every* ^xy(and (oddp x) (digit-char-p y))
                    #(1 3 5)
                    "123A"))
  (is-false (every* ^xy(and (oddp x) (digit-char-p y))
                    #(1 3 5)
                    "12A"))
  ;; lazy-sequence
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #(1 3 5)
                    #[2 4 6]))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #(1 3 5)
                    #[2 4 99999999]))
  
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (oddp x) (evenp y))
                     #(1 3 5)
                     #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (oddp x) (evenp y))
                     #(1 3 5)
                     #[2 3 ..]))
           nil))  
  ;; array
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #(1 3 5 7)
                    #2A((2 4) (6 8))))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #(1 3 5)
                    #2A((2 4) (6 99999999))))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #(1 3 5 7)
                    #2A((2 4) (6 99999999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      '(1 3 5)
                      #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      '(1 3 5)
                      #{:foo 0 :bar 2 :baz 9999999}))))


(test ?every*.bit-vector
  (is-true  (every* #'oddp  #*111))
  (is-false (every* #'evenp #*010))
  ;; list
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #*111
                    '(2 4 6)))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #*111
                    '(2 4 6 7)))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #*111
                    '(2 4 99999999)))
  ;; vector
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #*111
                    #(2 4 6)))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #*111
                    #(2 4 6 7)))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #*111
                    #(2 4 99999999)))
  ;; bit-vector
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #*111
                    #*000))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #*111
                    #*0001))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #*111
                    #*001))
  ;; string
  (is-true  (every* ^xy(and (oddp x) (digit-char-p y))
                    #*111
                    "123"))
  (is-true  (every* ^xy(and (oddp x) (digit-char-p y))
                    #*111
                    "123A"))
  (is-false (every* ^xy(and (oddp x) (digit-char-p y))
                    #*111
                    "12A"))
  ;; lazy-sequence
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #*111
                    #[2 4 6]))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #*111
                    #[2 4 99999999]))
  
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (oddp x) (evenp y))
                     #*111
                     #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (oddp x) (evenp y))
                     #*111
                     #[2 3 ..]))
           nil))
  ;; array
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #*1111
                    #2A((2 4) (6 8))))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #*111
                    #2A((2 4) (6 99999999))))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #*1111
                    #2A((2 4) (6 99999999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #*111
                      #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #*111
                      #{:foo 0 :bar 2 :baz 9999999}))))


(test ?every*.string
  (is-true  (every* #'digit-char-p  "123"))
  (is-false (every* #'digit-char-p  "1a3"))
  ;; list
  (is-true  (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    '(2 4 6)))
  (is-true  (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    '(2 4 6 9999999)))
  (is-false (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    '(2 4 99999999)))
  ;; vector
  (is-true  (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    #(2 4 6)))
  (is-true  (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    #(2 4 6 9999999)))
  (is-false (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    #(2 4 99999999)))  
  ;; bit-vector
  (is-true  (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    #*000))
  (is-true  (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    #*0001))
  (is-false (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    #*001))
  ;; string
  (is-true  (every* ^xy(and (digit-char-p x) (alpha-char-p y))
                    "123"
                    "abc"))
  (is-true  (every* ^xy(and (digit-char-p x) (alpha-char-p y))
                    "123"
                    "abc1"))
  (is-false (every* ^xy(and (digit-char-p x) (alpha-char-p y))
                    "123"
                    "ab3"))
  ;; lazy-sequence
  (is-true  (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    #[2 4 6]))
  (is-false (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    #[2 4 99999999]))
  
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (digit-char-p x) (evenp y))
                     "123"
                     #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (digit-char-p x) (evenp y))
                     "123"
                     #[2 3 ..]))
           nil))
  ;; array
  (is-true  (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    #2A((2 4) (6 8))))
  (is-true  (every* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    #2A((2 4) (6 99999999))))
  (is-false (every* ^xy(and (digit-char-p x) (evenp y))
                    "1234"
                    #2A((2 4) (6 99999999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (every* ^xy(and (digit-char-p x) (evenp y))
                      "123"
                      #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (every* ^xy(and (digit-char-p x) (evenp y))
                      "123"
                      #{:foo 0 :bar 2 :baz 9999999}))))


(test ?every*.lazy-seqnence
  ;; finite
  (is-true  (every* #'numberp #[1 2 3]))
  (is-false (every* #'numberp #[1 :foo 3]))
  ;; inifite
  (is (eql (with-time-limit (1 :boom!)
             (every* #'numberp  #[1 ..]))
           :boom!))
  (is (eql (with-time-limit (1 :boom!)
             (every* #'numberp  #[#\a #\b ..]))
           nil))
  ;; list
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #[1 3 5]
                    '(2 4 6)))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #[1 3 5]
                    '(2 4 6 7)))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #[1 3 5]
                    '(2 4 99999999)))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (numberp x) (evenp y))
                     #[1 ..]
                     '(2 4 6)))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (numberp x) (evenp y))
                     #[1 ..]
                     '(2 4 9999999)))
           nil))
  ;; vector
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #[1 3 5]
                    #(2 4 6)))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #[1 3 5]
                    #(2 4 6 7)))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #[1 3 5]
                    #(2 4 99999999)))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (numberp x) (evenp y))
                     #[1 ..]
                     #(2 4 6)))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (numberp x) (evenp y))
                     #[1 ..]
                     #(2 4 9999999)))
           nil))
  ;; bit-vector
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #[1 3 5]
                    #*000))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #[1 3 5]
                    #*0001))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #[1 3 5]
                    #*001))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (numberp x) (evenp y))
                     #[1 ..]
                     #*000))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (numberp x) (evenp y))
                     #[1 ..]
                     #*001))
           nil))
  ;; string
  (is-true  (every* ^xy(and (oddp x) (digit-char-p y))
                    #[1 3 5]
                    "123"))
  (is-true  (every* ^xy(and (oddp x) (digit-char-p y))
                    #[1 3 5]
                    "123A"))
  (is-false (every* ^xy(and (oddp x) (digit-char-p y))
                    #[1 3 5]
                    "12A"))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (numberp x) (digit-char-p y))
                     #[1 ..]
                     "123"))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (numberp x) (digit-char-p y))
                     #[1 ..]
                     "123A"))
           nil))
  ;; lazy-sequence
  (is-true  (every* ^xy(and (numberp x) (alpha-char-p y))
                    #[1 2 3]
                    #[#\a #\b #\c]))
  (is-true  (every* ^xy(and (numberp x) (alpha-char-p y))
                    #[1 2 3]
                    #[#\a #\b #\c 1]))

  (is-true  (every* ^xy(and (numberp x) (alpha-char-p y))
                    #[1 2 3]
                    #[#\a #\b #\c 1]))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (oddp x) (evenp y))
                     #[1 3 ..]
                     #[2 4 ..]))
           :boom!))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (oddp x) (evenp y))
                     #[1 3 ..]
                     #[2 4 5 ..]))
           nil))
  ;; array
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #[1 3 5]
                    #2A((2 4) (6 8))))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #[1 3 5]
                    #2A((2 4) (6 99999999))))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #[1 3 5 7]
                    #2A((2 4) (6 99999999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #[1 3 5]
                      #{:foo 0 :bar 2 :baz 4})))
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #[1 3 5 8]
                      #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #[1 3 5]
                      #{:foo 0 :bar 2 :baz 9999999}))))


(test ?every*.hash-table
  (is-true  (every* #'oddp  #{:foo 1 :bar 3 :baz 5}))
  (is-false (every* #'evenp #{:foo 1 :bar 3 :baz 5}))
  ;; list
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      '(2 4 6))))
  (is-false (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      '(2 4 999999999))))
  ;; vector
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      #(2 4 6))))
  (is-false (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      #(2 4 999999999))))
  ;; bit-vector
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      #*000)))
  (is-false (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      #*001)))
  ;; string
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (digit-char-p y))
                      #{:foo 1 :bar 3 :baz 5}
                      "246")))
  (is-false (with-muffle-warnings
              (every* ^xy(and (oddp x) (digit-char-p y))
                      #{:foo 1 :bar 3 :baz 5}
                      "24A")))
  ;; lazy-sequence
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      #[2 4 6])))
  (is-false (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      #[2 4 99999999])))
  (is (eql (with-time-limit (1 :boom!)
             (with-muffle-warnings
               (every* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5}
                       #[2 4 ..])))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (with-muffle-warnings

               (every* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5}
                       #[2 3 ..])))
           nil))
  ;; array
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      #2A((2 4) (6 8)))))
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      #2A((2 4) (6 99999999)))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      #{:who 2 :how 4 :when 6})))
  (is-false (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      #{:who 2 :how 4 :when 99999999}))))


(test ?every*.array
  (is-true  (every* #'oddp  #2A((1 3) (5 7))))
  (is-false (every* #'evenp #2A((1 3) (5 7))))
  ;; list
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    '(2 4 6 8)))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    '(2 4 6 8 9999999)))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    '(2 4 6 99999999)))
  ;; vector
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    #(2 4 6 8)))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    #(2 4 6 8 9999999)))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    #(2 4 6 99999999)))
  ;; bit-vector
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    #*0000))
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    #*00001))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    #*0001))
  ;; string
  (is-true  (every* ^xy(and (oddp x) (digit-char-p y))
                    #2A((1 3) (5 7))
                    "1234"))
  (is-true  (every* ^xy(and (oddp x) (digit-char-p y))
                    #2A((1 3) (5 7))
                    "1234A"))
  (is-false (every* ^xy(and (oddp x) (digit-char-p y))
                    #2A((1 3) (5 7))
                    "123A"))
  ;; lazy-sequence
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    #[2 4 6 8]))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    #[2 4 6 99999999]))
  
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (oddp x) (evenp y))
                     #2A((1 3) (5 7))
                     #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (every* ^xy(and (oddp x) (evenp y))
                     #2A((1 3) (5 7))
                     #[2 3 ..]))
           nil))
  ;; array
  (is-true  (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    #2A((2 4) (6 8))))
  (is-false (every* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    #2A((2 4) (6 99999999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #2A((1 3) (5 7))
                      #{:foo 0 :bar 2 :baz 4})))
  (is-true  (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #2A((1 3) (5 42))
                      #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (every* ^xy(and (oddp x) (evenp y))
                      #2A((1 3) (5 7))
                      #{:foo 0 :bar 2 :baz 9999999}))))


;;--------------------------------------------------------------------
;; notevery*
;;--------------------------------------------------------------------
;; TODO:


;;--------------------------------------------------------------------
;; some*
;;--------------------------------------------------------------------

(test ?some*.list
  (is-true  (some* #'oddp  '(1 3 5)))
  (is-false (some* #'evenp '(1 3 5)))
  ;; list
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   '(1 3 5)
                   '(2 4 6)))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   '(1 3 5)
                   '(99 999 9999)))
  ;; vector
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   '(1 3 5)
                   #(2 4 6)))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   '(1 3 5)
                   #(99 999 9999)))
  ;; bit-vector
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   '(1 3 5)
                   #*000))
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   '(1 3 5)
                   #*0001))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   '(1 3 5)
                   #*111))
  ;; string
  (is-true  (some* ^xy(and (oddp x) (digit-char-p y))
                   '(1 3 5)
                   "123"))
  (is-true  (some* ^xy(and (oddp x) (digit-char-p y))
                   '(1 3 5)
                   "123A"))
  (is-false (some* ^xy(and (oddp x) (digit-char-p y))
                   '(1 3 5)
                   "ABC"))
  ;; lazy-sequence
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   '(1 3 5)
                   #[2 4 6]))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   '(1 3 5)
                   #[9 99 999]))
  
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (oddp x) (evenp y))
                    '(1 3 5)
                    #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (oddp x) (evenp y))
                    '(1 3 5)
                    #[3 5 ..]))
           nil))
  ;; array
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   '(1 3 5 7)
                   #2A((2 4) (6 8))))
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   '(1 3 5)
                   #2A((9 99) (222 9999))))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   '(1 3 5 7)
                   #2A((9 99) (999 9999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     '(1 3 5)
                     #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     '(1 3 5)
                     #{:foo 9 :bar 99 :baz 9999}))))


(test ?some*.vector
  (is-true  (some* #'oddp  #(1 3 5)))
  (is-false (some* #'evenp #(1 3 5)))
  ;; list
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #(1 3 5)
                   '(2 4 6)))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #(1 3 5)
                   '(9 99 999)))
  ;; vector
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #(1 3 5)
                   #(2 4 6)))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #(1 3 5)
                   #(9 99 999)))
  ;; bit-vector
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #(1 3 5)
                   #*000))
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #(1 3 5)
                   #*0001))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #(1 3 5)
                   #*111))
  ;; string
  (is-true  (some* ^xy(and (oddp x) (digit-char-p y))
                   #(1 3 5)
                   "123"))
  (is-true  (some* ^xy(and (oddp x) (digit-char-p y))
                   #(1 3 5)
                   "123A"))
  (is-false (some* ^xy(and (oddp x) (digit-char-p y))
                   #(1 3 5)
                   "ABC"))
  ;; lazy-sequence
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #(1 3 5)
                   #[2 4 6]))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #(1 3 5)
                   #[9 99 999]))
  
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (oddp x) (evenp y))
                    #(1 3 5)
                    #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (oddp x) (evenp y))
                    #(1 3 5)
                    #[3 5 ..]))
           nil))  
  ;; array
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #(1 3 5 7)
                   #2A((2 4) (6 8))))
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #(1 3 5)
                   #2A((2 4) (6 99999999))))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #(1 3 5 7)
                   #2A((9 99) (999 9999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     '(1 3 5)
                     #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     '(1 3 5)
                     #{:foo 9 :bar 99 :baz 999}))))


(test ?some*.bit-vector
  (is-true  (some* #'oddp  #*111))
  (is-false (some* #'evenp #*111))
  ;; list
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                    #*111
                    '(2 4 6)))
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                    #*111
                    '(2 4 6 7)))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                    #*111
                    '(9 99 999)))
  ;; vector
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                    #*111
                    #(2 4 6)))
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                    #*111
                    #(2 4 6 7)))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                    #*111
                    #(9 99 999)))
  ;; bit-vector
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                    #*111
                    #*000))
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                    #*111
                    #*0001))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                    #*111
                    #*111))
  ;; string
  (is-true  (some* ^xy(and (oddp x) (digit-char-p y))
                    #*111
                    "123"))
  (is-false (some* ^xy(and (oddp x) (digit-char-p y))
                    #*111
                    "ABC"))
  ;; lazy-sequence
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #*111
                   #[2 4 6]))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #*111
                   #[9 99 999]))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (oddp x) (evenp y))
                     #*111
                     #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (oddp x) (evenp y))
                     #*111
                     #[3 5 ..]))
           nil))
  ;; array
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                    #*1111
                    #2A((2 4) (6 8))))
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                    #*111
                    #2A((2 4) (6 99999999))))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                    #*1111
                    #2A((9 99) (999 9999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                      #*111
                      #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                      #*111
                      #{:foo 9 :bar 99 :baz 999}))))


(test ?some*.string
  (is-true  (some* #'digit-char-p  "123"))
  (is-false (some* #'digit-char-p  "ABC"))
  ;; list
  (is-true  (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   '(2 4 6)))
  (is-false (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   '(9 99 999)))
  (is-false (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   '(9 99 999 2)))
  ;; vector
  (is-true  (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   #(2 4 6)))
  (is-false (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   #(9 99 999)))
  (is-false (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   #(9 99 999 2)))  
  ;; bit-vector
  (is-true  (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   #*000))
  (is-false (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   #*111))
  (is-false (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   #*1110))
  ;; string
  (is-true  (some* ^xy(and (digit-char-p x) (alpha-char-p y))
                   "123"
                   "abc"))
  (is-false (some* ^xy(and (digit-char-p x) (alpha-char-p y))
                   "123"
                   "123"))
  (is-false (some* ^xy(and (digit-char-p x) (alpha-char-p y))
                   "123"
                   "123a"))
  ;; lazy-sequence
  (is-true  (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   #[2 4 6]))
  (is-false (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   #[9 99 999]))
  (is-false (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   #[9 99 999 2]))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (digit-char-p x) (evenp y))
                    "123"
                    #[3 5 ..]))
           nil))
  ;; array
  (is-true  (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   #2A((2 4) (6 8))))
  (is-true  (some* ^xy(and (digit-char-p x) (evenp y))
                   "123"
                   #2A((2 4) (6 99999999))))
  (is-false (some* ^xy(and (digit-char-p x) (evenp y))
                   "1234"
                   #2A((9 99) (999 9999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (some* ^xy(and (digit-char-p x) (evenp y))
                     "123"
                     #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (some* ^xy(and (digit-char-p x) (evenp y))
                     "123"
                     #{:foo 9 :bar 99 :baz 999}))))


(test ?some*.lazy-seqnence
  ;; finite
  (is-true  (some* #'numberp #[1 2 3]))
  (is-false (some* #'numberp #[:foo :bar :baz]))
  ;; inifite
  (is (eql (with-time-limit (1 :boom!)
             (some* #'numberp  #[1 ..]))
           T))
  (is (eql (with-time-limit (1 :boom!)
             (some* #'numberp  #[#\a #\b ..]))
           :boom!))
  ;; list
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #[1 3 5]
                   '(2 4 6)))
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #[1 3 5]
                   '(2 4 6 7)))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #[1 3 5]
                   '(9 99 999)))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (numberp x) (evenp y))
                    #[1 ..]
                    '(2 4 6)))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (numberp x) (evenp y))
                    #[1 ..]
                    '(9 99 999)))
           nil))
  ;; vector
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #[1 3 5]
                   #(2 4 6)))
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #[1 3 5]
                   #(2 4 6 7)))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #[1 3 5]
                   #(9 99 999)))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (numberp x) (evenp y))
                    #[1 ..]
                    #(2 4 6)))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (numberp x) (evenp y))
                    #[1 ..]
                    #(9 99 999)))
           nil))
  ;; bit-vector
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #[1 3 5]
                   #*000))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #[1 3 5]
                   #*111))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #[1 3 5]
                   #*1110))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (numberp x) (evenp y))
                    #[1 ..]
                    #*000))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (numberp x) (evenp y))
                    #[1 ..]
                    #*111))
           nil))
  ;; string
  (is-true  (some* ^xy(and (oddp x) (digit-char-p y))
                   #[1 3 5]
                   "123"))
  (is-false (some* ^xy(and (oddp x) (digit-char-p y))
                   #[1 3 5]
                   "ABC"))
  (is-false (some* ^xy(and (oddp x) (digit-char-p y))
                   #[1 3 5]
                   "ABC1"))
  
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (numberp x) (digit-char-p y))
                    #[1 ..]
                    "123"))
           1))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (numberp x) (digit-char-p y))
                    #[1 ..]
                    "123A"))
           1))
  ;; lazy-sequence
  (is-true  (some* ^xy(and (numberp x) (alpha-char-p y))
                   #[1 2 3]
                   #[#\a #\b #\c]))
  (is-true  (some* ^xy(and (numberp x) (alpha-char-p y))
                   #[1 2 3]
                   #[#\a #\b #\c 1]))

  (is-true  (some* ^xy(and (numberp x) (alpha-char-p y))
                   #[1 2 3]
                   #[#\a #\b #\c 1]))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (oddp x) (evenp y))
                    #[2 4 ..]
                    #[1 3 ..]))
           :boom!))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (oddp x) (evenp y))
                    #[1 3 ..]
                    #[5 7 ..]))
           :boom!))
  ;; array
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #[1 3 5]
                   #2A((2 4) (6 8))))
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #[1 3 5]
                   #2A((2 4) (6 99999999))))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #[1 3 5 7]
                   #2A((9 99) (999 9999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #[1 3 5]
                     #{:foo 0 :bar 2 :baz 4})))
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #[1 3 5 8]
                     #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #[1 3 5]
                     #{:foo 9 :bar 99 :baz 999}))))


(test ?some*.hash-table
  (is-true  (some* #'oddp  #{:foo 1 :bar 3 :baz 5}))
  (is-false (some* #'evenp #{:foo 1 :bar 3 :baz 5}))
  ;; list
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #{:foo 1 :bar 3 :baz 5}
                     '(2 4 6))))
  (is-false (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #{:foo 1 :bar 3 :baz 5}
                     '(99 999 999999999))))
  ;; vector
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #{:foo 1 :bar 3 :baz 5}
                     #(2 4 6))))
  (is-false (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #{:foo 1 :bar 3 :baz 5}
                     #(99 999 999999999))))
  ;; bit-vector
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #{:foo 1 :bar 3 :baz 5}
                     #*000)))
  (is-false (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #{:foo 1 :bar 3 :baz 5}
                     #*111)))
  ;; string
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (digit-char-p y))
                     #{:foo 1 :bar 3 :baz 5}
                     "246")))
  (is-false (with-muffle-warnings
              (some* ^xy(and (oddp x) (digit-char-p y))
                     #{:foo 1 :bar 3 :baz 5}
                     "ABC")))
  ;; lazy-sequence
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #{:foo 1 :bar 3 :baz 5}
                     #[2 4 6])))
  (is-false (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #{:foo 1 :bar 3 :baz 5}
                     #[99 999 9999])))
  (is (eql (with-time-limit (1 :boom!)
             (with-muffle-warnings
               (some* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      #[2 4 ..])))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (with-muffle-warnings
               (some* ^xy(and (oddp x) (evenp y))
                      #{:foo 1 :bar 3 :baz 5}
                      #[3 5 ..])))
           nil))
  ;; array
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #{:foo 1 :bar 3 :baz 5}
                     #2A((2 4) (6 8)))))
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #{:foo 1 :bar 3 :baz 5}
                     #2A((2 4) (6 99999999)))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #{:foo 1 :bar 3 :baz 5}
                     #{:who 2 :how 4 :when 6})))
  (is-false (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #{:foo 1 :bar 3 :baz 5}
                     #{:who 99 :how 999 :when 9999}))))


(test ?some*.array
  (is-true  (some* #'oddp  #2A((1 3) (5 7))))
  (is-false (some* #'evenp #2A((1 3) (5 7))))
  ;; list
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   '(2 4 6 8)))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   '(9 99 999 9999)))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   '(9 99 999 9999 2)))
  ;; vector
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   #(2 4 6 8)))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   #(9 99 999 9999)))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   #(9 99 999 9999 2)))
  ;; bit-vector
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   #*0000))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   #*1111))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   #*11110))
  ;; string
  (is-true  (some* ^xy(and (oddp x) (digit-char-p y))
                   #2A((1 3) (5 7))
                   "1234"))
  (is-false (some* ^xy(and (oddp x) (digit-char-p y))
                   #2A((1 3) (5 7))
                   "ABCD"))
  (is-false (some* ^xy(and (oddp x) (digit-char-p y))
                   #2A((1 3) (5 7))
                   "ABCD1"))
  ;; lazy-sequence
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   #[2 4 6 8]))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   #[9 99 999 9999]))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   #[9 99 999 9999 2]))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (some* ^xy(and (oddp x) (evenp y))
                    #2A((1 3) (5 7))
                    #[3 5 ..]))
           nil))
  ;; array
  (is-true  (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   #2A((2 4) (6 8))))
  (is-false (some* ^xy(and (oddp x) (evenp y))
                   #2A((1 3) (5 7))
                   #2A((9 99) (999 9999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #2A((1 3) (5 7))
                     #{:foo 0 :bar 2 :baz 4})))
  (is-true  (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #2A((1 3) (5 42))
                     #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (some* ^xy(and (oddp x) (evenp y))
                     #2A((1 3) (5 7))
                     #{:foo 9 :bar 99 :baz 999}))))


;;--------------------------------------------------------------------
;; notany*
;;--------------------------------------------------------------------

(test ?notany*.list
  (is-true  (notany* #'evenp '(1 3 5)))
  (is-false (notany* #'oddp  '(1 3 5)))
  ;; list
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     '(2 4 6)
                     '(1 3 5)))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     '(2 4 5)
                     '(1 3 6)))
  ;; vector
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     '(2 4 6)
                     #(1 3 5)))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     '(2 4 5)
                     #(1 3 6)))
  ;; bit-vector
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     '(2 4 6)
                     #*000))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     '(2 4 6)
                     #*0001))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     '(2 4 5)
                     #*000))
  ;; string
  (is-true  (notany* ^xy(and (oddp x) (digit-char-p y))
                     '(2 4 6)
                     "123"))
  (is-true  (notany* ^xy(and (oddp x) (digit-char-p y))
                     '(2 4 6)
                     "123A"))
  (is-false (notany* ^xy(and (oddp x) (digit-char-p y))
                     '(2 4 7)
                     "AB1"))
  ;; lazy-sequence
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     '(2 4 6)
                     #[1 3 5]))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     '(2 4 5)
                     #[1 3 6]))
  
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (oddp x) (evenp y))
                      '(2 4 6)
                      #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (oddp x) (evenp y))
                      '(2 4 7)
                      #[2 4 ..]))
           nil))
  ;; array
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     '(2 4 6 8)
                     #2A((1 3) (5 7))))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     '(2 4 6)
                     #2A((2 4) (6 99999999))))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     '(1 3 5 7)
                     #2A((2 4) (6 99999999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       '(2 4 6)
                       #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       '(2 4 5)
                       #{:foo 2 :bar 4 :baz 6}))))


(test ?notany*.vector
  (is-true  (notany* #'evenp #(1 3 5)))
  (is-false (notany* #'oddp  #(1 3 5)))
  ;; list
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #(2 4 6)
                     '(1 3 5)))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #(2 4 5)
                     '(1 3 6)))
  ;; vector
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #(2 4 6)
                     #(1 3 5)))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #(2 4 5)
                     #(1 3 6)))
  ;; bit-vector
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #(2 4 6)
                     #*000))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #(2 4 6)
                     #*0001))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #(2 4 5)
                     #*000))
  ;; string
  (is-true  (notany* ^xy(and (oddp x) (digit-char-p y))
                     #(2 4 6)
                     "123"))
  (is-true  (notany* ^xy(and (oddp x) (digit-char-p y))
                     #(2 4 6)
                     "123A"))
  (is-false (notany* ^xy(and (oddp x) (digit-char-p y))
                     #(2 4 7)
                     "AB1"))
  ;; lazy-sequence
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #(2 4 6)
                     #[1 3 5]))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #(2 4 5)
                     #[1 3 6]))
  
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (oddp x) (evenp y))
                      #(2 4 6)
                      #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (oddp x) (evenp y))
                      #(2 4 7)
                      #[2 4 ..]))
           nil))
  ;; array
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #(2 4 6 8)
                     #2A((1 3) (5 7))))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #(2 4 6)
                     #2A((2 4) (6 99999999))))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #(1 3 5 7)
                     #2A((2 4) (6 99999999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #(2 4 6)
                       #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #(2 4 5)
                       #{:foo 2 :bar 4 :baz 6}))))


(test ?notany*.bit-vector
  (is-true  (notany* #'evenp #*111))
  (is-false (notany* #'oddp  #*111))
  ;; list
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #*000
                     '(1 3 5)))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #*001
                     '(1 3 6)))
  ;; vector
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #*000
                     #(1 3 5)))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #*001
                     #(1 3 6)))
  ;; bit-vector
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #*000
                     #*000))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #*000
                     #*0001))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #*001
                     #*000))
  ;; string
  (is-true  (notany* ^xy(and (oddp x) (digit-char-p y))
                     #*000
                     "123"))
  (is-true  (notany* ^xy(and (oddp x) (digit-char-p y))
                     #*000
                     "123A"))
  (is-false (notany* ^xy(and (oddp x) (digit-char-p y))
                     #*001
                     "AB1"))
  ;; lazy-sequence
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #*000
                     #[1 3 5]))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #*001
                     #[1 3 6]))
  
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (oddp x) (evenp y))
                      #*000
                      #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (oddp x) (evenp y))
                      #*001
                      #[2 4 ..]))
           nil))
  ;; array
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #*0000
                     #2A((1 3) (5 7))))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #*000
                     #2A((2 4) (6 99999999))))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #*1111
                     #2A((2 4) (6 99999999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #*000
                       #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #*001
                       #{:foo 2 :bar 4 :baz 6}))))


(test ?notany*.string
  (is-true  (notany* #'alpha-char-p  "123"))
  (is-false (notany* #'alpha-char-p  "12A"))
  ;; list
  (is-true  (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "123"
                     '(2 4 6)))
  (is-true  (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "123A"
                     '(2 4 6)))
  (is-false (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "12A"
                     '(2 4 6)))
  ;; vector
  (is-true  (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "123"
                     #(2 4 6)))
  (is-true  (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "123A"
                     #(2 4 6)))
  (is-false (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "12A"
                     #(2 4 6)))
  ;; bit-vector
  (is-true  (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "123"
                     #*000))
  (is-true  (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "123A"
                     #*000))
  (is-false (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "12A"
                     #*000))
  ;; string
  (is-true  (notany* ^xy(and (alpha-char-p x) (alpha-char-p y))
                     "123"
                     "ABC"))
  (is-true  (notany* ^xy(and (alpha-char-p x) (alpha-char-p y))
                     "123"
                     "ABC1"))
  (is-false (notany* ^xy(and (alpha-char-p x) (alpha-char-p y))
                     "12X"
                     "ABC"))
  ;; lazy-sequence
  (is-true  (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "123"
                     #[2 4 6]))
  (is-false (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "12A"
                     #[2 4 6]))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (alpha-char-p x) (evenp y))
                      "123"
                      #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (alpha-char-p x) (evenp y))
                      "123A"
                      #[2 4 ..]))
           nil))
  ;; array
  (is-true  (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "123"
                     #2A((2 4) (6 8))))
  (is-true  (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "1234A"
                     #2A((2 4) (6 8))))
  (is-false (notany* ^xy(and (alpha-char-p x) (evenp y))
                     "123A"
                     #2A((2 4) (6 8))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (alpha-char-p x) (evenp y))
                       "123"
                       #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (alpha-char-p x) (evenp y))
                       "12A"
                       #{:foo 0 :bar 2 :baz 6}))))


(test ?notany*.lazy-seqnence
  ;; finite
  (is-true  (notany* #'symbolp #[1 2 3]))
  (is-false (notany* #'symbolp #[1 :foo 3]))
  ;; inifite
  (is (eql (with-time-limit (1 :boom!)
             (notany* #'symbolp  #[1 ..]))
           :boom!))
  (is (eql (with-time-limit (1 :boom!)
             (notany* #'symbolp  #[#\a :foo ..]))
           nil))
  ;; list
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #[2 4 6]
                     '(2 4 6)))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #[2 4 6 7]
                     '(2 4 6)))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #[2 4 6 7]
                     '(2 4 6 8)))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (symbolp x) (evenp y))
                      #[1 ..]
                      '(2 4 6)))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (symbolp x) (evenp y))
                      #[:foo ..]
                      '(1 3 6)))
           nil))
  ;; vector
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #[2 4 6]
                     #(2 4 6)))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #[2 4 6]
                     #(2 4 6 8)))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #[2 4 6 7]
                     #(2 4 6 8)))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (symbolp x) (evenp y))
                      #[1 ..]
                      #(2 4 6)))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (symbolp x) (evenp y))
                      #[:foo ..]
                      #(1 3 4)))
           nil))
  ;; bit-vector
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #[2 4 6]
                     #*000))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #[2 4 6]
                     #*0001))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #[2 4 7]
                     #*000))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (symbolp x) (evenp y))
                      #[1 ..]
                      #*000))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (symbolp x) (evenp y))
                      #[:foo ..]
                      #*110))
           nil))
  ;; string
  (is-true  (notany* ^xy(and (oddp x) (digit-char-p y))
                     #[2 4 6]
                     "123"))
  (is-true  (notany* ^xy(and (oddp x) (digit-char-p y))
                     #[2 4 6]
                     "ABC1"))
  (is-false (notany* ^xy(and (oddp x) (digit-char-p y))
                     #[2 4 6 7]
                     "ABC1"))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (symbolp x) (digit-char-p y))
                      #[1 ..]
                      "123"))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (symbolp x) (digit-char-p y))
                      #[:foo ..]
                      "ABC1"))
           nil))
  ;; lazy-sequence
  (is-true  (notany* ^xy(and (symbolp x) (alpha-char-p y))
                     #[1 2 3]
                     #[#\a #\b #\c]))
  (is-true  (notany* ^xy(and (symbolp x) (alpha-char-p y))
                     #[1 2 3]
                     #[#\a #\b #\c 1]))

  (is-true  (notany* ^xy(and (symbolp x) (alpha-char-p y))
                     #[1 2 3]
                     #[#\a #\b #\c 1]))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (oddp x) (evenp y))
                      #[2 4 ..]
                      #[1 3 ..]))
           :boom!))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (oddp x) (evenp y))
                      #[1 3 ..]
                      #[2 4 5 ..]))
           nil))
  ;; array
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #[2 4 6]
                     #2A((2 4) (6 8))))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #[2 4 6]
                     #2A((2 4) (6 99999999))))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #[1 3 5 7]
                     #2A((2 4) (6 99999999))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #[2 4 6]
                       #{:foo 0 :bar 2 :baz 4})))
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #[2 4 6]
                       #{:foo 0 :bar 2 :baz 4 :quux 6})))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #[2 4 6 7]
                       #{:foo 0 :bar 2 :baz 4 :quux 6}))))


(test ?notany*.hash-table
  (is-true  (notany* #'evenp #{:foo 1 :bar 3 :baz 5}))
  (is-false (notany* #'evenp #{:foo 1 :bar 2 :baz 5}))
  ;; list
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5}
                       '(9 9 999))))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5}
                       '(9 9 2))))
  ;; vector
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5}
                       #(9 9 999))))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5}
                       #(9 9 2))))
  ;; bit-vector
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5}
                       #*111)))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5}
                       #*001)))
  ;; string
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (digit-char-p y))
                       #{:foo 1 :bar 3 :baz 5}
                       "ABC")))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (oddp x) (digit-char-p y))
                       #{:foo 1 :bar 3 :baz 5}
                       "24A")))
  ;; lazy-sequence
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5}
                       #[9 99 999])))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5}
                       #[2 4 99999999])))
  (is (eql (with-time-limit (1 :boom!)
             (with-muffle-warnings
               (notany* ^xy(and (oddp x) (evenp y))
                        #{:foo 1 :bar 3 :baz 5}
                        #[1 3 ..])))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (with-muffle-warnings
               (notany* ^xy(and (oddp x) (evenp y))
                        #{:foo 1 :bar 3 :baz 5}
                        #[2 3 ..])))
           nil))
  ;; array
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5}
                       #2A((9 99) (999 9999)))))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5 :quux 6}
                       #2A((9 99) (999 2)))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #{:who 2 :how 4 :when 6}
                       #{:foo 1 :bar 3 :baz 5})))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #{:foo 1 :bar 3 :baz 5}
                       #{:who 2 :how 4 :when 99999999}))))


(test ?notany*.array
  (is-true  (notany* #'oddp  #2A((2 4) (6 8))))
  (is-false (notany* #'evenp #2A((2 4) (6 8))))
  ;; list
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 8))
                     '(1 3 5 7)))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 7))
                     '(1 3 5)))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 7))
                     '(1 3 5 8)))
  ;; vector
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 8))
                     #(2 4 6 8)))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 7))
                     #(2 4 6)))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 7))
                     #(2 4 6 8)))
  ;; bit-vector
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 8))
                     #*0000))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 7))
                     #*000))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 7))
                     #*0000))
  ;; string
  (is-true  (notany* ^xy(and (oddp x) (digit-char-p y))
                     #2A((2 4) (6 8))
                     "ABCD"))
  (is-true  (notany* ^xy(and (oddp x) (digit-char-p y))
                     #2A((2 4) (6 7))
                     "ABC"))
  (is-false (notany* ^xy(and (oddp x) (digit-char-p y))
                     #2A((2 4) (6 7))
                     "ABC1"))
  ;; lazy-sequence
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 8))
                     #[2 4 6 8]))
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 7))
                     #[2 4 6]))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 7))
                     #[2 4 6 8]))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (oddp x) (evenp y))
                      #2A((2 4) (6 8))
                      #[2 4 ..]))
           t))
  (is (eql (with-time-limit (1 :boom!)
             (notany* ^xy(and (oddp x) (evenp y))
                      #2A((2 4) (6 7))
                      #[2 4 ..]))
           nil))
  ;; array
  (is-true  (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 8))
                     #2A((2 4) (6 8))))
  (is-false (notany* ^xy(and (oddp x) (evenp y))
                     #2A((2 4) (6 7))
                     #2A((2 4) (6 8))))
  ;; hash-table
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #2A((2 4) (6 8))
                       #{:foo 0 :bar 2 :baz 4})))
  (is-true  (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #2A((2 4) (6 7))
                       #{:foo 0 :bar 2 :baz 4})))
  (is-false (with-muffle-warnings
              (notany* ^xy(and (oddp x) (evenp y))
                       #2A((2 4) (6 7))
                       #{:foo 0 :bar 2 :baz 4 :quux 6}))))


;;--------------------------------------------------------------------
;; map*
;;--------------------------------------------------------------------
;; TODO:

(test ?map*.error
  (signals type-error (map* :foo #'1+ '(0 1 2 3)))
  (signals type-error (map* t    #()  '(0 1 2 3)))
  (signals type-error (map* t    #'1+  :foo))
  (signals type-error (map* t    #'1+  '(0 1 2 3) :foo)))

(test ?map*.list
  (for-all ((lst0 (gen-list))
            (lst1 (gen-list)))
    (is-equal  (map*  t      #'1+ lst0)
               (map  'list   #'1+ lst0))
    (is-equal  (map*  nil    #'1+ lst0)
               (map   nil    #'1+ lst0))
    (is-equal  (map* 'list   #'1+ lst0)
               (map  'list   #'1+ lst0))
    (is-equalp (map* 'vector #'1+ lst0)
               (map  'vector #'1+ lst0))
    (is-equal  (map* 'bit-vector (lambda (x) (if (plusp x) 1 0)) lst0)
               (map  'bit-vector (lambda (x) (if (plusp x) 1 0)) lst0))
    (is-equal  (map* 'string (lambda (x) (code-char (abs x))) lst0)
               (map  'string (lambda (x) (code-char (abs x))) lst0))

    ;; list
    (is-equal  (map*  t      #'+ lst0 lst1)
               (map  'list   #'+ lst0 lst1))
    (is-equal  (map*  nil    #'+ lst0 lst1)
               (map   nil    #'+ lst0 lst1))
    (is-equal  (map* 'list   #'+ lst0 lst1)
               (map  'list   #'+ lst0 lst1))
    (is-equalp (map* 'vector #'+ lst0 lst1)
               (map  'vector #'+ lst0 lst1))
    (is-equal  (map* 'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) lst0 lst1)
               (map  'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) lst0 lst1))
    (is-equal  (map* 'string (lambda (x y) (code-char (abs (* x y)))) lst0 lst1)
               (map  'string (lambda (x y) (code-char (abs (* x y)))) lst0 lst1))

    ;; vector
    (let ((vec (coerce lst1 'vector)))
      (is-equal  (map*  t      #'+ lst0 vec)
                 (map  'list   #'+ lst0 vec))
      (is-equal  (map*  nil    #'+ lst0 vec)
                 (map   nil    #'+ lst0 vec))
      (is-equal  (map* 'list   #'+ lst0 vec)
                 (map  'list   #'+ lst0 vec))
      (is-equalp (map* 'vector #'+ lst0 vec)
                 (map  'vector #'+ lst0 vec))
      (is-equal  (map* 'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) lst0 vec)
                 (map  'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) lst0 vec))
      (is-equal  (map* 'string (lambda (x y) (code-char (abs (* x y)))) lst0 vec)
                 (map  'string (lambda (x y) (code-char (abs (* x y)))) lst0 vec)))

    ;; lazy-sequence
    (let ((lseq (lfor x :in lst1)))
      (is-equal  (map*  t      #'+ lst0 lseq)
                 (map  'list   #'+ lst0 lst1))
      (is-equal  (map*  nil    #'+ lst0 lseq)
                 (map   nil    #'+ lst0 lst1))
      (is-equal  (map* 'list   #'+ lst0 lseq)
                 (map  'list   #'+ lst0 lst1))
      (is-equalp (map* 'vector #'+ lst0 lseq)
                 (map  'vector #'+ lst0 lst1))
      (is-equal  (map* 'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) lst0 lseq)
                 (map  'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) lst0 lst1))
      (is-equal  (map* 'string (lambda (x y) (code-char (abs (* x y)))) lst0 lseq)
                 (map  'string (lambda (x y) (code-char (abs (* x y)))) lst0 lst1)))

    ;; array
    (let ((ary (make-array (list 3 (length lst1))
                           :initial-contents (list lst1 lst1 lst1)))
          (lst2 (append lst1 lst1 lst1)))
      (is-equal  (map*  t      #'+ lst0 ary)
                 (map  'list   #'+ lst0 lst2))
      (is-equal  (map*  nil    #'+ lst0 ary)
                 (map   nil    #'+ lst0 lst2))
      (is-equal  (map* 'list   #'+ lst0 ary)
                 (map  'list   #'+ lst0 lst2))
      (is-equalp (map* 'vector #'+ lst0 ary)
                 (map  'vector #'+ lst0 lst2))
      (is-equal  (map* 'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) lst0 ary)
                 (map  'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) lst0 lst2))
      (is-equal  (map* 'string (lambda (x y) (code-char (abs (* x y)))) lst0 ary)
                 (map  'string (lambda (x y) (code-char (abs (* x y)))) lst0 lst2)))

    ;; hash-table
    (with-muffle-warnings
      (let ((ht (loop :with h := (make-hash-table)
                      :for k :from 0 :to (length lst0) ;; size = length + 1
                      :do (setf (gethash k h) 1)
                      :finally (return h))))
        (is-equal  (map*  t      #'+  lst0 ht)
                   (map  'list   #'1+ lst0))
        (is-equal  (map*  nil    #'+ lst0 ht)
                   (map   nil    #'1+ lst0))
        (is-equal  (map* 'list   #'+ lst0 ht)
                   (map  'list   #'1+ lst0))
        (is-equalp (map* 'vector #'+ lst0 ht)
                   (map  'vector #'1+ lst0))
        (is-equal  (map* 'bit-vector (lambda (x y) (if (plusp (+ x y)) 1 0)) lst0 ht)
                   (map  'bit-vector (lambda (x) (if (plusp (1+ x)) 1 0)) lst0))
        (is-equal  (map* 'string (lambda (x y) (code-char (abs (+ x y)))) lst0 ht)
                   (map  'string (lambda (x) (code-char (abs (1+ x)))) lst0))))))


(test ?map*.vector
  (for-all ((lst0 (gen-list))
            (lst1 (gen-list)))
    (let ((vec0 (coerce lst0 'vector)))
      (is-equalp (map*  t      #'1+ vec0)
                 (map  'vector #'1+ vec0))
      (is-equal  (map*  nil    #'1+ vec0)
                 (map   nil    #'1+ vec0))
      (is-equal  (map* 'list   #'1+ vec0)
                 (map  'list   #'1+ vec0))
      (is-equalp (map* 'vector #'1+ vec0)
                 (map  'vector #'1+ vec0))
      (is-equal  (map* 'bit-vector (lambda (x) (if (plusp x) 1 0)) vec0)
                 (map  'bit-vector (lambda (x) (if (plusp x) 1 0)) vec0))
      (is-equal  (map* 'string (lambda (x) (code-char (abs x))) vec0)
                 (map  'string (lambda (x) (code-char (abs x))) vec0))
      
      ;; list
      (is-equalp (map*  t      #'+ vec0 lst1)
                 (map  'vector #'+ vec0 lst1))
      (is-equal  (map*  nil    #'+ vec0 lst1)
                 (map   nil    #'+ vec0 lst1))
      (is-equal  (map* 'list   #'+ vec0 lst1)
                 (map  'list   #'+ vec0 lst1))
      (is-equalp (map* 'vector #'+ vec0 lst1)
                 (map  'vector #'+ vec0 lst1))
      (is-equal  (map* 'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) vec0 lst1)
                 (map  'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) vec0 lst1))
      (is-equal  (map* 'string (lambda (x y) (code-char (abs (* x y)))) vec0 lst1)
                 (map  'string (lambda (x y) (code-char (abs (* x y)))) vec0 lst1))

      ;; vector
      (let ((vec (coerce lst1 'vector)))
        (is-equalp (map*  t      #'+ vec0 vec)
                   (map  'vector #'+ vec0 vec))
        (is-equal  (map*  nil    #'+ vec0 vec)
                   (map   nil    #'+ vec0 vec))
        (is-equal  (map* 'list   #'+ vec0 vec)
                   (map  'list   #'+ vec0 vec))
        (is-equalp (map* 'vector #'+ vec0 vec)
                   (map  'vector #'+ vec0 vec))
        (is-equal  (map* 'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) vec0 vec)
                   (map  'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) vec0 vec))
        (is-equal  (map* 'string (lambda (x y) (code-char (abs (* x y)))) vec0 vec)
                   (map  'string (lambda (x y) (code-char (abs (* x y)))) vec0 vec)))

      ;; lazy-sequence
      (let ((lseq (lfor x :in lst1)))
        (is-equalp (map*  t      #'+ vec0 lseq)
                   (map  'vector #'+ vec0 lst1))
        (is-equal  (map*  nil    #'+ vec0 lseq)
                   (map   nil    #'+ vec0 lst1))
        (is-equal  (map* 'list   #'+ vec0 lseq)
                   (map  'list   #'+ vec0 lst1))
        (is-equalp (map* 'vector #'+ vec0 lseq)
                   (map  'vector #'+ vec0 lst1))
        (is-equal  (map* 'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) vec0 lseq)
                   (map  'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) vec0 lst1))
        (is-equal  (map* 'string (lambda (x y) (code-char (abs (* x y)))) vec0 lseq)
                   (map  'string (lambda (x y) (code-char (abs (* x y)))) vec0 lst1)))

      ;; array
      (let ((ary (make-array (list 3 (length lst1))
                             :initial-contents (list lst1 lst1 lst1)))
            (lst2 (append lst1 lst1 lst1)))
        (is-equalp (map*  t      #'+ vec0 ary)
                   (map  'vector #'+ vec0 lst2))
        (is-equal  (map*  nil    #'+ vec0 ary)
                   (map   nil    #'+ vec0 lst2))
        (is-equal  (map* 'list   #'+ vec0 ary)
                   (map  'list   #'+ vec0 lst2))
        (is-equalp (map* 'vector #'+ vec0 ary)
                   (map  'vector #'+ vec0 lst2))
        (is-equal  (map* 'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) vec0 ary)
                   (map  'bit-vector (lambda (x y) (if (plusp (* x y)) 1 0)) vec0 lst2))
        (is-equal  (map* 'string (lambda (x y) (code-char (abs (* x y)))) vec0 ary)
                   (map  'string (lambda (x y) (code-char (abs (* x y)))) vec0 lst2)))

      ;; hash-table
      (with-muffle-warnings
        (let ((ht (loop :with h := (make-hash-table)
                        :for k :from 0 :to (length vec0) ;; size = length + 1
                        :do (setf (gethash k h) 1)
                        :finally (return h))))
          (is-equalp (map*  t      #'+  vec0 ht)
                     (map  'vector #'1+ vec0))
          (is-equal  (map*  nil    #'+ vec0 ht)
                     (map   nil    #'1+ vec0))
          (is-equal  (map* 'list   #'+ vec0 ht)
                     (map  'list   #'1+ vec0))
          (is-equalp (map* 'vector #'+ vec0 ht)
                     (map  'vector #'1+ vec0))
          (is-equal  (map* 'bit-vector (lambda (x y) (if (plusp (+ x y)) 1 0)) vec0 ht)
                     (map  'bit-vector (lambda (x) (if (plusp (1+ x)) 1 0)) vec0))
          (is-equal  (map* 'string (lambda (x y) (code-char (abs (+ x y)))) vec0 ht)
                     (map  'string (lambda (x) (code-char (abs (1+ x)))) vec0)))))))


(test ?map*.bit-vector
  (for-all ((lst0 (gen-list :elements (gen-integer :min 0 :max 1)))
            (lst1 (gen-list :elements (gen-integer :min 0 :max 1))))
    (flet ((flip (b) (if (zerop b) 1 0)))
      (let ((bvec0 (coerce lst0 'bit-vector)))
      
        (is-equal  (map*  t      #'flip bvec0)
                   (map  'bit-vector #'flip bvec0))
        (is-equal  (map*  nil    #'flip bvec0)
                   (map   nil    #'flip bvec0))
        (is-equal  (map* 'list   #'flip bvec0)
                   (map  'list   #'flip bvec0))
        (is-equalp (map* 'vector #'flip bvec0)
                   (map  'vector #'flip bvec0))
        (is-equal  (map* 'bit-vector #'flip bvec0)
                   (map  'bit-vector #'flip bvec0))
        (is-equal  (map* 'string #'code-char bvec0)
                   (map  'string #'code-char bvec0))
      
        ;; list
        (is-equalp (map*  t      #'logxor bvec0 lst1)
                   (map  'bit-vector #'logxor bvec0 lst1))
        (is-equal  (map*  nil    #'logxor bvec0 lst1)
                   (map   nil    #'logxor bvec0 lst1))
        (is-equal  (map* 'list   #'logxor bvec0 lst1)
                   (map  'list   #'logxor bvec0 lst1))
        (is-equalp (map* 'vector #'logxor bvec0 lst1)
                   (map  'vector #'logxor bvec0 lst1))
        (is-equal  (map* 'bit-vector #'logxor bvec0 lst1)
                   (map  'bit-vector #'logxor bvec0 lst1))
        (is-equal  (map* 'string (lambda (x y) (code-char (logxor x y))) bvec0 lst1)
                   (map  'string (lambda (x y) (code-char (logxor x y))) bvec0 lst1))

        ;; vector
        (let ((vec (coerce lst1 'vector)))
          (is-equalp (map*  t      #'logxor bvec0 vec)
                     (map  'bit-vector #'logxor bvec0 vec))
          (is-equal  (map*  nil    #'logxor bvec0 vec)
                     (map   nil    #'logxor bvec0 vec))
          (is-equal  (map* 'list   #'logxor bvec0 vec)
                     (map  'list   #'logxor bvec0 vec))
          (is-equalp (map* 'vector #'logxor bvec0 vec)
                     (map  'vector #'logxor bvec0 vec))
          (is-equal  (map* 'bit-vector #'logxor bvec0 vec)
                     (map  'bit-vector #'logxor bvec0 vec))
          (is-equal  (map* 'string (lambda (x y) (code-char (logxor x y))) bvec0 vec)
                     (map  'string (lambda (x y) (code-char (logxor x y))) bvec0 vec)))

        ;; lazy-sequence
        (let ((lseq (lfor x :in lst1)))
          (is-equalp (map*  t      #'logxor bvec0 lseq)
                     (map  'bit-vector #'logxor bvec0 lst1))
          (is-equal  (map*  nil    #'logxor bvec0 lseq)
                     (map   nil    #'logxor bvec0 lst1))
          (is-equal  (map* 'list   #'logxor bvec0 lseq)
                     (map  'list   #'logxor bvec0 lst1))
          (is-equalp (map* 'vector #'logxor bvec0 lseq)
                     (map  'vector #'logxor bvec0 lst1))
          (is-equal  (map* 'bit-vector #'logxor bvec0 lseq)
                     (map  'bit-vector #'logxor bvec0 lst1))
          (is-equal  (map* 'string (lambda (x y) (code-char (logxor x y))) bvec0 lseq)
                     (map  'string (lambda (x y) (code-char (logxor x y))) bvec0 lst1)))

        ;; array
        (let ((ary (make-array (list 3 (length lst1))
                               :initial-contents (list lst1 lst1 lst1)))
              (lst2 (append lst1 lst1 lst1)))
          (is-equalp (map*  t      #'logxor bvec0 ary)
                     (map  'bit-vector #'logxor bvec0 lst2))
          (is-equal  (map*  nil    #'logxor bvec0 ary)
                     (map   nil    #'logxor bvec0 lst2))
          (is-equal  (map* 'list   #'logxor bvec0 ary)
                     (map  'list   #'logxor bvec0 lst2))
          (is-equalp (map* 'vector #'logxor bvec0 ary)
                     (map  'vector #'logxor bvec0 lst2))
          (is-equal  (map* 'bit-vector #'logxor bvec0 ary)
                     (map  'bit-vector #'logxor bvec0 lst2))
          (is-equal  (map* 'string (lambda (x y) (code-char (logxor x y))) bvec0 ary)
                     (map  'string (lambda (x y) (code-char (logxor x y))) bvec0 lst2)))

        ;; hash-table
        (with-muffle-warnings
          (let ((ht (loop :with h := (make-hash-table)
                          :for k :from 0 :to (length bvec0) ;; size = length logxor 1
                          :do (setf (gethash k h) 1)
                          :finally (return h))))
            (is-equalp (map*  t      #'logxor  bvec0 ht)
                       (map  'bit-vector #'flip bvec0))
            (is-equal  (map*  nil    #'logxor bvec0 ht)
                       (map   nil    #'flip bvec0))
            (is-equal  (map* 'list   #'logxor bvec0 ht)
                       (map  'list   #'flip bvec0))
            (is-equalp (map* 'vector #'logxor bvec0 ht)
                       (map  'vector #'flip bvec0))
            (is-equal  (map* 'bit-vector #'logxor bvec0 ht)
                       (map  'bit-vector #'flip bvec0))
            (is-equal  (map* 'string (lambda (x y) (code-char (logxor x y))) bvec0 ht)
                       (map  'string (lambda (x) (code-char (flip x))) bvec0))))))))


;; TODO:
(test ?map*.string
  )

(test ?map*.lazy-sequence
  (is-equal (with-time-limit (1 :boom!)
              (map* 5 ^bp(expt b p) #[1 2 4..] #[1..]))
            '(1 4 64 4096 1048576)))

(test ?map*.array
  )

(test ?map*.hash-table
  )


;;--------------------------------------------------------------------
;; reduce*
;;--------------------------------------------------------------------

(test ?reduce*.error
  (signals type-error (reduce* #() '()))
  (signals type-error (reduce* '+ '() :key #()))
  (signals type-error (reduce* '+ '() :start nil))
  (signals type-error (reduce* '+ '() :start -1))
  (signals type-error (reduce* '+ '() :end   -1))
  (signals error      (reduce* '+ '() :start 42 :end 24)))


(test ?reduce*.empty
  (is-= 42 (reduce* #'* '()     :initial-value 42))
  (is-= 42 (reduce* #'* #()     :initial-value 42))
  (is-= 42 (reduce* #'* ""      :initial-value 42))
  (is-= 42 (reduce* #'* #*      :initial-value 42))
  (is-= 42 (reduce* #'* #[]     :initial-value 42))
  (is-= 42 (reduce* #'* #2A(()) :initial-value 42))
  (is-= 42 (reduce* #'* #{}     :initial-value 42))
  (is-= 0 (reduce* #'+ '()))
  (is-= 0 (reduce* #'+ #()))
  (is-= 0 (reduce* #'+ ""))
  (is-= 0 (reduce* #'+ #*))
  (is-= 0 (reduce* #'+ #[]))
  (is-= 0 (reduce* #'+ #2A(())))
  (is-= 0 (reduce* #'+ #{})))


(test ?reduce*.only-one-element
  (is-= 42 (reduce* #'* '(42)))
  (is-= 42 (reduce* #'* #(42)))
  (is (char= #\s (reduce* #'* "s")))
  (is-= 0 (reduce* #'* #*0))
  (is-= 42 (reduce* #'* #[42]))
  (is-= 42 (reduce* #'* #2A((42))))
  (is-= 42 (reduce* #'* #{:foo 42})))


(test ?reduce*.list
  (for-all ((lst (gen-list)))
    (is-= (reduce* '+ lst)
          (reduce  '+ lst))

    (is-= (reduce* '+ lst :key '1+)
          (reduce  '+ lst :key '1+))
    
    (is-= (reduce* '+ lst :from-end t)
          (reduce  '+ lst :from-end t))

    (let ((init (random most-positive-fixnum)))
      (is-= (reduce* '+ lst :initial-value init)
            (reduce  '+ lst :initial-value init)))

    (let ((rand (random (max 1 (length lst)))))
      (is-= (reduce* '+ lst :start rand)
            (reduce  '+ lst :start rand))

      (let ((rand2 (random (max 1 rand))))
        (is-= (reduce* '+ lst :start rand2 :end rand)
              (reduce  '+ lst :start rand2 :end rand))))))


(test ?reduce*.vector
  (for-all ((lst (gen-list)))
    (let ((vec (coerce lst 'vector)))

      (is-= (reduce* '+ vec)
            (reduce  '+ vec))

      (is-= (reduce* '+ vec :key '1+)
            (reduce  '+ vec :key '1+))

      (is-= (reduce* '+ vec :from-end t)
            (reduce  '+ vec :from-end t))
      
      (let ((init (random most-positive-fixnum)))
        (is-= (reduce* '+ vec :initial-value init)
              (reduce  '+ vec :initial-value init)))

      (let ((rand (random (max 1 (length vec)))))
        (is-= (reduce* '+ vec :start rand)
              (reduce  '+ vec :start rand))

        (let ((rand2 (random (max 1 rand))))
          (is-= (reduce* '+ vec :start rand2 :end rand)
                (reduce  '+ vec :start rand2 :end rand)))))))


(test ?reduce*.array
  (for-all ((lst1 (gen-list))
            (lst2 (gen-list)))
    (let* ((dim2 (min (length lst1) (length lst2)))
           (ary  (make-array (list 2 dim2)
                             :initial-contents (list (subseq lst1 0 dim2)
                                                     (subseq lst2 0 dim2)))))
      ;; (repl-utilities:dbgv () dim2 ary)
      (is-= (reduce* '+ ary)
            (loop :for i :from 0 :below (* 2 dim2)
                  :sum (row-major-aref ary i)))
      
      (is-= (reduce* '+ ary :key '1+)
            (loop :for i :from 0 :below (* 2 dim2)
                  :sum (1+ (row-major-aref ary i))))
      
      (let ((init (random most-positive-fixnum)))
        (is-= (reduce* '+ ary :initial-value init)
              (+ init (loop :for i :from 0 :below (* 2 dim2)
                            :sum (row-major-aref ary i)))))
      
      (is-= (reduce* '+ ary :from-end t)
            (loop :for i :downfrom (1- (* 2 dim2)) :to 0
                  :sum (row-major-aref ary i)))

      (let ((rand (random (max 1 (* 2 dim2)))))
        (is-= (reduce* '+ ary :start rand)
              (loop :for i :from rand :below (* 2 dim2)
                            :sum (row-major-aref ary i)))

        (let ((rand2 (random (max 1 rand))))
          ;; (repl-utilities:dbgv () rand rand2)
          (is-= (reduce* '+ ary :start rand2 :end rand)
                (loop :for i :from rand2 :below rand
                            :sum (row-major-aref ary i))))))))


(test ?reduce*.hash-table
  (for-all ((vals (gen-list)))
    (let ((ht  (loop :with h := (make-hash-table :test 'equal)
                     :for v :in vals
                     :for k :from 0
                     :do (setf (gethash k h) v)
                     :finally (return h))))
      ;; (repl-utilities:dbgv () vals)
      (is-= (reduce* '+ ht)
            (loop :for v :being :the :hash-values :of ht
                  :sum v))
      
      (is-= (reduce* '+ ht :key '1+)
            (loop :for v :being :the :hash-values :of ht
                  :sum (1+ v)))
      
      (let ((init (random most-positive-fixnum)))
        (is-= (reduce* '+ ht :initial-value init)
              (+ init (loop :for v :being :the :hash-values :of ht
                            :sum v))))
      
      (is-= (reduce* '+ ht :from-end t)
            (loop :for v :being :the :hash-values :of ht
                  :sum v))

      (let ((rand (random (max 1 (length vals)))))
        (is-= (reduce* '+ ht :start rand)
              (loop :for v :being :the :hash-values :of ht
                    :sum v))

        (let ((rand2 (random (max 1 rand))))
          ;; (repl-utilities:dbgv () rand rand2)
          (is-= (reduce* '+ ht :start rand2 :end rand)
                (loop :for v :being :the :hash-values :of ht
                      :sum v)))))))


;;--------------------------------------------------------------------
;; count*
;;--------------------------------------------------------------------

(test ?count*.error
  (signals type-error (count* 42 :foo))
  (signals type-error (count* 42 '(0 1 2) :key #()))
  (signals type-error (count* 42 '(0 1 2) :start -1))
  (signals type-error (count* 42 '(0 1 2) :start nil))
  (signals type-error (count* 42 '(0 1 2) :start '(0 1)))
  (signals type-error (count* 42 '(0 1 2) :end -1))
  (signals type-error (count* 42 '(0 1 2) :end '(0 1)))
  (signals error      (count* 42 '(0 1 2) :start 42 :end 24))
  (finishes (count* 4 #2A((0 1 2) (3 4 5)) :start '(0 1)))
  (finishes (count* 2 #2A((0 1 2) (3 4 5)) :end   '(1 1)))
  (finishes (count* 2 #2A((0 1 2) (3 4 5)) :start '(0 1) :end '(1 1)))
  (signals error (count* 2 #2A((0 1 2) (3 4 5)) :start '(1 1) :end '(0 1))))


(test ?count*.list
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (is (= (count* 0 lst)
           (count  0 lst)))

    (is (= (count* 0 lst :key #'1+)
           (count  0 lst :key #'1+)))

    (unless (null lst)
      (let ((start (random (length lst))))
        (is (= (count* 0 lst :start start)
               (count  0 lst :start start))))
    
      (let ((end (random (length lst))))
        (is (= (count* 0 lst :end end)
               (count  0 lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (= (count* 0 lst :start start :end end)
               (count  0 lst :start start :end end)))))

    (is (= (count* 0 lst :from-end t)
           (count  0 lst :from-end t)))))


(test ?count*.vector
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((vec (coerce lst 'vector)))
      
      (is (= (count* 0 vec)
             (count  0 vec)))

      (is (= (count* 0 vec :key #'1+)
             (count  0 vec :key #'1+)))

      (unless (null lst)
        (let ((start (random (length vec))))
          (is (= (count* 0 vec :start start)
                 (count  0 vec :start start))))
        
        (let ((end (random (length vec))))
          (is (= (count* 0 vec :end end)
                 (count  0 vec :end end))))

        (let* ((end   (random (length vec)))
               (start (random (1+ end))))
          (is (= (count* 0 vec :start start :end end)
                 (count  0 vec :start start :end end)))))

      (is (= (count* 0 vec :from-end t)
             (count  0 vec :from-end t))))))


(test ?count*.lazy-sequence
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (is (= (count* 0 (lfor x :in lst))
           (count  0 lst)))
    
    (is (= (count* 0 (lfor x :in lst) :key #'1+)
           (count  0 lst :key #'1+)))
    
    (unless (null lst)
      (let ((start (random (length lst))))
        (is (= (count* 0 (lfor x :in lst) :start start)
               (count  0 lst :start start))))
    
      (let ((end (random (length lst))))
        (is (= (count* 0 (lfor x :in lst) :end end)
               (count  0 lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (= (count* 0 (lfor x :in lst) :start start :end end)
               (count  0 lst :start start :end end)))))))


(test ?count*.array
  (is-= (count* 42 #2A((1 42 3) (42 5 6)) :start '(0 1))
        2)
  (is-= (count* 42 #2A((1 42 3) (42 5 6)) :start '(1 2))
        0)
  (is-= (count* 42 #2A((1 42 3) (42 5 6)) :end '(0 2))
        1)
  (is-= (count* 42 #2A((1 42 3) (42 5 6)) :start '(1 0) :end '(1 1))
        1)
  
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (let* ((dim0 3)
           (dim1 (length lst))
           (init (loop :repeat dim0 :collect lst))
           (ary  (make-array (list dim0 dim1) :initial-contents init)))
      
      (is-= (count* 0 ary)
            (* dim0 (count 0 lst)))

      (is-= (count* 0 ary :key #'1+)
            (* dim0 (count 0 lst :key #'1+)))

      (let ((rand (random (max 1 (* dim0 dim1)))))
        (is-= (count* 0 ary :start rand)
              (loop :for i :from rand :below (* dim0 dim1)
                    :count (= 0 (row-major-aref ary i))))
        
        (is-= (count* 0 ary :end rand)
              (loop :for i :from 0 :below rand
                    :count (= 0 (row-major-aref ary i))))

        (let ((start (random (max 1 rand))))
          (is-= (count* 0 ary :start start :end rand)
                (loop :for i :from start :below rand
                      :count (= 0 (row-major-aref ary i))))))

      (is-= (count* 0 ary :from-end t)
            (count* 0 ary)))))


(test ?count*.hash-table
  (for-all ((keys (gen-list :length (gen-integer :min 0 :max 30)))
            (vals (gen-list :length (gen-integer :min 0 :max 30)
                            :elements (gen-integer :min -1 :max 1))))
    (let ((ht (loop :with ht := (make-hash-table)
                    :for k :in keys
                    :for v :in vals
                    :do (setf (gethash k ht) v)
                    :finally (return ht))))
      
      (is (= (count* 0 ht)
             (loop :for v :being :the :hash-values :of ht
                   :count (= 0 v))))
      
      (is (= (count* 0 ht :key #'1+)
             (loop :for v :being :the :hash-values :of ht
                   :count (= 0 (1+ v)))))

      ;; keyword start should be ignored.
      (is (= (count* 0 ht :start 10)
             (loop :for v :being :the :hash-values :of ht
                   :count (= 0 v))))
      
      ;; keyword end should be ignored.      
      (is (= (count* 0 ht :end 10)
             (loop :for v :being :the :hash-values :of ht
                   :count (= 0 v))))
      
      ;; keyword from-end should be ignored.
      (is (= (count* 0 ht :from-end t)
             (loop :for v :being :the :hash-values :of ht
                   :count (= 0 v)))))))


;;--------------------------------------------------------------------
;; count-if*
;;--------------------------------------------------------------------

(test ?count-if*.error
  (signals type-error (count-if* #() '(0 1 2)))
  (signals type-error (count-if* #'oddp :foo))
  (signals type-error (count-if* #'oddp '(0 1 2) :key #()))
  (signals type-error (count-if* #'oddp '(0 1 2) :start -1))
  (signals type-error (count-if* #'oddp '(0 1 2) :start nil))
  (signals type-error (count-if* #'oddp '(0 1 2) :start '(0 1)))
  (signals type-error (count-if* #'oddp '(0 1 2) :end -1))
  (signals type-error (count-if* #'oddp '(0 1 2) :end '(0 1)))
  (signals error      (count-if* #'oddp '(0 1 2) :start 42 :end 24))
  (finishes (count-if* #'oddp #2A((0 1 2) (3 4 5)) :start '(0 1)))
  (finishes (count-if* #'oddp #2A((0 1 2) (3 4 5)) :end   '(1 1)))
  (finishes (count-if* #'oddp #2A((0 1 2) (3 4 5)) :start '(0 1) :end '(1 1)))
  (signals error (count-if* #'oddp #2A((0 1 2) (3 4 5)) :start '(1 1) :end '(0 1))))


(test ?count-if*.list
  (for-all ((lst (gen-list)))
    (is (= (count-if* #'oddp lst)
           (count-if  #'oddp lst)))

    (is (= (count-if* #'oddp lst :key #'1+)
           (count-if  #'oddp lst :key #'1+)))

    (unless (null lst)
      (let ((start (random (length lst))))
        (is (= (count-if* #'oddp lst :start start)
               (count-if  #'oddp lst :start start))))
    
      (let ((end (random (length lst))))
        (is (= (count-if* #'oddp lst :end end)
               (count-if  #'oddp lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (= (count-if* #'oddp lst :start start :end end)
               (count-if  #'oddp lst :start start :end end)))))

    (is (= (count-if* #'oddp lst :from-end t)
           (count-if  #'oddp lst :from-end t)))))


(test ?count-if*.vector
  (for-all ((lst (gen-list)))
    (let ((vec (coerce lst 'vector)))
      
     (is (= (count-if* #'oddp vec)
            (count-if  #'oddp vec)))

     (is (= (count-if* #'oddp vec :key #'1+)
            (count-if  #'oddp vec :key #'1+)))

     (unless (null lst)
       (let ((start (random (length vec))))
         (is (= (count-if* #'oddp vec :start start)
                (count-if  #'oddp vec :start start))))
    
       (let ((end (random (length vec))))
         (is (= (count-if* #'oddp vec :end end)
                (count-if  #'oddp vec :end end))))

       (let* ((end   (random (length vec)))
              (start (random (1+ end))))
         (is (= (count-if* #'oddp vec :start start :end end)
                (count-if  #'oddp vec :start start :end end)))))

     (is (= (count-if* #'oddp vec :from-end t)
            (count-if  #'oddp vec :from-end t))))))


(test ?count-if*.lazy-sequence
  (for-all ((lst (gen-list)))
    (is (= (count-if* #'oddp (lfor x :in lst))
           (count-if  #'oddp lst)))
    
    (is (= (count-if* #'oddp (lfor x :in lst) :key #'1+)
           (count-if  #'oddp lst :key #'1+)))
    
    (unless (null lst)
      (let ((start (random (length lst))))
        (is (= (count-if* #'oddp (lfor x :in lst) :start start)
               (count-if  #'oddp lst :start start))))
    
      (let ((end (random (length lst))))
        (is (= (count-if* #'oddp (lfor x :in lst) :end end)
               (count-if  #'oddp lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (= (count-if* #'oddp (lfor x :in lst) :start start :end end)
               (count-if  #'oddp lst :start start :end end)))))))


(test ?count-if*.array
  (is-= (count-if* #'evenp #2A((1 42 1) (42 1 1)) :start '(0 1))
        2)
  (is-= (count-if* #'evenp #2A((1 42 1) (42 1 1)) :start '(1 2))
        0)
  (is-= (count-if* #'evenp #2A((1 42 1) (42 1 1)) :end '(0 2))
        1)
  (is-= (count-if* #'evenp #2A((1 42 1) (42 1 1)) :start '(1 0) :end '(1 1))
        1)
  
  (for-all ((lst (gen-list)))
    (let* ((dim0 3)
           (dim1 (length lst))
           (init (loop :repeat dim0 :collect lst))
           (ary  (make-array (list dim0 dim1) :initial-contents init)))
      
      (is-= (count-if* #'oddp ary)
            (* dim0 (count-if #'oddp lst)))

      (is-= (count-if* #'oddp ary :key #'1+)
            (* dim0 (count-if #'oddp lst :key #'1+)))

      (let ((rand (random (max 1 (* dim0 dim1)))))
        (is-= (count-if* #'oddp ary :start rand)
              (loop :for i :from rand :below (* dim0 dim1)
                    :count (funcall #'oddp (row-major-aref ary i))))
        
        (is-= (count-if* #'oddp ary :end rand)
              (loop :for i :from 0 :below rand
                    :count (funcall #'oddp (row-major-aref ary i))))

        (let ((start (random (max 1 rand))))
          (is-= (count-if* #'oddp ary :start start :end rand)
                (loop :for i :from start :below rand
                      :count (funcall #'oddp (row-major-aref ary i))))))

      (is-= (count-if* #'oddp ary :from-end t)
            (count-if* #'oddp ary)))))


(test ?count-if*.hash-table
  (for-all ((keys (gen-list :length (gen-integer :min 0 :max 30)))
            (vals (gen-list :length (gen-integer :min 0 :max 30))))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :in keys
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      
      (is (= (count-if* #'oddp ht)
             (loop :for v :being :the :hash-values :of ht
                   :count (oddp v))))
      
      (is (= (count-if* #'oddp ht :key #'1+)
             (loop :for v :being :the :hash-values :of ht
                   :count (oddp (1+ v)))))

      ;; keyword start should be ignored.
      (is (= (count-if* #'oddp ht :start 10)
             (loop :for v :being :the :hash-values :of ht
                   :count (oddp v))))
      
      ;; keyword end should be ignored.      
      (is (= (count-if* #'oddp ht :end 10)
             (loop :for v :being :the :hash-values :of ht
                   :count (oddp v))))
      
      ;; keyword from-end should be ignored.
      (is (= (count-if* #'oddp ht :from-end t)
             (loop :for v :being :the :hash-values :of ht
                   :count (oddp v)))))))


;;--------------------------------------------------------------------
;; count-if-not*
;;--------------------------------------------------------------------

(test ?count-if-not*.error
  (signals type-error (count-if-not* #() '(0 1 2)))
  (signals type-error (count-if-not* #'oddp :foo))
  (signals type-error (count-if-not* #'oddp '(0 1 2) :key #()))
  (signals type-error (count-if-not* #'oddp '(0 1 2) :start -1))
  (signals type-error (count-if-not* #'oddp '(0 1 2) :start nil))
  (signals type-error (count-if-not* #'oddp '(0 1 2) :start '(0 1)))
  (signals type-error (count-if-not* #'oddp '(0 1 2) :end -1))
  (signals type-error (count-if-not* #'oddp '(0 1 2) :end '(0 1)))
  (signals error      (count-if-not* #'oddp '(0 1 2) :start 42 :end 24))
  (signals error      (count-if-not* #'oddp #2A((0 1 2) (3 4 5))
                                     :start '(1 1) :end '(0 1)))
  (finishes (count-if-not* #'oddp #2A((0 1 2) (3 4 5)) :start '(0 1)))
  (finishes (count-if-not* #'oddp #2A((0 1 2) (3 4 5)) :end '(1 1)))
  (finishes (count-if-not* #'oddp #2A((0 1 2) (3 4 5)) :start '(0 1) :end '(1 1))))


(test ?count-if-not*.list
  (for-all ((lst (gen-list)))
    (is (= (count-if-not* #'oddp lst)
           (count-if-not  #'oddp lst)))

    (is (= (count-if-not* #'oddp lst :key #'1+)
           (count-if-not  #'oddp lst :key #'1+)))

    (unless (null lst)
      (let ((start (random (length lst))))
        (is (= (count-if-not* #'oddp lst :start start)
               (count-if-not  #'oddp lst :start start))))
    
      (let ((end (random (length lst))))
        (is (= (count-if-not* #'oddp lst :end end)
               (count-if-not  #'oddp lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (= (count-if-not* #'oddp lst :start start :end end)
               (count-if-not  #'oddp lst :start start :end end)))))

    (is (= (count-if-not* #'oddp lst :from-end t)
           (count-if-not  #'oddp lst :from-end t)))))


(test ?count-if-not*.vector
  (for-all ((lst (gen-list)))
    (let ((vec (coerce lst 'vector)))
      
      (is (= (count-if-not* #'oddp vec)
             (count-if-not  #'oddp vec)))

      (is (= (count-if-not* #'oddp vec :key #'1+)
             (count-if-not  #'oddp vec :key #'1+)))

      (unless (null lst)
        (let ((start (random (length vec))))
          (is (= (count-if-not* #'oddp vec :start start)
                 (count-if-not  #'oddp vec :start start))))
    
        (let ((end (random (length vec))))
          (is (= (count-if-not* #'oddp vec :end end)
                 (count-if-not  #'oddp vec :end end))))

        (let* ((end   (random (length vec)))
               (start (random (1+ end))))
          (is (= (count-if-not* #'oddp vec :start start :end end)
                 (count-if-not  #'oddp vec :start start :end end)))))

      (is (= (count-if-not* #'oddp vec :from-end t)
             (count-if-not  #'oddp vec :from-end t))))))


(test ?count-if-not*.lazy-sequence
  (for-all ((lst (gen-list)))
    (is (= (count-if-not* #'oddp (lfor x :in lst))
           (count-if-not  #'oddp lst)))
    
    (is (= (count-if-not* #'oddp (lfor x :in lst) :key #'1+)
           (count-if-not  #'oddp lst :key #'1+)))
    
    (unless (null lst)
      (let ((start (random (length lst))))
        (is (= (count-if-not* #'oddp (lfor x :in lst) :start start)
               (count-if-not  #'oddp lst :start start))))
    
      (let ((end (random (length lst))))
        (is (= (count-if-not* #'oddp (lfor x :in lst) :end end)
               (count-if-not  #'oddp lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (= (count-if-not* #'oddp (lfor x :in lst) :start start :end end)
               (count-if-not  #'oddp lst :start start :end end)))))))


(test ?count-if-not*.array
  (is (= (count-if-not* #'oddp #2A((1 42 1) (42 1 1)) :start '(0 1))
         2))
  (is (= (count-if-not* #'oddp #2A((1 42 1) (42 1 1)) :start '(1 2))
         0))
  
  (for-all ((lst (gen-list)))
    (let* ((dim0 3)
           (dim1 (length lst))
           (init (loop :repeat dim0 :collect lst))
           (ary  (make-array (list dim0 dim1) :initial-contents init)))
      
      (is (= (count-if-not* #'oddp ary)
             (* dim0 (count-if-not #'oddp lst))))

      (is (= (count-if-not* #'oddp ary :key #'1+)
             (* dim0 (count-if-not #'oddp lst :key #'1+))))
      
      (let ((rand (random (1+ (* dim0 dim1)))))
        (unless (zerop rand)
          (is (= (count-if-not* #'oddp ary :start rand)
                 (loop :for i :from rand :below (* dim0 dim1)
                       :count (funcall (complement #'oddp)
                                       (row-major-aref ary i)))))
          
          (is (= (count-if-not* #'oddp ary :end rand)
                 (loop :for i :from 0 :below rand
                       :count (funcall (complement #'oddp)
                                       (row-major-aref ary i)))))

          (let ((start (1+ (random rand))))
            (is (= (count-if-not* #'oddp ary :start start :end rand)
                   (loop :for i :from start :below rand
                         :count (funcall (complement #'oddp)
                                         (row-major-aref ary i))))))))

      (is (= (count-if-not* #'oddp ary :from-end t)
             (count-if-not* #'oddp ary))))))


(test ?count-if-not*.hash-table
  (for-all ((keys (gen-list :length (gen-integer :min 0 :max 30)))
            (vals (gen-list :length (gen-integer :min 0 :max 30))))
    (let ((ht (loop :with ht := (make-hash-table)
                    :for k :in keys
                    :for v :in vals
                    :do (setf (gethash k ht) v)
                    :finally (return ht))))
      
      (is (= (count-if-not* #'oddp ht)
             (loop :for v :being :the :hash-values :of ht
                   :count (funcall (complement #'oddp) v))))
      
      (is (= (count-if-not* #'oddp ht :key #'1+)
             (loop :for v :being :the :hash-values :of ht
                   :count (funcall (complement #'oddp) (1+ v)))))

      ;; keyword start should be ignored.
      (is (= (count-if-not* #'oddp ht :start 10)
             (loop :for v :being :the :hash-values :of ht
                   :count (funcall (complement #'oddp) v))))
      
      ;; keyword end should be ignored.      
      (is (= (count-if-not* #'oddp ht :end 10)
             (loop :for v :being :the :hash-values :of ht
                   :count (funcall (complement #'oddp) v))))
      
      ;; keyword from-end should be ignored.
      (is (= (count-if-not* #'oddp ht :from-end t)
             (loop :for v :being :the :hash-values :of ht
                   :count (funcall (complement #'oddp) v)))))))


;;--------------------------------------------------------------------
;; find*
;;--------------------------------------------------------------------

(test ?find*.error
  (signals type-error (find* 42 :foo))
  (signals type-error (find* 42 '(0 1 2) :key #()))
  (signals type-error (find* 42 '(0 1 2) :start -1))
  (signals type-error (find* 42 '(0 1 2) :start nil))
  (signals type-error (find* 42 '(0 1 2) :start '(0 1)))
  (signals type-error (find* 42 '(0 1 2) :end -1))
  (signals type-error (find* 42 '(0 1 2) :end '(0 1)))
  (signals error (find* 42 '(0 1 2) :start 42 :end 24))
  (finishes (find* 4 #2A((0 1 2) (3 4 5)) :start '(0 1)))
  (finishes (find* 2 #2A((0 1 2) (3 4 5)) :end '(1 1)))
  (finishes (find* 2 #2A((0 1 2) (3 4 5)) :start '(0 1) :end '(1 1)))
  (signals error (find* 2 #2A((0 1 2) (3 4 5)) :start '(1 1) :end '(0 1))))


(test ?find*.list
  (for-all ((lst (gen-list :elements (gen-integer :min 0 :max 10)
                           :length (gen-integer :min 0 :max 6))))
    (is (eql (find* 0 lst)
             (find  0 lst)))

    (is (eql (find* 0 lst :key #'1+)
             (find  0 lst :key #'1+)))

    (unless (null lst)
      (let ((start (random (length lst))))
        (is (eql (find* 0 lst :start start)
                 (find  0 lst :start start))))
      
      (let ((end (random (length lst))))
        (is (eql (find* 0 lst :end end)
                 (find  0 lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (eql (find* 0 lst :start start :end end)
                 (find  0 lst :start start :end end)))))

    (is (eql (find* 0 lst :from-end t)
             (find  0 lst :from-end t)))))


(test ?find*.vector
  (for-all ((lst (gen-list :elements (gen-integer :min 0 :max 10)
                           :length (gen-integer :min 0 :max 6))))
    (let ((vec (coerce lst 'vector)))
      
      (is (eql (find* 0 vec)
               (find  0 vec)))

      (is (eql (find* 0 vec :key #'1+)
               (find  0 vec :key #'1+)))

      (unless (null lst)
        (let ((start (random (length vec))))
          (is (eql (find* 0 vec :start start)
                   (find  0 vec :start start))))
        
        (let ((end (random (length vec))))
          (is (eql (find* 0 vec :end end)
                   (find  0 vec :end end))))

        (let* ((end   (random (length vec)))
               (start (random (1+ end))))
          (is (eql (find* 0 vec :start start :end end)
                   (find  0 vec :start start :end end)))))

      (is (eql (find* 0 vec :from-end t)
               (find  0 vec :from-end t))))))


(test ?find*.lazy-sequence
  (for-all ((lst (gen-list :elements (gen-integer :min 0 :max 10)
                           :length (gen-integer :min 0 :max 10))))
    (is-eql (find* 0 (lfor x :in lst))
            (find  0 lst))
    
    (is-eql (find* 0 (lfor x :in lst) :key #'1+)
            (find  0 lst :key #'1+))
    
    (unless (null lst)
      (let ((start (random (length lst))))
        (is-eql (find* 0 (lfor x :in lst) :start start)
                (find  0 lst :start start)))
      
      (let ((end (random (length lst))))
        (is-eql (find* 0 (lfor x :in lst) :end end)
                (find  0 lst :end end)))

      (let* ((end   (random (length lst)))
             (start (random (max 1 end))))
        (is-eql (find* 0 (lfor x :in lst) :start start :end end)
                (find  0 lst :start start :end end))))))


(test ?find*.array
  (is-eql (find* 42 #2A((1 42 3) (4 5 6)) :start '(0 1))
          42)
  (is-eql (find* 42 #2A((1 42 3) (4 5 6)) :start '(0 2))
          nil)
  (is-eql (find* 42 #2A((1 42 3) (4 5 6)) :end '(0 2))
          42)
  (is-eql (find* 42 #2A((1 42 3) (4 5 6)) :start '(0 1) :end '(0 1))
          nil)
  
  (for-all ((lst (gen-list :elements (gen-integer :min 0 :max 10)
                           :length (gen-integer :min 0 :max 10))))
    (let* ((dim0 3)
           (dim1 (length lst))
           (init (loop :repeat dim0 :collect lst))
           (ary  (make-array (list dim0 dim1) :initial-contents init)))
      
      (is-eql (find* 0 ary)
              (find 0 lst))

      (is-eql (find* 0 ary :key #'1+)
              (find 0 lst :key #'1+))
      
      (let ((rand (random (max 1 (* dim0 dim1)))))
        (is-eql (find* 0 ary :start rand)
                (loop :for i :from rand :below (* dim0 dim1)
                      :when (eql 0 (row-major-aref ary i))
                        :return 0))
        
        (is-eql (find* 0 ary :end rand)
                (loop :for i :from 0 :below rand
                      :when (eql 0 (row-major-aref ary i))
                        :return 0))

        (let ((start (random (max 1 rand))))
          (is-eql (find* 0 ary :start start :end rand)
                  (loop :for i :from start :below rand
                        :when (eql 0 (row-major-aref ary i))
                          :return 0))))

      (is-eql (find* 0 ary :from-end t)
              (find* 0 ary)))))


(test ?find*.hash-table
  (for-all ((keys (gen-list :length   (gen-integer :min 0 :max 30)))
            (vals (gen-list :elements (gen-integer :min 0 :max 10)
                            :length   (gen-integer :min 0 :max 30))))
    (let ((ht (loop :with ht := (make-hash-table)
                    :for k :in keys
                    :for v :in vals
                    :do (setf (gethash k ht) v)
                    :finally (return ht))))

      (is (eql (find* 0 ht)
               (loop :for v :being :the :hash-values :of ht
                     :when (eql 0 v)
                       :return 0)))
      
      (is (eql (find* 0 ht :key #'1+)
               (loop :for v :being :the :hash-values :of ht
                     :when (eql 0 (1+ v))
                       :return 0)))

      ;; keyword start should be ignored.
      (is (eql (find* 0 ht :start 10)
               (loop :for v :being :the :hash-values :of ht
                     :when (eql 0 v)
                       :return 0)))
      
      ;; keyword end should be ignored.      
      (is (eql (find* 0 ht :end 10)
               (loop :for v :being :the :hash-values :of ht
                     :when (eql 0 v)
                       :return 0)))
      
      ;; keyword from-end should be ignored.
      (is (eql (find* 0 ht :from-end t)
               (loop :for v :being :the :hash-values :of ht
                     :when (eql 0 v)
                       :return 0))))))


;;--------------------------------------------------------------------
;; find-if*
;;--------------------------------------------------------------------

(test ?find-if*.error
  (signals type-error (find-if* #() '(0 1 2)))
  (signals type-error (find-if* #'oddp :foo))
  (signals type-error (find-if* #'oddp '(0 1 2) :key #()))
  (signals type-error (find-if* #'oddp '(0 1 2) :start -1))
  (signals type-error (find-if* #'oddp '(0 1 2) :start nil))
  (signals type-error (find-if* #'oddp '(0 1 2) :start '(0 1)))
  (signals type-error (find-if* #'oddp '(0 1 2) :end -1))
  (signals type-error (find-if* #'oddp '(0 1 2) :end '(0 1)))
  (signals simple-error (find-if* #'oddp '(0 1 2) :start 42 :end 24))
  (finishes (find-if* #'oddp #2A((0 1 2) (3 4 5)) :start '(0 1)))
  (finishes (find-if* #'oddp #2A((0 1 2) (3 4 5)) :end '(1 1)))
  (finishes (find-if* #'oddp #2A((0 1 2) (3 4 5)) :start '(0 1) :end '(1 1)))
  (signals error (find-if* #'oddp #2A((0 1 2) (3 4 5)) :start '(1 1) :end '(0 1))))


(test ?find-if*.list
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (is (eql (find-if* #'oddp lst)
             (find-if  #'oddp lst)))

    (is (eql (find-if* #'oddp lst :key #'1+)
             (find-if  #'oddp lst :key #'1+)))

    (unless (null lst)
      (let ((start (random (length lst))))
        (is (eql (find-if* #'oddp lst :start start)
                 (find-if  #'oddp lst :start start))))
    
      (let ((end (random (length lst))))
        (is (eql (find-if* #'oddp lst :end end)
                 (find-if  #'oddp lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (eql (find-if* #'oddp lst :start start :end end)
                 (find-if  #'oddp lst :start start :end end)))))

    (is (eql (find-if* #'oddp lst :from-end t)
             (find-if  #'oddp lst :from-end t)))))


(test ?find-if*.vector
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (let ((vec (coerce lst 'vector)))
      
      (is (eql (find-if* #'oddp vec)
               (find-if  #'oddp vec)))

      (is (eql (find-if* #'oddp vec :key #'1+)
               (find-if  #'oddp vec :key #'1+)))

      (unless (null lst)
        (let ((start (random (length vec))))
          (is (eql (find-if* #'oddp vec :start start)
                   (find-if  #'oddp vec :start start))))
        
        (let ((end (random (length vec))))
          (is (eql (find-if* #'oddp vec :end end)
                   (find-if  #'oddp vec :end end))))

        (let* ((end   (random (length vec)))
               (start (random (1+ end))))
          (is (eql (find-if* #'oddp vec :start start :end end)
                   (find-if  #'oddp vec :start start :end end)))))

      (is (eql (find-if* #'oddp vec :from-end t)
               (find-if  #'oddp vec :from-end t))))))


(test ?find-if*.lazy-sequence
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (is-eql (find-if* #'oddp (lfor x :in lst))
            (find-if  #'oddp lst))
    
    (is-eql (find-if* #'oddp (lfor x :in lst) :key #'1+)
            (find-if  #'oddp lst :key #'1+))
    
    (let ((rand (random (max 1 (length lst)))))
      (is-eql (find-if* #'oddp (lfor x :in lst) :start rand)
              (find-if  #'oddp lst :start rand))
      
      (is-eql (find-if* #'oddp (lfor x :in lst) :end rand)
              (find-if  #'oddp lst :end rand))

      (let ((start (random (max 1 rand))))
        (is-eql (find-if* #'oddp (lfor x :in lst) :start start :end rand)
                (find-if  #'oddp lst :start start :end rand))))))


(test ?find-if*.array
  (is-eql (find-if* #'evenp #2A((1 42 1) (1 1 1)) :start '(0 1))
          42)
  (is-eql (find-if* #'evenp #2A((1 42 1) (1 1 1)) :start '(0 2))
          nil)
  (is-eql (find-if* #'evenp #2A((1 42 1) (1 1 1)) :end '(0 2))
          42)
  (is-eql (find-if* #'evenp #2A((1 42 1) (1 1 1)) :start '(0 1) :end '(0 1))
          nil)
  
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (let* ((dim0 3)
           (dim1 (length lst))
           (init (loop :repeat dim0 :collect lst))
           (ary  (make-array (list dim0 dim1) :initial-contents init)))
      
      (is-eql (find-if* #'oddp ary)
              (find-if  #'oddp lst))

      (is-eql (find-if* #'oddp ary :key #'1+)
              (find-if  #'oddp lst :key #'1+))
      
      (let ((rand (random (max 1 (* dim0 dim1)))))
        (is-eql (find-if* #'oddp ary :start rand)
                (loop :for i :from rand :below (* dim0 dim1)
                      :when (funcall #'oddp (row-major-aref ary i))
                        :return (row-major-aref ary i)))
        
        (is-eql (find-if* #'oddp ary :end rand)
                (loop :for i :from 0 :below rand
                      :when (funcall #'oddp (row-major-aref ary i))
                        :return (row-major-aref ary i)))

        (let ((start (random (max 1 rand))))
          (is-eql (find-if* #'oddp ary :start start :end rand)
                  (loop :for i :from start :below rand
                        :when (funcall #'oddp (row-major-aref ary i))
                          :return (row-major-aref ary i)))))

      (is-eql (find-if* #'oddp ary :from-end t)
              (loop :for i :downfrom (1- (array-total-size ary)) :to 0
                    :when (funcall #'oddp (row-major-aref ary i))
                      :return (row-major-aref ary i))))))


(test ?find-if*.hash-table
  (for-all ((keys (gen-list :length (gen-integer :min 0 :max 5)))
            (vals (gen-list :length (gen-integer :min 0 :max 5))))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :in keys
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      
      (is-eql (find-if* #'oddp ht)
              (loop :for v :being :the :hash-values :of ht
                    :when (oddp v)
                      :return v))
      
      (is-eql (find-if* #'oddp ht :key #'1+)
              (loop :for v :being :the :hash-values :of ht
                    :when (oddp (1+ v))
                      :return v))

      ;; keyword start should be ignored.
      (is-eql (find-if* #'oddp ht :start 10)
              (loop :for v :being :the :hash-values :of ht
                    :when (oddp v)
                      :return v))
      
      ;; keyword end should be ignored.      
      (is-eql (find-if* #'oddp ht :end 10)
              (loop :for v :being :the :hash-values :of ht
                    :when (oddp v)
                      :return v))
      
      ;; keyword from-end should be ignored.
      (is-eql (find-if* #'oddp ht :from-end t)
              (loop :for v :being :the :hash-values :of ht
                    :when (oddp v)
                      :return v)))))


;;--------------------------------------------------------------------
;; find-if-not*
;;--------------------------------------------------------------------

(test ?find-if-not*.error
  (signals type-error (find-if-not* #() '(0 1 2)))
  (signals type-error (find-if-not* #'oddp :foo))
  (signals type-error (find-if-not* #'oddp '(0 1 2) :key #()))
  (signals type-error (find-if-not* #'oddp '(0 1 2) :start -1))
  (signals type-error (find-if-not* #'oddp '(0 1 2) :start nil))
  (signals type-error (find-if-not* #'oddp '(0 1 2) :start '(0 1)))
  (signals type-error (find-if-not* #'oddp '(0 1 2) :end -1))
  (signals type-error (find-if-not* #'oddp '(0 1 2) :end '(0 1)))
  (signals error (find-if-not* #'oddp '(0 1 2) :start 42 :end 24))
  (finishes (find-if-not* #'oddp #2A((0 1 2) (3 4 5)) :start '(0 1)))
  (finishes (find-if-not* #'oddp #2A((0 1 2) (3 4 5)) :end '(1 1)))
  (finishes (find-if-not* #'oddp #2A((0 1 2) (3 4 5)) :start '(0 1) :end '(1 1)))
  (signals error (find-if-not* #'oddp #2A((0 1 2) (3 4 5)) :start '(1 1) :end '(0 1))))


(test ?find-if-not*.list
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (is (eql (find-if-not* #'oddp lst)
             (find-if-not  #'oddp lst)))

    (is (eql (find-if-not* #'oddp lst :key #'1+)
             (find-if-not  #'oddp lst :key #'1+)))

    (unless (null lst)
      (let ((start (random (length lst))))
        (is (eql (find-if-not* #'oddp lst :start start)
                 (find-if-not  #'oddp lst :start start))))
    
      (let ((end (random (length lst))))
        (is (eql (find-if-not* #'oddp lst :end end)
                 (find-if-not  #'oddp lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (eql (find-if-not* #'oddp lst :start start :end end)
                 (find-if-not  #'oddp lst :start start :end end)))))

    (is (eql (find-if-not* #'oddp lst :from-end t)
             (find-if-not  #'oddp lst :from-end t)))))


(test ?find-if-not*.vector
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (let ((vec (coerce lst 'vector)))
      
      (is (eql (find-if-not* #'oddp vec)
               (find-if-not  #'oddp vec)))

      (is (eql (find-if-not* #'oddp vec :key #'1+)
               (find-if-not  #'oddp vec :key #'1+)))

      (unless (null lst)
        (let ((start (random (length vec))))
          (is (eql (find-if-not* #'oddp vec :start start)
                   (find-if-not  #'oddp vec :start start))))
        
        (let ((end (random (length vec))))
          (is (eql (find-if-not* #'oddp vec :end end)
                   (find-if-not  #'oddp vec :end end))))

        (let* ((end   (random (length vec)))
               (start (random (1+ end))))
          (is (eql (find-if-not* #'oddp vec :start start :end end)
                   (find-if-not  #'oddp vec :start start :end end)))))

      (is (eql (find-if-not* #'oddp vec :from-end t)
               (find-if-not  #'oddp vec :from-end t))))))


(test ?find-if-not*.lazy-sequence
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (is (eql (find-if-not* #'oddp (lfor x :in lst))
             (find-if-not  #'oddp lst)))
    
    (is (eql (find-if-not* #'oddp (lfor x :in lst) :key #'1+)
             (find-if-not  #'oddp lst :key #'1+)))
    
    (unless (null lst)
      (let ((start (random (length lst))))
        (is (eql (find-if-not* #'oddp (lfor x :in lst) :start start)
                 (find-if-not  #'oddp lst :start start))))
      
      (let ((end (random (length lst))))
        (is (eql (find-if-not* #'oddp (lfor x :in lst) :end end)
                 (find-if-not  #'oddp lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (eql (find-if-not* #'oddp (lfor x :in lst) :start start :end end)
                 (find-if-not  #'oddp lst :start start :end end)))))))


(test ?find-if-not*.array
  (is (eql (find-if-not* #'oddp #2A((1 42 1) (1 1 1)) :start '(0 1))
           42))
  (is (eql (find-if-not* #'oddp #2A((1 42 1) (1 1 1)) :start '(0 2))
           nil))
  
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (let* ((dim0 3)
           (dim1 (length lst))
           (init (loop :repeat dim0 :collect lst))
           (ary  (make-array (list dim0 dim1) :initial-contents init)))
      
      (is (eql (find-if-not* #'oddp ary)
               (find-if-not  #'oddp lst)))

      (is (eql (find-if-not* #'oddp ary :key #'1+)
               (find-if-not  #'oddp lst :key #'1+)))
      
      (let ((rand (random (1+ (* dim0 dim1)))))
        (unless (zerop rand)
          (is (eql (find-if-not* #'oddp ary :start rand)
                   (loop :for i :from rand :below (* dim0 dim1)
                         :unless (funcall #'oddp (row-major-aref ary i))
                           :return (row-major-aref ary i))))
          
          (is (eql (find-if-not* #'oddp ary :end rand)
                   (loop :for i :from 0 :below rand
                         :unless (funcall #'oddp (row-major-aref ary i))
                           :return (row-major-aref ary i))))

          (let ((start (1+ (random rand))))
            (is (eql (find-if-not* #'oddp ary :start start :end rand)
                     (loop :for i :from start :below rand
                           :unless (funcall #'oddp (row-major-aref ary i))
                             :return (row-major-aref ary i)))))))

      (is (eql (find-if-not* #'oddp ary :from-end t)
               (loop :for i :downfrom (1- (array-total-size ary)) :to 0
                     :unless (funcall #'oddp (row-major-aref ary i))
                       :return (row-major-aref ary i)))))))


(test ?find-if-not*.hash-table
  (for-all ((keys (gen-list :length (gen-integer :min 0 :max 5)))
            (vals (gen-list :length (gen-integer :min 0 :max 5))))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :in keys
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      
      (is (eql (find-if-not* #'oddp ht)
               (loop :for v :being :the :hash-values :of ht
                     :unless (oddp v)
                       :return v)))
      
      (is (eql (find-if-not* #'oddp ht :key #'1+)
               (loop :for v :being :the :hash-values :of ht
                     :unless (oddp (1+ v))
                       :return v)))

      ;; keyword start should be ignored.
      (is (eql (find-if-not* #'oddp ht :start 10)
               (loop :for v :being :the :hash-values :of ht
                     :unless (oddp v)
                       :return v)))
      
      ;; keyword end should be ignored.      
      (is (eql (find-if-not* #'oddp ht :end 10)
               (loop :for v :being :the :hash-values :of ht
                     :unless (oddp v)
                       :return v)))
      
      ;; keyword from-end should be ignored.
      (is (eql (find-if-not* #'oddp ht :from-end t)
               (loop :for v :being :the :hash-values :of ht
                     :unless (oddp v)
                       :return v))))))


;;--------------------------------------------------------------------
;; position*
;;--------------------------------------------------------------------

(test ?position*.error
  (signals type-error (position* 42 :foo))
  (signals type-error (position* 42 '(0 1 2) :key #()))
  (signals type-error (position* 42 '(0 1 2) :start -1))
  (signals type-error (position* 42 '(0 1 2) :start nil))
  (signals type-error (position* 42 '(0 1 2) :start '(0 1)))
  (signals type-error (position* 42 '(0 1 2) :end -1))
  (signals error      (position* 42 '(0 1 2) :start 42 :end 24))
  (signals error      (position* 42 '(0 1 2) :end '(0 1)))
  (signals error      (position* 2 #2A((0 1 2) (3 4 5)) :start '(1 1) :end '(0 1)))
  (finishes (position* 4 #2A((0 1 2) (3 4 5)) :start '(0 1)))
  (finishes (position* 2 #2A((0 1 2) (3 4 5)) :end '(1 1)))
  (finishes (position* 2 #2A((0 1 2) (3 4 5)) :start '(0 1) :end '(1 1))))


(test ?position*.list
  (for-all ((lst (gen-list :elements (gen-integer :min 0 :max 10)
                           :length (gen-integer :min 0 :max 6))))
    (is (eql (position* 0 lst)
             (position  0 lst)))

    (is (eql (position* 0 lst :key #'1+)
             (position  0 lst :key #'1+)))

    (unless (null lst)
      (let ((start (random (length lst))))
        (is (eql (position* 0 lst :start start)
                 (position  0 lst :start start))))
      
      (let ((end (random (length lst))))
        (is (eql (position* 0 lst :end end)
                 (position  0 lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (eql (position* 0 lst :start start :end end)
                 (position  0 lst :start start :end end)))))

    (is (eql (position* 0 lst :from-end t)
             (position  0 lst :from-end t)))))


(test ?position*.vector
  (for-all ((lst (gen-list :elements (gen-integer :min 0 :max 10)
                           :length (gen-integer :min 0 :max 6))))
    (let ((vec (coerce lst 'vector)))
      
      (is (eql (position* 0 vec)
               (position  0 vec)))

      (is (eql (position* 0 vec :key #'1+)
               (position  0 vec :key #'1+)))

      (unless (null lst)
        (let ((start (random (length vec))))
          (is (eql (position* 0 vec :start start)
                   (position  0 vec :start start))))
        
        (let ((end (random (length vec))))
          (is (eql (position* 0 vec :end end)
                   (position  0 vec :end end))))

        (let* ((end   (random (length vec)))
               (start (random (1+ end))))
          (is (eql (position* 0 vec :start start :end end)
                   (position  0 vec :start start :end end)))))

      (is (eql (position* 0 vec :from-end t)
               (position  0 vec :from-end t))))))


(test ?position*.lazy-sequence
  (for-all ((lst (gen-list :elements (gen-integer :min 0 :max 10)
                           :length (gen-integer :min 0 :max 10))))
    (is-eql (position* 0 (lfor x :in lst))
            (position  0 lst))
    
    (is-eql (position* 0 (lfor x :in lst) :key #'1+)
            (position  0 lst :key #'1+))
    
    (let ((rand (random (max 1 (length lst)))))
      (is-eql (position* 0 (lfor x :in lst) :start rand)
              (position  0 lst :start rand))
      
      (is-eql (position* 0 (lfor x :in lst) :end rand)
              (position  0 lst :end rand))

      (let* ((start (random (max 1 rand))))
        (is-eql (position* 0 (lfor x :in lst) :start start :end rand)
                (position  0 lst :start start :end rand))))))


(test ?position*.array
  (is-eql (position* 42 #2A((1 42 3) (4 5 6)) :start '(0 1))
          1)
  (is-eql (position* 42 #2A((1 42 3) (4 5 6)) :start '(0 2))
          nil)
  (is-equal (position* 42 #2A((1 2 3) (42 5 6)) :subscript t)
            '(1 0))
  (is-equal (position* 42 #2A((1 2 3) (4 5 6)) :subscript t)
            nil)
  
  (for-all ((lst (gen-list :elements (gen-integer :min 0 :max 10)
                           :length (gen-integer :min 0 :max 10))))
    (let* ((dim0 3)
           (dim1 (length lst))
           (init (loop :repeat dim0 :collect lst))
           (ary  (make-array (list dim0 dim1) :initial-contents init)))
      
      (is-eql (position* 0 ary)
              (position 0 lst))

      (is-eql (position* 0 ary :key #'1+)
              (position 0 lst :key #'1+))
      
      (let ((rand (random (1+ (* dim0 dim1)))))
        (unless (zerop rand)
          (is-eql (position* 0 ary :start rand)
                  (loop :for i :from rand :below (* dim0 dim1)
                        :when (eql 0 (row-major-aref ary i))
                          :return i))
          
          (is-eql (position* 0 ary :end rand)
                  (loop :for i :from 0 :below rand
                        :when (eql 0 (row-major-aref ary i))
                          :return i))

          (let ((start (1+ (random rand))))
            (is-eql (position* 0 ary :start start :end rand)
                    (loop :for i :from start :below rand
                          :when (eql 0 (row-major-aref ary i))
                            :return i)))))

      (is-eql (position* 0 ary :from-end t)
              (loop :for i :downfrom (1- (array-total-size ary)) :to 0
                    :when (eql 0 (row-major-aref ary i))
                      :return i)))))


(test ?position*.hash-table
  (for-all ((keys (gen-list :length   (gen-integer :min 0 :max 30)))
            (vals (gen-list :elements (gen-integer :min 0 :max 10)
                            :length   (gen-integer :min 0 :max 30))))
    (let ((ht (loop :with ht := (make-hash-table)
                    :for k :in keys
                    :for v :in vals
                    :do (setf (gethash k ht) v)
                    :finally (return ht))))

      (is-eql (position* 0 ht)
              (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                    :when (eql 0 v)
                      :return k))
      
      (is-eql (position* 0 ht :key #'1+)
              (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                    :when (eql 0 (1+ v))
                      :return k))

      ;; keyword start should be ignored.
      (is-eql (position* 0 ht :start 10)
              (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                    :when (eql 0 v)
                      :return k))
      
      ;; keyword end should be ignored.      
      (is-eql (position* 0 ht :end 10)
              (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                    :when (eql 0 v)
                      :return k))
      
      ;; keyword from-end should be ignored.
      (is-eql (position* 0 ht :from-end t)
              (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                    :when (eql 0 v)
                      :return k)))))


;;--------------------------------------------------------------------
;; position-if*
;;--------------------------------------------------------------------

(test ?position-if*.error
  (signals type-error (position-if* #() '(0 1 2)))
  (signals type-error (position-if* #'oddp :foo))
  (signals type-error (position-if* #'oddp '(0 1 2) :key #()))
  (signals type-error (position-if* #'oddp '(0 1 2) :start -1))
  (signals type-error (position-if* #'oddp '(0 1 2) :start nil))
  (signals type-error (position-if* #'oddp '(0 1 2) :start '(0 1)))
  (is-true (position-if* #'oddp #2A((0 1 2) (3 4 5)) :start '(0 1))) ; check no-error

  (signals type-error (position-if* #'oddp '(0 1 2) :end -1))
  (signals type-error (position-if* #'oddp '(0 1 2) :end '(0 1)))
  (is-true (position-if* #'oddp #2A((0 1 2) (3 4 5)) :end '(1 1))) ; check no-error

  (signals simple-error (position-if* #'oddp '(0 1 2) :start 42 :end 24))
  (is-true (position-if* #'oddp #2A((0 1 2) (3 4 5)) :start '(0 1) :end '(1 1))) ; check no-error
  )


(test ?position-if*.list
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (is (eql (position-if* #'oddp lst)
             (position-if  #'oddp lst)))

    (is (eql (position-if* #'oddp lst :key #'1+)
             (position-if  #'oddp lst :key #'1+)))

    (unless (null lst)
      (let ((start (random (length lst))))
        (is (eql (position-if* #'oddp lst :start start)
                 (position-if  #'oddp lst :start start))))
      
      (let ((end (random (length lst))))
        (is (eql (position-if* #'oddp lst :end end)
                 (position-if  #'oddp lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (eql (position-if* #'oddp lst :start start :end end)
                 (position-if  #'oddp lst :start start :end end)))))

    (is (eql (position-if* #'oddp lst :from-end t)
             (position-if  #'oddp lst :from-end t)))))


(test ?position-if*.vector
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (let ((vec (coerce lst 'vector)))
      
      (is (eql (position-if* #'oddp vec)
               (position-if  #'oddp vec)))

      (is (eql (position-if* #'oddp vec :key #'1+)
               (position-if  #'oddp vec :key #'1+)))

      (unless (null lst)
        (let ((start (random (length vec))))
          (is (eql (position-if* #'oddp vec :start start)
                   (position-if  #'oddp vec :start start))))
        
        (let ((end (random (length vec))))
          (is (eql (position-if* #'oddp vec :end end)
                   (position-if  #'oddp vec :end end))))

        (let* ((end   (random (length vec)))
               (start (random (1+ end))))
          (is (eql (position-if* #'oddp vec :start start :end end)
                   (position-if  #'oddp vec :start start :end end)))))

      (is (eql (position-if* #'oddp vec :from-end t)
               (position-if  #'oddp vec :from-end t))))))


(test ?position-if*.lazy-sequence
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (is (eql (position-if* #'oddp (lfor x :in lst))
             (position-if  #'oddp lst)))
    
    (is (eql (position-if* #'oddp (lfor x :in lst) :key #'1+)
             (position-if  #'oddp lst :key #'1+)))
    
    (unless (null lst)
      (let ((start (random (length lst))))
        (is (eql (position-if* #'oddp (lfor x :in lst) :start start)
                 (position-if  #'oddp lst :start start))))
      
      (let ((end (random (length lst))))
        (is (eql (position-if* #'oddp (lfor x :in lst) :end end)
                 (position-if  #'oddp lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (eql (position-if* #'oddp (lfor x :in lst) :start start :end end)
                 (position-if  #'oddp lst :start start :end end)))))))


(test ?position-if*.array
  (is (eql (position-if* #'evenp #2A((1 42 1) (1 1 1)) :start '(0 1))
           1))
  (is (eql (position-if* #'evenp #2A((1 42 1) (1 1 1)) :start '(0 2))
           nil))
  (is (equal (position-if* #'evenp #2A((1 1 1) (42 1 1)) :subscript t)
             '(1 0)))
  (is (equal (position-if* #'evenp #2A((1 1 1) (1 1 1)) :subscript t)
             nil))
  
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (let* ((dim0 3)
           (dim1 (length lst))
           (init (loop :repeat dim0 :collect lst))
           (ary  (make-array (list dim0 dim1) :initial-contents init)))
      
      (is (eql (position-if* #'oddp ary)
               (position-if  #'oddp lst)))

      (is (eql (position-if* #'oddp ary :key #'1+)
               (position-if  #'oddp lst :key #'1+)))
      
      (let ((rand (random (1+ (* dim0 dim1)))))
        (unless (zerop rand)
          (is (eql (position-if* #'oddp ary :start rand)
                   (loop :for i :from rand :below (* dim0 dim1)
                         :when (funcall #'oddp (row-major-aref ary i))
                           :return i)))
          
          (is (eql (position-if* #'oddp ary :end rand)
                   (loop :for i :from 0 :below rand
                         :when (funcall #'oddp (row-major-aref ary i))
                           :return i)))

          (let ((start (1+ (random rand))))
            (is (eql (position-if* #'oddp ary :start start :end rand)
                     (loop :for i :from start :below rand
                           :when (funcall #'oddp (row-major-aref ary i))
                             :return i))))))

      (is (eql (position-if* #'oddp ary :from-end t)
               (loop :for i :downfrom (1- (array-total-size ary)) :to 0
                     :when (funcall #'oddp (row-major-aref ary i))
                       :return i))))))


(test ?position-if*.hash-table
  (for-all ((keys (gen-list :length (gen-integer :min 0 :max 5)))
            (vals (gen-list :length (gen-integer :min 0 :max 5))))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :in keys
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      
      (is (eql (position-if* #'oddp ht)
               (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                     :when (oddp v)
                       :return k)))
      
      (is (eql (position-if* #'oddp ht :key #'1+)
               (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                     :when (oddp (1+ v))
                       :return k)))

      ;; keyword start should be ignored.
      (is (eql (position-if* #'oddp ht :start 10)
               (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                     :when (oddp v)
                       :return k)))
      
      ;; keyword end should be ignored.      
      (is (eql (position-if* #'oddp ht :end 10)
               (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                     :when (oddp v)
                       :return k)))
      
      ;; keyword from-end should be ignored.
      (is (eql (position-if* #'oddp ht :from-end t)
               (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                     :when (oddp v)
                       :return k))))))


;;--------------------------------------------------------------------
;; position-if-not*
;;--------------------------------------------------------------------


(test ?position-if-not*.error
  (signals type-error (position-if-not* #() '(0 1 2)))
  (signals type-error (position-if-not* #'oddp :foo))
  (signals type-error (position-if-not* #'oddp '(0 1 2) :key #()))
  (signals type-error (position-if-not* #'oddp '(0 1 2) :start -1))
  (signals type-error (position-if-not* #'oddp '(0 1 2) :start nil))
  (signals type-error (position-if-not* #'oddp '(0 1 2) :start '(0 1)))
  (is-true (position-if-not* #'oddp #2A((0 1 2) (3 4 5)) :start '(0 1))) ; check no-error

  (signals type-error (position-if-not* #'oddp '(0 1 2) :end -1))
  (signals type-error (position-if-not* #'oddp '(0 1 2) :end '(0 1)))
  (is-true (position-if-not* #'oddp #2A((0 1 2) (3 4 5)) :end '(1 1))) ; check no-error

  (signals simple-error (position-if-not* #'oddp '(0 1 2) :start 42 :end 24))
  (is-true (position-if-not* #'oddp #2A((0 1 2) (3 4 5)) :start '(0 1) :end '(1 1))) ; check no-error
  )

(test ?position-if-not*.list
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (is (eql (position-if-not* #'oddp lst)
             (position-if-not  #'oddp lst)))

    (is (eql (position-if-not* #'oddp lst :key #'1+)
             (position-if-not  #'oddp lst :key #'1+)))

    (unless (null lst)
      (let ((start (random (length lst))))
        (is (eql (position-if-not* #'oddp lst :start start)
                 (position-if-not  #'oddp lst :start start))))
      
      (let ((end (random (length lst))))
        (is (eql (position-if-not* #'oddp lst :end end)
                 (position-if-not  #'oddp lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (eql (position-if-not* #'oddp lst :start start :end end)
                 (position-if-not  #'oddp lst :start start :end end)))))

    (is (eql (position-if-not* #'oddp lst :from-end t)
             (position-if-not  #'oddp lst :from-end t)))))


(test ?position-if-not*.vector
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (let ((vec (coerce lst 'vector)))
      
      (is (eql (position-if-not* #'oddp vec)
               (position-if-not  #'oddp vec)))

      (is (eql (position-if-not* #'oddp vec :key #'1+)
               (position-if-not  #'oddp vec :key #'1+)))

      (unless (null lst)
        (let ((start (random (length vec))))
          (is (eql (position-if-not* #'oddp vec :start start)
                   (position-if-not  #'oddp vec :start start))))
        
        (let ((end (random (length vec))))
          (is (eql (position-if-not* #'oddp vec :end end)
                   (position-if-not  #'oddp vec :end end))))

        (let* ((end   (random (length vec)))
               (start (random (1+ end))))
          (is (eql (position-if-not* #'oddp vec :start start :end end)
                   (position-if-not  #'oddp vec :start start :end end)))))

      (is (eql (position-if-not* #'oddp vec :from-end t)
               (position-if-not  #'oddp vec :from-end t))))))


(test ?position-if-not*.lazy-sequence
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (is (eql (position-if-not* #'oddp (lfor x :in lst))
             (position-if-not  #'oddp lst)))
    
    (is (eql (position-if-not* #'oddp (lfor x :in lst) :key #'1+)
             (position-if-not  #'oddp lst :key #'1+)))
    
    (unless (null lst)
      (let ((start (random (length lst))))
        (is (eql (position-if-not* #'oddp (lfor x :in lst) :start start)
                 (position-if-not  #'oddp lst :start start))))
      
      (let ((end (random (length lst))))
        (is (eql (position-if-not* #'oddp (lfor x :in lst) :end end)
                 (position-if-not  #'oddp lst :end end))))

      (let* ((end   (random (length lst)))
             (start (random (1+ end))))
        (is (eql (position-if-not* #'oddp (lfor x :in lst) :start start :end end)
                 (position-if-not  #'oddp lst :start start :end end)))))))


(test ?position-if-not*.array
  (is (eql (position-if-not* #'oddp #2A((1 42 1) (1 1 1)) :start '(0 1))
           1))
  (is (eql (position-if-not* #'oddp #2A((1 42 1) (1 1 1)) :start '(0 2))
           nil))
  (is (equal (position-if-not* #'oddp #2A((1 1 1) (42 1 1)) :subscript t)
             '(1 0)))
  (is (equal (position-if-not* #'oddp #2A((1 1 1) (1 1 1)) :subscript t)
             nil))
  
  (for-all ((lst (gen-list :length (gen-integer :min 0 :max 5))))
    (let* ((dim0 3)
           (dim1 (length lst))
           (init (loop :repeat dim0 :collect lst))
           (ary  (make-array (list dim0 dim1) :initial-contents init)))
      
      (is (eql (position-if-not* #'oddp ary)
               (position-if-not  #'oddp lst)))

      (is (eql (position-if-not* #'oddp ary :key #'1+)
               (position-if-not  #'oddp lst :key #'1+)))
      
      (let ((rand (random (1+ (* dim0 dim1)))))
        (unless (zerop rand)
          (is (eql (position-if-not* #'oddp ary :start rand)
                   (loop :for i :from rand :below (* dim0 dim1)
                         :unless (funcall #'oddp (row-major-aref ary i))
                           :return i)))
          
          (is (eql (position-if-not* #'oddp ary :end rand)
                   (loop :for i :from 0 :below rand
                         :unless (funcall #'oddp (row-major-aref ary i))
                           :return i)))

          (let ((start (1+ (random rand))))
            (is (eql (position-if-not* #'oddp ary :start start :end rand)
                     (loop :for i :from start :below rand
                           :unless (funcall #'oddp (row-major-aref ary i))
                             :return i))))))

      (is (eql (position-if-not* #'oddp ary :from-end t)
               (loop :for i :downfrom (1- (array-total-size ary)) :to 0
                     :unless (funcall #'oddp (row-major-aref ary i))
                       :return i))))))


(test ?position-if-not*.hash-table
  (for-all ((keys (gen-list :length (gen-integer :min 0 :max 5)))
            (vals (gen-list :length (gen-integer :min 0 :max 5))))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :in keys
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      
      (is (eql (position-if-not* #'oddp ht)
               (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                     :unless (oddp v)
                       :return k)))
      
      (is (eql (position-if-not* #'oddp ht :key #'1+)
               (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                     :unless (oddp (1+ v))
                       :return k)))

      ;; keyword start should be ignored.
      (is (eql (position-if-not* #'oddp ht :start 10)
               (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                     :unless (oddp v)
                       :return k)))
      
      ;; keyword end should be ignored.      
      (is (eql (position-if-not* #'oddp ht :end 10)
               (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                     :unless (oddp v)
                       :return k)))
      
      ;; keyword from-end should be ignored.
      (is (eql (position-if-not* #'oddp ht :from-end t)
               (loop :for v :being :the :hash-values :of ht :using (:hash-key k)
                     :unless (oddp v)
                       :return k))))))


;;--------------------------------------------------------------------
;; remove*
;;--------------------------------------------------------------------

(test ?remove*.error
  (signals type-error (remove* 0 :foo))
  (signals type-error (remove* 0 '(0 1 2 3) :key #()))
  (signals type-error (remove* 0 '(0 1 2 3) :start nil))
  (signals type-error (remove* 0 '(0 1 2 3) :start -1))
  (signals type-error (remove* 0 '(0 1 2 3) :start '(1)))
  (signals type-error (remove* 0 '(0 1 2 3) :end '(1)))
  (signals type-error (remove* 0 '(0 1 2 3) :end -1))
  (signals type-error (remove* 0 '(0 1 2 3) :count -1))
  (signals type-error (remove* 0 '(0 1 2 3) :count '(-1)))
  (signals simple-error (remove* 0 '(0 1 2 3) :start 42 :end 24)))


(test ?remove*.list
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (is-equal (remove* 0 lst)
              (remove  0 lst))

    (is-equal (remove* 0 lst :key #'1+)
              (remove  0 lst :key #'1+))

    (is-equal (remove* 0 lst :from-end t)
              (remove  0 lst :from-end t))

    (let ((rand (random (max 1 (length lst)))))
      (is-equal (remove* 0 lst :count rand)
                (remove  0 lst :count rand))
      
      (is-equal (remove* 0 lst :count rand :from-end t)
                (remove  0 lst :count rand :from-end t))
      
      (is-equal (remove* 0 lst :start rand)
                (remove  0 lst :start rand))
      
      (is-equal (remove* 0 lst :end rand)
                (remove  0 lst :end rand))
      
      (let ((rand2 (random (max 1 rand))))
        (is-equal (remove* 0 lst :start rand2 :end rand)
                  (remove  0 lst :start rand2 :end rand))))))


(test ?remove*.vector
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((vec (coerce lst 'vector)))
      (is-equalp (remove* 0 vec)
                 (remove  0 vec))

      (is-equalp (remove* 0 vec :key #'1+)
                 (remove  0 vec :key #'1+))

      (is-equalp (remove* 0 vec :from-end t)
                 (remove  0 vec :from-end t))

      (let ((rand (random (max 1 (length vec)))))
        (is-equalp (remove* 0 vec :count rand)
                   (remove  0 vec :count rand))
      
        (is-equalp (remove* 0 vec :count rand :from-end t)
                   (remove  0 vec :count rand :from-end t))
      
        (is-equalp (remove* 0 vec :start rand)
                   (remove  0 vec :start rand))
      
        (is-equalp (remove* 0 vec :end rand)
                   (remove  0 vec :end rand))
      
        (let ((rand2 (random (max 1 rand))))
          (is-equalp (remove* 0 vec :start rand2 :end rand)
                     (remove  0 vec :start rand2 :end rand)))))))


(test ?remove*.lazy-sequence
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((lseq (lfor x :in lst)))

      (is-equal (lazy-take :all (remove* 0 lseq))
                (remove 0 lst))

      (is-equal (lazy-take :all (remove* 0 lseq :key #'1+))
                (remove 0 lst :key #'1+))

      (is-equal (lazy-take :all (remove* 0 lseq :from-end t))
                (remove 0 lst :from-end t))

      (let ((rand (random (max 1 (length lst)))))
        ;; (REMOVE 0 '(0 -1 0 0 -1) :COUNT 0) => (0 -1 0 0 -1)
        ;; (lazy-take :all (remove* 0 (lfor x :in '(0 -1 0 0 -1)) :count 0)) => (-1 1)
        (is-equal (lazy-take :all (remove* 0 lseq :count rand))
                  (remove 0 lst :count rand))
        
        ;; :from-end without :end is ignored.
        ;; (is-equal (lazy-take :all (remove* 0 lseq :count rand :from-end t))
        ;;           (remove 0 lst :count rand :from-end t))

        (is-equal (lazy-take :all (remove* 0 lseq :start rand))
                  (remove 0 lst :start rand))

        (is-equal (lazy-take :all (remove* 0 lseq :end rand))
                  (remove 0 lst :end rand))
        
        (let ((rand2 (random (max 1 rand))))
          (is-equal (lazy-take :all (remove* 0 lseq :start rand2 :end rand))
                    (remove 0 lst :start rand2 :end rand)))))))


(test ?remove*.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (alst (loop :for i :from 0
                      :for v :in vals
                      :collect (cons i v))))
            
      (let ((result* (remove* 0 ht))
            (result  (remove  0 alst :key #'cdr)))
        (is (= (hash-table-count result*)
               (length result)))
        (is-true (loop :for (i . v) :in result
                       :always (= v (gethash i result*)))))

      (let ((result* (remove* 0 ht :key #'1+))
            (result  (remove  0 alst :key (lambda (e) (1+ (cdr e))))))
        (is (= (hash-table-count result*)
               (length result)))
        (is-true (loop :for (i . v) :in result
                       :always (= v (gethash i result*))))))))

(test ?remove*.array
  (signals simple-error (remove* 42 #2A((0 1 2) (3 4 5)))))


;;--------------------------------------------------------------------
;; remove-if*
;;--------------------------------------------------------------------

(test ?remove-if*.error
  (signals type-error (remove-if* #() '(0 1 2 3)))
  (signals type-error (remove-if* #'oddp :foo))
  (signals type-error (remove-if* #'oddp '(0 1 2 3) :key #()))
  (signals type-error (remove-if* #'oddp '(0 1 2 3) :start nil))
  (signals type-error (remove-if* #'oddp '(0 1 2 3) :start -1))
  (signals type-error (remove-if* #'oddp '(0 1 2 3) :start '(1)))
  (signals type-error (remove-if* #'oddp '(0 1 2 3) :end '(1)))
  (signals type-error (remove-if* #'oddp '(0 1 2 3) :end -1))
  (signals type-error (remove-if* #'oddp '(0 1 2 3) :count -1))
  (signals type-error (remove-if* #'oddp '(0 1 2 3) :count '(-1)))
  (signals simple-error (remove-if* #'oddp '(0 1 2 3) :start 42 :end 24)))


(test ?remove-if*.list
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (is (equal (remove-if* #'oddp lst)
               (remove-if  #'oddp lst)))

    (is (equal (remove-if* #'oddp lst :key #'1+)
               (remove-if  #'oddp lst :key #'1+)))

    (is (equal (remove-if*   #'oddp lst :from-end t)
               (remove-if #'oddp lst :from-end t)))

    (let ((rand (random (max 1 (length lst)))))
      (is (equal (remove-if* #'oddp lst :count rand)
                 (remove-if  #'oddp lst :count rand)))
      
      (is (equal (remove-if* #'oddp lst :count rand :from-end t)
                 (remove-if  #'oddp lst :count rand :from-end t)))
      
      (is (equal (remove-if* #'oddp lst :start rand)
                 (remove-if  #'oddp lst :start rand)))
      
      (is (equal (remove-if* #'oddp lst :end rand)
                 (remove-if  #'oddp lst :end rand)))
      
      (let ((rand2 (random (max 1 rand))))
        (is (equal (remove-if* #'oddp lst :start rand2 :end rand)
                   (remove-if  #'oddp lst :start rand2 :end rand)))))))


(test ?remove-if*.vector
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((vec (coerce lst 'vector)))
      (is (equalp (remove-if* #'oddp vec)
                  (remove-if  #'oddp vec)))

      (is (equalp (remove-if* #'oddp vec :key #'1+)
                  (remove-if  #'oddp vec :key #'1+)))

      (is (equalp (remove-if*   #'oddp vec :from-end t)
                  (remove-if #'oddp vec :from-end t)))

      (let ((rand (random (max 1 (length vec)))))
        (is (equalp (remove-if* #'oddp vec :count rand)
                    (remove-if  #'oddp vec :count rand)))
      
        (is (equalp (remove-if* #'oddp vec :count rand :from-end t)
                    (remove-if  #'oddp vec :count rand :from-end t)))
      
        (is (equalp (remove-if* #'oddp vec :start rand)
                    (remove-if  #'oddp vec :start rand)))
      
        (is (equalp (remove-if* #'oddp vec :end rand)
                    (remove-if  #'oddp vec :end rand)))
      
        (let ((rand2 (random (max 1 rand))))
          (is (equalp (remove-if* #'oddp vec :start rand2 :end rand)
                      (remove-if  #'oddp vec :start rand2 :end rand))))))))

;; TODO:
(test ?remove-if*.lazy-sequence
  )


(test ?remove-if*.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (alst (loop :for i :from 0
                      :for v :in vals
                      :collect (cons i v))))
      
      (let ((result* (remove-if* #'oddp ht))
            (result  (remove-if  #'oddp alst :key #'cdr)))
        (is (= (hash-table-count result*)
               (length result)))
        (is-true (loop :for (i . v) :in result
                       :always (= v (gethash i result*)))))

      (let ((result* (remove-if* #'oddp ht :key #'1+))
            (result  (remove-if  #'oddp alst :key (lambda (e) (1+ (cdr e))))))
        (is (= (hash-table-count result*)
               (length result)))
        (is-true (loop :for (i . v) :in result
                       :always (= v (gethash i result*))))))))


(test ?remove-if*.array
  (signals simple-error (remove-if* #'oddp #2A((0 1 2) (3 4 5)))))


;;--------------------------------------------------------------------
;; remove-if-not*
;;--------------------------------------------------------------------

(test ?remove-if-not*.error
  (signals type-error (remove-if-not* #() '(0 1 2 3)))
  (signals type-error (remove-if-not* #'oddp :foo))
  (signals type-error (remove-if-not* #'oddp '(0 1 2 3) :key #()))
  (signals type-error (remove-if-not* #'oddp '(0 1 2 3) :start nil))
  (signals type-error (remove-if-not* #'oddp '(0 1 2 3) :start -1))
  (signals type-error (remove-if-not* #'oddp '(0 1 2 3) :start '(1)))
  (signals type-error (remove-if-not* #'oddp '(0 1 2 3) :end '(1)))
  (signals type-error (remove-if-not* #'oddp '(0 1 2 3) :end -1))
  (signals type-error (remove-if-not* #'oddp '(0 1 2 3) :count -1))
  (signals type-error (remove-if-not* #'oddp '(0 1 2 3) :count '(-1)))
  (signals simple-error (remove-if-not* #'oddp '(0 1 2 3) :start 42 :end 24)))


(test ?remove-if-not*.list
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (is (equal (remove-if-not* #'oddp lst)
               (remove-if-not  #'oddp lst)))

    (is (equal (remove-if-not* #'oddp lst :key #'1+)
               (remove-if-not  #'oddp lst :key #'1+)))

    (is (equal (remove-if-not*   #'oddp lst :from-end t)
               (remove-if-not #'oddp lst :from-end t)))

    (let ((rand (random (max 1 (length lst)))))
      (is (equal (remove-if-not* #'oddp lst :count rand)
                 (remove-if-not  #'oddp lst :count rand)))
      
      (is (equal (remove-if-not* #'oddp lst :count rand :from-end t)
                 (remove-if-not  #'oddp lst :count rand :from-end t)))
      
      (is (equal (remove-if-not* #'oddp lst :start rand)
                 (remove-if-not  #'oddp lst :start rand)))
      
      (is (equal (remove-if-not* #'oddp lst :end rand)
                 (remove-if-not  #'oddp lst :end rand)))
      
      (let ((rand2 (random (max 1 rand))))
        (is (equal (remove-if-not* #'oddp lst :start rand2 :end rand)
                   (remove-if-not  #'oddp lst :start rand2 :end rand)))))))


(test ?remove-if-not*.vector
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((vec (coerce lst 'vector)))
      (is (equalp (remove-if-not* #'oddp vec)
                  (remove-if-not  #'oddp vec)))

      (is (equalp (remove-if-not* #'oddp vec :key #'1+)
                  (remove-if-not  #'oddp vec :key #'1+)))

      (is (equalp (remove-if-not*   #'oddp vec :from-end t)
                  (remove-if-not #'oddp vec :from-end t)))

      (let ((rand (random (max 1 (length vec)))))
        (is (equalp (remove-if-not* #'oddp vec :count rand)
                    (remove-if-not  #'oddp vec :count rand)))
      
        (is (equalp (remove-if-not* #'oddp vec :count rand :from-end t)
                    (remove-if-not  #'oddp vec :count rand :from-end t)))
      
        (is (equalp (remove-if-not* #'oddp vec :start rand)
                    (remove-if-not  #'oddp vec :start rand)))
      
        (is (equalp (remove-if-not* #'oddp vec :end rand)
                    (remove-if-not  #'oddp vec :end rand)))
      
        (let ((rand2 (random (max 1 rand))))
          (is (equalp (remove-if-not* #'oddp vec :start rand2 :end rand)
                      (remove-if-not  #'oddp vec :start rand2 :end rand))))))))

;; TODO:
(test ?remove-if-not*.lazy-sequence
  )


(test ?remove-if-not*.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (alst (loop :for i :from 0
                      :for v :in vals
                      :collect (cons i v))))
      
      (let ((result* (remove-if-not* #'oddp ht))
            (result  (remove-if-not  #'oddp alst :key #'cdr)))
        (is (= (hash-table-count result*)
               (length result)))
        (is-true (loop :for (i . v) :in result
                       :always (= v (gethash i result*)))))

      (let ((result* (remove-if-not* #'oddp ht :key #'1+))
            (result  (remove-if-not  #'oddp alst :key (lambda (e) (1+ (cdr e))))))
        (is (= (hash-table-count result*)
               (length result)))
        (is-true (loop :for (i . v) :in result
                       :always (= v (gethash i result*))))))))


(test ?remove-if-not*.array
  (signals simple-error (remove-if-not* #'oddp #2A((0 1 2) (3 4 5)))))


;;--------------------------------------------------------------------
;; collect-if*
;;--------------------------------------------------------------------

(test ?collect-if*.error
  (signals type-error (collect-if* #() '(0 1 2 3)))
  (signals type-error (collect-if* #'oddp :foo))
  (signals type-error (collect-if* #'oddp '(0 1 2 3) :key #()))
  (signals type-error (collect-if* #'oddp '(0 1 2 3) :from nil))
  (signals type-error (collect-if* #'oddp '(0 1 2 3) :from -1))
  (signals type-error (collect-if* #'oddp '(0 1 2 3) :from '(1)))
  (signals type-error (collect-if* #'oddp '(0 1 2 3) :below '(1)))
  (signals type-error (collect-if* #'oddp '(0 1 2 3) :below -1))
  (signals type-error (collect-if* #'oddp '(0 1 2 3) :count -1))
  (signals type-error (collect-if* #'oddp '(0 1 2 3) :count '(-1)))
  (signals simple-error (collect-if* #'oddp '(0 1 2 3) :from 42 :below 24)))


(test ?collect-if*.list
  (for-all ((lst (gen-list)))
    (is-equal (collect-if*   #'oddp lst)
              (remove-if-not #'oddp lst))

    (is-equal (collect-if*   #'oddp lst :key #'1+)
              (remove-if-not #'oddp lst :key #'1+))

    (is-equal (collect-if*   #'oddp lst :from-end t)
              (remove-if-not #'oddp lst :from-end t))

    (let ((rand (random (max 1 (length lst)))))
      ;; MEMO:
      ;; (collect-if*   #'oddp '(0 1 2 3 4 5) :count 2) => (1 3)
      ;; (remove-if-not #'oddp '(0 1 2 3 4 5) :count 2) => (1 3 4 5)
      (is-equal (collect-if* #'oddp lst :count rand)
                (loop :for v :in lst
                      :with count := rand
                      :until (zerop count)
                      :when (oddp v)
                        :do (decf count) :and :collect v))
      
      (is-equal (collect-if* #'oddp lst :count rand :from-end t)
                (loop :for v :in (reverse lst)
                      :with count := rand
                      :until (zerop count)
                      :when (oddp v)
                        :do (decf count) :and :collect v :into acc
                      :finally (return (nreverse acc))))
      
      (is-equal (collect-if*   #'oddp lst :from rand)
                (remove-if-not #'oddp (subseq lst rand)))
      
      (is-equal (collect-if*   #'oddp lst :below rand)
                (remove-if-not #'oddp (subseq lst 0 rand)))
      
      (let ((rand2 (random (max 1 rand))))
        (is-equal (collect-if*   #'oddp lst :from rand2 :below rand)
                  (remove-if-not #'oddp (subseq lst rand2 rand)))))))


(test ?collect-if*.vector
  (for-all ((lst (gen-list)))
    (let ((vec (coerce lst 'vector)))
      (is-equal (collect-if*   #'oddp vec)
                (remove-if-not #'oddp lst))

      (is-equal (collect-if*   #'oddp vec :key #'1+)
                (remove-if-not #'oddp lst :key #'1+))

      (is-equal (collect-if*   #'oddp vec :from-end t)
                (remove-if-not #'oddp lst :from-end t))

      (let ((rand (random (max 1 (length vec)))))
        (is-equal (collect-if* #'oddp vec :count rand)
                  (loop :for v :across vec
                        :with count := rand
                        :until (zerop count)
                        :when (oddp v)
                          :do (decf count) :and :collect v))
        
        (is-equal (collect-if* #'oddp vec :count rand :from-end t)
                  (loop :for v :across (reverse vec)
                        :with count := rand
                        :until (zerop count)
                        :when (oddp v)
                          :do (decf count) :and :collect v :into acc
                        :finally (return (nreverse acc))))
        
        (is-equal (collect-if*   #'oddp vec :from rand)
                  (remove-if-not #'oddp (subseq lst rand)))
        
        (is-equal (collect-if*   #'oddp vec :below rand)
                  (remove-if-not #'oddp (subseq lst 0 rand)))
        
        (let ((rand2 (random (max 1 rand))))
          (is-equal (collect-if*   #'oddp vec :from rand2 :below rand)
                    (remove-if-not #'oddp (subseq lst rand2 rand))))))))


(test ?collect-if*.lazy-sequence
  (for-all ((lst (gen-list)))
    (let ((lseq (lfor x :in lst)))

      (is-equal (collect-if*   #'oddp lseq)
                (remove-if-not #'oddp lst))

      (is-equal (collect-if*   #'oddp lseq :key #'1+)
                (remove-if-not #'oddp lst  :key #'1+))

      (is-equal (collect-if*   #'oddp lseq :from-end t)
                (remove-if-not #'oddp lst  :from-end t))
      
      (let ((rand (random (max 1 (length lst)))))
        (is-equal (collect-if* #'oddp lseq :count rand)
                  (loop :for v :in lst
                        :with count := rand
                        :until (zerop count)
                        :when (oddp v)
                          :do (decf count) :and :collect v))
        
        (is-equal (collect-if*   #'oddp lseq :from rand)
                  (remove-if-not #'oddp (subseq lst rand)))
        
        (is-equal (collect-if*   #'oddp lseq :below rand)
                  (remove-if-not #'oddp (subseq lst 0 rand)))
        
        (let ((rand2 (random (max 1 rand))))
          (is-equal (collect-if*   #'oddp lseq :from rand2 :below rand)
                    (remove-if-not #'oddp (subseq lst rand2 rand))))))))


(test ?collect-if*.array
  (for-all ((lst (gen-list)))
    (let* ((dim  (length lst))
           (ary  (make-array (list 2 dim) :initial-contents (list lst lst)))
           (wlst (append lst lst)))

      (is-equal (collect-if*   #'oddp ary)
                (remove-if-not #'oddp wlst))
      
      (is-equal (collect-if*   #'oddp ary  :key #'1+)
                (remove-if-not #'oddp wlst :key #'1+))
      
      (is-equal (collect-if*   #'oddp ary  :from-end t)
                (remove-if-not #'oddp wlst :from-end t))
      
      (let ((rand (random (max 1 (length wlst)))))
      
        (is-equal (collect-if* #'oddp ary :count rand)
                  (loop :for v :in wlst
                        :with count := rand
                        :until (zerop count)
                        :when (oddp v)
                          :do (decf count) :and :collect v))
        
        (is-equal (collect-if* #'oddp ary :count rand :from-end t)
                  (loop :for v :in (reverse wlst)
                        :with count := rand
                        :until (zerop count)
                        :when (oddp v)
                          :do (decf count) :and :collect v :into acc
                        :finally (return (nreverse acc))))
        
        (is-equal (collect-if*   #'oddp ary :from rand)
                  (remove-if-not #'oddp (subseq wlst rand)))

        (is-equal (collect-if*   #'oddp ary :below rand)
                  (remove-if-not #'oddp (subseq wlst 0 rand)))
        
        (unless (null lst)
          ;; MEMO: (array-subscripts #2A(() ()) 0) => error
          (is-equal (collect-if* #'oddp ary :from rand)
                    (collect-if* #'oddp ary :from (array-subscripts ary rand)))

          (is-equal (collect-if* #'oddp ary :below rand)
                    (collect-if* #'oddp ary :below (array-subscripts ary rand))))
        
        (let ((rand2 (random (max 1 rand))))
          (is-equal (collect-if*   #'oddp ary :from rand2 :below rand)
                    (remove-if-not #'oddp (subseq wlst rand2 rand))))))))


(test ?collect-if*.hash-table
  (is (find (with-muffle-warnings
              (collect-if* #'oddp #{:foo 0 :bar 1 :baz 2 :quux 3}))
            '((1 3) (3 1))
            :test #'equal)))


;;--------------------------------------------------------------------
;; substitute*
;;--------------------------------------------------------------------

(test ?substitute*.error
  (signals type-error (substitute* 42 0 #'oddp))
  (signals type-error (substitute* 42 0 '(-1 0 1) :key   #()))
  (signals type-error (substitute* 42 0 '(-1 0 1) :count :foo))
  
  (signals type-error (substitute* 42 0 '(-1 0 1) :start -1))
  (signals type-error (substitute* 42 0 '(-1 0 1) :start nil))
  (signals type-error (substitute* 42 0 '(-1 0 1) :start :foo))
  (signals type-error (substitute* 42 0 '(-1 0 1) :start '(0 1)))
  (is-true (substitute* 42 0 #2A((-1 0 1) (-1 0 1)) :start '(0 1))) ; check no-error

  (signals type-error (substitute* 42 0 '(-1 0 1) :end -1))
  (signals type-error (substitute* 42 0 '(-1 0 1) :end :foo))
  (signals type-error (substitute* 42 0 '(-1 0 1) :end '(0 1)))
  (is-true (substitute* 42 0 #2A((-1 0 1) (-1 0 1)) :end '(0 1))) ; check no-error

  (signals simple-error (substitute* 42 0 '(0 1 2 3) :start 42 :end 24))
  (signals simple-error (substitute* 42 0 #2A((-1 0 1) (-1 0 1))
                                     :start '(0 2) :end '(0 1)))
  (is-true (substitute* 42 0 #2A((-1 0 1) (-1 0 1)) ; check no-error
                        :start '(0 1) :end '(0 2))))


(test ?substitute*.list
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (is-equal (substitute* 42 0 lst)
              (substitute  42 0 lst))
    (is-equal (substitute* 42 0 lst :key #'1+)
              (substitute  42 0 lst :key #'1+))
    (is-equal (substitute* 42 0 lst :from-end t)
              (substitute  42 0 lst :from-end t))
    
    (let ((r (random (max 1 (length lst)))))
      (is-equal (substitute* 42 0 lst :count r)
                (substitute  42 0 lst :count r))
      (is-equal (substitute* 42 0 lst :count r :from-end t)
                (substitute  42 0 lst :count r :from-end t))
      (is-equal (substitute* 42 0 lst :start r)
                (substitute  42 0 lst :start r))
      (is-equal (substitute* 42 0 lst :end   r)
                (substitute  42 0 lst :end   r))

      (let ((r1 (random (max 1 r))))
        (is-equal (substitute* 42 0 lst :start r1 :end r)
                  (substitute  42 0 lst :start r1 :end r))))))


(test ?substitute*.vector
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((vec (coerce lst 'vector)))
      
      (is-equalp (substitute* 42 0 vec)
                 (substitute  42 0 vec))
      (is-equalp (substitute* 42 0 vec :key #'1+)
                 (substitute  42 0 vec :key #'1+))
      (is-equalp (substitute* 42 0 vec :from-end t)
                 (substitute  42 0 vec :from-end t))
    
      (let ((r (random (max 1 (length vec)))))
        (is-equalp (substitute* 42 0 vec :count r)
                   (substitute  42 0 vec :count r))
        (is-equalp (substitute* 42 0 vec :count r :from-end t)
                   (substitute  42 0 vec :count r :from-end t))
        (is-equalp (substitute* 42 0 vec :start r)
                   (substitute  42 0 vec :start r))
        (is-equalp (substitute* 42 0 vec :end   r)
                   (substitute  42 0 vec :end   r))

        (let ((r1 (random (max 1 r))))
          (is-equalp (substitute* 42 0 vec :start r1 :end r)
                     (substitute  42 0 vec :start r1 :end r)))))))


(test ?substitute*.lazy-sequence
  (for-all ((lst (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((lseq (lfor x :in lst)))

      (is-equal (lazy-take :all (substitute* 42 0 lseq))
                (substitute 42 0 lst))
      (is-equal (lazy-take :all (substitute* 42 0 lseq :key #'1+))
                (substitute 42 0 lst  :key #'1+))
      (is-equal (lazy-take :all (substitute* 42 0 lseq :from-end t))
                (substitute 42 0 lst  :from-end t))
      
      (let ((r (random (max 1 (length lst)))))
        (is-equal (lazy-take :all (substitute* 42 0 lseq :count r))
                  (substitute  42 0 lst  :count r))
        
        (is-equal (lazy-take :all (substitute* 42 0 lseq :start r))
                  (substitute  42 0 lst :start r))
        
        (is-equal (lazy-take :all (substitute* 42 0 lseq :end r))
                  (substitute  42 0 lst :end r))
        
        (let ((r1 (random (max 1 r))))
          (is-equal (lazy-take :all (substitute* 42 0 lseq :start r1 :end r))
                    (substitute  42 0 lst :start r1 :end r)))))))


(test ?substitute*.array
  (for-all ((lst (gen-list :length (gen-integer :min -1 :max 1))))
    (let* ((d0  3)
           (d1  (length lst))
           (ary (make-array (list d0 d1)
                            :initial-contents (loop :repeat d0 :collect lst))))
      
      (is-equalp (substitute* 42 0 ary)
                 (let ((init (substitute 42 0 lst)))
                   (make-array (list d0 d1)
                               :initial-contents (loop :repeat d0 :collect init))))
      
      (is-equalp (substitute* 42 0 ary :key #'1+)
                 (let ((init (substitute 42 0 lst :key #'1+)))
                   (make-array (list d0 d1)
                               :initial-contents (loop :repeat d0 :collect init))))
      
      (is-equalp (substitute* 42 0 ary :from-end t)
                 (let ((init (substitute 42 0 lst :from-end t)))
                   (make-array (list d0 d1)
                               :initial-contents (loop :repeat d0 :collect init))))
      
      (let ((r (random (max 1 (length lst)))))
        
        (is-equalp (substitute* 42 0 ary :count r)
                   (let ((init (substitute 42 0 lst :count r)))
                     (make-array (list d0 d1)
                                 :initial-contents (loop :repeat d0 :collect init))))

        (is-equalp (substitute* 42 0 ary :count r :from-end t)
                   (let ((init (substitute 42 0 lst :count r :from-end t)))
                     (make-array (list d0 d1)
                                 :initial-contents (loop :repeat d0 :collect init))))
        
        (is-equalp (substitute* 42 0 ary :start r)
                   (let ((init (substitute 42 0 lst :start r)))
                     (make-array (list d0 d1)
                                 :initial-contents (loop :repeat d0 :collect init))))
        
        (is-equalp (substitute* 42 0 ary :end r)
                   (let ((init (substitute 42 0 lst :end r)))
                     (make-array (list d0 d1)
                                 :initial-contents (loop :repeat d0 :collect init))))
                
        (let ((r1 (random (max 1 r))))
          (is-equalp (substitute* 42 0 ary :start r1 :end r)
                     (let ((init (substitute 42 0 lst :start r1 :end r)))
                       (make-array (list d0 d1)
                                   :initial-contents (loop :repeat d0 :collect init)))))))))


(test ?substitute*.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (alst (loop :for i :from 0
                      :for v :in vals
                      :collect (cons i v))))
      
      (let ((result* (substitute* 42 0 ht))
            (result  (loop :for (k . v) :in alst
                           :collect (cons k (if (= v 0) 42 v)))))
        (is (= (hash-table-count result*)
               (length result)))
        (is-true (loop :for (i . v) :in result
                       :always (= v (gethash i result*)))))

      (let ((result* (substitute* 42 0 ht :key #'1+))
            (result  (loop :for (k . v) :in alst
                           :collect (cons k (if (= 0 (1+ v)) 42 v)))))
        (is (= (hash-table-count result*)
               (length result)))
        (is-true (loop :for (i . v) :in result
                       :always (= v (gethash i result*))))))))


;;--------------------------------------------------------------------
;; substitute-if*
;;--------------------------------------------------------------------

(test ?substitute-if*.error
  (signals type-error (substitute-if* 42 #'oddp #'oddp))
  (signals type-error (substitute-if* 42 #'oddp '(-1 0 1) :key   #()))
  (signals type-error (substitute-if* 42 #'oddp '(-1 0 1) :count :foo))
  
  (signals type-error (substitute-if* 42 #'oddp '(-1 0 1) :start -1))
  (signals type-error (substitute-if* 42 #'oddp '(-1 0 1) :start nil))
  (signals type-error (substitute-if* 42 #'oddp '(-1 0 1) :start :foo))
  (signals type-error (substitute-if* 42 #'oddp '(-1 0 1) :start '(0 1)))
  (is-true (substitute-if* 42 #'oddp #2A((-1 0 1) (-1 0 1)) ; check no-error
                           :start '(0 1)))

  (signals type-error (substitute-if* 42 #'oddp '(-1 0 1) :end -1))
  (signals type-error (substitute-if* 42 #'oddp '(-1 0 1) :end '(0 1)))
  (signals type-error (substitute-if* 42 #'oddp '(-1 0 1) :end :foo))
  (is-true (substitute-if* 42 #'oddp #2A((-1 0 1) (-1 0 1)) :end '(0 1))) ; check no-error

  (signals simple-error (substitute-if* 42 #'oddp '(0 1 2 3) :start 42 :end 24))
  (signals simple-error (substitute-if* 42 #'oddp #2A((-1 0 1) (-1 0 1))
                                        :start '(0 2) :end '(0 1)))
  (is-true (substitute-if* 42 #'oddp #2A((-1 0 1) (-1 0 1)) ; check no-error
                           :start '(0 1) :end '(0 2))))


(test ?substitute-if*.list
  (for-all ((lst (gen-list)))
    (is-equal (substitute-if* 42 #'oddp lst)
              (substitute-if  42 #'oddp lst))
    (is-equal (substitute-if* 42 #'oddp lst :key #'1+)
              (substitute-if  42 #'oddp lst :key #'1+))
    (is-equal (substitute-if* 42 #'oddp lst :from-end t)
              (substitute-if  42 #'oddp lst :from-end t))
    
    (let ((r (random (max 1 (length lst)))))
      (is-equal (substitute-if* 42 #'oddp lst :count r)
                (substitute-if  42 #'oddp lst :count r))
      (is-equal (substitute-if* 42 #'oddp lst :count r :from-end t)
                (substitute-if  42 #'oddp lst :count r :from-end t))
      (is-equal (substitute-if* 42 #'oddp lst :start r)
                (substitute-if  42 #'oddp lst :start r))
      (is-equal (substitute-if* 42 #'oddp lst :end   r)
                (substitute-if  42 #'oddp lst :end   r))

      (let ((r1 (random (max 1 r))))
        (is-equal (substitute-if* 42 #'oddp lst :start r1 :end r)
                  (substitute-if  42 #'oddp lst :start r1 :end r))))))


(test ?substitute-if*.vector
  (for-all ((lst (gen-list)))
    (let ((vec (coerce lst 'vector)))
      (is-equalp (substitute-if* 42 #'oddp vec)
                 (substitute-if  42 #'oddp vec))
      (is-equalp (substitute-if* 42 #'oddp vec :key #'1+)
                 (substitute-if  42 #'oddp vec :key #'1+))
      (is-equalp (substitute-if* 42 #'oddp vec :from-end t)
                 (substitute-if  42 #'oddp vec :from-end t))
    
      (let ((r (random (max 1 (length vec)))))
        (is-equalp (substitute-if* 42 #'oddp vec :count r)
                   (substitute-if  42 #'oddp vec :count r))
        (is-equalp (substitute-if* 42 #'oddp vec :count r :from-end t)
                   (substitute-if  42 #'oddp vec :count r :from-end t))
        (is-equalp (substitute-if* 42 #'oddp vec :start r)
                   (substitute-if  42 #'oddp vec :start r))
        (is-equalp (substitute-if* 42 #'oddp vec :end   r)
                   (substitute-if  42 #'oddp vec :end   r))

        (let ((r1 (random (max 1 r))))
          (is-equalp (substitute-if* 42 #'oddp vec :start r1 :end r)
                     (substitute-if  42 #'oddp vec :start r1 :end r)))))))


(test ?substitute-if*.lazy-sequence
  (for-all ((lst (gen-list)))
    (let ((lseq (lfor x :in lst)))

      (is-equal (lazy-take :all (substitute-if* 42 #'oddp lseq))
                (substitute-if 42 #'oddp lst))
      (is-equal (lazy-take :all (substitute-if* 42 #'oddp lseq :key #'1+))
                (substitute-if 42 #'oddp lst  :key #'1+))
      (is-equal (lazy-take :all (substitute-if* 42 #'oddp lseq :from-end t))
                (substitute-if 42 #'oddp lst  :from-end t))
      
      (let ((r (random (max 1 (length lst)))))
        (is-equal (lazy-take :all (substitute-if* 42 #'oddp lseq :count r))
                  (substitute-if  42 #'oddp lst  :count r))
        
        (is-equal (lazy-take :all (substitute-if* 42 #'oddp lseq :start r))
                  (substitute-if  42 #'oddp lst :start r))
        
        (is-equal (lazy-take :all (substitute-if* 42 #'oddp lseq :end r))
                  (substitute-if  42 #'oddp lst :end r))
        
        (let ((r1 (random (max 1 r))))
          (is-equal (lazy-take :all (substitute-if* 42 #'oddp lseq :start r1 :end r))
                    (substitute-if  42 #'oddp lst :start r1 :end r)))))))


(test ?substitute-if*.array
  (for-all ((lst (gen-list :length (gen-integer :min -1 :max 1))))
    (let* ((d0  3)
           (d1  (length lst))
           (ary (make-array (list d0 d1)
                            :initial-contents (loop :repeat d0 :collect lst))))
      
      (is-equalp (substitute-if* 42 #'oddp ary)
                 (let ((init (substitute-if 42 #'oddp lst)))
                   (make-array (list d0 d1)
                               :initial-contents (loop :repeat d0 :collect init))))
      
      (is-equalp (substitute-if* 42 #'oddp ary :key #'1+)
                 (let ((init (substitute-if 42 #'oddp lst :key #'1+)))
                   (make-array (list d0 d1)
                               :initial-contents (loop :repeat d0 :collect init))))
      
      (is-equalp (substitute-if* 42 #'oddp ary :from-end t)
                 (let ((init (substitute-if 42 #'oddp lst :from-end t)))
                   (make-array (list d0 d1)
                               :initial-contents (loop :repeat d0 :collect init))))
      
      (let ((r (random (max 1 (length lst)))))
        
        (is-equalp (substitute-if* 42 #'oddp ary :count r)
                   (let ((init (substitute-if 42 #'oddp lst :count r)))
                     (make-array (list d0 d1)
                                 :initial-contents (loop :repeat d0 :collect init))))

        (is-equalp (substitute-if* 42 #'oddp ary :count r :from-end t)
                   (let ((init (substitute-if 42 #'oddp lst :count r :from-end t)))
                     (make-array (list d0 d1)
                                 :initial-contents (loop :repeat d0 :collect init))))
        
        (is-equalp (substitute-if* 42 #'oddp ary :start r)
                   (let ((init (substitute-if 42 #'oddp lst :start r)))
                     (make-array (list d0 d1)
                                 :initial-contents (loop :repeat d0 :collect init))))
        
        (is-equalp (substitute-if* 42 #'oddp ary :end r)
                   (let ((init (substitute-if 42 #'oddp lst :end r)))
                     (make-array (list d0 d1)
                                 :initial-contents (loop :repeat d0 :collect init))))
                
        (let ((r1 (random (max 1 r))))
          (is-equalp (substitute-if* 42 #'oddp ary :start r1 :end r)
                     (let ((init (substitute-if 42 #'oddp lst :start r1 :end r)))
                       (make-array (list d0 d1)
                                   :initial-contents (loop :repeat d0 :collect init)))))))))


(test ?substitute-if*.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (alst (loop :for i :from 0
                      :for v :in vals
                      :collect (cons i v))))
      
      (let ((result* (substitute-if* 42 #'oddp ht))
            (result  (loop :for (k . v) :in alst
                           :collect (cons k (if (oddp v) 42 v)))))
        (is (= (hash-table-count result*)
               (length result)))
        (is-true (loop :for (i . v) :in result
                       :always (= v (gethash i result*)))))

      (let ((result* (substitute-if* 42 #'oddp ht :key #'1+))
            (result  (loop :for (k . v) :in alst
                           :collect (cons k (if (oddp (1+ v)) 42 v)))))
        (is (= (hash-table-count result*)
               (length result)))
        (is-true (loop :for (i . v) :in result
                       :always (= v (gethash i result*))))))))


;;--------------------------------------------------------------------
;; substitute-if-not*
;;--------------------------------------------------------------------

(test ?substitute-if-not*.error
  (signals type-error (substitute-if-not* 42 #'oddp #'oddp))
  (signals type-error (substitute-if-not* 42 #'oddp '(-1 0 1) :key   #()))
  (signals type-error (substitute-if-not* 42 #'oddp '(-1 0 1) :count :foo))
  
  (signals type-error (substitute-if-not* 42 #'oddp '(-1 0 1) :start -1))
  (signals type-error (substitute-if-not* 42 #'oddp '(-1 0 1) :start nil))
  (signals type-error (substitute-if-not* 42 #'oddp '(-1 0 1) :start :foo))
  (signals type-error (substitute-if-not* 42 #'oddp '(-1 0 1) :start '(0 1)))
  (is-true (substitute-if-not* 42 #'oddp #2A((-1 0 1) (-1 0 1)) :start '(0 1))) ; check no-error

  (signals type-error (substitute-if-not* 42 #'oddp '(-1 0 1) :end -1))
  (signals type-error (substitute-if-not* 42 #'oddp '(-1 0 1) :end '(0 1)))
  (signals type-error (substitute-if-not* 42 #'oddp '(-1 0 1) :end :foo))
  (is-true (substitute-if-not* 42 #'oddp #2A((-1 0 1) (-1 0 1)) :end '(0 1))) ; check no-error

  (signals simple-error (substitute-if-not* 42 #'oddp '(0 1 2 3) :start 42 :end 24))
  (signals simple-error (substitute-if-not* 42 #'oddp #2A((-1 0 1) (-1 0 1))
                                            :start '(0 2) :end '(0 1)))
  (is-true (substitute-if-not* 42 #'oddp #2A((-1 0 1) (-1 0 1)) ; check no-error
                               :start '(0 1) :end '(0 2))))


(test ?substitute-if-not*.list
  (for-all ((lst (gen-list)))
    (is-equal (substitute-if-not* 42 #'oddp lst)
              (substitute-if-not  42 #'oddp lst))
    (is-equal (substitute-if-not* 42 #'oddp lst :key #'1+)
              (substitute-if-not  42 #'oddp lst :key #'1+))
    (is-equal (substitute-if-not* 42 #'oddp lst :from-end t)
              (substitute-if-not  42 #'oddp lst :from-end t))
    
    (let ((r (random (max 1 (length lst)))))
      (is-equal (substitute-if-not* 42 #'oddp lst :count r)
                (substitute-if-not  42 #'oddp lst :count r))
      (is-equal (substitute-if-not* 42 #'oddp lst :count r :from-end t)
                (substitute-if-not  42 #'oddp lst :count r :from-end t))
      (is-equal (substitute-if-not* 42 #'oddp lst :start r)
                (substitute-if-not  42 #'oddp lst :start r))
      (is-equal (substitute-if-not* 42 #'oddp lst :end   r)
                (substitute-if-not  42 #'oddp lst :end   r))

      (let ((r1 (random (max 1 r))))
        (is-equal (substitute-if-not* 42 #'oddp lst :start r1 :end r)
                  (substitute-if-not  42 #'oddp lst :start r1 :end r))))))


(test ?substitute-if-not*.vector
  (for-all ((lst (gen-list)))
    (let ((vec (coerce lst 'vector)))
      (is-equalp (substitute-if-not* 42 #'oddp vec)
                 (substitute-if-not  42 #'oddp vec))
      (is-equalp (substitute-if-not* 42 #'oddp vec :key #'1+)
                 (substitute-if-not  42 #'oddp vec :key #'1+))
      (is-equalp (substitute-if-not* 42 #'oddp vec :from-end t)
                 (substitute-if-not  42 #'oddp vec :from-end t))
    
      (let ((r (random (max 1 (length vec)))))
        (is-equalp (substitute-if-not* 42 #'oddp vec :count r)
                   (substitute-if-not  42 #'oddp vec :count r))
        (is-equalp (substitute-if-not* 42 #'oddp vec :count r :from-end t)
                   (substitute-if-not  42 #'oddp vec :count r :from-end t))
        (is-equalp (substitute-if-not* 42 #'oddp vec :start r)
                   (substitute-if-not  42 #'oddp vec :start r))
        (is-equalp (substitute-if-not* 42 #'oddp vec :end   r)
                   (substitute-if-not  42 #'oddp vec :end   r))

        (let ((r1 (random (max 1 r))))
          (is-equalp (substitute-if-not* 42 #'oddp vec :start r1 :end r)
                     (substitute-if-not  42 #'oddp vec :start r1 :end r)))))))


(test ?substitute-if-not*.lazy-sequence
  (for-all ((lst (gen-list)))
    (let ((lseq (lfor x :in lst)))

      (is-equal (lazy-take :all (substitute-if-not* 42 #'oddp lseq))
                (substitute-if-not 42 #'oddp lst))
      (is-equal (lazy-take :all (substitute-if-not* 42 #'oddp lseq :key #'1+))
                (substitute-if-not 42 #'oddp lst  :key #'1+))
      (is-equal (lazy-take :all (substitute-if-not* 42 #'oddp lseq :from-end t))
                (substitute-if-not 42 #'oddp lst  :from-end t))
      
      (let ((r (random (max 1 (length lst)))))
        (is-equal (lazy-take :all (substitute-if-not* 42 #'oddp lseq :count r))
                  (substitute-if-not  42 #'oddp lst  :count r))
        
        (is-equal (lazy-take :all (substitute-if-not* 42 #'oddp lseq :start r))
                  (substitute-if-not  42 #'oddp lst :start r))
        
        (is-equal (lazy-take :all (substitute-if-not* 42 #'oddp lseq :end r))
                  (substitute-if-not  42 #'oddp lst :end r))
        
        (let ((r1 (random (max 1 r))))
          (is-equal (lazy-take :all (substitute-if-not* 42 #'oddp lseq :start r1 :end r))
                    (substitute-if-not  42 #'oddp lst :start r1 :end r)))))))


(test ?substitute-if-not*.array
  (for-all ((lst (gen-list :length (gen-integer :min -1 :max 1))))
    (let* ((d0  3)
           (d1  (length lst))
           (ary (make-array (list d0 d1)
                            :initial-contents (loop :repeat d0 :collect lst))))
      
      (is-equalp (substitute-if-not* 42 #'oddp ary)
                 (let ((init (substitute-if-not 42 #'oddp lst)))
                   (make-array (list d0 d1)
                               :initial-contents (loop :repeat d0 :collect init))))
      
      (is-equalp (substitute-if-not* 42 #'oddp ary :key #'1+)
                 (let ((init (substitute-if-not 42 #'oddp lst :key #'1+)))
                   (make-array (list d0 d1)
                               :initial-contents (loop :repeat d0 :collect init))))
      
      (is-equalp (substitute-if-not* 42 #'oddp ary :from-end t)
                 (let ((init (substitute-if-not 42 #'oddp lst :from-end t)))
                   (make-array (list d0 d1)
                               :initial-contents (loop :repeat d0 :collect init))))
      
      (let ((r (random (max 1 (length lst)))))
        
        (is-equalp (substitute-if-not* 42 #'oddp ary :count r)
                   (let ((init (substitute-if-not 42 #'oddp lst :count r)))
                     (make-array (list d0 d1)
                                 :initial-contents (loop :repeat d0 :collect init))))

        (is-equalp (substitute-if-not* 42 #'oddp ary :count r :from-end t)
                   (let ((init (substitute-if-not 42 #'oddp lst :count r :from-end t)))
                     (make-array (list d0 d1)
                                 :initial-contents (loop :repeat d0 :collect init))))
        
        (is-equalp (substitute-if-not* 42 #'oddp ary :start r)
                   (let ((init (substitute-if-not 42 #'oddp lst :start r)))
                     (make-array (list d0 d1)
                                 :initial-contents (loop :repeat d0 :collect init))))
        
        (is-equalp (substitute-if-not* 42 #'oddp ary :end r)
                   (let ((init (substitute-if-not 42 #'oddp lst :end r)))
                     (make-array (list d0 d1)
                                 :initial-contents (loop :repeat d0 :collect init))))
                
        (let ((r1 (random (max 1 r))))
          (is-equalp (substitute-if-not* 42 #'oddp ary :start r1 :end r)
                     (let ((init (substitute-if-not 42 #'oddp lst :start r1 :end r)))
                       (make-array (list d0 d1)
                                   :initial-contents (loop :repeat d0 :collect init)))))))))


(test ?substitute-if-not*.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min -1 :max 1))))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (alst (loop :for i :from 0
                      :for v :in vals
                      :collect (cons i v))))
      
      (let ((result* (substitute-if-not* 42 #'oddp ht))
            (result  (loop :for (k . v) :in alst
                           :collect (cons k (if (not (oddp v)) 42 v)))))
        (is (= (hash-table-count result*)
               (length result)))
        (is-true (loop :for (i . v) :in result
                       :always (= v (gethash i result*)))))

      (let ((result* (substitute-if-not* 42 #'oddp ht :key #'1+))
            (result  (loop :for (k . v) :in alst
                           :collect (cons k (if (not (oddp (1+ v))) 42 v)))))
        (is (= (hash-table-count result*)
               (length result)))
        (is-true (loop :for (i . v) :in result
                       :always (= v (gethash i result*))))))))


;;--------------------------------------------------------------------
;; fill*
;;--------------------------------------------------------------------

(test ?fill*.error
  (signals type-error (fill* '(0 1 2) 42 :start -1))
  (signals type-error (fill* '(0 1 2) 42 :start '(0 1)))
  (is-true (fill* #2A((-1 0 1) (-1 0 1)) 42 :start '(0 1))) ; check no-error

  (signals type-error (fill* '(0 1 2) 42 :end -1))
  (signals type-error (fill* '(0 1 2) 42 :end '(0 1)))
  (is-true (fill* #2A((-1 0 1) (-1 0 1)) 42 :end '(0 1))) ; check no-error
  
  (signals simple-error (fill* '(0 1 2) 42 :start 42 :end 24))
  (is-true (fill* #2A((-1 0 1) (-1 0 1)) 42 :start '(0 1) :end '(1 1)))) ; check no-error


(test ?fill*.list
  (for-all ((lst (gen-list :length (gen-integer :min 2 :max 10))))
    (is-equal (fill* (copy-list lst) :foo)
              (fill  (copy-list lst) :foo))

    (is-equal (fill* (copy-list lst) :foo :start 2)
              (fill  (copy-list lst) :foo :start 2))

    (is-equal (fill* (copy-list lst) :foo :end 2)
              (fill  (copy-list lst) :foo :end 2))

    (is-equal (fill* (copy-list lst) :foo :start 2 :end 2)
              (fill  (copy-list lst) :foo :start 2 :end 2))

    (signals simple-error (fill* (copy-list lst) :foo :start 3 :end 2))))


(test ?fill*.vector
  (for-all ((lst (gen-list :length (gen-integer :min 2 :max 10))))
    (is-equalp (fill* (coerce lst 'vector) :foo)
               (fill  (coerce lst 'vector) :foo))

    (is-equalp (fill* (coerce lst 'vector) :foo :start 2)
               (fill  (coerce lst 'vector) :foo :start 2))

    (is-equalp (fill* (coerce lst 'vector) :foo :end 2)
               (fill  (coerce lst 'vector) :foo :end 2))

    (is-equalp (fill* (coerce lst 'vector) :foo :start 2 :end 2)
               (fill  (coerce lst 'vector) :foo :start 2 :end 2))
    
    (signals simple-error (fill* (coerce lst 'vector) :foo :start 3 :end 2))))


;; TODO:
(test ?fill*.lazy-sequence
  )


(test ?fill*.array
  (for-all ((lst (gen-list :length (gen-integer :min 2 :max 10))))
    (let* ((dim (length lst))
           (ary (make-array (list 2 dim) :initial-contents (list lst lst))))
      
      (is-equalp (fill* (copy-array ary) :foo)
                 (make-array (list 2 dim) :initial-element :foo))

      (is-equalp (fill* (copy-array ary) :foo :start 2)
                 (loop :with ary0 := (copy-array ary)
                       :for i :from 2 :below (array-total-size ary)
                       :do (setf (row-major-aref ary0 i) :foo)
                       :finally (return ary0)))

      (is-equalp (fill* (copy-array ary) :foo :end 2)
                 (loop :with ary0 := (copy-array ary)
                       :for i :from 0 :below 2
                       :do (setf (row-major-aref ary0 i) :foo)
                       :finally (return ary0)))

      (is-equalp (fill* (copy-array ary) :foo :start 2 :end 2)
                 (copy-array ary))
      
      (signals simple-error (fill* (copy-array ary) :foo :start 3 :end 2)))))


(test ?fill*.hash-table
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :in keys
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (foo-ht (loop :with h := (make-hash-table)
                        :for k :in keys
                        :for nil :in vals
                        :do (setf (gethash k h) :foo)
                        :finally (return h))))
      
      (is-equalp (fill* (copy-hash-table ht) :foo)
                 foo-ht)

      (is-equalp (fill* (copy-hash-table ht) :foo :start 2)
                 foo-ht)

      (is-equalp (fill* (copy-hash-table ht) :foo :end 2)
                 foo-ht)

      (is-equalp (fill* (copy-hash-table ht) :foo :start 2 :end 2)
                 foo-ht)
      
      (is-equalp (fill* (copy-hash-table ht) :foo :start 3 :end 2)
                 foo-ht))))


;;====================================================================


