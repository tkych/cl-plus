;;;; cl-plus/test/core/hash-table.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for Hash-Table 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.hash-table
  (:export #:?hash-table)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus.src.core.hash-table
                #:hash-table=
                #:dohash)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:cl-plus.src.readtables
                #:cl+sharp-brace)
  (:import-from #:alexandria
                #:copy-hash-table)
  (:import-from #:cl-plus-test
                #:all))

(in-package #:cl-plus.test.core.hash-table)

(def-suite ?hash-table :in all)
(in-suite ?hash-table)
(in-readtable cl+sharp-brace)


;;--------------------------------------------------------------------
;; hash-table=
;;--------------------------------------------------------------------

(test ?hash-table=
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let ((ht (loop :with h := (make-hash-table :test 'eql)
                    :for k :in keys
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (is-true (hash-table= ht
                            (loop :with h := (make-hash-table :test 'eql)
                                  :for k :in keys
                                  :for v :in vals
                                  :do (setf (gethash k h) v)
                                  :finally (return h))))
      (is-true (hash-table= ht
                             (loop :with h := (make-hash-table :test 'equal)
                                   :for k :in keys
                                   :for v :in vals
                                   :do (setf (gethash k h) v)
                                   :finally (return h))))
      (when (and keys vals)
        (is-false (hash-table= ht
                               (make-hash-table)))
        (is-false (hash-table= ht
                               (loop :with h := (make-hash-table)
                                     :for k :in keys
                                     :for v :in vals
                                     :do (setf (gethash k h) v)
                                     :finally (progn
                                                (remhash (first keys) h)
                                                (return h)))))))))


;;--------------------------------------------------------------------
;; dohash
;;--------------------------------------------------------------------

(test ?dohash
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let ((ht (loop :with h := (make-hash-table)
                    :for k :in keys
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return ht))))
      (is-true (hash-table= (let ((ht2 (make-hash-table)))
                              (dohash (k v ht)
                                (setf (gethash k ht2) v))
                              ht2)
                            ht)))))


;; check loop-unrolling

;; (enable-loop-unrolling 10)
;; (disassemble
;;  (compile nil '(lambda ()
;;                 (dohash (k v #{:foo 0 :bar 1 :baz 2})
;;                   (declare (ignore k v))))))

;; (disable-loop-unrolling)
;; (disassemble
;;  (compile nil '(lambda ()
;;                 (dohash (k v #{:foo 0 :bar 1 :baz 2})
;;                   (declare (ignore k v))))))


;;--------------------------------------------------------------------
;; brace-reader
;;--------------------------------------------------------------------

(defparameter ht
  (let ((table (make-hash-table :test #'equal)))
    (loop :for (k v) :in '((:foo 0) (:bar 1) (:baz 2))
          :do (setf (gethash k table) v))
    table))

(defparameter ht2 (copy-hash-table ht :test 'eq))
(defparameter ht3 (copy-hash-table ht :test 'eql))
(defparameter ht5 (copy-hash-table ht :test 'equal))
(defparameter ht6 (copy-hash-table ht :test 'equalp))

(test ?brace-reader.error
  (signals simple-error #10{:foo 0 :bar}) ; odd
  (signals simple-error #10{:foo 0 :bar 1 :foo 2})) ; duplicate

(test ?brace-reader.1 "make empty hash-table"
  (is (equalp  #{} (make-hash-table :test 'equal)))
  (is (equalp #2{} (make-hash-table :test 'eq)))
  (is (equalp #3{} (make-hash-table :test 'eql)))
  (is (equalp #4{} (make-hash-table :test 'equal)))
  (is (equalp #5{} (make-hash-table :test 'equal)))
  (is (equalp #6{} (make-hash-table :test 'equalp)))
  (is (equalp #7{} (make-hash-table :test 'equal)))
  (is (equalp #8{} (make-hash-table :test 'equal)))
  (is (equalp #9{} (make-hash-table :test 'equal))))

(test ?brace-reader.type
  (is (equalp ht #{:foo 0 :bar 1 :baz 2}))
  (is (equalp ht2 #2{:foo 0 :bar 1 :baz 2}))
  (is (equalp ht3 #3{:foo 0 :bar 1 :baz 2}))
  (is (equalp ht5 #5{:foo 0 :bar 1 :baz 2}))
  (is (equalp ht6 #6{:foo 0 :bar 1 :baz 2})))

(test ?brace-reader.reader-parameter
  (let ((zero 0) (one 1) (two 2))
    (is (equalp ht  #10{:foo zero :bar one :baz two}))
    (is (equalp ht2 #12{:foo zero :bar one :baz two}))
    (is (equalp ht3 #13{:foo zero :bar one :baz two}))
    (is (equalp ht5 #15{:foo zero :bar one :baz two}))
    (is (equalp ht6 #16{:foo zero :bar one :baz two}))))

;; TODO:
;; (test ?brace-reader.4 "nested hash-table"
;;   (is (equalp ht #{:foo 0 :bar 1 :baz 2}))
;;   (is (equalp ht2 #2{:foo 0 :bar 1 :baz 2}))
;;   (is (equalp ht3 #3{:foo 0 :bar 1 :baz 2}))
;;   (is (equalp ht5 #5{:foo 0 :bar 1 :baz 2}))
;;   (is (equalp ht6 #6{:foo 0 :bar 1 :baz 2})))


;;====================================================================
