;;;; cl-plus/test/core/alist.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for Alist 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.alist
  (:export #:?alist)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:?all
                #:is-=
                #:is-eql
                #:is-equal
                #:is-equalp)
  (:import-from #:cl-plus.src.core.alist
                #:alist
                #:alistp
                #:alist=
                #:normalize-alist
                #:doalist))

(in-package #:cl-plus.test.core.alist)

(def-suite ?alist :in ?all)
(in-suite ?alist)


;;--------------------------------------------------------------------
;; alist
;;--------------------------------------------------------------------

(test ?alist.type
  (is-true  (typep '() 'alist))
  (is-true  (typep '((:foo . 0) (:bar . 1) (:baz . 2)) 'alist))
  (is-false (typep '(:foo 0 :bar 1 :baz 2) 'alist))
  (is-false (typep '(:foo (0) :bar (1) :baz (2)) 'alist))
  (is-false (typep (make-hash-table) 'alist)))


(test ?alistp
  (is-true  (alistp '()))
  (is-true  (alistp '((:foo . 0) (:bar . 1) (:baz . 2))))
  (is-false (alistp '(:foo 0 :bar 1 :baz 2)))
  (is-false (alistp '(:foo (0) :bar (1) :baz (2))))
  (is-false (alistp (make-hash-table))))


(test ?alist=
  (signals type-error (alist= '(:foo 0 :bar 1 :baz 2)
                              '(:foo 0 :bar 1 :baz 2)))
  (signals type-error (alist= '(:foo 0 :bar 1 :baz 2)
                              '((:foo . 0) (:bar . 1) (:baz . 2))))
  (signals type-error (alist= '((:foo . 0) (:bar . 1) (:baz . 2))
                              '(:foo 0 :bar 1 :baz 2)))
  (is-true (alist= '((:foo . 0) (:bar . 1) (:baz . 2))
                   '((:foo . 0) (:bar . 1) (:baz . 2))))
  (is-true (alist= '((:foo . 0) (:bar . 1) (:baz . 2))
                   '((:bar . 1) (:baz . 2) (:foo . 0))))
  (is-true (alist= '((:foo . 0) (:bar . 1))
                   '((:foo . 0) (:foo . 42) (:bar . 1))))
  (is-true (alist= '((:foo . 0) (:bar . 1) (:baz . 2))
                   '((:foo . 0) (:bar . 1) (:foo . 42) (:baz . 2) (:baz . 200))))
  (is-false (alist= '((:foo . 9) (:bar . 1) (:baz . 2))
                    '((:bar . 1) (:baz . 2) (:foo . 0)))))


;;--------------------------------------------------------------------
;; normalize-alist
;;--------------------------------------------------------------------

(test ?normalize-alist
  (signals type-error (normalize-alist '(:foo 0 :bar 1 :baz 2)))

  (is (equal (sort (normalize-alist '((:foo . 0) (:bar . 1) (:baz . 2)))
                   #'< :key #'cdr)
             (sort (copy-seq '((:foo . 0) (:bar . 1) (:baz . 2)))
                   #'< :key #'cdr)))

  (is (equal (sort (normalize-alist '((:foo . 0) (:bar . 1) (:baz . 2)
                                      (:foo . 10) (:bar . 11) (:baz . 12)))
                   #'< :key #'cdr)
             (sort (copy-seq '((:foo . 0) (:bar . 1) (:baz . 2)))
                   #'< :key #'cdr))))


;;--------------------------------------------------------------------
;; doalist
;;--------------------------------------------------------------------

(test ?doalist
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let ((alst (loop :for k :in keys
                      :for v :in vals
                      :collect (cons k v))))
      (is-equal (let ((result '()))
                  (doalist (k v alst)
                    (push (cons k v) result))
                  (nreverse result))
                (normalize-alist alst)))))


;;====================================================================
