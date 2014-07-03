;;;; cl-plus/test/core/plist.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for Plist 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.plist
  (:export #:?plist)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:all
                #:is-=
                #:is-eql
                #:is-equal
                #:is-equalp)
  (:import-from #:alexandria
                #:mappend)
  (:import-from #:cl-plus.src.core.plist
                #:plist
                #:plistp
                #:plist+
                #:plistp+
                #:plist=
                #:normalize-plist
                #:doplist))

(in-package #:cl-plus.test.core.plist)

(def-suite ?plist :in all)
(in-suite ?plist)


;;--------------------------------------------------------------------
;; plist
;;--------------------------------------------------------------------

(test ?plist.type
  (is-true  (typep '() 'plist))
  (is-true  (typep '(:foo 0 :bar 1 :baz 2) 'plist))
  (is-true  (typep '(foo 0 bar 1 baz 2) 'plist))
  (is-false (typep (make-hash-table) 'plist))
  (is-false (typep '((:foo . 0) (:bar . 1) (:baz . 2)) 'plist))
  (is-false (typep '("foo" 0 "bar" 1 "baz" 2) 'plist)))

(test ?plistp
  (is-true  (plistp '()))
  (is-true  (plistp '(:foo 0 :bar 1 :baz 2)))
  (is-true  (plistp '(foo 0 bar 1 baz 2)))
  (is-false (plistp (make-hash-table)))
  (is-false (plistp '("foo" 0 "bar" 1 "baz" 2))))


;;--------------------------------------------------------------------
;; plist+
;;--------------------------------------------------------------------

(test ?plist+.type
  (is-true  (typep '() 'plist+))
  (is-true  (typep '(:foo 0 :bar 1 :baz 2) 'plist+))
  (is-true  (typep '(foo 0 bar 1 baz 2) 'plist+))
  (is-true  (typep '("foo" 0 "bar" 1 "baz" 2) 'plist+))
  (is-false (typep (make-hash-table) 'plist+))
  (is-false (typep '((:foo . 0) (:bar . 1) (:baz . 2)) 'plist+)))


(test ?plistp+
  (is-true (plistp+ '()))
  (is-true (plistp+ '(:foo 0 :bar 1 :baz 2)))
  (is-true (plistp+ '(foo 0 bar 1 baz 2)))
  (is-true (plistp+ '("foo" 0 "bar" 1 "baz" 2)))
  (is-false (plistp+ (make-hash-table))))


;;--------------------------------------------------------------------
;; plist=
;;--------------------------------------------------------------------

(test ?plist=.error
  (signals type-error (plist= '() '() :key-test #()))
  (signals type-error (plist= '() '() :val-test #()))
  (signals type-error (plist= '((:foo . 0) (:bar . 1) (:baz . 2))
                              '((:foo . 0) (:bar . 1) (:baz . 2))))
  (signals type-error (plist= '((:foo . 0) (:bar . 1) (:baz . 2))
                              '(:foo 0 :bar 1 :baz 2)))
  (signals type-error (plist= '(:foo 0 :bar 1 :baz 2)
                              '((:foo . 0) (:bar . 1) (:baz . 2))))
  (signals type-error (plist= '(:foo 0 :bar 1 :baz 2)
                              (make-hash-table))))


(test ?plist=
  (is-true  (plist= '()
                    '()))
  (is-true  (plist= '(:foo 0 :bar 1 :baz 2)
                    '(:foo 0 :bar 1 :baz 2)))
  (is-true  (plist= '(:foo 0 :bar 1 :baz 2)
                    '(:foo 0 :bar 1 :baz 2)
                    :key-test #'eq))
  (is-true  (plist= '(:foo 0 :bar 1 :baz 2)
                    '(:foo 0 :bar 1 :baz 2)
                    :key-test #'eql))
  (is-true  (plist= '(:foo 0 :bar 1 :baz 2)
                    '(:foo 0 :bar 1 :baz 2)
                    :key-test #'equal))
  (is-true  (plist= '(:foo 0 :bar 1 :baz 2)
                    '(:foo 0 :bar 1 :baz 2)
                    :key-test #'equalp))
  
  (is-true  (plist= '(:foo 0 :bar 1 :baz 2)
                    '(:baz 2 :foo 0 :bar 1)))
  (is-true  (plist= '(:foo 0 :bar 1 :baz 2)
                    '(:foo 0 :bar 1 :baz 2 :foo 893 :bar 893 :baz 893)))

    (is-false (plist= '("foo" 0 "foo" 0)
                    '("foo" 0 "FOO" 1)
                    :key-test #'string=))
  (is-true  (plist= '("foo" 0 "foo" 0)
                    '("foo" 0 "FOO" 1)
                    :key-test #'string-equal))
  
  (is-false (plist= '(:foo 0 :bar 1 :baz 2)
                    '(:foo 0)))
  (is-false (plist= '(:foo 0 :bar 1 :baz 2)
                    '(:foo 893 :bar 893 :baz 893)))
  (is-true  (plist= '(:foo 0 :bar 1 :baz 2)
                    '(:foo 893 :bar 893 :baz 893)
                    :val-test (constantly T))))


;;--------------------------------------------------------------------
;; normalizers
;;--------------------------------------------------------------------

(test ?normalize-plist
  (signals type-error (normalize-plist '((:foo . 0) (:bar . 1) (:baz . 2))))
  (is (equal (normalize-plist '(:foo 0 :bar 1 :baz 2))
             '(:foo 0 :bar 1 :baz 2)))
  (is (equal (normalize-plist '(:foo 0 :bar 1 :baz 2
                                :foo 10 :bar 11 :baz 12))
             '(:foo 0 :bar 1 :baz 2))))


;;--------------------------------------------------------------------
;; doplist
;;--------------------------------------------------------------------

(test ?doplist
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let ((plst (mappend 'list keys vals)))
      (is-equal (let ((result '()))
                  (doplist (k v plst)
                    (push k result)
                    (push v result))
                  (nreverse result))
                plst))))


;;====================================================================
