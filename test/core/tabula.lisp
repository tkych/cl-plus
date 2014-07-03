;;;; cl-plus/test/core/tabula.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Tests for Tabula 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.tabula
  (:export #:?tabula)
  (:use #:cl #:fiveam)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:cl-plus.src.readtables
                #:cl+)
  (:import-from #:alexandria
                #:copy-hash-table
                #:set-equal)
  (:import-from #:cl-plus-test
                #:all
                #:is-=
                #:is-eql
                #:is-equal
                #:is-equalp)
  (:import-from #:cl-plus.src.core.alist
                #:alist=
                #:alist
                #:alistp
                #:normalize-alist)
  (:import-from #:cl-plus.src.core.plist
                #:plist=
                #:plist
                #:plist+
                #:plistp
                #:plistp+
                #:normalize-plist)
  (:import-from #:cl-plus.src.core.hash-table
                #:hash-table=)
  (:import-from #:cl-plus.src.core.sequens
                #:sequens)
  (:import-from #:cl-plus.src.core.tabula
                #:tabula  #:tabulap
                #:dotab
                #:tabula=
                #:size+
                #:clear-tabula
                #:erase-entry
                #:add+
                #:ref+
                #:keys        #:vals
                #:to-hash     #:to-alist       #:to-plist
                #:map+        #:reduce+
                #:every+      #:notevery+
                #:some+       #:notany+
                #:count+      #:count-if+      #:count-if-not+
                #:find+       #:find-if+       #:find-if-not+
                #:position+   #:position-if+   #:position-if-not+
                #:remove+     #:remove-if+     #:remove-if-not+
                #:substitute+ #:substitute-if+ #:substitute-if-not+))

(in-package #:cl-plus.test.core.tabula)

(def-suite ?tabula :in all)
(in-suite ?tabula)
(in-readtable cl+)


;;--------------------------------------------------------------------
;; type
;;--------------------------------------------------------------------

(test ?tabula.type
  (is-true (typep '() 'tabula))
  (is-true (typep '((:foo . 0) (:bar . 1) (:baz . 2)) 'tabula))
  (is-true (typep '(:foo 0 :bar 1 :baz 2) 'tabula))
  (is-true (typep '(:foo (0) :bar (1) :baz (2)) 'tabula))
  (is-true (typep #{} 'tabula))
  (is-true (typep #{:foo 0 :bar 1 :baz 2} 'tabula)))


(test ?tabulap
  (is-true (tabulap '()))
  (is-true (tabulap '((:foo . 0) (:bar . 1) (:baz . 2))))
  (is-true (tabulap '(:foo 0 :bar 1 :baz 2)))
  (is-true (tabulap '(:foo (0) :bar (1) :baz (2))))
  (is-true (tabulap #{}))
  (is-true (tabulap #{:foo 0 :bar 1 :baz 2})))


;;--------------------------------------------------------------------
;; tabula=
;;--------------------------------------------------------------------

(test ?tabula=.error
  (signals type-error (tabula= :foo '(:foo 0)))
  (signals type-error (tabula= '(:foo 0) :foo))
  (signals type-error (tabula= '(:foo 0) '(:foo 0) :key-test #()))
  (signals type-error (tabula= '(:foo 0) '(:foo 0) :val-test #())))


(test ?tabula=
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :in keys
                    :for v :in vals
                    :unless (gethash k h nil)
                      :do (setf (gethash k h) v)
                    :finally (return h)))
          (alst (loop :for k :in keys
                      :for v :in vals
                      :collect (cons k v)))
          (plst (loop :for k :in keys
                      :for v :in vals
                      :append (list k v)))
          (key-test #'equal))

      ;; (repl-utilities:dbgv () keys vals ht alst plst)
      
      (is-true (tabula= ht   (copy-hash-table ht) :key-test key-test))
      (is-true (tabula= alst (copy-alist alst)    :key-test key-test))
      (is-true (tabula= plst (copy-list plst)     :key-test key-test))
      (is-true (tabula= ht   alst :key-test key-test))
      (is-true (tabula= ht   plst :key-test key-test))
      (is-true (tabula= alst   ht :key-test key-test))
      (is-true (tabula= alst plst :key-test key-test))
      (is-true (tabula= plst   ht :key-test key-test))
      (is-true (tabula= plst alst :key-test key-test))

      (is-true  (tabula= ht   (copy-hash-table ht) :key-test #'=))
      (is-false (tabula= ht   alst :key-test #'=))
      (is-false (tabula= ht   plst :key-test #'=))
      (is-false (tabula= alst   ht :key-test #'=))
      (is-false (tabula= plst   ht :key-test #'=))

      (let ((ht2   (copy-hash-table ht))
            (alst2 (acons :foo 42 alst))
            (plst2 (cons :foo (cons 42 plst))))
        (setf (gethash :foo ht2) 42)

        ;; (repl-utilities:dbgv () keys vals ht ht2 alst2 alst plst plst2)
        
        (is-false (tabula= ht     ht2 :key-test key-test))
        (is-false (tabula= ht   alst2 :key-test key-test))
        (is-false (tabula= ht   plst2 :key-test key-test))
        (is-false (tabula= alst   ht2 :key-test key-test))
        (is-false (tabula= alst alst2 :key-test key-test))
        (is-false (tabula= alst plst2 :key-test key-test))
        (is-false (tabula= plst   ht2 :key-test key-test))
        (is-false (tabula= plst alst2 :key-test key-test))
        (is-false (tabula= plst plst2 :key-test key-test))

        (is-false (tabula= ht2     ht :key-test key-test))
        (is-false (tabula= alst2   ht :key-test key-test))
        (is-false (tabula= plst2   ht :key-test key-test))
        (is-false (tabula= ht2   alst :key-test key-test))
        (is-false (tabula= alst2 alst :key-test key-test))
        (is-false (tabula= plst2 alst :key-test key-test))
        (is-false (tabula= ht2   plst :key-test key-test))
        (is-false (tabula= alst2 plst :key-test key-test))
        (is-false (tabula= plst2 plst :key-test key-test))
        ))))


;;--------------------------------------------------------------------
;; size+
;;--------------------------------------------------------------------

(test ?size+.error
  (signals type-error (size+ :foo))
  (signals type-error (size+ '(:foo 0) :key-test #())))


(test ?size+
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :in keys
                    :for v :in vals
                    :unless (gethash k h nil)
                      :do (setf (gethash k h) v)
                    :finally (return h)))
          (alst (loop :for k :in keys
                      :for v :in vals
                      :collect (cons k v)))
          (plst (loop :for k :in keys
                      :for v :in vals
                      :append (list k v)))
          (key-test #'equal))
      ;; (repl-utilities:dbgv () keys vals ht alst plst)
      (is (= (size+ ht)
             (size+ alst)
             (size+ plst)))
      (is (= (size+ ht   :key-test key-test)
             (size+ alst :key-test key-test)
             (size+ plst :key-test key-test)))
      (is (= (size+ ht   :key-test #'=)
             (size+ alst :key-test #'=)
             (size+ plst :key-test #'=)))
      (is (= (size+ alst :ensure-unique-keys t)
             (size+ plst :ensure-unique-keys t))))))


;;--------------------------------------------------------------------
;; clear-tabula
;;--------------------------------------------------------------------
;; TODO:

(test ?clear-tabula.error
  (let ((foo :foo))
    (signals type-error (clear-tabula foo))))


(test ?clear-tabula
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :in keys
                    :for v :in vals
                    :unless (gethash k h nil)
                      :do (setf (gethash k h) v)
                    :finally (return h)))
          (alst (loop :for k :in keys
                      :for v :in vals
                      :collect (cons k v)))
          (plst (loop :for k :in keys
                      :for v :in vals
                      :append (list k v))))
      (is-equalp (clear-tabula ht)
                 (make-hash-table :test 'equal))
      (is-eql    (clear-tabula alst)
                 '())
      (is-eql    (clear-tabula plst)
                 '()))))


;;--------------------------------------------------------------------
;; erase-entry
;;--------------------------------------------------------------------
;; TODO:

(defparameter ht   #{:foo 0 :bar 1 :baz 2})
(defparameter alst '((:foo . 0) (:bar . 1) (:baz . 2)))
(defparameter plst '(:foo 0 :bar 1 :baz 2))

(defun initialize-variables ()
  (setf ht   (copy-hash-table #{:foo 0 :bar 1 :baz 2})
        alst (copy-alist '((:foo . 0) (:bar . 1) (:baz . 2)))
        plst (copy-list '(:foo 0 :bar 1 :baz 2)))
  (values))


(test ?erase-entry
  (initialize-variables)

  (is (equalp (erase-entry ht :xxx)
              #{:foo 0 :bar 1 :baz 2}))
  (is (equalp (erase-entry ht :bar)
              #{:foo 0 :baz 2}))
  (is (equalp (erase-entry ht :baz)
              #{:foo 0}))
  (is (equalp (erase-entry ht :foo)
              #{}))
  (is (equalp (erase-entry ht :xxx)
              #{}))

  (is (equal (erase-entry alst :xxx)
             '((:foo . 0) (:bar . 1) (:baz . 2))))
  (is (equal (erase-entry alst :bar)
             '((:foo . 0) (:baz . 2))))
  (is (equal (erase-entry alst :baz)
             '((:foo . 0))))
  (is (equal (erase-entry alst :foo)
             '()))
  (is (equal (erase-entry alst :xxx)
             '()))

  (is (equal (erase-entry plst :xxx)
             '(:foo 0 :bar 1 :baz 2)))
  (is (equal (erase-entry plst :bar)
             '(:foo 0 :baz 2)))
  (is (equal (erase-entry plst :baz)
             '(:foo 0)))
  (is (equal (erase-entry plst :foo)
             '()))
  (is (equal (erase-entry plst :xxx)
             '())))


;;--------------------------------------------------------------------
;; add+
;;--------------------------------------------------------------------
;; TODO:

(test ?add+
  (initialize-variables)
  
  (is (equalp #{} (add+ #{} #{} #{})))
  (is (equalp ht (add+ #{:foo 0} #{:bar 1} #{:baz 2})))
  (is (equalp ht (add+ #{:foo 10 :bar 11 :baz 12}
                       #{:foo 0 :bar 1 :baz 2})))
  (is (equalp ht (add+ #{:foo 0} '(:bar 1) #{:baz 2})))
  (is (equalp ht (add+ #{:foo 0} #{:bar 1} '((:baz . 2)))))
  (is (equalp ht (add+ #{} '(:foo 0 :bar 1 :baz 2))))

  (is (equal '() (add+ '() '() '())))
  (is (alist= alst (add+ '((:foo . 0)) '((:bar . 1)) '((:baz . 2)))))
  (is (alist= alst (add+ '((:foo . 10) (:bar . 11) (:baz . 12))
                         '((:foo .  0) (:bar .  1) (:baz .  2)))))
  (is (alist= alst (add+ '((:foo . 0)) #{:bar 1} '((:baz . 2)))))
  (is (alist= alst (add+ '((:foo . 0)) '(:bar 1) #{:baz 2})))

  (is (plist= plst (add+ '(:foo 0) '(:bar 1) '(:baz 2))))
  (is (plist= plst (add+ '(:foo 10 :bar 11 :baz 12)
                         '(:foo  0 :bar  1 :baz  2))))
  (is (plist= plst (add+ '(:foo 0) #{:bar 1} '((:baz . 2)))))
  (is (plist= plst (add+ '(:foo 0) #{:bar 1} '(:baz 2)))))


;;--------------------------------------------------------------------
;; ref+
;;--------------------------------------------------------------------
;; TODO:

(test ?ref+
  (initialize-variables)

  ;; hash-table
  (is (not (ref+ ht :xxx)))
  (is (= 42 (ref+ ht :xxx 42)))
  (is (= (ref+ ht :foo) 0))
  (is (= (ref+ ht :bar) 1))
  (is (= (ref+ ht :baz) 2))

  (setf (ref+ ht :foo) 10)
  (is (= 10 (gethash :foo ht)))

  (setf (ref+ ht :answer) 42)
  (is (= 42 (gethash :answer ht)))

  (incf (ref+ ht :bar))
  (is (= 2 (gethash :bar ht)))

  (decf (ref+ ht :bar))
  (is (= 1 (gethash :bar ht)))
  
  (incf (ref+ ht :good 0))
  (is (= 1 (gethash :good ht)))

  ;; alist
  (is (not (ref+ alst :xxx)))
  (is (= 42 (ref+ alst :xxx 42)))
  (is (= (ref+ alst :foo) 0))
  (is (= (ref+ alst :bar) 1))
  (is (= (ref+ alst :baz) 2))

  (setf (ref+ alst :foo) 10)
  (is (= 10 (cdr (assoc :foo alst))))

  (setf (ref+ alst :answer) 42)
  (is (= 42 (cdr (assoc :answer alst))))

  (incf (ref+ alst :bar))
  (is (= 2 (cdr (assoc :bar alst))))

  (decf (ref+ alst :bar))
  (is (= 1 (cdr (assoc :bar alst))))
  
  (incf (ref+ alst :good 0))
  (is (= 1 (cdr (assoc :good alst))))

  ;; plist
  (is (not (ref+ plst :xxx)))
  (is (= 42 (ref+ plst :xxx 42)))
  (is (= (ref+ plst :foo) 0))
  (is (= (ref+ plst :bar) 1))
  (is (= (ref+ plst :baz) 2))

  (setf (ref+ plst :foo) 10)
  (is (= 10 (getf plst :foo)))

  (setf (ref+ plst :answer) 42)
  (is (= 42 (getf plst :answer)))

  (incf (ref+ plst :bar))
  (is (= 2 (getf plst :bar)))

  (decf (ref+ plst :bar))
  (is (= 1 (getf plst :bar)))
  
  (incf (ref+ plst :good 0))
  (is (= 1 (getf plst :good))))


;;--------------------------------------------------------------------
;; keys
;;--------------------------------------------------------------------

(test ?keys.error
  (signals type-error (keys :foo))
  (signals type-error (keys :foo :count -1))
  (signals type-error (keys :foo :when #()))
  (signals type-error (keys :foo :unless #()))
  (signals type-error (keys :foo :key-test #())))


(test ?keys.hash-table
  (for-all ((ks (gen-list)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :in ks
                    :for v :from 0
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      ;; MEMO: set-equal ignore duplicate key in ks.
      (is (set-equal (keys ht)
                     ks
                     :test #'=))
      (is (set-equal (keys ht :when (lambda (k v) (declare (ignore v)) (oddp k)))
                     (remove-if-not #'oddp ks)
                     :test #'=))
      (is (set-equal (keys ht :unless (lambda (k v) (declare (ignore v)) (oddp k)))
                     (remove-if #'oddp ks)
                     :test #'=))
      (is (null (keys ht :when   (lambda (k v) (declare (ignore v)) (oddp k))
                         :unless (lambda (k v) (declare (ignore v)) (oddp k)))))
      
      (let ((count (random (max 1 (length ks)))))
        (is (subsetp (keys ht :count count)
                     ks
                     :test #'=))))))


(test ?keys.alist
  (for-all ((ks (gen-list)))
    (let ((alst (loop :for k :in ks
                      :for v :from 0
                      :collect (cons k v))))
      (is (set-equal (keys alst)
                     ks
                     :test #'=))
      (is (set-equal (keys alst :when (lambda (k v) (declare (ignore v)) (oddp k)))
                     (remove-if-not #'oddp ks)
                     :test #'=))
      (is (set-equal (keys alst :unless (lambda (k v) (declare (ignore v)) (oddp k)))
                     (remove-if #'oddp ks)
                     :test #'=))
      (is (null (keys alst :when   (lambda (k v) (declare (ignore v)) (oddp k))
                           :unless (lambda (k v) (declare (ignore v)) (oddp k)))))
      (let ((count (random (max 1 (length ks)))))
        (is (subsetp (keys alst :count count)
                     ks
                     :test #'=))))))


(test ?keys.plist
  (for-all ((ks (gen-list)))
    (let ((plst (loop :for k :in ks
                      :for v :from 0
                      :append (list k v))))
      (is (set-equal (keys plst)
                     ks
                     :test #'=))
      (is (set-equal (keys plst :when (lambda (k v) (declare (ignore v)) (oddp k)))
                     (remove-if-not #'oddp ks)
                     :test #'=))
      (is (set-equal (keys plst :unless (lambda (k v) (declare (ignore v)) (oddp k)))
                     (remove-if #'oddp ks)
                     :test #'=))
      (is (null (keys plst :when   (lambda (k v) (declare (ignore v)) (oddp k))
                           :unless (lambda (k v) (declare (ignore v)) (oddp k)))))
      (let ((count (random (max 1 (length ks)))))
        (is (subsetp (keys plst :count count)
                     ks
                     :test #'=))))))


;;--------------------------------------------------------------------
;; vals
;;--------------------------------------------------------------------

(test ?vals.error
  (signals type-error (vals :foo))
  (signals type-error (vals :foo :count -1))
  (signals type-error (vals :foo :when #()))
  (signals type-error (vals :foo :unless #()))
  (signals type-error (vals :foo :key-test #())))


(test ?vals.hash-table
  (for-all ((vs (gen-list)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0
                    :for v :in vs
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (is (set-equal (vals ht)
                     vs
                     :test #'=))
      (is (set-equal (vals ht :when (lambda (k v) (declare (ignore k)) (oddp v)))
                     (remove-if-not #'oddp vs)
                     :test #'=))
      (is (set-equal (vals ht :unless (lambda (k v) (declare (ignore k)) (oddp v)))
                     (remove-if #'oddp vs)
                     :test #'=))
      (is (null (vals ht :when   (lambda (k v) (declare (ignore k)) (oddp v))
                         :unless (lambda (k v) (declare (ignore k)) (oddp v)))))
      (let ((count (random (max 1 (length vs)))))
        (is (subsetp (vals ht :count count)
                     vs
                     :test #'=))))))


(test ?vals.alist
  (for-all ((vs (gen-list)))
    (let ((alst (loop :for k :from 0
                      :for v :in vs
                      :collect (cons k v))))
      (is (set-equal (vals alst)
                     vs
                     :test #'=))
      (is (set-equal (vals alst :when (lambda (k v) (declare (ignore k)) (oddp v)))
                     (remove-if-not #'oddp vs)
                     :test #'=))
      (is (set-equal (vals alst :unless (lambda (k v) (declare (ignore k)) (oddp v)))
                     (remove-if #'oddp vs)
                     :test #'=))
      (is (null (vals alst :when   (lambda (k v) (declare (ignore k)) (oddp v))
                           :unless (lambda (k v) (declare (ignore k)) (oddp v)))))
      (let ((count (random (max 1 (length vs)))))
        (is (subsetp (vals alst :count count)
                     vs
                     :test #'=))))))


(test ?vals.plist
  (for-all ((vs (gen-list)))
    (let ((plst (loop :for k :from 0
                      :for v :in vs
                      :append (list k v))))
      (is (set-equal (vals plst)
                     vs
                     :test #'=))
      (is (set-equal (vals plst :when (lambda (k v) (declare (ignore k)) (oddp v)))
                     (remove-if-not #'oddp vs)
                     :test #'=))
      (is (set-equal (vals plst :unless (lambda (k v) (declare (ignore k)) (oddp v)))
                     (remove-if #'oddp vs)
                     :test #'=))
      (is (null (vals plst :when   (lambda (k v) (declare (ignore k)) (oddp v))
                           :unless (lambda (k v) (declare (ignore k)) (oddp v)))))
      (let ((count (random (max 1 (length vs)))))
        (is (subsetp (vals plst :count count)
                     vs
                     :test #'=))))))


;;--------------------------------------------------------------------
;; to-hash
;;--------------------------------------------------------------------
;; TODO:
;;  * key
;;  * val

(test ?to-hash.error
  (signals type-error (to-hash :foo))
  (signals type-error (to-hash '() :key #()))
  (signals type-error (to-hash '() :val #()))
  (signals type-error (to-hash '() :key-test #()))
  (signals type-error (to-hash '() :size -1))
  (signals type-error (to-hash '() :rehash-size -1))
  (signals type-error (to-hash '() :rehash-threshold -1)))


(test (?to-hash :depends-on ?tabula=)
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let* ((key-test #'equal)
           (ht (loop :with h := (make-hash-table :test key-test)
                     :for k :in keys
                     :for v :in vals
                     :unless (gethash k h nil)
                       :do (setf (gethash k h) v)
                     :finally (return h)))
           (alst (loop :for k :in keys
                       :for v :in vals
                       :collect (cons k v)))
           (plst (loop :for k :in keys
                       :for v :in vals
                       :append (list k v))))
      ;; (repl-utilities:dbgv () keys vals ht alst plst)
      (is (tabula= (to-hash alst)
                   ht))
      (is (tabula= (to-hash plst)
                   ht)))))


;;--------------------------------------------------------------------
;; to-alist
;;--------------------------------------------------------------------
;; TODO:
;;  * key
;;  * val

(test ?to-alist.error
  (signals type-error (to-alist :foo))
  (signals type-error (to-alist '() :key #()))
  (signals type-error (to-alist '() :val #())))


(test (?to-alist :depends-on ?tabula=)
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let* ((key-test #'equal)
           (ht (loop :with h := (make-hash-table :test key-test)
                     :for k :in keys
                     :for v :in vals
                     :unless (gethash k h nil)
                       :do (setf (gethash k h) v)
                     :finally (return h)))
           (alst (loop :for k :in keys
                       :for v :in vals
                       :collect (cons k v)))
           (plst (loop :for k :in keys
                       :for v :in vals
                       :append (list k v))))
      ;; (repl-utilities:dbgv () keys vals ht alst plst)
      (is (tabula= (to-alist ht)
                   alst))
      (is (tabula= (to-alist alst)
                   alst))
      (is (tabula= (to-alist plst)
                   alst)))))


;;--------------------------------------------------------------------
;; to-plist
;;--------------------------------------------------------------------
;; TODO:
;;  * key
;;  * val

(test ?to-plist.error
  (signals type-error (to-plist :foo))
  (signals type-error (to-plist '() :key #()))
  (signals type-error (to-plist '() :val #()))
  (signals error      (to-plist '(:foo 0) :check-keys-of-type #()))
  (signals type-error (to-plist '(:foo 0 "BAR" 1)
                                :check-keys-of-type 'symbol)))


(test (?to-plist :depends-on ?tabula=)
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let* ((key-test #'equal)
           (ht (loop :with h := (make-hash-table :test key-test)
                     :for k :in keys
                     :for v :in vals
                     :unless (gethash k h nil)
                       :do (setf (gethash k h) v)
                     :finally (return h)))
           (alst (loop :for k :in keys
                       :for v :in vals
                       :collect (cons k v)))
           (plst (loop :for k :in keys
                       :for v :in vals
                       :append (list k v))))
      ;; (repl-utilities:dbgv () keys vals ht alst plst)
      (is (tabula= (to-plist ht)
                   plst))
      (is (tabula= (to-plist alst)
                   plst))
      (is (tabula= (to-plist plst)
                   plst)))))


;;--------------------------------------------------------------------
;; map+
;;--------------------------------------------------------------------

(defun the-answer (k v)
  (declare (ignore k v))
  42)

(test ?map+.error
  (signals type-error (map+ t #() '()))
  (signals type-error (map+ t #'the-answer :foo)))

(test ?map+.hash-table
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let* ((ht (loop :with h := (make-hash-table)
                     :for k :in keys
                     :for v :in vals
                     :do (setf (gethash k h) v)
                     :finally (return h)))
           (size (hash-table-count ht)))
      (is-eql    (map+ nil #'the-answer ht)
                 nil)
      (is-equal  (map+ 'list #'the-answer ht)
                 (make-list size :initial-element 42))
      (is-equal  (map+ 'sequence #'the-answer ht)
                 (make-list size :initial-element 42))
      (is-equal  (map+ 'sequens #'the-answer ht)
                 (make-list size :initial-element 42))
      (is-equalp (map+ 'vector #'the-answer ht)
                 (make-array size :initial-element 42))
      (is-equalp (map+ 'array #'the-answer ht)
                 (make-array size :initial-element 42))
      (is-true   (hash-table= (map+ t #'the-answer ht)
                              (loop :with h := (make-hash-table)
                                    :for k   :in keys
                                    :for nil :in vals
                                    :do (setf (gethash k h) 42)
                                    :finally (return h))))
      (is-true   (hash-table= (map+ 'hash-table #'the-answer ht)
                              (loop :with h := (make-hash-table)
                                    :for k   :in keys
                                    :for nil :in vals
                                    :do (setf (gethash k h) 42)
                                    :finally (return h))))
      (is-true   (alist= (map+ 'alist #'the-answer ht)
                         (loop :for k   :in keys
                               :for nil :in vals
                               :collect (cons k 42))))
      (is-true   (plist= (map+ 'plist #'the-answer ht)
                         (loop :for k   :in keys
                               :for nil :in vals
                               :append (list k 42)))))))

(test ?map+.alist
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let* ((alst (loop :for k :in keys
                       :for v :in vals
                       :collect (cons k v)))
           (size (length (normalize-alist alst))))
      (is-eql    (map+ nil #'the-answer alst)
                 nil)
      (is-equal  (map+ 'list #'the-answer alst)
                 (make-list size :initial-element 42))
      (is-equal  (map+ 'sequence #'the-answer alst)
                 (make-list size :initial-element 42))
      (is-equal  (map+ 'sequens #'the-answer alst)
                 (make-list size :initial-element 42))
      (is-equalp (map+ 'vector #'the-answer alst)
                 (make-array size :initial-element 42))
      (is-equalp (map+ 'array #'the-answer alst)
                 (make-array size :initial-element 42))
      (is-true   (hash-table= (map+ 'hash-table #'the-answer alst)
                              (loop :with h := (make-hash-table :test 'equal)
                                    :for k   :in keys
                                    :for nil :in vals
                                    :do (setf (gethash k h) 42)
                                    :finally (return h))))
      (is-true   (alist= (map+ t #'the-answer alst)
                         (loop :for k   :in keys
                               :for nil :in vals
                               :collect (cons k 42))))
      (is-true   (alist= (map+ 'alist #'the-answer alst)
                         (loop :for k   :in keys
                               :for nil :in vals
                               :collect (cons k 42))))
      (is-true   (plist= (map+ 'plist #'the-answer alst)
                         (loop :for k   :in keys
                               :for nil :in vals
                               :append (list k 42)))))))


(test ?map+.plist
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let* ((plst (loop :for k :in keys
                       :for v :in vals
                       :append (list k v)))
           (size (/ (length plst) 2)))
      (is-eql    (map+ nil #'the-answer plst)
                 nil)
      (is-equal  (map+ 'list #'the-answer plst)
                 (make-list size :initial-element 42))
      (is-equal  (map+ 'sequence #'the-answer plst)
                 (make-list size :initial-element 42))
      (is-equal  (map+ 'sequens #'the-answer plst)
                 (make-list size :initial-element 42))
      (is-equalp (map+ 'vector #'the-answer plst)
                 (make-array size :initial-element 42))
      (is-equalp (map+ 'array #'the-answer plst)
                 (make-array size :initial-element 42))

      (is-true   (hash-table= (map+ 'hash-table #'the-answer plst)
                              (loop :with h := (make-hash-table)
                                    :for k   :in keys
                                    :for nil :in vals
                                    :do (setf (gethash k h) 42)
                                    :finally (return h))))
      (is-true   (alist= (map+ 'alist #'the-answer plst)
                         (loop :for k   :in keys
                               :for nil :in vals
                               :collect (cons k 42))))
      (is-true   (plist= (map+ t #'the-answer plst)
                         (loop :for k   :in keys
                               :for nil :in vals
                               :append (list k 42))))
      (is-true   (plist= (map+ 'plist #'the-answer plst)
                         (loop :for k   :in keys
                               :for nil :in vals
                               :append (list k 42)))))))


;;--------------------------------------------------------------------
;; reduce+
;;--------------------------------------------------------------------

(test ?reduce+.error
  (signals type-error (reduce+ #() '(:foo 0)))
  (signals type-error (reduce+ #() :foo))
  (signals type-error (reduce+ #() '(:foo 0) :key #()))
  (signals type-error (reduce+ #() '(:foo 0) :key-test #())))


(test ?reduce+.hash-table
  (for-all ((vals (gen-list)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (is-= (reduce+ '+ ht)
            (reduce  '+ vals))
      (is-= (reduce+ '+ ht   :initial-value 42)
            (reduce  '+ vals :initial-value 42))
      (is-= (reduce+ '+ ht   :key '1+)
            (reduce  '+ vals :key '1+)))))


(test ?reduce+.alist
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let* ((alst (loop :for k :in keys
                       :for v :in vals
                       :collect (cons k v)))
           (vals (mapcar #'cdr (normalize-alist alst '=))))
      (is-= (reduce+ '+ alst)
            (reduce  '+ vals))
      (is-= (reduce+ '+ alst :initial-value 42)
            (reduce  '+ vals :initial-value 42))
      (is-= (reduce+ '+ alst :key '1+)
            (reduce  '+ vals :key '1+)))))


(test ?reduce+.plist
  (for-all ((keys (gen-list))
            (vals (gen-list)))
    (let* ((plst (loop :for k :in keys
                       :for v :in vals
                       :append (list k v)))
           (alst (loop :for k :in keys
                       :for v :in vals
                       :collect (cons k v)))
           (vals (mapcar #'cdr (normalize-alist alst '=))))
      (is-= (reduce+ '+ plst)
            (reduce  '+ vals))
      (is-= (reduce+ '+ plst :initial-value 42)
            (reduce  '+ vals :initial-value 42))
      (is-= (reduce+ '+ plst :key '1+)
            (reduce  '+ vals :key '1+)))))


;;--------------------------------------------------------------------
;; every+
;;--------------------------------------------------------------------

(test ?every+.error
  (signals type-error (every+ #() '(:foo 0)))
  (signals type-error (every+ (constantly T) :foo))
  (signals type-error (every+ (constantly T) '(:foo 0) :key-test #()))
  (signals type-error (every+ (constantly T) '(:foo 0) :key #()))
  (signals type-error (every+ (constantly T) '(:foo 0) :val #())))


(test ?every+.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min 0 :max 5))))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (flet ((plusp2 (k v) (declare (ignore k)) (plusp v)))
        (is-eql (every+ #'plusp2 ht)
                (every  #'plusp  vals))
        (is-eql (every+ #'plusp2 ht   :key '1+)
                (every  #'plusp  vals))
        (is-eql (every+ #'plusp2 ht   :val '1+)
                (every  #'plusp  (mapcar '1+ vals)))))))


(test ?every+.alist
  (flet ((plusp2 (k v) (declare (ignore k)) (plusp v)))
    
    (is-false (every+ #'plusp2 '(("foo" . 1) ("FOO" . 0))
                      :key-test #'string=))
    (is-true  (every+ #'plusp2 '(("foo" . 1) ("FOO" . 0))
                      :key-test #'string-equal))
    
    (for-all ((vals (gen-list :elements (gen-integer :min 0 :max 5))))
      (let ((alst (loop :for k :from 0
                        :for v :in vals
                        :collect (cons k v))))
        (is-eql (every+ #'plusp2 alst)
                (every  #'plusp  vals))
        (is-eql (every+ #'plusp2 alst :key '1+)
                (every  #'plusp  vals))
        (is-eql (every+ #'plusp2 alst :val '1+)
                (every  #'plusp  (mapcar '1+ vals)))))))


(test ?every+.plist
  (flet ((plusp2 (k v) (declare (ignore k)) (plusp v)))
    
    (is-false (every+ #'plusp2 '("foo" 1 "FOO" 0)
                      :key-test #'string=))
    (is-true  (every+ #'plusp2 '("foo" 1 "FOO" 0)
                      :key-test #'string-equal))
    
    (for-all ((vals (gen-list :elements (gen-integer :min 0 :max 5))))
      (let ((plst (loop :for k :from 0
                        :for v :in vals
                        :append (list k v))))
        (is-eql (every+ #'plusp2 plst)
                (every  #'plusp  vals))
        (is-eql (every+ #'plusp2 plst :key '1+)
                (every  #'plusp  vals))
        (is-eql (every+ #'plusp2 plst :val '1+)
                (every  #'plusp  (mapcar '1+ vals)))))))


;;--------------------------------------------------------------------
;; notevery+
;;--------------------------------------------------------------------

(test ?notevery+.error
  (signals type-error (notevery+ #() '(:foo 0)))
  (signals type-error (notevery+ (constantly T) :foo))
  (signals type-error (notevery+ (constantly T) '(:foo 0) :key-test #()))
  (signals type-error (notevery+ (constantly T) '(:foo 0) :key #()))
  (signals type-error (notevery+ (constantly T) '(:foo 0) :val #())))


(test ?notevery+.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min 0 :max 5))))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (flet ((plusp2 (k v) (declare (ignore k)) (plusp v)))
        (is-eql (notevery+ #'plusp2 ht)
                (notevery  #'plusp  vals))
        (is-eql (notevery+ #'plusp2 ht :key '1+)
                (notevery  #'plusp  vals))
        (is-eql (notevery+ #'plusp2 ht :val '1+)
                (notevery  #'plusp  (mapcar '1+ vals)))))))


(test ?notevery+.alist
  (flet ((plusp2 (k v) (declare (ignore k)) (plusp v)))
    
    (is-true  (notevery+ #'plusp2 '(("foo" . 1) ("FOO" . 0))
                         :key-test #'string=))
    (is-false (notevery+ #'plusp2 '(("foo" . 1) ("FOO" . 0))
                         :key-test #'string-equal))
    
    (for-all ((vals (gen-list :elements (gen-integer :min 0 :max 5))))
      (let ((alst (loop :for k :from 0
                        :for v :in vals
                        :collect (cons k v))))
        (is-eql (notevery+ #'plusp2 alst)
                (notevery  #'plusp  vals))
        (is-eql (notevery+ #'plusp2 alst :key '1+)
                (notevery  #'plusp  vals))
        (is-eql (notevery+ #'plusp2 alst :val '1+)
                (notevery  #'plusp  (mapcar '1+ vals)))))))


(test ?notevery+.plist
  (flet ((plusp2 (k v) (declare (ignore k)) (plusp v)))
    
    (is-true  (notevery+ #'plusp2 '("foo" 1 "FOO" 0)
                         :key-test #'string=))
    (is-false (notevery+ #'plusp2 '("foo" 1 "FOO" 0)
                         :key-test #'string-equal))
    
    (for-all ((vals (gen-list :elements (gen-integer :min 0 :max 5))))
      (let ((plst (loop :for k :from 0
                        :for v :in vals
                        :append (list k v))))
        (is-eql (notevery+ #'plusp2 plst)
                (notevery  #'plusp  vals))
        (is-eql (notevery+ #'plusp2 plst :key '1+)
                (notevery  #'plusp  vals))
        (is-eql (notevery+ #'plusp2 plst :val '1+)
                (notevery  #'plusp  (mapcar '1+ vals)))))))


;;--------------------------------------------------------------------
;; some+
;;--------------------------------------------------------------------

(test ?some+.error
  (signals type-error (some+ #() '(:foo 0)))
  (signals type-error (some+ (constantly T) :foo))
  (signals type-error (some+ (constantly T) '(:foo 0) :key-test #()))
  (signals type-error (some+ (constantly T) '(:foo 0) :key #()))
  (signals type-error (some+ (constantly T) '(:foo 0) :val #())))


(test ?some+.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min 0 :max 5))))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (flet ((plusp2 (k v) (declare (ignore k)) (plusp v)))
        (is-eql (some+ #'plusp2 ht)
                (some  #'plusp  vals))
        (is-eql (some+ #'plusp2 ht   :key '1+)
                (some  #'plusp  vals))
        (is-eql (some+ #'plusp2 ht   :val '1+)
                (some  #'plusp  (mapcar '1+ vals)))))))


(test ?some+.alist
  (flet ((plusp2 (k v) (declare (ignore k)) (plusp v)))
    
    (is-true  (some+ #'plusp2 '(("foo" . 0) ("FOO" . 1))
                     :key-test #'string=))
    (is-false (some+ #'plusp2 '(("foo" . 0) ("FOO" . 1))
                     :key-test #'string-equal))
    
    (for-all ((vals (gen-list :elements (gen-integer :min 0 :max 5))))
      (let ((alst (loop :for k :from 0
                        :for v :in vals
                        :collect (cons k v))))
        (is-eql (some+ #'plusp2 alst)
                (some  #'plusp  vals))
        (is-eql (some+ #'plusp2 alst :key '1+)
                (some  #'plusp  vals))
        (is-eql (some+ #'plusp2 alst :val '1+)
                (some  #'plusp  (mapcar '1+ vals)))))))


(test ?some+.plist
  (flet ((plusp2 (k v) (declare (ignore k)) (plusp v)))
    
    (is-true  (some+ #'plusp2 '("foo" 0 "FOO" 1)
                     :key-test #'string=))
    (is-false (some+ #'plusp2 '("foo" 0 "FOO" 1)
                     :key-test #'string-equal))
    
    (for-all ((vals (gen-list :elements (gen-integer :min 0 :max 5))))
      (let ((plst (loop :for k :from 0
                        :for v :in vals
                        :append (list k v))))
        (is-eql (some+ #'plusp2 plst)
                (some  #'plusp  vals))
        (is-eql (some+ #'plusp2 plst :key '1+)
                (some  #'plusp  vals))
        (is-eql (some+ #'plusp2 plst :val '1+)
                (some  #'plusp  (mapcar '1+ vals)))))))


;;--------------------------------------------------------------------
;; notany+
;;--------------------------------------------------------------------

(test ?notany+.error
  (signals type-error (notany+ #() '(:foo 0)))
  (signals type-error (notany+ (constantly T) :foo))
  (signals type-error (notany+ (constantly T) '(:foo 0) :key-test #()))
  (signals type-error (notany+ (constantly T) '(:foo 0) :key #()))
  (signals type-error (notany+ (constantly T) '(:foo 0) :val #())))


(test ?notany+.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min 0 :max 5))))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (flet ((plusp2 (k v) (declare (ignore k)) (plusp v)))
        (is-eql (notany+ #'plusp2 ht)
                (notany  #'plusp  vals))
        (is-eql (notany+ #'plusp2 ht   :key '1+)
                (notany  #'plusp  vals))
        (is-eql (notany+ #'plusp2 ht   :val '1+)
                (notany  #'plusp  (mapcar '1+ vals)))))))


(test ?notany+.alist
  (flet ((plusp2 (k v) (declare (ignore k)) (plusp v)))
    
    (is-false (notany+ #'plusp2 '(("foo" . 0) ("FOO" . 1))
                       :key-test #'string=))
    (is-true  (notany+ #'plusp2 '(("foo" . 0) ("FOO" . 1))
                       :key-test #'string-equal))
    
    (for-all ((vals (gen-list :elements (gen-integer :min 0 :max 5))))
      (let ((alst (loop :for k :from 0
                        :for v :in vals
                        :collect (cons k v))))
        (is-eql (notany+ #'plusp2 alst)
                (notany  #'plusp  vals))
        (is-eql (notany+ #'plusp2 alst :key '1+)
                (notany  #'plusp  vals))
        (is-eql (notany+ #'plusp2 alst :val '1+)
                (notany  #'plusp  (mapcar '1+ vals)))))))


(test ?notany+.plist
  (flet ((plusp2 (k v) (declare (ignore k)) (plusp v)))
    
    (is-false (notany+ #'plusp2 '("foo" 0 "FOO" 1)
                      :key-test #'string=))
    (is-true  (notany+ #'plusp2 '("foo" 0 "FOO" 1)
                      :key-test #'string-equal))
    
    (for-all ((vals (gen-list :elements (gen-integer :min 0 :max 5))))
      (let ((plst (loop :for k :from 0
                        :for v :in vals
                        :append (list k v))))
        (is-eql (notany+ #'plusp2 plst)
                (notany  #'plusp  vals))
        (is-eql (notany+ #'plusp2 plst :key '1+)
                (notany  #'plusp  vals))
        (is-eql (notany+ #'plusp2 plst :val '1+)
                (notany  #'plusp  (mapcar '1+ vals)))))))


;;--------------------------------------------------------------------
;; count+
;;--------------------------------------------------------------------

(test ?count+.error
  (signals type-error (count+ 42 :foo))
  (signals type-error (count+ 42 '() :val #()))
  (signals type-error (count+ 42 '() :test #()))
  (signals type-error (count+ 42 '() :key-test #()))
  (signals type-error (count+ 42 '(:foo :bar :baz))))


(test ?count+.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (is-= (count+ item ht)
            (count  item vals))
      (is-= (count+ item ht   :val '1+)
            (count  item vals :key '1+))
      (is-= (count+ item ht   :test (lambda (x y) (< x y)))
            (count  item vals :test (lambda (x y) (< x y)))))))


(test ?count+.alist
  (is-= (count+ 1 '(("foo" . 1) ("FOO" . 1))
                :key-test #'string-equal)
        1)
  (is-= (count+ 1 '(("foo" . 1) ("FOO" . 1))
                :key-test #'string=)
        2)

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((alst (loop :for k :from 0
                      :for v :in vals
                      :collect (cons k v))))
      (is-= (count+ item alst)
            (count  item vals))
      (is-= (count+ item alst :val '1+)
            (count  item vals :key '1+))
      (is-= (count+ item alst :test (lambda (x y) (< x y)))
            (count  item vals :test (lambda (x y) (< x y)))))))


(test ?count+.plist
  (is-= (count+ 1 '("foo" 1 "FOO" 1)
                :key-test #'string-equal)
        1)
  (is-= (count+ 1 '("foo" 1 "FOO" 1)
                :key-test #'string=)
        2)

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((plst (loop :for k :from 0
                      :for v :in vals
                      :append (list k v))))
      (is-= (count+ item plst)
            (count  item vals))
      (is-= (count+ item plst :val '1+)
            (count  item vals :key '1+))
      (is-= (count+ item plst :test (lambda (x y) (< x y)))
            (count  item vals :test (lambda (x y) (< x y)))))))


;;--------------------------------------------------------------------
;; count-if+
;;--------------------------------------------------------------------

(test ?count-if+.error
  (signals type-error (count-if+ 'identity :foo))
  (signals type-error (count-if+ 'identity '(:foo :bar :baz)))
  (signals type-error (count-if+  42 '()))
  (signals type-error (count-if+ 'identity '() :key #()))
  (signals type-error (count-if+ 'identity '() :val #()))
  (signals type-error (count-if+ 'identity '() :key-test #())))


(test ?count-if+.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-= (count-if+ #'item<2 ht)
              (count-if  #'item<1 vals))
        (is-= (count-if+ #'item<2 ht   :val '1+)
              (count-if  #'item<1 vals :key '1+))
        (is-= (count-if+ #'item<2 ht   :key '1+)
              (count-if  #'item<1 vals))))))


(test ?count-if+.alist
  (is-= (count-if+ (lambda (k v) (declare (ignore k)) (= v 1))
                   '(("foo" . 1) ("FOO" . 1))
                   :key-test #'string-equal)
        1)
  (is-= (count-if+ (lambda (k v) (declare (ignore k)) (= v 1))
                   '(("foo" . 1) ("FOO" . 1))
                   :key-test #'string=)
        2)

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((alst (loop :for k :from 0
                      :for v :in vals
                      :collect (cons k v))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-= (count-if+ #'item<2 alst)
              (count-if  #'item<1 vals))
        (is-= (count-if+ #'item<2 alst :val '1+)
              (count-if  #'item<1 vals :key '1+))
        (is-= (count-if+ #'item<2 alst :key '1+)
              (count-if  #'item<1 vals))))))


(test ?count-if+.plist
  (is-= (count-if+ (lambda (k v) (declare (ignore k)) (= v 1))
                   '("foo" 1 "FOO" 1)
                   :key-test #'string-equal)
        1)
  (is-= (count-if+ (lambda (k v) (declare (ignore k)) (= v 1))
                   '("foo" 1 "FOO" 1)
                   :key-test #'string=)
        2)

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((plst (loop :for k :from 0
                      :for v :in vals
                      :append (list k v))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-= (count-if+ #'item<2 plst)
              (count-if  #'item<1 vals))
        (is-= (count-if+ #'item<2 plst :val '1+)
              (count-if  #'item<1 vals :key '1+))
        (is-= (count-if+ #'item<2 plst :key '1+)
              (count-if  #'item<1 vals))))))


;;--------------------------------------------------------------------
;; count-if-not+
;;--------------------------------------------------------------------

(test ?count-if-not+.error
  (signals type-error (count-if-not+ 'identity :foo))
  (signals type-error (count-if-not+ 'identity '(:foo :bar :baz)))
  (signals type-error (count-if-not+  42 '()))
  (signals type-error (count-if-not+ 'identity '() :key #()))
  (signals type-error (count-if-not+ 'identity '() :val #()))
  (signals type-error (count-if-not+ 'identity '() :key-test #())))


(test ?count-if-not+.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-= (count-if-not+ #'item<2 ht)
              (count-if-not  #'item<1 vals))
        (is-= (count-if-not+ #'item<2 ht   :val '1+)
              (count-if-not  #'item<1 vals :key '1+))
        (is-= (count-if-not+ #'item<2 ht   :key '1+)
              (count-if-not  #'item<1 vals))))))


(test ?count-if-not+.alist
  (is-= (count-if-not+ (lambda (k v) (declare (ignore k)) (= v 0))
                       '(("foo" . 1) ("FOO" . 1))
                       :key-test #'string-equal)
        1)
  (is-= (count-if-not+ (lambda (k v) (declare (ignore k)) (= v 0))
                       '(("foo" . 1) ("FOO" . 1))
                       :key-test #'string=)
        2)
  
  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((alst (loop :for k :from 0
                      :for v :in vals
                      :collect (cons k v))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-= (count-if-not+ #'item<2 alst)
              (count-if-not  #'item<1 vals))
        (is-= (count-if-not+ #'item<2 alst :val '1+)
              (count-if-not  #'item<1 vals :key '1+))
        (is-= (count-if-not+ #'item<2 alst :key '1+)
              (count-if-not  #'item<1 vals))))))


(test ?count-if-not+.plist
  (is-= (count-if-not+ (lambda (k v) (declare (ignore k)) (= v 0))
                       '("foo" 1 "FOO" 1)
                       :key-test #'string-equal)
        1)
  (is-= (count-if-not+ (lambda (k v) (declare (ignore k)) (= v 0))
                       '("foo" 1 "FOO" 1)
                       :key-test #'string=)
        2)

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((plst (loop :for k :from 0
                      :for v :in vals
                      :append (list k v))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-= (count-if-not+ #'item<2 plst)
              (count-if-not  #'item<1 vals))
        (is-= (count-if-not+ #'item<2 plst :val '1+)
              (count-if-not  #'item<1 vals :key '1+))
        (is-= (count-if-not+ #'item<2 plst :key '1+)
              (count-if-not  #'item<1 vals))))))


;;--------------------------------------------------------------------
;; find+
;;--------------------------------------------------------------------

(test ?find+.error
  (signals type-error (find+ 42 :foo))
  (signals type-error (find+ 42 '() :val #()))
  (signals type-error (find+ 42 '() :test #()))
  (signals type-error (find+ 42 '() :key-test #()))
  (signals type-error (find+ 42 '(:foo :bar :baz))))


(test ?find+.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (is-eql (find+ item ht)
              (find  item vals))
      (is-eql (find+ item ht   :val '1+)
              (find  item vals :key '1+))
      ;; UGLY
      (is-true (member (find+ item ht :test (lambda (x y) (< x y)))
                       (or (remove-if (lambda (x) (<= x item))
                                      vals)
                           '(NIL)))))))


(test ?find+.alist
  (is-eql (find+ 1 '(("foo" . 0) ("FOO" . 1))
                 :key-test #'string-equal)
          NIL)
  (is-eql (find+ 1 '(("foo" . 0) ("FOO" . 1))
                 :key-test #'string=)
          1)

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((alst (loop :for k :from 0
                      :for v :in vals
                      :collect (cons k v))))
      (is-eql (find+ item alst)
              (find  item vals))
      (is-eql (find+ item alst :val '1+)
              (find  item vals :key '1+))
      (is-eql (find+ item alst :test (lambda (x y) (< x y)))
              (find  item vals :test (lambda (x y) (< x y)))))))


(test ?find+.plist
  (is-eql (find+ 1 '("foo" 0 "FOO" 1)
                 :key-test #'string-equal)
          NIL)
  (is-eql (find+ 1 '("foo" 0 "FOO" 1)
                 :key-test #'string=)
          1)

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((plst (loop :for k :from 0
                      :for v :in vals
                      :append (list k v))))
      (is-eql (find+ item plst)
              (find  item vals))
      (is-eql (find+ item plst :val '1+)
              (find  item vals :key '1+))
      (is-eql (find+ item plst :test (lambda (x y) (< x y)))
              (find  item vals :test (lambda (x y) (< x y)))))))


;;--------------------------------------------------------------------
;; find-if+
;;--------------------------------------------------------------------

(test ?find-if+.error
  (signals type-error (find-if+ 'identity :foo))
  (signals type-error (find-if+ 'identity '(:foo :bar :baz)))
  (signals type-error (find-if+  42 '()))
  (signals type-error (find-if+ 'identity '() :key #()))
  (signals type-error (find-if+ 'identity '() :val #()))
  (signals type-error (find-if+ 'identity '() :key-test #())))


(test ?find-if+.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (flet ((item=1 (v) (= v item))
             (item=2 (k v) (declare (ignore k)) (= v item)))
        (is-eql (find-if+ #'item=2 ht)
                (find-if  #'item=1 vals))
        (is-eql (find-if+ #'item=2 ht   :val '1+)
                (find-if  #'item=1 vals :key '1+))
        (is-eql (find-if+ #'item=2 ht   :key '1+)
                (find-if  #'item=1 vals))))))


(test ?find-if+.alist
  (is-eql (find-if+ (lambda (k v) (declare (ignore k)) (= v 1))
                    '(("foo" . 0) ("FOO" . 1))
                    :key-test #'string-equal)
          NIL)
  (is-eql (find-if+ (lambda (k v) (declare (ignore k)) (= v 1))
                    '(("foo" . 0) ("FOO" . 1))
                    :key-test #'string=)
          1)

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((alst (loop :for k :from 0
                      :for v :in vals
                      :collect (cons k v))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-eql (find-if+ #'item<2 alst)
                (find-if  #'item<1 vals))
        (is-eql (find-if+ #'item<2 alst :val '1+)
                (find-if  #'item<1 vals :key '1+))
        (is-eql (find-if+ #'item<2 alst :key '1+)
                (find-if  #'item<1 vals))))))


(test ?find-if+.plist
  (is-eql (find-if+ (lambda (k v) (declare (ignore k)) (= v 1))
                    '("foo" 0 "FOO" 1)
                    :key-test #'string-equal)
          NIL)
  (is-eql (find-if+ (lambda (k v) (declare (ignore k)) (= v 1))
                    '("foo" 0 "FOO" 1)
                    :key-test #'string=)
          1)

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((plst (loop :for k :from 0
                      :for v :in vals
                      :append (list k v))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-eql (find-if+ #'item<2 plst)
                (find-if  #'item<1 vals))
        (is-eql (find-if+ #'item<2 plst :val '1+)
                (find-if  #'item<1 vals :key '1+))
        (is-eql (find-if+ #'item<2 plst :key '1+)
                (find-if  #'item<1 vals))))))


;;--------------------------------------------------------------------
;; find-if-not+
;;--------------------------------------------------------------------

(test ?find-if-not+.error
  (signals type-error (find-if-not+ 'identity :foo))
  (signals type-error (find-if-not+ 'identity '(:foo :bar :baz)))
  (signals type-error (find-if-not+  42 '()))
  (signals type-error (find-if-not+ 'identity '() :key #()))
  (signals type-error (find-if-not+ 'identity '() :val #()))
  (signals type-error (find-if-not+ 'identity '() :key-test #())))


(test ?find-if-not+.hash-table
  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0
                    :for v :in vals
                    :do (setf (gethash k h) v)
                    :finally (return h))))
      (flet ((item/=1 (v) (/= v item))
             (item/=2 (k v) (declare (ignore k)) (/= v item)))
        (is-eql (find-if-not+ #'item/=2 ht)
                (find-if-not  #'item/=1 vals))
        (is-eql (find-if-not+ #'item/=2 ht   :val '1+)
                (find-if-not  #'item/=1 vals :key '1+))
        (is-eql (find-if-not+ #'item/=2 ht   :key '1+)
                (find-if-not  #'item/=1 vals))))))


(test ?find-if-not+.alist
  (is-eql (find-if-not+ (lambda (k v) (declare (ignore k)) (= v 0))
                        '(("foo" . 0) ("FOO" . 1))
                        :key-test #'string-equal)
          NIL)
  (is-eql (find-if-not+ (lambda (k v) (declare (ignore k)) (= v 0))
                        '(("foo" . 0) ("FOO" . 1))
                        :key-test #'string=)
          1)

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((alst (loop :for k :from 0
                      :for v :in vals
                      :collect (cons k v))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-eql (find-if-not+ #'item<2 alst)
                (find-if-not  #'item<1 vals))
        (is-eql (find-if-not+ #'item<2 alst :val '1+)
                (find-if-not  #'item<1 vals :key '1+))
        (is-eql (find-if-not+ #'item<2 alst :key '1+)
                (find-if-not  #'item<1 vals))))))


(test ?find-if-not+.plist
  (is-eql (find-if-not+ (lambda (k v) (declare (ignore k)) (= v 0))
                        '("foo" 0 "FOO" 1)
                        :key-test #'string-equal)
          NIL)
  (is-eql (find-if-not+ (lambda (k v) (declare (ignore k)) (= v 0))
                        '("foo" 0 "FOO" 1)
                        :key-test #'string=)
          1)

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((plst (loop :for k :from 0
                      :for v :in vals
                      :append (list k v))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-eql (find-if-not+ #'item<2 plst)
                (find-if-not  #'item<1 vals))
        (is-eql (find-if-not+ #'item<2 plst :val '1+)
                (find-if-not  #'item<1 vals :key '1+))
        (is-eql (find-if-not+ #'item<2 plst :key '1+)
                (find-if-not  #'item<1 vals))))))


;;--------------------------------------------------------------------
;; position+
;;--------------------------------------------------------------------

(test ?position+.error
  (signals type-error (position+ 42 :foo))
  (signals type-error (position+ 42 '() :val #()))
  (signals type-error (position+ 42 '() :test #()))
  (signals type-error (position+ 42 '() :key-test #()))
  (signals type-error (position+ 42 '(:foo :bar :baz))))


(test ?position+.hash-table
  (for-all ((num-entries (gen-integer :min 0 :max 10)))
    (let* ((ht (loop :with h := (make-hash-table :test 'equal)
                     :for k :from 0 :below num-entries
                     :for v :from 0 :below num-entries
                     :do (setf (gethash k h) v)
                     :finally (return h)))
           (item (random (1+ num-entries))))
      
      (is-eql (position+ item ht)
              (if (= item num-entries) NIL item))

      (is-eql (position+ item ht :val '1+)
              (if (zerop item) NIL (1- item)))
      ;; UGLY
      (is-eql (position+ item ht :test (lambda (item v) (< item v (+ 2 item))))
              (if (<= (1- num-entries) item)
                  NIL
                  (1+ item))))))


(test ?position+.alist
  (is-equal (position+ 1 '(("foo" . 0) ("FOO" . 1))
                       :key-test #'string-equal)
            NIL)
  (is-equal (position+ 1 '(("foo" . 0) ("FOO" . 1))
                       :key-test #'string=)
            "FOO")

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((alst (loop :for k :from 0
                      :for v :in vals
                      :collect (cons k v))))
      (is-eql (position+ item alst)
              (position  item vals))
      (is-eql (position+ item alst :val '1+)
              (position  item vals :key '1+))
      (is-eql (position+ item alst :test (lambda (x y) (< x y)))
              (position  item vals :test (lambda (x y) (< x y)))))))


(test ?position+.plist
  (is-equal (position+ 1 '("foo" 0 "FOO" 1)
                       :key-test #'string-equal)
            NIL)
  (is-equal (position+ 1 '("foo" 0 "FOO" 1)
                       :key-test #'string=)
            "FOO")

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((plst (loop :for k :from 0
                      :for v :in vals
                      :append (list k v))))
      (is-eql (position+ item plst)
              (position  item vals))
      (is-eql (position+ item plst :val '1+)
              (position  item vals :key '1+))
      (is-eql (position+ item plst :test (lambda (x y) (< x y)))
              (position  item vals :test (lambda (x y) (< x y)))))))


;;--------------------------------------------------------------------
;; position-if+
;;--------------------------------------------------------------------

(test ?position-if+.error
  (signals type-error (position-if+ 'identity :foo))
  (signals type-error (position-if+ 'identity '(:foo :bar :baz)))
  (signals type-error (position-if+  42 '()))
  (signals type-error (position-if+ 'identity '() :key #()))
  (signals type-error (position-if+ 'identity '() :val #()))
  (signals type-error (position-if+ 'identity '() :key-test #())))


(test ?position-if+.hash-table
  (for-all ((num-entries (gen-integer :min 0 :max 10)))
    (let* ((ht (loop :with h := (make-hash-table :test 'equal)
                     :for k :from 0 :below num-entries
                     :for v :from 0 :below num-entries
                     :do (setf (gethash k h) v)
                     :finally (return h)))
           (item (random (1+ num-entries))))
      (flet ((item= (k v) (declare (ignore k)) (= v item)))
        
        (is-eql (position-if+ #'item= ht)
                (if (= item num-entries) NIL item))

        (is-eql (position-if+ #'item= ht :val '1+)
                (if (zerop item) NIL (1- item)))
        
        (is-eql (position-if+ #'item= ht :key '1+)
                (if (= item num-entries) NIL item))))))


(test ?position-if+.alist
  (is-equal (position-if+ (lambda (k v) (declare (ignore k)) (= v 1))
                          '(("foo" . 0) ("FOO" . 1))
                          :key-test #'string-equal)
            NIL)
  (is-equal (position-if+ (lambda (k v) (declare (ignore k)) (= v 1))
                          '(("foo" . 0) ("FOO" . 1))
                          :key-test #'string=)
            "FOO")

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((alst (loop :for k :from 0
                      :for v :in vals
                      :collect (cons k v))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-eql (position-if+ #'item<2 alst)
                (position-if  #'item<1 vals))
        (is-eql (position-if+ #'item<2 alst :val '1+)
                (position-if  #'item<1 vals :key '1+))
        (is-eql (position-if+ #'item<2 alst :key '1+)
                (position-if  #'item<1 vals))))))


(test ?position-if+.plist
  (is-equal (position-if+ (lambda (k v) (declare (ignore k)) (= v 1))
                          '("foo" 0 "FOO" 1)
                          :key-test #'string-equal)
            NIL)
  (is-equal (position-if+ (lambda (k v) (declare (ignore k)) (= v 1))
                          '("foo" 0 "FOO" 1)
                          :key-test #'string=)
            "FOO")

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((plst (loop :for k :from 0
                      :for v :in vals
                      :append (list k v))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-eql (position-if+ #'item<2 plst)
                (position-if  #'item<1 vals))
        (is-eql (position-if+ #'item<2 plst :val '1+)
                (position-if  #'item<1 vals :key '1+))
        (is-eql (position-if+ #'item<2 plst :key '1+)
                (position-if  #'item<1 vals))))))


;;--------------------------------------------------------------------
;; position-if-not+
;;--------------------------------------------------------------------

(test ?position-if-not+.error
  (signals type-error (position-if-not+ 'identity :foo))
  (signals type-error (position-if-not+ 'identity '(:foo :bar :baz)))
  (signals type-error (position-if-not+  42 '()))
  (signals type-error (position-if-not+ 'identity '() :key #()))
  (signals type-error (position-if-not+ 'identity '() :val #()))
  (signals type-error (position-if-not+ 'identity '() :key-test #())))


(test ?position-if-not+.hash-table
  (for-all ((num-entries (gen-integer :min 0 :max 10)))
    (let* ((ht (loop :with h := (make-hash-table :test 'equal)
                     :for k :from 0 :below num-entries
                     :for v :from 0 :below num-entries
                     :do (setf (gethash k h) v)
                     :finally (return h)))
           (item (random (1+ num-entries))))
      (flet ((item/= (k v) (declare (ignore k)) (/= v item)))
        
        (is-eql (position-if-not+ #'item/= ht)
                (if (= item num-entries) NIL item))

        (is-eql (position-if-not+ #'item/= ht :val '1+)
                (if (zerop item) NIL (1- item)))
        
        (is-eql (position-if-not+ #'item/= ht :key '1+)
                (if (= item num-entries) NIL item))))))


(test ?position-if-not+.alist
  (is-equal (position-if-not+ (lambda (k v) (declare (ignore k)) (= v 0))
                              '(("foo" . 0) ("FOO" . 1))
                              :key-test #'string-equal)
            NIL)
  (is-equal (position-if-not+ (lambda (k v) (declare (ignore k)) (= v 0))
                              '(("foo" . 0) ("FOO" . 1))
                              :key-test #'string=)
            "FOO")

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((alst (loop :for k :from 0
                      :for v :in vals
                      :collect (cons k v))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-eql (position-if-not+ #'item<2 alst)
                (position-if-not  #'item<1 vals))
        (is-eql (position-if-not+ #'item<2 alst :val '1+)
                (position-if-not  #'item<1 vals :key '1+))
        (is-eql (position-if-not+ #'item<2 alst :key '1+)
                (position-if-not  #'item<1 vals))))))


(test ?position-if-not+.plist
  (is-equal (position-if-not+ (lambda (k v) (declare (ignore k)) (= v 0))
                              '("foo" 0 "FOO" 1)
                              :key-test #'string-equal)
            NIL)
  (is-equal (position-if-not+ (lambda (k v) (declare (ignore k)) (= v 0))
                              '("foo" 0 "FOO" 1)
                              :key-test #'string=)
            "FOO")

  (for-all ((vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((plst (loop :for k :from 0
                      :for v :in vals
                      :append (list k v))))
      (flet ((item<1 (v) (< v item))
             (item<2 (k v) (declare (ignore k)) (< v item)))
        (is-eql (position-if-not+ #'item<2 plst)
                (position-if-not  #'item<1 vals))
        (is-eql (position-if-not+ #'item<2 plst :val '1+)
                (position-if-not  #'item<1 vals :key '1+))
        (is-eql (position-if-not+ #'item<2 plst :key '1+)
                (position-if-not  #'item<1 vals))))))


;;--------------------------------------------------------------------
;; remove+
;;--------------------------------------------------------------------

(test ?remove+.error
  (signals type-error (remove+ 42 :foo))
  (signals type-error (remove+ 42 '() :val #()))
  (signals type-error (remove+ 42 '() :test #()))
  (signals type-error (remove+ 42 '() :key-test #()))
  (signals type-error (remove+ 42 '(:foo :bar :baz))))


(test ?remove+.hash-table
  (for-all ((num-entries (gen-integer :min 0 :max 10)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0 :below num-entries
                    :for v :from 0 :below num-entries
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (item (random (1+ num-entries))))

      ;; To remove no test fail for num-entries=0.
      (is-true (hash-table-p (remove+ item ht)))
      
      (loop :for k :being :the :hash-keys
              :of (remove+ item ht) :using (:hash-value v)
            :do (is-= k v))
      (loop :for k :being :the :hash-keys
              :of (remove+ item ht :val '1+)
              :using (:hash-value v)
            :do (is-= k v))
      (loop :for k :being :the :hash-keys
              :of (remove+ item ht :count (random (max 1 num-entries)))
              :using (:hash-value v)
            :do (is-= k v))
      (loop :for k :being :the :hash-keys
              :of (remove+ item ht :test (lambda (x y) (< x y)))
                :using (:hash-value v)
            :do (is-= k v)))))


(test ?remove+.alist
  (is-equal (remove+ 0 '(("foo" . 0) ("FOO" . 1))
                     :key-test #'string-equal)
            '())
  (is-equal (remove+ 0 '(("foo" . 0) ("FOO" . 1))
                     :key-test #'string=)
            '(("FOO" . 1)))

  (for-all ((keys (gen-list :elements (gen-integer :min -10 :max 10)))
            (vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let* ((alst (loop :for k :in keys
                       :for v :in vals
                       :collect (cons k v)))
           (normalized (remove-duplicates alst :key #'car :from-end t)))

      (is-equal (remove+ item alst)
                (remove  item normalized :key #'cdr))

      (let ((count (random (max 1 (length alst))))
            (key-test #'equal))
        (is (alist= (remove+ item alst :count count :key-test key-test)
                    (remove  item normalized :key #'cdr :count count :test key-test)
                    :key-test key-test)))
      
      (is-equal (remove+ item alst :val '1+)
                (remove  item normalized :key (lambda (entry) (1+ (cdr entry)))))

      (is-equal (remove+ item alst :test (lambda (x y) (< x y)))
                (remove  item normalized :key #'cdr :test (lambda (x y) (< x y)))))))


(test ?remove+.plist
  (is-equal (remove+ 0 '("foo" 0 "FOO" 1)
                     :key-test #'string-equal)
            '())
  (is-equal (remove+ 0 '("foo" 0 "FOO" 1)
                     :key-test #'string=)
            '("FOO" 1))

  (is-equal (remove+ 42 '(:foo 42 :bar 42 :baz 1 :foo 3) :count 0)
            '(:foo 42 :bar 42 :baz 1 :foo 3))
  (is-equal (remove+ 42 '(:foo 42 :bar 42 :baz 1 :foo 3) :count 1)
            '(:bar 42 :baz 1))
  (is-equal (remove+ 42 '(:foo 42 :bar 42 :baz 1 :foo 3) :count 10)
            '(:baz 1))
  
  (for-all ((keys (gen-list :elements (gen-integer :min -10 :max 10)))
            (vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((plst (loop :for k :in keys
                      :for v :in vals
                      :append (list k v)))
          (test     #'=)
          (key-test #'equal))
      
      (is-equal (remove+ item plst :key-test key-test :test test)
                (loop :with checked-keys := '()
                      :for k :in keys
                      :for v :in vals
                      :unless (member k checked-keys :test key-test)
                        :do (push k checked-keys)
                        :and :unless (funcall test item v)
                               :append (list k v)))
      
      (is-equal (remove+ item plst :key-test key-test :val '1+ :test test)
                (loop :with checked-keys := '()
                      :for k :in keys
                      :for v :in vals
                      :unless (member k checked-keys :test key-test)
                        :do (push k checked-keys)
                        :and :unless (funcall test item (1+ v))
                               :append (list k v)))

      (is-equal (remove+ item plst :key-test key-test :test (lambda (x y) (< x y)))
                (loop :with checked-keys := '()
                      :for k :in keys
                      :for v :in vals
                      :unless (member k checked-keys :test key-test)
                        :do (push k checked-keys)
                        :and :unless (< item v)
                               :append (list k v))))))


;;--------------------------------------------------------------------
;; remove-if+
;;--------------------------------------------------------------------

(test ?remove-if+.error
  (signals type-error (remove-if+ 'identity :foo))
  (signals type-error (remove-if+ 'identity '(:foo :bar :baz)))
  (signals type-error (remove-if+  42 '()))
  (signals type-error (remove-if+ 'identity '() :key #()))
  (signals type-error (remove-if+ 'identity '() :val #()))
  (signals type-error (remove-if+ 'identity '() :key-test #())))


(test ?remove-if+.hash-table
  (for-all ((num-entries (gen-integer :min 0 :max 10)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0 :below num-entries
                    :for v :from 0 :below num-entries
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (item (random (1+ num-entries))))
      (flet ((item= (k v) (declare (ignore k)) (= v item)))

        ;; To remove-if no test fail for num-entries=0.
        (is-true (hash-table-p (remove-if+ #'item= ht)))
      
        (loop :for k :being :the :hash-keys
                :of (remove-if+ #'item= ht) :using (:hash-value v)
              :do (is-= k v))
        (loop :for k :being :the :hash-keys
                :of (remove-if+ #'item= ht :key '1+)
                :using (:hash-value v)
              :do (is-= k v))
        (loop :for k :being :the :hash-keys
                :of (remove-if+ #'item= ht :val '1+)
                :using (:hash-value v)
              :do (is-= k v))
        (loop :for k :being :the :hash-keys
                :of (remove-if+ #'item= ht :count (random (max 1 num-entries)))
                  :using (:hash-value v)
              :do (is-= k v))))))


(test ?remove-if+.alist
  (is-equal (remove-if+ (lambda (k v) (declare (ignore k)) (zerop v))
                        '(("foo" . 0) ("FOO" . 1))
                        :key-test #'string-equal)
            '())
  (is-equal (remove-if+ (lambda (k v) (declare (ignore k)) (zerop v))
                        '(("foo" . 0) ("FOO" . 1))
                        :key-test #'string=)
            '(("FOO" . 1)))
  
  (for-all ((keys (gen-list :elements (gen-integer :min -10 :max 10)))
            (vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let* ((alst (loop :for k :in keys
                       :for v :in vals
                       :collect (cons k v)))
           (normalized (remove-duplicates alst :key #'car :from-end t)))
      (flet ((item=1 (v) (= v item))
             (item=2 (k v) (declare (ignore k)) (= v item)))

        (let ((count (random (max 1 (length alst)))))
          (is (alist= (remove-if+ #'item=2 alst :count count)
                      (remove-if  #'item=1 normalized :key #'cdr :count count))))
        
        (is-equal (remove-if+ #'item=2 alst)
                  (remove-if  #'item=1 normalized :key #'cdr))

        (is-equal (remove-if+ #'item=2 alst :val '1+)
                  (remove-if  #'item=1 normalized :key (lambda (entry) (1+ (cdr entry)))))
        
        (is-equal (remove-if+ #'item=2 alst :key '1+)
                  (remove-if  #'item=1 normalized :key #'cdr))))))


(test ?remove-if+.plist
  (is-equal (remove-if+ (lambda (k v) (declare (ignore k)) (zerop v))
                        '("foo" 0 "FOO" 1)
                        :key-test #'string-equal)
            '())
  (is-equal (remove-if+ (lambda (k v) (declare (ignore k)) (zerop v))
                        '("foo" 0 "FOO" 1)
                        :key-test #'string=)
            '("FOO" 1))

  (is-equal (remove-if+ (lambda (k v) (declare (ignore k)) (= v 42))
                        '(:foo 42 :bar 42 :baz 1 :foo 3) :count 0)
            '(:foo 42 :bar 42 :baz 1 :foo 3))
  (is-equal (remove-if+ (lambda (k v) (declare (ignore k)) (= v 42))
                        '(:foo 42 :bar 42 :baz 1 :foo 3) :count 1)
            '(:bar 42 :baz 1))
  (is-equal (remove-if+ (lambda (k v) (declare (ignore k)) (= v 42))
                        '(:foo 42 :bar 42 :baz 1 :foo 3) :count 10)
            '(:baz 1))
  
  (for-all ((keys (gen-list :elements (gen-integer :min -10 :max 10)))
            (vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((plst (loop :for k :in keys
                      :for v :in vals
                      :append (list k v)))
          (key-test #'equal))
      (flet ((item=1 (v) (= v item))
             (item=2 (k v) (declare (ignore k)) (= v item)))
        
        (is-equal (remove-if+ #'item=2 plst :key-test key-test)
                  (loop :with checked-keys := '()
                        :for k :in keys
                        :for v :in vals
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                          :and :unless (item=1 v)
                                 :append (list k v)))

        (is-equal (remove-if+ #'item=2 plst :key-test key-test :key '1+)
                  (loop :with checked-keys := '()
                        :for k :in keys
                        :for v :in vals
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                          :and :unless (item=1 v)
                                 :append (list k v)))

        (is-equal (remove-if+ #'item=2 plst :key-test key-test :val '1+)
                  (loop :with checked-keys := '()
                        :for k :in keys
                        :for v :in vals
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                          :and :unless (item=1 (1+ v))
                                 :append (list k v)))))))


;;--------------------------------------------------------------------
;; remove-if-not+
;;--------------------------------------------------------------------

(test ?remove-if-not+.error
  (signals type-error (remove-if-not+ 'identity :foo))
  (signals type-error (remove-if-not+ 'identity '(:foo :bar :baz)))
  (signals type-error (remove-if-not+  42 '()))
  (signals type-error (remove-if-not+ 'identity '() :key #()))
  (signals type-error (remove-if-not+ 'identity '() :val #()))
  (signals type-error (remove-if-not+ 'identity '() :key-test #())))


(test ?remove-if-not+.hash-table
  (for-all ((num-entries (gen-integer :min 0 :max 10)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0 :below num-entries
                    :for v :from 0 :below num-entries
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (item (random (1+ num-entries))))
      (flet ((item= (k v) (declare (ignore k)) (= v item)))

       ;; To remove-if-not no test fail for num-entries=0.
       (is-true (hash-table-p (remove-if-not+ #'item= ht)))
      
       (loop :for k :being :the :hash-keys :of (remove-if-not+ #'item= ht) :using (:hash-value v)
             :do (is-= k v))
       (loop :for k :being :the :hash-keys :of (remove-if-not+ #'item= ht :key '1+)
                 :using (:hash-value v)
             :do (is-= k v))
       (loop :for k :being :the :hash-keys :of (remove-if-not+ #'item= ht :val '1+)
               :using (:hash-value v)
             :do (is-= k v))
       (loop :for k :being :the :hash-keys
               :of (remove-if-not+ #'item= ht :count (random (1+ num-entries)))
                 :using (:hash-value v)
             :do (is-= k v))))))


(test ?remove-if-not+.alist
  (is-equal (remove-if-not+ (lambda (k v) (declare (ignore k)) (not (zerop v)))
                            '(("foo" . 0) ("FOO" . 1))
                            :key-test #'string-equal)
            '())
  (is-equal (remove-if-not+ (lambda (k v) (declare (ignore k)) (not (zerop v)))
                            '(("foo" . 0) ("FOO" . 1))
                            :key-test #'string=)
            '(("FOO" . 1)))

  (for-all ((keys (gen-list :elements (gen-integer :min -10 :max 10)))
            (vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let* ((alst (loop :for k :in keys
                       :for v :in vals
                       :collect (cons k v)))
           (normalized (remove-duplicates alst :key #'car :from-end t)))
      (flet ((item=1 (v) (= v item))
             (item=2 (k v) (declare (ignore k)) (= v item)))

        (let ((count (random (max 1 (length alst)))))
          (is (alist= (remove-if-not+ #'item=2 alst :count count)
                      (remove-if-not  #'item=1 normalized :key #'cdr :count count))))
        
        (is-equal (remove-if-not+ #'item=2 alst)
                  (remove-if-not  #'item=1 normalized :key #'cdr))

        (is-equal (remove-if-not+ #'item=2 alst :val '1+)
                  (remove-if-not  #'item=1 normalized :key (lambda (entry) (1+ (cdr entry)))))
        
        (is-equal (remove-if-not+ #'item=2 alst :key '1+)
                  (remove-if-not  #'item=1 normalized :key #'cdr))))))


(test ?remove-if-not+.plist
  (is-equal (remove-if-not+ (lambda (k v) (declare (ignore k)) (not (zerop v)))
                            '("foo" 0 "FOO" 1)
                            :key-test #'string-equal)
            '())
  (is-equal (remove-if-not+ (lambda (k v) (declare (ignore k)) (not (zerop v)))
                            '("foo" 0 "FOO" 1)
                            :key-test #'string=)
            '("FOO" 1))

  (is-equal (remove-if-not+ (lambda (k v) (declare (ignore k)) (/= v 42))
                        '(:foo 42 :bar 42 :baz 1 :foo 3) :count 0)
            '(:foo 42 :bar 42 :baz 1 :foo 3))
  (is-equal (remove-if-not+ (lambda (k v) (declare (ignore k)) (/= v 42))
                        '(:foo 42 :bar 42 :baz 1 :foo 3) :count 1)
            '(:bar 42 :baz 1))
  (is-equal (remove-if-not+ (lambda (k v) (declare (ignore k)) (/= v 42))
                        '(:foo 42 :bar 42 :baz 1 :foo 3) :count 10)
            '(:baz 1))
  
  (for-all ((keys (gen-list :elements (gen-integer :min -10 :max 10)))
            (vals (gen-list :elements (gen-integer :min -10 :max 10)))
            (item (gen-integer :min -10 :max 10)))
    (let ((plst (loop :for k :in keys
                      :for v :in vals
                      :append (list k v)))
          (key-test #'equal))
      (flet ((item=1 (v) (= v item))
             (item=2 (k v) (declare (ignore k)) (= v item)))
        
        (is-equal (remove-if-not+ #'item=2 plst :key-test key-test)
                  (loop :with checked-keys := '()
                        :for k :in keys
                        :for v :in vals
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                          :and :when (item=1 v)
                                 :append (list k v)))

        (is-equal (remove-if-not+ #'item=2 plst :key-test key-test :key '1+)
                  (loop :with checked-keys := '()
                        :for k :in keys
                        :for v :in vals
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                          :and :when (item=1 v)
                                 :append (list k v)))

        (is-equal (remove-if-not+ #'item=2 plst :key-test key-test :val '1+)
                  (loop :with checked-keys := '()
                        :for k :in keys
                        :for v :in vals
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                          :and :when (item=1 (1+ v))
                                 :append (list k v)))))))


;;--------------------------------------------------------------------
;; substitute+
;;--------------------------------------------------------------------

(test ?substitute+.error
  (signals type-error (substitute+ 42 24 :foo))
  (signals type-error (substitute+ 42 24 '() :val #()))
  (signals type-error (substitute+ 42 24 '() :test #()))
  (signals type-error (substitute+ 42 24 '() :key-test #()))
  (signals type-error (substitute+ 42 24 '(:foo :bar :baz))))


(test ?substitute+.hash-table
  (for-all ((num-entries (gen-integer :min 0 :max 10)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0 :below num-entries
                    :for v :from 0 :below num-entries
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (old (random (1+ num-entries)))
          (new (gensym "NEW-")))
      
      ;; To remove no test fail for num-entries=0.
      (is-true (hash-table-p (substitute+ new old ht)))
      
      (loop :for k :being :the :hash-keys
              :of (substitute+ new old ht) :using (:hash-value v)
            :do (if (= k old)
                    (is-eql new v)
                    (is-eql k v)))
      (loop :for k :being :the :hash-keys
              :of (substitute+ new old ht :val '1+) :using (:hash-value v)
            :do (if (= (1+ k) old)
                    (is-eql new v)
                    (is-eql k v)))

      (loop :for k :being :the :hash-keys
              :of (substitute+ new old ht :count 0)
              :using (:hash-value v)
            :do (is-eql k v))
      
      (loop :for k :being :the :hash-keys
              :of (substitute+ new old ht :count 1)
              :using (:hash-value v)
            :do (if (= k old)
                    (is-eql new v)
                    (is-eql k v)))
      
      (loop :for k :being :the :hash-keys
              :of (substitute+ new old ht :count most-positive-fixnum
                                          :test (constantly T))
              :using (:hash-value v)
            :do (is-eql new v))
      
      (loop :for k :being :the :hash-keys
              :of (substitute+ new old ht :test (lambda (x y) (< x y)))
                :using (:hash-value v)
            :do (if (< old k)
                    (is-eql new v)
                    (is-eql k v))))))


(test ?substitute+.alist
  (is-equal (substitute+ 42 1 '(("foo" . 0) ("FOO" . 1))
                         :key-test #'string-equal)
            '(("foo" . 0)))
  (is-equal (substitute+ 42 1 '(("foo" . 0) ("FOO" . 1))
                         :key-test #'string=)
            '(("foo" . 0) ("FOO" . 42)))

  (is-equal (substitute+ 0 42 '((:foo . 42) (:bar . 42) (:baz . 1) (:foo . 3))
                         :count 0)
            '((:foo . 42) (:bar . 42) (:baz . 1) (:foo . 3)))
  (is-equal (substitute+ 0 42 '((:foo . 42) (:bar . 42) (:baz . 1) (:foo . 3))
                         :count 1)
            '((:foo . 0) (:bar . 42) (:baz . 1)))
  (is-equal (substitute+ 0 42 '((:foo . 42) (:bar . 42) (:baz . 1) (:foo . 3))
                         :count 10)
            '((:foo . 0) (:bar . 0) (:baz . 1)))
  
  (for-all ((keys (gen-list :elements (gen-integer :min -10 :max 10)))
            (vals (gen-list :elements (gen-integer :min -10 :max 10))))
    (let* ((alst (loop :for k :in keys
                       :for v :in vals
                       :collect (cons k v)))
           (old (random (1+ (min (length keys) (length vals)))))
           (new (gensym "NEW-"))
           (normalized (remove-duplicates alst :key #'car :from-end t))
           (test #'eql))

      (is-equal (substitute+ new old alst)
                (loop :for (k . v) :in normalized
                      :collect (if (funcall test old v)
                                   (cons k new)
                                   (cons k v))))

      (is-equal (substitute+ new old alst :val '1+)
                (loop :for (k . v) :in normalized
                      :collect (if (funcall test old (1+ v))
                                   (cons k new)
                                   (cons k v))))

      (is-equal (substitute+ new old alst :test (lambda (x y) (< x y)))
                (loop :for (k . v) :in normalized
                      :collect (if (< old v)
                                   (cons k new)
                                   (cons k v)))))))


(test ?substitute+.plist
  (is-equal (substitute+ 42 1 '("foo" 0 "FOO" 1)
                         :key-test #'string-equal)
            '("foo" 0))
  (is-equal (substitute+ 42 1 '("foo" 0 "FOO" 1)
                         :key-test #'string=)
            '("foo" 0 "FOO" 42))

  (is-equal (substitute+ 0 42 '(:foo 42 :bar 42 :baz 1 :foo 3)
                         :count 0)
            '(:foo 42 :bar 42 :baz 1 :foo 3))
  (is-equal (substitute+ 0 42 '(:foo 42 :bar 42 :baz 1 :foo 3)
                         :count 1)
            '(:foo 0 :bar 42 :baz 1))
  (is-equal (substitute+ 0 42 '(:foo 42 :bar 42 :baz 1 :foo 3)
                         :count 10)
            '(:foo 0 :bar 0 :baz 1))
  
  (for-all ((keys (gen-list :elements (gen-integer :min -10 :max 10)))
            (vals (gen-list :elements (gen-integer :min -10 :max 10))))
    (let* ((plst (loop :for k :in keys
                       :for v :in vals
                       :append (list k v)))
           (old (random (1+ (min (length keys) (length vals)))))
           (new (gensym "NEW-"))
           (test (lambda (k v) (declare (ignore k)) (eql v old)))
           (key-test #'equal))
      
      (is-equal (substitute+ new old plst :key-test key-test)
                (loop :with checked-keys := '()
                      :for k :in keys
                      :for v :in vals
                      :unless (member k checked-keys :test key-test)
                        :do (push k checked-keys)
                        :and :if (funcall test old v)
                               :append (list k new) :else :append (list k v)))

      (is-equal (substitute+ new old plst :key-test key-test :val '1+)
                (loop :with checked-keys := '()
                      :for k :in keys
                      :for v :in vals
                      :unless (member k checked-keys :test key-test)
                        :do (push k checked-keys)
                        :and :if (funcall test old (1+ v))
                               :append (list k new) :else :append (list k v)))

      (is-equal (substitute+ new old plst :key-test key-test
                                          :test (lambda (x y) (< x y)))
                (loop :with checked-keys := '()
                      :for k :in keys
                      :for v :in vals
                      :unless (member k checked-keys :test key-test)
                        :do (push k checked-keys)
                        :and :if (< old v)
                               :append (list k new) :else :append (list k v))))))


;;--------------------------------------------------------------------
;; substitute-if+
;;--------------------------------------------------------------------

(test ?substitute-if+.error
  (signals type-error (substitute-if+ 42 (constantly t) :foo))
  (signals type-error (substitute-if+ 42 (constantly t) '() :val #()))
  (signals type-error (substitute-if+ 42 (constantly t) '() :key #()))
  (signals type-error (substitute-if+ 42 (constantly t) '() :key-test #()))
  (signals type-error (substitute-if+ 42 (constantly t) '(:foo :bar :baz))))


(test ?substitute-if+.hash-table
  (for-all ((num-entries (gen-integer :min 0 :max 10)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0 :below num-entries
                    :for v :from 0 :below num-entries
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (old (random (1+ num-entries)))
          (new (gensym "NEW-")))
      (flet ((old= (k v) (declare (ignore k)) (= old v))
             (old< (k v) (declare (ignore k)) (< old v)))
        ;; To remove no test fail for num-entries=0.
        (is-true (hash-table-p (substitute-if+ new #'old= ht)))
      
        (loop :for k :being :the :hash-keys
                :of (substitute-if+ new #'old= ht) :using (:hash-value v)
              :do (if (= k old)
                      (is-eql new v)
                      (is-eql k v)))

        (loop :for k :being :the :hash-keys
                :of (substitute-if+ new #'old= ht :val '1+) :using (:hash-value v)
              :do (if (= (1+ k) old)
                      (is-eql new v)
                      (is-eql k v)))

        (loop :for k :being :the :hash-keys
                :of (substitute-if+ new #'old= ht :count 0)
                  :using (:hash-value v)
              :do (is-eql k v))
      
        (loop :for k :being :the :hash-keys
                :of (substitute-if+ new #'old= ht :count 1)
                  :using (:hash-value v)
              :do (if (= k old)
                      (is-eql new v)
                      (is-eql k v)))
      
        (loop :for v :being :the :hash-values
                :of (substitute-if+ new (constantly T) ht :count most-positive-fixnum)
              :do (is-eql new v))
      
        (loop :for k :being :the :hash-keys
                :of (substitute-if+ new #'old< ht)
                  :using (:hash-value v)
              :do (if (< old k)
                      (is-eql new v)
                      (is-eql k v)))))))


(test ?substitute-if+.alist
  (is-equal (substitute-if+ 42 (lambda (k v) (declare (ignore k)) (= v 1))
                            '(("foo" . 0) ("FOO" . 1))
                            :key-test #'string-equal)
            '(("foo" . 0)))
  (is-equal (substitute-if+ 42 (lambda (k v) (declare (ignore k)) (= v 1))
                            '(("foo" . 0) ("FOO" . 1))
                            :key-test #'string=)
            '(("foo" . 0) ("FOO" . 42)))

  (is-equal (substitute-if+ 42 (lambda (k v) (declare (ignore k)) (= v 0))
                            '((:foo . 0) (:bar . 0) (:baz . 1) (:foo . 3))
                            :count 0)
            '((:foo . 0) (:bar . 0) (:baz . 1) (:foo . 3)))
  (is-equal (substitute-if+ 42 (lambda (k v) (declare (ignore k)) (= v 0))
                            '((:foo . 0) (:bar . 0) (:baz . 1) (:foo . 3))
                            :count 1)
            '((:foo . 42) (:bar . 0) (:baz . 1)))
  (is-equal (substitute-if+ 42 (lambda (k v) (declare (ignore k)) (= v 0))
                            '((:foo . 0) (:bar . 0) (:baz . 1) (:foo . 3))
                            :count 10)
            '((:foo . 42) (:bar . 42) (:baz . 1)))
  
  (for-all ((keys (gen-list :elements (gen-integer :min -10 :max 10)))
            (vals (gen-list :elements (gen-integer :min -10 :max 10))))
    (let* ((alst (loop :for k :in keys
                       :for v :in vals
                       :collect (cons k v)))
           (old (random (1+ (min (length keys) (length vals)))))
           (new (gensym "NEW-"))
           (normalized (remove-duplicates alst :key #'car :from-end t)))
      (flet ((old= (k v) (declare (ignore k)) (= old v))
             (old< (k v) (declare (ignore k)) (< old v)))
        (is-equal (substitute-if+ new #'old= alst)
                  (loop :for (k . v) :in normalized
                        :collect (if (= old v)
                                     (cons k new)
                                     (cons k v))))

        (is-equal (substitute-if+ new #'old= alst :val '1+)
                  (loop :for (k . v) :in normalized
                        :collect (if (= old (1+ v))
                                     (cons k new)
                                     (cons k v))))

        (is-equal (substitute-if+ new #'old< alst)
                  (loop :for (k . v) :in normalized
                        :collect (if (< old v)
                                     (cons k new)
                                     (cons k v))))))))


(test ?substitute-if+.plist
  (is-equal (substitute-if+ 42 (lambda (k v) (declare (ignore k)) (= v 1))
                            '("foo" 0 "FOO" 1)
                            :key-test #'string-equal)
            '("foo" 0))
  (is-equal (substitute-if+ 42 (lambda (k v) (declare (ignore k)) (= v 1))
                            '("foo" 0 "FOO" 1)
                            :key-test #'string=)
            '("foo" 0 "FOO" 42))

  (is-equal (substitute-if+ 42 (lambda (k v) (declare (ignore k)) (= v 0))
                            '(:foo 0 :bar 0 :baz 1 :foo 3)
                            :count 0)
            '(:foo 0 :bar 0 :baz 1 :foo 3))
  (is-equal (substitute-if+ 42 (lambda (k v) (declare (ignore k)) (= v 0))
                            '(:foo 0 :bar 0 :baz 1 :foo 3)
                            :count 1)
            '(:foo 42 :bar 0 :baz 1))
  (is-equal (substitute-if+ 42 (lambda (k v) (declare (ignore k)) (= v 0))
                            '(:foo 0 :bar 0 :baz 1 :foo 3)
                            :count 10)
            '(:foo 42 :bar 42 :baz 1))
  
  (for-all ((keys (gen-list :elements (gen-integer :min -10 :max 10)))
            (vals (gen-list :elements (gen-integer :min -10 :max 10))))
    (let* ((plst (loop :for k :in keys
                       :for v :in vals
                       :append (list k v)))
           (old (random (1+ (min (length keys) (length vals)))))
           (new (gensym "NEW-"))
           (key-test #'equal))
      (flet ((old= (k v) (declare (ignore k)) (= old v))
             (old< (k v) (declare (ignore k)) (< old v)))
        
        (is-equal (substitute-if+ new #'old= plst :key-test key-test)
                  (loop :with checked-keys := '()
                        :for k :in keys
                        :for v :in vals
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                          :and :if (= old v)
                                 :append (list k new) :else :append (list k v)))

        (is-equal (substitute-if+ new #'old= plst :key-test key-test :val '1+)
                  (loop :with checked-keys := '()
                        :for k :in keys
                        :for v :in vals
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                          :and :if (= old (1+ v))
                                 :append (list k new) :else :append (list k v)))

        (is-equal (substitute-if+ new #'old< plst :key-test key-test)
                  (loop :with checked-keys := '()
                        :for k :in keys
                        :for v :in vals
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                          :and :if (< old v)
                                 :append (list k new) :else :append (list k v)))))))


;;--------------------------------------------------------------------
;; substitute-if-not+
;;--------------------------------------------------------------------

(test ?substitute-if-not+.error
  (signals type-error (substitute-if-not+ 42 (constantly t) :foo))
  (signals type-error (substitute-if-not+ 42 (constantly t) '() :val #()))
  (signals type-error (substitute-if-not+ 42 (constantly t) '() :key #()))
  (signals type-error (substitute-if-not+ 42 (constantly t) '() :key-test #()))
  (signals type-error (substitute-if-not+ 42 (constantly t) '(:foo :bar :baz))))


(test ?substitute-if-not+.hash-table
  (for-all ((num-entries (gen-integer :min 0 :max 10)))
    (let ((ht (loop :with h := (make-hash-table :test 'equal)
                    :for k :from 0 :below num-entries
                    :for v :from 0 :below num-entries
                    :do (setf (gethash k h) v)
                    :finally (return h)))
          (old (random (1+ num-entries)))
          (new (gensym "NEW-")))
      (flet ((old/= (k v) (declare (ignore k)) (/= old v))
             (old<  (k v) (declare (ignore k)) (< old v)))
        ;; To remove no test fail for num-entries=0.
        (is-true (hash-table-p (substitute-if-not+ new #'old/= ht)))
      
        (loop :for k :being :the :hash-keys
                :of (substitute-if-not+ new #'old/= ht) :using (:hash-value v)
              :do (if (= k old)
                      (is-eql new v)
                      (is-eql k v)))

        (loop :for k :being :the :hash-keys
                :of (substitute-if-not+ new #'old/= ht :val '1+) :using (:hash-value v)
              :do (if (= (1+ k) old)
                      (is-eql new v)
                      (is-eql k v)))

        (loop :for k :being :the :hash-keys
                :of (substitute-if-not+ new #'old/= ht :count 0)
                  :using (:hash-value v)
              :do (is-eql k v))
      
        (loop :for k :being :the :hash-keys
                :of (substitute-if-not+ new #'old/= ht :count 1)
                  :using (:hash-value v)
              :do (if (= k old)
                      (is-eql new v)
                      (is-eql k v)))
      
        (loop :for v :being :the :hash-values
                :of (substitute-if-not+ new (constantly nil) ht
                                        :count most-positive-fixnum)
              :do (is-eql new v))
      
        (loop :for k :being :the :hash-keys
                :of (substitute-if-not+ new #'old< ht)
                  :using (:hash-value v)
              :do (if (<= k old)
                      (is-eql new v)
                      (is-eql k v)))))))


(test ?substitute-if-not+.alist
  (is-equal (substitute-if-not+ 42 (lambda (k v) (declare (ignore k)) (/= v 1))
                                '(("foo" . 0) ("FOO" . 1))
                                :key-test #'string-equal)
            '(("foo" . 0)))
  (is-equal (substitute-if-not+ 42 (lambda (k v) (declare (ignore k)) (/= v 1))
                                '(("foo" . 0) ("FOO" . 1))
                                :key-test #'string=)
            '(("foo" . 0) ("FOO" . 42)))

  (is-equal (substitute-if-not+ 42 (lambda (k v) (declare (ignore k)) (= v 1))
                                '((:foo . 0) (:bar . 0) (:baz . 1) (:foo . 3))
                                :count 0)
            '((:foo . 0) (:bar . 0) (:baz . 1) (:foo . 3)))
  (is-equal (substitute-if-not+ 42 (lambda (k v) (declare (ignore k)) (= v 1))
                                '((:foo . 0) (:bar . 0) (:baz . 1) (:foo . 3))
                                :count 1)
            '((:foo . 42) (:bar . 0) (:baz . 1)))
  (is-equal (substitute-if-not+ 42 (lambda (k v) (declare (ignore k)) (= v 1))
                                '((:foo . 0) (:bar . 0) (:baz . 1) (:foo . 3))
                                :count 10)
            '((:foo . 42) (:bar . 42) (:baz . 1)))
  
  (for-all ((keys (gen-list :elements (gen-integer :min -10 :max 10)))
            (vals (gen-list :elements (gen-integer :min -10 :max 10))))
    (let* ((alst (loop :for k :in keys
                       :for v :in vals
                       :collect (cons k v)))
           (old (random (1+ (min (length keys) (length vals)))))
           (new (gensym "NEW-"))
           (normalized (remove-duplicates alst :key #'car :from-end t)))
      (flet ((old/= (k v) (declare (ignore k)) (/= old v))
             (old<  (k v) (declare (ignore k)) (< old v)))
        (is-equal (substitute-if-not+ new #'old/= alst)
                  (loop :for (k . v) :in normalized
                        :collect (if (= old v)
                                     (cons k new)
                                     (cons k v))))

        (is-equal (substitute-if-not+ new #'old/= alst :val '1+)
                  (loop :for (k . v) :in normalized
                        :collect (if (= old (1+ v))
                                     (cons k new)
                                     (cons k v))))

        (is-equal (substitute-if-not+ new #'old< alst)
                  (loop :for (k . v) :in normalized
                        :collect (if (<= v old)
                                     (cons k new)
                                     (cons k v))))))))


(test ?substitute-if-not+.plist
  (is-equal (substitute-if-not+ 42 (lambda (k v) (declare (ignore k)) (/= v 1))
                                '("foo" 0 "FOO" 1)
                                :key-test #'string-equal)
            '("foo" 0))

  (is-equal (substitute-if-not+ 42 (lambda (k v) (declare (ignore k)) (/= v 1))
                                '("foo" 0 "FOO" 1)
                                :key-test #'string=)
            '("foo" 0 "FOO" 42))

  (is-equal (substitute-if-not+ 42 (lambda (k v) (declare (ignore k)) (= v 1))
                                '(:foo 0 :bar 0 :baz 1 :foo 3)
                                :count 0)
            '(:foo 0 :bar 0 :baz 1 :foo 3))
  (is-equal (substitute-if-not+ 42 (lambda (k v) (declare (ignore k)) (= v 1))
                                '(:foo 0 :bar 0 :baz 1 :foo 3)
                                :count 1)
            '(:foo 42 :bar 0 :baz 1))
  (is-equal (substitute-if-not+ 42 (lambda (k v) (declare (ignore k)) (= v 1))
                                '(:foo 0 :bar 0 :baz 1 :foo 3)
                                :count 10)
            '(:foo 42 :bar 42 :baz 1))

  
  (for-all ((keys (gen-list :elements (gen-integer :min -10 :max 10)))
            (vals (gen-list :elements (gen-integer :min -10 :max 10))))
    (let* ((plst (loop :for k :in keys
                       :for v :in vals
                       :append (list k v)))
           (old (random (1+ (min (length keys) (length vals)))))
           (new (gensym "NEW-"))
           (key-test #'equal))
      (flet ((old/= (k v) (declare (ignore k)) (/= old v))
             (old>= (k v) (declare (ignore k)) (>= old v)))
        
        (is-equal (substitute-if-not+ new #'old/= plst :key-test key-test)
                  (loop :with checked-keys := '()
                        :for k :in keys
                        :for v :in vals
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                          :and :if (= old v)
                                 :append (list k new) :else :append (list k v)))

        (is-equal (substitute-if-not+ new #'old/= plst :key-test key-test :val '1+)
                  (loop :with checked-keys := '()
                        :for k :in keys
                        :for v :in vals
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                          :and :if (= old (1+ v))
                                 :append (list k new) :else :append (list k v)))

        (is-equal (substitute-if-not+ new #'old>= plst :key-test key-test)
                  (loop :with checked-keys := '()
                        :for k :in keys
                        :for v :in vals
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                          :and :if (< old v)
                                 :append (list k new) :else :append (list k v)))))))


;;====================================================================
