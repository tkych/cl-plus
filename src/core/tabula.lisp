;;;; cl-plus/src/core/tabula.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;; TODO
;; ----
;;  * map+
;;  * (subtypep 'plist 'plist+) => NIL, NIL
;;    (plistp  '("foo" 42 "bar" 3)) => NIL
;;    (plistp+ '("foo" 42 "bar" 3)) => T
;;  * OPTIMIZE: checked-keys vs normalize-[a|b]list.
;;  * OPTIMIZE: compiler-macro

;;====================================================================
;; Tabula
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.core.tabula
  (:documentation "
Tabula
======

    tabula := (or plist+ alist hash-table)
    plist+ := (and proper-list (satisfies even-length-p))
    plist  := (and proper-list (satisfies even-length-p) (satisfies eql-key-p))
    alist  := (and proper-list (satisfies all-cons-p))


TABULA is a collection of pair(key, object). TABULA is an abstraction
from PLIST, ALIST and HASH-TABLE as if SEQUENCE is an abstraction from
LIST and VECTOR. PLIST+ is a list whose length is even. Roughlly
speaking, PLIST+ is a PLIST whose keys are not necessarily symbol.
PLIST, PLIST+ and ALIST may have duplicate keys.  \"duplicate\" is
depends on equality and equality is supported by key-test (like
hash-table-test). key-test is supplied by user or by default (i.e. equal)
when TABULA is called by tabula-functions.


Functions
---------

 - clear-tabula
 - erase-entry
 - emptyp+
 - size+
 - ref+
 - add+
 - keys
 - vals
 - to-hash
 - to-alist
 - to-plist

 - map+
 - reduce+
 - every+
 - notevery+
 - some+
 - notany+
 - count+
 - count-if+
 - count-if-not+
 - find+
 - find-if+
 - find-if-not+
 - position+
 - position-if+
 - position-if-not+
 - remove+
 - remove-if+
 - remove-if-not+
 - substitute+
 - substitute-if+
 - substitute-if-not+

Macros
------

 - dotab
 - dotab2

")
  (:nicknames #:cl+tabula)
  (:export #:tabula
           #:tabulap
           #:dotab
           #:dotab2
           #:clear-tabula
           #:erase-entry
           #:tabula=
           #:add+
           #:size+
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
           #:substitute+ #:substitute-if+ #:substitute-if-not+)
  (:use #:cl)
  (:import-from #:alexandria
                #:once-only
                #:with-gensyms
                #:ensure-function
                #:if-let
                #:when-let
                #:mappend
                #:copy-hash-table
                #:alist-plist
                #:plist-alist
                #:plist-hash-table
                #:hash-table-plist
                #:hash-table-alist)
  (:import-from #:cl-plus.src.dev-util
                #:copy-empty-hash-table)
  (:import-from #:cl-plus.src.core.hash-table
                #:hash-table=
                #:dohash)
  (:import-from #:cl-plus.src.core.alist
                #:alist
                #:alistp
                #:alist=
                #:normalize-alist
                #:doalist)
  (:import-from #:cl-plus.src.core.plist
                #:plist
                #:plistp
                #:plist+
                #:plistp+
                #:plist=
                #:normalize-plist
                #:doplist))

(in-package :cl-plus.src.core.tabula)

;;--------------------------------------------------------------------
;; type
;;--------------------------------------------------------------------

(deftype tabula () `(or hash-table alist plist+))

(declaim (inline tabulap))
(defun tabulap (x) (typep x 'tabula))


;;--------------------------------------------------------------------
;; dotab
;;--------------------------------------------------------------------

(defmacro dotab ((val-var tabula &optional result) &body body)
  (once-only (tabula)
    (with-gensyms (key-var)
      `(progn
         (check-type ,tabula tabula)
         (etypecase ,tabula
           (hash-table
            (dohash (,key-var ,val-var ,tabula ,result)
              (declare (ignore ,key-var))
              ,@body))

           (alist
            (doalist (,key-var ,val-var ,tabula ,result)
              (declare (ignore ,key-var))
              ,@body))
           
           (plist+
            (doplist (,key-var ,val-var ,tabula ,result)
              (declare (ignore ,key-var))
              ,@body)))))))

(setf (documentation 'dotab 'function) "
DOTAB (val tabula &optional result) &body body => result
")


;;--------------------------------------------------------------------
;; dotab2
;;--------------------------------------------------------------------

(defmacro dotab2 ((key-var val-var tabula &optional result) &body body)
  (once-only (tabula)
    `(progn
       (check-type ,tabula tabula)
       (etypecase ,tabula
         (hash-table
          (dohash (,key-var ,val-var ,tabula ,result)
            ,@body))

         (alist
          (doalist (,key-var ,val-var ,tabula ,result)
            ,@body))
         
         (plist+
          (doplist (,key-var ,val-var ,tabula ,result)
            ,@body))))))

(setf (documentation 'dotab2 'function) "
DOTAB2 (key val tabula &optional result) &body body => result
")


;;--------------------------------------------------------------------
;; tabula=
;;--------------------------------------------------------------------

(declaim (inline lookup))
(defun lookup (key plist key-test)
  (loop :for (k v . nil) :on plist :by #'cddr
        :when (funcall key-test key k)
          :return (list k v)))

(defun tabula= (tabula1 tabula2 &key (key-test #'equal) (val-test #'eql))
  (check-type key-test (or function symbol))
  (check-type val-test (or function symbol))
  (etypecase tabula1
    (hash-table
     (etypecase tabula2
       (hash-table
        (hash-table= tabula1 tabula2 :key-test key-test
                                     :val-test val-test))
       (alist
        (and (eql (ensure-function key-test)
                  (ensure-function (hash-table-test tabula1)))
             (loop :with checked-keys := '()
                   :for (k2 . v2) :in tabula2
                   :unless (member k2 checked-keys :test key-test)
                     :do (push k2 checked-keys)
                         (multiple-value-bind (v1 presentp) (gethash k2 tabula1)
                           (unless (and presentp
                                        (funcall val-test v1 v2))
                             (return NIL)))
                   :finally (return T))
             (with-hash-table-iterator (get-next-entry1 tabula1)
               (loop
                 (multiple-value-bind (more? k1 v1) (get-next-entry1)
                   (unless more? (return T))
                   (let ((entry2 (assoc k1 tabula2 :test key-test)))
                     (unless (and entry2
                                  (funcall val-test v1 (cdr entry2)))
                       (return NIL))))))))
       
       (plist+
        (and (eql (ensure-function key-test)
                  (ensure-function (hash-table-test tabula1)))
             (loop :with checked-keys := '()
                   :for (k2 v2 . nil) :on tabula2 :by #'cddr
                   :for num-entries2 :from 1
                   :unless (member k2 checked-keys :test key-test)
                     :do (push k2 checked-keys)
                         (multiple-value-bind (v1 presentp) (gethash k2 tabula1)
                           (unless (and presentp
                                        (funcall val-test v1 v2))
                             (return NIL)))
                   :finally (return T))
             (with-hash-table-iterator (get-next-entry1 tabula1)
               (loop
                 (multiple-value-bind (more? k1 v1) (get-next-entry1)
                   (unless more? (return T))
                   (let ((entry2 (lookup k1 tabula2 key-test)))
                     (unless (and entry2
                                  (funcall val-test v1 (second entry2)))
                       (return NIL))))))))))

    (alist
     (etypecase tabula2
       (hash-table
        (tabula= tabula2 tabula1 :key-test key-test
                                 :val-test val-test))
       (alist
        (alist=  tabula1 tabula2 :key-test key-test
                                 :val-test val-test))
       (plist+
        (and 
         (loop :with checked-keys := '()
               :for (k1 . v1) :in tabula1
               :unless (member k1 checked-keys :test key-test)
                 :do (push k1 checked-keys)
                     (let ((entry2 (lookup k1 tabula2 key-test)))
                       (unless (and entry2
                                    (funcall val-test v1 (second entry2)))
                         (return NIL)))
               :finally (return T))
         (loop :with checked-keys := '()
               :for (k2 v2 . nil) :on tabula2 :by #'cddr
               :unless (member k2 checked-keys :test key-test)
                 :do (push k2 checked-keys)
                     (let ((entry1 (assoc k2 tabula1 :test key-test)))
                       (unless (and entry1
                                    (funcall val-test (cdr entry1) v2))
                         (return NIL)))
               :finally (return T))))))
    
    (plist+
     (etypecase tabula2
       (hash-table
        (tabula= tabula2 tabula1 :key-test key-test
                                 :val-test val-test))
       (alist
        (tabula= tabula2 tabula1 :key-test key-test
                                 :val-test val-test))
       (plist+
        (plist=  tabula2 tabula1 :key-test key-test
                                 :val-test val-test))))))

(setf (documentation 'tabula= 'function) "
TABULA= tabula1 tabula2 &key (key-test 'equal) (value-test 'eql) => boolean
")


;;--------------------------------------------------------------------
;; size+
;;--------------------------------------------------------------------

(defun size+ (tabula &key (key-test #'equal) ensure-unique-keys)
  (check-type key-test (or function symbol))
  (etypecase tabula
    (hash-table
     (hash-table-count tabula))
    
    (alist
     (if ensure-unique-keys
         (length tabula)
         (loop :with checked-keys := '()
               :for (k . nil) :in tabula
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :count t)))
    
    (plist+
     (if ensure-unique-keys
         (/ (length tabula) 2)
         (loop :with checked-keys := '()
               :for (k . nil) :on tabula :by #'cddr
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :count t)))))

(setf (documentation 'size+ 'function) "
SIZE+ tabula &key (key-test 'equal) ensure-unique-keys => non-negative-integer
")


;;--------------------------------------------------------------------
;; clear-tabula
;;--------------------------------------------------------------------

(define-modify-macro clear-tabula ()
  (lambda (tabula)
    (etypecase tabula
      (hash-table (clrhash tabula))
      (alist     '())
      (plist+    '()))))

(setf (documentation 'clear-tabula 'function) "
CLEAR-TABULA tabula => empty-tabula
Erases all entries from tabula, and returns that empty tabula.
")


;;--------------------------------------------------------------------
;; erase-entry
;;--------------------------------------------------------------------
;; TODO:
;;  * returns (values tabula erase-entrydp)
;;     (lex ht #{:FOO 3 :BAZ 8 :BAR 0})
;;     xxx (erase-entry ht :foo) => #{:BAZ 8 :BAR 0}
;;     ooo (erase-entry ht :foo) => #{:BAZ 8 :BAR 0}, T

(define-modify-macro erase-entry (key &optional (key-test #'equal))
  (lambda (tabula key &optional key-test)
    (check-type key-test (or function symbol))
    (etypecase tabula
      (hash-table
       (remhash key tabula)
       tabula)
      
      (alist
       (loop :for (k . v) :in tabula
             :unless (funcall key-test key k)
               :collect (cons k v)))
      
      (plist+
       (loop :for (k v . nil) :on tabula :by #'cddr
             :unless (funcall key-test key k)
               :append (list k v))))))


(setf (documentation 'erase-entry 'function) "
ERASE-ENTRY tabula key &optional test => (modified-)tabula

Erases the entry whose key is `key' under `key-test', and returns
modified-tabula.  If the entry is not found, returns `tabula' itself.


Notes
-----
  - If there is no entry whose key is `key', returns `tabula' itself.
  - For hash-table, default `test' is hash-table-test.
  - For alist and plist, default `test' is equal.
")


;;--------------------------------------------------------------------
;; ref+
;;--------------------------------------------------------------------

(defun ref+ (tabula key &optional default (key-test #'equal))
  (check-type key-test (or function symbol))
  (etypecase tabula
    (hash-table
     (gethash key tabula default))
    
    (alist
     (if-let (entry (assoc key tabula :test key-test))
       (values (cdr entry) t)
       (values default nil)))
    
    (plist+
     (loop :for (k v . nil) :on tabula :by #'cddr
           :when (funcall key-test key k)
             :return (values v t)
           :finally (return (values default nil))))))

(setf (documentation 'ref+ 'function) "
REF+ tabula key &optional default key-test => value, present-p

Returns the value whose key is the same as `key' under the equivalence
`key-test'. If there is no such entry in `tabula', `default' is returned
as value.  `present-p' is T if an entry is found, otherwise NIL.

Notes
-----
 - If `tabula' is a hash-table, `key-test' is always hash-table-test.
 - If `tabula' is an alist or plist, default `key-test' is 'equal.
")


(define-setf-expander ref+ (tabula key &optional default (test nil test-supplied-p)
                            &environment env)
  (multiple-value-bind
        (tmps vals stores store-form access-form) (get-setf-expansion tabula env)
    (declare (ignore tmps vals stores store-form))
    ;; (repl-utilities:dbgv ('ref+) tmps vals stores store-form access-form)
    (with-gensyms (key-tmp tabula-tmp default-tmp test-tmp store pos test-pred i k)
      (values `(,key-tmp ,tabula-tmp ,default-tmp ,test-tmp)
              `(,key ,tabula ,default ,test)
              `(,store)
              `(progn
                 (etypecase ,tabula-tmp
                   (hash-table
                    (if (or (not ,test-supplied-p)
                            (eql (ensure-function ,test-tmp)
                                 (ensure-function (hash-table-test ,tabula-tmp))))
                        (setf (gethash ,key-tmp ,tabula-tmp ,default-tmp) ,store)
                        (error "~S is supplied for equivalence test, whereas hash-table-test is ~S."
                               ,test (if (functionp ,test-tmp)
                                         (ensure-function (hash-table-test ,tabula-tmp))
                                         (hash-table-test ,tabula-tmp)))))

                   (alist
                    (setf ,access-form
                          (acons ,key-tmp ,store
                                 (remove ,key-tmp ,access-form
                                         :key #'car :test (or ,test-tmp 'equal)))))

                   (plist+
                    (let ((,pos (loop :with ,test-pred := (or ,test-tmp 'equal)
                                      :for ,i :from 0 :by 2
                                      :for ,k :in ,tabula-tmp :by #'cddr
                                      :when (funcall ,test-pred ,k ,key-tmp)
                                        :return ,i)))
                      (if ,pos
                          (setf ,access-form
                                ;; updating entry goes head.
                                (append (list ,key-tmp ,store)
                                        (subseq ,tabula-tmp 0 ,pos)
                                        (subseq ,tabula-tmp (+ ,pos 2))))
                          (setf ,access-form
                                (cons ,key-tmp (cons ,store ,tabula-tmp)))))))

                 ,store)
              `(ref+ ,access-form ,key-tmp ,default-tmp)))))

(setf (documentation '(setf ref+) 'function) "
 (SETF REF+) new-value tabula-place key &optional default test => value

Note
----
 - `tabula' must be place.
    e.g.,
      * (setf (ref+ #{:foo 8} :baz) 7)     => error!
      * (setf (ref+ '((:foo . 8)) :baz) 7) => error!
      * (setf (ref+ '(:foo 8) :baz) 7)     => error!
    cf.
      * (setf (getf '(:foo 8) :baz) 7)     => error!
")


;;--------------------------------------------------------------------
;; add+
;;--------------------------------------------------------------------
;;  &key (key-test 'equal) ensure-unique-keys

(defun add+ (&rest tabulae)
  (labels ((rec (acc tabs)
             (if (null tabs)
                 acc
                 (destructuring-bind (t1 . ts) tabs
                   (dotab2 (k v t1)
                     (setf (ref+ acc k) v))
                   (rec acc ts)))))
    (if (null tabulae)
        nil
        (destructuring-bind (t1 . ts) tabulae
          (etypecase t1
            (hash-table (rec (copy-hash-table t1) ts))
            (alist      (rec (copy-alist t1) ts))
            (t          (rec t1 ts)))))))


;;--------------------------------------------------------------------
;; to-hash
;;--------------------------------------------------------------------

(defun to-hash (tabula &key key val (key-test #'equal)
                         (size nil ssp)
                         (rehash-size nil rssp)
                         (rehash-threshold nil rtsp))
  
  (check-type key              (or symbol function))
  (check-type val              (or symbol function))
  (check-type key-test         (or function symbol))
  (check-type size             (or null (real 0 *)))
  (check-type rehash-size      (or null (integer 1 *) (float (1.0) *)))
  (check-type rehash-threshold (or null (real 0 1)))
  
  (etypecase tabula
    (hash-table
     (let ((result (make-hash-table
                    :test key-test
                    :size (or size (hash-table-size tabula))
                    :rehash-size (or rehash-size (hash-table-rehash-size tabula))
                    :rehash-threshold (or rehash-threshold (hash-table-rehash-threshold tabula)))))
       (if key
           (if val
               (maphash (lambda (k v)
                          (let ((k1 (funcall key k)))
                            (setf (gethash k1 result) (funcall val v))))
                        tabula)
               (maphash (lambda (k v)
                          (let ((k1 (funcall key k)))
                            (setf (gethash k1 result) v)))
                        tabula))
           (if val
               (maphash (lambda (k v)
                          (setf (gethash k result) (funcall val v)))
                        tabula)
               (maphash (lambda (k v)
                          (setf (gethash k result) v))
                        tabula)))
       result))
    
    (alist
     (let ((result (apply #'make-hash-table :test key-test
                          (append
                           (when ssp  (list :size size))
                           (when rssp (list :rehash-size rehash-size))
                           (when rtsp (list :rehash-threshold rehash-threshold))))))
       (if key
           (if val
               (loop :for (k . v) :in (reverse tabula)
                     :for k1 := (funcall key k)
                     :do (setf (gethash k1 result) (funcall val v)))
               (loop :for (k . v) :in (reverse tabula)
                     :for k1 := (funcall key k)
                     :do (setf (gethash k1 result) v)))
           (if val
               (loop :for (k . v) :in (reverse tabula)
                     :do (setf (gethash k result) (funcall val v)))
               (loop :for (k . v) :in (reverse tabula)
                     :do (setf (gethash k result) v))))
       result))
    
    (plist+
     (let ((result (apply #'make-hash-table :test key-test
                          (append
                           (when ssp  (list :size size))
                           (when rssp (list :rehash-size rehash-size))
                           (when rtsp (list :rehash-threshold rehash-threshold))))))
       (if key
           (if val
               (loop :for (v k . nil) :on (reverse tabula) :by #'cddr
                     :for k1 := (funcall key k)
                     :do (setf (gethash k1 result) (funcall val v)))
               (loop :for (v k . nil) :on (reverse tabula) :by #'cddr
                     :for k1 := (funcall key k)
                     :do (setf (gethash k1 result) v)))
           (if val
               (loop :for (v k . nil) :on (reverse tabula) :by #'cddr
                     :do (setf (gethash k result) (funcall val v)))
               (loop :for (v k . nil) :on (reverse tabula) :by #'cddr
                     :do (setf (gethash k result) v))))
       result))))


(setf (documentation 'to-hash 'function) "
TO-HASH tabula &key key val (key-test #'equal) size rehash-size rehash-threshold => result-hash-table
")


;;--------------------------------------------------------------------
;; to-alist
;;--------------------------------------------------------------------

(defun to-alist (tabula &key key val)
  (check-type key (or symbol function))
  (check-type val (or symbol function))
  (etypecase tabula
    (hash-table
     (if key
         (if val
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :collect (cons (funcall key k) (funcall val v)))
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :collect (cons (funcall key k) v)))
         (if val
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :collect (cons k (funcall val v)))
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :collect (cons k v)))))
    
    (alist
     (if key
         (if val
             (loop :for (k . v) :in tabula
                   :collect (cons (funcall key k) (funcall val v)))
             (loop :for (k . v) :in tabula
                   :collect (cons (funcall key k) v)))
         (if val
             (loop :for (k . v) :in tabula
                   :collect (cons k (funcall val v)))
             (copy-alist tabula))))

    (plist+
     (if key
         (if val
             (loop :for (k v . nil) :on tabula :by #'cddr
                   :collect (cons (funcall key k) (funcall val v)))
             (loop :for (k v . nil) :on tabula :by #'cddr
                   :collect (cons (funcall key k) v)))
         (if val
             (loop :for (k v . nil) :on tabula :by #'cddr
                   :collect (cons k (funcall val v)))
             (loop :for (k v . nil) :on tabula :by #'cddr
                   :collect (cons k v)))))))

(setf (documentation 'to-alist 'function) "
TO-ALIST tabula &key key val => result-alist
")


;;--------------------------------------------------------------------
;; to-plist
;;--------------------------------------------------------------------

(defun to-plist (tabula &key key val check-keys-of-type)
  (check-type key (or symbol function))
  (check-type val (or symbol function))
  (flet ((check-key-type (key)
           (unless (typep key check-keys-of-type)
             (error 'type-error :datum key
                                :expected-type check-keys-of-type))))
    (etypecase tabula
      (hash-table
       (if key
           (if val
               (let ((result '()))
                 (with-hash-table-iterator (get-next-entry tabula)
                   (if check-keys-of-type
                       (loop
                         (multiple-value-bind (more? k v) (get-next-entry)
                           (unless more? (return))
                           (let ((k1 (funcall key k)))
                             (check-key-type k1)
                             (push (funcall val v) result)
                             (push k1 result))))
                       (loop
                         (multiple-value-bind (more? k v) (get-next-entry)
                           (unless more? (return))
                           (push (funcall val v) result)
                           (push (funcall key k) result))))
                   result))
               ;; key, no-val:
               (let ((result '()))
                 (with-hash-table-iterator (get-next-entry tabula)
                   (if check-keys-of-type
                       (loop
                         (multiple-value-bind (more? k v) (get-next-entry)
                           (unless more? (return))
                           (let ((k1 (funcall key k)))
                             (check-key-type k1)
                             (push v result)
                             (push k1 result))))
                       (loop
                         (multiple-value-bind (more? k v) (get-next-entry)
                           (unless more? (return))
                           (push v result)
                           (push (funcall key k) result))))
                   result)))
           ;; no-key:
           (if val
               (let ((result '()))
                 (with-hash-table-iterator (get-next-entry tabula)
                   (if check-keys-of-type
                       (loop
                         (multiple-value-bind (more? k v) (get-next-entry)
                           (unless more? (return))
                           (check-key-type k)
                           (push (funcall val v) result)
                           (push k result)))
                       (loop
                         (multiple-value-bind (more? k v) (get-next-entry)
                           (unless more? (return))
                           (push (funcall val v) result)
                           (push k result))))
                   result))
               ;; no-key, no-val:
               (let ((result '()))
                 (with-hash-table-iterator (get-next-entry tabula)
                   (if check-keys-of-type
                       (loop
                         (multiple-value-bind (more? k v) (get-next-entry)
                           (unless more? (return))
                           (check-key-type k)
                           (push v result)
                           (push k result)))
                       (loop
                         (multiple-value-bind (more? k v) (get-next-entry)
                           (unless more? (return))
                           (push v result)
                           (push k result))))
                   result)))))
      
      (alist
       (if key
           (if val
               (let ((result '()))
                 (if check-keys-of-type
                     (loop :for (k . v) :in tabula
                           :for k1 := (funcall key k)
                           :do (check-key-type k1)
                               (push k1 result)
                               (push (funcall val v) result))
                     (loop :for (k . v) :in tabula
                           :for k1 := (funcall key k)
                           :do (push k1 result)
                               (push (funcall val v) result)))
                 (nreverse result))
               ;; key, no-val
               (let ((result '()))
                 (if check-keys-of-type
                     (loop :for (k . v) :in tabula
                           :for k1 := (funcall key k)
                           :do (check-key-type k1)
                               (push k1 result)
                               (push v result))
                     (loop :for (k . v) :in tabula
                           :for k1 := (funcall key k)
                           :do (push k1 result)
                               (push v result)))
                 (nreverse result)))
           ;; no-key:
           (if val
               (let ((result '()))
                 (if check-keys-of-type
                     (loop :for (k . v) :in tabula
                           :do (check-key-type k)
                               (push k result)
                               (push (funcall val v) result))
                     (loop :for (k . v) :in tabula
                           :do (push k result)
                               (push (funcall val v) result)))
                 (nreverse result))
               ;; no-key, no-val
               (let ((result '()))
                 (if check-keys-of-type
                     (loop :for (k . v) :in tabula
                           :do (check-key-type k)
                               (push k result)
                               (push v result))
                     (loop :for (k . v) :in tabula
                           :do (push k result)
                               (push v result)))
                 (nreverse result)))))
      
      (plist+
       (if key
           (if val
               (let ((result '()))
                 (if check-keys-of-type
                     (loop :for (k v . nil) :on tabula :by #'cddr
                           :for k1 := (funcall key k)
                           :do (check-key-type k1)
                               (push k1 result)
                               (push (funcall val v) result))
                     (loop :for (k v . nil) :on tabula :by #'cddr
                           :for k1 := (funcall key k)
                           :do (push k1 result)
                               (push (funcall val v) result)))
                 (nreverse result))
               ;; key, no-val:
               (let ((result '()))
                 (if check-keys-of-type
                     (loop :for (k v . nil) :on tabula :by #'cddr
                           :for k1 := (funcall key k)
                           :do (check-key-type k1)
                               (push k1 result)
                               (push v result))
                     (loop :for (k v . nil) :on tabula :by #'cddr
                           :for k1 := (funcall key k)
                           :do (push k1 result)
                               (push v result)))
                 (nreverse result)))
           ;; no-key:
           (if val
               (let ((result '()))
                 (if check-keys-of-type
                     (loop :for (k v . nil) :on tabula :by #'cddr
                           :do (check-key-type k)
                               (push k result)
                               (push (funcall val v) result))
                     (loop :for (k v . nil) :on tabula :by #'cddr
                           :do (push k result)
                               (push (funcall val v) result)))
                 (nreverse result))
               ;; no-key, no-val:
               (if check-keys-of-type
                   (let ((result '()))
                     (loop :for (k v . nil) :on tabula :by #'cddr
                           :do (check-key-type k)
                               (push k result)
                               (push v result))
                     (nreverse result))
                   (copy-list tabula))))))))


(setf (documentation 'to-plist 'function) "
TO-PLIST tabula &key key val check-keys-of-type => result-plist

Examples
--------
  * (to-plist '(\"FOO\" 0 \"BAR\" 1))
    => ("FOO" 0 "BAR" 1)

  * (to-plist '(\"FOO\" 0 \"BAR\" 1)
              :check-keys-of-type 'symbol)
    => TYPE-ERROR

  * (to-plist '(\"FOO\" 0 \"BAR\" 1)
              :check-keys-of-type 'symbol
              :key ^(intern @ :keyword))
    => (:FOO 0 :BAR 1)
")


;;--------------------------------------------------------------------
;; keys
;;--------------------------------------------------------------------

(defun keys (tabula &key count (when nil when-pred-p) (unless nil unless-pred-p)
                      (key-test #'equal))

  (check-type tabula   tabula)
  (check-type count    (or null (integer 0 *)))
  (check-type when     (or symbol function))
  (check-type unless   (or symbol function))
  (check-type key-test (or function symbol))

  (let ((satisfies (if when-pred-p
                       (if unless-pred-p
                           (lambda (k v) (and (funcall when k v)
                                              (not (funcall unless k v))))
                           when)
                       (if unless-pred-p
                           (ensure-function (complement unless))
                           nil))))
    (if count
        (if satisfies
            (etypecase tabula
              (hash-table
               (loop :until (zerop count)
                     :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                     :when (funcall satisfies k v)
                       :do (decf count)
                       :and :collect k))
            
              (alist
               (loop :with checked-keys := '()
                     :until (zerop count)
                     :for (k . v) :in tabula
                     :unless (member k checked-keys :test key-test)
                       :do (push k checked-keys)
                           (decf count)
                       :and :when (funcall satisfies k v)
                              :collect k))
              (plist+
               (loop :with checked-keys := '()
                     :until (zerop count)
                     :for (k v . nil) :on tabula :by #'cddr
                     :unless (member k checked-keys :test key-test)
                       :do (push k checked-keys)
                           (decf count)
                       :and :when (funcall satisfies k v)
                              :collect k)))
      
            ;; count, no-satisfies:
            (etypecase tabula
              (hash-table
               (loop :until (zerop count)
                     :for k :being :the :hash-keys :of tabula
                     :do (decf count)
                     :collect k))
            
              (alist
               (loop :until (zerop count)
                     :with checked-keys := '()
                     :for (k . nil) :in tabula
                     :unless (member k checked-keys :test key-test)
                       :do (push k checked-keys) (decf count)
                       :and :collect k))
            
              (plist+     
               (loop :until (zerop count)
                     :with checked-keys := '()
                     :for (k . nil) :on tabula :by #'cddr
                     :unless (member k checked-keys :test key-test)
                       :do (push k checked-keys) (decf count)
                       :and :collect k)))))

    ;; no-count:
    (if satisfies
        (etypecase tabula
          (hash-table
           (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                 :when (funcall satisfies k v)
                   :collect k))
          
          (alist
           (loop :with checked-keys := '()
                 :for (k . v) :in tabula
                 :unless (member k checked-keys :test key-test)
                   :do (push k checked-keys)
                   :and :when (funcall satisfies k v)
                          :collect k))
          
          (plist+
           (loop :with checked-keys := '()
                 :for (k v . nil) :on tabula :by #'cddr
                 :unless (member k checked-keys :test key-test)
                   :do (push k checked-keys)
                   :and :when (funcall satisfies k v)
                          :collect k)))
        
        ;; no-count, no-satisfies:
        (etypecase tabula
          (hash-table
           (loop :for k :being :the :hash-keys :of tabula :collect k))
          
          (alist
           (loop :with checked-keys := '()
                 :for (k . nil) :in tabula
                 :unless (member k checked-keys :test key-test)
                   :do (push k checked-keys)
                   :and :collect k))
          
          (plist+     
           (loop :with checked-keys := '()
                 :for (k . nil) :on tabula :by #'cddr
                 :unless (member k checked-keys :test key-test)
                   :do (push k checked-keys)
                   :and :collect k))))))


(setf (documentation 'keys 'function) "
KEYS tabula &key count when unless (key-test 'equal) => keys-list
")


;;--------------------------------------------------------------------
;; vals
;;--------------------------------------------------------------------

(defun vals (tabula &key count (when nil when-pred-p) (unless nil unless-pred-p)
                      (key-test #'equal))
  
  (check-type tabula   tabula)
  (check-type count    (or null (integer 0 *)))
  (check-type when     (or symbol function))
  (check-type unless   (or symbol function))
  (check-type key-test (or function symbol))

  (let ((satisfies (if when-pred-p
                       (if unless-pred-p
                           (lambda (k v) (and (funcall when k v)
                                              (not (funcall unless k v))))
                           when)
                       (if unless-pred-p
                           (ensure-function (complement unless))
                           nil))))
    (if count
        (if satisfies
            (etypecase tabula
              (hash-table
               (loop :until (zerop count)
                     :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                     :when (funcall satisfies k v)
                       :do (decf count)
                       :and :collect v))
              (alist
               (loop :with checked-keys := '()
                     :until (zerop count)
                     :for (k . v) :in tabula
                     :unless (member k checked-keys :test key-test)
                       :do (push k checked-keys)
                           (decf count)
                       :and :when (funcall satisfies k v)
                              :collect v))
              (plist+
               (loop :with checked-keys := '()
                     :until (zerop count)
                     :for (k v . nil) :on tabula :by #'cddr
                     :unless (member k checked-keys :test key-test)
                       :do (push k checked-keys)
                           (decf count)
                       :and :when (funcall satisfies k v)
                              :collect v)))
          
            ;; count, no-satisfies:
            (etypecase tabula
              (hash-table
               (loop :until (zerop count)
                     :for v :being :the :hash-values :of tabula
                     :do (decf count)
                     :collect v))
              (alist
               (loop :until (zerop count)
                     :with checked-keys := '()
                     :for (k . v) :in tabula
                     :unless (member k checked-keys :test key-test)
                       :do (push k checked-keys) (decf count)
                       :and :collect v))
              (plist+     
               (loop :until (zerop count)
                     :with checked-keys := '()
                     :for (k v . nil) :on tabula :by #'cddr
                     :unless (member k checked-keys :test key-test)
                       :do (push k checked-keys) (decf count)
                       :and :collect v))))

        ;; no-count:
        (if satisfies
            (etypecase tabula
              (hash-table
               (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                     :when (funcall satisfies k v)
                       :collect v))
              (alist
               (loop :with checked-keys := '()
                     :for (k . v) :in tabula
                     :unless (member k checked-keys :test key-test)
                       :do (push k checked-keys)
                       :and :when (funcall satisfies k v)
                              :collect v))
              (plist+
               (loop :with checked-keys := '()
                     :for (k v . nil) :on tabula :by #'cddr
                     :unless (member k checked-keys :test key-test)
                       :do (push k checked-keys)
                       :and :when (funcall satisfies k v)
                              :collect v)))
          
            ;; no-count, no-satisfies:
            (etypecase tabula
              (hash-table
               (loop :for v :being :the :hash-values :of tabula :collect v))
              (alist
               (loop :with checked-keys := '()
                     :for (k . v) :in tabula
                     :unless (member k checked-keys :test key-test)
                       :do (push k checked-keys)
                       :and :collect v))
              (plist+     
               (loop :with checked-keys := '()
                     :for (k v . nil) :on tabula :by #'cddr
                     :unless (member k checked-keys :test key-test)
                       :do (push k checked-keys)
                       :and :collect v)))))))


(setf (documentation 'vals 'function) "
VALS tabula &key count when unless (key-test 'equal) => values-list
")


;;--------------------------------------------------------------------
;; map+
;;--------------------------------------------------------------------

(defun map+ (result-type function tabula)
  (check-type function (or symbol function))
  (check-type tabula   tabula)

  (when (or (eq result-type t)
            (eq result-type 'tabula))
    (setf result-type (etypecase tabula
                        (hash-table 'hash-table)
                        (alist      'alist)
                        (plist+     'plist))))

  ;; ADD: copy-empty-object*
  (case result-type
    ((nil)
     (dotab2 (k v tabula)
       (funcall function k v)))

    (hash-table
     (let ((result (if (hash-table-p tabula)
                       (copy-empty-hash-table tabula)
                       (make-hash-table :test 'equal))))
       (dotab2 (k v tabula)
         (setf (gethash k result) (funcall function k v)))
       result))

    ((vector array)
     (let ((result '()))
       (dotab2 (k v tabula)
         (push (funcall function k v) result))
       (coerce (nreverse result) 'vector)))
    
    ((list sequence sequens)
     (let ((result '()))
       (dotab2 (k v tabula)
         (push (funcall function k v) result))
       (nreverse result)))

    ((plist plist+)
     (let ((result '()))
       (dotab2 (k v tabula)
         (push k result)
         (push (funcall function k v) result))
       (nreverse result)))
    
    (alist
     (let ((result '()))
       (dotab2 (k v tabula)
         (push (cons k (funcall function k v)) result))
       (nreverse result)))
    
    ((bit-vector bit-array)
     (let ((result '()))
       (dotab2 (k v tabula)
         (push (funcall function k v) result))
       (coerce (nreverse result) 'bit-vector)))
    
    (t
     (let ((result '()))
       (dotab2 (k v tabula)
         (push (funcall function k v) result))
       (coerce (nreverse result) result-type)))))


(setf (documentation 'map+ 'function) "
MAP+ result-type function tabula => result-tabula

Notes
-----
 * If `result-type' is T, result-type is the type of `first-tabula'.
 * If `result-type' is NIL, returns NIL.
 * Hash-Table iteration order is implementation dependent.

Examples
--------
 * 
")


;;--------------------------------------------------------------------
;; reduce+
;;--------------------------------------------------------------------

(defun reduce+ (function tabula &key key (initial-value nil ivp) (key-test #'equal))
  (check-type function (or symbol function))
  (check-type key      (or symbol function))
  (check-type key-test (or function symbol))
  (etypecase tabula
    (hash-table
     (case (hash-table-count tabula)
       (0 (if ivp
              initial-value
              (funcall function)))
       (1
        (setf key (or key #'identity))
        (if ivp
            (with-hash-table-iterator (get-entry tabula)
              (multiple-value-bind (more? k v) (get-entry)
                (declare (ignore more? k))
                (funcall function initial-value (funcall key v))))
            (with-hash-table-iterator (get-entry tabula)
              (multiple-value-bind (more? k v) (get-entry)
                (declare (ignore more? k))
                (funcall key v)))))
       (t
        (if ivp
            (if key
                (with-hash-table-iterator (get-next-entry tabula)
                  (let ((result initial-value))
                    (loop
                      (multiple-value-bind (more? k v) (get-next-entry)
                        (declare (ignore k))
                        (unless more? (return))
                        (setf result (funcall function result (funcall key v)))))
                    result))
                (with-hash-table-iterator (get-next-entry tabula)
                  (let ((result initial-value))
                    (loop
                      (multiple-value-bind (more? k v) (get-next-entry)
                        (declare (ignore k))
                        (unless more? (return))
                        (setf result (funcall function result v))))
                    result)))
            (if key
                (with-hash-table-iterator (get-next-entry tabula)
                  (let ((result (funcall key (nth-value 2 (get-next-entry)))))
                    (loop
                      (multiple-value-bind (more? k v) (get-next-entry)
                        (declare (ignore k))
                        (unless more? (return))
                        (setf result (funcall function result (funcall key v)))))
                    result))
                (with-hash-table-iterator (get-next-entry tabula)
                  (let ((result (nth-value 2 (get-next-entry))))
                    (loop
                      (multiple-value-bind (more? k v) (get-next-entry)
                        (declare (ignore k))
                        (unless more? (return))
                        (setf result (funcall function result v))))
                    result)))))))

    (alist
     (case (length tabula)
       (0 (if ivp
              initial-value
              (funcall function)))
       (1
        (setf key (or key #'identity))
        (if ivp
            (funcall function initial-value (funcall key (cdar tabula)))
            (funcall function (funcall key (cdar tabula)))))
       (t
        (if ivp
            (if key
                (loop :with result := (funcall key initial-value)
                      :with checked-keys := '()
                      :for (k . v) :in tabula
                      :unless (member k checked-keys :test key-test)
                        :do (push k checked-keys)
                            (setf result (funcall function result (funcall key v)))
                      :finally (return result))
                (loop :with result := initial-value
                      :with checked-keys := '()
                      :for (k . v) :in tabula
                      :unless (member k checked-keys :test key-test)
                        :do (push k checked-keys)
                            (setf result (funcall function result v))
                      :finally (return result)))
            (if key
                (destructuring-bind ((k0 . v0) . rest) tabula
                  (loop :with result := (funcall key v0)
                        :with checked-keys := (list k0)
                        :for (k . v) :in rest
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                              (setf result (funcall function result (funcall key v)))
                        :finally (return result)))
                (destructuring-bind ((k0 . v0) . rest) tabula
                  (loop :with result := v0
                        :with checked-keys := (list k0)
                        :for (k . v) :in rest
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                              (setf result (funcall function result v))
                        :finally (return result))))))))
    
    (plist+
     (case (/ (length tabula) 2)
       (0 (if ivp
              initial-value
              (funcall function)))
       (1
        (setf key (or key #'identity))
        (if ivp
            (funcall function initial-value (funcall key (second tabula)))
            (funcall function (funcall key (second tabula)))))
       (t
        (if ivp
            (if key
                (loop :with result := (funcall key initial-value)
                      :with checked-keys := '()
                      :for (k v . nil) :on tabula :by #'cddr
                      :unless (member k checked-keys :test key-test)
                        :do (push k checked-keys)
                            (setf result (funcall function result (funcall key v)))
                      :finally (return result))
                (loop :with result := initial-value
                      :with checked-keys := '()
                      :for (k v . nil) :on tabula :by #'cddr
                      :unless (member k checked-keys :test key-test)
                        :do (push k checked-keys)
                            (setf result (funcall function result v))
                      :finally (return result)))
            (if key
                (destructuring-bind (k0 v0 . rest) tabula
                  (loop :with result := (funcall key v0)
                        :with checked-keys := (list k0)
                        :for (k v . nil) :on rest :by #'cddr
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                              (setf result (funcall function result (funcall key v)))
                        :finally (return result)))
                (destructuring-bind (k0 v0 . rest) tabula
                  (loop :with result := v0
                        :with checked-keys := (list k0)
                        :for (k v . nil) :on rest :by #'cddr
                        :unless (member k checked-keys :test key-test)
                          :do (push k checked-keys)
                              (setf result (funcall function result v))
                        :finally (return result))))))))))


(setf (documentation 'reduce+ 'function) "
REDUCE+ function tabula &key key initial-value (key-test 'equal) => result

Notes
-----
 - If the `tabula' contains exactly one entry and no `initial-value' is
   given, then the value is returned and `function' is not called.

 - If the `tabula' is empty and an `initial-value' is given,
   then the `initial-value' is returned and `function' is not called.

 - If the `tabula' is empty and no `initial-value' is given, then the
   `function' is called with zero arguments, and REDUCE+ returns
   whatever function does.

References
----------
 - CLHS, Function REDUCE.
 - CLtL2, Section 14.2. Concatenating, Mapping, and Reducing Sequences.
")


;;--------------------------------------------------------------------
;; every+
;;--------------------------------------------------------------------

(defun every+ (predicate tabula &key key val (key-test #'equal))
  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type key-test  (or function symbol))
  (etypecase tabula
    (hash-table
     (if key
         (if val
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :always (funcall predicate (funcall key k) (funcall val v)))
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :always (funcall predicate (funcall key k) v)))
         (if val
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :always (funcall predicate k (funcall val v)))
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :always (funcall predicate k v)))))
    
    (alist
     (if key
         (if val
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :do (unless (member k checked-keys :test key-test)
                         (push k checked-keys)
                         (unless (funcall predicate (funcall key k) (funcall val v))
                           (return NIL)))
                   :finally (return T))
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :do (unless (member k checked-keys :test key-test)
                         (push k checked-keys)
                         (unless (funcall predicate (funcall key k) v)
                           (return NIL)))
                   :finally (return T)))
         (if val
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :do (unless (member k checked-keys :test key-test)
                         (push k checked-keys)
                         (unless (funcall predicate k (funcall val v))
                           (return NIL)))
                   :finally (return T))
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :do (unless (member k checked-keys :test key-test)
                         (push k checked-keys)
                         (unless (funcall predicate k v)
                           (return NIL)))
                   :finally (return T)))))
    
    (plist+
     (if key
         (if val
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :do (unless (member k checked-keys :test key-test)
                         (push k checked-keys)
                         (unless (funcall predicate (funcall key k) (funcall val v))
                           (return NIL)))
                   :finally (return T))
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :do (unless (member k checked-keys :test key-test)
                         (push k checked-keys)
                         (unless (funcall predicate (funcall key k) v)
                           (return NIL)))
                   :finally (return T)))
         (if val
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :do (unless (member k checked-keys :test key-test)
                         (push k checked-keys)
                         (unless (funcall predicate k (funcall val v))
                           (return NIL)))
                   :finally (return T))
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :do (unless (member k checked-keys :test key-test)
                         (push k checked-keys)
                         (unless (funcall predicate k v)
                           (return NIL)))
                   :finally (return T)))))))


(setf (documentation 'every+ 'function) "
EVERY+ predicate tabula &key key val (key-test 'equal) => boolean
")


;;--------------------------------------------------------------------
;; notevery+
;;--------------------------------------------------------------------

(defun notevery+ (predicate tabula &key key val (key-test #'equal))
  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type key-test  (or function symbol))
  (not (every+ predicate tabula :key key :val val :key-test key-test)))


(setf (documentation 'notevery+ 'function) "
NOTEVERY+ predicate tabula &key key val (key-test #'equal) => boolean
")


;;--------------------------------------------------------------------
;; some+
;;--------------------------------------------------------------------

(defun some+ (predicate tabula &key key val (key-test #'equal))
  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type key-test  (or function symbol))
  (etypecase tabula
    (hash-table
     (if key
         (if val
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                     :thereis (funcall predicate (funcall key k) (funcall val v)))
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                     :thereis (funcall predicate (funcall key k) v)))
         (if val
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                     :thereis (funcall predicate k (funcall val v)))
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                     :thereis (funcall predicate k v)))))
    
    (alist
     (if key
         (if val
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate (funcall key k) (funcall val v))
                            :return T
                   :finally (return NIL))
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate (funcall key k) v)
                            :return T
                   :finally (return NIL)))
         (if val
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate k (funcall val v))
                            :return T
                   :finally (return NIL))
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate k v)
                            :return T
                   :finally (return NIL)))))
    
    (plist+
     (if key
         (if val
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate (funcall key k) (funcall val v))
                            :return T
                   :finally (return NIL))
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate (funcall key k) v)
                            :return T
                   :finally (return NIL)))
         (if val
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate k (funcall val v))
                            :return T
                   :finally (return NIL))
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate k v)
                            :return T
                   :finally (return NIL)))))))


(setf (documentation 'some+ 'function) "
SOME+ predicate tabula &optional (key-test 'equal) => boolean
")


;;--------------------------------------------------------------------
;; notany+
;;--------------------------------------------------------------------

(defun notany+ (predicate tabula &key key val (key-test #'equal))
  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type key-test  (or function symbol))
  (not (some+ predicate tabula :key key :val val :key-test key-test)))


(setf (documentation 'notany+ 'function) "
NOTANY+ predicate tabula &key key val (key-test 'equal) => boolean
")


;;--------------------------------------------------------------------
;; count+
;;--------------------------------------------------------------------

(defun count+ (value tabula &key val (test #'eql) (key-test #'equal))
  (check-type val      (or function symbol))
  (check-type test     (or symbol function))
  (check-type key-test (or symbol function))
  
  (etypecase tabula
    (hash-table
     (if val
         (loop :for v :being :the :hash-values :of tabula
               :count (funcall test value (funcall val v)))
         (loop :for v :being :the :hash-values :of tabula
               :count (funcall test value v))))
    
    (alist
     (if val
         (loop :with checked-keys := '()
               :for (k . v) :in tabula
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :count (funcall test value (funcall val v)))
         (loop :with checked-keys := '()
               :for (k . v) :in tabula
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :count (funcall test value v))))
    
    (plist+
     (if val
         (loop :with checked-keys := '()
               :for (k v . nil) :on tabula :by #'cddr
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :count (funcall test value (funcall val v)))
         (loop :with checked-keys := '()
               :for (k v . nil) :on tabula :by #'cddr
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :count (funcall test value v))))))

(setf (documentation 'count+ 'function) "
COUNT+ value tabula &key val (test 'eql) => non-negative-integer
")


;;--------------------------------------------------------------------
;; count-if+
;;--------------------------------------------------------------------

(defun count-if+ (predicate tabula &key key val (key-test #'equal))
  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type key-test  (or function symbol))
  (etypecase tabula
    (hash-table
     (if key
         (if val
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :count (funcall predicate (funcall key k) (funcall val v)))
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :count (funcall predicate (funcall key k) v)))
         (if val
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :count (funcall predicate k (funcall val v)))
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :count (funcall predicate k v)))))
    
    (alist
     (if key
         (if val
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :count (funcall predicate (funcall key k) (funcall val v)))
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :count (funcall predicate (funcall key k) v)))
         (if val
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :count (funcall predicate k (funcall val v)))
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :count (funcall predicate k v)))))
    
    (plist+
     (if key
         (if val
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :count (funcall predicate (funcall key k) (funcall val v)))
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :count (funcall predicate (funcall key k) v)))
         (if val
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :count (funcall predicate k (funcall val v)))
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :count (funcall predicate k v)))))))

(setf (documentation 'count-if+ 'function) "
COUNT-IF+ predicate tabula &key key val (key-test 'equal) => non-negative-integer
")


;;--------------------------------------------------------------------
;; count-if-not+
;;--------------------------------------------------------------------

(defun count-if-not+ (predicate tabula &key key val (key-test #'equal))
  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type key-test  (or function symbol))
  (count-if+ (complement (ensure-function predicate))
             tabula :key key :val val :key-test key-test))

(setf (documentation 'count-if-not+ 'function) "
COUNT-IF-NOT+ predicate tabula &key key val (key-test 'equal) => non-negative-integer
")


;;--------------------------------------------------------------------
;; find+
;;--------------------------------------------------------------------

(defun find+ (value tabula &key val (test 'eql) (key-test #'equal))
  (check-type val      (or symbol function))
  (check-type test     (or symbol function))
  (check-type key-test (or function symbol))
  (etypecase tabula
    (hash-table
     (if val
         (loop :for v :being :the :hash-values :of tabula
               :when (funcall test value (funcall val v))
                 :return v)
         (loop :for v :being :the :hash-values :of tabula
               :when (funcall test value v)
                 :return v)))
    
    (alist
     (if val
         (loop :with checked-keys := '()
               :for (k . v) :in tabula
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :when (funcall test value (funcall val v))
                        :return v)
         (loop :with checked-keys := '()
               :for (k . v) :in tabula
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :when (funcall test value v)
                        :return v)))
    
    (plist+
     (if val
         (loop :with checked-keys := '()
               :for (k v . nil) :on tabula :by #'cddr
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :when (funcall test value (funcall val v))
                        :return v)
         (loop :with checked-keys := '()
               :for (k v . nil) :on tabula :by #'cddr
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :when (funcall test value v)
                        :return v)))))

(setf (documentation 'find+ 'function) "
FIND+ value tabula &key val (test 'eql) => value, key/present-p
NB. (find+ nil '(nil nil)) => nil, nil
")


;;--------------------------------------------------------------------
;; find-if+
;;--------------------------------------------------------------------

(defun find-if+ (predicate tabula &key key val (key-test #'equal))
  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type key-test  (or function symbol))
  (etypecase tabula
    (hash-table
     (if key
         (if val
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall predicate (funcall key k) (funcall val v))
                     :return v)
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall predicate (funcall key k) v)
                     :return v))
         (if val
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall predicate k (funcall val v))
                     :return v)
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall predicate k v)
                     :return v))))
    
    (alist
     (if key
         (if val
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate (funcall key k) (funcall val v))
                            :return v)
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate (funcall key k) v)
                            :return v))
         (if val
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate k (funcall val v))
                            :return v)
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate k v)
                            :return v))))
    
    (plist+
     (if key
         (if val
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate (funcall key k) (funcall val v))
                            :return v)
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate (funcall key k) v)
                            :return v))
         (if val
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate k (funcall val v))
                            :return v)
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate k v)
                            :return v))))))

(setf (documentation 'find-if+ 'function) "
FIND-IF+ predicate tabula &key key val (key-test 'equal) => value
")


;;--------------------------------------------------------------------
;; find-if-not+
;;--------------------------------------------------------------------

(defun find-if-not+ (predicate tabula &key key val (key-test #'equal))
  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type key-test  (or function symbol))
  (find-if+ (complement (ensure-function predicate))
            tabula :key key :val val :key-test key-test))

(setf (documentation 'find-if-not+ 'function) "
FIND-IF-NOT+ predicate tabula &key key val => value, key/present-p
")


;;--------------------------------------------------------------------
;; position+
;;--------------------------------------------------------------------

(defun position+ (value tabula &key val (test #'eql) (key-test #'equal))
  (check-type val      (or symbol function))
  (check-type test     (or function symbol))
  (check-type key-test (or function symbol))
  (etypecase tabula
    (hash-table
     (if val
         (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
               :when (funcall test value (funcall val v))
                 :return k)
         (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
               :when (funcall test value v)
                 :return k)))
    
    (alist
     (if val
         (loop :with checked-keys := '()
               :for (k . v) :in tabula
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :when (funcall test value (funcall val v))
                        :return k)
         (loop :with checked-keys := '()
               :for (k . v) :in tabula
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :when (funcall test value v)
                        :return k)))
    
    (plist+
     (if val
         (loop :with checked-keys := '()
               :for (k v . nil) :on tabula :by #'cddr
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :when (funcall test value (funcall val v))
                        :return k)
         (loop :with checked-keys := '()
               :for (k v . nil) :on tabula :by #'cddr
               :unless (member k checked-keys :test key-test)
                 :do (push k checked-keys)
                 :and :when (funcall test value v)
                        :return k)))))


(setf (documentation 'position+ 'function) "
POSITION+ value tabula &key val (test 'eql) (key-test 'equal)  => key/NIL
")


;;--------------------------------------------------------------------
;; position-if+
;;--------------------------------------------------------------------

(defun position-if+ (predicate tabula &key key val (key-test #'equal))
  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type key-test  (or function symbol))
  (etypecase tabula
    (hash-table
     (if key
         (if val
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall predicate (funcall key k) (funcall val v))
                     :return k)
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall predicate (funcall key k) v)
                     :return k))
         (if val
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall predicate k (funcall val v))
                     :return k)
             (loop :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall predicate k v)
                     :return k))))
    
    (alist
     (if key
         (if val
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate (funcall key k) (funcall val v))
                            :return k)
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate (funcall key k) v)
                            :return k))
         (if val
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate k (funcall val v))
                            :return k)
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate k v)
                            :return k))))
    
    (plist+
     (if key
         (if val
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate (funcall key k) (funcall val v))
                            :return k)
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate (funcall key k) v)
                            :return k))
         (if val
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate k (funcall val v))
                            :return k)
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :when (funcall predicate k v)
                            :return k))))))


(setf (documentation 'position-if+ 'function) "
POSITION-IF+ predicate tabula &key val (key-test 'equal) => key/NIL
")


;;--------------------------------------------------------------------
;; position-if-not+
;;--------------------------------------------------------------------

(defun position-if-not+ (predicate tabula &key key val (key-test #'equal))
  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type key-test  (or function symbol))
  (position-if+ (complement (ensure-function predicate))
                tabula :key key :val val :key-test key-test))

(setf (documentation 'position-if-not+ 'function) "
POSITION-IF-NOT+ predicate tabula &key val (key-test 'equal) => key/NIL
")


;;--------------------------------------------------------------------
;; remove+
;;--------------------------------------------------------------------

(defun remove+ (value tabula &key val (test #'eql) (key-test #'equal) count)
  (check-type val      (or symbol function))
  (check-type test     (or function symbol))
  (check-type key-test (or function symbol))
  (check-type count    (or null (integer 0 *)))
  (etypecase tabula
    (hash-table
     (if count
         (if val
             (loop :with result := (copy-hash-table tabula)
                   :until (zerop count)
                   :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall test value (funcall val v))
                     :do (remhash k result)
                         (decf count)
                   :finally (return result))
             (loop :with result := (copy-hash-table tabula)
                   :until (zerop count)
                   :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall test value v)
                     :do (remhash k result)
                         (decf count)
                   :finally (return result)))
         ;; no-count:
         (if val
             (loop :with result := (copy-hash-table tabula)
                   :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall test value (funcall val v))
                     :do (remhash k result)
                   :finally (return result))
             (loop :with result := (copy-hash-table tabula)
                   :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall test value v)
                     :do (remhash k result)
                   :finally (return result)))))

    (alist
     (if count
         (if val
             (if (zerop count)
                 (copy-alist tabula)
                 ;; MEMO: normalized vs checked-keys.
                 ;; If checked-keys, implematation is too complicated
                 ;; for (remove+ 42 '((:foo . 42) (:foo . 0)) :count 1).
                 (loop :for ((k . v) . rest) :on (normalize-alist tabula key-test)
                       :if (funcall test value (funcall val v))
                         :do (decf count)
                       :else :collect (cons k v) :into keep
                       :until (zerop count)
                       :finally (return (append keep rest))))
             (if (zerop count)
                 (copy-alist tabula)
                 (loop :for ((k . v) . rest) :on (normalize-alist tabula key-test)
                       :if (funcall test value v)
                         :do (decf count)
                       :else :collect (cons k v) :into keep
                       :until (zerop count)
                       :finally (return (append keep rest)))))
         ;; no-count:
         (if val
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :unless (funcall test value (funcall val v))
                            :collect (cons k v))
             (loop :with checked-keys := '()
                   :for (k . v) :in tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :unless (funcall test value v)
                            :collect (cons k v)))))
    
    (plist+
     (if count
         ;; TODO:
         (if val
             (if (zerop count)
                 (copy-list tabula)
                 ;; MEMO: normalized vs checked-keys.
                 ;; If checked-keys, implematation is too complicated
                 ;; for (remove+ 42 '(:foo 42 :foo 0) :count 1).
                 (loop :for (k v . rest) :on (normalize-plist tabula key-test) :by #'cddr
                       :if (funcall test value (funcall val v))
                         :do (decf count)
                       :else :append (list k v) :into keep
                       :until (zerop count)
                       :finally (return (append keep rest))))
             (if (zerop count)
                 (copy-list tabula)
                 (loop :for (k v . rest) :on (normalize-plist tabula key-test) :by #'cddr
                       :if (funcall test value v)
                         :do (decf count)
                       :else :append (list k v) :into keep
                       :until (zerop count)
                       :finally (return (append keep rest)))))
         ;; no-count:
         (if val
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :unless (funcall test value (funcall val v))
                            :append (list k v))
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :unless (funcall test value v)
                            :append (list k v)))))))

(setf (documentation 'remove+ 'function) "
REMOVE+ value tabula &val key (test 'eql) count => new-tabula
")


;;--------------------------------------------------------------------
;; remove-if+
;;--------------------------------------------------------------------

(defun remove-if+ (predicate tabula &key key val count (key-test #'equal))
  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type count     (or null (integer 0 *)))
  (check-type key-test  (or function symbol))
  (etypecase tabula
    (hash-table
     (if count
         (if key
             (if val
                 (loop :with result := (copy-hash-table tabula)
                       :until (zerop count)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate (funcall key k) (funcall val v))
                         :do (remhash k result)
                             (decf count)
                       :finally (return result))
                 (loop :with result := (copy-hash-table tabula)
                       :until (zerop count)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate (funcall key k) v)
                         :do (remhash k result)
                             (decf count)
                       :finally (return result)))
             ;; count, no-key
             (if val
                 (loop :with result := (copy-hash-table tabula)
                       :until (zerop count)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate v (funcall val v))
                         :do (remhash k result)
                             (decf count)
                       :finally (return result))
                 (loop :with result := (copy-hash-table tabula)
                       :until (zerop count)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate k v)
                         :do (remhash k result)
                             (decf count)
                       :finally (return result))))
         ;; no-count:
         (if key
             (if val
                 (loop :with result := (copy-hash-table tabula)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate (funcall key k) (funcall val v))
                         :do (remhash k result)
                       :finally (return result))
                 (loop :with result := (copy-hash-table tabula)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate (funcall key k) v)
                         :do (remhash k result)
                       :finally (return result)))
             ;; no-count, no-key:
             (if val
                 (loop :with result := (copy-hash-table tabula)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate v (funcall val v))
                         :do (remhash k result)
                       :finally (return result))
                 (loop :with result := (copy-hash-table tabula)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate k v)
                         :do (remhash k result)
                       :finally (return result))))))

    (alist
     (if count
         (if (zerop count)
             (copy-alist tabula)
             (if key
                 (if val
                     (loop :for ((k . v) . rest) :on (normalize-alist tabula key-test)
                           :if (funcall predicate (funcall key k) (funcall val v))
                             :do (decf count)
                           :else :collect (cons k v) :into keep
                           :until (zerop count)
                           :finally (return (append keep rest)))
                     (loop :for ((k . v) . rest) :on (normalize-alist tabula key-test)
                           :if (funcall predicate (funcall key k) v)
                             :do (decf count)
                           :else :collect (cons k v) :into keep
                           :until (zerop count)
                           :finally (return (append keep rest))))
                 ;; count, no-key:
                 (if val
                     (loop :for ((k . v) . rest) :on (normalize-alist tabula key-test)
                           :if (funcall predicate k (funcall val v))
                             :do (decf count)
                           :else :collect (cons k v) :into keep
                           :until (zerop count)
                           :finally (return (append keep rest)))
                     (loop :for ((k . v) . rest) :on (normalize-alist tabula key-test)
                           :if (funcall predicate k v)
                             :do (decf count)
                           :else :collect (cons k v) :into keep
                           :until (zerop count)
                           :finally (return (append keep rest))))))
         ;; no-count:
         (if key
             (if val
                 (loop :with checked-keys := '()
                       :for (k . v) :in tabula
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :unless (funcall predicate (funcall key k) (funcall val v))
                                :collect (cons k v))
                 (loop :with checked-keys := '()
                       :for (k . v) :in tabula
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :unless (funcall predicate (funcall key k) v)
                                :collect (cons k v)))
             ;; no-count, no-key
             (if val
                 (loop :with checked-keys := '()
                       :for (k . v) :in tabula
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :unless (funcall predicate k (funcall val v))
                                :collect (cons k v))
                 (loop :with checked-keys := '()
                       :for (k . v) :in tabula
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :unless (funcall predicate k v)
                                :collect (cons k v))))))
    
    (plist+
     (if count
         (if (zerop count)
             (copy-list tabula)
             (if key
                 (if val
                     (loop :for (k v . rest) :on (normalize-plist tabula key-test) :by #'cddr
                           :if (funcall predicate (funcall key k) (funcall val v))
                             :do (decf count)
                           :else :append (list k v) :into keep
                           :until (zerop count)
                           :finally (return (append keep rest)))
                     (loop :for (k v . rest) :on (normalize-plist tabula key-test) :by #'cddr
                           :if (funcall predicate (funcall key k) v)
                             :do (decf count)
                           :else :append (list k v) :into keep
                           :until (zerop count)
                           :finally (return (append keep rest))))
                 ;; count, no-key
                 (if val
                     (loop :for (k v . rest) :on (normalize-plist tabula key-test) :by #'cddr
                           :if (funcall predicate k (funcall val v))
                             :do (decf count)
                           :else :append (list k v) :into keep
                           :until (zerop count)
                           :finally (return (append keep rest)))
                     (loop :for (k v . rest) :on (normalize-plist tabula key-test) :by #'cddr
                           :if (funcall predicate k v)
                             :do (decf count)
                           :else :append (list k v) :into keep
                           :until (zerop count)
                           :finally (return (append keep rest))))))
         ;; no-count:
         (if key
             (if val
                 (loop :with checked-keys := '()
                       :for (k v . nil) :on tabula :by #'cddr
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :unless (funcall predicate (funcall key k) (funcall val v))
                                :append (list k v))
                 (loop :with checked-keys := '()
                       :for (k v . nil) :on tabula :by #'cddr
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :unless (funcall predicate (funcall key k) v)
                                :append (list k v)))
             ;; no-count, no-key
             (if val
                 (loop :with checked-keys := '()
                       :for (k v . nil) :on tabula :by #'cddr
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :unless (funcall predicate k (funcall val v))
                                :append (list k v))
                 (loop :with checked-keys := '()
                       :for (k v . nil) :on tabula :by #'cddr
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :unless (funcall predicate k v)
                                :append (list k v))))))))


(setf (documentation 'remove-if+ 'function) "
REMOVE-IF+ predicate tabula &key key val count (key-test 'equal) => new-tabula
")


;;--------------------------------------------------------------------
;; remove-if-not+
;;--------------------------------------------------------------------

(defun remove-if-not+ (predicate tabula &key key val count (key-test #'equal))
  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type count     (or null (integer 0 *)))
  (check-type key-test  (or function symbol))
  (remove-if+ (complement (ensure-function predicate))
              tabula :key key :val val :key-test key-test :count count))


(setf (documentation 'remove-if-not+ 'function) "
REMOVE-IF-NOT+ predicate tabula &key key val count (key-test 'equal) => new-tabula
")


;;--------------------------------------------------------------------
;; substitute+
;;--------------------------------------------------------------------

(defun substitute+ (new old tabula &key val (test #'eql) (key-test #'equal) count)
  (check-type val      (or symbol function))
  (check-type test     (or function symbol))
  (check-type key-test (or function symbol))
  (check-type count    (or null (integer 0 *)))
  (etypecase tabula
    (hash-table
     (if count
         (if val
             (loop :with result := (copy-hash-table tabula)
                   :until (zerop count)
                   :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall test old (funcall val v))
                     :do (setf (gethash k result) new)
                         (decf count)
                   :finally (return result))
             (loop :with result := (copy-hash-table tabula)
                   :until (zerop count)
                   :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall test old v)
                     :do (setf (gethash k result) new)
                         (decf count)
                   :finally (return result)))
         ;; no-count:
         (if val
             (loop :with result := (copy-hash-table tabula)
                   :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall test old (funcall val v))
                     :do (setf (gethash k result) new)
                   :finally (return result))
             (loop :with result := (copy-hash-table tabula)
                   :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                   :when (funcall test old v)
                     :do (setf (gethash k result) new)
                   :finally (return result)))))

    (alist
     (if count
         (if (zerop count)
             (copy-alist tabula)
             (if val
                 (loop :for ((k . v) . rest) :on (normalize-alist tabula key-test)
                       :if (funcall test old (funcall val v))
                         :do (decf count) :and :collect (cons k new) :into acc
                       :else :collect (cons k v) :into acc
                       :until (zerop count)
                       :finally (return (append acc rest)))
                 (loop :for ((k . v) . rest) :on (normalize-alist tabula key-test)
                       :if (funcall test old v)
                         :do (decf count) :and :collect (cons k new) :into acc
                       :else :collect (cons k v) :into acc
                       :until (zerop count)
                       :finally (return (append acc rest)))))
         ;; no-count:
         (if val
             (loop :with checked-keys := '()
                   :for ((k . v) . nil) :on tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :if (funcall test old (funcall val v))
                            :collect (cons k new) :else :collect (cons k v))
             (loop :with checked-keys := '()
                   :for ((k . v) . nil) :on tabula
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :if (funcall test old v)
                            :collect (cons k new) :else :collect (cons k v)))))
    
    (plist+
     (if count
         (if (zerop count)
             (copy-list tabula)
             (if val
                 (loop :for (k v . rest) :on (normalize-plist tabula key-test) :by #'cddr
                       :if (funcall test old (funcall val v))
                         :do (decf count) :and :append (list k new) :into acc
                       :else :append (list k v) :into acc
                       :until (zerop count)
                       :finally (return (append acc rest)))
                 (loop :for (k v . rest) :on (normalize-plist tabula key-test) :by #'cddr
                       :if (funcall test old v)
                         :do (decf count) :and :append (list k new) :into acc
                       :else :append (list k v) :into acc
                       :until (zerop count)
                       :finally (return (append acc rest)))))
         ;; no-count:
         (if val
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :if (funcall test old (funcall val v))
                            :append (list k new) :else :append (list k v))
             (loop :with checked-keys := '()
                   :for (k v . nil) :on tabula :by #'cddr
                   :unless (member k checked-keys :test key-test)
                     :do (push k checked-keys)
                     :and :if (funcall test old v)
                            :append (list k new) :else :append (list k v)))))))


(setf (documentation 'substitute+ 'function) "
SUBSTITUTE+ new old tabula &key val (test 'eql) count => new-tabula
")


;;--------------------------------------------------------------------
;; substitute-if+
;;--------------------------------------------------------------------

(defun substitute-if+ (new predicate tabula &key key val (key-test #'equal) count)
  (check-type predicate (or function symbol))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type key-test  (or function symbol))
  (check-type count     (or null (integer 0 *)))
  (etypecase tabula
    (hash-table
     (if count
         (if key
             (if val
                 (loop :with result := (copy-hash-table tabula)
                       :until (zerop count)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate (funcall key k) (funcall val v))
                         :do (setf (gethash k result) new)
                             (decf count)
                       :finally (return result))
                 (loop :with result := (copy-hash-table tabula)
                       :until (zerop count)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate (funcall key k) v)
                         :do (setf (gethash k result) new)
                             (decf count)
                       :finally (return result)))
             ;; count, no-key:
             (if val
                 (loop :with result := (copy-hash-table tabula)
                       :until (zerop count)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate k (funcall val v))
                         :do (setf (gethash k result) new)
                             (decf count)
                       :finally (return result))
                 (loop :with result := (copy-hash-table tabula)
                       :until (zerop count)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate k v)
                         :do (setf (gethash k result) new)
                             (decf count)
                       :finally (return result))))
         ;; no-count
         (if key
             (if val
                 (loop :with result := (copy-hash-table tabula)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate (funcall key k) (funcall val v))
                         :do (setf (gethash k result) new)
                       :finally (return result))
                 (loop :with result := (copy-hash-table tabula)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate (funcall key k) v)
                         :do (setf (gethash k result) new)
                       :finally (return result)))
             (if val
                 (loop :with result := (copy-hash-table tabula)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate k (funcall val v))
                         :do (setf (gethash k result) new)
                       :finally (return result))
                 (loop :with result := (copy-hash-table tabula)
                       :for k :being :the :hash-keys :of tabula :using (:hash-value v)
                       :when (funcall predicate k v)
                         :do (setf (gethash k result) new)
                       :finally (return result))))))

    (alist
     (if count
         (if (zerop count)
             (copy-alist tabula)
             (if key
                 (if val
                     (loop :for ((k . v) . rest) :on (normalize-alist tabula key-test)
                           :if (funcall predicate (funcall key k) (funcall val v))
                             :do (decf count) :and :collect (cons k new) :into acc
                           :else :collect (cons k v) :into acc
                           :until (zerop count)
                           :finally (return (append acc rest)))
                     (loop :for ((k . v) . rest) :on (normalize-alist tabula key-test)
                           :if (funcall predicate (funcall key k) v)
                             :do (decf count) :and :collect (cons k new) :into acc
                           :else :collect (cons k v) :into acc
                           :until (zerop count)
                           :finally (return (append acc rest))))
                 ;; count, no-key
                 (if val
                     (loop :for ((k . v) . rest) :on (normalize-alist tabula key-test)
                           :if (funcall predicate k (funcall val v))
                             :do (decf count) :and :collect (cons k new) :into acc
                           :else :collect (cons k v) :into acc
                           :until (zerop count)
                           :finally (return (append acc rest)))
                     (loop :for ((k . v) . rest) :on (normalize-alist tabula key-test)
                           :if (funcall predicate k v)
                             :do (decf count) :and :collect (cons k new) :into acc
                           :else :collect (cons k v) :into acc
                           :until (zerop count)
                           :finally (return (append acc rest))))))
         ;; no-count:
         (if key
             (if val
                 (loop :with checked-keys := '()
                       :for ((k . v) . nil) :on tabula
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :if (funcall predicate (funcall key k) (funcall val v))
                                :collect (cons k new) :else :collect (cons k v))
                 (loop :with checked-keys := '()
                       :for ((k . v) . nil) :on tabula
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :if (funcall predicate (funcall key k) v)
                                :collect (cons k new) :else :collect (cons k v)))
             ;; no-count, no-key:
             (if val
                 (loop :with checked-keys := '()
                       :for ((k . v) . nil) :on tabula
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :if (funcall predicate k (funcall val v))
                                :collect (cons k new) :else :collect (cons k v))
                 (loop :with checked-keys := '()
                       :for ((k . v) . nil) :on tabula
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :if (funcall predicate k v)
                                :collect (cons k new) :else :collect (cons k v))))))
    
    (plist+
     (if count
         (if (zerop count)
             (copy-list tabula)
             (if key
                 (if val
                     (loop :for (k v . rest) :on (normalize-plist tabula key-test) :by #'cddr
                           :if (funcall predicate (funcall key k) (funcall val v))
                             :do (decf count) :and :append (list k new) :into acc
                           :else :append (list k v) :into acc
                           :until (zerop count)
                           :finally (return (append acc rest)))
                     (loop :for (k v . rest) :on (normalize-plist tabula key-test) :by #'cddr
                           :if (funcall predicate (funcall key k) v)
                             :do (decf count) :and :append (list k new) :into acc
                           :else :append (list k v) :into acc
                           :until (zerop count)
                           :finally (return (append acc rest))))
                 ;; count, no-key:
                 (if val
                     (loop :for (k v . rest) :on (normalize-plist tabula key-test) :by #'cddr
                           :if (funcall predicate k (funcall val v))
                             :do (decf count) :and :append (list k new) :into acc
                           :else :append (list k v) :into acc
                           :until (zerop count)
                           :finally (return (append acc rest)))
                     (loop :for (k v . rest) :on (normalize-plist tabula key-test) :by #'cddr
                           :if (funcall predicate k v)
                             :do (decf count) :and :append (list k new) :into acc
                           :else :append (list k v) :into acc
                           :until (zerop count)
                           :finally (return (append acc rest))))))
         ;; no-count:
         (if key
             (if val
                 (loop :with checked-keys := '()
                       :for (k v . nil) :on tabula :by #'cddr
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :if (funcall predicate (funcall key k) (funcall val v))
                                :append (list k new) :else :append (list k v))
                 (loop :with checked-keys := '()
                       :for (k v . nil) :on tabula :by #'cddr
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :if (funcall predicate (funcall key k) v)
                                :append (list k new) :else :append (list k v)))
             ;; no-count, no-key:
             (if val
                 (loop :with checked-keys := '()
                       :for (k v . nil) :on tabula :by #'cddr
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :if (funcall predicate k (funcall val v))
                                :append (list k new) :else :append (list k v))
                 (loop :with checked-keys := '()
                       :for (k v . nil) :on tabula :by #'cddr
                       :unless (member k checked-keys :test key-test)
                         :do (push k checked-keys)
                         :and :if (funcall predicate k v)
                                :append (list k new) :else :append (list k v))))))))


(setf (documentation 'substitute-if+ 'function) "
SUBSTITUTE-IF+ new predicate tabula &key key val count => new-tabula
")


;;--------------------------------------------------------------------
;; substitute-if-not+
;;--------------------------------------------------------------------

(defun  substitute-if-not+ (new predicate tabula &key key val count (key-test #'equal))
  (check-type predicate (or function symbol))
  (check-type key       (or symbol function))
  (check-type val       (or symbol function))
  (check-type count     (or null (integer 0 *)))
  (check-type key-test  (or function symbol))
  (substitute-if+ new (complement (ensure-function predicate))
                  tabula :key key :val val :count count :key-test key-test))

(setf (documentation 'substitute-if+ 'function) "
SUBSTITUTE-IF-NOT+ new predicate tabula &key key val count => new-tabula
")


;;====================================================================
