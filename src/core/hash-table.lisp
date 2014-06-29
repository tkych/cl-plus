;;;; Last modified: 2014-06-29 10:05:06 tkych

;; cl-plus/src/core/hash-table.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Hash Table
;;====================================================================

(in-package :cl-user)

(defpackage :cl-plus.src.core.hash-table
  (:documentation "
Hash-Table
==========


Sharp-Brace Reader
------------------

Notes
-----

 - Dispatch-read-macro parameters and hash-table-tests:

       2      -- eq
       3      -- eql
       5      -- equal
       6      -- equalp
       others -- equal

    * (hash-table-test #{})   => EQUAL ; default test is EQUAL.
    * (hash-table-test #2{})  => EQ
    * (hash-table-test #3{})  => EQL
    * (hash-table-test #5{})  => EQUAL
    * (hash-table-test #6{})  => EQUALP

 - If the parameter is null or < 10, sharp-brace-reader makes hash-table at
   READ TIME.
    e.g., (let ((x 42)) #{:foo x}) => #{:FOO X}

 - If the parameter is >= 10, sharp-brace-reader acts as ordinal macro.
    e.g., (let ((x 42)) #10{:foo x}) => #{:FOO 42}
")
  (:nicknames #:cl+hash-table)
  (:export #:pprint-readable-hash-table
           #:*print-readable-hash-table*
           #:*default-hash-table-test*
           #:sharp-brace-reader
           #:hash-table=
           #:dohash)
  (:import-from #:alexandria
                #:hash-table-plist
                #:ensure-function
                #:with-gensyms
                #:parse-body)
  (:use #:cl))

(in-package :cl-plus.src.core.hash-table)


;;--------------------------------------------------------------------
;; hash-table=
;;--------------------------------------------------------------------

(defun hash-table= (ht1 ht2 &key (key-test #'equal) (val-test #'eql))
  (check-type ht1 hash-table)
  (check-type ht2 hash-table)
  (check-type key-test (or function symbol))
  (check-type val-test (or function symbol))
  (and (= (hash-table-count ht1)
          (hash-table-count ht2))
       (let ((key-test-fn (ensure-function key-test)))
         (if (and (eql key-test-fn (ensure-function (hash-table-test ht1)))
                  (eql key-test-fn (ensure-function (hash-table-test ht2))))
             (with-hash-table-iterator (get-next-entry ht1)
               (loop
                 (multiple-value-bind (more? k1 v1) (get-next-entry)
                   (unless more? (return T))
                   (multiple-value-bind (v2 presentp) (gethash k1 ht2)
                     (unless (and presentp
                                  (funcall val-test v1 v2))
                       (return NIL))))))
             
             (let ((keys2 (loop :for k2 :being :the :hash-keys :of ht2 :collect k2)))
               (with-hash-table-iterator (get-next-entry ht1)
                 (loop
                   (multiple-value-bind (more? k1 v1) (get-next-entry)
                     ;; we already checked hash-table-count is same.
                     (unless more? (return T))
                     (let ((keys (member k1 keys2 :test key-test)))
                       (unless (and keys
                                    (funcall val-test v1 (gethash (first keys) ht2)))
                         (return NIL)))))))))))

(setf (documentation 'hash-table= 'function) "
HASH-TABLE= hash-table1 hash-table2 => boolean

Note
----
  * (hash-table= #5{:foo 0 :bar 1} #2{:foo 0 :bar 1}
                 :key-test 'eq)
    => T
")


;;--------------------------------------------------------------------
;; dohash
;;--------------------------------------------------------------------
;; Why we introduce `dohash' using `with-hash-table-iterator'?
;; Because:
;;   0. `with-hash-table-iterator' and `loop' is bit complicated.
;;   1. function `return' doesn't work for `maphash'.

(defmacro dohash ((key val table &optional result) &body body)
  (multiple-value-bind (tag-statements declarations) (parse-body body)
    (with-gensyms (more next-entry LOOP)
      `(block nil
         (with-hash-table-iterator (,next-entry ,table)
           (tagbody ,LOOP
              (multiple-value-bind (,more ,key ,val) (,next-entry)
                ,@declarations
                (when ,more
                  (tagbody ,@tag-statements)      
                  (GO ,LOOP))))
           ,(when result
              `(let (,key ,val)
                 (declare (ignorable ,key ,val))
                 ,result)))))))

(setf (documentation 'dohash 'function) "
DOHASH (key val table &optional result) &body body => result
")

(define-compiler-macro dohash (&whole form (key val table &optional result) &body body)
  (if (and (integerp cl-plus.src.core.iteration::*loop-unrolling-threshold*)
           (hash-table-p table)
           (<= (hash-table-count table)
               cl-plus.src.core.iteration::*loop-unrolling-threshold*))
      `(block nil
         ,@(loop :for k :being :the :hash-key :of table :using (:hash-value v)
                 :collect `(let ((,key ,k) (,val ,v)) (tagbody ,@body)))
         ,(when result
            `(let (,key ,val)
               (declare (ignorable ,key ,val))
               ,result)))
      form))


;;--------------------------------------------------------------------
;; Print Hash Table
;;--------------------------------------------------------------------
;; MEMO: versions of cl implementaions
;;  * clisp: 2.49 (2010-07-07)
;;  * sbcl:  1.2.0
;;  * ccl:   1.9
;;  * abcl:  1.3.1
;;  * ecl:   13.5.1

(defvar *print-readable-hash-table* t "
*PRINT-READABLE-HASH-TABLE* := generaized-boolean
")

(declaim (type #-clisp (member eq eql equal equalp)
               #+clisp (member eq
                               ext:fasthash-eq
                               eql
                               ext:fasthash-eql
                               equal
                               ext:fasthash-equal
                               equalp)
               *default-hash-table-test*))

(defvar *default-hash-table-test* #-clisp 'equal
                                  #+clisp 'ext:fasthash-equal
        "
*DEFAULT-HASH-TABLE-TEST* := (member eq eql equal equalp)

Test for hash-table that doesn't print explicitly.
For example,
 * (setf *default-hash-table-test* 'equal)
 * (hash-table-test #{}) => EQUAL
 * (setf *default-hash-table-test* 'eql)
 * (hash-table-test #{}) => EQL
")

(defun hash-table-test-to-num (ht)
  ;; CLHS, Function HASH-TABLE-TEST:
  ;;  > For the four standardized hash table test functions, the test
  ;;  > value returned is always a symbol. If an implementation permits
  ;;  > additional tests, it is implementation-dependent whether such
  ;;  > tests are returned as function objects or function names.
  (let ((ht-test (hash-table-test ht)))
    (if (eql ht-test *default-hash-table-test*)
        ""
        #-clisp
        (case ht-test
          (eq     #.(length "eq"))
          (eql    #.(length "eql"))
          (equal  #.(length "equal"))
          (equalp #.(length "equalp"))
          (t      "x"))
        
        #+clisp
        (case ht-test
          ((ext:fasthash-eq    eq)    #.(length "eq"))
          ((ext:fasthash-eql   eql)   #.(length "eql"))
          ((ext:fasthash-equal equal) #.(length "equal"))
          ((equalp)                   #.(length "equalp"))
          (t                          "x"))
        )))

;; TODO: cumcl, acl, lispworks, mocl, ...
(defun pprint-readable-hash-table (stream hash-table)
  (if *print-readable-hash-table*
      (pprint-logical-block
          (stream
           (loop :for k :being :the :hash-keys :of hash-table :using (:hash-value v)
                 :collect (cons k v))
           :prefix (format nil "#~A{" (hash-table-test-to-num hash-table))
           :suffix "}")
        (pprint-exit-if-list-exhausted)
        (loop
          (destructuring-bind (k . v) (pprint-pop)
            (prin1 k stream)
            (write-char #\Space stream)
            (prin1 v stream))
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)))

      #+ccl
      ;; cf. function PRINT-OBJECT in ccl/lib/hash.lisp
      (print-unreadable-object (hash-table stream :type t :identity t)
        (format stream "~S ~S size ~D/~D"
                :test
                (hash-table-test  hash-table)
                (hash-table-count hash-table)
                (hash-table-size  hash-table))
        (when (ccl::readonly-hash-table-p hash-table)
          (format stream " (Readonly)")))
      
      #+sbcl
      ;; cf. function PRINT-OBJECT in sbcl-1.2.0/src/code/target-hash-table.lisp
      (print-unreadable-object (hash-table stream :type t :identity t)
        (format stream ":TEST ~S :COUNT ~S"
                (hash-table-test hash-table) (hash-table-count hash-table)))
      
      #+clisp
      (format stream "#S(HASH-TABLE :TEST ~A)"
              (hash-table-test hash-table))
      
      #+abcl
      ;; cf. abcl-src-1.3.1/src/org/armedbear/lisp/HashTable.java
      (print-unreadable-object (hash-table stream :identity t)
        (format stream "~S HASH-TABLE ~D entries, ~D buckets"
                (hash-table-test  hash-table)
                (hash-table-count hash-table)
                (hash-table-size  hash-table)))

      #+ecl
      ;; cf. http://sourceforge.net/p/ecls/mailman/message/30278679/
      ;;     http://www.lispworks.com/reference/HyperSpec/Body/11_abab.htm
      (print-unreadable-object (hash-table stream :identity t)
        (format stream "hash-table"))
      
      #-(or ccl sbcl clisp abcl ecl)
      (print-unreadable-object (hash-table stream :type t :identity t)
        (format stream ":TEST ~S :COUNT ~S"
                (hash-table-test hash-table) (hash-table-count hash-table)))))


;;--------------------------------------------------------------------
;; Sharp Brace Reader
;;--------------------------------------------------------------------

(defun %plist-to-hash-table (plist test)
  (when (oddp (length plist))
    (error "There are odd number elements between braces."))
  (loop :with result := (make-hash-table :test test)
        :for (k v . nil) :on plist :by #'cddr
        :do (if (nth-value 1 (gethash k result))
                (error "There is at least two same key ~S between braces." k)
                (setf (gethash k result) v))
        :finally (return result)))

(defun num-to-hash-table-test (n)
  (case n
    (#.(length "eq")         'eq)
    (#.(length "eql")       'eql)
    (#.(length "equal")   'equal)
    (#.(length "equalp") 'equalp)
    (t                   *default-hash-table-test*)))

(defun sharp-brace-reader (stream char parameter)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\} (get-macro-character #\)))
    (if (or (null parameter)
            (< parameter 10))
        ;; Not-evaluate elements.
        ;; The time making hash-table is read-time.
        #-abcl
        (%plist-to-hash-table (read-delimited-list #\} stream t)
                              (num-to-hash-table-test parameter))
        #+abcl
        ;; MEMO: ABCL-1.3.1
        ;; Currently, in abcl-1.3.1, there is a bug that hash-table as a constant
        ;; can't be dumped into compiled files. If sharp-brace-reader makes a hash-table
        ;; at read time, it causes an error when files are compiled.
        ;; see. ABCL Bug Report #315: Hash tables as literal objects in compiled files 
        ;;      http://abcl.org/trac/ticket/315
        `(%plist-to-hash-table (list ,@(read-delimited-list #\} stream t))
                               ',(if parameter
                                     (num-to-hash-table-test (mod parameter 10))
                                     (num-to-hash-table-test parameter)))
        
        ;; Evaluate elements before make hash-table.
        ;; Sharp-brace-reader acts as ordinal macro.
        `(%plist-to-hash-table (list ,@(read-delimited-list #\} stream t))
                               ',(num-to-hash-table-test (mod parameter 10))))))


;;====================================================================
