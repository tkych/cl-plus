;;;; Last modified: 2014-06-29 10:07:39 tkych

;; cl-plus/src/core/polymorphics.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;; TODO
;; ----
;;  - compiler-macro

;;====================================================================
;; Polymorphics
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.core.polymorphics
  (:documentation "
Polymorphics
============

Polymorphic Extensible Operators.


Generic Functions
-----------------

 - emptyp
 - size
 - copy
 - ref
 - add
 - to
 - fmap
 - equals
 - compare


References
----------

 [0] Kent Pitman (1997),
     P.S.: The Best of Intentions: EQUAL Rights -- and Wrongs -- in Lisp.
     http://www.nhplace.com/kent/PS/EQUAL.html

 [1] Marco Antoniotti,
     CDR 8: Generic Equality and Comparison for Common Lisp.
     http://cdr.eurolisp.org/document/8/
")
  (:nicknames #:cl+polymorphics)
  (:export #:emptyp
           #:size
           #:copy
           #:tree #:ref
           #:add
           #:to
           #:fmap
           #:equals
           #:compare)
  (:use #:cl)
  (:import-from #:alexandria
                #:once-only
                #:with-gensyms
                #:copy-array
                #:copy-hash-table
                #:ensure-list
                #:if-let
                #:read-file-into-string
                #:read-file-into-byte-vector
                #:compose)
  (:import-from #:cl-plus.src.dev-util
                #:copy-empty-hash-table
                #:copy-empty-array)
  (:import-from #:cl-plus.src.srfi.srfi-45
                #:promise
                #:copy-promise)
  (:import-from #:cl-plus.src.cdr.cdr-5
                #:array-index)
  (:import-from #:cl-plus.src.cdr.cdr-8
                #:equals
                #:compare)
  (:import-from #:cl-plus.src.core.alist
                #:alist)
  (:import-from #:cl-plus.src.core.plist
                #:plist
                #:plist+)
  (:import-from #:cl-plus.src.core.hash-table
                #:dohash)
  (:import-from #:cl-plus.src.core.lazy-sequence
                #:lazy-sequence
                #:empty-lazy-seq-p
                #:accessed-length
                #:copy-lazy-seq
                #:make-lazy-seq
                #:lazy-take
                #:lazy-flow
                #:lseq->lflow
                #:lflow->lseq
                #:lref)
  (:import-from #:cl-plus.src.core.sequens
                #:sequens)
  (:import-from #:cl-plus.src.core.tabula
                #:tabula
                #:to-alist
                #:to-plist)
  (:import-from #:cl-plus.src.core.buxis
                #:buxis))

(in-package #:cl-plus.src.core.polymorphics)


;;--------------------------------------------------------------------
;; emptyp
;;--------------------------------------------------------------------

(defgeneric emptyp (object)

  (:method ((n number))
    (zerop n))
  
  (:method ((lst list))
    (null lst))
  
  (:method ((seq sequence))
    (zerop (length seq)))
  
  (:method ((ary array))
    (zerop (array-total-size ary)))
  
  (:method ((ht hash-table))
    (zerop (hash-table-count ht)))

  (:method ((lseq lazy-sequence))
    (empty-lazy-seq-p lseq))
  )


(setf (documentation 'emptyp 'function) "
EMPTYP object => boolean
Extensible Polymorphic Operator.
")


;;--------------------------------------------------------------------
;; copy
;;--------------------------------------------------------------------

(defgeneric copy (object &rest keys &key &allow-other-keys)

  (:method ((lst list) &rest keys &key (as 'list) &allow-other-keys)
    (declare (ignore keys))
    (ecase as
      ((list  :list)  (copy-list lst))
      ((alist :alist) (copy-alist lst))
      ((tree  :tree)  (copy-tree lst))))
  
  (:method ((seq sequence) &rest keys &key &allow-other-keys)
    (declare (ignore keys))
    (copy-seq seq))

  (:method ((ht hash-table) &rest keys &key &allow-other-keys)
    (apply #'copy-hash-table ht keys))
  
  (:method ((ary array) &rest keys &key &allow-other-keys)
    (apply #'copy-array ary keys))
  
  (:method ((lseq lazy-sequence) &rest keys &key &allow-other-keys)
    (declare (ignore keys))
    (copy-lazy-seq lseq))
  
  (:method ((promise promise) &rest keys &key &allow-other-keys)
    (declare (ignore keys))
    (copy-promise promise))
  )


(setf (documentation 'copy 'function) "
COPY object &rest keys &key &allow-other-keys => copied-object
Extensible Polymorphic Operator.
Generalized copy function.

 Object          Keywords
  List       --- as
  Hash-Table --- key test size rehash-size rehash-threshold
  Array      --- element-type fill-pointer adjustable
")


;;--------------------------------------------------------------------
;; size
;;--------------------------------------------------------------------

(defgeneric size (object &rest keys &key &allow-other-keys)

  (:method ((lst list) &rest keys &key (as 'list) &allow-other-keys)
    (declare (ignore keys))
    (ecase as
      ((list alist :list :alist)
       (length lst))
      
      ((plist :plist)
       (floor (length lst) 2))))
  
  (:method ((seq sequence) &rest keys &key &allow-other-keys)
    (declare (ignore keys))
    (length seq))
  
  (:method ((ary array) &rest keys &key &allow-other-keys)
    (declare (ignore keys))
    (array-total-size ary))

  (:method ((ht hash-table) &rest keys &key &allow-other-keys)
    (declare (ignore keys))
    (hash-table-count ht))

  (:method ((lseq lazy-sequence) &rest keys &key &allow-other-keys)
    (declare (ignore keys))
    (accessed-length lseq))
  )

(setf (documentation 'size 'function) "
SIZE object &rest keys &key &allow-other-keys => non-negative-integer
Extensible Polymorphic Operator.
Generalized LENGTH, ARRAY-TOTAL-SIZE, HASH-TABLE-COUNT and ACCESSED-LENGTH.
")


;;--------------------------------------------------------------------
;; Ref
;;--------------------------------------------------------------------

(defgeneric ref (collection reference &optional default)
  
  (:method ((i integer) byte-spec &optional default)
    (if (< (byte-position byte-spec) (integer-length i))
        (values (ldb byte-spec i) t)
        (values default nil)))
  
  (:method ((seq sequence) (index integer) &optional default)
    (if (< index (length seq))
        (values (elt seq index) t)
        (values default nil)))

  (:method ((lst list) (index integer) &optional default)
    ;; MEMO:
    ;; Doted-list works if `index' is upto number of cons-cells.
    ;; For example,
    ;;   * (nthcdr 3 '(0 1 2 . 3)) => 3
    ;;   * (nthcdr 4 '(0 1 2 . 3)) => error
    (let ((tail (nthcdr index lst)))
      (if (consp tail)
          (values (car tail) t)
          (values default nil))))
  
  (:method ((ary array) (major-index integer) &optional default)
    (if (< major-index (array-total-size ary))
        (values (row-major-aref ary major-index) t)
        (values default nil)))

  (:method ((ary array) (subscripts list) &optional default)
    (let ((major-index (apply #'array-row-major-index ary subscripts)))
      (if (< major-index (array-total-size ary))
          (values (apply #'aref ary subscripts) t)
          (values default nil))))

  (:method ((lseq lazy-sequence) index &optional default)
    (lref lseq index :force default))
  
  (:method ((ht hash-table) key &optional default)
    (gethash key ht default))
  )


(setf (documentation 'ref 'function) "
REF collection reference &optional default => value, present-p
Extensible Polymorphic Operator.

Generalized refer function.

  <collection> ::= <sequence> | <lazy-sequence> | <hash-table>
                 | <array> | <integer> | ...

  <reference> ::= <index> | <key> | <subscripts> | <byte-spec> | ...
")


(defgeneric (setf ref) (new-value collection reference &optional default)
  
  (:method (new-value (ht hash-table) key &optional default)
    (setf (gethash key ht default) new-value))

  (:method (new-value (lst list) index &optional default)
    (declare (ignore default))
    (setf (nth index lst) new-value))
  
  (:method (new-value (seq sequence) index &optional default)
    (declare (ignore default))
    (setf (elt seq index) new-value))

  (:method (new-value (ary array) (subscripts list) &optional default)
    (declare (ignore default))
    (setf (apply #'aref ary subscripts)
          new-value))

  (:method (new-value (ary array) (row-major-index integer) &optional default)
    (declare (ignore default))
    (setf (row-major-aref ary row-major-index)
          new-value))

  ;; TODO:
  ;; (:method (new-value (lseq lazy-sequence) index &optional default)
  ;;   (declare (ignore default))
  ;;   (labels ((rec (index lseq)
  ;;              (cond
  ;;                ((empty-lazy-seq-p lseq)
  ;;                 (values nil nil))
  ;;                ((eql index 0)
  ;;                 (values (head lseq) t))
  ;;                (t
  ;;                 (rec (1- index) (force lseq))))))
  ;;     (rec index lseq)))
  )


;;--------------------------------------------------------------------
;; add
;;--------------------------------------------------------------------

(defgeneric add (object1 object2 &rest more-objects)

  (:method ((n1 number) (n2 number) &rest more-objects)
    (if (null more-objects)
        (+ n1 n2)
        (apply #'add (+ n1 n2) more-objects)))

  (:method ((lst list) (seq sequence) &rest more-objects)
    (if (null more-objects)
        (concatenate 'list lst seq)
        (apply #'add (concatenate 'list lst seq) more-objects)))
  
  (:method ((str string) (seq sequence) &rest more-objects)
    (if (null more-objects)
        (concatenate 'string str seq)
        (apply #'add (concatenate 'string str seq) more-objects)))

  (:method ((bvec bit-vector) (seq sequence) &rest more-objects)
    (if (null more-objects)
        (concatenate 'bit-vector bvec seq)
        (apply #'add (concatenate 'bit-vector bvec seq) more-objects)))
  
  (:method ((vec vector) (seq sequence) &rest more-objects)
    (if (null more-objects)
        (concatenate 'vector vec seq)
        (apply #'add (concatenate 'vector vec seq) more-objects)))

  ;; NB. array+ is does not consistant with vector+.
  (:method ((ary1 array) (ary2 array) &rest more-objects)
    (if (not (equal (array-dimensions ary1) (array-dimensions ary2)))
        (error "~S and ~S are not same dimensions." ary1 ary2)
        (let ((ary3 (make-array (array-dimensions ary1))))
          (dotimes (i (array-total-size ary1))
            (declare (type array-index i))
            (setf (row-major-aref ary3 i) (+ (row-major-aref ary1 i)
                                             (row-major-aref ary2 i))))
          (if (null more-objects)
              ary3
              (apply #'add ary3 more-objects)))))
    
  (:method ((ht1 hash-table) (ht2 hash-table) &rest more-objects)
    (let ((ht3 (copy-hash-table ht1)))
      (dohash (k v ht2)
        (setf (gethash k ht3) v))
      (if (null more-objects)
          ht3
          (apply #'add ht3 more-objects))))

  (:method ((p1 pathname) (p2 pathname) &rest more-objects)
    (if (null more-objects)
        (merge-pathnames p1 p2)
        (apply #'add (merge-pathnames p1 p2) more-objects)))

  (:method ((fn1 function) (fn2 function) &rest more-objects)
    (let ((composed (lambda (&rest args)
                      (declare (dynamic-extent args))
                      (funcall fn1 (apply fn2 args)))))
      (if (null more-objects)
          composed
          (apply #'add composed more-objects))))
  )


(setf (documentation 'add 'function) "
ADD object1 object2 &rest more-objects => resutl

Extensible Polymorphic Operator.
Generailzed CONCATENATE and +.

Examples
--------
  * (add 40 1 1)               => 42
  * (add \"Hello\" \" \" \"World!\") => \"Hello World!\"
  * (add '(#\l #\i) \"sp\")        => (#\l #\i #\s #\p)
  * (add \"\" '(#\l #\i) \"sp\")     => \"lisp\"
  * (add #2A((1 2 3) (4 5 6))
         #2A((6 5 4) (3 2 1))) => #2A((7 7 7) (7 7 7))

  * (add #{:Roma \"Varro\"} #{:Carthage \"Hannibal\"})
    => #{:ROMA \"Varro\" :CARTHAGE \"Hannibal\"}

  * (add * #{:Roma \"Scipio\"})
    => #{:ROMA \"Scipio\" :CARTHAGE \"Hannibal\"}

  * (defstruct pocket contents)
    (defmethod add ((p1 pocket) (p2 pocket) &rest more)
      (let ((p3 (make-pocket :contents (append (pocket-contents p1)
                                               (pocket-contents p2)))))
        (if (null more)
            p3
            (apply #'add p3 more))))

    (let* ((crassus  (make-pocket :contents '(\"biscuit\")))
           (pompeius (make-pocket :contents '(\"chocolate\")))
           (caesar   (add crassus pompeius)))
      (pocket-contents caesar))
    => (\"biscuit\" \"chocolate\")
")


;;--------------------------------------------------------------------
;; to
;;--------------------------------------------------------------------

(defgeneric to (result-type object)

  ;; PATHNAME -> STRING
  ;; ------------------
  (:method ((type (eql 'string)) (path pathname))
    (declare (ignore type))
    (read-file-into-string path))

  (:method ((type (eql 'unsigned-byte)) (path pathname))
    (declare (ignore type))
    (read-file-into-byte-vector path))

  ;; number -> string
  (:method ((type (eql 'string)) (i integer))
    (declare (ignore type))
    (write-to-string i))
  
  ;; SEQUENCE -> *
  ;; -------------
  (:method ((type t) (seq sequence))
    (coerce seq type))

  (:method ((type (eql 'sequence)) (seq sequence))
    ;; NOTE: different from `coerce'
    ;; (lex seq '(0 1 2))
    ;; (eql seq (coerce seq 'sequence)) => T
    ;; (eql seq (to 'sequence seq))     => NIL
    (copy-seq seq))
  
  (:method ((type (eql 'sequens)) (seq sequence))
    (copy-seq seq))

  (:method ((type (eql 'lazy-sequence)) (seq sequence))
    (make-lazy-seq seq))
  
  (:method ((type (eql 'tabula)) (seq sequence))
    (to 'hash-table seq))
  
  (:method ((type (eql 'buxis)) (seq sequence))
    (copy-seq seq))


  ;; LIST -> *
  ;; ---------
  (:method ((type (eql 'list)) (lst list))
    (declare (ignore type))
    (copy-list lst))

  (:method ((type (eql 'string)) (lst list))
    (with-output-to-string (s)
      (dolist (v lst)
        (princ v s))))

  (:method ((type (eql 'plist)) (lst list))
    (loop :for k :from 0
          :for v :in lst
          :append (list k v)))

  (:method ((type (eql 'alist)) (lst list))
    (loop :for k :from 0
          :for v :in lst
          :collect (cons k v)))
  
  (:method ((type (eql 'hash-table)) (lst list))
    (loop :with result := (make-hash-table :test 'equal)
          :for v :in lst
          :for k :from 0
          :do (setf (gethash k result) v)
          :finally (return result)))

  
  ;; VECTOR -> *
  ;; -----------
  (:method ((type (eql 'vector)) (vec vector))
    (declare (ignore type))
    (copy-seq vec))
  
  (:method ((type (eql 'string)) (vec vector))
    (with-output-to-string (s)
      (loop :for v :across vec :do (princ v s))))

  (:method ((type (eql 'plist)) (vec vector))
    (loop :for k :from 0
          :for v :across vec
          :append (list k v) :into acc
          :finally (return (coerce acc 'vector))))
  
  (:method ((type (eql 'alist)) (vec vector))
    (loop :for k :from 0
          :for v :across vec
          :collect (cons k v)))

  (:method ((type (eql 'hash-table)) (vec vector))
    (loop :with result := (make-hash-table :test 'equal)
          :for v :across vec
          :for k :from 0
          :do (setf (gethash k result) v)
          :finally (return result)))


  ;; LAZY-SEQUENCE -> *
  ;; ------------------
  (:method ((type (eql 'sequens)) (lseq lazy-sequence))
    (declare (ignore type))
    (copy-lazy-seq lseq))

  (:method ((type (eql 'list)) (lseq lazy-sequence))
    (declare (ignore type))
    (lazy-take :all lseq))

  (:method ((type (eql 'sequence)) (lseq lazy-sequence))
    (declare (ignore type))
    (lazy-take :all lseq))
  
  (:method ((type (eql 'vector)) (lseq lazy-sequence))
    (declare (ignore type))
    (coerce (lazy-take :all lseq) 'vector))

  (:method ((type (eql 'string)) (lseq lazy-sequence))
    (declare (ignore type))
    (coerce (lazy-take :all lseq) 'string))
  
  (:method ((type (eql 'lazy-flow)) (lseq lazy-sequence))
    (declare (ignore type))
    (lseq->lflow lseq))

  ;; LAZY-FLOW -> *
  ;; --------------
  (:method ((type (eql 'lazy-sequence)) (lflow lazy-flow))
    (declare (ignore type))
    (lflow->lseq lflow))

  
  ;; ARRAY -> *
  ;; ----------
  (:method ((type (eql 'array)) (ary array))
    (declare (ignore type))
    (copy-array ary))

  (:method ((type (eql 'list)) (ary array))
    (declare (ignore type))
    (loop :for i :of-type array-index :from 0 :below (array-total-size ary)
          :collect (row-major-aref ary i)))

  (:method ((type (eql 'vector)) (ary array))
    (declare (ignore type))
    (loop :for i :of-type array-index :from 0 :below (array-total-size ary)
          :collect (row-major-aref ary i) :into acc
          :finally (return (coerce acc 'vector))))

  (:method ((type (eql 'string)) (ary array))
    (declare (ignore type))
    (loop :for i :of-type array-index :from 0 :below (array-total-size ary)
          :collect (row-major-aref ary i) :into acc
          :finally (return (coerce acc 'string))))

  (:method ((type (eql 'bit-vector)) (ary array))
    (declare (ignore type))
    (loop :for i :of-type array-index :from 0 :below (array-total-size ary)
          :collect (row-major-aref ary i) :into acc
          :finally (return (coerce acc 'bit-vector))))

  
  ;; HASH-TABLE -> *
  ;; ---------------
  (:method ((type (eql 'hash-table)) (ht hash-table))
    (declare (ignore type))
    (copy-hash-table ht))
  
  (:method ((type (eql 'plist)) (ht hash-table))
    (declare (ignore type))
    (to-plist ht))

  (:method ((type (eql 'alist)) (ht hash-table))
    (declare (ignore type))
    (to-alist ht))
  
  (:method ((type (eql 'list)) (ht hash-table))
    (declare (ignore type))
    (loop :for v :being :the :hash-values :of ht :collect v))

  (:method ((type (eql 'vector)) (ht hash-table))
    (declare (ignore type))
    (loop :for v :being :the :hash-values :of ht :collect v :into acc
          :finally (return (coerce acc 'vector))))

  (:method ((type (eql 'sequence)) (ht hash-table))
    (declare (ignore type))
    (loop :for v :being :the :hash-values :of ht :collect v))
  
  ;; TODO:
  ;; (:method ((type (eql 'lazy-sequence)) (ht hash-table))
  ;;   (declare (ignore type))
  ;;   )
  )


(setf (documentation 'to 'function) "
TO result-type object => result

Extensible Polymorphic Operator.
Generailzed COERCE.

Examples
--------
 * (defmethod to ((type (eql 'keys)) (ht hash-table))
     (declare (ignore type))
     (loop :for k :being :the :hash-keys :of ht :collect k))

   (to 'keys #{:foo 1 :bar 8 :baz 4}) => (:FOO :BAR :BAZ)

Note
----
 * If `object' is already of type `result-type', copied-object is returned.
   (`coerce' returns `object' itself. cf. CLHS, Function COERCE)
   For example,
     (defvar *seq* '(0 1 2))
     (eql *seq* (coerce *seq* 'sequence)) => T
     (eql *seq* (to 'sequence *seq*))     => NIL
")


;;--------------------------------------------------------------------
;; fmap
;;--------------------------------------------------------------------

#+nil
(test ?fmap
  (is-= (fmap #'1+ 99)
        110)
  (is-= (fmap #'1+ 0)
        1)
  )

(defgeneric fmap (function collection &rest keys &key &allow-other-keys)
  
  (:method (fn (int integer) &rest keys &key &allow-other-keys)
    (declare (ignore keys))
    (let ((result 0))
      (declare (type integer result))
      (loop :for i :from 0 :do
        (multiple-value-bind (ds d) (floor int 10)
          (incf result (* (funcall fn d) (expt 10 i)))
          (if (zerop ds)
              (return result)
              (setf int ds))))))
  
  (:method (fn (lst list) &rest keys &key (as 'list) &allow-other-keys)
    (declare (ignore keys))
    (ecase as
      ((list :list) (mapcar fn lst))
      ((tree :tree) (labels ((map-tree (tree)
                               (if (null tree)
                                   nil
                                   (if (atom tree)
                                       (funcall fn tree)
                                       (destructuring-bind (l . r) tree
                                         (cons (map-tree l)
                                               (map-tree r)))))))
                      (map-tree lst)))))

  (:method (fn (vec vector) &rest keys &key &allow-other-keys)
    (declare (ignore keys))
    (map 'vector fn vec))

  (:method (fn (ary array) &rest keys &key &allow-other-keys)
    (declare (ignore keys))
    (let ((result (copy-empty-array ary)))
      (dotimes (i (array-total-size ary))
        (setf (row-major-aref result i)
              (funcall fn (row-major-aref ary i))))
      result))
  
  (:method (fn (ht hash-table) &rest keys &key &allow-other-keys)
    (declare (ignore keys))
    (let ((result (copy-empty-hash-table ht)))
      (maphash (lambda (k v)
                 (setf (gethash k result)
                       (funcall fn v)))
               ht)
      result))

  ;; ($ (fmap #'1+ #'1+) 2) => 4
  (:method (fn (f function) &rest keys &key &allow-other-keys)
    (declare (ignore keys))
    (compose fn f))

  ;; TODO:
  ;; (:method (fn (lseq lazy-sequence))
  ;;   )
  )


(setf (documentation 'fmap 'function) "
          (((EXPERIMANTAL)))

FMAP function collection &rest keys &key &allow-other-keys => copyed-object
Extensible Polymorphic Operator.


1. (fmap 'identity x) == (identitiy x) == x

   e.g., 
    * (fmap 'identity 42) => 42
    * (identity 42)       => 42


2. (<< fmap (<< f _ g)) == (<< fmap 'f _ fmap 'g)

   e.g., 
    * (fmap (<< 1+ _ 1+) '(0 1 2))          => (2 3 4)
    * ($ (<< fmap '1+ _ fmap '1+) '(0 1 2)) => (2 3 4)
")


;;====================================================================
