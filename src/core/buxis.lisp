;;;; Last modified: 2014-06-29 21:48:53 tkych

;; cl-plus/src/core/buxis.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;; TODO:
;;  * compiler-macro
;;  ? ADD: destructive, n~*
;;  * with-iterators -> iterator.lisp

;;====================================================================
;; Buxis
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.core.buxis
  (:documentation "
Buxis
=====

    buxis := (or sequence lazy-sequence hash-table array)


BUXIS is a collection of pair(TAG, VALUE).
BUXIS is abstraction from (LAZY-)SEQUENCE, HASH-TABLE and ARRAY.
TAG is abstraction from SEQUENCE-INDEX, HASH-KEY and ARRAY-SUBSCRIPTS.
All functions for buxis have \"*\" suffix.


Functions
---------

 - map*
 - reduce*
 - scan*
 - count*
 - count-if*
 - count-if-notn*
 - find*
 - find-if*
 - find-if-not*
 - position*
 - position-if*
 - position-if-not*
 - remove*
 - remove-if*
 - remove-if-not*
 - collect-if*
 - substitute*
 - substitute-if*
 - substitute-if-not*
 - fill*


Macros
------

 - dobux
 - dobux2
 - with-iterator
 - with-iterators
")
  (:nicknames #:cl+buxis)
  (:export #:with-iterator
           #:with-iterators
           #:buxis        #:buxisp
           #:dobux        #:dobux2
           #:every*       #:notevery*
           #:some*        #:notany*
           #:map*         #:reduce*        #:scan*
           #:count*       #:count-if*      #:count-if-not*
           #:find*        #:find-if*       #:find-if-not*
           #:position*    #:position-if*   #:position-if-not*
           #:remove*      #:remove-if*     #:remove-if-not*
           #:collect-if*
           #:substitute*  #:substitute-if* #:substitute-if-not*
           #:fill*)
  (:use #:cl)
  (:import-from #:alexandria
                #:once-only
                #:with-gensyms
                #:parse-body
                #:ensure-list
                #:ensure-function
                #:copy-array
                #:copy-hash-table
                #:when-let)
  (:import-from #:cl-plus.src.dev-util
                #:copy-empty-array
                #:copy-empty-hash-table)
  (:import-from #:cl-plus.src.core.sequence
                #:sequencep
                #:scan)
  (:import-from #:cl-plus.src.core.iteration
                #:dolist2)
  (:import-from #:cl-plus.src.core.array
                #:array-subscripts
                #:doary
                #:doary2)
  (:import-from #:cl-plus.src.cdr.cdr-5
                #:array-index)
  (:import-from #:cl-plus.src.core.lazy-sequence
                #:lazy-sequence
                #:make-lazy-seq
                #:copy-lazy-seq
                #:with-lazy-seq-iterator
                #:lazy-take
                #:lazy-drop
                #:lazy-remove
                #:lazy-remove-if
                #:lazy-map
                #:replicate)
  (:import-from #:cl-plus.src.core.sequens
                #:sequens)
  (:import-from #:cl-plus.src.core.tabula
                #:vals))

(in-package #:cl-plus.src.core.buxis)


;;--------------------------------------------------------------------
;; type
;;--------------------------------------------------------------------

(declaim (inline buxisp))
(deftype buxis () '(or hash-table sequence lazy-sequence array))
(defun buxisp (x) (typep x 'buxis))

(deftype array-index-1 () `(integer -1 (,array-total-size-limit)))
(setf (documentation 'array-index-1 'type) "
optimization purpose only.
")


;;--------------------------------------------------------------------
;; iterators
;;--------------------------------------------------------------------
;; iterator := (ftype () (values more? tag value))

;; TODO:
;; ? -> iteration.lisp
;; * PROFILE:
;;   1. index 0, (type (integer 0 *) index), multiple-value-prog1.
;;   2. index -1, (type (integer -1 *) index).

(defun %make-list-iterator (list)
  (declare (type list list))
  (let ((lst   (copy-list list))
        (index 0))
    (declare (type list lst)
             (type (integer 0 *) index))
    (lambda ()
      (multiple-value-prog1
          (if (null lst)
              NIL
              (values T index (pop lst)))
        (incf index)))))

(defun %make-string-iterator (string)
  (declare (type string string))
  (let ((end   (length string))
        (index 0))
    (declare (type array-index end index))
    (lambda ()
      (multiple-value-prog1
          (if (<= end index)
              NIL
              (values T index (char string index)))
        (incf index)))))

(defun %make-bit-vector-iterator (bit-vector)
  (declare (type bit-vector bit-vector))
  (let ((end   (length bit-vector))
        (index 0))
    (declare (type array-index end index))
    (lambda ()
      (multiple-value-prog1
          (if (<= end index)
              NIL
              (values T index (bit bit-vector index)))
        (incf index)))))

(defun %make-vector-iterator (vector)
  (declare (type vector vector))
  (let ((end   (length vector))
        (index 0))
    (declare (type array-index end index))
    (lambda ()
      (multiple-value-prog1
          (if (<= end index)
              NIL
              (values T index (aref vector index)))
        (incf index)))))

(defun %make-sequence-iterator (sequence)
  (declare (type sequence sequence))
  (let ((end   (length sequence))
        (index 0))
    (declare (type (integer 0 *) end index))
    (lambda ()
      (multiple-value-prog1
          (if (<= end index)
              NIL
              (values T index (elt sequence index)))
        (incf index)))))

(defun warn-order-of-hash-table-entries ()
  (warn "The result might depend on hash-table implementation of your CL systems."))

(defun %make-hash-table-iterator (hash-table)
  (declare (type hash-table hash-table))
  (warn-order-of-hash-table-entries)
  (let ((keys (loop :for k :being :the :hash-keys :of hash-table :collect k)))
    (declare (type list keys))
    (lambda ()
      (if (null keys)
          NIL
          (let ((key (pop keys)))
            (values T key (gethash key hash-table)))))))

(defun %make-array-iterator (array)
  (declare (type array array))
  (let ((end   (array-total-size array))
        (index 0))
    (declare (type array-index end index))
    (lambda ()
      (multiple-value-prog1
          (if (<= end index)
              NIL
              (values T index (row-major-aref array index)))
        (incf index)))))

(defun %make-iterator (buxis)
  (etypecase buxis
    (list          (%make-list-iterator buxis))
    (string        (%make-string-iterator buxis))
    (bit-vector    (%make-bit-vector-iterator buxis))
    (vector        (%make-vector-iterator buxis))
    (sequence      (%make-sequence-iterator buxis))
    (lazy-sequence (cl+lazy-sequence::%make-lazy-seq-iterator buxis))
    (hash-table    (%make-hash-table-iterator buxis))
    (array         (%make-array-iterator buxis))))

(defmacro with-iterator ((name buxis) &body body)
  (with-gensyms (iter)
    `(let ((,iter (%make-iterator ,buxis)))
       (declare (type function ,iter))
       (macrolet ((,name () `(funcall ,',iter)))
         ,@body))))

(setf (documentation 'with-iterator 'function) "
WITH-ITERATOR (name buxis) &body body => result

 `name'  --- a name suitable for the first argument to macrolet.
 `buxis' --- a form, evaluated once, that should produce a buxis. 

An invocation (name) returns (values more? tag value):

 1. more? --- A generalized boolean that is true if an entries is returned.
 2. tag   --- The index/key from the buxis entry.
 3. value --- The value from the buxis entry.

Note
----
 - If the `buxis' is a hash-table, the order of invocated entries
   depends on hash-table implementaton of your CL system.
")

(defmacro with-iterators ((name buxides) &body body)
  (with-gensyms (iters iter entry vals)
    `(let ((,iters (mapcar #'%make-iterator ,buxides)))
       (declare (type list ,iters))
       (macrolet ((,name ()
                    `(loop :for ,',iter :of-type function :in ,',iters
                           :for ,',entry := (multiple-value-list (funcall ,',iter))
                           :if (first ,',entry)
                             :collect (third ,',entry) :into ,',vals
                           :else :return NIL
                           :finally (return (values-list (list* T ,',vals))))))
         ,@body))))

(setf (documentation 'with-iterators 'function) "
WITH-ITERATORS (name buxides) &body body => result

 `name' --- a name suitable for the first argument to macrolet.
 `buxides' --- a form, evaluated once, that should produce a list of buxis. 

An invocation (name) returns (values more? value0 value1 ...):

 1. more? --- A generalized boolean that is true if an entries is returned.
 2. value0 value1 ... --- The values from the buxides entries. 

Note
----

 - If the `buxides' contains at least one hash-table, the order of
   invocated entries depends on hash-table implementation of your cl
   system.
")


;;--------------------------------------------------------------------
;; dobux
;;--------------------------------------------------------------------

(defmacro dobux ((var buxis-form &optional result-form) &body body)
  (check-type var symbol)
  (multiple-value-bind (tag-statements declarations) (parse-body body)
    (once-only (buxis-form)
      (with-gensyms (name more? index)
        `(etypecase ,buxis-form
           (array
            (doary (,var ,buxis-form ,result-form)
              ,@body))
           
           (lazy-sequence
            (with-lazy-seq-iterator (,name ,buxis-form)
              (loop
                (multiple-value-bind (,more? ,index ,var) (,name)
                  (declare (ignore ,index))
                  ,@declarations
                  (unless ,more? (return ,result-form))
                  (tagbody ,@tag-statements)))))
           
           (hash-table
            (warn-order-of-hash-table-entries)
            (with-hash-table-iterator (,name ,buxis-form)
              (loop
                (multiple-value-bind (,more? ,index ,var) (,name)
                  (declare (ignore ,index))
                  ,@declarations
                  (unless ,more? (return ,result-form))
                  (tagbody ,@tag-statements)))))

           (list
            (dolist (,var ,buxis-form ,result-form)
              ,@body)))))))

(setf (documentation 'dobux 'function) "
DOBUX (var buxis-form &optional result-form) &body body => result
")


;;--------------------------------------------------------------------
;; dobux2
;;--------------------------------------------------------------------

(defmacro dobux2 ((tag-var value-var buxis-form &optional result-form) &body body)
  (check-type tag-var   symbol)
  (check-type value-var symbol)
  (multiple-value-bind (tag-statements declarations) (parse-body body)
    (once-only (buxis-form)
      (with-gensyms (name more?)
        `(etypecase ,buxis-form
           (array
            (doary2 (,tag-var ,value-var ,buxis-form ,result-form)
              ,@body))
           
           (lazy-sequence
            (with-lazy-seq-iterator (,name ,buxis-form)
              (loop
                (multiple-value-bind (,more? ,tag-var ,value-var) (,name)
                  ,@declarations
                  (unless ,more? (return ,result-form))
                  (tagbody ,@tag-statements)))))
           
           (hash-table
            (with-hash-table-iterator (,name ,buxis-form)
              (loop
                (multiple-value-bind (,more? ,tag-var ,value-var) (,name)
                  ,@declarations
                  (unless ,more? (return ,result-form))
                  (tagbody ,@tag-statements)))))

           (list
            (dolist2 (,tag-var ,value-var ,buxis-form ,result-form)
              ,@body)))))))

(setf (documentation 'dobux2 'function) "
DOBUX2 (tag-var value-var buxis-form &optional result-form) &body body => result
")


;;--------------------------------------------------------------------
;; every*
;;--------------------------------------------------------------------

(defun every* (predicate first-buxis &rest more-buxides)
  (check-type predicate (or symbol function))
  (if more-buxides
      (let ((buxides (cons first-buxis more-buxides)))
        (declare (type list buxides))
        (if (every #'sequencep buxides)
            (apply #'every predicate buxides)
            (loop :with iters := (mapcar #'%make-iterator buxides)
                  :for  vals := (loop :for iter :in iters
                                      ;; entry ::= (more? tag value)
                                      :for entry := (multiple-value-list (funcall iter))
                                      :if (first entry)
                                        :collect (third entry)
                                      :else :return nil)
                  :while vals
                  :always (apply predicate vals))))
      (etypecase first-buxis
        (sequence
         (every predicate first-buxis))
        (hash-table
         (loop :for v :being :the :hash-values :in first-buxis
               :always (funcall predicate v)))
        (array
         (do ((i 0 (1+ i))
              (size (array-total-size first-buxis)))
             ((<= size i) T)
           (declare (type array-index i size))
           (unless (funcall predicate (row-major-aref first-buxis i))
             (return NIL)))
         ;; (loop :for i :of-type array-index :from 0 :below (array-total-size first-buxis)
         ;;       :always (funcall predicate (row-major-aref first-buxis i)))
         )
        (lazy-sequence
         (with-lazy-seq-iterator (next-entry first-buxis)
           (loop
             (multiple-value-bind (more? index value) (next-entry)
               (declare (ignore index))
               (unless more?
                 (return-from every* T))
               (unless (funcall predicate value)
                 (return-from every* NIL)))))))))

(setf (documentation 'every* 'function) "
EVERY* predicate first-buxis &rest more-buxides => boolean

Notes
-----
 - If a hash-table is supplied as an argument, the result might depend
   on hash-table implementation of your cl systems.
    e.g., (every* ^xy(and (evenp x) (oddp y))
                  #(0 1) #{:foo 0 :bar 1})
          => T or NIL
")


;;--------------------------------------------------------------------
;; notevery*
;;--------------------------------------------------------------------

(defun notevery* (predicate first-buxis &rest more-buxides)
  (check-type predicate (or symbol function))
  (if more-buxides
      (let ((buxides (cons first-buxis more-buxides)))
        (declare (type list buxides))
        (if (every #'sequencep buxides)
            (apply #'notevery predicate buxides)
            (loop :with iters := (mapcar #'%make-iterator buxides)
                  :for  vals := (loop :for iter :in iters
                                      ;; entry ::= (more? tag value)
                                      :for entry := (multiple-value-list (funcall iter))
                                      :if (first entry)
                                        :collect (third entry)
                                      :else :return nil)
                  :while vals
                  :always (not (apply predicate vals)))))
      (etypecase first-buxis
        (sequence
         (notevery predicate first-buxis))
        (hash-table
         (loop :for v :being :the :hash-values :in first-buxis
               :always (not (funcall predicate v))))
        (array
         (do ((i 0 (1+ i))
              (size (array-total-size first-buxis)))
             ((<= size i) T)
           (declare (type array-index i size))
           (when (funcall predicate (row-major-aref first-buxis i))
             (return NIL)))
         ;; (loop :for i :of-type array-index :from 0 :below (array-total-size first-buxis)
         ;;       :always (not (funcall predicate (row-major-aref first-buxis i))))
         )
        (lazy-sequence
         (with-lazy-seq-iterator (next-entry first-buxis)
           (loop
             (multiple-value-bind (more? index value) (next-entry)
               (declare (ignore index))
               (unless more?
                 (return-from notevery* T))
               (when (funcall predicate value)
                 (return-from notevery* NIL)))))))))

(setf (documentation 'notevery* 'function) "
NOTEVERY* predicate first-buxis &rest more-buxides => boolean

Notes
-----
 - If a hash-table is supplied as an argument, the result might depend
   on hash-table implementation of your cl systems.
")


;;--------------------------------------------------------------------
;; some*
;;--------------------------------------------------------------------

(defun some* (predicate first-buxis &rest more-buxides)
  (check-type predicate (or symbol function))
  (if more-buxides
      (let ((buxides (cons first-buxis more-buxides)))
        (declare (type list buxides))
        (if (every #'sequencep buxides)
            (apply #'some predicate buxides)
            (loop :with iters := (mapcar #'%make-iterator buxides)
                  :for  vals := (loop :for iter :in iters
                                      ;; entry ::= (more? tag value)
                                      :for entry := (multiple-value-list (funcall iter))
                                      :if (first entry)
                                        :collect (third entry)
                                      :else :return nil)
                  :while vals
                  :thereis (apply predicate vals))))

      (etypecase first-buxis
        (sequence
         (some predicate first-buxis))

        (hash-table
         (loop :for v :being :the :hash-values :of first-buxis
               :thereis (funcall predicate v)))
        
        (array
         (loop :for i :of-type array-index
               :from 0 :below (array-total-size first-buxis)
               :thereis (funcall predicate (row-major-aref first-buxis i))))

        (lazy-sequence
         (with-lazy-seq-iterator (next-entry first-buxis)
           (loop
             (multiple-value-bind (more? index value) (next-entry)
               (declare (ignore index))
               (unless more?
                 (return-from some* NIL))
               (when-let (v (funcall predicate value))
                 (return-from some* v)))))))))

(setf (documentation 'SOME* 'function) "
SOME* predicate first-buxis &rest more-buxides => generalized-boolean

Notes
-----
 - If a hash-table is supplied as an argument, the result might depend
   on hash-table implementation of your cl systems.
")


;;--------------------------------------------------------------------
;; notany*
;;--------------------------------------------------------------------

(defun notany* (predicate first-buxis &rest more-buxides)
  (check-type predicate (or symbol function))
  (if more-buxides
      (let ((buxides (cons first-buxis more-buxides)))
        (declare (type list buxides))
        (if (every #'sequencep buxides)
            (apply #'notany predicate buxides)
            (loop :with iters := (mapcar #'%make-iterator buxides)
                  :for  vals := (loop :for iter :in iters
                                      ;; entry ::= (more? tag value)
                                      :for entry := (multiple-value-list (funcall iter))
                                      :if (first entry)
                                        :collect (third entry)
                                      :else :return nil)
                  :while vals
                  :never (apply predicate vals))))
      (etypecase first-buxis
        (sequence
         (notany predicate first-buxis))
        (hash-table
         (loop :for v :being :the :hash-values :in first-buxis
               :never (funcall predicate v)))
        (array
         (do ((i 0 (1+ i))
              (size (array-total-size first-buxis)))
             ((<= size i) NIL)
           (declare (type array-index i size))
           (unless (funcall predicate (row-major-aref first-buxis i))
             (return T)))
         ;; (loop :for i :of-type array-index
         ;;       :from 0 :below (array-total-size first-buxis)
         ;;       :never (funcall predicate (row-major-aref first-buxis i)))
         )
        (lazy-sequence
         (with-lazy-seq-iterator (next-entry first-buxis)
           (loop
             (multiple-value-bind (more? index value) (next-entry)
               (declare (ignore index))
               (unless more?
                 (return-from notany* T))
               (when (funcall predicate value)
                 (return-from notany* NIL)))))))))

(setf (documentation 'notany* 'function) "
NOTANY* predicate first-buxis &rest more-buxides => boolean

Notes
-----
 - If a hash-table is supplied as an argument, the result might depend
   on hash-table implementation of your cl systems.
")


;;--------------------------------------------------------------------
;; map*
;;--------------------------------------------------------------------
;; CHECK: nil case
;; string

(defun map*-no-more-buxides (result-type function buxis)
  (etypecase buxis
    (list
     (case result-type
       ((t sequence sequens buxis)
        (mapcar function buxis))
       
       ((lazy-sequence)
        (make-lazy-seq (mapcar function buxis)))
       
       ((hash-table)
        (let ((ht (make-hash-table)))
          (declare (type hash-table ht))
          (loop :for k :from 0
                :for v :in buxis
                :do (setf (gethash k ht) (funcall function v)))
          ht))
       
       ((string)
        (with-output-to-string (s)
          (dolist (v buxis)
            (princ (funcall function v) s))))
       
       ((array)
        (map 'vector function buxis))
       
       ;; nil, bit-vector, vector, ...
       (t (map result-type function buxis))))
    
    (vector
     (case result-type
       ((t vector buxis)
        (map (etypecase buxis
               (string     'string)
               (bit-vector 'bit-vector)
               (vector     'vector))
             function buxis))
       
       ((sequens)
        (map 'list function buxis))

       ((string)
        (with-output-to-string (s)
          (loop :for v :across buxis :do (princ (funcall function v) s))))
       
       ((lazy-sequence)
        (make-lazy-seq (map 'list function buxis)))
              
       ((hash-table)
        (let ((ht (make-hash-table)))
          (declare (type hash-table ht))
          (loop :for k :of-type array-index :from 0
                :for v :across buxis
                :do (setf (gethash k ht) (funcall function v)))
          ht))
       
       ((array)
        (map 'vector function buxis))
       
       ;; nil, sequence, bit-vector
       (t (map result-type function buxis))))
    
    (lazy-sequence
     (let ((result-lseq (lazy-map function buxis)))
       (declare (type lazy-sequence result-lseq))
       (if (or (integerp result-type)
               (eq :all result-type))
           (lazy-take result-type result-lseq) ;; (((EXPERIMENTAL)))
           (case result-type
             ((t lazy-sequence buxis)
              result-lseq)
             
             ((sequens)
              (lazy-take :all result-lseq))
             
             ((string)
              (with-output-to-string (s)
                (dolist (v (lazy-take :all result-lseq))
                  (princ v s))))
             
             ((array)
              (coerce (lazy-take :all result-lseq)
                      'vector))
             
             (hash-table
              (let ((ht (make-hash-table :test 'equal)))
                (declare (type hash-table ht))
                (loop :for k :from 0
                      :for v :in (lazy-take :all result-lseq)
                      :do (setf (gethash k ht) (funcall function v)))
                ht))
             
             (t
              (coerce (lazy-take :all result-lseq)
                      result-type))))))
    
    (hash-table
     (case result-type
       ((t hash-table buxis)
        (let ((ht (copy-empty-hash-table buxis)))
          (declare (type hash-table ht))
          (loop :for k :being :the :hash-keys :of buxis :using (:hash-value v)
                :do (setf (gethash k ht) (funcall function v)))
          ht))

       ((nil)
        (loop :for v :being :the :hash-values :of buxis
              :do (funcall function v)))

       ((sequens)
        (loop :for v :being :the :hash-values :of buxis
              :collect (funcall function v)))

       ((string)
        (with-output-to-string (s)
          (loop :for v :being :the :hash-values :of buxis
                :do (princ (funcall function v) s))))

       ((lazy-sequence)
        (loop :for v :being :the :hash-values :of buxis
              :collect (funcall function v) :into acc
              :finally (return (make-lazy-seq acc))))

       ((array)
        (coerce (loop :for v :being :the :hash-values :of buxis
                      :collect (funcall function v))
                'vector))
       
       (t
        (coerce (loop :for v :being :the :hash-values :of buxis
                      :collect (funcall function v))
                result-type))))
    
    (array
     (case result-type
       ((t array buxis)
        (do ((i 0 (1+ i))
             (ary (copy-empty-array buxis))
             (size (array-total-size buxis)))
            ((<= size i) ary)
          (declare (type array-index i size)
                   (type array ary))
          (setf (row-major-aref ary i)
                (funcall function (row-major-aref buxis i)))))
       
       ((nil)
        (dotimes (i (array-total-size buxis))
          (declare (type array-index i))
          (funcall function (row-major-aref buxis i))))
       
       ((string)
        (with-output-to-string (s)
          (dotimes (i (array-total-size buxis))
            (declare (type array-index i))
            (princ (funcall function (row-major-aref buxis i))
                   s))))
       
       ((sequens)
        (let ((result '()))
          (declare (type list result))
          (dotimes (i (array-total-size buxis))
            (declare (type array-index i))
            (push (funcall function (row-major-aref buxis i))
                  result))
          (nreverse result)))
       
       ((lazy-sequence)
        (let ((result '()))
          (declare (type list result))
          (dotimes (i (array-total-size buxis))
            (declare (type array-index i))
            (push (funcall function (row-major-aref buxis i))
                  result))
          (make-lazy-seq (nreverse result))))

       (t
        (let ((result '()))
          (declare (type list result))
          (dotimes (i (array-total-size buxis))
            (declare (type array-index i))
            (push (funcall function (row-major-aref buxis i))
                  result))
          (coerce (nreverse result) result-type)))))))


(declaim (inline get-type))
(defun %get-type (buxis)
  (etypecase buxis
    (list          'list)
    (bit-vector    'bit-vector)
    (string        'string)
    (vector        'vector)
    (hash-table    'hash-table)
    (array         'array)
    (lazy-sequence 'lazy-sequence)))


;; TODO:
;;  * Don't force when result-type is lazy-sequence and arguements
;;    contains a lazy-sequence.

(defun map* (result-type function first-buxis &rest more-buxides)
  (check-type function (or symbol function))
  (if (not more-buxides)
      (map*-no-more-buxides result-type function first-buxis)
      (with-iterators (get-next-values (cons first-buxis more-buxides))
        (if (eq result-type nil)

            (loop
              (destructuring-bind
                  (more? . vals) (multiple-value-list (get-next-values))
                (declare (type boolean more?))
                (unless more? (return))
                (apply function vals)))
            
            (let ((result '()))
              (declare (type list result))
              (loop
                (destructuring-bind
                    (more? . vals) (multiple-value-list (get-next-values))
                  (declare (type boolean more?))
                  (unless more? (return))
                  (push (apply function vals) result)))
              (setf result (nreverse result))

              (when (or (eq t result-type)
                        (eq 'buxis result-type))
                (setf result-type (%get-type first-buxis)))
              
              (case result-type
                ((list sequence sequens)
                 result)
                
                ((string)
                 (with-output-to-string (s)
                   (dolist (v result)
                     (princ v s))))
                
                ((lazy-sequence)
                 (make-lazy-seq result)) ; TODO: not compute
                
                ((hash-table)
                 (let ((ht (make-hash-table :test 'equal)) ; !!!
                       (i  0))
                   (dolist (v result)
                     (setf (gethash i ht) v)
                     (incf i))
                   ht))
                
                ((array)
                 (coerce result 'vector))
                
                (t
                 (coerce result result-type))))))))

(setf (documentation 'map* 'function) "
MAP* result-type function first-buxis &rest more-buxides => result

Notes
-----

 - If at least one hash-table is supplied and `result-type' is not
   hash-table nor t, the `result' might depend on hash-table
   implementation of your cl system.
    e.g., (map* 'list 'identity #{:foo 0 :bar 1 :baz 2})
          => (0 1 2) or (0 2 1), ...

 - If `first-tabula' is a lazy-sequence and `result-type' is integer or :all, 
   it returns a list that is generated by TAKE.
    e.g., (map* 5 '1+ #[0..]) <-> (take 5 (map* t '1+ #[0..]))
")


;;--------------------------------------------------------------------
;; reduce*
;;--------------------------------------------------------------------

(defun reduce* (function buxis &key key (initial-value nil ivp) from-end (start 0) end)
  (check-type function (or symbol function))
  (check-type key      (or null symbol function))

  (etypecase buxis
    (sequence
     (check-type start (integer 0 *))
     (check-type end   (or null (integer 0 *)))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (if ivp
         (reduce function buxis :key key :initial-value initial-value
                                :from-end from-end :start start :end end)
         (reduce function buxis :key key :from-end from-end :start start :end end)))

    (lazy-sequence
     (check-type start array-index)
     (check-type end   (or null array-index))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (let ((lst (if (zerop start)
                    (lazy-take (or end :all) buxis)
                    (lazy-take (if end (- end start) :all)
                               (lazy-drop start buxis)))))
       (if ivp
           (reduce function lst :key key :from-end from-end
                                :initial-value initial-value)
           (reduce function lst :key key :from-end from-end))))
    ;; TODO: key
    (hash-table
     (let ((num-entries (hash-table-count buxis)))
       (declare (type (integer 0 *) num-entries))
       (if (zerop num-entries)
           (if ivp initial-value (funcall function))
           (if (and (= 1 num-entries)
                    (not ivp))
               (loop :for v :being :the :hash-values :of buxis
                     :return v)
               (progn
                 (setf key (the (or function symbol) (or key #'identity)))
                 (let (result)
                   (with-hash-table-iterator (next-entry buxis)
                     (if ivp
                         (setf result initial-value)
                         (multiple-value-bind (_ __ value) (next-entry)
                           (declare (ignore _ __))
                           (setf result value)))
                     (loop
                       ;; we already ensured hash-table-count > 1.
                       (multiple-value-bind (more? _ value) (next-entry)
                         (declare (ignore _))
                         (unless more?
                           (return result))
                         (setf result
                               (funcall function result (funcall key value))))))))))))
    
    ;; TODO: key
    (array
     (let ((size (array-total-size buxis)))
       (declare (type array-index size))
       (if (zerop size)
           (if ivp initial-value (funcall function))
           (if (and (= 1 size)
                    (not ivp))
               (row-major-aref buxis 0)
               (progn
                 (check-type start (or list array-index))
                 (check-type end   (or list array-index))
                 (when (listp start)
                   (setf start (the array-index (apply #'array-row-major-index buxis start))))
                 (when (and end (listp end))
                   (setf end (the array-index (apply #'array-row-major-index buxis end))))
                 (when (and end (< end start))
                   (error "[~S , ~S) is bad interval." start end))
                 (setf key (the (or function symbol) (or key #'identity)))
                 (if from-end
                     (do ((i (1- (or end size)) (1- i))
                          (result initial-value
                                  (funcall function result
                                           (funcall key (row-major-aref buxis i)))))
                         ((< i start) result)
                       (declare (type array-index-1 i)))
                     ;; (loop :for i :of-type array-index-1
                     ;;       :downfrom (1- (or end size)) :to start
                     ;;       :for v := (row-major-aref buxis i)
                     ;;       :for result := initial-value
                     ;;         :then (funcall function result (funcall key v))
                     ;;       :finally (return result))
                     (do ((i 0 (1+ i))
                          (end (or end size))
                          (result initial-value
                                  (funcall function result
                                           (funcall key (row-major-aref buxis i)))))
                         ((<= end i) result)
                       (declare (type array-index i)))
                     ;; (loop :for i :of-type array-index
                     ;;       :from start :below (or end size)
                     ;;       :for v := (row-major-aref buxis i)
                     ;;       :for result := initial-value
                     ;;         :then (funcall function result (funcall key v))
                     ;;       :finally (return result))
                     ))))))))

(setf (documentation 'reduce* 'function) "
REDUCE* function buxis &key key initial-value from-end (start 0) end => result

Notes
-----
 - If `buxis' contains exactly one value and no `initial-value' is
   given, then that value is returned and `function' is not called.

 - If `buxis' is empty and an `initial-value' is given,
   then the `initial-value' is returned and `function' is not called.

 - If `buxis' is empty and no `initial-value' is given, then the
   `function' is called with zero arguments, and REDUCE* returns
   whatever function does.

 - If `buxis' is a hash-table, the `result' might depend on hash-table
   implementation of your cl system.
   e.g., (reduce* ^av(list a v) #{:foo 0 :bar 1 :baz 2})
         => ((0 2) 1) or ((0 1) 2), ...

 - If `buxis' is an array, it accepts subscript-list as `start/end' value.

References
----------
 - CLHS, Function REDUCE.
 - CLtL2, Section 14.2. Concatenating, Mapping, and Reducing Sequences.
")


;;--------------------------------------------------------------------
;; scan*
;;--------------------------------------------------------------------
;; TODO

#-common-lisp
(defun scan* (function buxis &key key (initial-value nil ivp) from-end (start 0) end)
  (check-type function (or symbol function))
  (check-type key      (or null symbol function))

  (etypecase buxis
    (sequence
     (if ivp
         (scan function buxis :key key :initial-value initial-value
                              :from-end from-end :start start :end end)
         (scan function buxis :key key :from-end from-end :start start :end end)))

    (lazy-sequence
     )

    (hash-table
     )

    (array
     (check-type start (or list array-index))
     (check-type end   (or list array-index))
     (when (listp start)
       (setf start (the array-index (apply #'array-row-major-index buxis start))))
     (when (and end (listp end))
       (setf end (the array-index (apply #'array-row-major-index buxis end))))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (setf end (the array-index (or end (array-total-size buxis))))
     (setf key (the (or function symbol) (or key #'identity)))
     (let ((len (array-total-size buxis)))
       (cond ((zerop len)
              (if ivp
                  (list initial-value)
                  (list (funcall function))))
             ((and (= 1 len)
                   (not ivp))
              (list (row-major-aref buxis 0)))
             (t
              (if from-end
                  (loop :with result := '()
                        :for i :of-type array-index :downfrom (1- end) :to start
                        :for prev := (row-major-aref buxis (1- end)) :then (funcall function prev curr)
                        :for curr := (row-major-aref buxis (- end 2)) :then (row-major-aref buxis i)
                        :do (push prev result)
                        :finally (return (nreverse (push (funcall function prev curr)
                                                         result))))
                  (loop :with result := '()
                        :for i :of-type array-index :from start :below end
                        :for prev := (row-major-aref buxis 0) :then (funcall function prev curr)
                        :for curr := (row-major-aref buxis 1) :then (row-major-aref buxis i)
                        :do (push prev result)
                        :finally (return (nreverse (push (funcall function prev curr)
                                                         result)))
                        ))))))))



;;--------------------------------------------------------------------
;; count*
;;--------------------------------------------------------------------

(defun count* (item buxis &key key (test #'eql) from-end (start 0) end)

  (check-type key  (or symbol function))
  (check-type test (or function symbol))
  
  (etypecase buxis
    (sequence
     (check-type start (integer 0 *))
     (check-type end   (or null (integer 0 *)))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (count item buxis
            :key key :test test :from-end from-end :start start :end end))

    (lazy-sequence
     (check-type start array-index)
     (check-type end   (or null array-index))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (if (zerop start)
         (count item (lazy-take (or end :all) buxis)
                :key key :test test :from-end from-end)
         (count item (lazy-take (if end (- end start) :all)
                                (lazy-drop start buxis))
                :key key :test test :from-end from-end)))

    (hash-table
     (if key
         (loop :for v :being :the :hash-values :of buxis
               :count (funcall test item (funcall key v)))
         (loop :for v :being :the :hash-values :of buxis
               :count (funcall test item v))))

    ;; TODO:
    (array
     (check-type start (or list array-index))
     (check-type end   (or list array-index))
     (when (listp start)
       (setf start (the array-index (apply #'array-row-major-index buxis start))))
     (when (and end (listp end))
       (setf end (the array-index (apply #'array-row-major-index buxis end))))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     
     (setf end (the array-index (or end (array-total-size buxis))))
     (setf key (the (or function symbol) (or key #'identity)))
     (if from-end
         (do ((i (1- end) (1- i))
              (count 0 (if (funcall test item (funcall key (row-major-aref buxis i)))
                           (1+ count)
                           count)))
             ((< i start) count)
           (declare (type array-index-1 i count)))
         ;; (loop :for i :of-type array-index-1
         ;;       :downfrom (1- end) :to start
         ;;       :for v := (row-major-aref buxis i)
         ;;       :count (funcall test item (funcall key v)))
         (do ((i start (1+ i))
              (count 0 (if (funcall test item (funcall key (row-major-aref buxis i)))
                           (1+ count)
                           count)))
             ((<= end i) count)
           (declare (type array-index i count)))
         ;; (loop :for i :of-type array-index
         ;;       :from start :below end
         ;;       :for v := (row-major-aref buxis i)
         ;;       :count (funcall test item (funcall key v)))
         ))))

(setf (documentation 'count* 'function) "
COUNT* item buxis &key key (test 'eql) from-end (start 0) end => result

Note
----
 - If `buxis' is an array, it accepts subscript-list as `start/end' value.
")


;;--------------------------------------------------------------------
;; count-if*
;;--------------------------------------------------------------------

(defun count-if* (predicate buxis &key key (start 0) end from-end)

  (check-type predicate (or symbol function))
  (check-type key       (or symbol function))

  (etypecase buxis
    (sequence
     (check-type start (integer 0 *))
     (check-type end   (or null (integer 0 *)))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (count-if predicate buxis
               :key key :start start :end end :from-end from-end))

    (lazy-sequence
     (check-type start array-index)
     (check-type end   (or null array-index))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (if (zerop start)
         (count-if predicate (lazy-take (or end :all) buxis)
                   :key key :from-end from-end)
         (count-if predicate (lazy-take (if end (- end start) :all)
                                        (lazy-drop start buxis))
                   :key key :from-end from-end)))

    (hash-table
     (setf key (the (or function symbol) (or key #'identity)))
     (loop :for v :being :the :hash-values :of buxis
           :count (funcall predicate (funcall key v))))

    (array
     (check-type start (or list array-index))
     (check-type end   (or list array-index))
     (when (listp start)
       (setf start (the array-index (apply #'array-row-major-index buxis start))))
     (when (and end (listp end))
       (setf end (the array-index (apply #'array-row-major-index buxis end))))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (setf end (the array-index (or end (array-total-size buxis))))
     (setf key (the (or function symbol) (or key #'identity)))
     (if from-end
         (do ((i (1- end) (1- i))
              (count 0 (if (funcall predicate (funcall key (row-major-aref buxis i)))
                           (1+ count)
                           count)))
             ((< i start) count)
           (declare (type array-index-1 i count)))
         ;; (loop :for i :of-type array-index-1
         ;;       :downfrom (1- end) :downto start
         ;;       :for v := (row-major-aref buxis i)
         ;;       :count (funcall predicate (funcall key v)))
         (do ((i start (1+ i))
              (count 0 (if (funcall predicate (funcall key (row-major-aref buxis i)))
                           (1+ count)
                           count)))
             ((<= end i) count)
           (declare (type array-index i count)))
         ;; (loop :for i :of-type array-index
         ;;       :from start :below end
         ;;       :for v := (row-major-aref buxis i)
         ;;       :count (funcall predicate (funcall key v)))
         ))))

(setf (documentation 'count-if* 'function) "
COUNT-IF* predicate buxis &key key (start 0) end from-end => result

Note
----
 - If `buxis' is an array, it accepts subscript-list as `start/end' value.
")


;;--------------------------------------------------------------------
;; count-if-not*
;;--------------------------------------------------------------------

(defun count-if-not* (predicate buxis &key key (start 0) end from-end)
  (count-if* (complement (ensure-function predicate)) buxis
             :key key :start start :end end :from-end from-end))

(setf (documentation 'count-if-not* 'function) "
COUNT-IF-NOT* predicate buxis &key key (start 0) end from-end => result

Note
----
 - If `buxis' is an array, it accepts subscript-list as `start/end' value.
")


;;--------------------------------------------------------------------
;; find*
;;--------------------------------------------------------------------

(defun find* (item buxis &key key (test 'eql) (start 0) end from-end)

  (check-type key  (or function symbol))
  (check-type test (or function symbol))
  
  (etypecase buxis
    (sequence
     (check-type start (integer 0 *))
     (check-type end   (or null (integer 0 *)))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (find item buxis
           :key key :test test :start start :end end :from-end from-end))

    (lazy-sequence
     (check-type start array-index)
     (check-type end   (or null array-index))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (setf key (the (or function symbol) (or key #'identity)))
     (unless (zerop start)
       (setf buxis (lazy-drop start buxis))
       (when end
         (setf end (- end start))))
     (with-lazy-seq-iterator (next-entry buxis)
       (loop
         (multiple-value-bind (more? index value) (next-entry)
           (declare (type boolean more?)
                    (type array-index index))
           (when (or (not more?)
                     (and end (<= end index)))
             (return-from find* nil))
           (when (funcall test item (funcall key value))
             (return-from find* value))))))

    (hash-table
     (setf key (the (or function symbol) (or key #'identity)))
     (loop :for v :being :the :hash-values :of buxis
           :when (funcall test item (funcall key v))
             :return v))
    
    (array
     (check-type start (or list array-index))
     (check-type end   (or list array-index))
     (when (listp start)
       (setf start (the array-index (apply #'array-row-major-index buxis start))))
     (when (and end (listp end))
       (setf end (the array-index (apply #'array-row-major-index buxis end))))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (setf end (the array-index (or end (array-total-size buxis))))
     (setf key (the (or function symbol) (or key #'identity)))
     (if from-end
         (do ((i (1- end) (1- i)))
             ((< i start) NIL)
           (declare (type array-index-1 i))
           (when (funcall test item (funcall key (row-major-aref buxis i)))
             (return (row-major-aref buxis i))))
         ;; (loop :for i :of-type array-index-1
         ;;       :downfrom (1- end) :to start
         ;;       :for v := (row-major-aref buxis i)
         ;;       :when (funcall test item (funcall key v))
         ;;         :return v)
         (do ((i start (1+ i)))
             ((<= end i) NIL)
           (declare (type array-index i))
           (when (funcall test item (funcall key (row-major-aref buxis i)))
             (return (row-major-aref buxis i))))
         ;; (loop :for i :of-type array-index
         ;;       :from start :below end
         ;;       :for v := (row-major-aref buxis i)
         ;;       :when (funcall test item (funcall key v))
         ;;         :return v)
         ))))

(setf (documentation 'find* 'function) "
FIND* item buxis &key key (test 'eql) (start 0) end from-end => result

Note
----
 - If `buxis' is an array, it accepts subscript-list as `start/end' value.
")


;;--------------------------------------------------------------------
;; find-if*
;;--------------------------------------------------------------------

(defun find-if* (predicate buxis &key key (start 0) end from-end)

  (check-type predicate (or function symbol))
  (check-type key       (or function symbol))
  
  (etypecase buxis
    (sequence
     (check-type start (integer 0 *))
     (check-type end   (or null (integer 0 *)))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (find-if predicate buxis
              :key key :start start :end end :from-end from-end))

    (lazy-sequence
     (check-type start array-index)
     (check-type end   (or null array-index))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (setf key (the (or function symbol) (or key #'identity)))
     (unless (zerop start)
       (setf buxis (the lazy-sequence (lazy-drop start buxis)))
       (when end
         (setf end (the array-index (- end start)))))
     (with-lazy-seq-iterator (next-entry buxis)
       (loop
         (multiple-value-bind (more? index value) (next-entry)
           (declare (type boolean more?)
                    (type array-index index))
           (when (or (not more?)
                     (and end (<= end index)))
             (return-from find-if* nil))
           (when (funcall predicate (funcall key value))
             (return-from find-if* value))))))

    (hash-table
     (setf key (the (or function symbol) (or key #'identity)))
     (loop :for v :being :the :hash-values :of buxis
           :when (funcall predicate (funcall key v))
             :return v))

    (array
     (check-type start (or list array-index))
     (check-type end   (or list array-index))
     (when (listp start)
       (setf start (the array-index (apply #'array-row-major-index buxis start))))
     (when (and end (listp end))
       (setf end (the array-index (apply #'array-row-major-index buxis end))))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (setf end (the array-index (or end (array-total-size buxis))))
     (setf key (the (or function symbol) (or key #'identity)))
     (if from-end
         (do ((i (1- end) (1- i)))
             ((< i start) NIL)
           (declare (type array-index-1 i))
           (when (funcall predicate (funcall key (row-major-aref buxis i)))
             (return (row-major-aref buxis i))))
         ;; (loop :for i :of-type array-index-1
         ;;       :downfrom (1- end) :to start
         ;;       :for v := (row-major-aref buxis i)
         ;;       :when (funcall predicate (funcall key v))
         ;;         :return v)
         (do ((i start (1+ i)))
             ((<= end i) NIL)
           (declare (type array-index i))
           (when (funcall predicate (funcall key (row-major-aref buxis i)))
             (return (row-major-aref buxis i))))
         ;; (loop :for i :of-type array-index
         ;;       :from start :below end
         ;;       :for v := (row-major-aref buxis i)
         ;;       :when (funcall predicate (funcall key v))
         ;;         :return v)
         ))))

(setf (documentation 'find-if* 'function) "
find-if* predicate buxis &key key (start 0) end from-end => result

note
----
 - if `buxis' is an array, it accepts subscript-list as `start/end' value.
")


;;--------------------------------------------------------------------
;; find-if-not*
;;--------------------------------------------------------------------

(defun find-if-not* (predicate buxis &key key (start 0) end from-end)
  (find-if* (complement (ensure-function predicate)) buxis
            :key key :start start :end end :from-end from-end))

(setf (documentation 'find-if-not* 'function) "
find-if-not* predicate buxis &key key (start 0) end from-end => result

note
----
 - if `buxis' is an array, it accepts subscript-list as `start/end' value.
")


;;--------------------------------------------------------------------
;; position*
;;--------------------------------------------------------------------

(defun position* (item buxis &key key (test 'eql) (start 0) end from-end subscript)

  (check-type key (or function symbol))
  
  (etypecase buxis
    (sequence
     (check-type start (integer 0 *))
     (check-type end   (or null (integer 0 *)))
     (when (and end (< end start))
       (error "[~s , ~s) is bad interval." start end))
     (position item buxis
               :key key :test test :start start :end end :from-end from-end))

    (lazy-sequence
     (check-type start array-index)
     (check-type end   (or null array-index))
     (when (and end (< end start))
       (error "[~s , ~s) is bad interval." start end))
     (setf key (the (or function symbol) (or key #'identity)))
     (unless (zerop start)
       (setf buxis (the lazy-sequence (lazy-drop start buxis)))
       (when end
         (setf end (the array-index (- end start)))))
     (with-lazy-seq-iterator (next-entry buxis)
       (loop
         (multiple-value-bind (more? index value) (next-entry)
           (declare (type boolean more?)
                    (type array-index index))
           (when (or (not more?)
                     (and end (<= end index)))
             (return-from position* nil))
           (when (funcall test item (funcall key value))
             (return-from position*
               (if (zerop start)
                   index
                   (+ index start))))))))

    (hash-table
     (setf key (the (or function symbol) (or key #'identity)))
     (loop :for v :being :the :hash-values :of buxis :using (:hash-key k)
           :when (funcall test item (funcall key v))
             :return k))

    (array
     (check-type start (or list array-index))
     (check-type end   (or list array-index))
     (when (listp start)
       (setf start (the array-index (apply #'array-row-major-index buxis start))))
     (when (and end (listp end))
       (setf end (the array-index (apply #'array-row-major-index buxis end))))
     (when (and end (< end start))
       (error "[~s , ~s) is bad interval." start end))
     (setf end (the array-index (or end (array-total-size buxis))))
     (setf key (the (or function symbol) (or key #'identity)))
     (if from-end
         (do ((i (1- end) (1- i)))
             ((< i start) nil)
           (declare (type array-index-1 i))
           (when (funcall test item (funcall key (row-major-aref buxis i)))
             (return (if subscript (array-subscripts buxis i) i))))
         ;; (loop :for i :of-type array-index-1
         ;;       :downfrom (1- end) :to start
         ;;       :for v := (row-major-aref buxis i)
         ;;       :when (funcall test item (funcall key v))
         ;;         :return (if subscript (array-subscripts buxis i) i))         
         (do ((i start (1+ i)))
             ((<= end i) nil)
           (declare (type array-index i))
           (when (funcall test item (funcall key (row-major-aref buxis i)))
             (return (if subscript (array-subscripts buxis i) i))))
         ;; (loop :for i :of-type array-index
         ;;       :from start :below end
         ;;       :for v := (row-major-aref buxis i)
         ;;       :when (funcall test item (funcall key v))
         ;;         :return (if subscript (array-subscripts buxis i) i))
         ))))

(setf (documentation 'position* 'function) "
position* item buxis &key key (test 'eql) (start 0) end from-end subscripts => result

notes
-----
 - if `buxis' is an array, it accepts subscript-list as `start/end' value.
 - if `buxis' is an array and `subcript' is t, it returns sbscript-list.
")


;;--------------------------------------------------------------------
;; position-if*
;;--------------------------------------------------------------------

(defun position-if* (predicate buxis &key key (start 0) end from-end subscript)

  (check-type predicate (or function symbol))
  (check-type key       (or function symbol))
  
  (etypecase buxis
    (sequence
     (check-type start (integer 0 *))
     (check-type end   (or null (integer 0 *)))
     (when (and end (< end start))
       (error "[~s , ~s) is bad interval." start end))
     (position-if predicate buxis
                  :key key :start start :end end :from-end from-end))

    (lazy-sequence
     (check-type start array-index)
     (check-type end   (or null array-index))
     (when (and end (< end start))
       (error "[~s , ~s) is bad interval." start end))
     (setf key (the (or function symbol) (or key #'identity)))
     (unless (zerop start)
       (setf buxis (the lazy-sequence (lazy-drop start buxis)))
       (when end
         (setf end (the array-index (- end start)))))
     (with-lazy-seq-iterator (next-entry buxis)
       (loop
         (multiple-value-bind (more? index value) (next-entry)
           (declare (type boolean more?)
                    (type array-index index))
           (when (or (not more?)
                     (and end (<= end index)))
             (return-from position-if* nil))
           (when (funcall predicate (funcall key value))
             (return-from position-if*
               (if (zerop start)
                   index
                   (+ index start))))))))

    (hash-table
     (setf key (the (or function symbol) (or key #'identity)))
     (loop :for k :being :the :hash-keys :of buxis :using (:hash-value v)
           :when (funcall predicate (funcall key v))
             :return k))

    (array
     (check-type start (or list array-index))
     (check-type end   (or list array-index))
     (when (listp start)
       (setf start (the array-index (apply #'array-row-major-index buxis start))))
     (when (and end (listp end))
       (setf end (the array-index (apply #'array-row-major-index buxis end))))
     (when (and end (< end start))
       (error "[~s , ~s) is bad interval." start end))
     (setf key (the (or function symbol) (or key #'identity)))
     (setf end (the array-index (or end (array-total-size buxis))))
     (if from-end
         (do ((i (1- end) (1- i)))
             ((< i start) nil)
           (declare (type array-index-1 i))
           (when (funcall predicate (funcall key (row-major-aref buxis i)))
             (return (if subscript (array-subscripts buxis i) i))))
         ;; (loop :for i :of-type array-index-1
         ;;       :downfrom (1- end) :to start
         ;;       :for v := (row-major-aref buxis i)
         ;;       :when (funcall predicate (funcall key v))
         ;;         :return (if subscript (array-subscripts buxis i) i))
         (do ((i start (1+ i)))
             ((<= end i) nil)
           (declare (type array-index i))
           (when (funcall predicate (funcall key (row-major-aref buxis i)))
             (return (if subscript (array-subscripts buxis i) i))))
         ;; (loop :for i :of-type array-index
         ;;       :from start :below end
         ;;       :for v := (row-major-aref buxis i)
         ;;       :when (funcall predicate (funcall key v))
         ;;         :return (if subscript (array-subscripts buxis i) i))
         ))))

(setf (documentation 'position-if* 'function) "
position-if* predicate buxis &key key (start 0) end from-end subscripts => result

notes
-----
 - if `buxis' is an array, it accepts subscript-list as `start/end' value.
 - if `buxis' is an array and `subcripts' is t, it returns sbscript-list.
")


;;--------------------------------------------------------------------
;; position-if-not*
;;--------------------------------------------------------------------

(defun position-if-not* (predicate buxis &key key (start 0) end from-end subscript)
  (position-if* (complement (ensure-function predicate)) buxis
                :key key :start start :end end :from-end from-end :subscript subscript))


(setf (documentation 'position-if-not* 'function) "
POSITION-IF-NOT* predicate buxis &key key (start 0) end from-end subscripts => result

Notes
-----
 - If `buxis' is an array, it accepts subscript-list as `start/end' value.
 - If `buxis' is an array and `subcript' is T, it returns sbscript-list.
")


;;--------------------------------------------------------------------
;; remove*
;;--------------------------------------------------------------------

(defun remove* (item buxis &key key count (test 'eql) (start 0) end from-end)

  (check-type key   (or function symbol))
  (check-type count (or null (integer 0 *)))
  (check-type test  (or function symbol))
  (check-type start (integer 0 *))
  (check-type end   (or null (integer 0 *)))
  (when (and end (< end start))
    (error "[~S , ~S) is bad interval." start end))
  
  (etypecase buxis
    (sequence
     (remove item buxis :key key :count count :test test
                        :start start :end end :from-end from-end))
    
    (lazy-sequence
     (lazy-remove item buxis :key key :count count :test test
                             :start start :end end))
    
    (hash-table
     (setf key (the (or function symbol) (or key #'identity)))
     (let ((result (copy-hash-table buxis)))
       (declare (type hash-table result))
       (loop :for k :being :the :hash-keys :of buxis :using (:hash-value v)
             :when (funcall test item (funcall key v))
               :do (remhash k result))
       result))
    
    (array
     (error "There does not exist REMOVE* for array."))))

(setf (documentation 'remove* 'function) "
REMOVE* item buxis &key key count (test 'eql) (start 0) end from-end => result
")


;;--------------------------------------------------------------------
;; remove-if*
;;--------------------------------------------------------------------

(defun remove-if* (predicate buxis &key key count (start 0) end from-end)

  (check-type predicate (or function symbol))
  (check-type key       (or function symbol))
  (check-type count     (or null (integer 0 *)))
  (check-type start     (integer 0 *))
  (check-type end       (or null (integer 0 *)))
  (when (and end (< end start))
    (error "[~S , ~S) is bad interval." start end))
  
  (etypecase buxis
    (sequence
     (remove-if predicate buxis :key key :count count
                                :start start :end end :from-end from-end))

    (lazy-sequence
     (lazy-remove-if predicate buxis :key key :count count
                                     :start start :end end))

    (hash-table
     (setf key (the (or function symbol) (or key #'identity)))
     (let ((result (copy-hash-table buxis)))
       (declare (type hash-table result))
       (loop :for k :being :the :hash-keys :of buxis :using (:hash-value v)
             :until (and count (zerop count))
             :when (funcall predicate (funcall key v))
               :do (remhash k result)
                   (when count (decf count)))
       result))

    (array
     (error "There does not exist REMOVE-IF* for array."))))

(setf (documentation 'remove-if* 'function) "
REMOVE-IF* predicate buxis &key key count (start 0) end from-end => result
")


;;--------------------------------------------------------------------
;; remove-if-not*
;;--------------------------------------------------------------------

(defun remove-if-not* (predicate buxis &key key count (start 0) end from-end)
  (remove-if* (complement (ensure-function predicate)) buxis
              :key key :count count :start start :end end :from-end from-end))

(setf (documentation 'remove-if-not* 'function) "
REMOVE-IF-NOT* predicate buxis &key key count (start 0) end from-end => result
")


;;--------------------------------------------------------------------
;; collect-if*
;;--------------------------------------------------------------------
;; TODO: merge

(defun collect-if*-sequence (predicate sequence &key key count (from 0) below from-end)
  (setf below (or below (length sequence)))
  (if (not count)
      (etypecase sequence
        (list
         (loop :for e :in (if from-end
                              (reverse (subseq sequence from below))
                              (subseq sequence from below))
               :when (funcall predicate (funcall key e))
                 :collect e :into result
               :finally (return (if from-end (reverse result) result))))
        (vector
         (if from-end
             (do ((i (1- below) (1- i))
                  (acc '() (if (funcall predicate (funcall key (aref sequence i)))
                               (cons (aref sequence i) acc)
                               acc)))
                 ((< i from) acc)
               (declare (type array-index-1 i)
                        (type list acc)))
             ;; (loop :for i :of-type array-index-1 :downfrom (1- below) :to from
             ;;       :for v := (aref sequence i)
             ;;       :when (funcall predicate (funcall key v))
             ;;         :collect v :into acc
             ;;       :finally (return (nreverse acc)))
             (do ((i from (1+ i))
                  (acc '() (if (funcall predicate (funcall key (aref sequence i)))
                               (cons (aref sequence i) acc)
                               acc)))
                 ((<= below i) (nreverse acc))
               (declare (type array-index i)
                        (type list acc)))
             ;; (loop :for i :of-type array-index :from from :below below
             ;;       :for v := (aref sequence i)
             ;;       :when (funcall predicate (funcall key v))
             ;;         :collect v)
             )))
      
      ;; Count exists case:
      (if (zerop count)
          '()
          (etypecase sequence
            (list
             (loop :for e :in (if from-end
                                  (reverse (subseq sequence from below))
                                  (subseq sequence from below))
                   :until (zerop count)
                   :when (funcall predicate (funcall key e))
                     :do (decf count) :and :collect e :into result
                   :finally (return (if from-end (reverse result) result))))
            (vector
             (if from-end
                 (do ((i (1- below) (1- i))
                      (acc '()))
                     ((< i from) acc)
                   (declare (type array-index-1 i)
                            (type list acc))
                   (when (funcall predicate (funcall key (aref sequence i)))
                     (push (aref sequence i) acc)
                     (decf count)
                     (when (zerop count)
                       (return acc))))
                 ;; (loop :for i :of-type array-index-1 :downfrom (1- below) :to from
                 ;;       :for v := (aref sequence i)
                 ;;       :until (zerop count)
                 ;;       :when (funcall predicate (funcall key v))
                 ;;         :do (decf count) :and :collect v :into acc
                 ;;       :finally (return (nreverse acc)))
                 (do ((i from (1+ i))
                      (acc '()))
                     ((<= below i) (nreverse acc))
                   (declare (type array-index i)
                            (type list acc))
                   (when (funcall predicate (funcall key (aref sequence i)))
                     (push (aref sequence i) acc)
                     (decf count)
                     (when (zerop count)
                       (return (nreverse acc)))))
                 ;; (loop :for i :of-type array-index :from from :below below
                 ;;       :for v := (aref sequence i)
                 ;;       :until (zerop count)
                 ;;       :when (funcall predicate (funcall key v))
                 ;;         :do (decf count) :and :collect v)
                 ))))))

(defun collect-if*-lazy-sequence-without-below
    (predicate lseq key count from)
  (let ((result '()))
    (unless (zerop from)
      (setf lseq (lazy-drop from lseq)))
    (if (not count)
        (with-lazy-seq-iterator (next lseq)
          (loop
            (multiple-value-bind (more? index value) (next)
              (declare (ignore index))
              (unless more?
                (return))
              (when (funcall predicate (funcall key value))
                (push value result)))))
        (with-lazy-seq-iterator (next lseq)
          (loop
            (multiple-value-bind (more? index value) (next)
              (declare (ignore index))
              (when (or (not more?)
                        (zerop count))
                (return))
              (when (funcall predicate (funcall key value))
                (push value result)
                (decf count))))))
    (nreverse result)))

(defun collect-if*-array (predicate array key count from below from-end)
  (setf below (or below (array-total-size array)))
  (if (not count)
      (if from-end
          (do ((i (1- below) (1- i))
               (acc '() (if (funcall predicate (funcall key (row-major-aref array i)))
                            (cons (row-major-aref array i) acc)
                            acc)))
              ((< i from) acc)
            (declare (type array-index-1 i)
                     (type list acc)))
          ;; (loop :for i :of-type array-index-1 :downfrom (1- below) :to from
          ;;       :for v := (row-major-aref array i)
          ;;       :when (funcall predicate (funcall key v))
          ;;         :collect v :into acc
          ;;       :finally (return (nreverse acc)))
          (do ((i from (1+ i))
               (acc '() (if (funcall predicate (funcall key (row-major-aref array i)))
                            (cons (row-major-aref array i) acc)
                            acc)))
              ((<= below i) (nreverse acc))
            (declare (type array-index i)
                     (type list acc)))
          ;; (loop :for i :of-type array-index :from from :below below
          ;;       :for v := (row-major-aref array i)
          ;;       :when (funcall predicate (funcall key v))
          ;;         :collect v)
          )
      (if (zerop count)
          '()
          (if from-end
              (do ((i (1- below) (1- i))
                   (acc '()))
                  ((< i from) acc)
                (declare (type array-index-1 i)
                         (type list acc))
                (when (funcall predicate (funcall key (row-major-aref array i)))
                  (push (row-major-aref array i) acc)
                  (decf count)
                  (when (zerop count)
                    (return acc))))
              ;; (loop :for i :of-type array-index-1 :downfrom (1- below) :to from
              ;;       :for v := (row-major-aref array i)
              ;;       :until (zerop count)
              ;;       :when (funcall predicate (funcall key v))
              ;;         :do (decf count) :and :collect v :into acc
              ;;       :finally (return (nreverse acc)))
              (do ((i from (1+ i))
                   (acc '()))
                  ((<= below i) (nreverse acc))
                (declare (type array-index i)
                         (type list acc))
                (when (funcall predicate (funcall key (row-major-aref array i)))
                  (push (row-major-aref array i) acc)
                  (decf count)
                  (when (zerop count)
                    (return (nreverse acc)))))
              ;; (loop :for i :of-type array-index :from from :below below
              ;;       :for v := (row-major-aref array i)
              ;;       :until (zerop count)
              ;;       :when (funcall predicate (funcall key v))
              ;;         :do (decf count) :and :collect v)
              ))))


(defun collect-if* (predicate buxis &key key count (from 0) below from-end)

  (check-type predicate (or function symbol))
  (check-type key       (or function symbol))
  (check-type count     (or null (integer 0 *)))
  (setf key (the (or function symbol) (or key #'identity)))
  
  (etypecase buxis
    
    (sequence
     (check-type from  (integer 0 *))
     (check-type below (or null (integer 0 *)))
     (when (and below (< below from))
       (error "[~S , ~S) is bad interval." from below))
     (collect-if*-sequence
      predicate buxis :key key :count count :from from :below below :from-end from-end))

    (lazy-sequence
     (check-type from  array-index)
     (check-type below (or null array-index))
     (when (and below (< below from))
       (error "[~S , ~S) is bad interval." from below))
     (if below
         (if (zerop from)
             (collect-if*-sequence predicate (lazy-take (or below :all) buxis)
                                   :key key :count count :from-end from-end)
             (collect-if*-sequence predicate (lazy-take (if below (- below from) :all)
                                                        (lazy-drop from buxis))
                                   :key key :count count :from-end from-end))
         (collect-if*-lazy-sequence-without-below
          predicate buxis key count from)))

    (array
     (check-type from  (or list array-index))
     (check-type below (or list array-index))
     (when (listp from)
       (setf from (apply #'array-row-major-index buxis from)))
     (when (and below (listp below))
       (setf below (apply #'array-row-major-index buxis below)))
     (when (and below (< below from))
       (error "[~S , ~S) is bad interval." from below))
     (collect-if*-array predicate buxis key count from below from-end))

    (hash-table
     (warn-order-of-hash-table-entries)
     (if (not count)
         (loop :for v :being :the :hash-values :of buxis
               :when (funcall predicate (funcall key v))
                 :collect v)
         (loop :for v :being :the :hash-values :of buxis
               :until (zerop count)
               :when (funcall predicate (funcall key v))
                 :do (decf count) :and :collect v)))))


(setf (documentation 'collect-if* 'function) "
COLLECT-IF* predicate buxis &key key count (from 0) below from-end => result
It returns list its elements satisfies `predicate'.

COLLECT-IF* is an abstraction from (loop :for x ... :when ... :collect x).

The difference from REMOVE-IF-NOT* is the followings:

 0. COLLECT-IF* always returns LIST.
    e.g., (collect-if*    #'oddp #(0 1 2 3 4 5)) =>  (1 3 5)
          (remove-if-not* #'oddp #(0 1 2 3 4 5)) => #(1 3 5)

 1. `count' is maximum length of `result'.
    e.g., (collect-if*    #'oddp '(0 1 2 3 4 5) :count 2) => (1 3)
          (remove-if-not* #'oddp '(0 1 2 3 4 5) :count 2) => (1 3 4 5)

 2. `from/below' is the range for collecting.
    e.g., (collect-if*    #'oddp '(0 1 2 3 4 5) :from  1 :below 3) => (1)
          (remove-if-not* #'oddp '(0 1 2 3 4 5) :start 1 :end   3) => (0 1 3 4 5)

Notes
-----
 - If `buxis' is an array, it accepts subscript-list as `from/below'.
 - If `buxis' is an lazy-sequence, it will be forced.
 - If `buxis' is an lazy-sequence and without `below', `from-end' is ignored.
")


;;--------------------------------------------------------------------
;; substitute*
;;--------------------------------------------------------------------

(defun substitute* (new old buxis &key key count (test 'eql) (start 0) end from-end)

  (check-type key   (or function symbol))
  (check-type count (or null integer))
  (check-type test  (or function symbol))
  (setf key (the (or function symbol) (or key #'identity)))
  
  (etypecase buxis
    (sequence
     (check-type start (integer 0 *))
     (check-type end   (or null (integer 0 *)))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (substitute new old buxis :test test :count count :key key
                               :from-end from-end :start start :end end))

    (lazy-sequence
     ;; TODO:
     ;;  ? OPTIMIZE: removing lazy-sequence layer. e.g.,
     ;;  (lazy-seq (pipe-map (lambda (v)
     ;;                        (if (funcall test old (funcall key v))
     ;;                            new
     ;;                            v))
     ;;                      buxis))
     (check-type start array-index)
     (check-type end   (or null array-index))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (if (and (zerop start) (not end))
         (if count
             (lazy-map (lambda (v) (if (and (plusp count)
                                            (funcall test old (funcall key v)))
                                       (progn
                                         (decf count) new)
                                       v))
                       buxis)
             (lazy-map (lambda (v) (if (funcall test old (funcall key v))
                                       new
                                       v))
                       buxis))

         (let ((index -1))
           (declare (type (integer -1 *) index))
           (if count
               (if end
                   (lazy-map (lambda (v)
                               (incf index)
                               (cond ((<= count 0) v)
                                     ((< index start) v)
                                     ((<= end index) v)
                                     ((funcall test old (funcall key v))
                                      (decf count) new)
                                     (t v)))
                             buxis)
                   (lazy-map (lambda (v)
                               (incf index)
                               (cond ((<= count 0) v)
                                     ((< index start) v)
                                     ((funcall test old (funcall key v))
                                      (decf count) new)
                                     (t v)))
                             buxis))
               (if end
                   (lazy-map (lambda (v)
                               (incf index)
                               (cond ((< index start) v)
                                     ((<= end index) v)
                                     ((funcall test old (funcall key v))
                                      new)
                                     (t v)))
                             buxis)
                   (lazy-map (lambda (v)
                               (incf index)
                               (cond ((< index start) v)
                                     ((funcall test old (funcall key v))
                                      new)
                                     (t v)))
                             buxis))))))
    
    (hash-table
     (let ((result (copy-hash-table buxis)))
       (declare (type hash-table result))
       (if (not count)
           (loop :for k :being :the :hash-keys :of buxis :using (:hash-value v)
                 :when (funcall test old (funcall key v))
                   :do (setf (gethash k result) new))
           (loop :for k :being :the :hash-keys :of buxis :using (:hash-value v)
                 :until (zerop count)
                 :when (funcall test old (funcall key v))
                   :do (setf (gethash k result) new)
                       (decf count)))
       result))

    (array
     (check-type start (or list array-index))
     (check-type end   (or list array-index))
     (when (listp start)
       (setf start (the array-index (apply #'array-row-major-index buxis start))))
     (when (and end (listp end))
       (setf end (the array-index (apply #'array-row-major-index buxis end))))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (setf end (the array-index (or end (array-total-size buxis))))
     (let ((result (copy-array buxis)))
       (declare (type array result))
       (if (not count)
           (if from-end
               (do ((i (1- end) (1- i)))
                   ((< i start) result)
                 (declare (type array-index-1 i))
                 (when (funcall test old (funcall key (row-major-aref result i)))
                   (setf (row-major-aref result i) new)))
               ;; (loop :for i :of-type array-index-1 :downfrom (1- end) :to start
               ;;       :for v := (row-major-aref buxis i)
               ;;       :when (funcall test old (funcall key v))
               ;;         :do (setf (row-major-aref result i) new))
               (do ((i start (1+ i)))
                   ((<= end i) result)
                 (declare (type array-index i))
                 (when (funcall test old (funcall key (row-major-aref result i)))
                   (setf (row-major-aref result i) new)))
               ;; (loop :for i :of-type array-index
               ;;       :from start :below end
               ;;       :for v := (row-major-aref buxis i)
               ;;       :when (funcall test old (funcall key v))
               ;;         :do (setf (row-major-aref result i) new))
               )
           (if (zerop count)
               result
               (if from-end
                   (do ((i (1- end) (1- i)))
                       ((< i start) result)
                     (declare (type array-index-1 i))
                     (when (funcall test old (funcall key (row-major-aref result i)))
                       (setf (row-major-aref result i) new)
                       (decf count)
                       (when (zerop count)
                         (return result))))
                   ;; (loop :for i :of-type array-index-1 :downfrom (1- end) :to start
                   ;;       :for v := (row-major-aref buxis i)
                   ;;       :until (zerop count)
                   ;;       :when (funcall test old (funcall key v))
                   ;;         :do (setf (row-major-aref result i) new)
                   ;;             (decf count))
                   (do ((i start (1+ i)))
                       ((<= end i) result)
                     (declare (type array-index i))
                     (when (funcall test old (funcall key (row-major-aref result i)))
                       (setf (row-major-aref result i) new)
                       (decf count)
                       (when (zerop count)
                         (return result))))
                   ;; (loop :for i :of-type array-index :from start :below end
                   ;;       :for v := (row-major-aref buxis i)
                   ;;       :until (zerop count)
                   ;;       :when (funcall test old (funcall key v))
                   ;;         :do (setf (row-major-aref result i) new)
                   ;;             (decf count))
                   )))
       result))))

(setf (documentation 'substitute* 'function) "
SUBSTITUTE* new old buxis &key key count (test 'eql) (start 0) end from-end => result

Note
----
 - If `buxis' is an array, it accepts subscript-list as `start/end' value.
")


;;--------------------------------------------------------------------
;; substitute-if*
;;--------------------------------------------------------------------

(defun substitute-if* (new predicate buxis &key key count (start 0) end from-end)

  (check-type predicate (or function symbol))
  (check-type key       (or function symbol))
  (check-type count     (or null integer))
  (setf key (the (or function symbol) (or key #'identity)))
  
  (etypecase buxis
    (sequence
     (check-type start (integer 0 *))
     (check-type end   (or null (integer 0 *)))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (substitute-if new predicate buxis
                    :key key :count count :from-end from-end :start start :end end))

    (lazy-sequence
     ;; TODO:
     ;;  ? OPTIMIZE: removing lazy-sequence layer.
     (check-type start array-index)
     (check-type end   (or null array-index))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))

     (if (and (zerop start) (not end))
         (if count
             (lazy-map (lambda (v) (if (and (plusp count)
                                            (funcall predicate (funcall key v)))
                                       (progn
                                         (decf count) new)
                                       v))
                       buxis)
             (lazy-map (lambda (v) (if (funcall predicate (funcall key v))
                                       new
                                       v))
                       buxis))

         (let ((index -1))
           (declare (type (integer -1 *) index))
           (if count
               (if end
                   (lazy-map (lambda (v)
                               (incf index)
                               (cond ((<= count 0) v)
                                     ((< index start) v)
                                     ((<= end index) v)
                                     ((funcall predicate (funcall key v))
                                      (decf count) new)
                                     (t v)))
                             buxis)
                   (lazy-map (lambda (v)
                               (incf index)
                               (cond ((<= count 0) v)
                                     ((< index start) v)
                                     ((funcall predicate (funcall key v))
                                      (decf count) new)
                                     (t v)))
                             buxis))
               (if end
                   (lazy-map (lambda (v)
                               (incf index)
                               (cond ((< index start) v)
                                     ((<= end index) v)
                                     ((funcall predicate (funcall key v))
                                      new)
                                     (t v)))
                             buxis)
                   (lazy-map (lambda (v)
                               (incf index)
                               (cond ((< index start) v)
                                     ((funcall predicate (funcall key v))
                                      new)
                                     (t v)))
                             buxis))))))
    
    (hash-table
     (let ((result (copy-hash-table buxis)))
       (declare (type hash-table result))
       (if (not count)
           (loop :for k :being :the :hash-keys :of buxis :using (:hash-value v)
                 :when (funcall predicate (funcall key v))
                   :do (setf (gethash k result) new))
           (loop :for k :being :the :hash-keys :of buxis :using (:hash-value v)
                 :until (zerop count)
                 :when (funcall predicate (funcall key v))
                   :do (setf (gethash k result) new)
                       (decf count)))
       result))    

    (array
     (check-type start (or list array-index))
     (check-type end   (or list array-index))
     (when (listp start)
       (setf start (the array-index (apply #'array-row-major-index buxis start))))
     (when (and end (listp end))
       (setf end (the array-index (apply #'array-row-major-index buxis end))))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (setf key (the (or function symbol) (or key #'identity)))
     (setf end (the array-index (or end (array-total-size buxis))))
     (let ((result (copy-array buxis)))
       (declare (type array result))
       
       (if (not count)
           (if from-end
               ;; (do ((i (1- end) (1- i)))
               ;;     ((< i start) result)
               ;;   (declare (type array-index-1 i))
               ;;   (when (funcall predicate (funcall key (row-major-aref result i)))
               ;;     (setf (row-major-aref result i) new)))
               (loop :for i :of-type array-index-1 :downfrom (1- end) :to start
                     :for v := (row-major-aref buxis i)
                     :when (funcall predicate (funcall key v))
                       :do (setf (row-major-aref result i) new))
               ;; (do ((i start (1+ i)))
               ;;     ((<= end i) result)
               ;;   (declare (type array-index i))
               ;;   (when (funcall predicate (funcall key (row-major-aref result i)))
               ;;     (setf (row-major-aref result i) new)))
               (loop :for i :of-type array-index :from start :below end
                     :for v := (row-major-aref buxis i)
                     :when (funcall predicate (funcall key v))
                       :do (setf (row-major-aref result i) new)))
           (if (zerop count)
               result
               (if from-end
                   ;; (do ((i (1- end) (1- i)))
                   ;;     ((< i start) result)
                   ;;   (declare (type array-index-1 i))
                   ;;   (when (funcall predicate (funcall key (row-major-aref result i)))
                   ;;     (setf (row-major-aref result i) new)
                   ;;     (decf count)
                   ;;     (when (zerop count)
                   ;;       (return result))))
                   (loop :for i :of-type array-index-1 :downfrom (1- end) :to start
                         :for v := (row-major-aref buxis i)
                         :until (zerop count)
                         :when (funcall predicate (funcall key v))
                           :do (setf (row-major-aref result i) new)
                               (decf count))
                   ;; (do ((i start (1+ i)))
                   ;;     ((<= end i) result)
                   ;;   (declare (type array-index i))
                   ;;   (when (funcall predicate (funcall key (row-major-aref result i)))
                   ;;     (setf (row-major-aref result i) new)
                   ;;     (decf count)
                   ;;     (when (zerop count)
                   ;;       (return result))))
                   (loop :for i :of-type array-index :from start :below end
                         :for v := (row-major-aref buxis i)
                         :until (zerop count)
                         :when (funcall predicate (funcall key v))
                           :do (setf (row-major-aref result i) new)
                               (decf count)))))
       result))))

(setf (documentation 'substitute-if* 'function) "
SUBSTITUTE-IF* new predicate buxis &key key count (start 0) end from-end => result

Note
----
 - If `buxis' is an array, it accepts subscript-list as `start/end' value.
")


;;--------------------------------------------------------------------
;; substitute-if-not*
;;--------------------------------------------------------------------

(defun substitute-if-not* (new predicate buxis &key key count (start 0) end from-end)
  (substitute-if* new (complement (ensure-function predicate)) buxis
                  :key key :count count :start start :end end :from-end from-end))

(setf (documentation 'substitute-if-not* 'function) "
SUBSTITUTE-IF-NOT* new predicate buxis &key key count (start 0) end from-end => result

Note
----
 - If `buxis' is an array, it accepts subscript-list as `start/end' value.
")


;;--------------------------------------------------------------------
;; fill*
;;--------------------------------------------------------------------
;; TODO:
;;  ? (destructive t) -> (destructive nil)
;;  destructive -> modified, override

(defun fill* (buxis item &key (start 0) end (destructive t))
  
  (etypecase buxis
    (sequence
     (check-type start (integer 0 *))
     (check-type end   (or null (integer 0 *)))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (if destructive
         (fill buxis item :start start :end end)
         (fill (copy-seq buxis) item :start start :end end)))

    (lazy-sequence
     (check-type start array-index)
     (check-type end   (or null array-index))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (let ((lseq (if destructive buxis (copy-lazy-seq buxis)))
           (index -1))
       (declare (type lazy-sequence lseq)
                (type (integer -1 *) index))
       (if end
           (lazy-map (lambda (x)
                       (incf index)
                       (cond ((< index start) x)
                             ((<= end index)  x)
                             (t item)))
                     lseq)
           (lazy-map (lambda (x)
                       (incf index)
                       (cond ((< index start) x)
                             (t item)))
                     lseq))))

    (hash-table
     (let ((result (if destructive buxis (copy-empty-hash-table buxis))))
       (declare (type hash-table result))
       (maphash (lambda (k v)
                  (declare (ignore v))
                  (setf (gethash k result) item))
                buxis)
       result))
    
    (array
     (check-type start (or list array-index))
     (check-type end   (or list array-index))
     (when (listp start)
       (setf start (the array-index (apply #'array-row-major-index buxis start))))
     (let ((result (if destructive buxis (copy-empty-array buxis))))
       (cond (end
              (when (listp end)
                (setf end (the array-index (apply #'array-row-major-index buxis end))))
              (when (< end start)
                (error "[~S , ~S) is bad interval." start end))
              (loop :for i :of-type array-index :from start :below end
                    :do (setf (row-major-aref result i)
                              item)))
             (t
              (loop :for i :of-type array-index
                    :from start :below (array-total-size buxis)
                    :do (setf (row-major-aref result i)
                              item))))
       result))
    #+nil
    (array
     ;; MEMO:
     ;; Should error if end > array-total-size?
     (check-type start (or list array-index))
     (check-type end   (or list array-index))
     (when (listp start)
       (setf start (the array-index (apply #'array-row-major-index buxis start))))
     (when (and end (listp end))
       (setf end (the array-index (apply #'array-row-major-index buxis end))))
     (when (and end (< end start))
       (error "[~S , ~S) is bad interval." start end))
     (let ((result (if destructive buxis (copy-empty-array buxis))))
       (declare (type array result))
       ;; (setf end (the array-index (or end (array-total-size buxis))))
       ;; (do ((i start (1+ i)))
       ;;     ((<= end i))
       ;;   (declare (type array-index i))
       ;;   (setf (row-major-aref result i) item))
       (loop :for i :of-type array-index
             :from start :below (or end (array-total-size buxis))
             :do (setf (row-major-aref result i)
                       item))
       result))))


(setf (documentation 'fill* 'function) "
FILL* buxis item &key (start 0) end (destructive t) => result


Note
----
 - If `buxis' is an array, it accepts subscript-list as `start/end' value.

 - If `destructive' is NIL, `buxis' is not modified and it returns
   copyed-buxis.
")

;;====================================================================
