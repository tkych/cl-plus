;;;; Last modified: 2014-06-29 10:07:14 tkych

;; cl-plus/src/core/sequens.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;; TODO
;; ----
;;  * compiler-macro

;;====================================================================
;; Sequens
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.core.sequens
  (:documentation "
Sequens
=======

    sequens := (or sequence lazy-sequence)


SEQUENS is an abstruction from SEQUENCE and LAZY-SEQUENCE.



Functions
---------

 - sequensp
 - seq->lseq
 - repeat
 - range
 - take
 - take-until
 - take-while
 - drop
 - drop-until
 - drop-while
 - zip
 - interleave
 - interpose


Macro
-----

 - doseq
 - doseq2
")
  (:nicknames #:cl+sequens)
  (:export #:sequens
           #:sequensp
           #:seq->lseq
           #:repeat  #:range
           #:doseq   #:doseq2
           #:take    #:take-until   #:take-while
           #:drop    #:drop-until   #:drop-while
           #:zip     #:interleave   #:interpose)
  (:use #:cl)
  (:import-from #:alexandria
                #:once-only
                #:with-gensyms
                #:mappend
                #:if-let)
  (:import-from #:cl-plus.src.dev-util
                #:append1)
  (:import-from #:cl-plus.src.core.lazy-sequence
                #:lazy-sequence
                #:lazy-sequence-p
                #:make-lazy-seq
                #:accessed-length
                #:lazy-scan
                #:lazy-repeat
                #:lazy-take
                #:lazy-take-until
                #:lazy-take-while
                #:lazy-drop
                #:lazy-drop-until
                #:lazy-drop-while
                #:lazy-zip
                #:lazy-interleave
                #:lazy-interpose)
  (:import-from #:cl-plus.src.srfi.srfi-41
                #:pipe-from))

(in-package #:cl-plus.src.core.sequens)


;;--------------------------------------------------------------------
;; Type
;;--------------------------------------------------------------------

(deftype sequens () `(or sequence lazy-sequence))

(declaim (inline sequensp))
(defun sequensp (x) (typep x 'sequens))


;;--------------------------------------------------------------------
;; range
;;--------------------------------------------------------------------

(defun range (&optional (start 0) (step 1) end)
  (check-type start number)
  (check-type step  number)
  (check-type end   (or null number))
  (if end
      ;; (+ start (- step step)) is start ::= integer and step ::= float.
      (loop :for i :from (+ start (- step step)) :below end :by step
            :collect i)
      (make-lazy-seq (list start)
                     (pipe-from (+ start step) step))))

(setf (documentation 'range 'function) "
RANGE &optional (start 0) (step 1) end => result

Returns a sequens whose element are in range from `start' below `end'
by `step'.  If `end' is NIL, result-type is (infinite) lazy-sequence,
otherwise list.
")


;;--------------------------------------------------------------------
;; misc
;;--------------------------------------------------------------------

(defun seq->lseq (sequens)
  (etypecase sequens
    (cl:sequence   (make-lazy-seq sequens))
    (lazy-sequence sequens)))

(defun %make-empty-seq (sequens)
  (etypecase sequens
    (cl:list       nil)
    (cl:bit-vector #*)
    (cl:string     "")
    (cl:vector     #())
    (lazy-sequence (make-lazy-seq))))

(defun %seq-to-string (seq)
  (with-output-to-string (s)
    (etypecase seq
      (cl:list   (dolist (v seq) (princ v s)))
      (cl:vector (loop :for v :across seq :do (princ v s))))))

(defun %coerce-seq (seq type)
  (case type
    (cl:string     (%seq-to-string seq))
    (sequens       seq)
    (lazy-sequence (make-lazy-seq seq nil))
    (t             (coerce seq type))))


;;--------------------------------------------------------------------
;; take
;;--------------------------------------------------------------------

(defun take (count sequens &optional (result-type 'cl:list))
  (check-type count (or (integer 0 *) (eql :all)))
  (etypecase sequens
    (cl:sequence
     (if (or (eq count :all)
             (<= (length sequens) count))
         (%coerce-seq (copy-seq sequens) result-type)
         (%coerce-seq (subseq sequens 0 count) result-type)))
    
    (lazy-sequence
     (let ((result-list (lazy-take count sequens)))
       (case result-type
         ((sequens lazy-sequence)
          (make-lazy-seq result-list))
         (t
          (%coerce-seq result-list result-type)))))))


(setf (documentation 'take 'function) "
TAKE count sequens &optional result-type => result-sequens

Takes first `count' elements from `sequens', and coerces it to
`result-type'.  Default `result-type' is `list'. If `count' is `:all',
return all elements.  If the elements of lazy-sequence is not
realized, it will be forced.
")


;;--------------------------------------------------------------------
;; take-until
;;--------------------------------------------------------------------

(defun take-until (predicate sequens &optional (result-type 'cl:list))
  (check-type predicate (or symbol function))
  (etypecase sequens
    (cl:sequence
     (if-let (end (position-if predicate sequens))
       (values (%coerce-seq (subseq sequens 0 end) result-type)
               t)
       (values (%coerce-seq (copy-seq sequens) result-type)
               nil)))

    (lazy-sequence
     (multiple-value-bind (result-list presentp) (lazy-take-until predicate sequens)
       (case result-type
         ((sequens lazy-sequence)
          (values (make-lazy-seq result-list) presentp))
         (t
          (values (%coerce-seq result-list result-type)
                  presentp)))))))


(setf (documentation 'take-until 'function) "
TAKE-UNTIL predicate sequens &optional result-type => result-sequens, presentp

If `sequens' is lazy-sequence, realized.  If there is no possible
element satisfing `predicate' in lazy-sequence, enters the INFINITE
LOOP.
")


;;--------------------------------------------------------------------
;; take-while
;;--------------------------------------------------------------------

(defun take-while (predicate sequens &optional (result-type 'cl:list))
  (check-type predicate (or symbol function))
  (etypecase sequens
    (cl:sequence
     (if-let (end (position-if-not predicate sequens))
       (values (%coerce-seq (subseq sequens 0 end) result-type) t)
       (values (%coerce-seq (copy-seq sequens) result-type) nil)))

    (lazy-sequence
     (multiple-value-bind
           (result-list presentp) (lazy-take-while predicate sequens)
       (case result-type
         ((sequens lazy-sequence)
          (values (make-lazy-seq result-list) presentp))
         (t
          (values (%coerce-seq result-list result-type)
                  presentp)))))))


(setf (documentation 'take-while 'function) "
TAKE-WHILE predicate sequens &optional result-type => result-sequens, presentp

If `sequens' is lazy-sequence, realized.  If there is no possible
element satisfing `predicate' in lazy-sequence, enters goes INFINITE
LOOP.
")


;;--------------------------------------------------------------------
;; drop
;;--------------------------------------------------------------------

(defun drop (count sequens &optional (result-type 'sequens))
  (check-type count (integer 0 *))
  (etypecase sequens
    (sequence
     (let ((result (subseq sequens (min count (length sequens)))))
       (if (eq 'sequens result-type)
           result
           (%coerce-seq result result-type))))

    (lazy-sequence
     (let ((result-lseq (lazy-drop count sequens)))
       (case result-type
         ((sequens lazy-sequence)
          result-lseq)
         (t
          (%coerce-seq (take :all result-lseq) result-type)))))))


(setf (documentation 'drop 'function) "
DROP count sequens => result-sequens
")


;;--------------------------------------------------------------------
;; drop-until
;;--------------------------------------------------------------------

(defun drop-until (predicate sequens &optional (result-type 'sequens))
  (check-type predicate (or symbol function))
  (etypecase sequens
    (sequence
     (let* ((start (position-if predicate sequens))
            (result (if start (subseq sequens start) nil)))
       (if (eq 'sequens result-type)
           (or result (%make-empty-seq sequens))
           (%coerce-seq result result-type))))
    
    (lazy-sequence
     (let ((result-lseq (lazy-drop-until predicate sequens)))
       (case result-type
         ((sequens lazy-sequence)
          result-lseq)
         (t
          (%coerce-seq (take :all result-lseq) result-type)))))))


(setf (documentation 'drop-until 'function) "
DROP-UNTIL predicate sequens => result-sequens
")


;;--------------------------------------------------------------------
;; drop-while
;;--------------------------------------------------------------------

(defun drop-while (predicate sequens &optional (result-type 'sequens))
  (check-type predicate (or symbol function))
  (etypecase sequens
    (sequence
     (let* ((start (position-if-not predicate sequens))
            (result (if start (subseq sequens start) nil)))
       (if (eq 'sequens result-type)
           (or result (%make-empty-seq sequens))
           (%coerce-seq result result-type))))
  
    (lazy-sequence
     (let ((result-lseq (lazy-drop-while predicate sequens)))
       (case result-type
         ((sequens lazy-sequence)
          result-lseq)
         (t
          (%coerce-seq (take :all result-lseq) result-type)))))))


(setf (documentation 'drop-while 'function) "
DROP-WHILE predicate sequens => result-sequens
")


;;--------------------------------------------------------------------
;; zip
;;--------------------------------------------------------------------

(defun zip (sequens1 sequens2 &rest more-sequentia)
  (check-type sequens1 sequens)
  (check-type sequens2 sequens)
  (when (some (complement #'sequensp) more-sequentia)
    (error "There is at least one non-sequens object in MORE-SEQUENTIA."))
  (let ((seqs (list* sequens1 sequens2 more-sequentia)))
    (if (notany #'lazy-sequence-p seqs)
        (apply #'map 'list #'list seqs)
        (apply #'lazy-zip (mapcar #'seq->lseq seqs)))))


(setf (documentation 'zip 'function) "
ZIP sequens1 sequens2 &rest more-sequentia => result-sequens

If there is at least one lazy-sequence is supplied, result-sequens is
a lazy-sequence, otherwise a list.
")


;;--------------------------------------------------------------------
;; interleave
;;--------------------------------------------------------------------

(defun interleave (sequens1 sequens2 &rest more-sequentia)
  (check-type sequens1 sequens)
  (check-type sequens2 sequens)
  (when (some (complement #'sequensp) more-sequentia)
    (error "There is at least one non-sequens object in MORE-SEQUENTIA."))
  (let ((seqs (list* sequens1 sequens2 more-sequentia)))
    (if (some #'lazy-sequence-p seqs)
        (apply #'lazy-interleave (mapcar #'seq->lseq seqs))
        (etypecase sequens1
          (list
           (apply #'append
                  (apply #'map 'list (lambda (&rest args) args) seqs)))
          (string
           (apply #'concatenate 'string
                  (apply #'map 'list (lambda (&rest args) (coerce args 'string))
                         seqs)))
          (bit-vector
           (apply #'concatenate 'bit-vector
                  (apply #'map 'list (lambda (&rest args) (coerce args 'bit-vector))
                         seqs)))
          (vector
           (apply #'concatenate 'vector
                  (apply #'map 'list (lambda (&rest args) (coerce args 'vector))
                         seqs)))))))


(setf (documentation 'interleave 'function) "
INTERLEAVE sequens1 sequens2 &rest more-sequentia => result-sequens
If `sequens' is lazy-sequence, not realized.
")


;;--------------------------------------------------------------------
;; interpose
;;--------------------------------------------------------------------

(defun interpose (separator sequens)
  (etypecase sequens
    (list
     (if (null sequens)
         '()
         (cons (first sequens)
               (mappend (lambda (v) (list separator v))
                        (rest sequens)))))
    (string
     (make-array (max 0 (1- (* 2 (length sequens))))
                 :element-type 'character
                 :initial-contents (interpose separator (coerce sequens 'list))))
    (bit-vector
     (make-array (max 0 (1- (* 2 (length sequens))))
                 :element-type 'bit
                 :initial-contents (interpose separator (coerce sequens 'list))))
    (vector
     (make-array (max 0 (1- (* 2 (length sequens))))
                 :initial-contents (interpose separator (coerce sequens 'list))))
    (lazy-sequence
     (lazy-interpose separator sequens))))


(setf (documentation 'interpose 'function) "
INTERPOSE separator sequens => result-sequens
")


;;--------------------------------------------------------------------
;; doseq
;;--------------------------------------------------------------------
;; TODO:
;;  ADD: for lazy-sequence: runall?, doall?
;;  USE: dolseq

(defmacro doseq ((var sequens &optional result) &body body)
  (once-only (sequens)
    `(etypecase ,sequens
       (list
        (dolist (,var ,sequens ,result)
          ,@body))

       (vector
        ,(with-gensyms (index)
           `(dotimes (,index (length ,sequens) ,result)
              (let ((,var (aref ,sequens ,index)))
                ,@body))))

       (lazy-sequence
        (dolist (,var (lazy-take (accessed-length ,sequens) ,sequens)
                      ,result)
          ,@body)))))


(setf (documentation 'doseq 'function) "
DOSEQ (var sequens &optional result) &body body => result
")


;;--------------------------------------------------------------------
;; doseq2
;;--------------------------------------------------------------------

(defmacro doseq2 ((index value sequens &optional result) &body body)
  (once-only (sequens)
    `(etypecase ,sequens
       (list
        (let ((,index 0))
          (dolist (,value ,sequens ,result)
            ,@body
            (incf ,index))))

       (vector
        (dotimes (,index (length ,sequens) ,result)
          (let ((,value (aref ,sequens ,index)))
            ,@body)))

       (lazy-sequence
        (let ((,index 0))
         (dolist (,value (lazy-take (accessed-length ,sequens) ,sequens)
                         ,result)
           ,@body
           (incf ,index)))))))


(setf (documentation 'doseq2 'function) "
DOSEQ2 (index value sequens &optional result) &body body => result
")


;;====================================================================
