;;;; Last modified: 2014-06-29 10:05:46 tkych

;; cl-plus/src/core/alist.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Association List
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.core.alist
  (:documentation "
Association List
================


")
  (:export #:alist
           #:alistp
           #:alist=
           #:normalize-alist
           #:doalist)
  (:nicknames #:cl+alist)
  (:use #:cl)
  (:import-from #:trivial-types
                #:association-list-p)
  (:import-from #:alexandria
                #:once-only
                #:with-gensyms
                #:parse-body))

(in-package #:cl-plus.src.core.alist)


;;--------------------------------------------------------------------
;; type
;;--------------------------------------------------------------------

(setf (symbol-function 'alistp)
      (symbol-function 'trivial-types:association-list-p))

(declaim (inline alistp))
(deftype alist ()
  `(and list (satisfies alistp)))


;;--------------------------------------------------------------------
;; normalize-alist
;;--------------------------------------------------------------------

(declaim (inline normalize-alist))
(defun normalize-alist (alist &optional (key-test #'equal))
  (check-type alist    alist)
  (check-type key-test (or function symbol))
  ;; CLHS, Function REMOVE-DUPLICATES
  ;; > The elements of sequence are compared pairwise, and if any two
  ;; > match, then the one occurring earlier in sequence is discarded,
  ;; > unless from-end is true, in which case the one later in sequence
  ;; > is discarded.
  (remove-duplicates alist :test key-test :key #'car :from-end t))

(setf (documentation 'normalize-alist 'function) "
NORMALIZE-ALIST alist &optional (key-test 'equal) => copyed-alist

Examples
--------

 * (normalize-alist '((:foo . 1) (:bar . 2) (:foo . 42) (:bar . 24)))
   => ((:FOO . 1) (:BAR . 2))

 * (normalize-alist '((\"foo\" . 0) (\"FOO\" . 42))
                     #'string=)
   => ((\"foo\" . 0) (\"FOO\" . 42))

 * (normalize-alist '((\"foo\" . 0) (\"FOO\" . 42))
                    #'string-equal)
   => (("foo" . 0))
")


;;--------------------------------------------------------------------
;; alist=
;;--------------------------------------------------------------------

(defun alist= (alist1 alist2 &key (key-test #'equal) (val-test #'eql) ensure-unique-keys)
  (check-type alist1 alist)
  (check-type alist2 alist)
  (check-type key-test (or function symbol))
  (check-type val-test (or function symbol))
  (if ensure-unique-keys
      (and (= (length alist1) (length alist2))
           (loop :for (k1 . v1) :in alist1
                 :for v2 := (cdr (assoc k1 alist2 :test key-test))
                 :always (funcall val-test v1 v2)))
      (and
       (loop :with checked-keys := '()
            :for (k1 . v1) :in alist1
            :unless (member k1 checked-keys :test key-test)
              :do (push k1 checked-keys)
                  (let ((entry2 (assoc k1 alist2 :test key-test)))
                    (unless (and entry2
                                 (funcall val-test v1 (cdr entry2)))
                      (return NIL)))
            :finally (return T))
       (loop :with checked-keys := '()
             :for (k2 . v2) :in alist2
             :unless (member k2 checked-keys :test key-test)
               :do (push k2 checked-keys)
                   (let ((entry1 (assoc k2 alist1 :test key-test)))
                     (unless (and entry1
                                  (funcall val-test (cdr entry1) v2))
                       (return NIL)))
             :finally (return T)))))

(setf (documentation 'alist= 'function) "
ALIST= alist1 alist2 &key (key-test 'equal) (val-test 'eql) ensure-unique-keys => boolean
")


;;--------------------------------------------------------------------
;; doalist
;;--------------------------------------------------------------------

(defmacro doalist ((key val alist &optional result) &body body)
  (multiple-value-bind (tag-statements declarations) (parse-body body)
    (with-gensyms (rest-alist LOOP rest)
      (once-only (alist)
        `(block nil
           (let ((,rest-alist (normalize-alist ,alist)))
             (tagbody ,LOOP
                (unless (endp ,rest-alist)
                  (if (not (consp (first ,rest-alist)))
                      (error "~S is malformed alist." ,alist)
                      (destructuring-bind ((,key . ,val) . ,rest) ,rest-alist
                        ,@declarations
                        (tagbody ,@tag-statements)
                        (setf ,rest-alist ,rest)))
                  (GO ,LOOP))))
           ,(when result
              `(let (,key ,val)
                 (declare (ignorable ,key ,val))
                 ,result)))))))

(setf (documentation 'doalist 'function) "
DOALIST (key val alist &optional result) &body body => result
")

(define-compiler-macro doalist (&whole form (key val alist &optional result) &body body)
  (if (and (integerp cl-plus.src.core.iteration::*loop-unrolling-threshold*)
           (alistp alist)
           (<= (length alist) cl-plus.src.core.iteration::*loop-unrolling-threshold*))
      `(block nil
         ,@(loop :for (k . v) :in (normalize-alist alist)
                 :collect `(let ((,key ,k) (,val ,v)) (tagbody ,@body)))
         ,(when result
            `(let (,key ,val)
               (declare (ignorable ,key ,val))
               ,result)))
      form))


;;====================================================================
