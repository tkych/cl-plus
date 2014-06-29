;;;; Last modified: 2014-06-29 10:32:18 tkych

;; cl-plus/src/srfi/srfi-41.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; See cl-plus/LICENSE, for more details.

;;--------------------------------------------------------------------
;; Copyright (C) Philip L. Bewig (2007). All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;--------------------------------------------------------------------

;; The following code is a Common Lisp translation from reference
;; implementation of SRFI-41.


;;====================================================================
;; SRFI-41 for Common Lisp
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.srfi.srfi-41
  (:documentation "
SRFI-41 for CL
==============

This is a Common Lisp implementation for \"Scheme Request for
Implementation 41\"[3] (a.k.a., SRFI-41).  Roughly speaking, SRFI-41
is a document about streams (i.e., pipe) for Scheme.  Since the name
\"stream\" is already used for I/O objects in Common Lisp, we have
introduced \"pipe\" in exchenge for \"stream\".

The pipes in this package are \"even\" style, whereas the pipes in
PAIP[5] (and the streams in SICP[4]) are \"odd\" style.  The style
\"even\" and \"odd\" are the prity of the number of constructors
\{delay, cons, nil} in pipe construction.  For instance (cf., [1]),

 Odd:  (cons 1 (delay (cons 2 (delay nil))))         --- 5 conctructors
 Even: (delay (cons 1 (delay (cons 2 (delay nil))))) --- 6 conctructors

The practical differences between even and odd are:

 1. odd is simple, even is rather complecated (the above example).
 2. odd is too eager, even is properly eager (the following example from [2]).

 Odd:
 ----
   (ql:quickload :pipes)

   (defun count-down1 (start)
     (pipes:make-pipe start (count-down1 (1- start))))

   (defun cut-off1 (n pipe)
     (cond ((zerop n) '())
           ((null pipe) '())
           (t (cons (pipes:pipe-head pipe)
                    (cut-off1 (1- n)
                              (pipes:pipe-tail pipe))))))

   (handler-case
       (cut-off1 4
                 (pipes:pipe-map (lambda (n) (/ 12 n))
                                 (count-down1 4)))
     (error (c) (type-of c)))
  ; => DIVISION-BY-ZERO

 Even:
 -----
   (ql:quickload :cl-plus)
   (use-package :cl+srfi-45)

   (defun count-down2 (start)
     (lazy
      (delay (cons start (count-down2 (1- start))))))

   (defun cut-off2 (n pipe)
     (cond ((zerop n) '())
           ((null (force pipe)) '())
           (t (cons (car (force pipe))
                    (cut-off2 (1- n)
                              (cdr (force pipe)))))))

   (defun map2 (fn pipe)
     (lazy
      (if (null (force pipe))
          (delay '())
          (delay (cons (funcall fn (car (force pipe)))
                       (map2 fn (cdr (force pipe))))))))

   (cut-off2 4
             (map2 (lambda (n) (/ 12 n))
                   (count-down2 4)))

  ; => (3 4 6 12)


References
----------

 [0] Philip L. Bewig (2007),
     Scheme Request for Implementation 41: Streams.
     http://srfi.schemers.org/srfi-41

 [1] Philip L. Bewig (2004),
     Scheme Request for Implementation 40: A Library of Streams.
     http://srfi.schemers.org/srfi-40
     (srfi-40 is deprecated by srfi-41)

 [2] Philip Wadler, Walid Taha and David MacQueen (1998),
     How to add laziness to a strict language without even being add.
     http://www.cs.rice.edu/~taha/publications/conference/sml98.pdf

 [3] Harold Abelson and Gerald Jay Sussman, with Julie Sussman (1996),
     Structure and Interpretation of Computer Programs -- 2nd ed,
     Section 3.5 Streams.

 [4] Peter Norvig (1992),
     Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp,
     Section 9.3 Delaying Computation.

 [5] g000001,
     srfi-41, https://github.com/g000001/srfi-41
     (scheme style common lisp translation)
")
  (:nicknames #:cl+srfi-41)
  (:export
   ;; PRIMITIVE       ORIGINAL NAME
   #:+empty-pipe+    ; stream-null
   #:pipe-cons       ; stream-cons
   #:pipep           ; stream?
   #:pipe-null       ; stream-null?
   #:pipe-cons-p     ; stream-pair?
   #:pipe-car        ; stream-car
   #:pipe-cdr        ; stream-cdr
   #:pipe-lambda     ; stream-lambda

   ;; DERIVED
   #:define-pipe     ; define-stream
   #:list->pipe      ; list->stream
   #:stream->pipe    ; port->stream
   #:pipe            ; stream
   #:pipe->list      ; stream->list
   #:pipe-append     ; stream-append
   #:pipe-concat     ; stream-concat
   #:pipe-constant   ; stream-constant
   #:pipe-drop       ; stream-drop
   #:pipe-drop-while ; stream-drop-while
   #:pipe-filter     ; stream-filter
   #:pipe-fold       ; stream-fold
   #:pipe-for-each   ; stream-for-each
   #:pipe-from       ; stream-from
   #:pipe-iterate    ; stream-iterate
   #:pipe-length     ; stream-length
   #:pipe-let        ; stream-let
   #:pipe-map        ; stream-map
   #:pipe-match      ; stream-match
   #:pipe-of         ; stream-of
   #:pipe-range      ; stream-range
   #:pipe-ref        ; stream-ref
   #:pipe-reverse    ; stream-reverse
   #:pipe-scan       ; stream-scan
   #:pipe-take       ; stream-take
   #:pipe-take-while ; stream-take-while
   #:pipe-unfold     ; stream-unfold
   #:pipe-unfolds    ; stream-unfolds
   #:pipe-zip        ; stream-zip
   )
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:switch)
  (:import-from #:cl-plus.src.dev-util
                #:append1)
  (:import-from #:cl-plus.src.srfi.srfi-45
                #:promise
                #:promisep
                #:lazy
                #:eager
                #:delay
                #:force))

(pushnew :srfi-41 *features*)

(in-package #:cl-plus.src.srfi.srfi-41)

;;--------------------------------------------------------------------
;; Primitive
;;--------------------------------------------------------------------

(defvar +empty-pipe+ (eager 'pipe-nil))

(deftype pipe () 'promise)
(declaim (inline pipep))
(defun pipep (x) (typep x 'promise))

(defun pipe-null (x)
  (and (pipep x)
       (eq (force x)
           (force +empty-pipe+))))

(defstruct (kons (:conc-name   nil)
                 (:constructor kons (kar kdr))
                 (:predicate   konsp)
                 (:print-function
                  ;; TODO:
                  ;; (p1 . (p2 . (p3 . +empty-pipe+)))
                  ;; -> (p1 p2 p3)
                  (lambda (self stream depth)
                    (declare (ignore depth))
                    (format stream "(~S . ~S)" (kar self) (kdr self)))))
  "Immutable cons."
  (kar nil :read-only t)
  (kdr nil :read-only t))

(defmethod make-load-form ((k kons) &optional env)
  (make-load-form-saving-slots k :slot-names '(kar kdr)
                                 :environment env))

(defmacro pipe-cons (object pipe)
  `(eager (kons (delay ,object) (lazy ,pipe))))

(defun pipe-cons-p (x)
  (and (pipep x)
       (konsp (force x))))

(defun pipe-car (pipe)
  "STREAM-CAR in srfi-41."
  (check-type pipe pipe)
  (if (pipe-null pipe)
      (error "~S is null pipe." pipe) ; ?! nil or +empty-pipe+
      (force (kar (force pipe)))))

(defun pipe-cdr (pipe)
  "STREAM-CDR in srfi-41."
  (check-type pipe pipe)
  (if (pipe-null pipe)
      (error "~S is null pipe." pipe) ; ?! nil or +empty-pipe+
      (kdr (force pipe))))

(defmacro pipe-lambda (lambda-list &body body)
  "STREAM-LAMBDA in srfi-41."
  `(lambda ,lambda-list (lazy (progn ,@body))))


;;--------------------------------------------------------------------
;; Derived
;;--------------------------------------------------------------------

(defmacro define-pipe (name lambda-list &body body)
  "DEFINE-STREAM in srfi-41."
  `(defun ,name ,lambda-list
     (lazy (progn ,@body))))

(defun list->pipe (list)
  "LIST->STREAM in srfi-41."
  (check-type list list)
  (labels ((%list->pipe (lst)
             (lazy
              (if (null lst)
                  +empty-pipe+
                  (pipe-cons (car lst)
                             (%list->pipe (cdr lst)))))))
    (%list->pipe list)))

(defun stream->pipe (&optional (stream *standard-input*))
  "PORT->STREAM in srfi-41."
  (check-type stream (and stream (satisfies input-stream-p)))
  (let ((eof (gensym "EOF-")))
    (labels ((%stream->pipe ()
               (lazy
                (let ((char (read-char stream nil eof)))
                  (if (eq char eof)
                      +empty-pipe+
                      (pipe-cons char
                                 (stream->pipe stream)))))))
      (%stream->pipe))))

;; (defmacro %pipe (&rest args)
;;   "STREAM in srfi-41."
;;   (if (null args)
;;       '+empty-pipe+
;;       `(pipe-cons ,(car args) (%pipe ,@(cdr args)))))
;; (make-lazy-seq '() (%pipe 0 1 2)) => #[[?] ..]
;; (make-lazy-seq '() (pipe 0 1 2))  => #[..]
(defmacro pipe (&rest args)
  "STREAM in srfi-41.
NB. result pipe is not eagered."
  `(lazy
    ,(if (null args)
         '+empty-pipe+
         `(pipe-cons ,(car args) (pipe ,@(cdr args))))))

(defun pipe->list (pipe &optional count)
  "PIPE->LIST pipe &optional count => list

STREAM->LIST in srfi-41.

Note:
-----
 * The order of arguments is the reverse of stream->list."
  (check-type pipe pipe)
  (check-type count (or null (integer 0 *)))
  (labels ((rec (count pipe)
             (if (or (zerop count)
                     (pipe-null pipe))
                 '()
                 (cons (pipe-car pipe)
                       (rec (1- count) (pipe-cdr pipe))))))
    (rec (if count count -1) pipe)))

(defun pipe-append (&rest pipes)
  "STREAM-APPEND in srfi-41."
  (labels ((%pipe-append (pipes)
             (lazy
              (cond ((null (cdr pipes)) (car pipes))
                    ((pipe-null (car pipes))
                     (%pipe-append (cdr pipes)))
                    (t
                     (pipe-cons
                      (pipe-car (car pipes))
                      (%pipe-append (cons (pipe-cdr (car pipes))
                                          (cdr pipes)))))))))
    (cond ((null pipes) +empty-pipe+)
          ((some (complement #'pipep) pipes)
           (error "There is at least one non-pipe object in arguments."))
          (t (%pipe-append pipes)))))

(defun pipe-concat (pipe-of-pipes)
  "STREAM-CONCAT in srfi-41."
  (check-type pipe-of-pipes pipe)
  (labels ((%pipe-concat (pipes)
             (lazy
              (cond ((pipe-null pipes) +empty-pipe+)
                    ((not (pipep (pipe-car pipes)))
                     (error "non-pipe object in input ~S." pipe-of-pipes))
                    ((pipe-null (pipe-car pipes))
                     (%pipe-concat (pipe-cdr pipes)))
                    (t (pipe-cons
                        (pipe-car (pipe-car pipes))
                        (%pipe-concat
                         (pipe-cons (pipe-cdr (pipe-car pipes))
                                    (pipe-cdr pipes)))))))))
    (%pipe-concat pipe-of-pipes)))

(defun pipe-constant (&rest objects)
  "STREAM-CONSTANT in srfi-41."
  (lazy
   (cond ((null objects) +empty-pipe+)
         ((null (cdr objects))
          (pipe-cons (car objects)
                     (pipe-constant (car objects))))
         (t (pipe-cons (car objects)
                       (apply #'pipe-constant
                              (append1 (cdr objects)
                                       (car objects))))))))

(defun pipe-drop (count pipe)
  "STREAM-DROP in srfi-41."
  (check-type count (integer 0 *))
  (check-type pipe  pipe)
  (labels ((%pipe-drop (count pipe)
             (lazy
              (if (or (zerop count)
                      (pipe-null pipe))
                  pipe
                  (%pipe-drop (1- count) (pipe-cdr pipe))))))
    (%pipe-drop count pipe)))

(defun pipe-drop-while (predicate pipe)
  "STREAM-DROP-WHILE in srfi-41."
  (check-type predicate (or symbol function))
  (check-type pipe      pipe)
  (labels ((%pipe-drop-while (pipe)
             (lazy
              (if (and (pipe-cons-p pipe)
                       (funcall predicate (pipe-car pipe)))
                  (%pipe-drop-while (pipe-cdr pipe))
                  pipe))))
    (%pipe-drop-while pipe)))

(defun pipe-filter (predicate pipe)
  "STREAM-FILTER in srfi-41."
  (check-type predicate (or symbol function))
  (check-type pipe      pipe)
  (labels ((%pipe-filter (pipe)
             (lazy
              (cond ((pipe-null pipe) +empty-pipe+)
                    ((funcall predicate (pipe-car pipe))
                     (pipe-cons (pipe-car pipe)
                                (%pipe-filter (pipe-cdr pipe))))
                    (t (%pipe-filter (pipe-cdr pipe)))))))
    (%pipe-filter pipe)))

(defun pipe-fold (function base pipe)
  "STREAM-FOLD in srfi-41."
  (check-type function (or symbol function))
  (check-type pipe      pipe)
  (labels ((%pipe-fold (base pipe)
             (if (pipe-null pipe)
                 base
                 (%pipe-fold (funcall function base (pipe-car pipe))
                             (pipe-cdr pipe)))))
    (%pipe-fold base pipe)))

(defun pipe-for-each (function pipe &rest more-pipes)
  "STREAM-FOR-EACH in srfi-41."
  (check-type function (or symbol function))
  (check-type pipe      pipe)
  (labels ((%pipe-for-each (pipes)
             (when (notany #'pipe-null pipes)
               (apply function (mapcar #'pipe-car pipes))
               (%pipe-for-each (mapcar #'pipe-cdr pipes)))))
    (if (some (complement #'pipep) more-pipes)
        (error "There is at least one non-pipe object in MORE-PIPES.")
        (%pipe-for-each (cons pipe more-pipes)))))

(defun pipe-from (start &optional (step 1))
  "STREAM-FROM in srfi-41."
  (check-type start number)
  (check-type step  number)
  (labels ((%pipe-from (start)
             (lazy
              (pipe-cons start (%pipe-from (+ start step))))))
    (%pipe-from start)))

(defun pipe-iterate (successor-fn seed)
  "STREAM-ITERATE in srfi-41."
  (check-type successor-fn (or symbol function))
  (labels ((%pipe-iterate (curr)
             (lazy
              (pipe-cons curr
                         (%pipe-iterate (funcall successor-fn curr))))))
    (%pipe-iterate seed)))

(defun pipe-length (pipe)
  "STREAM-LENGTH in srfi-41."
  (check-type pipe pipe)
  (labels ((rec (len pipe)
             (if (pipe-null pipe)
                 len
                 (rec (1+ len) (pipe-cdr pipe)))))
    (rec 0 pipe)))

(defun pipe-map (function pipe &rest more-pipes)
  "STREAM-MAP in srfi-41."
  (check-type function (or symbol function))
  (check-type pipe     pipe)
  (labels ((%pipe-map (pipes)
             (lazy
              (if (some #'pipe-null pipes)
                  +empty-pipe+
                  (pipe-cons (apply function (mapcar #'pipe-car pipes))
                             (%pipe-map (mapcar #'pipe-cdr pipes)))))))
    (if (some (complement #'pipep) more-pipes)
        (error "There is at least one non-pipe object in MORE-PIPES.")
        (%pipe-map (cons pipe more-pipes)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun wildp (x)
    (and (symbolp x)
         (or (string= "_" (symbol-name x))
             (eq x t)))))

(defmacro pipe-match-pattern (pipe pattern bindings &body body)
  (if (null pattern)
      `(and (pipe-null ,pipe)
            (let ,bindings
              ,(when bindings
                 `(declare (ignorable ,@(mapcar #'car bindings))))
              ,@body))
      (if (consp pattern)
          (destructuring-bind (var . rest) pattern
            (if (wildp var)
                (once-only (pipe)
                  (with-gensyms (p)
                    `(and (pipe-cons-p ,pipe)
                          (let ((,p (pipe-cdr ,pipe)))
                            (declare (ignorable ,p))
                            (pipe-match-pattern ,p ,rest ,bindings ,@body)))))
                (once-only (pipe)
                  (with-gensyms (p tmp)
                    `(and (pipe-cons-p ,pipe)
                          (let ((,tmp (pipe-car ,pipe))
                                (,p   (pipe-cdr ,pipe)))
                            (declare (ignorable ,tmp ,p))
                            (pipe-match-pattern ,p ,rest
                                ,(cons (list var tmp) bindings)
                              ,@body)))))))
          (if (wildp pattern)
              `(let ,bindings
                 ,(when bindings
                    `(declare (ignorable ,@(mapcar #'car bindings))))
                 ,@body)
              `(let ((,pattern ,pipe)
                     ,@bindings)
                 ,(when bindings
                    `(declare (ignorable ,@(mapcar #'car bindings))))
                 ,@body)))))

(defmacro pipe-match-test (pipe (pattern &rest args))
  (destructuring-bind (exp/fender . exp) args
    (if exp
        `(pipe-match-pattern ,pipe ,pattern () (and ,exp/fender (list ,@exp)))
        `(pipe-match-pattern ,pipe ,pattern () (list ,exp/fender)))))

(defmacro pipe-match (pipe-form &body clauses)
  "STREAM-MATCH in srfi-41."
  (with-gensyms (pipe match-result pipe-match)
    `(block ,pipe-match
       (let ((,pipe ,pipe-form))
         (check-type ,pipe pipe)
         ,@(mapcar (lambda (clause)
                     `(let ((,match-result (pipe-match-test ,pipe ,clause)))
                        (when ,match-result
                          (return-from ,pipe-match
                            (car ,match-result)))))
                   clauses)
         (error "Pattern Failure.")))))

(defmacro pipe-let (tag bindings &body body)
  "STREAM-LET in srfi-41."
  `(labels ((,tag ,(mapcar #'car bindings)
              (lazy (progn ,@body))))
     (,tag ,@(mapcar #'cadr bindings))))

(defmacro pipe-of-aux (&rest args)
  (case (length args)
    (2 (destructuring-bind (expr base) args
         `(pipe-cons ,expr ,base)))
    (t (destructuring-bind (expr base x . rest) args
         (case (length x)
           (3 (destructuring-bind (var y pipe/expr) x
                (switch ((string-upcase (symbol-name y)) :test #'string=)
                  ("IN"
                   (with-gensyms (%loop pipe)
                     `(pipe-let ,%loop ((,pipe ,pipe/expr))
                        (if (pipe-null ,pipe)
                            ,base
                            (let ((,var (pipe-car ,pipe)))
                              (pipe-of-aux ,expr (,%loop (pipe-cdr ,pipe)) ,@rest))))))
                  ("IS"
                   `(let ((,var ,pipe/expr))
                      (pipe-of-aux ,expr ,base ,@rest)))
                  (t
                   `(if ,x (pipe-of-aux ,expr ,base ,@rest) ,base)))))
           (t `(if ,x (pipe-of-aux ,expr ,base ,@rest) ,base)))))))

(defmacro pipe-of (expr &body clauses)
  "STREAM-OF in srfi-41."
  `(pipe-of-aux ,expr +empty-pipe+ ,@clauses))

(defun pipe-range (start end &optional (step 1))
  "STREAM-RANGE in srfi-41."
  (check-type start number)
  (check-type end   number)
  (check-type step  (or null number))
  (let* ((delta (cond (step step)
                      ((< start end) 1)
                      (t -1)))
         (order (if (plusp delta) #'< #'>)))
    (labels ((%pipe-range (start)
               (lazy
                (if (funcall order start end)
                    (pipe-cons start (%pipe-range (+ start delta)))
                    +empty-pipe+))))
      (%pipe-range start))))

(defun pipe-ref (pipe index)
  "STREAM-REF in srfi-41."
  (check-type pipe  pipe)
  (check-type index (integer 0 *))
  (labels ((%pipe-ref (pipe i)
             (cond
               ((pipe-null pipe)
                (error "Index ~S out of bounds for pipe ~S." index pipe))
               ((zerop i) (pipe-car pipe))
               (t (%pipe-ref (pipe-cdr pipe) (1- i))))))
    (%pipe-ref pipe index)))

(defun pipe-reverse (pipe)
  "STREAM-REVERSE in srfi-41."
  (check-type pipe pipe)
  (labels ((%pipe-reverse (pipe rev)
             (lazy
              (if (pipe-null pipe)
                  rev
                  (%pipe-reverse (pipe-cdr pipe)
                                 (pipe-cons (pipe-car pipe) rev))))))
    (%pipe-reverse pipe +empty-pipe+)))

(defun pipe-scan (function base pipe)
  "STREAM-SCAN in srfi-41."
  (check-type function (or symbol function))
  (check-type pipe     pipe)
  (labels ((%pipe-scan (base pipe)
             (lazy
              (if (pipe-null pipe)
                  (pipe base)
                  (pipe-cons base
                             (%pipe-scan (funcall function
                                                  base (pipe-car pipe))
                                         (pipe-cdr pipe)))))))
    (%pipe-scan base pipe)))

(defun pipe-take (count pipe)
  "STREAM-TAKE in srfi-41."
  (check-type count (integer 0 *))
  (check-type pipe  pipe)
  (labels ((%pipe-take (count pipe)
             (lazy
              (if (or (pipe-null pipe)
                      (zerop count))
                  +empty-pipe+
                  (pipe-cons (pipe-car pipe)
                             (%pipe-take (1- count)
                                         (pipe-cdr pipe)))))))
    (%pipe-take count pipe)))

(defun pipe-take-while (predicate pipe)
  "STREAM-TAKE-WHILE in srfi-41."
  (check-type predicate (or symbol function))
  (check-type pipe      pipe)
  (labels ((%pipe-take-while (pipe)
             (lazy
              (cond ((pipe-null pipe) +empty-pipe+)
                    ((funcall predicate (pipe-car pipe))
                     (pipe-cons (pipe-car pipe)
                                (%pipe-take-while (pipe-cdr pipe))))
                    (t +empty-pipe+)))))
    (%pipe-take-while pipe)))

(defun pipe-unfold (mapper predicate generator base)
  "STREAM-UNFOLD in srfi-41."
  (check-type mapper    (or symbol function))
  (check-type predicate (or symbol function))
  (check-type generator (or symbol function))
  (labels ((%pipe-unfold (base)
             (lazy
              (if (funcall predicate base)
                  (pipe-cons (funcall mapper base)
                             (%pipe-unfold (funcall generator base)))
                  +empty-pipe+))))
    (%pipe-unfold base)))

(defun pipe-unfolds (generator seed)
  "STREAM-UNFOLDS in srfi-41."
  (check-type generator (or symbol function))
  (labels ((len-values (seed)
             (1- (length (multiple-value-list (funcall generator seed)))))
           (unfold-result-pipe (seed)
             (lazy
              (destructuring-bind
                  (next . results) (multiple-value-list (funcall generator seed))
                (pipe-cons results
                           (unfold-result-pipe next)))))
           (result-pipe->output-pipe (result-pipe i)
             (lazy
              (let ((result (nth (1- i) (pipe-car result-pipe))))
                (cond ((consp result)
                       (pipe-cons (car result)
                                  (result-pipe->output-pipe (pipe-cdr result-pipe)
                                                            i)))
                      ;; MEMO:
                      ;; '() and nil are the same in CL.
                      ((eq result :false)
                       (result-pipe->output-pipe (pipe-cdr result-pipe) i))
                      ((null result)
                       +empty-pipe+)
                      (t (error "can't happen."))))))
           (result-pipe->output-pipes (result-pipe)
             (labels ((rec (i outputs)
                        (if (zerop i)
                            (apply #'values outputs)
                            (rec (1- i) (cons (result-pipe->output-pipe result-pipe i)
                                              outputs)))))
               (rec (len-values seed) '()))))
    (result-pipe->output-pipes (unfold-result-pipe seed))))

(defun pipe-zip (pipe1 pipe2 &rest more-pipes)
  "STREAM-ZIP in srfi-41."
  (check-type pipe1 pipe)
  (check-type pipe2 pipe)
  (when (some (complement #'pipep) more-pipes)
    (error "There is at least one non-pipe object in MORE-PIPES."))
  (labels ((%pipe-zip (pipes)
             (lazy
              (if (some #'pipe-null pipes)
                  +empty-pipe+
                  (pipe-cons (mapcar #'pipe-car pipes)
                             (%pipe-zip (mapcar #'pipe-cdr pipes)))))))
    (%pipe-zip (list* pipe1 pipe2 more-pipes))))


;;====================================================================
