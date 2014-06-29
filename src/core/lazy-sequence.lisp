;;;; Last modified: 2014-06-29 10:22:54 tkych

;; cl-plus/src/core/lazy-sequence.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Lazy-Sequences
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.core.lazy-sequence
  (:documentation "
LAZY-SEQUENCE
=============

LAZY-SEQUENCE is an api for PIPE (SRFI-41).

A lazy-sequence is a data structure consisting of two fields called
the `accessed' and the `prophecy'.  `accessed' is an adjustable-vector
strored already accessed (maybe not forced) elements.  `prophecy' is a
pipe that is on lazy, not forced yet.  Roughly speaking, `prophecy' is
a last cdr of \"pipe\" in PAIP (or \"stream\" in SICP).

Why we introduce `LAZY-SEQUENCE' whereas `PIPE' aleady exists?
 1. For pretty-printing and bracket-read-macro, e.g., #[0 1 ..].
 2. For space efficiency removing kons-structures from pipes.
 3. For time efficiency for refering element which has already been
    accessed (random access for `accessed').

 (NB. 2 and 3 are case-by-case. Most cases lazy-sequences are inefficient)


Creating a Lazy-Sequence
------------------------

 0. make-lazy-seq, copy-lazy-seq.
 1. seq->lseq, pipe->lseq.
 2. induce, cycle, replicate, lazy-for, lfor.
 3. (lazy-)range, (lazy-)drop, (lazy-)zip, (lazy-)interleave, (lazy-)interpose.
 4. map*, remove*, remove-if*, remove-if-not, lazy-remove, lazy-remove-if
    substitute*, substitute-if*, substitute-if-not*, fill*.


When elements are forced?
-------------------------

-> The time when a realizer is called with the lazy-sequence,
   and the realizer refers the element that is not forced yet.

 Realizers:
  0. realize,
  1. lref, (if optional argument force? is true)
  2. take, take-while, take-until,
  3. reduce*, every*, notevery*, some*, notany*,
     count*, count-if*, count-if-not*, find*, find-if*, find-if-not*
     position*, position-if*, position-if-not*, collect-if*


LAZY-FLOW
---------

A lazy-flow is a data structure consisting of two parts called the
`current' and the `prophecy'.  `current' is a object that is the just
previous value which `prophecy' was forecd to.  Roughly speaking,
`LAZY-FLOW' is a lazy-sequence that stores the only one value.


Examples
--------

 * #[1 ..]       -> 1 2 3 4 ...
 * #[1 .. 10]    -> 1 2 3 4 ... 10
 * #[1 1 ..]     -> 1 1 1 1 ...
 * #[1 2 4 ..]   -> 1 2 4 8 16 32 ..
 * #[#\a ..]     -> #\a #\b #\c ...
 * #[#\a .. #\z] -> #\a #\b #\c ... #\z

 * (induce #'1+ 0)              -> 0 1 2 3 4 5 6 7 8 9 ...
 * (induce ^xy(+ x y) 0 1)      -> 0 1 1 2 3 5 8 13 21 34 ...
 * (induce ^xyz(+ x y z) 0 0 1) -> 0 0 1 1 2 4 7 13 24 44 ...
 * (replicate :foo :bar :baz)   -> :FOO :BAR :BAZ :FOO :BAR ...
 * (cycle #*10)                 -> 0 1 0 1 0 1 ...

 * (lfor x from 10 by 3)               -> 10 13 16 19 22 25 28 31 34 37 ...
 * (lfor x from 10 by 3 unless 'evenp) -> 13 19 25 31 37 43 49 55 61 67 ...
 * (lfor (cons x y)
     (x replicate :foo :bar :baz)
     (y from 10 by 3 unless 'evenp))
   -> (:FOO . 13) (:BAR . 19) (:BAZ . 25) (:FOO . 31) (:BAR . 37) ...

 * (defun primes ()
     (labels ((sieve (p)
                (lazy
                 (pipe-cons (pipe-car p)
                            (sieve (pipe-filter ^(/= 0 (mod @ (pipe-car p)))
                                                (pipe-cdr p)))))))
       (lazy-seq (pipe-from 2 1))))

   (primes) -> 2 3 5 7 11 13 17 19 23 29 ...

 * (defun random-chars ()
     (induce ^(code-char (+ (random #.(- (char-code #\z) (char-code #\a)))
                            #.(char-code #\a)))))
 * (take 20 (random-chars) 'string)  => "wmhdghxvwrubfocadkcj"
 * (take 20 (random-chars) 'string)  => "ocncjpbbucnuiwuruimg"

 ;; Examples from SRFI-41
 * (defun make-random-flow (seed)
     (check-type seed (integer 1 #.(1- (expt 2 32))))
     (induce-flow ^(mod (* @ 16807) 2147483647) seed))

 * (defvar *golden-ratio* (induce-flow ^(1+ (/ @)) 1))

 * (defvar *positive-rational-numbers*
     (induce ^x(let* ((n (floor x))
                      (y (- x n)))
                 (/ (- n -1 y)))
             1))


Notes
-----

 * Abstraction layers.

   -------------------------------------------- 
     Sequens {sequence, lazy-sequence}
   -------------------------------------------- 
     Lazy-Sequence {lazy-sequence, lazy-flow}
   -------------------------------------------- 
     SRFI-41 {pipe}
   --------------------------------------------  
     SRFI-45 {promise}
   -------------------------------------------- 
     Common Lisp
   --------------------------------------------    <- abstraction barrier
     Machine


References
----------

 [0] Philip L. Bewig (2004),
     Scheme Request for Implementation 40: A Library of Streams.
     http://srfi.schemers.org/srfi-40
     (srfi-40 is deprecated by srfi-41)

 [1] Philip L. Bewig (2007),
     Scheme Request for Implementation 41: Streams.
     http://srfi.schemers.org/srfi-41

 [2] André van Tonder (2004),
     Scheme Request for Implementation 45:
     Primitives for Expressing Iterative Lazy Algorithms.
     http://srfi.schemers.org/srfi-45
")
  (:nicknames #:cl+lazy-sequence)
  (:export ;; #:*print-length-for-lazy-sequence*
           #:lazy-sequence
           #:lazy-sequence-p
           #:empty-lazy-seq-p
           #:make-lazy-seq
           #:copy-lazy-seq
           #:lazy-seq
           #:sharp-bracket-reader
           #:lazy-for
           #:lfor
           #:pipe->lseq
           #:lseq->pipe
           #:realize    
           #:lref
           #:accessed-length
           #:all-accessed-p
           #:with-lazy-seq-iterator     ; Not export as cl+'s.
           #:dolseq
           #:replicate
           #:cycle
           #:induce

           ;; Export into sequens package.
           ;; Don't export as cl+'s symbol.
           #:lazy-repeat
           #:lazy-range
           #:lazy-replicate
           #:lazy-map
           #:lazy-reduce
           #:lazy-scan
           #:lazy-take
           #:lazy-take-until
           #:lazy-take-while
           #:lazy-drop
           #:lazy-drop-until
           #:lazy-drop-while
           #:lazy-remove
           #:lazy-remove-if
           #:lazy-zip
           #:lazy-interleave
           #:lazy-interpose
           #:lazy-repeat
           
           ;; (((EXPERIMANTAL)))
           #:lazy-flow
           #:lazy-flow-p
           #:make-lazy-flow
           #:copy-lazy-flow
           #:lseq->lflow
           #:lflow->lseq
           #:flush
           #:current-flow
           #:replicate-flow
           #:cycle-flow
           #:induce-flow)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:parse-body
                #:ensure-function
                #:switch
                #:if-let)
  (:import-from #:cl-plus.src.dev-util
                #:append1
                #:last1)
  (:import-from #:cl-plus.src.srfi.srfi-45
                #:promise
                #:promisep
                #:copy-promise
                #:lazy
                #:delay
                #:force
                #:lazyp
                #:eagerp)
  (:import-from #:cl-plus.src.srfi.srfi-41
                #:pipe
                #:+empty-pipe+
                #:pipe-car
                #:pipe-cdr
                #:pipe-cons
                #:pipe-null
                #:pipe-constant
                #:pipe-iterate
                #:pipe-filter
                #:pipe-drop
                #:pipe-drop-while
                #:pipe-take
                #:pipe-take-while
                #:pipe-scan
                #:pipe-from
                #:pipe-range
                #:pipe-map
                #:pipe-append
                #:list->pipe
                #:pipe-zip))

(in-package #:cl-plus.src.core.lazy-sequence)


;;--------------------------------------------------------------------
;; Lazy-Sequence
;;--------------------------------------------------------------------

(defstruct (lazy-sequence
            (:conc-name      %lseq-)
            (:constructor    %make-lseq (length accessed prophecy))
            (:copier         nil)
            (:predicate      lazy-sequence-p)
            (:print-function %print-lseq))
  (length     0 :type (integer 0 *))    ; max accessed index
  (accessed nil :type vector)
  (prophecy nil :type pipe))

(setf (documentation 'lazy-sequence 'structure) "
LAZY-SEQUENCE
=============

A lazy-sequence is a data structure consisting of two fields called
the `accessed' and the `prophecy'.  `accessed' is an adjustable-vector
strored already accessed (maybe not forced) elements.  `prophecy' is a
pipe that is on lazy, not forced yet.  Roughly speaking, `prophecy' is
a last cdr of \"pipe\" in PAIP (or \"stream\" in SICP).


Creating a Lazy-Sequence
------------------------

 0. make-lazy-seq, copy-lazy-seq
 1. seq->lseq, pipe->lseq
 2. induce, cycle, replicate, lazy-for (lfor)
 3. zip, interleave, interpose
 4. map*,


When elements are forced?
-------------------------

-> The time when a realizer is called with the lazy-sequence,
   and the realizer refers the element that is not forced yet.

 Realizers:
  0. realize,
  1. lref, (if optional argument force? is true)
  2. take, take-while, take-until,
  3. reduce*, every*, notevery*, some*, notany*,
     count*, count-if*, count-if-not*, find*, find-if*, find-if-not*
     position*, position-if*, position-if-not*, collect-if*


Examples
--------

 * #[1 ..]       -> 1 2 3 4 ...
 * #[1 .. 10]    -> 1 2 3 4 ... 10
 * #[1 1 ..]     -> 1 1 1 1 ...
 * #[1 2 4 ..]   -> 1 2 4 8 16 32 ..
 * #[#\a ..]     -> #\a #\b #\c ...
 * #[#\a .. #\z] -> #\a #\b #\c ... #\z

 * (induce #'1+ 0)              -> 0 1 2 3 4 5 6 7 8 9 ...
 * (induce ^xy(+ x y) 0 1)      -> 0 1 1 2 3 5 8 13 21 34 ...
 * (induce ^xyz(+ x y z) 0 0 1) -> 0 0 1 1 2 4 7 13 24 44 ...
 * (replicate :foo :bar :baz)   -> :FOO :BAR :BAZ :FOO :BAR ...
 * (cycle #*10)                 -> 0 1 0 1 0 1 ...

 * (lfor x from 10 by 3)               -> 10 13 16 19 22 25 28 31 34 37 ...
 * (lfor x from 10 by 3 unless 'evenp) -> 13 19 25 31 37 43 49 55 61 67 ...
 * (lfor (cons x y)
     (x replicate :foo :bar :baz)
     (y from 10 by 3 unless 'evenp))
   -> (:FOO . 13) (:BAR . 19) (:BAZ . 25) (:FOO . 31) (:BAR . 37) ...

 * (defun primes ()
     (labels ((sieve (p)
                (lazy
                 (pipe-cons (pipe-car p)
                            (sieve (pipe-filter ^(/= 0 (mod @ (pipe-car p)))
                                                (pipe-cdr p)))))))
       (lazy-seq (pipe-from 2 1))))

   (primes) -> 2 3 5 7 11 13 17 19 23 29 ...
")

;; TODO:
;;  * ADD: *print-level*
;;  * REFACTOR: TOO UGLY!
(defun %print-lseq (lseq stream depth)
  (declare (ignore depth))
  (let ((prophecy (%lseq-prophecy lseq))
        (accessed (%lseq-accessed lseq))
        (len      (%lseq-length   lseq)))
    (if (not *print-length*)
        ;; 1. Print ALL accessed elements.
        (format stream (if (and (eagerp prophecy) ; for not to force.
                                (pipe-null prophecy))
                           "#[~{~S~^ ~}]" "#[~{~S ~}..]")
                (coerce accessed 'list))
        ;; 2. Print accessed elements.
        (if (and (eagerp prophecy)      ; for not to force.
                 (pipe-null prophecy))
            ;; 2.1. Print non-prophecy sequence.
            (cond
              ((< (1+ *print-length*)
                  len)
               (format stream "#[~{~S ~}.. ~S]"
                       (coerce (subseq accessed 0 *print-length*)
                               'list)
                       (aref accessed (1- len))))
              (t
               (format stream "#[~{~S~^ ~}]"
                       (coerce accessed 'list))))
            ;; 2.2. Print lazy-sequence.
            (cond
              ((< (1+ *print-length*)
                  len)
               (format stream "#[~{~S ~}.. ~S ..]"
                       (coerce (subseq accessed 0 *print-length*)
                               'list)
                       (aref accessed (1- len))))
              ((<= len *print-length*)
               (format stream "#[~{~S ~}..]"
                       (coerce (subseq accessed 0 len) 'list)))
              (t
               (format stream "#[~{~S ~}..]"
                       (coerce (subseq accessed 0 (1+ *print-length*))
                               'list))))))))

(defmethod make-load-form ((lseq lazy-sequence) &optional env)
  (make-load-form-saving-slots lseq :slot-names '(accessed prophecy length)
                                    :environment env))

(defun accessed-length (lazy-sequence)
  (check-type lazy-sequence lazy-sequence)
  (%lseq-length lazy-sequence))

(defun empty-lazy-seq-p (lazy-sequence)
  (check-type lazy-sequence lazy-sequence)
  (and (zerop (%lseq-length lazy-sequence))
       (pipe-null (%lseq-prophecy  lazy-sequence))))

(declaim (function %lflow-prophecy))
(defun all-accessed-p (lazy-sequence)
  (etypecase lazy-sequence
    (lazy-sequence (pipe-null (%lseq-prophecy  lazy-sequence)))
    (lazy-flow     (pipe-null (%lflow-prophecy lazy-sequence)))))

(defun copy-lazy-seq (lseq)
  (let ((prophecy (%lseq-prophecy lseq)))
    (%make-lseq (%lseq-length lseq)
                (copy-seq (%lseq-accessed lseq))
                (if (pipe-null prophecy)
                    +empty-pipe+
                    (copy-promise prophecy)))))

;; CHECK: fill-pointer initial value
(defun make-lazy-seq (&optional (realized '()) (pipe +empty-pipe+))
  (check-type pipe     pipe)
  (check-type realized sequence)
  (let* ((len (length realized))
         (vec (make-array len :fill-pointer len :adjustable t
                              :initial-contents (coerce realized 'list))))
    (if (lazyp pipe)
        (%make-lseq len vec pipe)
        (let ((pipe-nil (force +empty-pipe+)))
          (labels ((rec (pipe)
                     (let ((kons (force pipe)))
                       (if (eq kons pipe-nil)
                           (%make-lseq len vec +empty-pipe+)
                           (let ((kar (cl+srfi-41::kar kons))) ; UGLY!
                             (vector-push-extend (if (lazyp kar) kar (force kar))
                                                 vec)
                             (incf len)
                             (let ((kdr (cl+srfi-41::kdr kons))) ; UGLY!
                               (if (lazyp kdr)
                                   (%make-lseq len vec kdr)
                                   (rec kdr))))))))
            (rec pipe))))))

(defmacro lazy-seq (pipe)
  `(make-lazy-seq '() ,pipe))


;;--------------------------------------------------------------------
;; sharp-bracket-reader, #[]
;;--------------------------------------------------------------------

;; check for %read-between-brackets
;; (defun read-by (reader input-string &rest args)
;;   (let* ((s (make-string-input-stream input-string)))
;;     (multiple-value-prog1
;;         (unwind-protect
;;              (apply reader s args)
;;           (close s)))))

;; (read-by #'%read-between-brackets "]")
;; (read-by #'%read-between-brackets "0 1]")
;; (read-by #'%read-between-brackets "0 1 ..]")
;; (read-by #'%read-between-brackets "0 1 \"object\" ..]")
;; (read-by #'%read-between-brackets "0 1 \"\"object\"\" ..]")
;; (read-by #'%read-between-brackets "0 1 \"..\" ..]")
;; (read-by #'%read-between-brackets "0 1 \"\" ..]")
;; (read-by #'%read-between-brackets "0 #[1..] ..]")

(defun %read-between-brackets (stream)
  (let ((left 1) (right 0) (buff (list #\()))
    (labels
        ((peek-next-char () (peek-char nil stream t nil t))
         (read-next-char () (read-char stream t nil t))
         (push-next-char () (push (read-next-char) buff))
         (read-string    () (if (char= #\" (peek-next-char))
                                (push-next-char) ; "" case:
                                (loop :for prev := (read-next-char) :then curr
                                      :for curr := (read-next-char) :then (read-next-char)
                                      :do (push prev buff)
                                      :until (and (char/= prev #\\)
                                                  (char= curr #\"))
                                      :finally (push curr buff)))))
      (loop :for char := (read-next-char)
            :while char
            :do ;; (print (coerce (reverse buff) 'string)) ; for DBG
                (case char
                  (#\"
                   (push #\" buff)
                   (read-string))
                  (#\#
                   (push #\# buff)
                   ;; dispatch-macro-char parameter exists case:
                   (when (digit-char-p (peek-next-char))
                     (push-next-char)
                     (loop :for char := (peek-next-char)
                           :if (digit-char-p char)
                             :do (push-next-char)
                           :else :do (loop-finish)))
                   ;; Nest case:
                   (when (char= #\[ (peek-next-char))
                     (let ((%left 0) (%right 0))
                       (loop :for c := (read-next-char)
                             :do (push c buff)
                                 (case c ; nest nest ... case:
                                   (#\[ (incf %left))
                                   (#\] (incf %right))
                                   (#\" (read-string)))
                             :until (and (char= c #\])
                                         (= %left %right))))))
                  (#\.
                   (if (char/= #\. (peek-next-char))
                       (push char buff)
                       (progn
                         (read-next-char) ; discard next #\.
                         (when (char= #\. (peek-next-char))
                           (error "Too many dots.")) ; e.g., #[1...10]
                         ;; MEMO: 2014-05-15
                         ;; Following (setf ...) form is for:
                         ;;  1. not exporting the symbol |..| from :cl-plus.src.core.lazy-sequence.
                         ;;  2. not importing the symbol |..| into any package at read time.
                         ;; NB. The spaces of the string (at first and last) are for #[1..10] => #[1 .. 10].
                         (setf buff
                               (append '#.(coerce (reverse " CL-PLUS.SRC.CORE.LAZY-SEQUENCE::|..| ")
                                                  'list)
                                       buff)))))
                  (#\[
                   (incf left)
                   (push char buff))
                  (#\]
                   (incf right)
                   (if (= left right)
                       (loop-finish)
                       (push char buff)))
                  (t
                   (push char buff)))))
    (push #\) buff)
    (read-from-string (coerce (nreverse buff) 'string)
                      nil nil)))


(defun %generate-lazy-seq (dots-sym elements)
  (let ((dots-pos (position dots-sym elements :test 'eq)))

    (when (and dots-pos (zerop dots-pos)) ; e.g., #[ .. xn] or #[..]
      (error "Start term is missing."))
    
    (if (not dots-pos)
        ;; 1. All realized case:
        (make-lazy-seq elements +empty-pipe+)

        ;; 2. DOTS exists case:
        (let ((last-pos   (1- (length elements)))
              (char-lseq? (every (lambda (x) (or (characterp x) (eq x dots-sym)))
                                 elements)))
          (when char-lseq?
            (setf elements
                  (mapcar (lambda (x) (if (characterp x) (char-code x) x))
                          elements)))
          (if (/= dots-pos last-pos)          ; <-> if not #[x0 x1 ~ xi ..]
              (if (/= dots-pos (1- last-pos)) ; <-> if not #[x0 x1 ~ xi .. xn]
                  (error "Dots is not the last term nor the second last term.")

                  ;; 2.1. Finite case: #[x0 x1 ~ xi .. xn]
                  (let ((xn (car (last elements)))
                        (contents (butlast elements 2))) ; (x0 x1 ~ xi)
                    (flet ((mk-lseq (pipe)
                             (if char-lseq?
                                 (make-lazy-seq (mapcar #'code-char contents)
                                                (pipe-map #'code-char pipe))
                                 (make-lazy-seq contents pipe))))
                      (if (every (lambda (x) (equal x xn)) contents)
                          (if (null (cdr contents))
                              (mk-lseq +empty-pipe+)        ; #[1 .. 1]   -> #[1]
                              (mk-lseq (pipe-constant xn))) ; #[1 1 .. 1] -> #[1 ..]
                          (if (some (complement #'numberp) contents)
                              (error "There is no order relation between terms: ~{~S, ~}... ~S."
                                     contents xn)
                              (destructuring-bind (x0 . xs) contents
                                (if (null xs) ; if #[x0 .. xn]
                                    (if (<= x0 xn)
                                        (mk-lseq (pipe-range (1+ x0) (1+ xn) 1))
                                        (lazy-seq +empty-pipe+))
                                    (let ((xi (last1 xs)))
                                      (if-let (diff (arithmetic-progression-p contents))
                                        (if (plusp diff)
                                            (if (< xn xi)
                                                (error "Can't parse.") ; e.g., #[1 4 .. 1]
                                                (mk-lseq (pipe-range (+ xi diff) (1+ xn) diff)))
                                            (if (< xi xn)
                                                (error "Can't parse.") ; e.g., #[10 5 .. 6]
                                                (mk-lseq (pipe-range (+ xi diff) (1- xn) diff))))
                                        (if-let (ratio (geometric-progression-p contents))
                                          ;; UGLY
                                          (cond ( ;; Monotinic Increase case:
                                                 (<= 1 ratio)
                                                 (if (< xn xi)
                                                     (error "Can't parse.")                                                       
                                                     (mk-lseq (pipe-take-while
                                                               (lambda (x) (<= x xn))
                                                               (pipe-iterate (lambda (term) (* ratio term))
                                                                             (* xi ratio))))))
                                                ( ;; Monotinic Decrease case:
                                                 (< 0 ratio 1) ; ensure ratio /= 0.
                                                 (if (< xi xn)
                                                     (error "Can't parse.")
                                                     (mk-lseq (pipe-take-while
                                                               (lambda (x) (<= xn x))
                                                               (pipe-iterate (lambda (term) (* ratio term))
                                                                             (* xi ratio))))))
                                                ( ;; Oscillating Decrease case:
                                                 (<= -1 ratio) ; ensure ratio < 0.
                                                 (let ((more? t))
                                                   (if (plusp xn)
                                                       ;; #[1 -1/2 1/4 .. 1/15] -> 1 -1/2 1/4 -1/8
                                                       ;; #[1 -1/2 1/4 .. 1/16] -> 1 -1/2 1/4 -1/8 1/16
                                                       ;; #[1 -1/2 1/4 .. 1/17] -> 1 -1/2 1/4 -1/8 1/16 -1/32
                                                       (mk-lseq (pipe-take-while
                                                                 (lambda (x)
                                                                   (cond ((not more?) nil)
                                                                         ((= x xn) (setf more? nil) t)
                                                                         (t (or (minusp x)
                                                                                (< xn x)))))
                                                                 (pipe-iterate (lambda (term) (* ratio term))
                                                                               (* xi ratio))))
                                                       ;; #[1 -1/2 1/4 .. -1/31] -> 1 -1/2 1/4 -1/8 1/16
                                                       ;; #[1 -1/2 1/4 .. -1/32] -> 1 -1/2 1/4 -1/8 1/16 -1/32
                                                       ;; #[1 -1/2 1/4 .. -1/33] -> 1 -1/2 1/4 -1/8 1/16 -1/32 1/64
                                                       (mk-lseq (pipe-take-while
                                                                 (lambda (x)
                                                                   (cond ((not more?) nil)
                                                                         ((= x xn) (setf more? nil) t)
                                                                         (t (or (plusp x)
                                                                                (< x xn)))))
                                                                 (pipe-iterate (lambda (term) (* ratio term))
                                                                               (* xi ratio)))))))
                                                (t
                                                 ;; Oscillating Increase case:
                                                 ;; (< ratio -1)
                                                 (let ((more? t))
                                                   (if (plusp xn)
                                                       ;; #[1 -2 4..15] -> 1 -2 4 -8
                                                       ;; #[1 -2 4..16] -> 1 -2 4 -8 16
                                                       ;; #[1 -2 4..17] -> 1 -2 4 -8 16 -32
                                                       (mk-lseq (pipe-take-while
                                                                 (lambda (x)
                                                                   (cond ((not more?) nil)
                                                                         ((= x xn) (setf more? nil) t)
                                                                         (t (< x xn))))
                                                                 (pipe-iterate (lambda (term) (* ratio term))
                                                                               (* xi ratio))))
                                                       ;; #[1 -2 4..-31] -> 1 -2 4 -8 16
                                                       ;; #[1 -2 4..-32] -> 1 -2 4 -8 16 -32
                                                       ;; #[1 -2 4..-33] -> 1 -2 4 -8 16 -32 64
                                                       (mk-lseq (pipe-take-while
                                                                 (lambda (x)
                                                                   (cond ((not more?) nil)
                                                                         ((= x xn) (setf more? nil) t)
                                                                         (t (< xn x))))
                                                                 (pipe-iterate (lambda (term) (* ratio term))
                                                                               (* xi ratio))))))))
                                          (error "Can't parse.")))))))))))

              ;; 2.2. Infinite case: #[x0 x1 ~ xi ..]
              (let ((contents (butlast elements)))
                (flet ((mk-lseq (pipe)
                         (if char-lseq?
                             (make-lazy-seq (mapcar #'code-char contents)
                                            (pipe-map #'code-char pipe))
                             (make-lazy-seq contents pipe))))
                  (if (= 1 last-pos)    ; <-> if #[x0 ..]
                      (let ((x0 (first contents)))
                        (if (numberp x0)
                            (mk-lseq (pipe-from (1+ x0)))
                            (mk-lseq (pipe-constant x0))))
                      (if (some (complement #'numberp) contents)
                          (mk-lseq (apply #'pipe-constant contents))
                          (let ((xi (last1 contents)))
                            (if-let (diff (arithmetic-progression-p contents))
                              (mk-lseq (pipe-from (+ xi diff) diff))
                              (if-let (ratio (geometric-progression-p contents))
                                (mk-lseq (pipe-iterate (lambda (term) (* ratio term))
                                                       (* xi ratio)))
                                (mk-lseq (apply #'pipe-constant contents))))))))))))))


(defun arithmetic-progression-p (contents)               ; a, a+d, a+2d, a+3d, ...
  (let* ((diffs (mapcar #'- (rest contents) contents)))  ; d, d, d, ...
    (when (= 1 (length (remove-duplicates diffs :test #'=)))
      (first diffs))))

(defun geometric-progression-p (contents)
  (destructuring-bind (a ar  . terms) contents  ; a, ar, ar^2, ar^3, ...
    (if (zerop a)
        nil
        (let ((ratio (/ ar a)))
          (when (loop :for term :in terms
                      :for term0 := (* ar ratio) :then (* term0 ratio)
                      :always (= term term0))
            ratio)))))

;; TODO:
;;  ? progression by user-defined order (cl+cdr-8:compare and ??metric??).
;;  * CHECK: cl implementations that are able to dump functions into
;;           compiled files, or not.

(defun sharp-bracket-reader (stream char parameter)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\] (get-macro-character #\)))
    (let ((elements (%read-between-brackets stream)))
      (if parameter
          (with-gensyms (dots)
            `(%generate-lazy-seq ',dots (list ,@(sublis `((|..| . ',dots))
                                                        elements :test #'eq))))
          
          ;; CL implementations that can dump functions into compiled files.
          #+(or ccl clisp) (%generate-lazy-seq '|..| elements)
          
          ;; CL implementations that can not dump functions into compiled files.
          #+(or sbcl abcl ecl)
          ;; FIXME: 2014-04-26
          ;;  For smooth unification of lazy-sequence into common lisp,
          ;;  the read-macro `#[' should create a lazy-sequence at READ TIME, like `#('.
          ;;  If `#[' would create a lazy-sequence at read time, it is required
          ;;  that functions are dumped into compiled files when we compile the file
          ;;  that contains `#[..]'. However, accoding to CLHS, functions are
          ;;  not externalizable objects, i.e. an object that can be used as a
          ;;  literal object in code to be processed by the file compiler.
          ;;   cf. http://clhs.lisp.se/Body/03_bdbb.htm
          ;;       http://clhs.lisp.se/Body/26_glo_e.htm#externalizable_object
          ;;       http://clhs.lisp.se/Body/03_bda.htm
          (with-gensyms (dots)
            `(%generate-lazy-seq ',dots (list ,@(sublis `((|..| . ',dots))
                                                        elements :test #'eq))))
          
          ;; CL implementations that are not checked yet.
          #-(or ccl clisp sbcl abcl ecl)
          (with-gensyms (dots)
            `(%generate-lazy-seq ',dots (list ,@(sublis `((|..| . ',dots))
                                                        elements :test #'eq))))
          ))))


(setf (documentation 'sharp-bracket-reader 'function) "
#[ Sharp-Bracket-Reader ]
=========================

Sharp-bracket-reader, #[] is a read-macro for lazy-sequences.
Usually (currently, ccl and clisp only), #[] makes a lazy-sequence at read time.
If the parameter is supplied (e.g., #42[]), it does not make a
lazy-sequence at read time and evaluates all terms before making a
lazy-sequence (i.e., sharp-bracket-reader acts as ordinal macros).

cf. Haskell 2010, Languege Report, p.21, 3.10 Arithmetic Sequences.


Supplied Terms and Result
=========================

 #[term_0 term_1 term_2 ~ term_i .. term_n]


#[term_0 ..]
------------

 0. If term_0 is a number:
    --> (infinite arithmetic progression for numbers):
        term_0, term_0 + 1, term_0 + 2, ...

 1. If term_0 is a character:
    --> (infinite arithmetic progression for characters):
        code-char(code_0), code-char(code_0 + 1), code-char(code_0 + 2), ...
        where
          code0 := char-code(term_0).

 2. Otherwise:
    --> (infinite replicate progression):
        term_0, term_0, term_0, ...


#[term_0 .. term_n]
-------------------

 0. If term_0 and term_n are both number:
    --> (finite arithmetic progression for numbers):
        term_0, term_0 + 1, term_0 + 2, ..., term_n.
        NB. if term_0 > term_n, returns the empty lazy-sequence.

 1. If term_0 and term_n are both character:
    --> (finite arithmetic progression for characters):
        code-char(code_0), code-char(code_0 + 1), code-char(code_0 + 2), ..., code-char(code_0 + n).
        where
          code_0 := (char-code term_0).
        NB. if code_0 > code_n, returns the empty lazy-sequence.

 2. Otherwise:
    2.1. If (equal term_0 term_n): --> term_0.
    2.2. Otherwise:                --> ERROR!


#[term_0 term_1 ..]
-------------------

 0. If term_0 and term_1 are both number:
    --> (infinite arithmetic progression for numbers):
        term_0, term_0 + diff, term_0 + diff*2, ...
        where
          diff := term_1 - term_0.

 1. If term_0 and term_1 are both character:
    --> (infinite arithmetic progression for characters):
        code-char(code_0), code-char(code_0 + diff), code-char(code_0 + diff*2), ...
        where
          code_i := char-code(term_i), for i = 0,1.,
          diff   := code_1 - code_0.

 2. Otherwise:
    --> (infinite replicate progression):
        term_0, term_1, term_0, term_1, term_0, term_1, ...


#[term_0 term_1 .. term_n]
--------------------------

 0. If (equal term_0 term_1 term_n):
    --> (infinite replicate progression):
        term_0, term_0, term_0, term_0, ...

 1. If term_0, term_1 and term_n are all number:
    --> (finite arithmetic progression for numbers):
        term_0, term_0 + diff, term_0 + diff*2, ..., last.
        where
          diff := term_1 - term_0,
          last := min((term_0 + diff*n), term_n) if diff >= 0
                  max((term_0 + diff*n), term_n) if diff <  0
        NB. If term_1 > term_n and diff >= 0, then ERROR!
            If term_1 < term_n and diff <= 0, then ERROR!

 2. If term_0, term_1 and term_n are all character:
    --> (finite arithmetic progression for characters):
        code-char(code_0), code-char(code_0 + diff), code-char(code_0 + diff*2), ..., last.
        where
          code_i := char-code(term_i), for i = 0,1.,
          diff   := code_1 - code_0,
          last   := char-code(min((code_0 + n*diff), code_n)).
        NB. If code_1 > code_n and diff >= 0, then ERROR!
            If code_1 < code_n and diff <= 0, then ERROR!

 3. Otherwise:
    --> ERROR!


#[term_0 term_1 term_2 ..]
--------------------------

 0. If term_0, term_1 and term_2 are all number and term_1 - term_0 = term_2 - term_1:
    --> (infinite arithmetic progression for numbers):
        term_0, term_0 + diff, term_0 + diff*2, term_0 + diff*3, ...
        where
          diff := term_1 - term_0.

 1. If term_0, term_1 and term_2 are all number and term_1/term_0 = term_2/term_1:
    --> (infinite geometric progression for numbers):
        term_0, term_0 * ratio, term_0 * ratio^2, term_0 * ratio^3, ...
        where
          ratio := term_1/term_0.

 2. If term_0, term_1 and term_2 are all character and code_1 - code_0 = code_2 - code_1:
    --> (infinite arithmetic progression for characters):
        code-char(code_0), code-char(code_0 + diff), code-char(code_0 + diff*2), code-char(code_0 + diff*3), ...
        where
          code_i := char-code(term_i), for i = 0,1,2.,
          diff   := code_1 - code_0.

 3. If term_0, term_1 and term_2 are all character and code_1/code_0 = code_2/code_1:
    --> (infinite geometric progression with characters):
        code-char(code_0), code-char(code_0 * ratio), code-char(code_0 * ratio^2), code-char(code_0 * ratio^3), ...
        where
          code_i := char-code(term_i), for i = 0,1,2.,
          ratio  := code_1/code_0.

 4. Otherwise:
    --> (infinite replicate progression):
        term_0, term_1, term_2, term_0, term_1, term_2, ...


#[term_0 term_1 term_2 .. term_n]
---------------------------------

 0. If term_0, term_1 and term_2 are all number and term_1 - term_0 = term_2 - term_1:
    --> (finite arithmetic progression for numbers):
        term_0, term_0 + diff, term_0 + diff*2, term_0 + diff*3, ..., last.
        where
          diff := term_1 - term_0,
          last := min((term_0 + diff*n), term_n).
        NB. If term_2 > term_n and diff >= 0, then ERROR!
            If term_2 < term_n and diff <= 0, then ERROR!

 1. If term_0, term_1 and term_2 are all number and term_1/term_0 = term_2/term_1:
    --> (finite geometric progression for numbers):
        term_0, term_0 * ratio, term_0 * ratio^2, term_0 * ratio^3, ..., last.
        where
          ratio := term_1/term_0,
          last  := min((term_0 * ratio^n), term_n).

 2. If term_0, term_1 and term_2 are all character and code_1 - code_0 = code_2 - code_1:
    --> (finite arithmetic progression for characters):
        code-char(code_0), code-char(code_0 + diff), code-char(code_0 + diff*2), code-char(code_0 + diff*3), ..., last.
        where
          code_i := char-code(term_i), for i = 0,1,2.,
          diff   := code_1 - code_0,
          last   := char-code(min((code_0 + n*diff), code_n)).
        NB. If code_2 > code_n and diff >= 0, then ERROR!
            If code_2 < code_n and diff <= 0, then ERROR!

 3. If term_0, term_1 and term_2 are all character and code_1/code_0 = code_2/code_1:
    --> (finite geometric progression with characters):
        code-char(code_0), code-char(code_0 * ratio), code-char(code_0 * ratio^2), code-char(code_0 * ratio^3), ..., last.
        where
          code_i := char-code(term_i), for i = 0,1,2.,
          ratio  := code_1/code_0,
          last   := code-char(min((term_0 * ratio^n), term_n)).

 4. Otherwise:
    If (equal term_0 term_1 term_2 term_n):
       --> (infinite replicate progression):
           term_0, term_0, term_0, term_0, ...
    Otherwise:
       --> ERROR!


#[term_0 ~ term_i .. term_n], i > 2
-----------------------------------

 A. All terms are number:

    0. term_0 ~ term_i is satisfies arithmetic relations:
       0. term_n does not exists:
          --> (infinite arithmetic progression)
       1. term_n exists, diff >= 0 and term_i <= term_n:
          --> (finite arithmetic progression)
       2. term_n exists, diff < 0 and term_i >= term_n:
          --> (finite arithmetic progression)
       3. Otherwise:
          --> ERROR!

    1. term_0 ~ term_i is satisfies geometric relations:
       0. term_n does not exists:
          --> (infinite geometric progression)
       1. term_n exists and term_i <= term_n:
          --> (finite geometric progression)
       2. Otherwise:
          --> ERROR!

    3. Otherwise:
       --> ERROR!

 B. Otherwise:
    0. term_n does not exists:
       --> (infinite replicate progression)

    1. term_n exists:
       0. All terms are equal:
       --> (infinite term_0 progression)

       1.Otherwise:
       --> ERROR!
")


;;--------------------------------------------------------------------
;; realizers
;;--------------------------------------------------------------------
;; realize
;; lref
;; with-lazy-seq-iterator

(defun realize (lazy-sequence)
  (check-type lazy-sequence lazy-sequence)
  (let ((prophecy (%lseq-prophecy lazy-sequence)))
    (if (pipe-null prophecy) ; FORCE!
        (values nil nil)
        (let ((new-value    (pipe-car prophecy))  ; FORCE!
              (new-prophecy (pipe-cdr prophecy))) ; FORCE!
          (vector-push-extend new-value (%lseq-accessed lazy-sequence))
          (setf (%lseq-prophecy lazy-sequence) new-prophecy)
          (incf (%lseq-length lazy-sequence))
          (values new-value t)))))

(setf (documentation 'realize 'function) "
REALIZE lazy-sequence => forced-value continuep

Forces the edge of `lazy-sequence'.
Returns forced-value and boolean that expresses whether the lazy-sequence
continues or not.
")

(defun lref (lazy-sequence index &optional (force? t) default)
  (check-type lazy-sequence lazy-sequence)
  (check-type index         (integer 0 *))
  (if (< index (%lseq-length lazy-sequence))
      (let ((val (aref (%lseq-accessed lazy-sequence) index)))
        (if (lazyp val)
            (if force?
                (values (setf (aref (%lseq-accessed lazy-sequence) index)
                              (force val)) ; FORCE!
                        t)
                (values default nil))
            (values val t)))
      (if (not force?)
          (values default nil)
          (let ((pipe-nil (force +empty-pipe+)))
            (dotimes (_ (- index (%lseq-length lazy-sequence)))
              (let* ((prophecy (%lseq-prophecy lazy-sequence))
                     (kons     (force prophecy))) ; FORCE!
                (if (eq kons pipe-nil)
                    (values nil nil)
                    (progn
                      (vector-push-extend (cl+srfi-41::kar kons) ; UGLY but NOT-FORCE!
                                          (%lseq-accessed lazy-sequence))
                      (setf (%lseq-prophecy lazy-sequence) (pipe-cdr prophecy)) ; FORCE!
                      (incf (%lseq-length lazy-sequence))))))
            (realize lazy-sequence)))))

(setf (documentation 'lref 'function) "
LREF lazy-sequence index &optional (force? t) default => element, continuep

Returns the element of `lazy-sequence' whose index is `index'.
If the element is on eager, returns it.
If the element is on delay and `force?' is true, returns the forced element.
If the element is on delay and `force?' is false, returns `default'.

LREF accsesses the all elements whose index is within 0 to `index'
 (NB. `access' does not mean `force').
")

(defun %make-lazy-seq-iterator (lazy-sequence &optional (force? t))
  (check-type lazy-sequence lazy-sequence)
  (let ((index 0)
        (accessed-length (%lseq-length lazy-sequence)))
    (lambda ()
      (multiple-value-bind (value more?)
          (if (< index accessed-length)
              (let ((val (aref (%lseq-accessed lazy-sequence) index)))
                (if (lazyp val)
                    (if force?
                        (values (setf (aref (%lseq-accessed lazy-sequence) index)
                                      (force val)) ; FORCE!
                                t)
                        (values NIL NIL))
                    (values val t)))
              (if force?
                  (realize lazy-sequence)
                  (values NIL NIL)))
        (multiple-value-prog1
            (values more? index value)
          (incf index))))))

(defmacro with-lazy-seq-iterator ((name lazy-sequence &optional (force? t))
                                  &body body)
  (with-gensyms (iter)
    `(let ((,iter (%make-lazy-seq-iterator ,lazy-sequence ,force?)))
       (macrolet ((,name () `(funcall ,',iter)))
         ,@body))))

(defmacro dolseq ((var lseq result) &body body)
  (multiple-value-bind (tag-statements declarations) (alexandria:parse-body body)
    (once-only (lseq)
      (with-gensyms (next index more?)
        `(with-lazy-seq-iterator (,next ,lseq)
           (loop
             (multiple-value-bind (,index ,var ,more?) (,next)
               ,@declarations
               (declare (ignore ,index))
               ;; TODO:
               ;; force? == nil, default
               (unless ,more?
                 (return ,result))
               (tagbody ,@tag-statements)))
           ,result)))))


;;------------------------------------------------
;; convertors
;;------------------------------------------------

(defun pipe->lseq (pipe)
  (check-type pipe pipe)
  (labels ((rec (acc pipe)
             (if (or (lazyp pipe)
                     (pipe-null pipe))
                 (values acc pipe)
                 (let* ((kons (force pipe))
                        (kar  (cl-plus.src.srfi.srfi-41::kar kons))) ; UGLY!
                   (rec (cons (if (lazyp kar) kar (force kar))
                              acc)
                        (cl-plus.src.srfi.srfi-41::kdr kons)))))) ; UGLY!
    (multiple-value-bind (accessed prophecy) (rec '() pipe)
      (make-lazy-seq accessed prophecy))))

(defun lseq->pipe (lseq)
  (check-type lseq lazy-sequence)
  (let ((accessed (coerce (%lseq-accessed lseq) 'list))
        (prophecy (%lseq-prophecy lseq)))
    (pipe-append (list->pipe accessed) prophecy)))


;;------------------------------------------------
;; generators
;;------------------------------------------------

(defun replicate (&rest objects)
  (lazy-seq (apply #'pipe-constant objects)))

(setf (documentation 'replicate 'function) "
LAZY-REPLICATE &rest objects => lazy-sequence

 (replicate obj0 obj1 ...) == (cycle '(obj0 obj1 ...))
 (cycle seq) == (apply #'replicate (coerce seq 'list))

Examples
--------
 * (replicate 0)   -> 0 0 0 0 0 ...
 * (replicate 0 1) -> 0 1 0 1 0 ...
")

(defun %repeatedly (successor-function)
  (labels ((%%repeater ()
             (lazy (pipe-cons (funcall successor-function) (%%repeater)))))
    (lazy-seq (%%repeater))))

(defun cycle (sequence)
  (check-type sequence sequence)
  (let ((len (length sequence))
        (vec (if (simple-vector-p sequence)
                 (copy-seq sequence)    ; COERCE does not always return new object.
                 (coerce sequence 'simple-vector))))
    (declare (type (integer 0 *) len))
    (declare (type simple-vector vec))
    (when (zerop len)
      (error "The length of SEQUENCE must be at least 1."))
    (%repeatedly (let ((i 0))
                   (declare (type (integer 0 *) i))
                   (lambda ()
                     (prog1
                         (svref vec (mod i len))
                       (incf i)))))))

(setf (documentation 'cycle 'function) "
CYCLE sequence => lazy-sequence

 (replicate obj0 obj1 ...) == (cycle '(obj0 obj1 ...))
 (cycle seq) == (apply #'replicate (coerce seq 'list))

NB. `sequence' must have at least one element.

Examples
--------
 * (cycle '(3 2 1 boom!))             -> 3 2 1 BOOM! 3 2 1 BOOM! 3 2 ...
 * (cycle #(foo baz bar))             -> FOO BAZ BAR FOO BAZ BAR FOO ...
 * (take 10 (cycle #*01) 'bit-vector) => #*1010101010
")

(defun %iterate (successor-function seed)
  (make-lazy-seq (list seed)
                 (pipe-cdr (pipe-iterate successor-function seed))))

(defun %induce (successor-function &rest seeds)
  (labels ((%%inducer (prevs)
             (lazy
              (let ((next (apply successor-function prevs)))
                (pipe-cons next
                           (%%inducer (append1 (rest prevs) next)))))))
    (make-lazy-seq seeds (%%inducer seeds))))

(defun induce (successor-function &rest seeds)
  (check-type successor-function (or symbol function))
  (case (length seeds)
    (0 (%repeatedly successor-function))
    (1 (%iterate successor-function (first seeds)))
    (t (apply #'%induce successor-function seeds))))

#-common-lisp
(define-compiler-macro induce (&whole form successor-function &rest seeds)
  (if (listp seeds)
      (case (length seeds)
        (0 (once-only (successor-function)
             `(progn
                (check-type ,successor-function (or function symbol))
                (%repeatedly ,successor-function))))
    
        (1 (once-only (successor-function)
             `(progn
                (check-type ,successor-function (or function symbol))
                (%iterate ,successor-function ,(first seeds)))))
    
        (t (once-only (successor-function)
             `(progn
                (check-type ,successor-function (or function symbol))
                (%induce ,successor-function ,@seeds)))))
      form))

(setf (documentation 'induce 'function) "
INDUCE successor-function &rest seeds => lazy-sequence

Examples
--------
 * (defvar *randoms*
     (induce ^(random 10))) => *RANDOMS*
   (take 10 *randoms*)      => (1 3 7 2 8 9 1 7 9 8)
   (take 11 *randoms*)      => (1 3 7 2 8 9 1 7 9 8 0)

 * (induce #'1+ 1)              -> 1 2 3 4 5 6 7 8 9 10 ...
 * (induce ^xy(+ x y) 0 1)      -> 0 1 1 2 3 5 8 13 21 34 ...
 * (induce ^xyz(+ x y z) 0 0 1) -> 0 0 1 1 2 4 7 13 24 44 ...
")

(defun lazy-range (&optional (start 0) (end nil) (step 1))
  (check-type start number)
  (check-type end   (or null number))
  (check-type step  number)
  (if end
      (make-lazy-seq (list start)
                     (pipe-range (+ start step) end step))
      (make-lazy-seq (list start)
                     (pipe-from (+ start step) step))))

(defun lazy-repeat (count object)
  (check-type count (integer 0 *))
  (lazy-seq (pipe-take count (pipe-constant object))))


;;--------------------------------------------------------------------
;; lazy-for/lfor
;;--------------------------------------------------------------------

(defun %gard-clause->pipe (base-pipe gard-clause)
  (unless (zerop (mod (length gard-clause) 2))
    (error "malfold LAZY-FOR clause: (... ~{~S~^ ~})."
           gard-clause))
  (labels
      ((rec (clause)
         (if (null clause)
             base-pipe
             (destructuring-bind (pred keyword . rest) clause
               (switch (keyword :test #'string-equal)
                 ("WHEN"
                  `(pipe-filter ,pred ,(rec rest)))
                 ("UNLESS"
                  `(pipe-filter (complement (ensure-function ,pred)) ,(rec rest)))
                 ("WHILE"
                  `(pipe-take-while ,pred ,(rec rest)))
                 ("UNTIL"
                  `(pipe-take-while (complement (ensure-function ,pred)) ,(rec rest)))
                 (t (error "~S is an unknown keyword in gard-clause in LAZY-FOR."
                           keyword)))))))
    (rec (reverse gard-clause))))


;; (var :in seq {gard}*)
(defun %in-clause->pipe (seq gard-clause)
  (flet ((%%ensure-pipe (seq)
           (once-only (seq)
             `(etypecase ,seq
                (sequence      (list->pipe (coerce ,seq 'list)))
                (lazy-sequence (lseq->pipe ,seq))
                (pipe          ,seq)))))
    (if gard-clause
        (%gard-clause->pipe (%%ensure-pipe seq) gard-clause)
        (%%ensure-pipe seq))))

(defun %by-clause->pipe (from-key start step &optional to-key end)
  (if (not end)
      (once-only (step)
        `(if (or (functionp ,step) (symbolp ,step))
             (pipe-iterate ,step ,start)
             (pipe-from ,start
                        ,(switch (from-key :test #'string=)
                           ("upfrom"   step)
                           ("downfrom" `(- ,step))
                           (t (error "never never never occur!"))))))
      (if (string= "from" from-key)
          (once-only (start end step)
            `(cond ((< ,end ,start) +empty-pipe+)
                   ((or (functionp ,step) (symbolp ,step))
                    ,(with-gensyms (x)
                       `(pipe-take-while (lambda (,x) (<= ,x ,end))
                                         (pipe-iterate ,step ,start))))
                   (t (pipe-range ,start (1+ ,end) ,step))))
          (once-only (step)
            `(if (or (functionp ,step) (symbolp ,step))
                 (pipe-take-while ,(with-gensyms (x)
                                     `(lambda (,x)
                                        ,(switch (to-key :test #'string=)
                                           ("upto"   `(<= ,x ,end))
                                           ("downto" `(<= ,end ,x))
                                           ("below"  `(< ,x ,end))
                                           ("above"  `(< ,end ,x))
                                           (t (error "never never never occur!")))))
                                  (pipe-iterate ,step ,start))
                 (pipe-range ,start
                             ,@(switch (to-key :test #'string=)
                                 ("upto"   `((1+ ,end) ,step))
                                 ("downto" `((1- ,end) (- ,step)))
                                 ("below"  `(,end ,step))
                                 ("above"  `(,end (- ,step)))
                                 (t (error "never never never occur!")))))))))


;; (var :from start [{:upto|:downto|:to|:below|:above} end] [:by step] {gard}*)
(defun %from-clause->pipe (start rest-clause)
  (if (null rest-clause)
      `(pipe-from ,start)
      (destructuring-bind (keyword end/step . xs) rest-clause
        (switch (keyword :test #'string-equal)
          ("BY"
           (let ((base-pipe (%by-clause->pipe "upfrom" start end/step)))
             (if xs
                 (%gard-clause->pipe base-pipe xs)
                 base-pipe)))
          
          ("TO"
           (if (null xs)
               `(pipe-range ,start (1+ ,end/step))
               (if (string-not-equal "BY" (first xs))
                   (%gard-clause->pipe `(pipe-range ,start (1+ ,end/step)) xs)
                   (destructuring-bind (by step . gard-clause) xs
                     (declare (ignore by))
                     (%gard-clause->pipe
                      (%by-clause->pipe "from" start step "to" end/step)
                      gard-clause)))))
          ("UPTO"
           ;; following clause is same as "TO".
           (if (null xs)
               `(pipe-range ,start (1+ ,end/step))
               (if (string-not-equal "BY" (first xs))
                   (%gard-clause->pipe `(pipe-range ,start (1+ ,end/step)) xs)
                   (destructuring-bind (by step . gard-clause) xs
                     (declare (ignore by))
                     (%gard-clause->pipe
                      (%by-clause->pipe "from" start step "to" end/step)
                      gard-clause)))))
          ("BELOW"
           (if (null xs)
               `(pipe-range ,start ,end/step)
               (if (string-not-equal "BY" (first xs))
                   (%gard-clause->pipe `(pipe-range ,start ,end/step) xs)
                   (destructuring-bind (by step . gard-clause) xs
                     (declare (ignore by))
                     (%gard-clause->pipe
                      (%by-clause->pipe "upfrom" start step "below" end/step)
                      gard-clause)))))
          ("DOWNTO"
           (if (null xs)
               `(pipe-range ,start (1- ,end/step) -1)
               (if (string-not-equal "BY" (first xs))
                   (%gard-clause->pipe `(pipe-range ,start (1- ,end/step) -1) xs)
                   (destructuring-bind (by step . gard-clause) xs
                     (declare (ignore by))
                     (%gard-clause->pipe
                      (%by-clause->pipe "downfrom" start step "downto" end/step)
                      gard-clause)))))
          ("ABOVE"
           (if (null xs)
               `(pipe-range ,start ,end/step -1)
               (if (string-not-equal "BY" (first xs))
                   (%gard-clause->pipe `(pipe-range ,start ,end/step -1) xs)
                   (destructuring-bind (by step . gard-clause) xs
                     (declare (ignore by))
                     (%gard-clause->pipe
                      (%by-clause->pipe "upfrom" start step "above" end/step)
                      gard-clause)))))

          (t
           (%gard-clause->pipe (pipe-from start 1) rest-clause))))))


;; (var :upfrom start [{:to|:upto|:below} end] [:by step] {gard}*)
(defun %upfrom-clause->pipe (start rest-clause)
  (if (null rest-clause)
      `(pipe-from ,start)
      (destructuring-bind (keyword end/step . xs) rest-clause
        (switch (keyword :test #'string-equal)
          ("BY"
           (let ((base-pipe (%by-clause->pipe "upfrom" start end/step)))
             (if xs
                 (%gard-clause->pipe base-pipe xs)
                 base-pipe)))
          ("TO"
           (if (null xs)
               `(pipe-range ,start (1+ ,end/step))
               (if (string-not-equal "BY" (first xs))
                   (%gard-clause->pipe `(pipe-range ,start (1+ ,end/step)) xs)
                   (destructuring-bind (by step . gard-clause) xs
                     (declare (ignore by))
                     (%gard-clause->pipe
                      (%by-clause->pipe "upfrom" start step "upto" end/step)
                      gard-clause)))))
          ("UPTO"
           ;; following clause is same as "TO".
           (if (null xs)
               `(pipe-range ,start (1+ ,end/step))
               (if (string-not-equal "BY" (first xs))
                   (%gard-clause->pipe `(pipe-range ,start (1+ ,end/step)) xs)
                   (destructuring-bind (by step . gard-clause) xs
                     (declare (ignore by))
                     (%gard-clause->pipe
                      (%by-clause->pipe "upfrom" start step "upto" end/step)
                      gard-clause)))))
          ("BELOW"
           (if (null xs)
               `(pipe-range ,start ,end/step)
               (if (string-not-equal "BY" (first xs))
                   (%gard-clause->pipe `(pipe-range ,start ,end/step) xs)
                   (destructuring-bind (by step . gard-clause) xs
                     (declare (ignore by))
                     (%gard-clause->pipe
                      (%by-clause->pipe "upfrom" start step "below" end/step)
                      gard-clause)))))
          
          (t
           (%gard-clause->pipe (pipe-from start 1) rest-clause))))))


;; (var :downfrom start [{:downto|:to|:above} end] [:by step] {gard}*)
(defun %downfrom-clause->pipe (start rest-clause)
  (if (null rest-clause)
      `(pipe-from ,start -1)
      (destructuring-bind (keyword end/step . xs) rest-clause
        (switch (keyword :test #'string-equal)
          ("BY"
           (let ((base-pipe (%by-clause->pipe "downfrom" start end/step)))
             (if xs
                 (%gard-clause->pipe base-pipe xs)
                 base-pipe)))
          ("TO"
           (if (null xs)
               `(pipe-range ,start (1- ,end/step) -1)
               (if (string-not-equal "BY" (first xs))
                   (%gard-clause->pipe `(pipe-range ,start (1- ,end/step) -1) xs)
                   (destructuring-bind (by step . gard-clause) xs
                     (declare (ignore by))
                     (%gard-clause->pipe
                      (%by-clause->pipe "downfrom" start step "downto" end/step)
                      gard-clause)))))
          ("DOWNTO"
           (if (null xs)
               `(pipe-range ,start (1- ,end/step) -1)
               (if (string-not-equal "BY" (first xs))
                   (%gard-clause->pipe `(pipe-range ,start (1- ,end/step) -1) xs)
                   (destructuring-bind (by step . gard-clause) xs
                     (declare (ignore by))
                     (%gard-clause->pipe
                      (%by-clause->pipe "downfrom" start step "downto" end/step)
                      gard-clause)))))
          ("ABOVE"
           (if (null xs)
               `(pipe-range ,start ,end/step -1)
               (if (string-not-equal "BY" (first xs))
                   (%gard-clause->pipe `(pipe-range ,start ,end/step -1) xs)
                   (destructuring-bind (by step . gard-clause) xs
                     (declare (ignore by))
                     (%gard-clause->pipe
                      (%by-clause->pipe "downfrom" start step "above" end/step)
                      gard-clause)))))
          
          (t
           (%gard-clause->pipe (pipe-from start -1) rest-clause))))))


(defun %arithmetic-clause->pipe (keyword x xs)
  (switch (keyword :test #'string-equal)
    ;; gard ::= {:when|:unless|:while|:until} pred
    
    ;; (var :from start [{:upto|:downto|:to|:below|:above} end] [:by step] {gard}*)
    ("FROM" (%from-clause->pipe x xs))
       
    ;; (var :upfrom start [{:to|:upto|:below} end] [:by step] {gard}*)
    ("UPFROM" (%upfrom-clause->pipe x xs))
       
    ;; (var :downfrom start [{:downto|:to|:above} end] [:by step] {gard}*)
    ("DOWNFROM" (%downfrom-clause->pipe x xs))

    ;; the default value of `from' is 0.
    ;; (var :upto end [:by step] {gard}*)
    ("UPTO" (%upfrom-clause->pipe 0 (list* 'upto x xs)))
    
    ;; (var :downto end [:by step] {gard}*)
    ("DOWNTO" (%downfrom-clause->pipe 0 (list* 'downto x xs)))

    ;; (var :to end [:by step] {gard}*)
    ("TO" (%from-clause->pipe 0 (list* 'to x xs)))

    ;; (var :below end [:by step] {gard}*)
    ("BELOW" (%upfrom-clause->pipe 0 (list* 'below x xs)))
    
    ;; (var :above end [:by step] {gard}*)
    ("ABOVE" (%downfrom-clause->pipe 0 (list* 'above x xs)))
    
    ;; (var :by step {gard}*)
    ("BY" (let ((base-pipe (%by-clause->pipe "upfrom" 0 x)))
            (if xs
                (%gard-clause->pipe base-pipe xs)
                base-pipe)))
    
    ;; (var {gard}+)
    (t (%gard-clause->pipe (pipe-from 0 1)
                           (list* keyword x xs)))))

(defun %clause->entry (clause)
  (destructuring-bind (var . rest-clause) clause
    (if (null rest-clause)
        ;; (var)
        (cons var `(pipe-from 0 1))
        (destructuring-bind (keyword x . xs) rest-clause
          (cons
           var
           (switch (keyword :test #'string-equal)
             ;; gard ::= {:when|:unless|:while|:until} pred
             ;; (var :in seq {gard}*)
             ("IN" (%in-clause->pipe x xs))
             
             ;; (var :cycle seq {gard}*)
             ("CYCLE"
              (let ((base-pipe `(apply #'pipe-constant (coerce ,x 'list))))
                (if xs
                    (%gard-clause->pipe base-pipe xs)
                    base-pipe)))
             
             ;; (var :replicate obj+)
             ("REPLICATE" `(apply #'pipe-constant ,x ',xs))

             ;; (var :repeat num obj {gard}*)
             ("REPEAT"
              (destructuring-bind (obj . gard-clause) xs
                (let ((base-pipe `(pipe-take ,x (pipe-constant ,obj))))
                  (if gard-clause
                      (%gard-clause->pipe base-pipe gard-clause)
                      base-pipe))))
             
             ;; (var [{:upfrom|:from|:downfrom} start]
             ;;      [{:upto|:downto|:to|:below|:above} end]
             ;;      [:by step]
             ;;      {gard}*)
             ;; NB. (var) exclusive.
             (t (%arithmetic-clause->pipe keyword x xs))))))))


;; TODO:
;; -----
;;  ? RENAME:
;;     :when   -> satisfies
;;     :unless -> unsatisfies
;;  ? ADD: Destructuring.
;;  ? ADD: is-clause, are-clause
;;  * OPTIMIZATION:
;;    (lfor a :in '(1 2 3) :when #'oddp :unless #'minusp)
;;    -> XXX
;;    (pipe-filter (complement #'minusp)
;;                 (pipe-filter #'oddp
;;                              (list->pipe '(1 2 3))))
;;    -> OOO
;;    (pipe-filter (lambda (x) (and (not (minusp x)) (addp x))
;;                 (list->pipe '(1 2 3)))
(defmacro lazy-for (expression &body clauses)
  (cond ((null clauses)
         `(lazy-seq (pipe-from 0 1)))
        ((symbolp (first clauses))
         `(lazy-seq ,(cdr (%clause->entry (cons expression clauses)))))
        (t
         (let ((clause-alist (mapcar #'%clause->entry clauses)))
           (typecase expression
             (symbol `(lazy-seq ,(cdr (assoc expression clause-alist))))
             (t      `(lazy-seq (pipe-map (lambda ,(mapcar #'car clause-alist)
                                            ,expression)
                                          ,@(mapcar #'cdr clause-alist)))))))))

(setf (documentation 'lazy-for 'function) "
LAZY-FOR/LFOR : Lazy-Sequence Facility
======================================

 LAZY-FOR expression &body clauses => lazy-sequence
 LFOR     expression &body clauses => lazy-sequence

LAZY-FOR is a macro for generating lazy-sequence.
LFOR is an alias for LAZY-FOR.

Examples
--------
 * (lfor x by 5 unless ^(zerop (mod @ 3)))
   -> 5 10 20 25 35 40 50 55 65 70 ...

 * (lfor x from 2 below 42 by 3)
   -> 2 5 8 11 14 17 20 23 26 29 32 35 38 41

 * (lfor x from 2 by ^(* @ @))
   -> 2 4 16 256 65536 4294967296 ...

 * (lfor (cons x y)
     (x in #[1 ..] when 'oddp)
     (y replicate :foo :bar :baz))
   -> (1 . :FOO) (3 . :BAR) (5 . :BAZ) (7 . :FOO) (9 . :BAR) ...

 * (lfor (cons x y)
     (x in #[1 ..] when #'oddp)
     (y repeat 3 :foo))
   -> (1 . :FOO) (3 . :foo) (5 . :foo)

 * (lfor (string x) (x in #[#\a .. #\k]))
   -> \"a\" \"b\" \"c\" \"d\" \"e\" \"f\" \"g\" \"h\" \"i\" \"j\" \"k\"

Grammar
-------
 (<for> <expression> <clause>+)
 (<for> . <clause>)

 <for> ::= lazy-for | lfor
 <clause>  ::= (<var> in {<sequens>|<pipe>} <gard>*)
            |  (<var> [{from|upfrom} <start>] [{upto|to|below} <end>] [by <step>] <gard>*)
            |  (<var> [from <start>] [{downto|above} <end>] [by <step>] <gard>*)
            |  (<var> [downfrom <start>] [{downto|to|above} <end>] [by <step>] <gard>*)
            |  (<var> cycle <sequence> <gard>*)
            |  (<var> replicate <object>+)
            |  (<var> repeat <count> <object> <gard>*)
 <var> ::= <symbol>
 <conditional> ::= {when|unless} <predicate>
 <termination> ::= {while|until} <predicate>
 <gard> ::= <conditional> | <termination>
 <start>, <end>, <count> ::= <integer>
 <step> ::= <integer> | <function-designator>
 <function-designator> ::= <function> | <symbol>

Notes
-----
 * If the expression is a symbol, there exists shorthand form.
   e.g., (lfor x (x :by 5)) <-> (lfor x :by 5)
 * The default `start' is 0. The default `step' is 1.
   e.g., (lfor x :when 'oddp) <-> (lfor x :from 0 :by 1 :when 'oddp)
 * in-clause accepts not only list, but also vector, lazy-sequence or pipe.
 * in-clause does not accept keyword `by'.
 * from-clause, upfrom-clause and downfrom-clause accept the keywrod `by' which is either
   function-designators or numbers.

Tips
----
 * CONDITONAL? or TERMINATION?

    (take 4 (lfor x :by 2 :when ^(< @ 8))) => (0 2 4 6)
    (take 5 (lfor x :by 2 :when ^(< @ 8))) => INFINITE LOOP!

   The above second form means that:

    1. make infinite lazy-sequence which starts from 0 with step 2,
    2. extract elements that satisfy predicate ^(< @ 8) from the infinite sequence,
    3. force and take 5 elements from it.

   However, because there are only 4 elements that satisfy the predicate, it forever
   continues to force the initinite sequence until mythical 5th element is found
   (i.e., it enters the infinite loop).
   We should use TERMINATION instead CONDITIONAL above case.
   For example:

    (take 5 (lfor x :by 2 :while ^(< @ 8))) => (0 2 4 6)
    (take 5 (lfor x :below 8 :by 2))        => (0 2 4 6)

   These works fine, because these make a finite lazy-sequence.

   cf. Folowing examples also never stop.
       CL+USER> (collect-if* ^(< @ 8) #[0 2 ..] :count 5)
       CL+USER> (take 5 (remove-if-not* ^(< @ 8) #[0 2 ..]))
       clojure> (take 5 (filter #(< % 8) (iterate #(+ % 2) 0)))
       haskell> take 5 $ filter (\x -> x < 8) [0,2..]

   p.s. (take 5 #[0 2 .. 7]) => (0 2 4 6)
")

(defmacro lfor (expression &body clauses)
  `(lazy-for ,expression ,@clauses))

(setf (documentation 'lfor     'function)
      (documentation 'lazy-for 'function))


;; MEMO: 2014-05-11
;; Should we introduce the following macro `for'?
;;
;; (defmacro for (expression &body clauses)
;;   `(lazy-take :all (lazy-for ,expression ,@clauses)))
;;
;; -> Currently, NO.
;;  1. It conflicts with iterate:for.
;;  2. It easily enters the infinite loop.
;;  3. If it is really necessity, the user is able to define it, easily.


;;------------------------------------------------
;; Utilities
;;------------------------------------------------

(defun lazy-take (n lseq)
  (check-type n    (or (integer 0 *) (eql :all)))
  (check-type lseq lazy-sequence)
  (if (eq n :all)
      (let ((pipe-nil (force +empty-pipe+)))
        (nth-value 0 (lazy-take-until (lambda (x) (eq x pipe-nil))
                                      lseq)))
      (if (<= n (%lseq-length lseq))
          (loop :for i :from 0 :below n ; UGLY! every time exe when called
                :for e :across (%lseq-accessed lseq)
                :collect (if (lazyp e)
                             (setf (aref (%lseq-accessed lseq) i) (force e))
                             e))
          (progn
            (loop :for i :from 0
                  :for e :across (%lseq-accessed lseq)
                  :when (lazyp e)
                    :do (setf (aref (%lseq-accessed lseq) i) (force e)))
            (dotimes (_ (- n (%lseq-length lseq)))
              (realize lseq))
            (coerce (%lseq-accessed lseq) 'list)))))

(setf (documentation 'lazy-take 'function) "
LAZY-TAKE n lazy-sequence => result-list

Notes
-----
  * If `n' is `:all' and `lazy-sequence' is finite, then it returns
    all realized list.
  * If `n' is `:all' and `lazy-sequence' is infinite, then it enters
    the infinit loop.
")

(defun lazy-take-until (pred lseq)
  (check-type pred (or symbol function))
  (check-type lseq lazy-sequence)
  (multiple-value-bind (result found?)
      (loop :for e :across (%lseq-accessed lseq)
            :for v := (force e)
            :when (funcall pred v)
              :return (values acc t)
            :collect v :into acc
            :finally (return (values acc nil)))
    (if found?
        (values result t)
        (loop :for (v1 more?) := (multiple-value-list (realize lseq))
              :while more?
              :when (funcall pred v1)
                :return (values (append result acc1) t)
              :collect v1 :into acc1
              :finally (return (values (append result acc1)
                                       nil))))))

(defun lazy-take-while (pred lseq)
  (check-type pred (or symbol function))
  (check-type lseq lazy-sequence)
  (lazy-take-until (complement pred) lseq))

(defun lazy-drop (n lseq)
  (check-type n    (integer 0 *))
  (check-type lseq lazy-sequence)
  (let ((new-prophecy (copy-promise (%lseq-prophecy lseq))))
    (if (< n (%lseq-length lseq))
        (let ((new-accessed (subseq (%lseq-accessed lseq) n)))
          (make-lazy-seq new-accessed new-prophecy))
        (lazy-seq (pipe-drop (- n (%lseq-length lseq))
                             new-prophecy)))))

(setf (documentation 'lazy-drop 'function) "
LAZY-DROP n lazy-sequence => result
")

(defun lazy-drop-while (pred lseq)
  (check-type pred (or symbol function))
  (check-type lseq lazy-sequence)
  (let ((new-prophecy (copy-promise (%lseq-prophecy lseq))))
    (multiple-value-bind (new-accessed end?)
     (loop :for i :from 0
           :for e :across (%lseq-accessed lseq)
           :unless (funcall pred (force e))
             :return (values (subseq (%lseq-accessed lseq) i) t)
           :finally (return (values '() nil)))
      (make-lazy-seq new-accessed (if end?
                                      new-prophecy
                                      (pipe-drop-while pred new-prophecy))))))

(defun lazy-drop-until (pred lseq)
  (lazy-drop-while (complement pred) lseq))

;; TODO: remove lseq->pipe
(defun lazy-remove (item lseq &key (test 'eql) key count (start 0) end)
  (check-type lseq  lazy-sequence)
  (check-type test  (or symbol function))
  (check-type key   (or symbol function))
  (check-type count (or null (integer 0 *)))
  (check-type start (integer 0 *))
  (check-type end   (or null (integer 0 *)))
  (setf key (or key #'identity))
  (let ((not-test (complement (ensure-function test)))
        (index -1))
    (if count
        (if end
            (lazy-seq
             (pipe-filter (lambda (x)
                            (incf index)
                            (cond ((<= count 0)    T)
                                  ((< index start) T)
                                  ((<= end index)  T)
                                  ((funcall not-test item (funcall key x)) T)
                                  (t (decf count) nil)))
                          (lseq->pipe lseq)))
            (lazy-seq
             (pipe-filter (lambda (x)
                            (incf index)
                            (cond ((<= count 0)    T)
                                  ((< index start) T)
                                  ((funcall not-test item (funcall key x)) T)
                                  (t (decf count) nil)))
                          (lseq->pipe lseq))))
        (if end
            (lazy-seq
             (pipe-filter (lambda (x)
                            (incf index)
                            (cond ((< index start) T)
                                  ((<= end index)  T)
                                  (t (funcall not-test item (funcall key x)))))
                          (lseq->pipe lseq)))
            (lazy-seq
             (pipe-filter (lambda (x)
                            (incf index)
                            (cond ((< index start) T)
                                  (t (funcall not-test item (funcall key x)))))
                          (lseq->pipe lseq)))))))

;; TODO: remove lseq->pipe
(defun lazy-remove-if (pred lseq &key key count (start 0) end)
  (check-type lseq  lazy-sequence)
  (check-type pred  (or symbol function))
  (check-type key   (or symbol function))
  (check-type count (or null (integer 0 *)))
  (check-type start (integer 0 *))
  (check-type end   (or null (integer 0 *)))
  (setf key (or key #'identity))
  (let ((not-pred (complement (ensure-function pred)))
        (index -1))
    (if count
        (if end
            (lazy-seq
             (pipe-filter (lambda (x)
                            (incf index)
                            (cond ((<= count 0)    T)
                                  ((< index start) T)
                                  ((<= end index)  T)
                                  ((funcall not-pred (funcall key x)) T)
                                  (t (decf count) nil)))
                          (lseq->pipe lseq)))
            (lazy-seq
             (pipe-filter (lambda (x)
                            (incf index)
                            (cond ((<= count 0)    T)
                                  ((< index start) T)
                                  ((funcall not-pred (funcall key x)) T)
                                  (t (decf count) nil)))
                          (lseq->pipe lseq))))
        (if end
            (lazy-seq
             (pipe-filter (lambda (x)
                            (incf index)
                            (cond ((< index start) T)
                                  ((<= end index)  T)
                                  (t (funcall not-pred (funcall key x)))))
                          (lseq->pipe lseq)))
            (lazy-seq
             (pipe-filter (lambda (x)
                            (incf index)
                            (cond ((< index start) T)
                                  (t (funcall not-pred (funcall key x)))))
                          (lseq->pipe lseq)))))))

(defun lazy-map (fn lseq &rest more-lseqs)
  (check-type fn   (or symbol function))
  (check-type lseq lazy-sequence)
  (when (some (complement #'lazy-sequence-p) more-lseqs)
    (error "There is at least one non-pipe object in MORE-LSEQS."))
  (lazy-seq (apply #'pipe-map fn
                   (mapcar #'lseq->pipe
                           (cons lseq more-lseqs)))))

(defun lazy-reduce (fn lseq &optional initial-value)
  (check-type fn   (or symbol function))
  (check-type lseq lazy-sequence)
  (reduce fn (lazy-take :all lseq) :initial-value initial-value))


#+nil
(defun pipe-scan (function base pipe)
  (labels ((%pipe-scan (base pipe)
             (lazy
              (if (pipe-null pipe)
                  (pipe base)
                  (pipe-cons base
                             (%pipe-scan (funcall function
                                                  base (pipe-car pipe))
                                         (pipe-cdr pipe)))))))
    (%pipe-scan base pipe)))

#+nil
(defun lazy-scan (fn lseq &optional key initial-value start end)
  (check-type fn   (or symbol function))
  (check-type lseq lazy-sequence)
  (reduce fn (lazy-take :all lseq) :initial-value initial-value))


(defun lazy-zip (lseq1 lseq2 &rest more-lseqs)
  (check-type lseq1 lazy-sequence)
  (check-type lseq2 lazy-sequence)
  (when (some (complement #'lazy-sequence-p) more-lseqs)
    (error "There is at least one non-lazy-sequence object in MORE-LSEQS."))
  (lazy-seq (apply #'pipe-zip
                   (mapcar #'lseq->pipe
                           (list* lseq1 lseq2 more-lseqs)))))

(defun lazy-interleave (lseq1 lseq2 &rest more-lseqs)
  (check-type lseq1 lazy-sequence)
  (check-type lseq2 lazy-sequence)
  (when (some (complement #'lazy-sequence-p) more-lseqs)
    (error "There is at least one non-lazy-sequence object in MORE-LSEQS."))
  (labels ((%lazy-interleave (pipes)
             (lazy
              (if (some #'pipe-null pipes)
                  +empty-pipe+
                  (pipe-append (list->pipe (mapcar #'pipe-car pipes))
                               (%lazy-interleave (mapcar #'pipe-cdr pipes)))))))
    (lazy-seq (%lazy-interleave (mapcar #'lseq->pipe
                                        (list* lseq1 lseq2 more-lseqs))))))

(defun lazy-interpose (separator lseq)
  (check-type lseq lazy-sequence)
  (lazy-drop 1 (lazy-interleave (replicate separator) lseq)))


;;--------------------------------------------------------------------
;; Lazy-Flow (((EXPERIMANTAL)))
;;--------------------------------------------------------------------

(defvar *lazy-flow-origin* (gensym "LAZY-FLOW-ORIGIN-"))

(defstruct (lazy-flow (:conc-name %lflow-)
                      (:constructor %make-lflow)
                      (:copier nil)
                      (:predicate lazy-flow-p)
                      (:print-function
                       (lambda (obj stream depth)
                         (declare (ignore depth))
                         (if (eq *lazy-flow-origin*
                                 (%lflow-current obj))
                             (format stream "[~~]")
                             (format stream "[~S]~~" (%lflow-current obj))))))
  current
  (prophecy nil :type pipe))

;; cf., http://www.aozora.gr.jp/cards/000196/card975.html  
(setf (documentation 'lazy-flow 'type) "
行く川のながれは絶えずして、しかも本の水にあらず。
よどみに浮ぶうたかたは、かつ消えかつ結びて久しくとゞまることなし。
                            -- 鴨長明、方丈記")

(setf (documentation 'lazy-flow 'structure) "
           (((EXPERIMANTAL)))

LAZY-FLOW
=========

A lazy-flow is a data structure consisting of two parts called the
`current' and the `prophecy'.  `current' is a object that is the just
previous value which `prophecy' was forecd to.  Roughly speaking,
`LAZY-FLOW' is a lazy-sequence that stores the only one value.

Examples (from srfi-41)
-----------------------

 * (defun make-random-flow (seed)
     (check-type seed (integer 1 #.(1- (expt 2 32))))
     (induce-flow ^(mod (* @ 16807) 2147483647) seed))

 * (defvar *golden-ratio* (induce-flow ^(1+ (/ @)) 1))

 * (defvar *positive-rational-numbers*
     (induce ^x(let* ((n (floor x))
                      (y (- x n)))
                 (/ (- n -1 y)))
             1))
")


(defun current-flow (lflow)
  (check-type lflow lazy-flow)
  (%lflow-current lflow))

(defun make-lazy-flow (prophecy)
  (%make-lflow :current *lazy-flow-origin* :prophecy prophecy))

(defun lseq->lflow (lseq)
  (check-type lseq lazy-sequence)
  (make-lazy-flow (copy-promise (%lseq-prophecy lseq))))

(defun lflow->lseq (lflow)
  (check-type lflow lazy-flow)
  (make-lazy-seq (list (%lflow-current lflow))
                 (copy-promise (%lflow-prophecy lflow))))

(defun flush (lflow &optional (count 1))
  (check-type lflow lazy-flow)
  (check-type count (integer 1 *))
  (labels ((rec (n prev pipe)
             (if (= 1 n)
                 (values (setf (%lflow-prophecy lflow) pipe
                               (%lflow-current lflow)  (force prev))
                         t)
                 (if (pipe-null pipe)
                     (values (setf (%lflow-prophecy lflow) pipe
                                   (%lflow-current lflow)  (force prev))
                             nil)
                     (rec (1- n)
                          (cl-plus.src.srfi.srfi-41::kar (force pipe))
                          (pipe-cdr pipe))))))
    (let ((prophecy (%lflow-prophecy lflow)))
      (if (pipe-null prophecy)
          (values (%lflow-current lflow) nil)
          (let ((kons (force prophecy)))
            (rec count
                 (cl-plus.src.srfi.srfi-41::kar kons)
                 (cl-plus.src.srfi.srfi-41::kdr kons)))))))

;; (defun repeat-flow (count object)
;;   (check-type count (integer 0 *)))

(defun replicate-flow (object)
  (labels ((%repeater ()
             (lazy (pipe-cons object (%repeater)))))
    (%make-lflow :current object :prophecy (%repeater))))

(defun %repeatedly-flow (successor-function)
  (labels ((%%repeater ()
             (lazy (pipe-cons (funcall successor-function) (%%repeater)))))
    (make-lazy-flow (%%repeater))))

(defun cycle-flow (sequence)
  (check-type sequence sequence)
  (let ((len (length sequence))
        (vec (coerce sequence 'simple-vector)))
    (declare (type (integer 0 *) len))
    (declare (type simple-vector vec))
    (when (zerop len)
      (error "The length of SEQUENCE must be at least 1."))
    (%repeatedly-flow (let ((i -1))
                        (declare (type (integer -1 *) i))
                        (lambda ()
                          (incf i)
                          (svref vec (mod i len)))))))

(setf (documentation 'cycle-flow 'function) "
CYCLE-FLOW sequence => lazy-flow
")

(defun %iterate-flow (successor-function seed)
  (labels ((%%iterater (prev)
             (lazy
              (let ((next (funcall successor-function prev)))
                (pipe-cons next (%%iterater next))))))
    (%make-lflow :current seed :prophecy (%%iterater seed))))

(defun %induce-flow (successor-function &rest seeds)
  (labels ((%%inducer (prevs)
             (lazy
              (let ((next (apply successor-function prevs)))
                (pipe-cons next (%%inducer (append1 (rest prevs) next)))))))
    (%make-lflow :current (last1 seeds) :prophecy (%%inducer seeds))))

(defun induce-flow (successor-function &rest seeds)
  (check-type successor-function (or symbol function))
  (case (length seeds)
    (0 (%repeatedly-flow successor-function))
    (1 (%iterate-flow successor-function (first seeds)))
    (t (apply #'%induce-flow successor-function seeds))))

(setf (documentation 'induce-flow 'function) "
INDUCE-FLOW successor-function &rest seeds => lazy-flow

Examples
--------

 * (defvar *golden-ratio* (induce-flow ^(1+ (/ @)) 1))

   *golden-ratio* => [1]

   (flush *golden-ratio*)    => 2, T
   (flush *golden-ratio* 30) => 3524578/2178309, T

   (float (current-flow *golden-ratio*)) => 1.618034

 * (defun make-random-flow (seed)
     (check-type seed (integer 1 (#.(expt 2 32))))
     (induce-flow ^(mod (* @ 16807) 2147483647) seed))

   (defvar *rf* (make-random-flow 42))

   (flush *rf*) => 705894, T
   (flush *rf*) => 1126542223, T
   (flush *rf*) => 1579310009, T
")


;;====================================================================
