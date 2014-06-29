;;;; Last modified: 2014-06-29 10:48:26 tkych

;; cl-plus/src/core/sequence.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;; TODO:
;;  * compiler-macro

;;====================================================================
;; Sequence
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.core.sequence
  (:documentation "
Sequence
========

Functions
---------

 - sequencep
 - repeat
 - permutations
 - combinations
 - inits
 - tails
 - scan
 - unreduce
 - split
 - split-if
 - split-if-not
 - split-at

")
  (:export #:sequencep
           #:repeat
           #:permutations
           #:combinations
           #:inits
           #:tails
           #:scan
           #:unreduce
           #:split  #:split-if  #:split-if-not  #:split-at)
  (:nicknames #:cl+sequence)
  (:use #:cl)
  (:import-from #:split-sequence
                #:split-sequence
                #:split-sequence-if
                #:split-sequence-if-not))

(in-package #:cl-plus.src.core.sequence)


;;--------------------------------------------------------------------
;; sequencep
;;--------------------------------------------------------------------

(declaim (inline sequencep))
(defun sequencep (x) (typep x 'sequence))


;;--------------------------------------------------------------------
;; split, split-if, split-if-not, split-at
;;--------------------------------------------------------------------
;; split        <- split-sequence
;; split-if     <- split-sequence-if
;; split-if-not <- split-sequence-if-not

(setf (fdefinition 'split)
      (fdefinition 'split-sequence)
      
      (fdefinition 'split-if)
      (fdefinition 'split-sequence-if)
      
      (fdefinition 'split-if-not)
      (fdefinition 'split-sequence-if-not))


(defun split-at (index sequence)
  (etypecase sequence
    (list
     (cond ((<= index 0)
            (list '()
                  (copy-list sequence)))
           ((< index (length sequence))
            (list (subseq sequence 0 index)
                  (subseq sequence index)))
           (t
            (list (copy-list sequence)
                  '()))))
    (vector
     (cond ((<= index 0)
            (list (subseq sequence 0 0)
                  (subseq sequence 0)))
           ((< index (length sequence))
            (list (subseq sequence 0 index)
                  (subseq sequence index)))
           (t
            (list (subseq sequence 0)
                  (subseq sequence 0 0)))))))


(setf (documentation 'split-at 'function) "
SPLIT-AT index sequence => list-of-subsequces
")


;;--------------------------------------------------------------------
;; repeat
;;--------------------------------------------------------------------

(defun repeat (count object &optional (result-type 'list))
  (check-type count (integer 0 *))
  (case result-type
    ((list sequence)
     (make-list count :initial-element object))
    
    ((vector)
     (make-array count :initial-element object))
    
    ((string)
     (make-string count :initial-element object))

    (t
     (make-sequence result-type count :initial-element object))))

(setf (documentation 'repeat 'function) "
REPEAT count object &optional (result-type 'list) => result-sequence
")


;;--------------------------------------------------------------------
;; permutations
;;--------------------------------------------------------------------
;; cf. alexandria:MAP-PERMUTATIONS

;; TODO:
;;  LEXICAL ORDER:
;;  (permutations '(0 1 2)) => ((2 1 0) (1 2 0) (0 2 1) (2 0 1) (1 0 2) (0 1 2))

(defun permutations (sequence &key (start 0) end)
  (check-type sequence sequence)
  (check-type start    (integer 0 *))
  (check-type end      (or null (integer 0 *)))
  (when (and end (< end start))
    (error "[~S , ~S) is bad interval." start end))
  (let ((length (- (or end (length sequence)) start))
        (result '()))
    (labels ((permute (seq len)
               (let ((len-1 (1- len)))
                 (if (zerop len-1)
                     (push (copy-seq seq) result)
                     (loop :for i :from 0 :to len-1
                           :do (permute seq len-1)
                               (if (evenp len-1)
                                   (rotatef (elt seq 0) (elt seq len-1))
                                   (rotatef (elt seq i) (elt seq len-1))))))))
      (permute (subseq sequence start end)
               length)
      result)))


(setf (documentation 'permutations 'function) "
PERMUTATIONS sequence &key (start 0) end => list-of-permutations
cf. alexandria:MAP-PERMUTATIONS
")


;;--------------------------------------------------------------------
;; combinations
;;--------------------------------------------------------------------
;; cf. alexandria:MAP-COMBINATIONS


;; (map-combinations 'print '(0 1 2) :length 2 :start 1)
;; prints
;; ; (0 1)
;;
;; However, it should print
;; ; (1 2)
;;
;; MODIFIED: combination -> (subseq sequence start end)


#+#:comment "
Calls FUNCTION with each combination of LENGTH constructable from the
elements of the subsequence of SEQUENCE delimited by START and END. START
defaults to 0, END to length of SEQUENCE, and LENGTH to the length of the
delimited subsequence. (So unless LENGTH is specified there is only a single
combination, which has the same elements as the delimited subsequence.)
"

;; TODO
;; ----
;; * BE CONSISTANT ORDER:
;;   (combinations 2 '(0 1 2)) => ((1 2) (0 2) (0 1))
;;   (combinations 2 #(0 1 2)) => (#(2 1) #(2 0) #(1 0))
;;
;; * RENAME: count -> ?n, number, take?

(defun combinations (count sequence &key (start 0) end)
  (check-type count (integer 0 *))
  (check-type start (integer 0 *))
  (check-type end   (or null (integer 0 *)))
  (when (and end (< end start))
    (error "[~S , ~S) is bad interval." start end))

  (let* ((end    (or end (length sequence)))
         (len    (- end start))
         (seq    (subseq sequence 0 count))
         (result '()))
    (if (= count len)
        ;; modified: seq -> (subseq sequence start end)
        (push (subseq sequence start end) result)
        (etypecase sequence
          (list
           (labels ((rec (c-tail o-tail)
                      (if (endp c-tail)
                          (push (copy-list seq) result)
                          (loop :for (x . xs) :on o-tail
                                :do (setf (car c-tail) x)
                                    (rec (cdr c-tail) xs)))))
             (rec seq (nthcdr start sequence))))

          (vector
           (labels ((rec (count start)
                      (if (zerop count)
                          (push (copy-seq seq) result)
                          (loop :for i :from start :below end
                                :do (let ((j (1- count)))
                                      (setf (aref seq j) (aref sequence i))
                                      (rec j (1+ i)))))))
             (rec count start)))))
    result))


(setf (documentation 'combinations 'function) "
COMBINATIONS count sequence &key (start 0) end => list-of-combinations
cf. alexandria:MAP-COMBINATIONS
")


;;--------------------------------------------------------------------
;; inits
;;--------------------------------------------------------------------

(defun inits (sequence &key key count (start 0) end remove-empty-subseqs)

  (check-type key   (or symbol function))
  (check-type count (or null (integer 0 *)))
  (check-type start (integer 0 *))
  (check-type end   (or null (integer 0 *)))
  (when (and end (< end start))
    (error "[~S , ~S) is bad interval." start end))

  (etypecase sequence
    ;; UGLY, butlast.
    (list
     (if count
         (if key
             (loop :with lst := (mapcar key (subseq sequence start end))
                   :for i fixnum :downfrom (if remove-empty-subseqs
                                               (1- (length lst))
                                               (length lst)) :to 0
                   :until (zerop count) 
                   :collect (butlast lst i)
                   :do (decf count))
             (loop :with lst := (subseq sequence start end)
                   :for i fixnum :downfrom (if remove-empty-subseqs
                                               (1- (length lst))
                                               (length lst)) :to 0
                   :until (zerop count)
                   :collect (butlast lst i)
                   :do (decf count)))
         (if key
             (loop :with lst := (mapcar key (subseq sequence start end))
                   :for i fixnum :downfrom (if remove-empty-subseqs
                                               (1- (length lst))
                                               (length lst)) :to 0
                   :collect (butlast lst i))
             (loop :with lst := (subseq sequence start end)
                   :for i fixnum :downfrom (if remove-empty-subseqs
                                               (1- (length lst))
                                               (length lst)) :to 0
                   :collect (butlast lst i)))))

    (vector
     (if count
         (if key
             (let* ((subseq (subseq sequence start end))
                    (seq    (map (type-of subseq) key subseq)))
               (loop :for i fixnum :from (if remove-empty-subseqs 1 0)
                       :to (or (and end (- end start)) (length seq))
                     :until (zerop count)
                     :collect (subseq seq 0 i)
                     :do (decf count)))
             (loop :for i fixnum :from (if remove-empty-subseqs (1+ start) start)
                     :to (or end (length sequence))
                   :until (zerop count)
                   :collect (subseq sequence start i)
                   :do (decf count)))
         (if key
             (let* ((subseq (subseq sequence start end))
                    (seq    (map (type-of subseq) key subseq)))
               (loop :for i fixnum :from (if remove-empty-subseqs 1 0)
                       :to (or (and end (- end start)) (length seq))
                     :collect (subseq seq 0 i)))
             (loop :for i fixnum :from (if remove-empty-subseqs (1+ start) start)
                     :to (or end (length sequence))
                   :collect (subseq sequence start i)))))))


(setf (documentation 'inits 'function) "
INITS sequence &key key count (start 0) end remove-empty-subseqs => result-list

")


;;--------------------------------------------------------------------
;; tails
;;--------------------------------------------------------------------

(defun tails (sequence &key key count (start 0) end remove-empty-subseqs)

  (check-type key   (or symbol function))
  (check-type count (or null (integer 0 *)))
  (check-type start (integer 0 *))
  (check-type end   (or null (integer 0 *)))
  (when (and end (< end start))
    (error "[~S , ~S) is bad interval." start end))

  (etypecase sequence
    (list
     (let ((result '()))
       (if count
           (if key
               (loop :for lst :on (mapcar key (subseq sequence start end))
                     :until (zerop count)
                     :do (push lst result)
                         (decf count))
               (loop :for lst :on (subseq sequence start end)
                     :until (zerop count)
                     :do (push lst result)
                         (decf count)))
           (if key
               (loop :for lst :on (mapcar key (subseq sequence start end))
                     :do (push lst result))
               (loop :for lst :on (subseq sequence start end)
                     :do (push lst result))))
       (if (or remove-empty-subseqs
               (and count (zerop count)))
           (nreverse result)
           (nreverse (push '() result)))))

    (vector
     (if count
         (if key
             (let* ((subseq (subseq sequence start end))
                    (seq    (map (type-of subseq) key subseq))
                    (len    (or (and end (- end start))
                                (length subseq))))
               (loop :for i :from 0 :to (if remove-empty-subseqs (1- len) len)
                     :until (zerop count)
                     :collect (subseq seq i len)
                     :do (decf count)))
             (let ((len (or end (length sequence))))
               (loop :for i :from start :to (if remove-empty-subseqs (1- len) len)
                     :until (zerop count)
                     :collect (subseq sequence i end)
                     :do (decf count))))
         (if key
             (let* ((subseq (subseq sequence start end))
                    (seq    (map (type-of subseq) key subseq))
                    (len    (or (and end (- end start))
                                (length subseq))))
               (loop :for i :from 0 :to (if remove-empty-subseqs (1- len) len)
                     :collect (subseq seq i len)))
             (let ((len (or end (length sequence))))
               (loop :for i :from start :to (if remove-empty-subseqs (1- len) len)
                     :collect (subseq sequence i end))))))))


(setf (documentation 'tails 'function) "
TAILS sequence &key key count (start 0) end remove-empty-subseqs => result-list
")


;;--------------------------------------------------------------------
;; scan
;;--------------------------------------------------------------------

(defun scan (function sequence &key key (initial-value nil ivp) (start 0) end from-end)

  (check-type function (or symbol function))
  (check-type sequence sequence)
  (check-type key      (or symbol function))
  (check-type start    (integer 0 *))
  (check-type end      (or null (integer 0 *)))
  (when (and end (< end start))
    (error "[~S , ~S) is bad interval." start end))

  ;; TODO:
  ;; Don't coerce vector into list.
  (setf sequence (coerce sequence 'list))

  (cond (;; cf. CLHS, function REDUCE.
         ;; > If the subsequence is empty and no initial-value is
         ;; > given, then the function is called with zero arguments,
         ;; > and reduce returns whatever function does.
         (null sequence)
         (if ivp
             (list initial-value)
             (list (funcall function))))
        
        (;; cf. CLHS, function REDUCE.
         ;; > If the subsequence contains exactly one element and no
         ;; > initial-value is given, then that element is returned
         ;; > and function is not called.
         (and (null (cdr sequence))
              (not ivp))
         sequence)
        
        (t
         (if from-end
             ;; scan right:
             (if ivp
                 (if key
                     (loop :with result := '()
                           :for prev := initial-value :then (funcall function (funcall key curr) prev)
                           :for curr :in (nreverse (subseq sequence start end))
                           :do (push prev result)
                           :finally (return (push prev result)))
                     (loop :with result := '()
                           :for prev := initial-value :then (funcall function curr prev)
                           :for curr :in (nreverse (subseq sequence start end))
                           :do (push prev result)
                           :finally (return (push prev result))))
                 (if key
                     (let ((lst (nreverse (subseq sequence start end))))
                       (loop :with result := '()
                             :for prev := (funcall key (first lst)) :then (funcall function (funcall key curr) prev)
                             :for curr :in (rest lst)
                             :do (push prev result)
                             :finally (return (push prev result))))
                     (let ((lst (nreverse (subseq sequence start end))))
                       (loop :with result := '()
                             :for prev := (first lst) :then (funcall function curr prev)
                             :for curr :in (rest lst)
                             :do (push prev result)
                             :finally (return (push prev result))))))         
             ;; scan left:
             (if ivp
                 (if key
                     (loop :with result := '()
                           :for prev := initial-value :then (funcall function prev (funcall key curr))
                           :for curr :in (subseq sequence start end)
                           :do (push prev result)
                           :finally (return (nreverse (push prev result))))
                     (loop :with result := '()
                           :for prev := initial-value :then (funcall function prev curr)
                           :for curr :in (subseq sequence start end)
                           :do (push prev result)
                           :finally (return (nreverse (push prev result)))))
                 (if key
                     (let ((lst (subseq sequence start end)))
                       (loop :with result := '()
                             :for prev := (funcall key (first lst)) :then (funcall function prev (funcall key curr))
                             :for curr :in (rest lst)
                             :do (push prev result)
                             :finally (return (nreverse (push prev result)))))
                     (let ((lst (subseq sequence start end)))
                       (loop :with result := '()
                             :for prev := (first lst) :then (funcall function prev curr)
                             :for curr :in (rest lst)
                             :do (push prev result)
                             :finally (return (nreverse (push prev result)))))))))))


(setf (documentation 'scan 'function) "
SCAN function sequence &key key initial-value (start 0) end from-end => result

Notes
-----
 - If `from-end' is NIL (default), it scans `sequence' from left.
 - If `from-end' is T, it scans `sequence' from right.
 - If `key' is supplied, it is applied exactly once to each element of `sequence'.
 - (first (last (scan fn xs)) == (reduce fn xs)
 - (first (scan fn xs :from-end t)) == (reduce fn xs :from-end t)

References
----------
 [0] Simon Marlow ed., Haskell 2010: Language Report, 20.4.1 Scans.
 [1] CLHS, Function REDUCE.
")


;;--------------------------------------------------------------------
;; unreduce
;;--------------------------------------------------------------------

;; ? RENAME: unreduce -> unfold ?

(defun unreduce (successor-function seed stop-test &key key count from-end)

  (check-type successor-function (or symbol function))
  (check-type stop-test          (or symbol function))
  (check-type key                (or symbol function))
  (check-type count              (or null (integer 0 *)))
  
  ;; TODO: key
  (setf key (or key #'identity))

  (if from-end
      (if count
          (labels ((rec (curr acc count)
                     (if (or (funcall stop-test curr)
                             (zerop count))
                         acc
                         (rec (funcall successor-function curr)
                              (cons (funcall key curr) acc)
                              (1- count)))))
            (rec seed '() count))
          (labels ((rec (curr acc)
                     (if (funcall stop-test curr)
                         acc
                         (rec (funcall successor-function curr)
                              (cons (funcall key curr) acc)))))
            (rec seed '())))
      (if count
          (labels ((rec (curr acc count)
                     (if (or (funcall stop-test curr)
                             (zerop count))
                         (nreverse acc)
                         (rec (funcall successor-function curr)
                              (cons (funcall key curr) acc)
                              (1- count)))))
            (rec seed '() count))
          (labels ((rec (curr acc)
                     (if (funcall stop-test curr)
                         (nreverse acc)
                         (rec (funcall successor-function curr)
                              (cons (funcall key curr) acc)))))
            (rec seed '())))))


(setf (documentation 'unreduce 'function) "
UNREDUCE successor-function seed stop-test &key key count from-end => result-list
")



;;====================================================================
