;;;; cl-plus/test/srfi/srfi-41.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

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

;;====================================================================
;; Test for SRFI-41
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.srfi.srfi-41
  (:documentation "
Test for SRFI-41
================

References
----------
 [0] Philip L. Bewig.
     Scheme Request for Implementation 40: A Library of Streams.
     http://srfi.schemers.org/srfi-40
     (srfi-40 is deprecated by srfi-41)

 [1] Philip L. Bewig.
     Scheme Request for Implementation 41: Streams.
     http://srfi.schemers.org/srfi-41

 [2] g000001.
     srfi-41, https://github.com/g000001/srfi-41
     (Scheme style Common Lisp translation)

 [3] Rosetta Code, N-queens problem.
     http://rosettacode.org/wiki/N-queens_problem#Common_Lisp
")
  (:export #:?srfi-41)
  (:use #:cl #:fiveam)
  (:import-from #:alexandria
                #:read-file-into-string)
  (:import-from #:cl-plus-test
                #:?all)
  (:import-from #:cl-plus.src.srfi.srfi-45
                #:lazy
                #:eager
                #:delay
                #:force)
  (:import-from #:cl-plus.src.srfi.srfi-41
                ;; PRIMITIVE     Original name
                #:+empty-pipe+ ; stream-null
                #:pipe-cons    ; stream-cons
                #:pipep        ; stream?
                #:pipe-null    ; stream-null?
                #:pipe-cons-p  ; stream-pair?
                #:pipe-car     ; stream-car
                #:pipe-cdr     ; stream-cdr
                #:pipe-lambda  ; stream-lambda

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
                ))

(in-package #:cl-plus.test.srfi.srfi-41)

(def-suite ?srfi-41 :in ?all)
(in-suite ?srfi-41)

;;--------------------------------------------------------------------
;; Util
;;--------------------------------------------------------------------

(defmacro with-muffle-style-warning (&body body)
  #+sbcl
  `(locally (declare (sb-ext:muffle-conditions style-warning))
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun square (x) (* x x))

;;--------------------------------------------------------------------
;; Feature
;;--------------------------------------------------------------------

(test ?srfi-41-feature
  (is-true (find :srfi-41 *features*)))

;;--------------------------------------------------------------------
;; Primitive
;;--------------------------------------------------------------------

(test ?pipe-null
  (is-true  (pipe-null +empty-pipe+))
  (is-false (pipe-null (pipe-cons 0 +empty-pipe+))))

(test ?pipe-cons-p
  (is-true  (pipe-cons-p (pipe-cons 0 +empty-pipe+)))
  (is-false (pipe-cons-p (pipe-car (pipe-cons 0 +empty-pipe+))))
  (is-false (pipe-cons-p 1))
  (is-false (pipe-cons-p 'foo))
  (is-false (pipe-cons-p (cons 1 3)))
  (is-false (pipe-cons-p +empty-pipe+)))

(test ?example.0
  (let ((pipe123 (pipe-cons 1
                            (pipe-cons 2
                                       (pipe-cons 3
                                                  +empty-pipe+)))))
    (is (= 1 (pipe-car pipe123)))
    (is (= 2 (pipe-car (pipe-cdr pipe123))))
    (is (= 3 (pipe-car (pipe-cdr (pipe-cdr pipe123)))))
    (is-true  (pipe-null (pipe-cdr (pipe-cdr (pipe-cdr pipe123)))))
    (is-false (pipe-null pipe123))
    (is-false (handler-case
                  (pipe-cons-p
                   (pipe-cdr (pipe-cons (with-muffle-style-warning (/ 1 0))
                                        +empty-pipe+)))
                (error () t)))
    (is-true  (pipep +empty-pipe+))
    (is-true  (pipep pipe123))
    (is-false (pipep (list 1 2 3)))))

(defun iter (fn x)
  (lazy (pipe-cons x (iter fn (funcall fn x)))))

(defun pipe-add (p1 p2)
  (lazy
   (pipe-cons (+ (pipe-car p1) (pipe-car p2))
              (pipe-add (pipe-cdr p1) (pipe-cdr p2)))))

(test ?example.1
  (let* ((nats  (iter #'1+ 0))
         (evens (pipe-add nats nats)))
    (is (= 1 (pipe-car (pipe-cdr nats))))
    (is (= 0 (pipe-car evens)))
    (is (= 2 (pipe-car (pipe-cdr evens))))
    (is (= 4 (pipe-car (pipe-cdr (pipe-cdr evens)))))))


;;--------------------------------------------------------------------
;; Derived
;;--------------------------------------------------------------------

(test ?list->pipe
  (let ((pipe123 (list->pipe '(1 2 3))))
    (is (= 1 (pipe-car pipe123)))
    (is (= 2 (pipe-car (pipe-cdr pipe123))))
    (is (= 3 (pipe-car (pipe-cdr (pipe-cdr pipe123)))))
    (is-true (pipe-null (pipe-cdr (pipe-cdr (pipe-cdr pipe123))))))
  (is-true (handler-case
               (list->pipe (list 1 (with-muffle-style-warning (/ 1 0)) -1))
             (error () t)
             (no-error () nil))))


;; !! DANGER !! The followings may not close stream.
;; (defun file->pipe (pathname)
;;   (let ((in (open pathname)))
;;     (pipe-let %loop ((char (read-char in nil :eof)))
;;       (if (eq char :eof)
;;           (progn
;;             (close in)
;;             +empty-pipe+)
;;           (pipe-cons char
;;                      (%loop (read-char in nil :eof)))))))
;; (defun file->pipe (pathname)
;;   (let ((in (open pathname)))
;;     (labels ((%loop (char)
;;                (lazy
;;                 (if (eq char :eof)
;;                     (progn
;;                       (close in)
;;                       +empty-pipe+)
;;                     (pipe-cons char
;;                                (%loop (read-char in nil :eof)))))))
;;       (%loop (read-char in nil :eof)))))

(test ?stream->pipe
  (let ((system-file (asdf:system-source-file :cl-plus-test)))
    (is (string= (with-open-file (in system-file)
                   (coerce (pipe->list (stream->pipe in)) 'string))
                 (alexandria:read-file-into-string system-file)))))

(test ?pipe
  (let ((pipe123 (pipe 1 2 3)))
    (is (= 1 (pipe-car pipe123)))
    (is (= 2 (pipe-car (pipe-cdr pipe123))))
    (is (= 3 (pipe-car (pipe-cdr (pipe-cdr pipe123)))))
    (is-true (pipe-null (pipe-cdr (pipe-cdr (pipe-cdr pipe123))))))
  (is-true (handler-case
               (pipe 1 (with-muffle-style-warning (/ 1 0)) -1)
             (error () nil)
             (no-error () t))))

(test ?pipe->list
  (is (equal (pipe->list (pipe-map #'square (pipe-from 0))
                         10)
             '(0 1 4 9 16 25 36 49 64 81))))

(defun qsort (order pipe)
  (lazy
   (if (pipe-null pipe)
       +empty-pipe+
       (let ((x  (pipe-car pipe))
             (xs (pipe-cdr pipe)))
         (pipe-append (qsort order (pipe-filter
                                    (lambda (u) (funcall order u x))
                                    xs))
                      (pipe x)
                      (qsort order (pipe-filter
                                    (lambda (u) (not (funcall order u x)))
                                    xs)))))))

(test ?pipe-append
  (is-true (pipe-null (pipe-append)))
  (is-true (pipe-null (pipe-append +empty-pipe+)))
  (is-true (pipe-null (pipe-append +empty-pipe+ +empty-pipe+)))
  (for-all ((lst1 (gen-list))
            (lst2 (gen-list))
            (lst3 (gen-list)))
    (is (equal (append lst1 lst2 lst3)
               (pipe->list
                (pipe-append (list->pipe lst1)
                             (list->pipe lst2)
                             (list->pipe lst3))))))
  (for-all ((to-be-sorted (gen-list)))
    (is (equal (pipe->list
                (qsort #'< (list->pipe to-be-sorted)))
               (sort (copy-list to-be-sorted) #'<)))
    (is (equal (pipe->list
                (qsort #'> (list->pipe to-be-sorted)))
               (sort (copy-list to-be-sorted) #'>)))))

;; (defun interleave (x yy)
;;   (pipe-match yy
;;     (()       (pipe (pipe x)))
;;     ((y . ys) (pipe-append (pipe (pipe-cons x yy))
;;                            (pipe-map (lambda (z) (pipe-cons y z))
;;                                      (interleave x ys))))))

;; (defun perms (xs)
;;   (lazy
;;    (if (pipe-null xs)
;;        (pipe (pipe))
;;        (pipe-concat (pipe-map (lambda (ys) (interleave (pipe-car xs) ys))
;;                               (perms (pipe-cdr xs)))))))

(test ?pipe-concat
  (is-true (pipe-null (pipe-concat (pipe (pipe) (pipe) (pipe)))))
  (is (equal (pipe->list (pipe-concat (pipe (pipe 1 2) (pipe) (pipe 3 2 1))))
             '(1 2 3 2 1))))

(test ?pipe-constant
  (let ((111... (pipe-constant 1)))
    (is (= 1 (pipe-car 111...)))
    (is (= 1 (pipe-car (pipe-cdr 111...))))
    (is (= 1 (pipe-car (pipe-cdr (pipe-cdr 111...))))))
  (let ((t-nil... (pipe-constant t nil)))
    (is (eq t   (pipe-car t-nil...)))
    (is (eq nil (pipe-car (pipe-cdr t-nil...))))
    (is (eq t   (pipe-car (pipe-cdr (pipe-cdr t-nil...)))))
    (is (eq nil (pipe-car (pipe-cdr (pipe-cdr (pipe-cdr t-nil...))))))))

(defun pipe-split (n pipe)
  (values (pipe-take n pipe)
          (pipe-drop n pipe)))

(test ?pipe-drop
  (is (equal (pipe->list (pipe-drop 0 (pipe 0 1 2 3 4 5)))
             '(0 1 2 3 4 5)))
  (is (equal (pipe->list (pipe-drop 3 (pipe 0 1 2 3 4 5)))
             '(3 4 5)))
  (is-true (pipe-null (pipe-drop 30 (pipe 0 1 2 3 4 5))))
  (is (equal (mapcar #'pipe->list
                     (multiple-value-list (pipe-split 3 (pipe 0 1 2 3 4 5))))
             '((0 1 2) (3 4 5)))))

(defun pipe-unique (test pipe)
  (if (pipe-null pipe)
      +empty-pipe+
      (pipe-cons (pipe-car pipe)
                 (pipe-unique
                  test
                  (pipe-drop-while (lambda (x) (eql (pipe-car pipe) x))
                                   pipe)))))

(test ?pipe-drop-while
  (is-true (pipe-null (pipe-drop-while #'identity
                                       +empty-pipe+)))
  (is (equal (pipe->list
              (pipe-drop-while (lambda (x) (< x 3))
                               (pipe 0 1 2 3 4 5)))
             '(3 4 5)))
  (is (equal (pipe->list
              (pipe-drop-while (lambda (x) (< x 30))
                               (pipe 0 1 2 3 4 5)))
             '()))
  (is (equal (pipe->list
              (pipe-drop-while (lambda (x) (< 30 x))
                               (pipe 0 1 2 3 4 5)))
             '(0 1 2 3 4 5)))
  (is (equal (pipe->list
              (pipe-unique #'= (pipe 0 0 0 1 1 1 0 0 0 1 1 1)))
             '(0 1 0 1))))

(test ?pipe-filter
  (is (equal (pipe->list
              (pipe-filter #'oddp (pipe-from 0)) 5)
             '(1 3 5 7 9)))
  (is (equal (pipe->list
              (pipe-filter #'evenp (pipe 1 3 5 7 9)))
             '())))

(defun pipe-max (pipe)
  (pipe-fold (lambda (x y) (if (< x y) y x))
             (pipe-car pipe)
             (pipe-cdr pipe)))

(defun pipe-fold-one (fn pipe)
  (pipe-fold fn
             (pipe-car pipe)
             (pipe-cdr pipe)))

(defun pipe-min (pipe)
  (pipe-fold (lambda (x y) (if (> x y) y x))
             (pipe-car pipe)
             (pipe-cdr pipe)))

(defun isort (order pipe)
  (labels ((insert (pipe x)
             (lazy
              (pipe-match pipe
                (() (pipe x))
                ((y . ys) (if (funcall order y x)
                              (pipe-cons y (insert ys x))
                              (pipe-cons x pipe)))))))
    (pipe-fold #'insert +empty-pipe+ pipe)))

(test ?pipe-fold
  (is (= (pipe-max (pipe 0 8 7 42 3 4))
         42))
  (is (= (pipe-min (pipe 0 8 7 42 3 4))
         0))
  (for-all ((to-be-sorted (gen-list)))
    (is (equal (pipe->list
                (isort #'< (list->pipe to-be-sorted)))
               (sort (copy-list to-be-sorted) #'<)))
    (is (equal (pipe->list
                (isort #'> (list->pipe to-be-sorted)))
               (sort (copy-list to-be-sorted) #'>)))))


;; TODO:
(test ?pipe-for-each
  )

(test ?pipe-from
  (is (equal (pipe->list (pipe-from 0) 10)
             '(0 1 2 3 4 5 6 7 8 9)))
  (is (equal (pipe->list (pipe-from 1 2) 5)
             '(1 3 5 7 9)))
  (is (equal (pipe->list (pipe-from -1 -2) 5)
             '(-1 -3 -5 -7 -9))))

(test ?pipe-iterate
  (is (equal (pipe->list (pipe-iterate #'1+ 0) 10)
             (loop :repeat 10 :for i :from 0 :collect i)))
  (is (equal (pipe->list (pipe-iterate (lambda (x) (* 2 x)) 1) 10)
             (loop :repeat 10 :for i :from 0 :collect (expt 2 i)))))

(test ?pipe-length
  (is (= 0 (pipe-length +empty-pipe+)))
  (is (= 1 (pipe-length (pipe 1))))
  (is (= 2 (pipe-length (pipe 1 2))))
  (is (= 3 (pipe-length (pipe 1 2 3)))))

(defun pipe-member (obj pipe)
  (pipe-let rec ((p pipe))
    (cond ((pipe-null p)          p)
          ((eql obj (pipe-car p)) p)
          (t (rec (pipe-cdr p))))))

(test ?pipe-let
  (is (equal (pipe->list (pipe-member 1 (pipe 0 1 2 3)))
             '(1 2 3)))
  (is (equal (pipe->list (pipe-member 42 (pipe 0 1 2 3)))
             '())))

(test ?pipe-map
  (let ((square-pipe (pipe-map #'square (pipe 9 3))))
    (is (equal (pipe->list square-pipe)
               '(81 9))))
  (flet ((sigma (f m n)
           (pipe-fold #'+ 0 (pipe-map f (pipe-range m (1+ n))))))
    (is (= (sigma #'square 1 100)
           338350))))


(defun len (pipe)
  (pipe-match pipe
    (() 0)
    ((head . tail) (1+ (len tail)))))

(defun pipe-merge (order &rest pipes)
  (labels ((%merge (xx yy)
             (lazy
              (pipe-match xx
                (() yy)
                ((x . xs) (pipe-match yy
                            (() xx)
                            ((y . ys) (if (funcall order y x)
                                          (pipe-cons y (%merge xx ys))
                                          (pipe-cons x (%merge xs yy))))))))))
    (pipe-let %loop ((ps pipes))
      (cond ((null ps)       +empty-pipe+)
            ((null (cdr ps)) (car ps))
            (t (%merge (car ps)
                       (apply #'pipe-merge order (cdr ps))))))))

(test ?pipe-match
  (is (= 3 (len (pipe 1 2 3))))
  (is-false (pipe-match (pipe 1 2 3)
              ((x y . _) (equal x y) t)
              (t nil)))
  (is-true (pipe-match (pipe 1 1 3)
             ((x y . _) (equal x y) t)
             (t nil)))
  (for-all ((lst1 (gen-list))
            (lst2 (gen-list)))
    (is (equal (pipe->list
                (pipe-merge #'< (list->pipe lst1) (list->pipe lst2)))
               (merge 'list (copy-list lst1) (copy-list lst2) #'<)))))

;; TODO: add is-test
(test ?pipe-of
  (is (equal (pipe->list
              (pipe-of (* x x)
                (x :in (pipe-range 0 10))
                (evenp x)))
             '(0 4 16 36 64)))
  (is (equal (pipe->list
              (pipe-of (* x x)
                (x in (pipe-range 0 10))
                (evenp x)))
             '(0 4 16 36 64)))
  (is (equal (pipe->list
              (pipe-of (list a b)
                (a :in (pipe-range 1 4))
                (b :in (pipe-range 1 3))))
             '((1 1) (1 2) (2 1) (2 2) (3 1) (3 2))))
  (is (equal (pipe->list
              (pipe-of (list a b)
                (a in (pipe-range 1 4))
                (b in (pipe-range 1 3))))
             '((1 1) (1 2) (2 1) (2 2) (3 1) (3 2))))
  (is (equal (pipe->list
              (pipe-of (list i j)
                (i :in (pipe-range 1 5))
                (j :in (pipe-range (+ i 1) 5))))
             '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))))
  (is (equal (pipe->list
              (pipe-of (list i j)
                (i in (pipe-range 1 5))
                (j in (pipe-range (+ i 1) 5))))
             '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))))

(test ?pipe-range
  (is (equal (pipe->list (pipe-range 0 10) 10)
             '(0 1 2 3 4 5 6 7 8 9)))
  (is (equal (pipe->list (pipe-range 0 10 2))
             '(0 2 4 6 8))))

(defun fact (n)
  (labels ((rec (acc n)
             (if (<= n 1)
                 acc
                 (rec (* n acc) (1- n)))))
    (rec 1 n)))

(defun pipe-fact (n)
  (pipe-ref
   (pipe-scan #'* 1 (pipe-from 1))
   n))

(test ?pipe-ref
  (let ((pipe (pipe 0 (with-muffle-style-warning (/ 1 0)) 2)))
    (is (= 0 (pipe-ref pipe 0)))
    (is (= 2 (pipe-ref pipe 2)))
    (signals division-by-zero (pipe-ref pipe 1)))
  (for-all ((i (gen-integer :min 1 :max 20)))
    (is (= (pipe-fact i) (fact i)))))

(test ?pipe-reverse
  (let* ((s (pipe 1 (with-muffle-style-warning (/ 1 0)) -1))
         (r (pipe-reverse s)))
    (is (= -1 (pipe-ref r 0)))
    (is (=  1 (pipe-ref r 2)))
    (is-true (handler-case
                 (pipe-ref r 1)
               (division-by-zero () t)
               (error () nil)
               (no-error () nil)))))

(test ?pipe-scan
  (is (equal (pipe->list (pipe-scan #'+ 0 (pipe-from 1)) 6)
             '(0 1 3 6 10 15)))
  (is (equal (pipe->list (pipe-scan #'* 1 (pipe-from 1)) 6)
             '(1 1 2 6 24 120))))

(defun msort (order pipe)
  (let* ((n  (truncate (pipe-length pipe) 2))
         (ts (pipe-take n pipe))
         (ds (pipe-drop n pipe)))
    (if (zerop n)
        pipe
        (pipe-merge order (msort #'< ts) (msort #'< ds)))))

(test ?pipe-take
  (for-all ((to-be-sorted (gen-list)))
    (is (equal (pipe->list
                (msort #'< (list->pipe to-be-sorted)))
               (sort (copy-list to-be-sorted) #'<)))))

(defun primes ()
  (labels ((next (base mult pipe)
             (lazy
              (let ((first (pipe-car pipe))
                    (rest  (pipe-cdr pipe)))
                (cond ((< first mult)
                       (pipe-cons first
                                  (next base mult rest)))
                      ((< mult first)
                       (next base (+ base mult) pipe))
                      (t (next base (+ base mult) rest))))))
           
           (sift (base pipe)
             (lazy (next base (+ base base) pipe)))
           
           (sieve (pipe)
             (lazy (let ((first (pipe-car pipe))
                         (rest  (pipe-cdr pipe)))
                     (pipe-cons first
                                (sieve (sift first rest)))))))
    (sieve (pipe-from 2))))

(test ?pipe-take-while
  (is (equal (pipe->list (pipe-take-while #'oddp (pipe 1 3 5 7)))
             '(1 3 5 7)))
  (is (equal (pipe->list (pipe-take-while #'oddp (pipe 1 3 5 7 8 9)))
             '(1 3 5 7)))
  (pipe->list (pipe-take-while #'oddp (pipe-from 0)) 10)
  (is (= (pipe-car
          (pipe-reverse
           (pipe-take-while
            (lambda (x) (< x 1000))
            (primes))))
         997)))

(test ?pipe-unfold
  (is (equal (pipe->list (pipe-unfold (lambda (x) (expt x 2))
                                      (lambda (x) (< x 10))
                                      (lambda (x) (+ x 1))
                                      0))
             '(0 1 4 9 16 25 36 49 64 81))))

(defun pipe-partition (pred pipe)
  (pipe-unfolds
   (lambda (p)
     (if (pipe-null p)
         (values p '() '())
         (let ((a (pipe-car p))
               (d (pipe-cdr p)))
           (if (funcall pred a)
               (values d (list a) :false)
               (values d :false (list a))))))
   pipe))

(test ?pipe-unfolds
  (is (equal (multiple-value-call
                 (lambda (odds evens)
                   (list (pipe->list odds)
                         (pipe->list evens)))
               (pipe-partition #'oddp (pipe-range 1 6)))
             '((1 3 5) (2 4)))))

(defun pipe-finds (test object pipe)
  (pipe-of (first x)
    (x :in (pipe-zip (pipe-from 0) pipe))
    (funcall test object (second x))))

(defun pipe-find (test object pipe)
  (pipe-car
   (pipe-append (pipe-finds test object pipe)
                (pipe nil))))

(test ?pipe-zip
  (is (= 2 (pipe-find #'char= #\l (list->pipe (coerce "hello" 'list)))))
  (is-false (pipe-find #'char= #\l (list->pipe (coerce "goodbye" 'list)))))


;;--------------------------------------------------------------------
;; Eight Queens
;;--------------------------------------------------------------------

(defun conflictp (i j m n)
  (or (= j n)
      (= (+ i j) (+ m n))
      (= (- i j) (- m n))))

(defun pipe-and (pipe)
  (labels ((rec (pipe)
             (cond ((pipe-null pipe) t)
                   ((not (pipe-car pipe)) nil)
                   (t (rec (pipe-cdr pipe))))))
    (rec pipe)))

(defun safep (p n)
  (let* ((len (pipe-length p))
         (m (1+ len)))
    (pipe-and
     (pipe-of (not (conflictp (first ij) (second ij) m n))
       (ij in (pipe-zip (pipe-range 1 m)
                        p))))))

(defun queens (m)
  (if (zerop m)
      (pipe (pipe))
      (pipe-of (pipe-append p (pipe n))
        (p in (queens (1- m)))
        (n in (pipe-range 1 9))
        (safep p n))))

;; cf. Rosetta Code, N-queens problem.
;; http://rosettacode.org/wiki/N-queens_problem#Common_Lisp
(defun n-queens (n m)
  (if (= n 1)
      (loop :for x :from 1 :to m :collect (list x))
      (loop :for sol :in (n-queens (1- n) m)
            :append (loop :for col :from 1 :to m
                          :when (loop :for row :from 0 :to (length sol)
                                      :for c :in sol
                                      :always (and (/= col c)
                                                   (/= (abs (- c col)) (1+ row)))
                                      :finally (return (cons col sol)))
                            :collect it))))

(test ?8-queens
  (is-true (not (set-difference (pipe->list (pipe-map #'pipe->list (queens 8)))
                                (n-queens 8 8)
                                :test #'equal))))


;;====================================================================
