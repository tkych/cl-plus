;;;; cl-plus/test/core/lazy-sequence.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;; ECL?
;;  Running test ?NOTANY*.LAZY-SEQNENCE 
;;; OPTIMIZE levels: Safety=0, Space=0, Speed=3, Debug=0
;;;
;;; End of Pass 1...; Evaluation aborted on #<a EXT:STORAGE-EXHAUSTED>.
;;; => ERROR! HEAP EXHOUST


;;====================================================================
;; Test for Lazy-Sequence 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.lazy-sequence
  (:export #:?lazy-sequence)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:all)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:cl-plus.src.readtables
                #:cl+sharp-bracket)
  (:import-from #:cl-plus.src.srfi.srfi-45
                #:delay
                #:eager
                #:lazy)
  (:import-from #:cl-plus.src.srfi.srfi-41
                #:+empty-pipe+
                #:pipe
                #:pipe-range)
  (:import-from #:cl-plus.src.core.lazy-sequence
                #:lazy-sequence
                #:lazy-sequence-p
                #:make-lazy-seq
                #:accessed-length
                #:copy-lazy-seq
                #:realize           
                #:lref
                #:all-accessed-p
                #:with-lazy-seq-iterator
                #:pipe->lseq
                #:lseq->pipe
                #:lazy-repeat
                #:replicate
                #:cycle
                #:induce
                #:lazy-for
                #:lfor
                #:lazy-range
                #:lazy-map
                #:lazy-reduce
                #:lazy-remove
                #:lazy-remove-if
                #:lazy-take
                #:lazy-take-until
                #:lazy-take-while
                #:lazy-drop
                #:lazy-drop-until
                #:lazy-drop-while
                #:lazy-zip
                #:lazy-interleave
                #:lazy-interpose
                #:lazy-flow
                #:lazy-flow-p
                #:make-lazy-flow
                #:copy-lazy-flow
                #:current-flow
                #:replicate-flow
                #:cycle-flow
                #:induce-flow
                #:lseq->lflow
                #:lflow->lseq))

(in-package #:cl-plus.test.core.lazy-sequence)

(def-suite ?lazy-sequence :in all)
(in-suite ?lazy-sequence)

(in-readtable cl+sharp-bracket)

;;--------------------------------------------------------------------

(test ?lazy-sequence.type
  (is-true  (typep (make-lazy-seq)
                   'lazy-sequence))
  (is-true  (typep (make-lazy-seq '(0 1))
                   'lazy-sequence))
  (is-true  (typep (make-lazy-seq '(0 1) (pipe 2 3))
                   'lazy-sequence))
  (is-true  (typep (make-lazy-seq '() (pipe 2 3))
                   'lazy-sequence))
  (is-false (typep 42    'lazy-sequence))
  (is-false (typep "42"  'lazy-sequence))
  (is-false (typep 'foo  'lazy-sequence))
  (is-false (typep '(42) 'lazy-sequence))
  (is-false (typep #(42) 'lazy-sequence))
  (is-false (typep #*10  'lazy-sequence))
  (is-false (typep (make-hash-table) 'lazy-sequence))
  (is-false (typep (lazy  42) 'lazy-sequence))
  (is-false (typep (eager 42) 'lazy-sequence))
  (is-false (typep (delay 42) 'lazy-sequence))
  (is-false (typep +empty-pipe+ 'lazy-sequence))
  (is-false (typep (pipe 0 1 2) 'lazy-sequence)))

(test ?lazy-sequence-p
  (is-true  (lazy-sequence-p (make-lazy-seq)))
  (is-true  (lazy-sequence-p (make-lazy-seq '(0 1))))
  (is-true  (lazy-sequence-p (make-lazy-seq '(0 1) (pipe 2 3))))
  (is-true  (lazy-sequence-p (make-lazy-seq '() (pipe 2 3))))
  (is-false (lazy-sequence-p 42   ))
  (is-false (lazy-sequence-p "42" ))
  (is-false (lazy-sequence-p 'foo ))
  (is-false (lazy-sequence-p '(42)))
  (is-false (lazy-sequence-p #(42)))
  (is-false (lazy-sequence-p #*10 ))
  (is-false (lazy-sequence-p (make-hash-table)))
  (is-false (lazy-sequence-p (lazy  42)))
  (is-false (lazy-sequence-p (eager 42)))
  (is-false (lazy-sequence-p (delay 42)))
  (is-false (lazy-sequence-p +empty-pipe+))
  (is-false (lazy-sequence-p (pipe 0 1 2))))

(test ?copy-lazy-seq
  (let ((lseq (make-lazy-seq '(0 1) (pipe 2 3))))
    (is (equalp lseq (copy-lazy-seq lseq)))))

(test ?lazy-take
  (is (equal (lazy-take 0 (make-lazy-seq '(0 1 2 3 4)))
             '()))
  (is (equal (lazy-take 0 (make-lazy-seq '(0 1 2 3 4)))
             '()))
  (is (equal (lazy-take 5 (make-lazy-seq '(0 1 2 3 4)))
             '(0 1 2 3 4)))
  (is (equal (lazy-take 50 (make-lazy-seq '(0 1 2 3 4)))
             '(0 1 2 3 4)))
  (is (equal (lazy-take 50 (make-lazy-seq '(0 1 2 3 4) +empty-pipe+))
             '(0 1 2 3 4)))
  (is (equal (lazy-take 50 (make-lazy-seq '(-2 -1) (pipe 0 1 2 3 4)))
             '(-2 -1 0 1 2 3 4)))
  (is (equal (lazy-take :all (make-lazy-seq '(-2 -1) (pipe 0 1 2 3 4)))
             '(-2 -1 0 1 2 3 4))))

(test ?realize
  (let ((empty-lseq (make-lazy-seq)))
    (is (equal (multiple-value-list (realize empty-lseq))
               '(nil nil))))
  (let ((all-accessed-lseq (make-lazy-seq '(0 1 2))))
    (is (equal (multiple-value-list (realize all-accessed-lseq))
               '(nil nil))))
  (let ((lseq (make-lazy-seq '() (pipe 0 1))))
    (is (equal (multiple-value-list (realize lseq))
               '(0 t)))
    (is (equal (multiple-value-list (realize lseq))
               '(1 t)))
    (is (equal (multiple-value-list (realize lseq))
               '(nil nil)))))

(test ?lref
  (let ((lseq (make-lazy-seq '() (pipe 0 1 2 3 4))))
    (is (equal (multiple-value-list (lref lseq 0))
               '(0 t)))
    (is (equal (multiple-value-list (lref lseq 3 nil))
               '(nil nil)))
    (is (equal (multiple-value-list (lref lseq 3))
               '(3 t)))
    (is (equal (multiple-value-list (lref lseq 3 nil))
               '(3 t)))
    (is (equal (multiple-value-list (lref lseq 30 nil))
               '(nil nil)))))

(test ?with-lazy-seq-iterator
  (let ((lseq #42[0 ..]))
    (is (= 1 (accessed-length lseq)))
    (with-lazy-seq-iterator (gen lseq nil)
      (dotimes (_ 10)
        (gen)))
    (is (= 1 (accessed-length lseq)))
    (let ((elements '()))
      (with-lazy-seq-iterator (gen lseq t)
        (dotimes (_ 10)
          (multiple-value-bind (more? index value) (gen)
            (declare (ignore more? index))
            (push value elements))))
      (is (equal (reverse elements)
                 '(0 1 2 3 4 5 6 7 8 9)))
      (is (= 10 (accessed-length lseq))))))

(test ?pipe->lseq
  (let ((lseq2 (pipe->lseq (pipe))))
    (is-true (lazy-sequence-p lseq2))
    (is (equal (lazy-take 10 lseq2)
               '())))
  (let ((lseq (pipe->lseq (pipe 0 1 2 3 4))))
    (is-true (lazy-sequence-p lseq))
    (is (equal (lazy-take 10 lseq)
               '(0 1 2 3 4)))))

;;--------------------------------------------------------------------
;; bracket-reader
;;--------------------------------------------------------------------

(test ?bracket-reader.result-type
  (is-true (lazy-sequence-p #[0 1]))
  (is-true (lazy-sequence-p #[]))
  (is-true (lazy-sequence-p #[0 .. 10]))
  (is-true (lazy-sequence-p #[0 1 ..]))
  (is-true (lazy-sequence-p #[1 2 4 ..])))

(test ?bracket-reader.all-accessed
  (is (equal (lazy-take :all #[])
             '()))
  (is (equal (lazy-take :all #[0])
             '(0)))
  (is (equal (lazy-take :all #[0 1 2 3 4 5 6 7 8 9 10])
             '(0 1 2 3 4 5 6 7 8 9 10)))
  (is (equal (lazy-take :all #[:foo :bar :baz])
             '(:foo :bar :baz))))

(test ?bracket-reader.finite
  ;; arithmetic progression
  (is (equal (lazy-take :all #[1 .. 10])
             '(1 2 3 4 5 6 7 8 9 10)))
  (is (equal (lazy-take :all #[10 .. 1])
             '()))
  (is (equal (lazy-take :all #[10 9 .. 1])
             '(10 9 8 7 6 5 4 3 2 1)))
  (is (equal (lazy-take :all #[10 8 .. 1])
             '(10 8 6 4 2)))
  (signals simple-error #42[1 5 .. 3])
  (signals simple-error #42[10 5 .. 6])

  ;; Geometric Progression
  ;; Monotonic increase
  (is (equal (lazy-take :all #[1 2 4 .. 512])
             '(1 2 4 8 16 32 64 128 256 512)))
  ;; Monotonic decrease
  (is (equal (lazy-take :all #[512 256 128 .. 1])
             '(512 256 128 64 32 16 8 4 2 1)))

  ;; Oscillating increase
  (is (equal (lazy-take 10 #[1 -2 4..15])
             '(1 -2 4 -8)))
  (is (equal (lazy-take 10 #[1 -2 4..16])
             '(1 -2 4 -8 16)))
  (is (equal (lazy-take 10 #[1 -2 4..17])
             '(1 -2 4 -8 16 -32)))

  (is (equal (lazy-take 10 #[1 -2 4..-31])
             '(1 -2 4 -8 16)))
  (is (equal (lazy-take 10 #[1 -2 4..-32])
             '(1 -2 4 -8 16 -32)))
  (is (equal (lazy-take 10 #[1 -2 4..-33])
             '(1 -2 4 -8 16 -32 64)))
  
  ;; Oscillating decrease
  (is (equal (lazy-take :all #[1 -1/2 1/4 .. 1/15])
             '(1 -1/2 1/4 -1/8)))
  (is (equal (lazy-take :all #[1 -1/2 1/4 .. 1/16])
             '(1 -1/2 1/4 -1/8 1/16)))
  (is (equal (lazy-take :all #[1 -1/2 1/4 .. 1/17])
             '(1 -1/2 1/4 -1/8 1/16 -1/32)))
                                                 
  (is (equal (lazy-take :all #[1 -1/2 1/4 .. -1/31])
             '(1 -1/2 1/4 -1/8 1/16)))
  (is (equal (lazy-take :all #[1 -1/2 1/4 .. -1/32])
             '(1 -1/2 1/4 -1/8 1/16 -1/32)))
  (is (equal (lazy-take :all #[1 -1/2 1/4 .. -1/33])
             '(1 -1/2 1/4 -1/8 1/16 -1/32 1/64)))
  
  ;; misc
  (signals simple-error #42[:foo .. :quux])
  (is (string= (coerce (lazy-take :all #[#\a .. #\z]) 'string)
               "abcdefghijklmnopqrstuvwxyz"))
  (is (string= (coerce (lazy-take :all #[#\z .. #\a]) 'string)
               ""))
  (is (string= (coerce (lazy-take :all #[#\z #\y .. #\a]) 'string)
               "zyxwvutsrqponmlkjihgfedcba"))
  (is (string= (coerce (lazy-take :all #[#\a #\c .. #\z]) 'string)
               "acegikmoqsuwy"))
  (is (string= (coerce (lazy-take :all #[#\z #\x .. #\a]) 'string)
               "zxvtrpnljhfdb"))
  (signals simple-error #42[#\a #\c #\a .. #\z])
  (signals simple-error #42[:foo :bar .. :baz]))

(test ?bracket-reader.infinite
  (is (equal (lazy-take 5 #[0 ..])
             '(0 1 2 3 4)))
  (is (equal (lazy-take 5 #[0 1 ..])
             '(0 1 2 3 4)))
  (is (equal (lazy-take 5 #[1 2 ..])
             '(1 2 3 4 5)))
  (is (equal (lazy-take 5 #[1 2 4 ..])
             '(1 2 4 8 16)))
  (is (equal (lazy-take 5 #[1 2 8 ..])
             '(1 2 8 1 2)))
  (is (equal (lazy-take 5 #[:foo :bar :baz ..])
             '(:foo :bar :baz :foo :bar)))
  (is (equal (lazy-take 5 #[1 2 :foo 4 ..])
             '(1 2 :foo 4 1)))
  (is (string= (coerce (lazy-take 10 #[#\a ..]) 'string)
               "abcdefghij"))
  (is (string= (coerce (lazy-take 10 #[#\a #\c ..]) 'string)
               "acegikmoqs"))
  (is (string= (coerce (lazy-take 10 #[#\a #\c #\a .. ]) 'string)
               "acaacaacaa")))

(test ?bracket-reader.nest_string
 (is (equal (lazy-take 3 #["#[1..]"..])
            '("#[1..]" "#[1..]" "#[1..]")))
 (is (equal (lazy-take 10 (third (lazy-take 3 #[#[1..]..])))
            '(1 2 3 4 5 6 7 8 9 10)))
 (is (equal (lazy-take 3 #[""..])
            '("" "" "")))
 (is (equal (lazy-take 3 #["]]]"..])
            '("]]]" "]]]" "]]]")))
 (is (equal (lazy-take 3 (second (lazy-take 2 #[#["]]]"..] ..])))
            '("]]]" "]]]" "]]]"))))


;;--------------------------------------------------------------------
;; generators
;;--------------------------------------------------------------------
(test ?replicate
  (is-true (lazy-sequence-p (replicate :foo)))
  (is (equal (lazy-take 3 (replicate))
             '()))
  (is (equal (lazy-take 3 (replicate 0))
             '(0 0 0)))
  (is (equal (lazy-take 4 (replicate 0 1))
             '(0 1 0 1))))

(test ?cycle
  (signals simple-error (cycle nil))
  (signals simple-error (cycle #()))
  (signals simple-error (cycle ""))
  (signals simple-error (cycle #*))
  (is-true (lazy-sequence-p (cycle '(1))))
  (is (equal (lazy-take 5 (cycle '(:foo :bar :baz)))
             '(:foo :bar :baz :foo :bar)))
  (is (equal (lazy-take 5 (cycle #(:foo :bar :baz)))
             '(:foo :bar :baz :foo :bar)))
  (is (equal (lazy-take 6 (cycle #*1011))
             '(1 0 1 1 1 0)))
  (is (equal (lazy-take 8 (cycle "string"))
             '(#\s #\t #\r #\i #\n #\g #\s #\t))))

(test ?induce
  (is-true (lazy-sequence-p (induce #'identity 42)))
  (let ((lseq (induce (lambda (x) (1+ x)))))
    (signals error (lazy-take 1 lseq)))
  (is (equal (lazy-take 10 (induce #'identity 42))
             '(42 42 42 42 42 42 42 42 42 42)))
  (is (equal (lazy-take 10 (induce (lambda (x y) (+ x y)) 0 1))
             '(0 1 1 2 3 5 8 13 21 34)))
  (is (equal (with-output-to-string (s)
               (lazy-take 3 (induce (lambda () (princ 'lazy! s)))))
             "LAZY!LAZY!LAZY!")))

(test ?lazy-range
  (is-true   (lazy-sequence-p (lazy-range)))
  (is (equal (lazy-take 10 (lazy-range))
             '(0 1 2 3 4 5 6 7 8 9)))
  (is (equal (lazy-take 10 (lazy-range 0 0 0))
             '(0)))
  (is (equal (lazy-take 10 (lazy-range 0 nil 0))
             '(0 0 0 0 0 0 0 0 0 0)))
  (is (equal (lazy-take 10 (lazy-range 10 50 5))
             '(10 15 20 25 30 35 40 45)))
  (is (equal (lazy-take 10 (lazy-range 10 nil 5))
             '(10 15 20 25 30 35 40 45 50 55)))
  (is (equal (lazy-take 10 (lazy-range 10 -5 -5))
             '(10 5 0)))
  (is (equal (lazy-take 10 (lazy-range 10 nil -5))
             '(10 5 0 -5 -10 -15 -20 -25 -30 -35))))


;;--------------------------------------------------------------------
;; utils
;;--------------------------------------------------------------------

(test ?lazy-map
  (is-true (lazy-sequence-p (lazy-map #'1+ (lazy-range 0 nil 1))))
  (is (equal (lazy-take 10 (lazy-map #'1+ (make-lazy-seq)))
             '()))
  (is (equal (lazy-take 10 (lazy-map #'1+ (lazy-range 0 nil 1)))
             '(1 2 3 4 5 6 7 8 9 10))))

(test ?lazy-reduce
  (is (= (lazy-reduce (lambda (a v) (+ a v))
                      (lazy-range 0 10 1)
                      0)
         45))
  (is (equal (lazy-reduce #'cons
                          (lazy-range 1 4 1)
                          0)
             '(((0 . 1) . 2) . 3))))

(test ?lazy-take-until
  (is (equal (lazy-take-until #'oddp (make-lazy-seq))
             '()))
  (is (equal (lazy-take-until #'oddp (make-lazy-seq '(1 3 5 7)))
             '()))
  (is (equal (lazy-take-until #'oddp (make-lazy-seq '(0 2 4 8 9)))
             '(0 2 4 8)))
  (is (equal (lazy-take-until #'oddp (make-lazy-seq '() (pipe 1 3 5 7)))
             '()))
  (is (equal (lazy-take-until #'oddp (make-lazy-seq '() (pipe 0 2 4 8 9)))
             '(0 2 4 8))))

(test ?lazy-take-while
  (is (equal (lazy-take-while #'evenp (make-lazy-seq))
             '()))
  (is (equal (lazy-take-while #'evenp (make-lazy-seq '(1 3 5 7)))
             '()))
  (is (equal (lazy-take-while #'evenp (make-lazy-seq '(0 2 4 8 9)))
             '(0 2 4 8)))
  (is (equal (lazy-take-while #'evenp (make-lazy-seq '() (pipe 1 3 5 7)))
             '()))
  (is (equal (lazy-take-while #'evenp (make-lazy-seq '() (pipe 0 2 4 8 9)))
             '(0 2 4 8))))

(test ?lazy-drop
  (is (equal (lazy-take :all (lazy-drop 2 (make-lazy-seq)))
             '()))
  (is (equal (lazy-take :all (lazy-drop 0 (make-lazy-seq '(1 3 5 7))))
             '(1 3 5 7)))
  (is (equal (lazy-take :all (lazy-drop 3 (make-lazy-seq '(0 2 4 8 9))))
             '(8 9)))
  (is (equal (lazy-take :all (lazy-drop 2 (make-lazy-seq '() (pipe 1 3 5 7))))
             '(5 7)))
  (is (equal (lazy-take :all (lazy-drop 10 (make-lazy-seq '() (pipe 0 2 4 8 9))))
             '())))

(test ?lazy-drop-until
  (is (equal (lazy-take :all (lazy-drop-until #'oddp (make-lazy-seq)))
             '()))
  (is (equal (lazy-take :all (lazy-drop-until #'oddp (make-lazy-seq '(1 3 5 7))))
             '(1 3 5 7)))
  (is (equal (lazy-take :all (lazy-drop-until #'oddp (make-lazy-seq '(0 2 4 8 9))))
             '(9)))
  (is (equal (lazy-take :all (lazy-drop-until #'oddp (make-lazy-seq '() (pipe 1 3 5 7))))
             '(1 3 5 7)))
  (is (equal (lazy-take :all (lazy-drop-until #'oddp (make-lazy-seq '() (pipe 0 2 4 8 9))))
             '(9))))

(test ?lazy-drop-while
  (is (equal (lazy-take :all (lazy-drop-while #'evenp (make-lazy-seq)))
             '()))
  (is (equal (lazy-take :all (lazy-drop-while #'evenp (make-lazy-seq '(1 3 5 7))))
             '(1 3 5 7)))
  (is (equal (lazy-take :all (lazy-drop-while #'evenp (make-lazy-seq '(0 2 4 8 9))))
             '(9)))
  (is (equal (lazy-take :all (lazy-drop-while #'evenp (make-lazy-seq '() (pipe 1 3 5 7))))
             '(1 3 5 7)))
  (is (equal (lazy-take :all (lazy-drop-while #'evenp (make-lazy-seq '() (pipe 0 2 4 8 9))))
             '(9))))

;; TODO:
(test ?lazy-remove
  )

;; TODO:
(test ?lazy-remove-if
  )

(test ?lazy-zip
  (is-true (lazy-sequence-p (lazy-zip (lazy-range 0) (replicate :foo))))
  (is (equal (lazy-take 5 (lazy-zip (lazy-range 0) (make-lazy-seq)))
             '()))
  (is (equal (lazy-take 5 (lazy-zip (lazy-range 0) (replicate :foo)))
             '((0 :FOO) (1 :FOO) (2 :FOO) (3 :FOO) (4 :FOO))))
  (is (equal (lazy-take 5 (lazy-zip (lazy-range 0) (replicate :foo) (cycle #*10)))
             '((0 :FOO 1) (1 :FOO 0) (2 :FOO 1) (3 :FOO 0) (4 :FOO 1))))
  (is (equal (lazy-take 5 (lazy-zip (make-lazy-seq '(0 1)) (replicate :foo) (cycle #*10)))
             '((0 :FOO 1) (1 :FOO 0)))))

;; (test ?lazy-interleave
;;   (is-true (lazy-sequence-p (lazy-interleave (lazy-range 0) (repeat :foo))))
;;   (is (equal (lazy-take 5 (lazy-interleave (lazy-range 0) (make-lazy-seq)))
;;              '()))
;;   (is (equal (lazy-take 5 (lazy-interleave (lazy-range 0) (repeat :foo)))
;;              '(0 :FOO 1 :FOO 2)))
;;   (is (equal (lazy-take 10 (lazy-interleave (lazy-range 0) (repeat :foo) (cycle #*10)))
;;              '(0 :FOO 1 1 :FOO 0 2 :FOO 1 3)))
;;   (is (equal (lazy-take 5 (lazy-interleave (make-lazy-seq '(0 1)) (repeat :foo)))
;;              '(0 :FOO 1 :FOO))))

;; (test ?lazy-interpose
;;   (is-true (lazy-sequence-p (lazy-interpose :foo (lazy-range 0))))
;;   (is (equal (lazy-take 5 (lazy-interpose :foo (make-lazy-seq)))
;;              '()))
;;   (is (equal (lazy-take 5 (lazy-interpose :foo (lazy-range 0)))
;;              '(0 :FOO 1 :FOO 2)))
;;   (is (equal (lazy-take 5 (lazy-interpose :foo (make-lazy-seq '(0 1))))
;;              '(0 :FOO 1))))



;;--------------------------------------------------------------------
;; lazy-for, lfor
;;--------------------------------------------------------------------
;; TODO:
;; * gard-clause
;; * by-clause
;; * downto, upto above, below -clause

(test ?lfor
  (is (equal (macroexpand-1 '(lfor expr caluse0 caluse1 caluse2))
             '(lazy-for expr caluse0 caluse1 caluse2)))
  (is (equal (macroexpand-1 '(lfor S H O R T H A N D C A L U S E))
             '(lazy-for S H O R T H A N D C A L U S E))))

(test ?lazy-for.in-clause
  ;; (var :in seq {gard}*)
  (is (equal (lazy-take 10 (lazy-for x (x :in '(0 1 2 3))))
             '(0 1 2 3)))
  (is (equal (lazy-take 10 (lazy-for x (x :in '(0 1 2 3) :when #'oddp)))
             '(1 3)))
  (is (equal (lazy-take 10 (lazy-for x (x :in '(0 1 2 3) :unless #'oddp)))
             '(0 2)))
  (is (equal (lazy-take 10 (lazy-for x (x :in '(0 1 2 3 -1 42) :until #'minusp)))
             '(0 1 2 3)))
  (is (equal (lazy-take 10 (lazy-for x (x :in '(1 2 3 -1 42) :while #'plusp)))
             '(1 2 3))))

(test ?lazy-for.upfrom-clause
  ;; (var :upfrom start [{:to|:upto|:below} end] [:by step] {gard}*)
  (is (equal (lazy-take 10 (lazy-for x (x :upfrom 1)))
             '(1 2 3 4 5 6 7 8 9 10)))
  (is (equal (lazy-take 10 (lazy-for x :upfrom 1))
             '(1 2 3 4 5 6 7 8 9 10)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :upfrom 1)))
             '(1 4 9 16 25 36 49 64 81 100)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :upfrom 1 :to 5)))
             '(1 4 9 16 25)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :upfrom 1 :upto 5)))
             '(1 4 9 16 25)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :upfrom 1 :below 5)))
             '(1 4 9 16)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :upfrom 0 :to 10 :by 2)))
             '(0 4 16 36 64 100)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :upfrom 0 :to 10 :by (lambda (y) (+ y 2)))))
             '(0 4 16 36 64 100)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :upfrom 0 :upto 10 :by 2)))
             '(0 4 16 36 64 100)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :upfrom 0 :upto 10 :by (lambda (y) (+ y 2)))))
             '(0 4 16 36 64 100)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :upfrom 0 :below 10 :by 2)))
             '(0 4 16 36 64)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :upfrom 0 :below 10 :by (lambda (y) (+ y 2)))))
             '(0 4 16 36 64)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :upfrom 0 :by 2)))
             '(0 4 16 36 64 100 144 196 256 324)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :upfrom 0 :by (lambda (y) (+ y 2)))))
             '(0 4 16 36 64 100 144 196 256 324)))
  
  (is (equal (lazy-take 10 (lazy-for (* x x)
                             (x :upfrom 0 :by 2 :while (lambda (y) (< y 10)))))
             '(0 4 16 36 64))))

(test ?lazy-for.from-clause
  ;; (var :from start [{:upto|:downto|:to|:below|:above} end] [:by step] {gard}*)
  (is (equal (lazy-take 10 (lazy-for x (x :from 1)))
             '(1 2 3 4 5 6 7 8 9 10)))
  (is (equal (lazy-take 10 (lazy-for x :from 1))
             '(1 2 3 4 5 6 7 8 9 10)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 1)))
             '(1 4 9 16 25 36 49 64 81 100)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 1 :to 5)))
             '(1 4 9 16 25)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 1 :upto 5)))
             '(1 4 9 16 25)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 1 :below 5)))
             '(1 4 9 16)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 0 :to 10 :by 2)))
             '(0 4 16 36 64 100)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 0 :to 10 :by (lambda (y) (+ y 2)))))
             '(0 4 16 36 64 100)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 0 :upto 10 :by 2)))
             '(0 4 16 36 64 100)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 0 :upto 10 :by (lambda (y) (+ y 2)))))
             '(0 4 16 36 64 100)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 0 :below 10 :by 2)))
             '(0 4 16 36 64)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 0 :below 10 :by (lambda (y) (+ y 2)))))
             '(0 4 16 36 64)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 0 :by 2)))
             '(0 4 16 36 64 100 144 196 256 324)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 0 :by (lambda (y) (+ y 2)))))
             '(0 4 16 36 64 100 144 196 256 324)))
  
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 10 :downto 5)))
             '(100 81 64 49 36 25)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 10 :above 5)))
             '(100 81 64 49 36)))

  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 10 :to 0 :by -2)))
             '()))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 10 :to 0 :by 2)))
             '()))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 10 :to 0 :by (lambda (y) (- y 2)))))
             '()))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 10 :downto 0 :by 2)))
             '(100 64 36 16 4 0)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 10 :downto 0 :by (lambda (y) (- y 2)))))
             '(100 64 36 16 4 0)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 10 :above 0 :by 2)))
             '(100 64 36 16 4)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 10 :above 0 :by (lambda (y) (- y 2)))))
             '(100 64 36 16 4)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :from 10 :by (lambda (y) (- y 2)))))
             '(100 64 36 16 4 0 4 16 36 64))))


(test ?lazy-for.downfrom-clause
  ;; (var :downfrom start [{:downto|:to|:above} end] [:by step] {gard}*)
  (is (equal (lazy-take 10 (lazy-for x (x :downfrom 10)))
             '(10 9 8 7 6 5 4 3 2 1)))
  (is (equal (lazy-take 10 (lazy-for x :downfrom 10))
             '(10 9 8 7 6 5 4 3 2 1)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10)))
             '(100 81 64 49 36 25 16 9 4 1)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10 :downto 5)))
             '(100 81 64 49 36 25)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10 :to 5)))
             '(100 81 64 49 36 25)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10 :above 5)))
             '(100 81 64 49 36)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10 :to 0 :by -2)))
             '()))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10 :to 0 :by 2)))
             '(100 64 36 16 4 0)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10 :to 0 :by (lambda (y) (- y 2)))))
             '(100 64 36 16 4 0)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10 :downto 0 :by 2)))
             '(100 64 36 16 4 0)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10 :downto 0 :by (lambda (y) (- y 2)))))
             '(100 64 36 16 4 0)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10 :above 0 :by 2)))
             '(100 64 36 16 4)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10 :above 0 :by (lambda (y) (- y 2)))))
             '(100 64 36 16 4)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10 :by 2)))
             '(100 64 36 16 4 0 4 16 36 64)))
  (is (equal (lazy-take 10 (lazy-for (* x x) (x :downfrom 10 :by (lambda (y) (- y 2)))))
             '(100 64 36 16 4 0 4 16 36 64))))

(test ?lazy-for.cycle-clause
  ;; (var :cycle seq {gard}*)
  (is (equal (lazy-take 10 (lazy-for x :cycle '()))
             '()))
  (is (equal (lazy-take 10 (lazy-for x :cycle '(0 1 2)))
             '(0 1 2 0 1 2 0 1 2 0)))
  (is (equal (lazy-take 10 (lazy-for x :cycle #(0 1 2)))
             '(0 1 2 0 1 2 0 1 2 0)))
  (is (equal (lazy-take 10 (lazy-for x :cycle #(0 1 2) :when 'plusp))
             '(1 2 1 2 1 2 1 2 1 2)))
  (is (equal (lazy-take 10 (lazy-for x :cycle #(0 1 2) :unless 'oddp))
             '(0 2 0 2 0 2 0 2 0 2)))
  (is (equal (lazy-take 10 (lazy-for x :cycle #(0 2 4 5) :while 'evenp))
             '(0 2 4)))
  (is (equal (lazy-take 10 (lazy-for x :cycle #(0 2 4 5) :until 'oddp))
             '(0 2 4))))

(test ?lazy-for.repeat-clause
  ;; (var :repeat cont obj {gard}*)
  (is (equal (lazy-take 10 (lazy-for x :repeat 0 :foo))
             '()))
  (is (equal (lazy-take 10 (lazy-for x :repeat 5 :foo))
             '(:foo :foo :foo :foo :foo))))

(test ?lazy-for.replicate-clause
  ;; (var :replicate obj+)
  (is (equal (lazy-take 10 (lazy-for x :replicate :obj))
             '(:obj :obj :obj :obj :obj :obj :obj :obj :obj :obj)))
  (is (equal (lazy-take 10 (lazy-for x :replicate :foo :bar :baz :quux))
             '(:foo :bar :baz :quux :foo :bar :baz :quux :foo :bar))))

(test ?lazy-sequence.misc
  (is (equal (lazy-take 10 (lfor x))
             '(0 1 2 3 4 5 6 7 8 9)))
  (is (equal (lazy-take 10 (lfor x when 'oddp))
             '(1 3 5 7 9 11 13 15 17 19))))


;;--------------------------------------------------------------------
;; lazy-flow
;;--------------------------------------------------------------------






;;====================================================================
