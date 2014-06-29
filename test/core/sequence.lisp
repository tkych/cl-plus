;;;; Last modified: 2014-06-29 10:26:49 tkych

;; cl-plus/test/core/sequence.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for Sequence 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.sequence
  (:export #:?sequence)
  (:use #:cl #:fiveam)
  (:import-from #:alexandria
                #:set-equal)
  (:import-from #:cl-plus-test
                #:all
                #:is-=
                #:is-eql
                #:is-equal
                #:is-equalp)
  (:import-from #:cl-plus.src.core.sequence
                #:sequencep
                #:repeat
                #:permutations
                #:combinations
                #:inits
                #:tails
                #:unreduce
                #:scan
                #:split-at))

(in-package #:cl-plus.test.core.sequence)

(def-suite ?sequence :in all)
(in-suite ?sequence)


;;--------------------------------------------------------------------
;; sequencep
;;--------------------------------------------------------------------

(test ?sequencep
  (is-true  (sequencep '()))
  (is-true  (sequencep '(0 1)))
  (is-true  (sequencep #()))
  (is-true  (sequencep #(0 1)))
  (is-true  (sequencep ""))
  (is-true  (sequencep "string"))
  (is-true  (sequencep #*))
  (is-true  (sequencep #*01))
  (is-false (sequencep :foo))
  (is-false (sequencep (make-hash-table)))
  (is-false (sequencep #2A())))


;;--------------------------------------------------------------------
;; split-at
;;--------------------------------------------------------------------

(test ?split-at.list
  (is-equal (split-at -1 '(0 1 2 3 4 5))
            '(() (0 1 2 3 4 5)))
  (is-equal (split-at 0 '(0 1 2 3 4 5))
            '(() (0 1 2 3 4 5)))
  (is-equal (split-at 1 '(0 1 2 3 4 5))
            '((0) (1 2 3 4 5)))
  (is-equal (split-at 5 '(0 1 2 3 4 5))
            '((0 1 2 3 4) (5)))
  (is-equal (split-at 6 '(0 1 2 3 4 5))
            '((0 1 2 3 4 5) ())))

(test ?split-at.vector
  (is-equalp (split-at -1 #(0 1 2 3 4 5))
             '(#() #(0 1 2 3 4 5)))
  (is-equalp (split-at 0 #(0 1 2 3 4 5))
             '(#() #(0 1 2 3 4 5)))
  (is-equalp (split-at 1 #(0 1 2 3 4 5))
             '(#(0) #(1 2 3 4 5)))
  (is-equalp (split-at 5 #(0 1 2 3 4 5))
             '(#(0 1 2 3 4) #(5)))
  (is-equalp (split-at 6 #(0 1 2 3 4 5))
             '(#(0 1 2 3 4 5) #())))


(test ?split-at.string
  (is-equalp (split-at -1 "string")
             '("" "string"))
  (is-equalp (split-at 0 "string")
             '("" "string"))
  (is-equalp (split-at 1 "string")
             '("s" "tring"))
  (is-equalp (split-at 5 "string")
             '("strin" "g"))
  (is-equalp (split-at 6 "string")
             '("string" "")))


(test ?split-at.bit-vector
  (is-equalp (split-at -1 #*101010)
             '(#* #*101010))
  (is-equalp (split-at 0 #*101010)
             '(#* #*101010))
  (is-equalp (split-at 1 #*101010)
             '(#*1 #*01010))
  (is-equalp (split-at 5 #*101010)
             '(#*10101 #*0))
  (is-equalp (split-at 6 #*101010)
             '(#*101010 #*)))


;;--------------------------------------------------------------------
;; repeat
;;--------------------------------------------------------------------

(test ?repeat
  (signals type-error (repeat -1 :foo))
  (signals type-error (repeat :foo :bar))
  (is (equal  (repeat 4 :foo)
              '(:foo :foo :foo :foo)))
  (is (equalp (repeat 4 :foo 'vector)
              #(:foo :foo :foo :foo)))
  (is (equal  (repeat 4 #\a 'string)
              "aaaa"))
  (is (equal  (repeat 4 1 'bit-vector)
              #4*1)))


;;--------------------------------------------------------------------
;; permutations
;;--------------------------------------------------------------------

(test ?permutations.error
  (signals type-error (permutations :foo))
  (signals type-error (permutations '(0 1 2) :start -1))
  (signals type-error (permutations '(0 1 2) :start nil))
  (signals type-error (permutations '(0 1 2) :end -1))
  (signals type-error (permutations '(0 1 2) :end :foo))
  (signals simple-error (permutations '(0 1 2) :start 2 :end 1)))


(test ?permutations.list
  (is-equal (permutations '())
            '())
  (is (set-equal (permutations '(0 1 2))
                 '((0 1 2) (1 0 2) (2 0 1) (0 2 1) (1 2 0) (2 1 0))
                 :test 'equal))
  (is (set-equal (permutations '(0 1 2) :start 1)
                 '((1 2) (2 1))
                 :test 'equal))
  (is (set-equal (permutations '(0 1 2) :end 2)
                 '((0 1) (1 0))
                 :test 'equal)))


(test ?permutations.bit-vector
  (is-equal (permutations #*)
            '())
  (is (set-equal (permutations #*010)
                 '(#*010 #*100 #*001 #*001 #*100 #*010)
                 :test 'equal))
  (is (set-equal (permutations #*010 :start 1)
                 '(#*10 #*01)
                 :test 'equal))
  (is (set-equal (permutations #*010 :end 2)
                 '(#*01 #*10)
                 :test 'equal)))


(test ?permutations.vector
  (is-equal (permutations #())
            '())
  (is (set-equal (permutations #(0 1 2))
                 '(#(0 1 2) #(1 0 2) #(2 0 1) #(0 2 1) #(1 2 0) #(2 1 0))
                 :test 'equalp))
  (is (set-equal (permutations #(0 1 2) :start 1)
                 '(#(1 2) #(2 1))
                 :test 'equalp))
  (is (set-equal (permutations #(0 1 2) :end 2)
                 '(#(0 1) #(1 0))
                 :test 'equalp)))

(test ?permutations.string
  (is-equal (permutations "")
            '())
  (is (set-equal (permutations "abc")
                 '("abc" "bac" "cba" "bca" "cab" "acb")
                 :test 'equal))
  (is (set-equal (permutations "abc" :start 1)
                 '("bc" "cb")
                 :test 'equal))
  (is (set-equal (permutations "abc" :end 2)
                 '("ab" "ba")
                 :test 'equal)))


;;--------------------------------------------------------------------
;; combinations
;;--------------------------------------------------------------------

(test ?combinations.error
  (signals type-error (combinations :foo '(0 1 2)))
  (signals type-error (combinations 2 :foo))
  (signals type-error (combinations 2 '(0 1 2) :start -1))
  (signals type-error (combinations 2 '(0 1 2) :start nil))
  (signals type-error (combinations 2 '(0 1 2) :end -1))
  (signals type-error (combinations 2 '(0 1 2) :end :foo))
  (signals simple-error (combinations 2 '(0 1 2) :start 2 :end 1)))


(test ?combinations.list
  (is-equal (combinations 0 '())
            '(()))
  (is (set-equal (combinations 2 '(0 1 2))
                 '((0 1) (0 2) (1 2))
                 :test 'equal))
  (is (set-equal (combinations 2 '(0 1 2) :start 1)
                 '((1 2))
                 :test 'equal))
  (is (set-equal (combinations 2 '(0 1 2) :end 2)
                 '((0 1))
                 :test 'equal)))

;; TODO: refactor, order
#+nil
(test ?combinations.vector
  (is-equalp (combinations 0 #())
             '(#()))
  (is (set-equal (combinations 2 #(0 1 2))
                 '(#(0 1) #(0 2) #(1 2))
                 :test 'equalp))
  (is (set-equal (combinations 2 #(0 1 2) :start 1)
                 '(#(1 2))
                 :test 'equalp))
  (is (set-equal (combinations 2 #(0 1 2) :end 2)
                 '(#(0 1))
                 :test 'equalp)))


;;--------------------------------------------------------------------
;; inits
;;--------------------------------------------------------------------

(test ?inits.error
  (signals type-error (inits :foo))
  (signals type-error (inits '(0 1 2) :key #()))
  (signals type-error (inits '(0 1 2) :count -1))
  (signals type-error (inits '(0 1 2) :count #()))
  (signals type-error (inits '(0 1 2) :start -1))
  (signals type-error (inits '(0 1 2) :start nil))
  (signals type-error (inits '(0 1 2) :end -1))
  (signals type-error (inits '(0 1 2) :end #()))
  (signals simple-error (inits '(0 1 2) :start 42 :end 24)))

(test ?inits.list
  (is-equal (inits '(0 1 2 3 4))
            '(() (0) (0 1) (0 1 2) (0 1 2 3) (0 1 2 3 4)))
  (is-equal (inits '(0 1 2 3 4) :remove-empty-subseqs t)
            '((0) (0 1) (0 1 2) (0 1 2 3) (0 1 2 3 4)))
  (is-equal (inits '(0 1 2 3 4) :key '1+)
            '(() (1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5)))
  (is-equal (inits '(0 1 2 3 4) :count 3)
            '(() (0) (0 1)))
  (is-equal (inits '(0 1 2 3 4) :remove-empty-subseqs t
                                :count 3)
            '((0) (0 1) (0 1 2)))
  (is-equal (inits '(0 1 2 3 4) :start 2)
            '(() (2) (2 3) (2 3 4)))
  (is-equal (inits '(0 1 2 3 4) :end 2)
            '(() (0) (0 1)))
  (is-equal (inits '(0 1 2 3 4) :start 2 :end 4)
            '(() (2) (2 3)))
  (is-equal (inits '(0 1 2 3 4) :start 2 :end 2)
            '(())))

(test ?inits.vector
  (is-equalp (inits #(0 1 2 3 4))
             '(#() #(0) #(0 1) #(0 1 2) #(0 1 2 3) #(0 1 2 3 4)))
  (is-equalp (inits #(0 1 2 3 4) :remove-empty-subseqs t)
             '(#(0) #(0 1) #(0 1 2) #(0 1 2 3) #(0 1 2 3 4)))
  (is-equalp (inits #(0 1 2 3 4) :key '1+)
             '(#() #(1) #(1 2) #(1 2 3) #(1 2 3 4) #(1 2 3 4 5)))
  (is-equalp (inits #(0 1 2 3 4) :count 3)
             '(#() #(0) #(0 1)))
  (is-equalp (inits #(0 1 2 3 4) :remove-empty-subseqs t
                                 :count 3)
             '(#(0) #(0 1) #(0 1 2)))
  (is-equalp (inits #(0 1 2 3 4) :start 2)
             '(#() #(2) #(2 3) #(2 3 4)))
  (is-equalp (inits #(0 1 2 3 4) :end 2)
             '(#() #(0) #(0 1)))
  (is-equalp (inits #(0 1 2 3 4) :start 2 :end 4)
             '(#() #(2) #(2 3)))
  (is-equalp (inits #(0 1 2 3 4) :start 2 :end 2)
             '(#())))


(test ?inits.bit-vector
  (is-equal (inits #*101010)
             '(#* #*1 #*10 #*101 #*1010 #*10101 #*101010))
  (is-equal (inits #*101010 :remove-empty-subseqs t)
             '(#*1 #*10 #*101 #*1010 #*10101 #*101010))
  (is-equal (inits #*101010 :count 3)
             '(#* #*1 #*10))
  (is-equal (inits #*101010 :remove-empty-subseqs t
                             :count 3)
             '(#*1 #*10 #*101))
  (is-equal (inits #*101010 :key (lambda (b) (logxor b 1)))
             '(#* #*0 #*01 #*010 #*0101 #*01010 #*010101))
  (is-equal (inits #*101010 :start 2)
             '(#* #*1 #*10 #*101 #*1010))
  (is-equal (inits #*101010 :end 2)
             '(#* #*1 #*10))
  (is-equal (inits #*101010 :start 2 :end 3)
             '(#* #*1))
  (is-equal (inits #*101010 :start 2 :end 2)
             '(#*)))


(test ?inits.string
  (is-equal (inits "string")
            '("" "s" "st" "str" "stri" "strin" "string"))
  (is-equal (inits "string" :remove-empty-subseqs t)
            '("s" "st" "str" "stri" "strin" "string"))
  (is-equal (inits "string" :count 3)
            '("" "s" "st"))
  (is-equal (inits "string" :remove-empty-subseqs t
                            :count 3)
            '("s" "st" "str"))
  (is-equal (inits "string" :key (lambda (c) (code-char (1+ (char-code c)))))
            '("" "t" "tu" "tus" "tusj" "tusjo" "tusjoh"))
  (is-equal (inits "string" :start 2)
            '("" "r" "ri" "rin" "ring"))
  (is-equal (inits "string" :end 2)
            '("" "s" "st"))
  (is-equal (inits "string" :start 2 :end 3)
            '("" "r"))
  (is-equal (inits "string" :start 2 :end 2)
            '("")))


;;--------------------------------------------------------------------
;; tails
;;--------------------------------------------------------------------

(test ?tails.error
  (signals type-error (tails :foo))
  (signals type-error (tails '(0 1 2) :key #()))
  (signals type-error (tails '(0 1 2) :count -1))
  (signals type-error (tails '(0 1 2) :count #()))
  (signals type-error (tails '(0 1 2) :start -1))
  (signals type-error (tails '(0 1 2) :start nil))
  (signals type-error (tails '(0 1 2) :end -1))
  (signals type-error (tails '(0 1 2) :end #()))
  (signals simple-error (tails '(0 1 2) :start 42 :end 24)))


(test ?tails.list
  (is-equal (tails '(0 1 2 3 4))
            '((0 1 2 3 4) (1 2 3 4) (2 3 4) (3 4) (4) ()))
  (is-equal (tails '(0 1 2 3 4) :remove-empty-subseqs t)
            '((0 1 2 3 4) (1 2 3 4) (2 3 4) (3 4) (4)))
  (is-equal (tails '(0 1 2 3 4) :count 3)
            '((0 1 2 3 4) (1 2 3 4) (2 3 4)))  
  (is-equal (tails '(0 1 2 3 4) :key '1+)
            '((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5) ()))
  (is-equal (tails '(0 1 2 3 4) :start 2)
            '((2 3 4) (3 4) (4) ()))
  (is-equal (tails '(0 1 2 3 4) :end 2)
            '((0 1) (1) ()))
  (is-equal (tails '(0 1 2 3 4) :start 2 :end 3)
            '((2) ()))
  (is-equal (tails '(0 1 2 3 4) :start 2 :end 2)
            '(())))


(test ?tails.vector
  (is-equalp (tails #(0 1 2 3 4))
             '(#(0 1 2 3 4) #(1 2 3 4) #(2 3 4) #(3 4) #(4) #()))
  (is-equalp (tails #(0 1 2 3 4) :remove-empty-subseqs t)
             '(#(0 1 2 3 4) #(1 2 3 4) #(2 3 4) #(3 4) #(4)))
  (is-equalp (tails #(0 1 2 3 4) :count 3)
             '(#(0 1 2 3 4) #(1 2 3 4) #(2 3 4)))
  (is-equalp (tails #(0 1 2 3 4) :key '1+)
             '(#(1 2 3 4 5) #(2 3 4 5) #(3 4 5) #(4 5) #(5) #()))
  (is-equalp (tails #(0 1 2 3 4) :start 2)
             '(#(2 3 4) #(3 4) #(4) #()))
  (is-equalp (tails #(0 1 2 3 4) :end 2)
             '(#(0 1) #(1) #()))
  (is-equalp (tails #(0 1 2 3 4) :start 2 :end 4)
             '(#(2 3) #(3) #()))
  (is-equalp (tails #(0 1 2 3 4) :start 2 :end 2)
             '(#())))


(test ?tails.bit-vector
  (is-equal (tails #*101010)
             '(#*101010 #*01010 #*1010 #*010 #*10 #*0 #*))
  (is-equal (tails #*101010 :remove-empty-subseqs t)
             '(#*101010 #*01010 #*1010 #*010 #*10 #*0))
  (is-equal (tails #*101010 :count 3)
             '(#*101010 #*01010 #*1010))
  (is-equal (tails #*101010 :key (lambda (b) (logxor b 1)))
             '(#*010101 #*10101 #*0101 #*101 #*01 #*1 #*))
  (is-equal (tails #*101010 :start 2)
             '(#*1010 #*010 #*10 #*0 #*))
  (is-equal (tails #*101010 :end 2)
             '(#*10 #*0 #*))
  (is-equal (tails #*101010 :start 2 :end 3)
             '(#*1 #*))
  (is-equal (tails #*101010 :start 2 :end 2)
             '(#*)))


(test ?tails.string
  (is-equal (tails "string")
            '("string" "tring" "ring" "ing" "ng" "g" ""))
  (is-equal (tails "string" :remove-empty-subseqs t)
            '("string" "tring" "ring" "ing" "ng" "g"))
  (is-equal (tails "string" :count 3)
            '("string" "tring" "ring"))
  (is-equal (tails "string" :key (lambda (c) (code-char (1+ (char-code c)))))
            '("tusjoh" "usjoh" "sjoh" "joh" "oh" "h" ""))
  (is-equal (tails "string" :start 2)
            '("ring" "ing" "ng" "g" ""))
  (is-equal (tails "string" :end 2)
            '("st" "t" ""))
  (is-equal (tails "string" :start 2 :end 3)
            '("r" ""))
  (is-equal (tails "string" :start 2 :end 2)
            '("")))


;;--------------------------------------------------------------------
;; scan
;;--------------------------------------------------------------------

(test ?scan.error
  (signals type-error (scan #() '(0 1 2)))
  (signals type-error (scan 'identity :foo))
  (signals type-error (scan 'identity '(0 1 2) :key #()))
  (signals type-error (scan 'identity '(0 1 2) :start -1))
  (signals type-error (scan 'identity '(0 1 2) :start :foo))
  (signals type-error (scan 'identity '(0 1 2) :end -1))
  (signals type-error (scan 'identity '(0 1 2) :end :foo))
  (signals simple-error (scan 'identity '(0 1 2) :start 42 :end 24)))


(test ?scan.limit-case
  (is-equal (scan (lambda (&rest args) (declare (ignore args)) 42)
                  '())
            '(42))
  (is-equal (scan (lambda (a v) (declare (ignore a v)) 42)
                  '()
                  :initial-value 1)
            '(1))
  (is-equal (scan (lambda (a v) (declare (ignore a v)) 42)
                  '(1))
            '(1))
  (is-equal (scan (lambda (&rest args) (declare (ignore args)) 42)
                  #())
            '(42))
  (is-equal (scan (lambda (a v) (declare (ignore a v)) 42)
                  #()
                  :initial-value 1)
            '(1))
  (is-equal (scan (lambda (a v) (declare (ignore a v)) 42)
                  #(1))
            '(1)))


(test ?scan.list
  (is-equal (scan 'list '(0 1 2 3))
            '(0 (0 1) ((0 1) 2) (((0 1) 2) 3)))
  (is-equal (scan 'list '(0 1 2 3) :initial-value -1)
            '(-1 (-1 0) ((-1 0) 1) (((-1 0) 1) 2) ((((-1 0) 1) 2) 3)))
  (is-equal (scan 'list '(0 1 2 3) :key '1+)
            '(1 (1 2) ((1 2) 3) (((1 2) 3) 4)))
  (is-equal (scan 'list '(0 1 2 3) :key '1+ :initial-value -1)
            '(-1 (-1 1) ((-1 1) 2) (((-1 1) 2) 3) ((((-1 1) 2) 3) 4)))
  (is-equal (scan 'list '(0 1 2 3) :start 2)
            '(2 (2 3)))
  (is-equal (scan 'list '(0 1 2 3) :end 2)
            '(0 (0 1)))
  (is-equal (scan 'list '(0 1 2 3) :start 2 :end 3)
            '(2))
  (is-equal (scan 'list '(0 1 2 3) :start 2 :end 2)
            '(NIL))

  (is-equal (scan 'list '(0 1 2 3) :from-end t)
            '((0 (1 (2 3))) (1 (2 3)) (2 3) 3))
  (is-equal (scan 'list '(0 1 2 3) :from-end t :initial-value 4)
            '((0 (1 (2 (3 4)))) (1 (2 (3 4))) (2 (3 4)) (3 4) 4))
  (is-equal (scan 'list '(0 1 2 3) :from-end t :key '1+)
            '((1 (2 (3 4))) (2 (3 4)) (3 4) 4))
  (is-equal (scan 'list '(0 1 2 3) :from-end t :key '1+ :initial-value 5)
            '((1 (2 (3 (4 5)))) (2 (3 (4 5))) (3 (4 5)) (4 5) 5))
  (is-equal (scan 'list '(0 1 2 3) :from-end t :start 2)
            '((2 3) 3))
  (is-equal (scan 'list '(0 1 2 3) :from-end t :end 2)
            '((0 1) 1))
  (is-equal (scan 'list '(0 1 2 3) :from-end t :start 2 :end 4)
            '((2 3) 3))
  (is-equal (scan 'list '(0 1 2 3) :from-end t :start 2 :end 2)
            '(NIL)))


(test ?scan.vector
  (is-equal (scan 'list #(0 1 2 3))
            '(0 (0 1) ((0 1) 2) (((0 1) 2) 3)))
  (is-equal (scan 'list #(0 1 2 3) :initial-value -1)
            '(-1 (-1 0) ((-1 0) 1) (((-1 0) 1) 2) ((((-1 0) 1) 2) 3)))
  (is-equal (scan 'list #(0 1 2 3) :key '1+)
            '(1 (1 2) ((1 2) 3) (((1 2) 3) 4)))
  (is-equal (scan 'list #(0 1 2 3) :key '1+ :initial-value -1)
            '(-1 (-1 1) ((-1 1) 2) (((-1 1) 2) 3) ((((-1 1) 2) 3) 4)))
  (is-equal (scan 'list #(0 1 2 3) :start 2)
            '(2 (2 3)))
  (is-equal (scan 'list #(0 1 2 3) :end 2)
            '(0 (0 1)))
  (is-equal (scan 'list #(0 1 2 3) :start 2 :end 3)
            '(2))
  (is-equal (scan 'list #(0 1 2 3) :start 2 :end 2)
            '(NIL))

  (is-equal (scan 'list #(0 1 2 3) :from-end t)
            '((0 (1 (2 3))) (1 (2 3)) (2 3) 3))
  (is-equal (scan 'list #(0 1 2 3) :from-end t :initial-value 4)
            '((0 (1 (2 (3 4)))) (1 (2 (3 4))) (2 (3 4)) (3 4) 4))
  (is-equal (scan 'list #(0 1 2 3) :from-end t :key '1+)
            '((1 (2 (3 4))) (2 (3 4)) (3 4) 4))
  (is-equal (scan 'list #(0 1 2 3) :from-end t :key '1+ :initial-value 5)
            '((1 (2 (3 (4 5)))) (2 (3 (4 5))) (3 (4 5)) (4 5) 5))

  (is-equal (scan 'list #(0 1 2 3) :from-end t :start 2)
            '((2 3) 3))
  (is-equal (scan 'list #(0 1 2 3) :from-end t :end 2)
            '((0 1) 1))
  (is-equal (scan 'list #(0 1 2 3) :from-end t :start 2 :end 4)
            '((2 3) 3))
  (is-equal (scan 'list #(0 1 2 3) :from-end t :start 2 :end 2)
            '(NIL)))


;;====================================================================
