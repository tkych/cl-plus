;;;; cl-plus/test/core/function.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for Function 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.function
  (:export #:?function)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:all)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:cl-plus.src.readtables
                #:cl+)
  (:import-from #:alexandria
                #:compose)
  (:import-from #:cl-plus.src.core.function
                #:$  #:$*
                #:<< #:>>
                #:$>
                #:&  #:v)
  (:import-from #:cl-plus.src.core.sequence
                #:repeat)
  (:import-from #:cl-plus.src.core.sequens
                #:drop)
  (:import-from #:cl-plus.src.core.buxis
                #:map* #:reduce*))

(in-package #:cl-plus.test.core.function)

(def-suite ?function :in all)
(in-suite ?function)

(in-readtable cl+)


;;--------------------------------------------------------------------
;; Function Applications: $ and $*
;;--------------------------------------------------------------------

(test ?$
  (is (equal (macroexpand-1 '($ f))
             '(funcall #'f)))

  (is (equal (macroexpand-1 '($ (lambda () 42)))
             '(funcall (lambda () 42))))

  (is (equal (macroexpand-1 '($ f a b c))
             '(funcall #'f a b c)))

  (is (equal (macroexpand-1 '($ (lambda () 42) a b c))
             '(funcall (lambda () 42) a b c)))

  (is (equal (macroexpand-1 '($ f a b c $ g d e))
             '(funcall #'f a b c (funcall #'g d e))))

  (is (equal (macroexpand-1 '($ f a b c $* g d e))
             '(funcall #'f a b c (apply #'g d e))))

  (is (equal (macroexpand-1 '($ f a b c $* (lambda () 42) d e))
             '(funcall #'f a b c (apply (lambda () 42) d e))))

  (is (equal (funcall ($ cons 1 $) '(2 3 4 5))
             '(1 2 3 4 5)))

  (is (equal (funcall ($ list* 1 2 $*) 3 4 5)
             '(1 2 3 4 5)))

  ;; repeat 2 . product . map (*3) $ zipWith max [1,2] [4,5]
  (is (equal ($ repeat 2 $* * $ mapcar ^(* @ 3) $ mapcar 'max '(1 2) '(4 5))
             '(180 180)))

  (is (equal ($ (<< repeat 2 _ ^($* * @) _ mapcar ^(* @ 3)) $ mapcar 'max '(1 2) '(4 5))
             '(180 180))))


(test ?$*
  (is (equal (macroexpand-1 '($* f))
             '(apply #'f nil)))

  (is (equal (macroexpand-1 '($* (lambda () 42)))
             '(apply (lambda () 42) nil)))

  (is (equal (macroexpand-1 '($* f a b c))
             '(apply #'f a b c)))

  (is (equal (macroexpand-1 '($* (lambda () 42) a b c))
             '(apply (lambda () 42) a b c)))

  (is (equal (macroexpand-1 '($* f a b c $ g d e))
             '(apply #'f a b c (funcall #'g d e))))

  (is (equal (macroexpand-1 '($* f a b c $ (lambda () 42) d e))
             '(apply #'f a b c (funcall (lambda () 42) d e))))

  (is (equal (macroexpand-1 '($* f a b c $* g d e))
             '(apply #'f a b c (apply #'g d e))))

  (is (equal (macroexpand-1 '($* f a b c $* (lambda () 42) d e))
             '(apply #'f a b c (apply (lambda () 42) d e))))

  (is (equal (funcall ($* list 1 $) '(2 3 4 5))
             '(1 2 3 4 5)))

  (is (equal (funcall ($* list* 1 2 $*) 3 4 5)
             '(1 2 3 4 . 5)))

  ;; sum . repeat 5 $ max 6.7 6.8
  (is (= ($* + $ repeat 5 $ max 6.7 6.8)
         34.0))

  (is (= ($* + $ (<< repeat 5 _ max) 6.7 6.8)
         34.0)))


;;--------------------------------------------------------------------
;; Compose Functions: << and >>
;;--------------------------------------------------------------------

(test ?<<
  (is (equal (macroexpand-1 '(<< h _ g _ f))
             '(compose #'H #'G #'F)))

  (is (equal (mapcar (<< - _ abs)
                     '(5 -3 -6 7 -3 2 -19 -24))
             '(-5 -3 -6 -7 -3 -2 -19 -24)))

  (is (equal (mapcar (<< write-to-string _ * 3 _ + 12)
                     '(1 2 3))
             '("39" "42" "45")))

  (is (equal (mapcar (<< write-to-string _ (<< * 3 _ (<< + 12)))
                     '(1 2 3))
             '("39" "42" "45")))

  (is (equal (mapcar (<< (>> write-to-string) _ (>> + 12 _ (<< * 3)))
                     '(1 2 3))
             '("39" "42" "45")))

  (is (equal (mapcar (<< abs _ - _ * 33 _ - 100 _ max 50)
                     '(42 78 55 28 13 95))
             '(1650 726 1485 1650 1650 165)))
  
  (is (equal (map* 'list (<< - _ reduce* '+ _ drop 1)
                   (list #[1..5] #[3..6] #[1..7]))
             '(-14 -15 -27))))

(test ?>>
  (is (equal (macroexpand-1 '(<< f _ g _ h))
             '(compose #'F #'G #'H)))

  (is (equal (mapcar (>> abs _ -)
                     '(5 -3 -6 7 -3 2 -19 -24))
             '(-5 -3 -6 -7 -3 -2 -19 -24)))

  (is (equal (mapcar (>> + 12 _ * 3 _ write-to-string)
                     '(1 2 3))
             '("39" "42" "45")))

  (is (equal (mapcar (>> + 12 _ (>> * 3 _ (>> write-to-string)))
                     '(1 2 3))
             '("39" "42" "45")))

  (is (equal (mapcar (>> (<< + 12) _ (<< write-to-string _ (>> * 3)))
                     '(1 2 3))
             '("39" "42" "45")))

  (is (equal (mapcar (>> max 50 _ - 100 _ * 33 _ - _ abs)
                     '(42 78 55 28 13 95))
             '(1650 726 1485 1650 1650 165))))


;;--------------------------------------------------------------------
;; Chain: $>
;;--------------------------------------------------------------------

(test ?$>
  (is (equal (macroexpand-1 '($> data _ fn0 a0 _ fn1 _ fn2))
             '($ (>> fn0 a0 _ fn1 _ fn2)
               data))))


;;--------------------------------------------------------------------
;; Conjoin: &
;;--------------------------------------------------------------------

(test ?&
  (is (equal (mapcar (& integerp _ plusp)
                     '(-44 55 7 6.7 -2 -33 -55.5))
             '(NIL T T NIL NIL NIL NIL)))

  (is (equal (mapcar (& integerp & plusp)
                     '(-44 55 7 6.7 -2 -33 -55.5))
             '(NIL T T NIL NIL NIL NIL)))

  (is (equal (mapcar (& integerp _ < -10)
                     '(-44 55 7 6.7 -2 -33 -55.5))
             '(T NIL NIL NIL NIL T NIL)))

  (is (equal (mapcar (& integerp & < -10)
                     '(-44 55 7 6.7 -2 -33 -55.5))
             '(T NIL NIL NIL NIL T NIL)))

  (is (equal (mapcar (& integerp _ (v plusp _ < -10))
                     '(-44 55 7 6.7 -2 -33 -55.5))
             '(T T T NIL NIL T NIL)))

  (is (equal (mapcar (& integerp & (v plusp v < -10))
                     '(-44 55 7 6.7 -2 -33 -55.5))
             '(T T T NIL NIL T NIL))))


;;--------------------------------------------------------------------
;; Disjoin: v
;;--------------------------------------------------------------------

(test ?v
  (is (equal (mapcar (v integerp _ < -10)
                     '(-44 55 7 6.7 -2 -33 -55.5))
             '(T T T NIL T T T)))

  (is (equal (mapcar (v integerp v < -10)
                     '(-44 55 7 6.7 -2 -33 -55.5))
             '(T T T NIL T T T)))

  (is (equal (mapcar (v integerp _ plusp)
                     '(-44 55 7 6.7 -2 -33 -55.5))
             '(T T T T T T NIL)))

  (is (equal (mapcar (v integerp v plusp)
                     '(-44 55 7 6.7 -2 -33 -55.5))
             '(T T T T T T NIL)))

  (is (equal (mapcar (v integerp _ (& minusp _ < -10))
                     '(-44 55 7 6.7 -2 -33 -55.5))
             '(T T T NIL T T T)))

  (is (equal (mapcar (v integerp v (& minusp & < -10))
                     '(-44 55 7 6.7 -2 -33 -55.5))
             '(T T T NIL T T T))))


;;====================================================================
