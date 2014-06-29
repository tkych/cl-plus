;;;; Last modified: 2014-06-29 10:04:57 tkych

;; cl-plus/src/core/function.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;; TODO
;; ----
;;  OPTIMIZE: compose -> funcall, apply

;; BUG
;; ---

;;====================================================================
;; Utilities for Functional Programming Style
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.core.function
  (:documentation "
          (((EXPERIMENTAL)))

Utilities for Functional Programming Style
==========================================

 - Function Application (funcall): $
 - Function Application (apply):   $*
 - Left  Compose: >>
 - Right Compose: <<
 - Left  Conjoin: &
 - Left  Disjoin: v
 - Left  Chain:   $>

The purposes of the above utilities are:
   1. to reduce parentheses,
   2. to abbreviate:
       - FUNCALL
       - APPLY
       - alexandria:COMPOSE
       - alexandria:CONJOIN
       - alexandria:DISJOIN
   3. to make partial function, easily.


Examples
--------

 * (>> f _ g _ h) -> (COMPOSE #'H #'G #'F)
 * (<< h _ g _ f) -> (COMPOSE #'H #'G #'F)

 * (<< ceiling _ - _ tan _ cos _ max 50)
   -> (COMPOSE #'CEILING #'- #'TAN #'COS (LAMBDA (#:X50647) (MAX 50 #:X50647)))

 * (>> max 50 _ cos _ tan _ - _ ceiling)
   -> (COMPOSE #'CEILING #'- #'TAN #'COS (LAMBDA (#:X50648) (MAX #:X50648 50)))

 * ($ repeat 2 $* * $ mapcar ^(* @ 3) $ mapcar 'max '(1 2) '(4 5))
   -> (FUNCALL #'REPEAT 2
               (APPLY #'*
                      (FUNCALL #'MAPCAR
                               ^(* @ 3)
                               (FUNCALL #'MAPCAR 'MAX '(1 2) '(4 5)))))

 * (& integerp _ plusp) -> (CONJOIN #'INTEGERP #'PLUSP)
 * (v integerp _ plusp) -> (DISJOIN #'INTEGERP #'PLUSP)
 * (& integerp _ (v plusp _ < -10))
   -> (CONJOIN #'INTEGERP (DISJOIN #'PLUSP (LAMBDA (#:X50646) (< #:X50646 -10))))


Note
----

 - The symbols *, **, ***, +, ++, +++, /, // and /// doesn't work properly at repl.


References
----------

 [0] Simon Marlow ed.,
     Haskell 2010 Language Report,
     6.2 Strict Evaluation, p. 75
     
 [1] Gauche
     

")
  (:nicknames #:cl+function)
  (:export #:$  #:$*
           #:<< #:>>
           #:$>
           #:& #:v)
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:compose
                #:conjoin
                #:disjoin)
  (:import-from #:cl-plus.src.dev-util
                #:last1
                #:append1))

(in-package #:cl-plus.src.core.function)


;;--------------------------------------------------------------------
;; Function Applications: $ and $*
;;--------------------------------------------------------------------

(defun generate-funcall-form (expr)
  (destructuring-bind (fn . args) expr
    `(funcall ,(if (listp fn) fn `(function ,fn))
              ,@args)))

(defun generate-apply-form (expr)
  (destructuring-bind (fn . args) expr
    `(apply ,(if (listp fn) fn `(function ,fn))
            ,@(if args args '(nil))))) ; for symmetry: ($ fn) <-> ($* fn)

(declaim (function parse-dollar parse-dollar*))

(defun parse-dollar (expr &optional lambda-arg)
  (labels
      ((rec (expr acc)
         (if (null expr)
             (generate-funcall-form (nreverse acc))
             (destructuring-bind (arg . args) expr
               (case arg
                 ($  (if (null args)
                         (generate-funcall-form (nreverse (cons lambda-arg acc)))
                         (generate-funcall-form (nreconc acc (list (parse-dollar args lambda-arg))))))
                 ($* (if (null args)
                         (generate-funcall-form (nreverse (cons lambda-arg acc)))
                         (generate-funcall-form (nreconc acc (list (parse-dollar* args lambda-arg))))))
                 (t  (rec args (cons arg acc))))))))
    (rec expr '())))

(defun parse-dollar* (expr &optional lambda-arg)
  (labels
      ((rec (expr acc)
         (if (null expr)
             (generate-apply-form (nreverse acc))
             (destructuring-bind (arg . args) expr
               (case arg
                 ($  (if (null args)
                         (generate-apply-form (nreverse (cons lambda-arg acc)))
                         (generate-apply-form (nreconc acc (list (parse-dollar args lambda-arg))))))
                 ($* (if (null args)
                         (generate-apply-form (nreverse (cons lambda-arg acc)))
                         (generate-apply-form (nreconc acc (list (parse-dollar* args lambda-arg))))))
                 (t  (rec args (cons arg acc))))))))
    (rec expr '())))

(defmacro $ (function &rest arguments)
  (case (last1 arguments)
    ($  (with-gensyms (arg)
          `(lambda (,arg)
             ,(parse-dollar (cons function arguments) arg))))
    ($* (with-gensyms (args)
          `(lambda (&rest ,args)
             ,(parse-dollar (cons function arguments) args))))
    (t  (parse-dollar (cons function arguments)))))

(defmacro $* (function &rest arguments)
  (case (last1 arguments)
    ($  (with-gensyms (arg)
          `(lambda (,arg)
             ,(parse-dollar* (cons function arguments) arg))))
    ($* (with-gensyms (args)
          `(lambda (&rest ,args)
             ,(parse-dollar* (cons function arguments) args))))
    (t  (parse-dollar* (cons function arguments)))))


(setf (documentation '$ 'function) "
     (((EXPERIMENTAL)))

Function Applications: $ and $*
===============================

 - Function Application (funcall): $ 
 - Function Application (apply):   $*


Grammer
-------

  ({<app> <clause>}+ [<app>])

  where
    <app>      ::= $|$*
    <fn>       ::= <symbol>|<lambda-form>
    <argument> ::= <object>
    <clause>   ::= <fn> <argument>*


Examples
--------

 * ($  + 1 2 3)    -> (FUNCALL #'+ 1 2 3)
 * ($* + 1 2 '(3)) -> (APPLY #'+ 1 2 '(3))
 * ($  + 1 2 3 $)  -> (LAMBDA (ARG) (FUNCALL #'+ 1 2 3 ARG))
 * ($  + 1 2 3 $*) -> (LAMBDA (&REST ARGS) (FUNCALL #'+ 1 2 3 ARGS))
 * ($* + 1 2 3 $)  -> (LAMBDA (ARG) (APPLY #'+ 1 2 3 ARG))
 * ($* + 1 2 3 $*) -> (LAMBDA (&REST ARGS) (APPLY #'+ 1 2 3 ARGS))

 * ($* +
       $ collect-if* ^(> @ 10)
       $ map* 'list ^(* @ 2) #[2..10])

 * (apply #'+
          (collect-if* ^(> @ 10)
                       (map* 'list ^(* @ 2) #[2..10])))

 * (reduce #'+
           (remove-if-not (lambda (x) (> x 10))
                          (mapcar (lambda (y) (* y 2))
                                  (loop for i from 2 to 10 collect i))))
")

;; HASKELL> sum $ filter (> 10) $ map (*2) [2..10]

;; (loop :for i :from 2 :to 10
;;       :for x := (* i 2)
;;       :when (> x 10) :sum x)

(setf (documentation '$* 'function)
      (documentation '$  'function))


;;--------------------------------------------------------------------
;; Compose Functions: << and >>
;;--------------------------------------------------------------------

(defparameter *clause-delimiter-symbol-name* "_")

(defun clause-delimiter-symbol-p (x &optional another-delimiter-symbol-name)
  (and (symbolp x)
       (or (string-equal x *clause-delimiter-symbol-name*)
           (and another-delimiter-symbol-name
                (string-equal x another-delimiter-symbol-name)))))

(defun generate-right-function-form (xs)
  (destructuring-bind (fn . args) xs
    (if (null args)
        (if (listp fn) fn `(function ,fn))
        (with-gensyms (x)
          (if (listp fn)
              (once-only (fn)
                `(lambda (,x) (funcall ,fn ,@args ,x)))
              `(lambda (,x) (,fn ,@args ,x)))))))

(defmacro << (&rest function-clauses)
  (labels ((rec (xs buff acc)
             (if (null xs)
                 `(compose ,@(nreverse acc)
                           ,(generate-right-function-form (nreverse buff)))
                 (destructuring-bind (arg . args) xs
                   (if (clause-delimiter-symbol-p arg)
                       (rec args '() (cons (generate-right-function-form (nreverse buff))
                                           acc))
                       (rec args (cons arg buff) acc))))))
    (if (notany #'clause-delimiter-symbol-p function-clauses)
        (generate-right-function-form function-clauses)
        (rec function-clauses '() '()))))

(defun generate-left-function-form (xs)
  (destructuring-bind (fn . args) xs
    (if (null args)
        (if (listp fn) fn `(function ,fn))
        (with-gensyms (x)
          (if (listp fn)
              (once-only (fn)
                `(lambda (,x) (funcall ,fn ,x ,@args)))
              `(lambda (,x) (,fn ,x ,@args)))))))

(defmacro >> (&rest function-clauses)
  (labels ((rec (xs buff acc)
             (if (null xs)
                 `(compose ,(generate-left-function-form (nreverse buff))
                           ,@acc)
                 (destructuring-bind (arg . args) xs
                   (if (clause-delimiter-symbol-p arg)
                       (rec args '() (cons (generate-left-function-form (nreverse buff))
                                           acc))
                       (rec args (cons arg buff) acc))))))
    (if (notany #'clause-delimiter-symbol-p function-clauses)
        (generate-left-function-form function-clauses)
        (rec function-clauses '() '()))))

(setf (documentation '<< 'function) "
     (((EXPERIMENTAL)))

Function Composition: <<, >>
=============================

 - Right Compose: <<
 - Left  Compose: >>

Grammar
-------

  ({<<|>>} <clause> {<delimiter> <clause>}*)

  where
    <fn>        ::= <symbol>
    <argument>  ::= <object>
    <clause>    ::= <fn> <argument>*
    <delimiter> ::= _


Examples
--------

 * (<< h _ g _ f) -> (COMPOSE #'H #'G #'F)
 * (>> f _ g _ h) -> (COMPOSE #'H #'G #'F)

 * (mapcar (<< to 'string _ * 3 _ + 12)
           '(1 2 3))
   => (\"39\" \"42\" \"45\")
 * (mapcar (>> + 12 _ * 3 _ ^(to 'string @))
           '(1 2 3))
   => (\"39\" \"42\" \"45\")

 * (map* t (>> char-code _ + 1 _ code-char)
           \"HAL\")
   => \"IBM\"

 * (<< ceiling _ - _ tan _ cos _ max 50)
   -> (COMPOSE #'CEILING #'- #'TAN #'COS (LAMBDA (#:X14771) (MAX 50 #:X14771)))
 * (>> max 50 _ cos _ tan _ - _ ceiling)
   -> (COMPOSE #'CEILING #'- #'TAN #'COS (LAMBDA (#:X15059) (MAX #:X15059 50)))

 * (<< repeat 2 _ * _ mapcar ^(* @ 3))
   -> (COMPOSE (LAMBDA (#:X14772) (REPEAT 2 #:X14772))
               #'*
               (LAMBDA (#:X14773) (MAPCAR ^(* @ 3) #:X14773)))

Note
----

 - ^(fn @) == (<< fn) == (>> fn) == ($ fn $)

")

(setf (documentation '>> 'function)
      (documentation '<< 'function))


;;--------------------------------------------------------------------
;; Chain: $>
;;--------------------------------------------------------------------

(defmacro $> (data &rest function-clauses)
  (destructuring-bind (delim . clauses) function-clauses
    (if (clause-delimiter-symbol-p delim)
        `($ (>> ,@clauses)
            ,data)
        (error "~S is unknown delimiter." delim))))

(setf (documentation '$> 'function) "
  (((EXPERIMENTAL)))

Chain: $>
=========

Grammar
-------

 ($> <argument> {<clause>}+)

  where
    <argument> ::= <object>
    <clause>   ::= <delimiter> <function> <argument>*
")


;;--------------------------------------------------------------------
;; Conjoin Functions: &
;;--------------------------------------------------------------------

(defparameter *conjoin-delimiter-symbol-name* "&")

(defmacro & (&rest predicate-clauses)
  (labels ((rec (xs buff acc)
             (if (null xs)
                 `(conjoin ,@(nreverse acc)
                           ,(generate-left-function-form (nreverse buff)))
                 (destructuring-bind (arg . args) xs
                   (if (clause-delimiter-symbol-p arg
                                                  *conjoin-delimiter-symbol-name*)
                       (rec args '() (cons (generate-left-function-form (nreverse buff))
                                           acc))
                       (rec args (cons arg buff) acc))))))
    (if (notany (lambda (sym)
                  (clause-delimiter-symbol-p sym *conjoin-delimiter-symbol-name*))
                predicate-clauses)
        (generate-left-function-form predicate-clauses)
        (rec predicate-clauses '() '()))))

(setf (documentation '& 'function) "
  (((EXPERIMENTAL)))

Conjoin Predicates: &
======================

 - Left Conjoin: &


Grammar
-------

  (&  <clause> {<delimiter> <clause>}*)

  where
    <predicate> ::= <symbol>
    <argument>  ::= <object>
    <clause>    ::= <predicate> <argument>*
    <delimiter> ::= &|_

Examples
--------

 * (& pred0 & pred1 33 & pred2)
 * (& pred0 _ pred1 33 _ pred2)

")


;;--------------------------------------------------------------------
;; Disjoin Functions: v
;;--------------------------------------------------------------------

(defparameter *disjoin-delimiter-symbol-name* "V")

(defmacro v (&rest predicate-clauses)
  (labels ((rec (xs buff acc)
             (if (null xs)
                 `(disjoin ,@(nreverse acc)
                           ,(generate-left-function-form (nreverse buff)))
                 (destructuring-bind (arg . args) xs
                   (if (clause-delimiter-symbol-p arg
                                                  *disjoin-delimiter-symbol-name*)
                       (rec args '() (cons (generate-left-function-form (nreverse buff))
                                           acc))
                       (rec args (cons arg buff) acc))))))
    (if (notany (lambda (sym)
                  (clause-delimiter-symbol-p sym *disjoin-delimiter-symbol-name*))
                predicate-clauses)
        (generate-left-function-form predicate-clauses)
        (rec predicate-clauses '() '()))))

(setf (documentation 'v 'function) "
  (((EXPERIMENTAL)))

Disjoin Predicates: v
======================

 - Left Disjoin: v

Grammar
-------

  (v <clause> {<delimiter> <clause>}*)

  where
    <predicate> ::= <symbol>
    <argument>  ::= <object>
    <clause>    ::= <predicate> <argument>*
    <delimiter> ::= v|_


Examples
--------
 * (v pred0 3 v pred1 v pred2)
 * (v pred0 3 _ pred1 _ pred2)
")


;; TODO:
;;--------------------------------------------------------------------
;;$= : infix mathmatical expressions
;;--------------------------------------------------------------------
;; ($= 0.302 * x + 0.141 * y + 0.169)
;; ->
;; (+ (+ (* x 0.302) (* y 0.141)) 0.169)
;; ->?
;; (+ (* x 0.302) (* y 0.141) 0.169)


;; (defmacro $= (math-expr)
;;   )


;;====================================================================
