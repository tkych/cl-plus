Version: 0.0.54 (alpha)


((CL+)) : Interface for Prototyping
===================================

> The use of rapid prototyping enables early review and earlier discovery of problems.
> The sooner you get feedback, the more time you have to fix things and the higher the quality of the final product.
>> -- Kent Pitman, [Accelerationg Hindsight: Lisp as a Vehicle for Rapid Prototyping](http://www.nhplace.com/kent/PS/Hindsight.html)

> Lisp is still the best prototyping language. We need to push this forward.
>> -- Richard P. Gabriel (1991), [Lisp: Good News, Bad News, How to Win Big](http://www.dreamsongs.com/WIB.html)

> What programmers in a hundred years will be looking for, most of all, is a language where
> you can throw together an unbelievably inefficient version 1 of a program with the least possible effort. 
>> -- Paul Graham, [The Hundred-Year Language](http://paulgraham.com/hundred.html)


Introduction
------------

CL+ is an Interface on Common Lisp for Rapid Prototyping.
The goal of this project is __not__ to create a new lisp dialect, __nor__ to become a new utility collection.
Instead, the goal of CL+ is a modern interface on CL for making inefficient ver.1 program, quickly.
Removing CL+ layer on CL, we are able to optimize inefficient ver.1 program to efficient ver.2 program.

We need your feedback for improvement.


Features
--------

 1. Read-macros: ^, #{} and #[].
 2. Abstract data types: *SEQUENS*, *TABULA* and *BUXIS*. 
 3. Lazy data structures: *PROMISE*, *PIPE*, *LAZY-SEQUENCE* and *LAZY-FLOW*.
 4. Sub-Packages:
     - Core: *LAZY-SEQUENCE*, *SEQUENS*, *TABULA*, *BUXIS*, ...
     - Library: *CDR-5*, *CDR-8*, *CLRFI-1*, *SRFI-45* and *SRFI-41*, ...
 5. Extensible Polymorphic Operators: *TO*, *REF*, *ADD*, *COPY*, *EMPTYP*, *SIZE*, *EQUALS* and *COMPARE*.
 6. Utilities for Functional Programming Style: $, $*, <<, >>, &, v.

For more details, please see [cl-plus/doc/features.md](https://github.com/tkych/cl-plus/blob/master/doc/features.md), [cl-plus/doc/compare-with-cl.md](https://github.com/tkych/cl-plus/blob/master/doc/compare-with-cl.md).


Examples
--------

#### ^shorthand-lambda:

```lisp
 ;; Church style:
 * (mapcar ^xy(+ x y)      '(1 2 3) '(41 40 39)) => (42 42 42)
 * (mapcar ^x0.y0(+ x0 y0) '(1 2 3) '(41 40 39)) => (42 42 42)

 ;; Hickey style:
 * (funcall ^(if (< 100 @)
                 (- @ 10)
                 (@me (@me (+ @ 11))))
            (random 102))
   => 91

 ;; Lazy-Sequence:
 * (induce ^xyz(+ x y z) 0 0 1) => #[0 0 1 ..]
 * (take 10 *)                  => (0 0 1 1 2 4 7 13 24 44)
 * **                           => #[0 0 1 1 2 4 7 13 24 44 ..]
```


#### #[read macro for lazy-sequence]:

```lisp
 * (take 10 #[0 ..])                 => (0 1 2 3 4 5 6 7 8 9)
 * (take 10 #[0 3 ..])               => (0 3 6 9 12 15 18 21 24 27)
 * (take 10 #[1 2 4 ..])             => (1 2 4 8 16 32 64 128 256 512)
 * (take :all #[1 .. 10])            => (1 2 3 4 5 6 7 8 9 10)
 * (take :all #[#\a .. #\z] 'string) => "abcdefghijklmnopqrstuvwxyz"
 * (reduce* '+ #[1..100])            => 5050
```


#### LAZY-FOR/LFOR, lazy-sequence facility:

```lisp
 * (take 10 (lazy-for x from 1 by 2)) => (1 3 5 7 9 11 13 15 17 19)

 * (take 10 (lazy-for (* x x)
              (x from 1 by 2 when ^(zerop (mod @ 3)))))
   => (9 81 225 441 729 1089 1521 2025 2601 3249)

 ;; LFOR is just an alias for LAZY-FOR:
 * (take 6 (lfor (cons x y)
             (x replicate :foo :bar :baz :quux)
             (y in #[1 2 4 ..])))
   => ((:FOO . 1) (:BAR . 2) (:BAZ . 4) (:QUUX . 8) (:FOO . 16) (:BAR . 32))
```


#### #{read macro for hash-table}:

```lisp
 * #{:foo 0 :bar 1 :baz 2}     => #{:foo 0 :bar 1 :baz}
 * (hash-table-test #{:foo 0})  => EQUAL
 * (hash-table-test #2{:foo 0}) => EQ
 * (hash-table-test #3{:foo 0}) => EQL
 * (hash-table-test #5{:foo 0}) => EQUAL
 * (hash-table-test #6{:foo 0}) => EQUALP
```


#### *-Suffix-Functions for BUXIS (hash-table, sequence, lazy-sequence and array):

```lisp
 * (map* t '1+ #{:foo 0 :bar 1 :baz 2 :quux 3})
   => #{:BAR 2 :BAZ 3 :QUUX 4 :FOO 1}

 * (reduce* '+ #{:foo 0 :bar 1 :baz 2 :quux 3})
   => 6

 * (take :all (remove-if* 'evenp #[0..20]))
   => (1 3 5 7 9 11 13 15 17 19)

 * (remove-if* 'evenp #{:foo 0 :bar 1 :baz 2 :quux 3})
   => #{:BAR 1 :QUUX 3}

 * (collect-if* 'evenp #[0..] :from 10 :count 4)
   => (10 12 14 16)
```


#### +-Suffix-Functions for TABULA (hash-table, alist and plist): 

```lisp
 * (remove-if-not+ ^kv(and (oddp v) (= 3 (length (string k))))
                   #{:foo 0 :bar 1 :baz 2 :quux 3})
   => #{:BAR 1}

 * (remove-if-not+ ^kv(and (oddp v) (= 3 (length (string k))))
                   '(:foo 0 :bar 1 :baz 2 :quux 3))
   => (:BAR 1)

 * (remove-if-not+ ^kv(and (oddp v) (= 3 (length (string k))))
                   '((:foo . 0) (:bar . 1) (:baz . 2) (:quux . 3)))
   => ((:BAR . 1))
```


#### Extensible Polymorphic Operators: 

```lisp
 ;; ADD, generailzed CONCATENATE.
 * (add "Li" '(#\s #\p)) => "Lisp"

 * (add #{:Roma "Varro"} #{:Carthage "Hannibal"})
   => #{:ROMA "Varro"  :CARTHAGE "Hannibal"}

 * (add * #{:Roma "Scipio"})
   => #{:ROMA "Scipio" :CARTHAGE "Hannibal"}

 * (apply #'add (interpose ", " '("lazy" "delay" "eager")))
   => "lazy, delay, eager"

 ;; TO, generailzed COERCE.
 * (to 'list "string") => (#\s #\t #\r #\i #\n #\g)
 
 * (let ((txt (to 'txting #p"./README.md")))
     (subseq txt 0 (position #\Newline txt)))
   => "Last modified: 2014-06-29 11:02:27 tkych"
``` 


#### Sub-Packages:

```lisp
 ;; SRFI-45: Primitives for Expressing Iterative Lazy Algorithms.
 * (use-package :cl+srfi-45)
 * (defvar *the-answer*
     (delay (+ 1 3 5 9 11 13))) => *THE-ANSWER*
 * *the-answer*                 => [?]
 * (promisep *the-answer*)      => T
 * (force *the-answer*)         => 42
 * *the-answer*                 => [42]
```


#### Utilities for Functional Style Programming: $, $*, <<, >>, &, v:

```lisp
 ;; Function Application: $, $*
 * ($ repeat 2 $* * $ mapcar ^(* @ 3) $ mapcar 'max '(1 2) '(4 5))
   => (180 180)
 * ($* * $ mapcar ^(* @ 3) $ mapcar 'max '(1 2) '(4 5))
   => 180

 ;; Composition: <<, >>
 * (defun caesar-encode (shift text)
     (map 'string (<< code-char _ + shift _ char-code)
          text))
 * (caesar-encode 3 "VENI VIDI VICI") => "YHQL#YLGL#YLFL"
 
 * (defun caesar-decode (shift text)
     (map 'string (>> char-code _ - shift _ code-char)
          text))
 * (caesar-decode 3 "YHQL#YLGL#YLFL") => "VENI VIDI VICI"

 ;; Conjoin, Disjoin: &, v
 * (collect-if* (& integerp _ (v plusp _ < -10))
                '(42 3.9 -3 -66 -9))
   => (42 -66)
```


Depends-on
----------

 - CL-PLUS
   - alexandria
   - split-sequence
   - named-readtables
   - trivial-types

 - CL-PLUS-TEST
   - cl-plus
   - fiveam
   - trivial-timeout
   - trivial-features


Installation
------------

 0. SHELL$   `cd /path-to-quicklisp/quicklisp/local-projects/`
 1. SHELL$   `git clone https://github.com/tkych/cl-plus`
 2. CL-USER> `(ql:quickload :cl-plus)`


#### Usage:

 0. CL-USER> `(in-package :cl+user)`
 1. CL+USER> `(named-readtables:in-readtable :cl+)` ; for ^, #{}, #[]
 2. CL+USER> `(setf *print-pprint-dispatch* *cl+pprint-dispatch-table*)` ; for print #{}
 3. CL+USER> `(setf *print-pretty* t)` ; for print #{}

or

 0. CL-USER> `(use-package <CL+SUB-PACKAGE-NAME>)` ; e.g., `(use-package :cl+lazy-sequence)`

or

 0. CL-USER> `(import <CL+SUB-PACKAGE-NAME>:<SYMBOL>)` ; e.g., `(import 'cl+lazy-sequence:induce)`


#### Test:

 0. CL-USER> `(ql:quickload :cl-plus-test)`
 1. CL-USER> `(cl-plus-test:run-tests)` or `(cl+t:run-tests)`


##### Note:

If you don't need to run all tests, you can run only the particular suites by supplying the name of suite to `run-tests`.
e.g., `(cl+t:run-tests 'cl+t:?lazy-sequence 'cl+t:?lambda)`


##### Status:

 - Full tested: ccl, sbcl on linux.
 - Partial tested: clisp, ecl, abcl on linux.
 - Not tested yet: cmucl, allegro, lispworks, mocl, ...


Reference
---------

 - Please see package documentation for each file.


Author
------

 - Takaya OCHIAI  <#.(reverse "moc.liamg@lper.hcykt")>


License
-------

 - The MIT License (see cl-plus/LICENSE for details)


Copyright
---------

 - Copyright (c) 2014 Takaya OCHIAI
 - Copyright (C) André van Tonder (2003) (for srfi-45.lisp).
 - Copyright (C) Philip L. Bewig (2007)  (for srfi-41.lisp).


<!-- (cl-gfm:preview #p"~/Dropbox/cl-projects/cl-plus/README.md") -->
