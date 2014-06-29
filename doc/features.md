Last modified: 2014-06-29 11:26:19 tkych

<!-- (cl-gfm:preview #p"~/Dropbox/cl-projects/cl-plus/doc/features.md") -->

((CL+)) : Features
==================

 1. [Read-macros](#read-macros): `^`, `#{}` and `#[]`.
 2. [Sub-Packages](#sub-packages): *CDR-5*, *CDR-8*, *CLRFI-1*, *SRFI-45* and *SRFI-41*, ...
 3. [Lazy data structures](#lazy): *PROMISE*, *PIPE*, *LAZY-SEQUENCE* and *LAZY-FLOW*.
 4. [Abstract data types](#atype): *SEQUENS*, *TABULA* and *BUXIS*.
 5. [Polymorphes](#polys): *TO*, *REF*, *ADD*, *COPY*, *EMPTYP*, *SIZE*, *EQUALS* and *COMPARE*.
 6. [Utilities for Funtional Programming Style](#uilts-ps): $, $*, <<, >>, &, v.


1. Read-Macros <a name="read-macros">
-------------------------------------

#### 1.1 CARET-READER `^` for shorthand lambda forms.

- Church style:

```lisp
  ^x(+ x x)       -> (lambda (x)
                       (declare (ignorable x))
                       (+ x x))
  ^xyz(+ x y z)   -> (lambda (x y z)
                       (declare (ignorable x y z))
                       (+ x y z))
  ^x0.x1(+ x0 x1) -> (lambda (x0 x1)
                       (declare (ignorable x0 x1))
                       (+ x0 x1))
```
  
- McCarthy style:

```lisp
  (^ (x) (+ x x)) -> (lambda (x) (+ x x))
```
     
- Hickey style:

```lisp
  ^(+ @ @)      -> (lambda (@0 &rest @r)
                     (declare (ignore @r))
                     (+ @0 @0))
  ^(+ @1 @0)    -> (lambda (@0 @1 &rest @r)
                     (declare (ignore @r))
                     (+ @1 @0))
  ^(+ @2 @3)    -> (lambda (@0 @1 @2 @3 &rest @r)
                     (declare (ignore @0 @1 @r))
                     (+ @2 @3))
  ^(list @2 @r) -> (lambda (@0 @1 @2 &rest @r)
                     (declare (ignore @0 @1))
                     (list @2 @r))
  ^(cons @2 @w) -> (lambda (@0 @1 @2 &rest @r)
                     (let ((@w (list* @0 @1 @2 @r)))
                       (cons @2 @w))

  ^(if (<= @ 1)
       1
       (* @ (@me (1- @))))
  ->
  (labels ((@me (@0 &rest @rest)
              (declare (ignore @rest))
              (if (<= @0 1)
                  1
                  (* @0 (@me (1- @0))))))
    #'@me)

;; NB. all @-arguments are interned into CL-PLUS.SRC.CORE.LAMBDA package.
```


#### 1.2 BRACE-READER `#{}` for hash-tables.

```lisp
 #{:foo 0 :bar 2 :baz 3} => #{:BAR 2 :BAZ 3 :FOO 0} ; printing order of entries is depends on your cl system.
 (hash-table-test #{})   => EQUAL ; default test is equal.
 (hash-table-test #5{})  => EQUAL
 (hash-table-test #2{})  => EQ
 (hash-table-test #3{})  => EQL
 (hash-table-test #6{})  => EQUALP
  
 ;; If num-arg is over 10, keys and vals are evaled.
 #10{:foo 0 :bar (+ 1 1) :baz (+ 1 1 1)} => #{:BAR 2 :BAZ 3 :FOO 0} 
 #12{:foo 0 :bar (+ 1 1) :baz (+ 1 1 1)} => #2{:BAR 2 :BAZ 3 :FOO 0}
```


#### 1.3 BRACKET-READER `#[]` for lazy-sequences.

```lisp
 (take  5 #[:foo :bar :baz ..])    => (:FOO :BAR :BAZ :FOO :BAR)
 (take 10 #[#\A ..])               => (#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J)
 (take 10 #[1 ..])                 => (1 2 3 4 5 6 7 8 9 10)
 (take 10 #[1 4 ..])               => (1 4 7 10 13 16 19 22 25 28)
 (take 10 #[1 2 4 ..])             => (1 2 4 8 16 32 64 128 256 512)
 (take 10 #[1 2 4 .. 200])         => (1 2 4 8 16 32 64 128)
 (take :all #[#\a .. #\z] 'string) => "abcdefghijklmnopqrstuvwxyz"
 (take :all #[0..10])              => (0 1 2 3 4 5 6 7 8 9 10)

 ;; NB. Only ccl can make a lazy-sequence at read time.
 ;;     If num-arg is supplied, #[] acts as ordinal macros.
```


2.Sub-Packages <a name="sub-packages">
---------------------------------

All sub-packages have package-nickname `"CL+<NAME>"` (e.g., `"CL+LAZY-SEQUENSE"` for *lazy-sequence*).


2.1 Core-Packages
-----------------

 All Core packages are imported into CL+ package.

 - *CL+SEQUENCE*
 - *CL+LAZY-SEQUENCE* (partial)
 - *CL+SEQUENS*
 - *CL+HASH-TABLE*
 - *CL+PLIST*
 - *CL+ALIST*
 - *CL+TABULA*
 - *CL+ARRAY*
 - *CL+BUXIS*
 - *CL+POLYMORPH*
 - *CL+FUNCTION*
 - *CL+ITERATION* (partial)
 <!-- - *CL+ALIAS* -->


2.2 Library-Packages
--------------------

 All Library packages are not imported into CL+ package (except for cdr-8:EQUALS, cdr-8:COMPARE).

 - [*CDR-5*: Sub-interval Numerical Types.](http://cdr.eurolisp.org/document/5/)
 - [*CDR-8*: Generic Equality and Comparison.](http://cdr.eurolisp.org/document/8/)
 - [*CLRFI-1*: FEATUREP.](http://web.archive.org/web/20041016005224/http://clrfi.alu.org/clrfi/clrfi-1-featurep) (web archive link)
 - [*SRFI-45*: Primitives for Expressing Iterative Lazy Algorithms.](http://srfi.schemers.org/srfi-45/)
 - [*SRFI-41*: Streams.](http://srfi.schemers.org/srfi-41/) (Pipes)
 - *... TO BE CONTINUED ===>*


3. Lazy data structures <a name="lazy">
---------------------------------------

#### 3.1 *PROMISE*: "delayed-object" defined at SRFI-45.

```lisp
 (use-package :cl+srfi-45)
 (defvar *the-answer*
   (delay (+ 1 3 5 9 11 13))) => *THE-ANSWER*
 *the-answer*                 => [?]
 (promisep *the-answer*)      => T
 (lazyp *the-answer*)         => T
 (force *the-answer*)         => 42
 *the-answer*                 => [42]
```


#### 3.2 *PIPE*: "delayed-list" defined at SRFI-41. 

```lisp
 ;; Note:
 ;;  In srfi-41, "delayed-list" called "stream".
 ;;  In CL, however, the name "stream" is already used for I/O objects.
 ;;  Following Norvig's PAIP, we changed "stream" to "pipe".
 (use-package :cl+srfi-41)
 (defvar *squares*
   (pipe-of (* x x)
     (x in (pipe-from 1)))) => *SQUARES*
 *squares*                  => [?]
 (pipe->list *squares* 5)   => (1 4 9 16 25)
 *squares*                  => [([1] . [([4] . [([9] . [([16] . [([25] . [?])])])])])]
```


#### 3.3 *LAZY-SEQUENCE*: an api for PIPE.

generator
realizer

```lisp
 ;; CYCLE
 (take 10 (cycle #*01) 'bit-vector)         => #*0101010101

 ;; INDUCE
 (take 10 (induce ^(random 2)) 'bit-vector) => #*0110011101 ; result depends on your cl system.

 (defvar *fib*
   (induce ^xy(+ x y) 0 1))   => *FIB*
 *fib*                        => #[0 1 ..]
 (realize *fib*)              => 1, T
 (realize *fib*)              => 2, T
 *fib*                        => #[0 1 1 2 ..]
 (take-while ^(< @ 50) *fib*) => (0 1 1 2 3 5 8 13 21 34), T
 *fib*                        => #[0 1 1 2 .. 55 ..]

 ;; LAZY-FOR
 (lazy-for (* x y)
   (x in #[1 ..])
   (y from 3 by ^(+ @ @))) => #[..]
 (take 10 *)               => (3 12 36 96 240 576 1344 3072 6912 15360)

 ;; LFOR is alias for LAZY-FOR
 (lfor (cons x y)
   (x from 0)
   (y cycle '(:foo :bar :baz))) => #[..]
 (take 5 *)                     => ((0 . :FOO) (1 . :BAR) (2 . :BAZ) (3 . :FOO) (4 . :BAR))
 
 ;; One Liner.
 (lfor x in #[1 ..] when 'oddp) => #[..]
 (take 10 *)                    => (1 3 5 7 9 11 13 15 17 19)

 ;; 
 (use-package :cl+srfi-41)
 (defvar *lseq*
   (pipe->lseq
     (pipe 0 (/ 1 0) 2))) => *LSEQ*
 *lseq*                   => #[..]
 (lref *lseq* 0)          => 0, T
 *lseq*                   => #[0 ..]
 (lref *lseq* 10)         => NIL, NIL
 *lseq*                   => #[0 [?] [?]]
 (lref *lseq* 2)          => 2, T
 *lseq*                   => #[0 [?] 2]
 (lref *lseq* 1)          => ERROR DIVISION-BY-ZERO !!
```

*LAZY-SEQUENCE* operators:
```lisp
 lazy-sequence-p, make-lazy-seq, copy-lazy-seq,
 pipe->lseq, lseq->pipe, lseq->lflow,
 realize, all-realized-p, max-accessed-index,
 lref, replicate, cycle, induce, lazy-for, lfor
 lazy-range, lazy-map, lazy-reduce,
 lazy-take, lazy-take-until, lazy-take-while,
 lazy-drop, lazy-drop-until, lazy-drop-while,
 lazy-zip, lazy-interleave, lazy-interpose,
```


#### 3.4 *LAZY-FLOW*: special lazy-sequence that stores the only just previous value.

```lisp
 (defvar *golden-ratio*
   (induce-flow ^(1+ (/ @)) 1))        => *GOLDEN-RATIO*
 *golden-ratio*                        => [1]~
 (realize *golden-ratio*)              => 2, T
 (realize *golden-ratio* 20)           => 28657/17711, T
 *golden-ratio*                        => [28657/17711]~
 (float (current-flow *golden-ratio*)) => 1.618034
```

*LAZY-FLOW* operators:
```lisp
 lazy-flow-p, make-lazy-flow, copy-lazy-flow, lflow->lseq,
 current-flow, repeat-flow, cycle-flow, induce-flow.
```


4. Abstract data types <a name="atype">
---------------------------------------

    +-----------------------------------------------+   +----------------------------+
    |                 Buxis (~*)                    |   |        Tabula (~+)         |
    |-----------------------------------------------|   |----------------------------|
    |          Sequens         | Hash-Table | Array |   | Alist | Plist | Hash-Table |
    |--------------------------+--------------------+   +----------------------------+
    | Lazy-Sequence | Sequence |
    +--------------------------+


"Abstract data type", here, means that as if SEQUENCE is an abstraction from LIST and VECTOR.


#### 4.1 *SEQUENS* is an abstract type from SEQUENCE and LAZY-SEQUENCE.

```lisp
 (take-while #'characterp '(#\s #\t #\r i n g) 'string) => "str"
 (take-while #'characterp #[#\s #\t #\r i n g] 'string) => "str"
 
 (apply #'add (interpose ", "  '("foo" "bar" "baz")))              => "foo, bar, baz"
 (apply #'add (take 10 (interpose ", " #[ "foo" "bar" "baz" ..]))) => "foo, bar, baz, foo, bar, "
```

*SEQUENS* operators:
```lisp
 sequensp, seq->lseq, doseq, repeat
 take, take-until, take-while,
 drop, drop-until, drop-while,
 zip, interleave, interpose.
```


#### 4.2 *TABULA* is an abstruct type from HASH-TABLE, PLIST and ALIST.

```lisp
 (remove-if+ ^kv(oddp v) #{:foo 0 :bar 1 :baz 2})             => #{:FOO 0 :BAZ 2}
 (remove-if+ ^kv(oddp v) '(:foo 0 :bar 1 :baz 2))             => (:FOO 0 :BAZ 2)
 (remove-if+ ^kv(oddp v) '((:foo . 0) (:bar . 1) (:baz . 2))) => ((:FOO . 0) (:BAZ . 2))

 (reduce+ ^akv(+ a v) #{:foo 0 :bar 1 :baz 2} :initial-value 0) => 3
 (reduce+ ^akv(+ a v) '(:foo 0 :bar 1 :baz 2) :initial-value 0) => 3
 (reduce+ ^akv(+ a v) '((:foo . 0) (:bar . 1) (:baz . 2))
                      :initial-value 0)                         => 3
```

*TABULA* operators:
```lisp
 tabulap, dotab, dotab2, clear-tabula, erase-entry, tabula=,
 emptyp+, copy+, add+, size+, ref+,
 keys, vals, to-hash, to-alist, to-plist,
 map+, reduce+, every+, notevery+, some+, notany+,
 count+, count-if+, count-if-not+,
 find+, find-if+, find-if-not+,
 position+, position-if+, position-if-not+,
 remove+, remove-if+, remove-if-not+, collect-if+,
 substitute+, substitute-if+, substitute-if-not+.
```


#### 4.3 *BUXIS* is an abstract type from HASH-TABLE, SEQUENCE, LAZY-SEQUENCE and ARRAY.

```lisp
 (count-if* #'oddp #{:foo 0 :bar 1 :baz 2}) => 1
 (count-if* #'oddp #(0 1 2 3 4))            => 2
 (count-if* #'oddp '(0 1 2 3 4))            => 2
 (count-if* #'oddp #[0 1 2 3 4])            => 2
 (count-if* #'oddp #2A((0 1 2) (3 4 5)))    => 3
```

*BUXIS* operators:
```lisp
 buxisp, dobux, dobux2,
 every*, notevery*, some*, notany*,
 map*, reduce*,
 count*, count-if*, count-if-not*,
 find*, find-if*, find-if-not*,
 position*, position-if*, position-if-not*,
 remove*, remove-if*, remove-if-not*, collect-if*,
 substitute*, substitute-if*, substitute-if-not*
```

5. Polymorphes <a name="polys">
-------------------------------

Polymorphes is a collection of generic functions, user-extensible and user-modifiable.


#### 5.1 *TO*: abstraction from `coerce`.

```lisp
 (let ((log (to 'string #p"./README.md")))
   (subseq log 0 (position #\Newline log)))
 => "Last modified: 2014-06-29 11:02:27 tkych"
```


#### 5.2 *REF*: generic refer function.



#### 5.3 *ADD*: abstraction from `+` and `concatenate`.

```lisp
 (add 40 1 1)               => 42
 (add "Hello" " " "World!") => "Hello World!"
 (add '(#l #i) "sp")        => (#l #i #s #p)
 (add "" '(#l #i) "sp")     => "lisp"
 (add #2A((1 2 3) (4 5 6))
      #2A((6 5 4) (3 2 1))) => #2A((7 7 7) (7 7 7))
 (add #{:Roma "Varro" :Carthage "Hannibal"}
      #{:Roma "Scipio"})
   => #{:ROMA "Scipio" :CARTHAGE "Hannibal"}

 (defstruct pocket contents)
 (defmethod add ((p1 pocket) (p2 pocket) &rest more)
   (let ((p3 (make-pocket :contents (append (pocket-contents p1)
                                            (pocket-contents p2)))))
     (if (null more)
         p3
         (apply #'add p3 more))))

 (let* ((crassus  (make-pocket :contents '("biscuit")))
        (pompeius (make-pocket :contents '("chocolate")))
        (caesar   (add crassus pompeius)))
   (pocket-contents caesar))
 => ("biscuit" "chocolate")
```

#### 5.4 *COPY*: 


#### 5.5 *EMPTYP*


#### 5.6 *SIZE*


#### 5.7 *EQUALS*

EQUALS is specified by CDR-8.
```lisp
 (equals 1.0d0
         (reduce #'+ (take 10 (repeat 0.1d0)))
         :epsilon 1d-10)
 => T
 (equals #{:foo 0 :bar 1 :baz 2}      ; <- hash-table-tast is EQUAL
         #2{:foo 0 :bar 1 :baz 2})    ; <- hash-table-tast is EQ
 => T
 (equals #{:foo 0 :bar 1 :baz 2}
         #2{:foo 0 :bar 1 :baz 2}
         :check-properties t)
 => NIL
```

#### 5.8 *COMPARE*

COMPARE is specified by CDR-8.

```lisp
 (compare 1 42)        => <
 (compare 42 1)        => >
 (compare 42 42)       => =
 (compare #c(0 42) 42) => /=
```


6. Utilities for Funtional Programming Style <a name="uilts-ps">
----------------------------------------------------------------


```lisp
 $, $*, <<, >>, &, v.
```

<!-- ============================================================= -->
