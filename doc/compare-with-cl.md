
((CL+)) : Compare with pure CL
==============================

Hash-Table
----------

#### Make hash-table.

```lisp
;; CL
(defparameter *ht*
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash :foo ht) 0
          (gethash :bar ht) 1
          (gethash :baz ht) 2
          (gethash "quux" ht) 42)
    ht))

;; CL+CL
(defparameter *ht* #{:foo 0 :bar 1 :baz 2})
```


#### Check entries.

```lisp
;; CL
(loop :for v :being :the :hash-values :of *ht*
      :always (< v 10))

(loop :for v :being :the :hash-values :of *ht* :using (:hash-key k)
      :always (and (keywordp k) (< v 10)))

;; CL+CL
(every* ^(< @ 10) *ht*)

(every+ ^kv(and (keywordp k) (< v 10))
        *ht*)
```


#### Collect keys.

```lisp
;; CL
(loop :for k :being :the :hash-keys :of *ht*
      :when (symbolp k) :collect k)

;; CL+CL
(keys *ht* :when ^kv(symbolp k))
```


#### Collect values.

```lisp
;; CL
(loop :for k :being :the :hash-keys :of *ht* :using (:hash-value v)
      :when (stringp k) :collect v)

;; CL+CL
(vals *ht* :when ^kv(stringp k))
```


#### Find key.

```lisp
;; CL
(loop :for k :being :the :hash-keys :of *ht* :using (:hash-value v)
      :when (eql v 42)
        :return k)

;; CL+CL
(position* 42 *ht*)
```


#### Find value.

```lisp
;; CL
(loop :for k :being :the :hash-keys :of *ht* :using (:hash-value v)
      :when (stringp k)
        :return v)

;; CL+CL
(find-if+ ^kv(stringp k) *ht*)
```

Lazy-Sequence
-------------

#### fibonacci numbers.
```lisp
;; CL
(defun fib (n)
  (do ((i 0 (1+ i))
       (curr 0)
       (next 1))
      ((<= n i) curr)
    (psetq curr next
           next (+ next curr))))

(fib 42)

;; CL+CL
(lref (induce ^xy(+ x y) 0 1) 42)
```


#### iota, range.
```lisp
;; CL
(loop :for i :from 0 :to 10 :by 2 :collect i)

;; CL+CL
(take :all #[0 2..10])
```


#### collect.
```lisp
;; CL
(loop :for i :from 0
      :for p :from 1 :to 10
      :collect (expt (expt 2 i) p))

;; CL+CL
(map* 'list ^bp(expt b p) #[1 2 4..] #[1..10])
(map* 10 ^bp(expt b p) #[1 2 4..] #[1..])
```


*===> TO BE CONTINUED*
