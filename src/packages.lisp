;;;; cl-plus/src/packages.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Packages for CL-PLUS
;;====================================================================

(in-package #:cl-user)

;;--------------------------------------------------------------------
;; CL+
;;--------------------------------------------------------------------

(defpackage :common-lisp-plus
  (:nicknames #:cl+)
  (:documentation "
CL-PLUS
=======

   An Interface on Common Lisp for Rapid Prototyping.

 -------------------------------------------------   ------------------------------
 |                 Buxis (~*)                    |   |        Tabula (~+)         |
 -------------------------------------------------   ------------------------------
 |          Sequens         | Hash-Table | Array |   | Alist | Plist | Hash-Table |
 ---------------------------+---------------------   ------------------------------
 | Lazy-Sequence | Sequence |
 ----------------+-----------


Types
-----

    <sequens> := (or <sequence> <lazy-sequence>)
    <tabula>  := (or <plist> <alist> <hash-table>)
    <buxis>   := (or <sequens> <hash-table> <array>)

Sequens is a collection of pair(index, value) whether it realized or not.
A sequens is a sequance or lazy-sequence.

Tabula is a collection of pair(key, value).
Tabula is abstraction from plists, alists and hash-tables.

Buxis is a collection of pair(tag, value).
Buxis is abstraction from (lazy-)sequences, hash-tables and arrays.

    <index>      ::= <integer 0 *>
    <subscripts> ::= <list-of-index>
    <key>        ::= <object>
    <tag>        ::= <index> | <key> | <subscripts>
    <value>      ::= <object>


 TYPES
 -----

 - PLIST
 - PLIST+
 - ALIST
 - LAZY-SEQUENCE
 - SEQUENS
 - TABULA
 - BUXIS


 SEQUENCE              TABULA                 BUXIS (SEQUENS)
 --------              ------                 ---------------

 - MAP                 - MAP+                 - MAP*
 - REDUCE              - REDUCE+              - REDUCE*
 - SCAN                                       - SCAN*
 - EVERY               - EVERY+               - EVERY*
 - NOTEVERY            - NOTEVERY+            - NOTEVERY*
 - SOME                - SOME+                - SOME*
 - NOTANY              - NOTANY+              - NOTANY*
 - COUNT               - COUNT+               - COUNT*
 - COUNT-IF            - COUNT-IF+            - COUNT-IF*
 - COUNT-IF-NOT        - COUNT-IF-NOT+        - COUNT-IF-NOT*
 - FIND                - FIND+                - FIND*
 - FIND-IF             - FIND-IF+             - FIND-IF*
 - FIND-IF-NOT         - FIND-IF-NOT+         - FIND-IF-NOT*
 - POSITION            - POSITION+            - POSITION*
 - POSITION-IF         - POSITION-IF+         - POSITION-IF*
 - POSITION-IF-NOT     - POSITION-IF-NOT+     - POSITION-IF-NOT*
 - REMOVE              - REMOVE+              - REMOVE*
 - REMOVE-IF           - REMOVE-IF+           - REMOVE-IF*
 - REMOVE-IF-NOT       - REMOVE-IF-NOT+       - REMOVE-IF-NOT*
 - REMOVE-IF-NOT-NOT   - REMOVE-IF-NOT-NOT+   - REMOVE-IF-NOT-NOT*
                       - KEYS, VALS           - COLLECT-IF*
 - SUBSTITUTE          - SUBSTITUTE+          - SUBSTITUTE*
 - SUBSTITUTE-IF       - SUBSTITUTE-IF+       - SUBSTITUTE-IF*
 - SUBSTITUTE-IF-NOT   - SUBSTITUTE-IF-NOT+   - SUBSTITUTE-IF-NOT*

 - COERCE              - TO-PLIST             - TO (polymorphic)
                         TO-ALIST
                         TO-HASH

 - COPY-SEQ                  - COPY (polymorphic)

 - LENGTH              - SIZE+                - SIZE (polymorphic)

 - ELT                 - REF+                 - REF (polymorphic)

 - DOLIST (list)       - DOTAB                - DOBUX

 - CONCATENATE         - ADD+                 - ADD (polymorphic)

 - NULL (list)               - EMPTYP (polymorphic)



LAZY-SEQUENCE
-------------

 - INDUCE
 - CYCLE


SEQUENS
-------

?- DOSEQ
 - TAKE
 - TAKE-UNTIL
 - TAKE-WHILE
 - DROP
 - DROP-UNTIL
 - DROP-WHILE
 - ZIP
 - INTERPOSE


ITERATION
---------

 - DOHASH
 - DOPLIST
 - DOALIST
 - DOARY
 - DOTAB
 - DOBUX
?- DOSEQ


POLYMORPHES
-----------

 - EMPTYP
 - SIZE
 - COPY
 - TO
 - ADD
 - REF
 - FMAP


MISC
----

 - ARRAY-SUBSCRIPTS
 - COPY-HT


SYNTAX
------

 - CL+, :CL+
 - CL+CARET
 - CL+SHARP-BRACE
 - CL+SHARP-BRACKET

")
  (:export
   ;; READTABLES
   #:cl+
   #:cl+caret
   #:cl+sharp-brace
   #:cl+sharp-bracket

   ;; PPRINT-DISPATCH-TABLE
   #:*cl+pprint-dispatch-table*

   ;; SEQUENCE
   #:sequencep
   #:repeat
   #:permutations
   #:combinations
   #:inits
   #:tails
   #:scan
   #:unreduce
   #:split
   #:split-if
   #:split-if-not
   #:split-at
   
   ;; LAZY-SEQUENCE
   #:lazy-sequence
   #:lazy-sequence-p
   #:empty-lazy-seq-p
   #:make-lazy-seq
   #:lazy-seq
   #:lazy-for
   #:lfor
   #:pipe->lseq
   #:lseq->pipe
   #:copy-lazy-seq
   #:realize           
   #:lref
   #:with-lazy-seq-iterator
   #:accessed-length
   #:all-accessed-p
   #:replicate
   #:cycle
   #:induce
   
   ;; Lazy-Flow
   #:lazy-flow
   #:lazy-flow-p
   #:make-lazy-flow
   #:copy-lazy-flow
   #:current-flow
   #:replicate-flow
   #:cycle-flow
   #:induce-flow
   #:lseq->lflow
   #:lflow->lseq
   
   ;; SEQUENS
   #:sequens
   #:sequensp
   #:seq->lseq
   #:range
   #:doseq   #:doseq2
   #:take    #:take-until   #:take-while
   #:drop    #:drop-until   #:drop-while
   #:zip     #:interleave   #:interpose

   ;; ITERATION
   #:enable-loop-unrolling
   #:disable-loop-unrolling
   #:dolist2

   ;; HASH-TABLE
   #:*print-readable-hash-table*
   #:*default-hash-table-test*
   #:hash-table=
   #:dohash

   ;; ALIST
   #:alist
   #:alistp
   #:alist=
   #:normalize-alist
   #:doalist
   
   ;; PLIST
   #:plist
   #:plistp
   #:plist+
   #:plistp+
   #:plist=
   #:doplist
   
   ;; TABULA
   #:tabula     #:tabulap
   #:dotab      #:dotab2
   #:clear-tabula
   #:erase-entry
   #:tabula=
   #:add+
   #:size+
   #:ref+
   #:keys        #:vals
   #:to-hash     #:to-alist       #:to-plist
   #:map+        #:reduce+
   #:every+      #:notevery+
   #:some+       #:notany+
   #:count+      #:count-if+      #:count-if-not+
   #:find+       #:find-if+       #:find-if-not+
   #:position+   #:position-if+   #:position-if-not+
   #:remove+     #:remove-if+     #:remove-if-not+
   #:substitute+ #:substitute-if+ #:substitute-if-not+

   ;; ARRAY
   #:array-subscripts
   #:doary
   #:doary2

   ;; BUXIS
   #:with-iterator
   #:with-iterators
   #:buxis       #:buxisp
   #:dobux       #:dobux2
   #:map*        #:reduce*        #:scan*
   #:every*      #:notevery*
   #:some*       #:notany*
   #:count*      #:count-if*      #:count-if-not*
   #:find*       #:find-if*       #:find-if-not*
   #:position*   #:position-if*   #:position-if-not*
   #:remove*     #:remove-if*     #:remove-if-not*
   #:collect-if*
   #:substitute* #:substitute-if* #:substitute-if-not*
   #:fill*
   
   ;; POLYMORPHICS
   #:fmap
   #:emptyp
   #:size
   #:copy #:tree
   #:ref
   #:to
   #:add
   #:equals
   #:compare
   
   ;; Function
   #:$  #:$*
   #:<< #:>>
   #:$>
   #:&  #:v

   ;; ALIAS
   ;; #:copy-ht
   ;; #:dbind #:mvbind #:mvsetq #:mvcall #:mvprog1
   ;; #:1st #:2nd #:3rd #:4th #:5th #:6th #:7th #:8th #:9th #:10th
   )
  
  (:use #:cl)
  
  (:import-from #:cl-plus.src.readtables
                #:cl+
                #:cl+caret
                #:cl+sharp-brace
                #:cl+sharp-bracket)

  (:import-from #:cl-plus.src.pprint-dispatch-table
                #:*cl+pprint-dispatch-table*)

  (:import-from #:cl-plus.src.core.iteration
                #:enable-loop-unrolling
                #:disable-loop-unrolling
                #:dolist2)

  (:import-from #:cl-plus.src.core.sequence
                #:sequencep
                #:repeat
                #:permutations
                #:combinations
                #:inits
                #:tails
                #:scan
                #:unreduce
                #:split  #:split-if  #:split-if-not  #:split-at)

  (:import-from #:cl-plus.src.core.lazy-sequence
                #:lazy-sequence
                #:lazy-sequence-p
                #:empty-lazy-seq-p
                #:make-lazy-seq
                #:lazy-seq
                #:lazy-for
                #:lfor
                #:accessed-length
                #:pipe->lseq
                #:lseq->pipe
                #:copy-lazy-seq
                #:realize           
                #:lref
                #:with-lazy-seq-iterator
                #:all-accessed-p
                #:replicate
                #:cycle
                #:induce
                #:lazy-flow
                #:lazy-flow-p
                #:make-lazy-flow
                #:copy-lazy-flow
                #:current-flow
                #:lseq->lflow
                #:lflow->lseq
                #:replicate-flow
                #:cycle-flow
                #:induce-flow)

  (:import-from #:cl-plus.src.core.sequens
                #:sequens
                #:sequensp
                #:seq->lseq
                #:range
                #:doseq   #:doseq2
                #:take    #:take-until   #:take-while
                #:drop    #:drop-until   #:drop-while
                #:zip     #:interleave   #:interpose)
  
  (:import-from #:cl-plus.src.core.hash-table
                #:*print-readable-hash-table*
                #:*default-hash-table-test*
                #:hash-table=
                #:dohash)

  (:import-from #:cl-plus.src.core.alist
                #:alist
                #:alistp
                #:alist=
                #:normalize-alist
                #:doalist)

  (:import-from #:cl-plus.src.core.plist
                #:plist
                #:plist+
                #:plistp
                #:plistp+
                #:plist=
                #:normalize-plist
                #:doplist)

  (:import-from #:cl-plus.src.core.tabula
                #:tabula
                #:tabulap
                #:dotab
                #:dotab2
                #:clear-tabula
                #:erase-entry
                #:tabula=
                #:add+
                #:size+
                #:ref+
                #:keys        #:vals
                #:to-hash     #:to-alist       #:to-plist
                #:map+        #:reduce+
                #:every+      #:notevery+
                #:some+       #:notany+
                #:count+      #:count-if+      #:count-if-not+
                #:find+       #:find-if+       #:find-if-not+
                #:position+   #:position-if+   #:position-if-not+
                #:remove+     #:remove-if+     #:remove-if-not+
                #:substitute+ #:substitute-if+ #:substitute-if-not+)

  (:import-from #:cl-plus.src.core.array
                #:array-subscripts
                #:doary
                #:doary2)
  
  (:import-from #:cl-plus.src.core.buxis
                #:with-iterator
                #:with-iterators
                #:buxis       #:buxisp
                #:dobux       #:dobux2
                #:map*        #:reduce*        #:scan*
                #:every*      #:notevery*
                #:some*       #:notany*
                #:count*      #:count-if*      #:count-if-not*
                #:find*       #:find-if*       #:find-if-not*
                #:position*   #:position-if*   #:position-if-not*
                #:remove*     #:remove-if*     #:remove-if-not*
                #:collect-if*
                #:substitute* #:substitute-if* #:substitute-if-not*
                #:fill*)
  
  (:import-from #:cl-plus.src.core.polymorphics
                #:fmap
                #:emptyp
                #:size
                #:copy #:tree
                #:ref
                #:add
                #:to
                #:equals
                #:compare)
  
  (:import-from #:cl-plus.src.core.function
                #:$  #:$*
                #:<< #:>>
                #:$>
                #:&  #:v)
  
  ;; (:import-from #:cl-plus.src.core.alias
  ;;               #:pipe-remove-if-not
  ;;               #:pipe-reduce
  ;;               #:pipe-mapc
  ;;               #:copy-ht
  ;;               #:dbind #:mvbind #:mvsetq #:mvcall #:mvprog1
  ;;               #:1st #:2nd #:3rd #:4th #:5th #:6th #:7th #:8th #:9th #:10th)
  )


;;--------------------------------------------------------------------
;; CL+USER
;;--------------------------------------------------------------------

(in-package #:cl-user)

(defpackage #:common-lisp-plus-user
  (:nicknames #:cl+user)
  (:documentation "common-lisp-plus user interface package.")
  (:use #:cl #:cl+))


;;--------------------------------------------------------------------

#|
;; for DEV
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:repl-utilities :named-readtables)))

(do-external-symbols (sym :repl-utilities)
  (import sym :cl+user))
(do-external-symbols (sym :named-readtables)
  (import sym :cl+user))

(in-package :cl+user)

;; Exist from cl+ to cl
(defun cl ()
  (in-package :cl-user)
  (named-readtables:in-readtable :standard)
  (setf *print-pprint-dispatch* (copy-pprint-dispatch nil))
  (setf *print-pretty* nil)
  (values))

;; Reload cl+
(defun cl+ (&optional (syntax :cl+))
  (in-package :cl-user)
  (named-readtables:in-readtable :standard)
  (ql:quickload :cl-plus)
  (in-package :cl+user)
  (when syntax
    (eval (read-from-string
           (format nil "(named-readtables:in-readtable ~S)" syntax))))
  (setf *print-pprint-dispatch* cl+:*cl+pprint-dispatch-table*)
  (setf *print-pretty* t)
  (values))

;; Switch to cl+ from cl
(defun cl-user::cl+ (&optional (syntax :cl+))
  (ql:quickload :cl-plus)
  (in-package :cl+user)
  (when syntax
    (eval (read-from-string
           (format nil "(named-readtables:in-readtable ~S)" syntax))))
  (setf *print-pprint-dispatch*
        (eval (read-from-string "cl+:*cl+pprint-dispatch-table*")))
  (setf *print-pretty* t)
  (values))

(export 'cl-user::cl+ :cl-user)

|#

;;====================================================================
