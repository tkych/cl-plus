;;;; Last modified: 2014-06-27 22:40:17 tkych

;; cl-plus/src/core/lambda.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Shorthand Lambda
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.core.lambda
  (:documentation "
Shorthand Lambda: Caret Reader
==============================


Examples
--------

 1. Church style:

    * ^x(+ x x)       -> (lambda (x)
                           (declare (ignorable x))
                           (+ x x))

    * ^xy(+ x y)      -> (lambda (x y)
                           (declare (ignorable x y))
                           (+ x y))

    * ^xyz(+ x y z)   -> (lambda (x y z)
                           (declare (ignorable x y z))
                           (+ x y z))

    * ^x0.x1(+ x0 x1) -> (lambda (x0 x1)
                           (declare (ignorable x0 x1))
                           (+ x0 x1))

    * ^key.(+ key 42) -> (lambda (key)
                           (declare (ignorable key))
                           (+ key 42))


 2. McCarthy style:

    * (^ (x) (+ x x)) -> (lambda (x) (+ x x))


 3. Hickey style:

    * ^(+ @ @0 @2)     ->  (lambda (@0 @1 @2 &rest @rest)
                             (declare (ignore @1 @rest))
                             (+ @0 @0 @2))

    * ^(+ @1 @2 @4)    ->  (lambda (@0 @1 @2 @3 @4 &rest @rest)
                             (declare (ignore @0 @3 @rest))
                             (+ @1 @2 @4))

    * ^(list @1 @rest) ->  (lambda (@0 @1 &rest @rest)
                              (declare (ignore @0))
                              (list @1 @rest))

    * ^(cons @1 @all)  ->  (lambda (@0 @1 &rest @rest)
                              (let ((@all (list* @0 @1 @rest)))
                                (cons @1 @all)))

    * ^(if (< 100 @)
           (- @ 10)
           (@me (@m (+ @ 11))))
      ->
      (labels ((@me (@0 &rest @rest)
                 (declare (ignore @rest))
                 (if (< 100 @0)
                     (- @0 10)
                     (@me (@me (+ @0 11))))))
         #'@me)


Notes
-----

 - Mixed styles does not work properly.

 - We do not prohibit from making nested shrothand-lambda, but be
   careful, it tends to mislead for lambda-list scope.

 - Default *shorthand-lambda-parameter-prefix* is #\@.

 - Within lambda-body ^(...) for hickey-style, user-defined
   @-read-macro doesn't work (outside works).
   cf. (setf *shorthand-lambda-parameter-prefix* #\%)

 - @ is an alias for @0, also @r for @rest, @a for @all and @m for @me.

 - Because a named-lambda (i.e. shorthand-lambda which contains at
   least one @m or @me) is not a lambda-expression but a labels-expression,
   we can't use the named-lambda at car position.

    e.g., (^(if (<= @ 1) 1 (* @ (@M (1- @)))) 5)         => ERROR!
          (funcall ^(if (<= @ 1) 1 (* @ (@M (1- @)))) 5) => 120

 - Default *shorthand-lambda-parameters-limit* is 10.
   (since get-decoded-time returns 9 values)
   This parameter controls only hickey-style lambdas.

    e.g., church-style: ^abcdefghijklmnopqrsuvwxyz() is ok.


Grammar
-------

 1. Church like style:

    ^<parameter-chunk><lambda-body>

    where
      <parameter-chunk>  ::= <long-parameters>|<short-parameters>
      <long-parameters>  ::= <long-parameter>.|{<long-parameter>.}+<long-parameter>
      <long-parameter>   ::= <symbol>
      <short-parameters> ::= <symbol>
      <short-parameter>  ::= <symbol>
                            s.t. its symbol-name is a one of (explode <short-parameters>).
                            e.g. if 'xyz' is a short-parameters, then 'x','y','z'
                                 are all short-parameter.
      <lambda-body>      ::= <form>


 2. McCarthy like style:

    `^' is just an alias for symbol `lambda'.


 3. Hickey like style:

    ^<lambda-body>

    where
      <lambda-body> ::= <form>
      <parameter>   ::= <prefix>{<num>|r|rest|a|all|m|me}
      <prefix>      ::= <character *shorthand-lambda-parameter-prefix*>
      <num>         ::= <integer 0 <limit>>
      <limit>       ::= <integer 1 *shorthand-lambda-parameters-limit*>


References
----------

 [0] Peter Norvig (1992),
     Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp,
     Section 1.7 Higher-Order Functinos.

 [1] Gauche, Gauche Users' Reference, 4.3 Making Procedures.
     http://practical-scheme.net/gauche/man/gauche-refe_24.html#Making-Procedures

 [2] Clojure, anonymous function shorthand.
     http://clojuredocs.org/clojure_core/clojure.core/fn
     http://clojure.org/reader

 [3] CL21, issue #35, Add \"fn\" for a shorthand of \"lambda\".
     https://github.com/fukamachi/cl21/issues/35
")
  (:nicknames #:cl+lambda)
  (:export #:caret-reader
           #:*shorthand-lambda-parameters-limit*
           #:*shorthand-lambda-parameter-prefix*)
  (:use #:cl)
  (:import-from #:alexandria
                #:switch))

(in-package #:cl-plus.src.core.lambda)


;;--------------------------------------------------------------------
;; Caret Reader
;;--------------------------------------------------------------------

;; For Church like style:
(defun read-church-style-parameters (stream)
  (let (paras buff)
    (flet ((force-buff ()
             (push (read-from-string (coerce (nreverse buff) 'string))
                   paras)
             (setf buff '())))
      (loop :with piriod-exists? := nil
            :for c := (read-char stream nil :end t)
            :until (eq c :end)
            :do (case c
                  ((#\space #\))
                   (error "Caret-reader syntax error."))
                  ((#\()
                   (unread-char #\( stream)
                   (if piriod-exists?
                       (when buff
                         (force-buff))
                       (setf paras (mapcar (lambda (c) (read-from-string (string c)))
                                          buff)))
                   (loop-finish))
                  ((#\.)
                   (setf piriod-exists? t)
                   (force-buff))
                  (t
                   (push c buff)))))
    (nreverse paras)))

;; Test for read-church-style-parameters
;; (let* ((s (make-string-input-stream "x0.x1(+ x0 x1)")))
;;   (multiple-value-prog1
;;       (unwind-protect
;;            (%read-args s)
;;         (close s))))

;; For Hickey like style:
(deftype shorthand-lambda-parameters ()
  `(integer 1 ,lambda-parameters-limit)) ; 1 for @.
(declaim (type shorthand-lambda-parameters
               *shorthand-lambda-parameters-limit*))
(defparameter *shorthand-lambda-parameters-limit* 10)

(declaim (type character *shorthand-lambda-parameter-prefix*))
(defparameter *shorthand-lambda-parameter-prefix* #\@)

(defvar *positional-parameters*)
(defvar *all-parameter-exists?*)
(defvar *rest-parameter-exists?*)
(defvar *named-lambda?*)

(defun intern-parameter (thing)
  ;; MEMO: 2014-05-28
  ;;  ecl: a read-macro returning multiple-values causes an error.
  (nth-value 0
             (intern (format nil "~A~A"
                             *shorthand-lambda-parameter-prefix*
                             thing)
                     "CL-PLUS.SRC.CORE.LAMBDA")))

;; MEMO: 2014-05-18
;; At-reader macro is for:
;;  1. not importing symbols @, @0, @1, ... into each package,
;;  2. parsing parameters,
;;  3. removing potential confliction with user-defined @-read-macro
;;     (e.g., cl-annot) in shorthand-lambda form, ^(...).

;; MEMO: @ME or @SELF
;;   1. @self is longer than @me.
;;   2. @s, @S and @5 looks similar.
;; 
;; * Since I am not english native, if they feel too weird for @ME,
;;   then it should be renamed @ME -> @SELF.

(defun at-reader (stream macro-char)
  (declare (ignore macro-char))
  (flet ((error-unknown-parameter (para)
           (error "~A~A is unknown parameter symbol for shorthand-lambda."
                  *shorthand-lambda-parameter-prefix* para))
         (error-out-of-parameters-limit (num)
           (error "~D is out of *SHORTHAND-LAMBDA-PARAMETERS-LIMIT*: ~D"
                   num *shorthand-lambda-parameters-limit*)))
    (let ((char (peek-char nil stream t #\Newline t)))
      (if (digit-char-p char)

          (let ((num (read stream)))    ; @0, @1, @2, ...
            (cond ((not (numberp num))
                   (error-unknown-parameter num))
                  ((< (1- *shorthand-lambda-parameters-limit*) num) ; count from 0.
                   (error-out-of-parameters-limit num))
                  (t
                   (pushnew num *positional-parameters*)
                   (intern-parameter num))))
          
          (case char
            ((#\Space #\( #\))          ; @ is alias for @0
             (pushnew 0 *positional-parameters*)
             (intern-parameter 0))
            
            ((#\a #\A)                  ; @A, @ALL
             (let ((para (read stream)))
               (switch (para :test #'string-equal)
                 ("A"     (setf *all-parameter-exists?* t)
                          (intern-parameter "ALL"))
                 ("ALL" (setf *all-parameter-exists?* t)
                          (intern-parameter "ALL"))
                 (t       (error-unknown-parameter para)))))
            
            ((#\r #\R)                  ; @R, @REST
             (let ((para (read stream)))
               (switch (para :test #'string-equal)
                 ("R"    (setf *rest-parameter-exists?* t)
                         (intern-parameter "REST"))
                 ("REST" (setf *rest-parameter-exists?* t)
                         (intern-parameter "REST"))
                 (t      (error-unknown-parameter para)))))
            
            ((#\m #\M)                  ; @M, @ME
             (let ((para (read stream)))
               (switch (para :test #'string-equal)
                 ("M"    (setf *named-lambda?* t)
                         (intern-parameter "ME"))
                 ("ME" (setf *named-lambda?* t)
                         (intern-parameter "ME"))
                 (t      (error-unknown-parameter para)))))
            
            (t (error-unknown-parameter (read stream))))))))


(defun caret-reader (stream char)
  (declare (ignore char))
  (case (peek-char nil stream t #\Newline t)

    ;; McCarthy like style:
    (#\Space
     'lambda)

    ;; Hickey like style:
    (#\(
     (read-char stream t nil t)         ; discard #\(
     (let ((*readtable* (copy-readtable)))
       (set-macro-character *shorthand-lambda-parameter-prefix*
                            #'at-reader)
       (let* ((*positional-parameters*   '())
              (*all-parameter-exists?* nil)
              (*rest-parameter-exists?*  nil)
              (*named-lambda?*           nil)
              ;; MEMO: 2014-05-18
              ;; When the following read-delimited-list is called,
              ;; at-reader parses parameters and stores the results in
              ;; above special variables. This might be complex, but
              ;; it supports to work nested-lambdas and user-defined
              ;; read macros (except for *shorthand-lambda-parameter-prefix*).
              (body (read-delimited-list #\) stream t)))
         
         (multiple-value-bind (at-numbers ignored)
             (if (null *positional-parameters*)
                 (values '() '())
                 (loop :for i fixnum :from 0 :to (apply #'max *positional-parameters*)
                       :for at-num := (intern-parameter i)
                       :collect at-num :into notice
                       :unless (find i *positional-parameters*)
                         :collect at-num :into ignored
                       :finally (return (values notice ignored))))
           
           (let* ((at-rest     (intern-parameter "REST"))
                  (lambda-list (append at-numbers (list '&rest at-rest))))

             (if (not *named-lambda?*)
                 
                 ;; Not named-lambda case:
                 (if *all-parameter-exists?*
                     `(lambda ,lambda-list
                        (let ((,(intern-parameter "ALL")
                                (list* ,@at-numbers ,at-rest)))
                          ,body))
                     `(lambda ,lambda-list
                        ,@(cond ((and ignored *rest-parameter-exists?*)
                                 `((declare (ignore ,@ignored))))
                                (*rest-parameter-exists?*
                                 '())
                                (ignored
                                 `((declare (ignore ,@ignored ,at-rest))))
                                (t
                                 `((declare (ignore ,at-rest)))))
                        ,body))
                 
                 ;; Named-lambda case:
                 (let ((at-me (intern-parameter "ME")))
                   (if *all-parameter-exists?*
                       `(labels ((,at-me ,lambda-list
                                   (let ((,(intern-parameter "ALL")
                                           (list* ,@at-numbers ,at-rest)))
                                     ,body)))
                          (function ,at-me))
                       `(labels ((,at-me ,lambda-list
                                   ,@(cond ((and ignored *rest-parameter-exists?*)
                                            `((declare (ignore ,@ignored))))
                                           (*rest-parameter-exists?*
                                            '())
                                           (ignored
                                            `((declare (ignore ,@ignored ,at-rest))))
                                           (t
                                            `((declare (ignore ,at-rest)))))
                                   ,body))
                          (function ,at-me))))))))))

    ;; Church like style:
    (t
     (let ((parameters (read-church-style-parameters stream)))
       `(lambda ,parameters
          (declare (ignorable ,@parameters))
          ,(read stream t nil t))))))


;;====================================================================
