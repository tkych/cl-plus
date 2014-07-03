;;;; cl-plus/test/test-at-reader.lisp

;;====================================================================
;; Test, At-reader
;;====================================================================

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-annot))

(defpackage :tmp2
  (:use :cl :cl+))

(in-package :tmp2)

(in-readtable :cl+)
(cl-annot:enable-annot-syntax)

@export
(defun tmp2-fn (x)
  (+ x 33))

@export
(defun tmp2-fn2 (x)
  `^(+ @ @2 ,x))

;;====================================================================
