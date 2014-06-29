;;;; Last modified: 2014-06-29 10:31:49 tkych

;; cl-plus/src/dev-util.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Utilities for CL-PLUS Development
;;====================================================================

(in-package :cl-user)

(defpackage #:cl-plus.src.dev-util
  (:documentation
   "Utilities for cl-plus development. Don't export as cl-plus utlilities.")
  (:export #:*optimization-qualities*
           #:append1
           #:last1
           #:copy-empty-array
           #:copy-empty-hash-table)
  (:use #:cl))

(in-package #:cl-plus.src.dev-util)


;;--------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *optimization-qualities* '((speed 3) (safety 0) (debug 0))))

(declaim (inline append1))
(defun append1 (lst obj)
  (append lst (list obj)))

(declaim (inline last1))
(defun last1 (lst)
  (car (last lst)))

(declaim (inline copy-empty-array))
(defun copy-empty-array (array)
  (make-array (array-dimensions array)
              :element-type (array-element-type array)
              :adjustable   (and (array-has-fill-pointer-p array)
                                 (fill-pointer array))
              :fill-pointer (adjustable-array-p array)))

(declaim (inline copy-empty-hash-table))
(defun copy-empty-hash-table (hash-table)
  (make-hash-table :test             (hash-table-test hash-table)
                   :size             (hash-table-size hash-table)
                   :rehash-size      (hash-table-rehash-size hash-table)
                   :rehash-threshold (hash-table-rehash-threshold hash-table)))


;;====================================================================
