;;;; cl-plus/src/cdr/cdr-14.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; CDR-14: Detecting Implementation of CDR's in Common Lisp Runtimes
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.cdr.cdr-14
  (:documentation "
CDR-14
======

Reference
---------

 [0] Marco Antoniotti.
     CDR 14: Detecting Implementation of CDR's in Common Lisp Runtimes.
     http://cdr.eurolisp.org/document/14/
")
  (:use #:cl))

(in-package #:cl-plus.src.cdr.cdr-14)

;;--------------------------------------------------------------------
;; CDR-5: Sub-interval Numerical Types for Common Lisp
;;--------------------------------------------------------------------

(pushnew :cdr-5 *features*)


;;--------------------------------------------------------------------
;; CDR-8: Generic Equality and Comparison for Common Lisp
;;--------------------------------------------------------------------

(pushnew :cdr-8 *features*)


;;--------------------------------------------------------------------
;; CDR-13: Priority Queues for Common Lisp
;;--------------------------------------------------------------------
;; TODO:

;;  (pushnew :cdr-13 *features*)


;;====================================================================
