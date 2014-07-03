;;;; cl-plus/src/core/pprint-dispatch-table.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; PPrint-Dispatch-Table for CL+
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.pprint-dispatch-table
  (:use #:cl)
  (:nicknames #:cl+pprint-dispatch-table)
  (:export #:*cl+pprint-dispatch-table*)
  (:import-from #:cl-plus.src.core.hash-table
                #:pprint-readable-hash-table))

(in-package #:cl-plus.src.pprint-dispatch-table)


;;--------------------------------------------------------------------

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defparameter *cl+pprint-dispatch-table* (copy-pprint-dispatch nil)))

(defparameter *cl+pprint-dispatch-table* (copy-pprint-dispatch nil))

(set-pprint-dispatch 'hash-table
                     #'pprint-readable-hash-table
                     42 *cl+pprint-dispatch-table*)


;;====================================================================
