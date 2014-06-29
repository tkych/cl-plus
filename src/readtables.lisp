;;;; Last modified: 2014-06-29 10:32:07 tkych

;; cl-plus/src/core/readtables.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Readtables
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.readtables
  (:use #:cl)
  (:export #:cl+
           #:cl+caret
           #:cl+sharp-brace
           #:cl+sharp-bracket)
  (:import-from #:cl-plus.src.core.lambda
                #:caret-reader)
  (:import-from #:cl-plus.src.core.hash-table
                #:sharp-brace-reader
                #:*print-readable-hash-table*)
  (:import-from #:cl-plus.src.core.lazy-sequence
                #:sharp-bracket-reader)
  (:import-from #:named-readtables
                #:defreadtable))

(in-package #:cl-plus.src.readtables)


;;--------------------------------------------------------------------
;; Readtables
;;--------------------------------------------------------------------

(defreadtable :cl+
  (:merge :standard)
  (:macro-char #\^ #'caret-reader)
  (:dispatch-macro-char #\# #\{ #'sharp-brace-reader)
  (:dispatch-macro-char #\# #\[ #'sharp-bracket-reader))

(defreadtable cl+
  (:merge :standard)
  (:macro-char #\^ #'caret-reader)
  (:dispatch-macro-char #\# #\{ #'sharp-brace-reader)
  (:dispatch-macro-char #\# #\[ #'sharp-bracket-reader))

(defreadtable cl+caret
  (:merge :standard)
  (:macro-char #\^ #'caret-reader))

(defreadtable cl+sharp-brace
  (:merge :standard)
  (:dispatch-macro-char #\# #\{ #'sharp-brace-reader))

(defreadtable cl+sharp-bracket
  (:merge :standard)
  (:dispatch-macro-char #\# #\[ #'sharp-bracket-reader))


;;====================================================================
