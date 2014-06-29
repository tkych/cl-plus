;;;; Last modified: 2014-05-28 21:18:29 tkych

;; cl-plus/src/clrfi/clrfi-1.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; CLRFI-1
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.clrfi.clrfi-1
  (:documentation "
CLRFI-1
=======

Reference
---------

 [0] Heow Eide-Goodman and Marco Antoniotti,
     CLRFI 1: FEATUREP.
")
  (:export #:featurep)
  (:nicknames #:cl+clrfi-1)
  (:use #:cl)
  (:import-from #:cl-plus.src.dev-util
                #:*optimization-qualities*))

(in-package #:cl-plus.src.clrfi.clrfi-1)

(pushnew :clrfi-1 *features*)


;;--------------------------------------------------------------------

(defun featurep (feature-expression)
  (declare (optimize . #.*optimization-qualities*))
  (etypecase feature-expression
    (symbol
     (and (member feature-expression *features*)
          t))
    (cons
     (ecase (first feature-expression)
       (:and
        (every #'featurep (rest feature-expression)))
       (:or
        (some #'featurep (rest feature-expression)))
       (:not
        (check-type (cddr feature-expression) null)
        (not (featurep (second feature-expression))))))))


;;====================================================================
