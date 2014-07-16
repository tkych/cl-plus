;;;; cl-plus/test/cdr/cdr-14.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for CDR-14 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.cdr.cdr-14
  (:export #:?cdr-14)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:?all))

(in-package #:cl-plus.test.cdr.cdr-14)

(def-suite ?cdr-14 :in ?all)
(in-suite ?cdr-14)

;;--------------------------------------------------------------------

(test ?*features*
  (is-true (find :cdr-5 *features*))
  (is-true (find :cdr-8 *features*)))


;;====================================================================
