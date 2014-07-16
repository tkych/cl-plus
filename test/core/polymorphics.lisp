;;;; cl-plus/test/core/polymorphics.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Test for Polymorphics 
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.core.polymorphics
  (:export #:?polymorphics)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:?all)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:cl-plus.src.readtables
                #:cl+)
  (:import-from #:cl-plus.src.core.polymorphics
                #:emptyp
                #:size
                #:copy
                #:ref
                #:add
                #:to))

(in-package #:cl-plus.test.core.polymorphics)

(def-suite ?polymorphics :in ?all)
(in-suite ?polymorphics)
(in-readtable cl+)


;;--------------------------------------------------------------------


(test ?emptyp
  )

(test ?size
  )

(test ?copy
  )

(test ?ref
  )

(test ?add
  )

(test ?to
  )


;;====================================================================
