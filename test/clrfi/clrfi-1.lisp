;;;; cl-plus/test/clrfi/clrfi-1.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; CLRFI-1
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.test.clrfi.clrfi-1
  (:export #:?clrfi-1)
  (:use #:cl #:fiveam)
  (:import-from #:cl-plus-test
                #:?all)
  (:import-from #:cl-plus.src.clrfi.clrfi-1
                #:featurep))

(in-package #:cl-plus.test.clrfi.clrfi-1)

(def-suite ?clrfi-1 :in ?all)
(in-suite ?clrfi-1)

;; Note: We may suppose that TRIVIAL-FEATRURES has been loaded into lisp-image.

;;--------------------------------------------------------------------

(defparameter *os*
  #+:windows :windows
  #+:darwin  :darwin
  #+:linux   :linux
  #+:netbsd  :netbsd
  #+:openbsd :openbsd
  #+:freebsd :freebsd
  #-(or :windows :darwin :linux :netbsd :openbsd :freebsd)
  :common-lisp)

(defparameter *cpu*
  #+:X86     :X86
  #+:X86-64  :X86-64
  #+:PPC     :PPC
  #+:PPC64   :PPC64
  #+:MIPS    :MIPS
  #+:ALPHA   :ALPHA
  #+:SPARC   :SPARC
  #+:SPARC64 :SPARC64
  #+:HPPA    :HPPA
  #+:HPPA64  :HPPA64
  #-(or :X86 :X86-64 :PPC :PPC64 :MIPS :ALPHA :SPARC :SPARC64 :HPPA :HPPA64)
  :common-lisp)

(defparameter *non-feature0* (gensym "NON-FEAURE-0-"))
(defparameter *non-feature1* (gensym "NON-FEAURE-1-"))

;;--------------------------------------------------------------------

(test ?clrfi-1-feature
  (is-true (find :clrfi-1 *features*)))

(test ?featurep.0.symbol
  (is-true  (featurep :common-lisp))
  (is-true  (featurep *os*))
  (is-true  (featurep *cpu*))
  (is-false (featurep nil))
  (is-false (featurep *non-feature0*))
  (signals  type-error (featurep 42))
  (signals  type-error (featurep #(:common-lisp)))
  (signals  type-error (featurep ":common-lisp"))
  (signals  type-error (featurep #*0101))
  (signals  type-error (featurep (make-hash-table))))

(test ?featurep.1.or
  (is-true  (featurep `(:or :common-lisp)))
  (is-true  (featurep `(:or (:or :common-lisp))))
  (is-true  (featurep `(:or :common-lisp ,*os*)))
  (is-true  (featurep `(:or :common-lisp ,*non-feature0* ,*cpu*)))
  (is-true  (featurep `(:or ,*non-feature0* ,*os* :common-lisp)))
  (is-false (featurep `(:or ,*non-feature0*)))
  (is-false (featurep `(:or ,*non-feature0* ,*non-feature1*))))

(test ?featurep.2.and
  (is-true  (featurep `(:and :common-lisp)))
  (is-true  (featurep `(:and (:and :common-lisp))))
  (is-true  (featurep `(:and :common-lisp ,*os* ,*cpu*)))
  (is-false (featurep `(:and :common-lisp ,*non-feature0* ,*cpu*)))
  (is-false (featurep `(:and ,*non-feature0* ,*os* :common-lisp)))
  (is-false (featurep `(:and ,*non-feature0*)))
  (is-false (featurep `(:and ,*non-feature0* ,*non-feature1*))))

(test ?featurep.3.not
  (is-false (featurep `(:not :common-lisp)))
  (is-false (featurep `(:not ,*os*)))
  (is-true  (featurep `(:not (:not ,*os*))))
  (is-true  (featurep `(:not ,*non-feature0*)))
  (is-false (featurep `(:not (:not ,*non-feature0*))))
  (signals  type-error (featurep `(:not :common-lisp ,*non-feature0*)))
  (signals  type-error (featurep `(:not ,*non-feature0* :common-lisp)))
  (signals  type-error (featurep `(:not ,*non-feature0* ,*non-feature1*))))


(test ?featurep.4.composite
  (is-true  (featurep `(:or ,*os* (:not ,*os*))))
  (is-false (featurep `(:not (:or ,*os*, *cpu*))))
  (is-false (featurep `(:and :common-lisp (:not :common-lisp))))
  (is-true  (featurep `(:not (:and ,*non-feature0* ,*cpu*))))
  (is-true  (featurep `(:and :common-lisp (:or ,*non-feature0* ,*cpu*))))
  (is-true  (featurep `(:or :common-lisp (:and ,*non-feature0* ,*cpu*))))
  (is-false (featurep `(:and ,*non-feature0* (:not :common-lisp ,*os*)))) ; short cut.
  (is-true  (featurep `(:or ,*cpu* (:not :common-lisp ,*non-feature0*)))) ; short cut.
  (signals  type-error (featurep `(:and ,*cpu*
                                        (:not :common-lisp ,*os*))))
  (signals  type-error (featurep `(:or ,*non-feature0*
                                       (:not :common-lisp ,*non-feature1*))))
  (signals  type-error (featurep `(:not ,*non-feature0*
                                        (:not :common-lisp ,*non-feature1*)))))


;;====================================================================
