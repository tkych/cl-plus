;;;; Last modified: 2014-06-29 10:34:10 tkych

;; cl-plus/cl-plus-test.asd

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; System for CL+ Test
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus-test-asd (:use :cl :asdf))
(in-package #:cl-plus-test-asd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-version-from-readme ()
    (handler-case
        (with-open-file (in (make-pathname
                             :name "README"
                             :type "md"
                             :defaults (or *compile-file-truename*
                                           *load-truename*)))
          (let ((read-lines-upto 10))
            (loop :repeat read-lines-upto
                  :for line := (read-line in t)
                  :when (search "Version:" line :test #'char-equal)
                    :return (string-trim
                             '(#\.)
                             (remove-if-not (lambda (c)
                                              (or (digit-char-p c)
                                                  (char= #\. c)))
                                            line))
                  :finally (return "0.0.0.0.0.0")))) ; missing version:
      (error () "0.0.0.0.0.0.0.0.0.0.0.0"))))

(defsystem #:cl-plus-test
  :name        "cl-plus-test"
  :description "Test for CL-PLUS. https://github.com/tkych/cl-plus"
  :version     #.(parse-version-from-readme)
  :licence     "MIT (see file LICENCE for details)"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :depends-on  (#:cl-plus
                #:fiveam
                #-ccl #:bordeaux-threads
                #+ccl #:trivial-timeout
                #:trivial-features)
  :pathname    "test"
  :components  ((:file "prologue")

                (:module cdr
                 :depends-on ("prologue")
                 :components ((:file "cdr-5")
                              (:file "cdr-8")
                              (:file "cdr-14")))

                (:module clrfi
                 :depends-on ("prologue")
                 :components ((:file "clrfi-1")))
                
                (:module srfi
                 :depends-on ("prologue")
                 :components ((:file "srfi-45")
                              (:file "srfi-41")))
                
                (:module core
                 :depends-on ("prologue")
                 :components ((:file "alist")
                              (:file "plist")
                              (:file "array")
                              (:file "hash-table")
                              (:file "lambda")
                              (:file "sequence")
                              (:file "iteration")
                              (:file "lazy-sequence")
                              (:file "sequens")
                              
                              ;; FIXME: abcl, ecl: don't stop test ?SOME*.LAZY-SEQNENCE.
                              #-(or ecl abcl) (:file "buxis")
                              (:file "tabula")
                              (:file "polymorphics")
                              (:file "function")
                              ))
                
                (:file "epilogue"
                 :depends-on ("prologue" cdr clrfi srfi core))))


;;====================================================================
