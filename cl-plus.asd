;;;; Last modified: 2014-06-29 09:55:51 tkych

;; cl-plus/cl-plus.asd

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; CL-PLUS: Interface for Rapid Prototyping
;;====================================================================
;; cl-plus/
;;   README.md
;;   LICENSE
;;   CHANGELOG
;;   cl-plus.asd
;;   cl-plus-test.asd
;;   src/
;;     packages.lisp
;;     readtables.lisp
;;     pprint-dispatch-table.lisp
;;     dev-util.lisp
;;     cdr/
;;       cdr-5.lisp
;;       cdr-8.lisp
;;       cdr-14.lisp
;;     clrfi/
;;       clrfi-1.lisp
;;     srfi/
;;       srfi-45.lisp
;;       srfi-41.lisp
;;     core/
;;       lambda.lisp
;;       misc.lisp
;;       iteration.lisp
;;       sequence.lisp
;;       lazy-sequence.lisp
;;       sequens.lisp
;;       hash-table.lisp
;;       alist.lisp
;;       plist.lisp
;;       tabula.lisp
;;       buxis.lisp
;;       polymorphics.lisp
;;       function.lisp
;;   test/
;;     prologue.lisp
;;     epilogue.lisp
;;     cdr/
;;       cdr-5.lisp
;;       cdr-8.lisp
;;     srfi/
;;       srfi-45.lisp
;;       srfi-41.lisp
;;     core/
;;       lambda.lisp
;;       iteration.lisp
;;       sequence.lisp
;;       lazy-sequence.lisp
;;       sequens.lisp
;;       hash-table.lisp
;;       alist.lisp
;;       plist.lisp
;;       tabula.lisp
;;       buxis.lisp
;;       polymorph.lisp
;;       function.lisp


;;====================================================================
;; System for CL-PLUS
;;====================================================================

(in-package #:cl-user)
(defpackage #:cl-plus-asd (:use :cl :asdf))
(in-package #:cl-plus-asd)

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

(defsystem :cl-plus
  :name        "cl-plus"
  :description "Interface for Rapid Prototyping. https://github.com/tkych/cl-plus"
  :version     #.(parse-version-from-readme)
  :licence     "MIT"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :depends-on  (#:alexandria
                #:trivial-types
                #:split-sequence
                #:named-readtables)
  :pathname    "src"
  :components  ((:file "packages"
                 :depends-on (core "readtables" "pprint-dispatch-table"))
                (:file "readtables"
                 :depends-on (core))
                
                (:file "pprint-dispatch-table"
                 :depends-on (core))
                
                (:file "dev-util")
                
                (:module cdr
                 :pathname "cdr"
                 :depends-on ("dev-util")
                 :components ((:file "cdr-5")
                              (:file "cdr-8")
                              (:file "cdr-14"
                               :depends-on ("cdr-5" "cdr-8"))))

                (:module clrfi
                 :pathname "clrfi"
                 :depends-on ("dev-util")
                 :components ((:file "clrfi-1")))

                (:module srfi
                 :pathname "srfi"
                 :depends-on ("dev-util")
                 :components ((:file "srfi-45")
                              (:file "srfi-41"
                               :depends-on ("srfi-45"))))

                (:module core
                 :pathname "core"
                 :depends-on (cdr srfi "dev-util")
                 :components ((:file "lambda")
                              
                              (:file "iteration")
                              
                              (:file "sequence")
                              (:file "lazy-sequence")
                              (:file "sequens"
                               :depends-on ("lazy-sequence"))
                              
                              (:file "hash-table"
                               :depends-on ("iteration"))
                              (:file "alist"
                               :depends-on ("iteration"))
                              (:file "plist"
                               :depends-on ("iteration"))
                              (:file "tabula"
                               :depends-on ("alist" "plist" "hash-table"))
                              
                              (:file "array"
                               :depends-on ("iteration"))
                              (:file "buxis"
                               :depends-on ("iteration" "tabula"
                                            "lazy-sequence" "sequens"))

                              (:file "polymorphics"
                               :depends-on ("lazy-sequence" "sequens"
                                            "tabula"))
                              
                              (:file "function")
                              ;; (:file "alias")
                              ))
                )

  ;; for asdf 3
  ;; :in-order-to ((test-op (test-op :cl-plus-test)))
  ;; :perform (test-op (o c) (symbol-call :cl-plus-test :run-tests))
  )


(defmethod perform ((o test-op) (s (eql (find-system :cl-plus))))
  (declare (ignore o s))
  (load-system :cl-plus-test)
  (funcall (read-from-string "cl+t:run-tests")))

(defmethod perform :after ((o load-op) (s (eql (find-system :cl-plus))))
  (declare (ignore o s))
  (pushnew :cl+ *features*))


;;====================================================================
