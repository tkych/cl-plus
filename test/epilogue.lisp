;;;; cl-plus/test/epilogue.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;;====================================================================
;; Epilogue for CL-PLUS-TEST
;;====================================================================

(in-package #:cl-plus-test)


(let ((suites '(;; CDR
                cl-plus.test.cdr.cdr-5:?cdr-5
                cl-plus.test.cdr.cdr-8:?cdr-8
                cl-plus.test.cdr.cdr-14:?cdr-14
                
                ;; CLRFI
                cl-plus.test.clrfi.clrfi-1:?clrfi-1

                ;; SRFI
                cl-plus.test.srfi.srfi-41:?srfi-41
                cl-plus.test.srfi.srfi-45:?srfi-45

                ;; CORE
                cl-plus.test.core.lambda:?lambda
                cl-plus.test.core.hash-table:?hash-table
                cl-plus.test.core.sequence:?sequence
                ;; cl-plus.test.core.misc:?misc
                cl-plus.test.core.alist:?alist
                cl-plus.test.core.plist:?plist
                cl-plus.test.core.array:?array
                cl-plus.test.core.iteration:?iteration
                cl-plus.test.core.lazy-sequence:?lazy-sequence
                cl-plus.test.core.sequens:?sequens
                cl-plus.test.core.tabula:?tabula
                
                ;; FIXME: abcl, ecl: don't stop test ?SOME*.LAZY-SEQNENCE
                #-(or abcl ecl) cl-plus.test.core.buxis:?buxis
                cl-plus.test.core.polymorphics:?polymorphics
                cl-plus.test.core.function:?function)))
  (import suites)
  (export suites))

(defun %run-tests (suite)
  (let* ((result-list  (run suite))
         (total-result (every (lambda (r) (typep r 'fiveam::test-passed)) ; !
                              result-list)))
    (explain! result-list)
    total-result))

(defun run-tests (&rest suites)
  "Runs tests for cl-plus. If no suite is supplied, runs all tests."
  (if (null suites)
      (%run-tests '?all)
      (loop :with final-result := t ; ensured at least one suit in the suits.
            :for s :in suites
            :unless (%run-tests s)
              :do (setf final-result nil)
            :finally (return final-result))))


;;====================================================================
