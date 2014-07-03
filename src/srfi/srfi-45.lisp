;;;; cl-plus/src/srfi/srfi-45.lisp

;; Copyright (C) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; See cl-plus/LICENSE, for more details.

;;--------------------------------------------------------------------
;; Copyright (C) André van Tonder (2003). All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;--------------------------------------------------------------------

;; The following code is a Common Lisp translation from reference
;; implementation of SRFI-45.


;;====================================================================
;; SRFI-45 for CL
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.srfi.srfi-45
  (:documentation "
SRFI-45 for CL
==============

 * promise
 * promisep
 * copy-promise
 * lazy
 * lazyp
 * eager
 * eagerp
 * delay
 * force


References
----------

 [0] André van Tonder (2004),
     Scheme Request for Implementation 45:
     Primitives for Expressing Iterative Lazy Algorithms.
     http://srfi.schemers.org/srfi-45

 [1] g000001,
     srfi-45, https://github.com/g000001/srfi-45
     (scheme style common lisp translation)
")
  (:nicknames #:cl+srfi-45)
  (:export #:promise
           #:promisep
           #:copy-promise
           #:lazy
           #:eager
           #:delay
           #:force
           #:lazyp
           #:eagerp)
  (:use #:cl))

(pushnew :srfi-45 *features*)

(in-package #:cl-plus.src.srfi.srfi-45)

;;--------------------------------------------------------------------

(defstruct (content (:constructor make-content (mood value))
                    (:conc-name nil))
  (mood nil :type (member eager lazy))
  value)

(defmethod make-load-form ((c content) &optional env)
  (make-load-form-saving-slots c :slot-names '(mood value)
                                 :environment env))

(defstruct (promise (:constructor make-promise (content))
                    (:predicate promisep)
                    (:copier nil)
                    (:conc-name nil)
                    (:print-function
                     (lambda (self stream depth)
                       (declare (ignore depth))
                       (ecase (mood (content self))
                         (lazy  (format stream "[?]"))
                         (eager (format stream "[~S]"
                                        (value (content self))))))))
  (content nil :type content))

(defmethod make-load-form ((p promise) &optional env)
  (make-load-form-saving-slots p :slot-names '(content)
                                 :environment env))

(defun copy-promise (promise)
  (make-promise (copy-content (content promise))))

(defun lazyp (promise)
  (and (promisep promise)
       (eq 'lazy (mood (content promise)))))

(defun eagerp (promise)
  (and (promisep promise)
       (eq 'eager (mood (content promise)))))

(defmacro lazy (exp)
  `(make-promise (make-content 'lazy (lambda () ,exp))))

(defun eager (exp)
  (make-promise (make-content 'eager exp)))

(defmacro delay (exp)
  `(lazy (eager ,exp)))

(defun force (promise)
  (if (not (promisep promise))          ; ADD
      promise                           ; ADD
      (let ((content (content promise)))
        (ecase (mood content)
          (eager (value content))
          (lazy  (let* ((promise* (funcall (value content)))
                        (content  (content promise))) ; *
                   (unless (eq (mood content) 'eager) ; *
                     (setf (mood  content) (mood  (content promise*))
                           (value content) (value (content promise*))
                           (content promise*) content))
                   (force promise)))))))

;; (*) These two lines re-fetch and check the original promise in case
;;     the first line of the let* caused it to be forced.  For an example
;;     where this happens, see reentrancy test 3 (at cl-plus/test/srfi/srfi-45.lisp).


;;====================================================================
