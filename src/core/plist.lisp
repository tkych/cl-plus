;;;; Last modified: 2014-06-29 10:07:49 tkych

;; cl-plus/src/core/plist.lisp

;; Copyright (c) 2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-plus/LICENSE

;; TODO:
;;  * plist < plist+
;;    subtypep

;;====================================================================
;; Property List
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-plus.src.core.plist
  (:documentation "
Property List
=============

    plist := (and proper-list (satisfies even-length-p) (satisfies symbol-key-p))

")
  (:nicknames #:cl+plist)
  (:export #:plist
           #:plist+
           #:plistp
           #:plistp+
           #:plist=
           #:normalize-plist
           #:doplist)
  (:use #:cl)
  (:import-from #:alexandria
                #:ensure-function
                #:when-let
                #:once-only
                #:with-gensyms
                #:parse-body
                #:make-hash-table)
  (:import-from #:trivial-types
                #:property-list-p))

(in-package #:cl-plus.src.core.plist)


;;--------------------------------------------------------------------
;; plist
;;--------------------------------------------------------------------

(setf (symbol-function 'plistp)
      (symbol-function 'trivial-types:property-list-p))

(defun plistp+ (x)
  (and (listp x)
       (evenp (length x))))

(declaim (inline plistp))
(deftype plist ()
  `(and list (satisfies plistp)))

(declaim (inline plistp+))
(deftype plist+ ()
  `(and list (satisfies plistp+)))


(declaim (inline normalize-plist))
(defun normalize-plist (plist &optional (test 'equal))
  (check-type plist plist+)
  (loop :for (k v . rest) := plist :then rest
        :with checked-keys := '()
        :unless (find k checked-keys :test test)
          :do (push k checked-keys) :and :append (list k v)
        :until (endp rest)))


(setf (documentation 'normalize-plist 'function) "
NORMALIZE-PLIST plist &optional (test 'equal) => copyed-plist

stable

Example
-------
 * (normalize-plist '(:foo 0 :bar 1))
   => (:FOO 0 :BAR 1)

 * (normalize-plist '(:foo 0 :bar 1 :foo 42 :bar 24))
   => (:FOO 0 :BAR 1)
")


(defun plist= (plist1 plist2 &key (key-test #'equal) (val-test #'eql)
                               ensure-unique-keys)
  (check-type plist1 plist+)
  (check-type plist2 plist+)
  (check-type key-test (or function symbol))
  (check-type val-test (or function symbol))
  (if (member (ensure-function key-test) (list #'eq #'eql) :test #'eql)
      (let ((not-found (gensym "NOT-FOUND-")))
        (loop :for (k1 v1 .nil) :on plist1 :by #'cddr
              :for v2 := (getf plist2 k1 not-found)
              :always (and (not (eq v2 not-found))
                           (funcall val-test v1 v2))))
      (flet ((lookup (key plist)
               (loop :for (k v . nil) :on plist :by #'cddr
                     :when (funcall key-test key k)
                       :return (list k v))))
        (if ensure-unique-keys
            (and (= (length plist1) (length plist2))
                 (loop :for (k1 v1 . nil) :on plist1 :by #'cddr
                       :for entry2 := (lookup k1 plist2)
                       :always (and entry2
                                    (funcall val-test v1 (second entry2)))))
            (and
             (loop :with checked-keys := '()
                   :for (k1 v1 . nil) :on plist1 :by #'cddr
                   :unless (member k1 checked-keys :test key-test)
                     :do (push k1 checked-keys)
                         (let ((entry2 (lookup k1 plist2)))
                           (unless (and entry2
                                        (funcall val-test v1 (second entry2)))
                             (return NIL)))
                   :finally (return T))
             ;; MEMO:
             ;; If above only, follwing occurs.
             ;; (plist= '(:foo 0 :bar 1)
             ;;         '(:foo 0 :bar 1 :baz 2)) => T
             (loop :with checked-keys := '()
                   :for (k2 v2 . nil) :on plist2 :by #'cddr
                   :unless (member k2 checked-keys :test key-test)
                     :do (push k2 checked-keys)
                         (let ((entry1 (lookup k2 plist1)))
                           (unless (and entry1
                                        (funcall val-test v2 (second entry1)))
                             (return NIL)))
                   :finally (return T)))))))

(setf (documentation 'plist= 'function) "
PLIST= plist1 plist2 &key (key-test 'equal) (value-test 'eql) ensure-unique-keys => boolean
")


;;--------------------------------------------------------------------
;; doplist
;;--------------------------------------------------------------------
;; Why we introduce new `doplist', though `alexandria:doplist' already exists?
;; Because:
;;   `alexandria:doplist' contains (declare (ignorable key val)).
;;   Though it is sometimes convenient (e.g., when not using keys),
;;   I think it is not consistant with `dolist'.

;; TODO: duplicate-keys

(defmacro doplist ((key val plist &optional result) &body body)
  (multiple-value-bind (tag-statements declarations) (parse-body body)
    (with-gensyms (rest-plist LOOP)
      (once-only (plist)
        `(block nil
           (let ((,rest-plist ,plist))
             (tagbody ,LOOP
                (unless (endp ,rest-plist)
                  (let ((,key (pop ,rest-plist))
                        (,val (if ,rest-plist
                                  (pop ,rest-plist)
                                  (error "~S is malformed plist." ,plist))))
                    ,@declarations
                    (tagbody ,@tag-statements))
                  (GO ,LOOP))))
           ,(when result
              `(let (,key ,val)
                 (declare (ignorable ,key ,val))
                 ,result)))))))


(setf (documentation 'doplist 'function) "
DOPLIST (key val plist &optional result) &body body => result
")

(define-compiler-macro doplist (&whole form (key val plist &optional result) &body body)
  (if (and (integerp cl-plus.src.core.iteration::*loop-unrolling-threshold*)
           (plistp+ plist)
           (<= (/ (length plist) 2)
               cl-plus.src.core.iteration::*loop-unrolling-threshold*))
      `(block nil
         ,@(loop :for (k v . nil) :on plist :by #'cddr
                 :collect `(let ((,key ,k) (,val ,v)) (tagbody ,@body)))
         ,(when result
            `(let (,key ,val)
               (declare (ignorable ,key ,val))
               ,result)))
      form))


;;====================================================================
