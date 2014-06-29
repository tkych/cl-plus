Last modified: 2014-06-29 10:31:18 tkych

<!-- (cl-gfm:preview #p"~/Dropbox/cl-projects/cl-plus/doc/tips.md") -->

((CL+ )): Tips
==============


Switch: CL <-> CL+
------------------

Following two functions are useful for switching cl <-> cl+ at repl.

```lisp
;; Switch to CL from CL+.
(defun cl+user::cl ()
  (in-package :cl-user)
  (named-readtables:in-readtable :standard)
  (setf *print-pprint-dispatch* (copy-pprint-dispatch nil))
  (setf *print-pretty* nil)
  (values))
  
(export 'cl+user::cl :cl+user)

;; Switch to CL+ from CL.
(defun cl-user::cl+ (&optional (syntax :cl+))
  (ql:quickload :cl-plus)
  (in-package :cl+user)
  (when syntax
    (eval (read-from-string
           (format nil "(named-readtables:in-readtable ~S)" syntax))))
  (setf *print-pprint-dispatch*
        (eval (read-from-string "cl+:*cl+pprint-dispatch-table*")))
  (setf *print-pretty* t)
  (values))

(export 'cl-user::cl+ :cl-user)
```


dot-emacs
---------

Config for slime and paredit mode for cl+'s read macros: 

```elisp
;; cf. http://stackoverflow.com/questions/8598116/paredit-curly-brace-matching-in-swank-clojure-repl
(defun setup-cl+slime-repl-paredit ()
  (define-key slime-repl-mode-map (kbd "DEL") 'paredit-backward-delete)
  ;; for caret-reader: ^xy(+ x y)
  (modify-syntax-entry ?^ "'")
  ;; for sharp-brace-reader: #{:foo 0 :bar 1 :baz 2}
  (define-key slime-repl-mode-map (kbd "{") 'paredit-open-curly)
  (define-key slime-repl-mode-map (kbd "}") 'paredit-close-curly)
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  ;; for sharp-bracket-reader: #[1..]
  (define-key slime-repl-mode-map (kbd "[") 'paredit-open-bracket)
  (define-key slime-repl-mode-map (kbd "]") 'paredit-open-bracket)
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")["))

(add-hook 'slime-repl-mode-hook 'setup-cl+slime-repl-paredit)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

(defun setup-cl+slime-paredit ()
  (define-key slime-mode-map (kbd "DEL") 'paredit-backward-delete)
  ;; for caret-reader: ^xy(+ x y)
  (modify-syntax-entry ?^ "'")
  ;; for sharp-brace-reader: #{:foo 0 :bar 1 :baz 2}
  (define-key slime-mode-map (kbd "{") 'paredit-open-curly)
  (define-key slime-mode-map (kbd "}") 'paredit-close-curly)
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  ;; for sharp-bracket-reader: #[1..]
  (define-key slime-mode-map (kbd "[") 'paredit-open-bracket)
  (define-key slime-mode-map (kbd "]") 'paredit-open-bracket)
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")["))

(add-hook 'slime-mode-hook 'setup-cl+slime-paredit)
(add-hook 'slime-mode-hook 'enable-paredit-mode)
```


^(+ %0 %1) <- ^(+ @0 @1)
------------------------

We can chenge the parameter-prefix for shorthand lambda.
For example, we will change @ for %, like clojure.

```lisp
  * (^(+ @0 @1) 40 2) => 42
  * (^(+ %0 %1) 40 2) => ERROR!
  * (eval-when (:compile-toplevel :load-toplevel :execute)
      (setf cl+lambda:*shorthand-lambda-parameter-prefix* #\%))
  * (^(+ %0 %1) 40 2) => 42
  * (^(+ @0 @1) 40 2) => ERROR!
```

NB. This imitational clojure's % is counted from 1 whereas real % is counted from 1.
