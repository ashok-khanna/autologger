# AUTOLOGGER - Simple, Convenient, Automatic (?) Logging for Common Lisp

AUTOLOGGER is a simple (and hopefully convenient) logging tool for Common Lisp, built on the back of [Image Based Common Lisp (IBCL) by Pascal J. Bourguignon](https://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/ibcl/), a very useful tool for tracking the source code of any functions you define, among other things.

AUTOLOGGER has an Emacs interface (written in Elisp), and I want the documentation to serve as a useful guide for anyone trying to write Common Lisp applications with an Emacs front end. Please don't be put off by the length of the below, it is hopefully very simple. I assume that you are familiar with CL to some degree, the below might be a bit advanced for beginners.

## 1. SCREENSHOTS & SUMMARY


## 2. USING AUTOLOGGER
The six functions of AUTOLOGGER are `LOG`, `UNLOG`, `SELECT-LOGS`, `ALL-LOGS`, `UNLOG-ALL` and `LAUNCH`.

`LOG` is used to turn on logging for the supplied function. It accepts one argument (a symbol denoting a function) and *redefines* the supplied function to start logging its inputs and outputs. As an example, the below shows how a simple function SUM is redefined by the autologging process.

```lisp
IBCL-USER> (defun sum (a b)
             (+ a b))

IBCL-USER> (autologger:log 'sum)

;; Now let us view the revised source code (using IBCL's SOURCE function)

IBCL-USER> (source 'sum :function)

;; Output

(DEFUN SUM
    (A B)
  (PROGN
   (INCF AUTOLOGGER::*COUNTER*)
   (LET ((AUTOLOGGER::*LEVELS*
          (LET ((AUTOLOGGER::X (AUTOLOGGER::MY-COPY AUTOLOGGER::*LEVELS*)))
            (VECTOR-PUSH-EXTEND AUTOLOGGER::*COUNTER* AUTOLOGGER::X)
            AUTOLOGGER::X))
         (AUTOLOGGER::*COUNTER* 0))
     (LET ((AUTOLOGGER::RESULT
            (FUNCALL
             (LAMBDA (AUTOLOGGER::*COUNTER* AUTOLOGGER::*LEVELS* A B) (+ A B))
             AUTOLOGGER::*COUNTER* AUTOLOGGER::*LEVELS* A B)))
       (PUSH
        (CONS (AUTOLOGGER::VECTOR-TO-LIST AUTOLOGGER::*LEVELS*)
              (LIST 'SUM '(A B) '((+ A B))
                    (MAPCAR
                     (LAMBDA (AUTOLOGGER::A) (FORMAT NIL "~s" AUTOLOGGER::A))
                     '(A B))
                    (FORMAT NIL "~s" AUTOLOGGER::RESULT)))
        AUTOLOGGER::*FLAT-LIST*)
       AUTOLOGGER::RESULT))))
       
;; Yikes!
```

As you can see, AUTOLOGGER completely redefined our simple SUM function. You don't have to worry about the details, but basically we are adding in some automatic logging into the function. You can turn off logging and return back to the old function with `UNLOG`, as in the below example.

```lisp
IBCL-USER> (autologger-unlog 'sum)

;; Now let us view the source code

IBCL-USER> (source 'sum :function)

(DEFUN SUM
    (A B)
  (+ A B))
  
;; ALl is well again
```

Perhaps the most important function is `LAUNCH`, which evaluates an expression and launches an Emacs Output buffer with the results of the logging.

# 2: Installation
### IBCL Dependency
You will need to switch from the `CL` package and into `IBCL` in your programs to be able to make use of Autologger. For example, you can do the following:

```lisp
(defpackage :my-package
  (:use :ibcl)           ; <-- Use :IBCL here and not :CL
  (:export ...))
```

Of course, you will need to have IBCL installed on your system. You can hotload it with:

```lisp
(ql:quickload :com.informatimago.common-lisp.lisp.ibcl)
```

Alternatively, if you are using ASDF files (for an introduction, [see my guide here](https://ashok-khanna.medium.com/introduction-to-asdf-d25efe2780c2)), you can just add IBCL as a dependency, such as I have done in the system definition for AUTOLOGGER:

```lisp
(defsystem "autologger"
  :description "Autologger: A Simple Logging Tool"
  :version "0.0.1"
  :author "Ashok Khanna <ashok.khanna@hotmail.com>"
  :license "AGPL"
  :depends-on ("com.informatimago.common-lisp.lisp.ibcl")
  :serial t
  :components ((:file "autologger"))
  :in-order-to ((test-op (test-op :autologger-test))))
```
### Installing Autologger



