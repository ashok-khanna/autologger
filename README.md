# AUTOLOGGER - Simple, Automatic (?) Logging for Common Lisp

Autologger is a simple (and hopefully convenient) logging tool for Common Lisp, built on the back of [Image Based Common Lisp (IBCL) by Pascal J. Bourguignon](https://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/ibcl/), a very useful tool for tracking the source code of any functions you define, among other things. The program requires Emacs currently, although it is on my list to make it available to other editing enviornments. Autologger is in ***experimental state*** so there may (likely) be some unintended consequences. Feel free to fork the project or submit issues for any bugs/improvements.



# Usage
Autologger works off transforming your functions (not macros) to a version that logs both the incoming arguments and the outgoing results to a global list, which is then displayed by Emacs. You can turn logging on for a function with `LOG` and turn logging off with `UNLOG`:

```lisp
;; You must be working within IBCL and not CL (refer installation notes below)

IBCL > (defun sum (a b) (+ a b))


;; 'LOG' is the package nickname for Autologger. Turn on logging with the function 'LOG':

IBCL > (log:log 'sum)


;; Turn off logging with 'UNLOG':

IBCL > (log:unlog 'sum)
```

You can view logging results for a logged function with `LAUNCH`. This will bring up an Emacs buffer with the log results and ability to navigate up/down/next/previous across logged functions.

```lisp
IBCL > (log:launch (sum 1 2)
```

The best part of Autologger is that it will recursively step through a function call and log the arguments/results of all functions logged. The difference to STEP is that you can move backwards and upwards as well as we are not stepping through the function call, but rather stepping through a log of the logged function calls within it. Furthermore, you have granular control over which functions to log. The below example partly illustrates this.

```lisp
(defun sum (a b) (+ a b))
(defun prd (a b) (* a b))
(defun div (a b) (/ a b))

(defun complex-fn (a b)
  (if (equal a 0)
      (sum a (sum b (sum a b)))
      (div (prd b (sum a b)) a)))
      
(log:log 'sum)
(log:log 'prd)
(log:log 'div)
(log:log 'complex-fn)

(log:launch (complex-fn 1 2))
```

Note that Autologger currently works for functions that do not have any `&optional`, `&key` and `&rest` lambda list keywords. It does not work for macros as well.

# Installation
Autologger consists of two modules, `cl-autologger.lisp` and `autologger.asd` for Common Lisp and *el-autologger.el* for Emacs/Elisp. It is also built upon IBCL. You can download IBCL with the following:

```lisp
(ql:quickload :com.informatimago.common-lisp.lisp.ibcl)
```


## Installing Emacs Components
To install the Emacs files, download `el-autologger.el` to a directory of your choice and add the following to your `.emacs` and evaluate both forms. The first is required as we will manipulate Emacs from Autologger.

```elisp
(setq slime-enable-evaluate-in-emacs t)
(load "path-to-where-you-saved-autologger/el-autologger.el")
```
## Installing Common Lisp Components
There are two ways to install the Common Lisp files. You can save down both *autologger.asd* and *cl-autologger.lisp* within a folder that ASDF can scan (for those not familiar with ASDF, [you can read my 5 minute guide on it](https://ashok-khanna.medium.com/introduction-to-asdf-d25efe2780c2)). Then you simply need to run

```lisp
(asdf:load-system "autologger")
```

Alternatively, you can do the following (note that in this approach you need to manually load IBCL via Quicklisp each time you use Autologger):

```lisp
(ql:quickload :com.informatimago.common-lisp.lisp.ibcl)
(load "path-to-where-you-saved-autologger/cl-autologger.lisp")
```

## Final Setup Steps
You will need to switch from the `CL` package and into `IBCL` in your programs to be able to make use of Autologger. If you are working in REPL, you can simply do `(in-package :ibcl)`. If you are working in files, switch the dependency from CL to IBCL, such as in the following.

```lisp
(defpackage :my-package
  (:use :ibcl)           ; <-- Use :IBCL here and not :CL
  (:export ...))
```




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

### Installing Autologger



