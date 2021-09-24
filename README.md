# Autologger - Simple, Automatic (?) Logging for Common Lisp

Autologger is a simple (and hopefully convenient) logging tool for Common Lisp, built on the back of [Image Based Common Lisp (IBCL) by Pascal J. Bourguignon](https://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/ibcl/), a very useful tool for tracking the source code of any functions you define, among other things.

The purpose of Autologger is to **enable logging without polluting your codebase** and allow for **convenient, keyboard-based navigation of logging** across nested function calls.


# Part 1: Usage
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

The best part of Autologger is that it will recursively step through a function call and log the arguments/results of all functions logged. The difference to `CL:STEP` is that you can move backwards and upwards as well as we are not stepping through the function call, but rather stepping through a log of the logged function calls within it. Furthermore, you have granular control over which functions to log. The below example partly illustrates this.

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
![Screenshot](https://github.com/ashok-khanna/autologger/blob/5ccc1db899c8b2382df8debd5acecc9c2fa18da5/screenshot.png)

## Exported Functions
Function | Description 
-------- | -----------
*(launch expr)* | Evaluates the supplied form and launches an Emac buffer with the logging results
*(log &rest symbols)* | Turns on logging for the supplied function(s) (accepts a &rest argument)
*(log-all-within &rest symbols)* | Turns on logging for all user-defined functions within the supplied function(s) (accepts a &rest argument)
*(unlog &rest symbols)* | Turns off logging for the supplied function(s) (accepts a &rest argument)
*(unlog-all)* | Remove logging from all functions
*(select-logs symbol)* | Opens an Emacs Buffer to control logging for each function within SYMBOL
*(all-logs)* | Opens an Emacs Buffer listing all logged functions to control logging for each


## Limitations & Future Developments
Autologger is in **experimental and proof of concept state** so there may (likely) be some unintended consequences and bugs. Feel free to fork the project or submit issues for any bugs/improvements. Note that Autologger currently works for functions that do not have any `&optional`, `&key` and `&rest` lambda list keywords. It does not work for macros as well. My todo includes these features and also to improve and document the Elisp code (its more Italian than spaghetti at the moment). I also want to add some unit tests and make the overall program more reliable / intuitive.

# Part 2: Installation
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
