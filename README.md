# Autologger - Simple, Convenient Logging for Common Lisp

AUTOLOGGER is a simple (and hopefully convenient) logging tool for Common Lisp, built on the back of [Image Based Common Lisp (IBCL) by Pascal J. Bourguignon](https://www.informatimago.com/develop/lisp/com/informatimago/small-cl-pgms/ibcl/), a very useful tool for tracking the source code of any functions you define, among other things.

AUTOLOGGER has an Emacs interface (written in Elisp), and I want the documentation to serve as a useful guide for anyone trying to write Common Lisp applications with an Emacs front end.

## Installation
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

Alternatively, if you are using ASDF files (for an introduction, [see my guide here](https://ashok-khanna.medium.com/introduction-to-asdf-d25efe2780c2), you can just add IBCL as a dependency, such as I have done in the system definition for AUTOLOGGER:

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

