(defsystem "autologger"
  :description "Autologger: A Simple Logging Tool"
  :version "0.0.1"
  :author "Ashok Khanna <ashok.khanna@hotmail.com>"
  :license "AGPL"
  :depends-on ("com.informatimago.common-lisp.lisp.ibcl")
  :serial t
  :components ((:file "cl-autologger"))
  :in-order-to ((test-op (test-op :autologger-test))))
