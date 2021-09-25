;;;; -*- mode:lisp;coding:utf-8 -*-
;;;; *************************************************************************
;;;; AUTHORS
;;;;    <AK> Ashok Khanna <ashok.khanna@hotmail.com>
;;;; MODIFICATIONS
;;;;    2021-09-24 <AK> Initial Version
;;;; BUGS
;;;;   Logging functions when not in IBCL causes errors
;;;;   Logged functions cannot have &key, &optional and &rest parameters as
;;;;     we use a funcall/lambda in LOG-DEFUN and such arguments cannot be easily
;;;;     spliced into the funcall [We will try and modify in the future to allow this]
;;;;   Only logs functions, not macros (no type check currently applied to
;;;;     prevent this)
;;;;   Does not have full code walker, so when trying to collect all functions
;;;;     within a function, any variables that appear in the CAR of a list,
;;;;     e.g. (let ((A ...) will spuriously be collected if they are also
;;;;     bound to a function at the top-level)
;;;; LEGAL
;;;;    AGPL3
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;; *************************************************************************


(defpackage :autologger
  (:use :ibcl)
  (:shadow :log)
  (:export :log :unlog :unlog-all :launch :select-logs :all-logs :log-all-within)
  (:nicknames :log)
  (:documentation " 

[To Add]."))

(in-package :autologger)



;;; 0.0 Special Variables
;;; *************************************************************************

(defparameter *logged-functions* (make-hash-table)
  "Hash table of functions being logged (key) and their source definitions
prior to tranformation by autologger (value).")

(defparameter *flat-list* nil
  "A flat list of logging results, together with their nesting level
within the function call. Emacs front-end manipulates this to present nested
logging results.")

(defparameter *counter* 0
  "Counter reflecting position of logged function call within its parent function.")


;; Note that we use vector not lists simply because I wanted to learn
;; how to use vectors in my code

(defparameter *levels* (make-array 1 :fill-pointer 0 :adjustable t)
  "Vector reflecting nested position of logged function call.
For example, #(1 2 3) is the third function logged within the second function logged
within the first function logged within the top-level function call.")

;; Technically DEFUN is a macro and also not a higher order function
;; However, for our purposes we include it in the below because we want
;; to pick up name in (DEFUN name ...)

(defvar *higher-order-cl-functions*
  (list 'funcall 'apply 'mapcar 'mapc 'defun
	'maplist 'mapcan 'mapcon 'mapl)
  "Partial list of functions that accept a function as their first argument.")


;; Thanks to PJB

(defparameter *cl-functions* (let ((macros nil) (funs nil) (specops nil))
			       (declare (ignore macros))
			       (declare (ignore specops))
			       (do-external-symbols (s "CL")
				 (cond ((special-operator-p s) (push s funs))
				       ((macro-function s)     (push s funs))
				       ((fboundp s)            (push s funs))))
			 funs)
  "List of standard CL functions (refer DEFPARAMETER form below).")



;;; 1.0 Primary Logging Function
;;; *************************************************************************

(defmacro log-defun (name parameters &body body)
  "Transforms supplied source code into an equivalent function that logs
its arguments and results into global variable *flat-list*."
  `(defun ,name ,parameters
     (incf *counter*)
     (let ((*levels* (generate-logging-level *levels* *counter*))
	   (*counter* 0))
       (let ((result (funcall (lambda (*counter* *levels* ,@parameters) ,@body) *counter* *levels* ,@parameters)))
	 (push-to-log *levels* ',name ',parameters ',body (list ,@parameters) result)
	 result))))

(defun generate-logging-level (levels counter)
  "Generate a new logging level by appending counter to the existing level."
  (let ((x (vector-copy levels)))
    (vector-push-extend counter x)
    x))

(defun push-to-log (logging-level function-name function-parameters function-body function-arguments function-result)
  "Pushes logging data to *flat-list* as strings (except logging-level, which is a list).
We use lists here vs. a structure as we transfer this data to Emacs via Swank."
  (push (cons (vector-to-list logging-level)
	      (list function-name
		    function-parameters
		    function-body
		    (mapcar (lambda (a) (write-to-string a)) function-arguments)
		    (write-to-string function-result)))
	*flat-list*))



;;; 2.0 Exported Functions
;;; *************************************************************************

;; Autologger is typically called from top-level

(defmacro launch (expr)
  "Evalautes an expression and then displays logging results within an Autologger Buffer."
  `(progn
     (reset-log-data)
     (let ((result ,expr))
       (swank::eval-in-emacs
	`(autologger-create-buffer ',*flat-list*))
       result)))

(defun log (&rest symbols)
  "Add a function to the *logged-functions* table unless it already is there.
Store its unlogged source code within the hash table."
  (loop for symbol in symbols
     do (unless (gethash symbol *logged-functions*)
	  (let ((source-def (ibcl:source symbol :function)))
	    (unless (or (contains-lambda-list-keywords source-def)
			(check-null source-def))
	      (setf (gethash symbol *logged-functions*) source-def)
	      (eval `(log-defun ,(second source-def) ,(third source-def) ,@(nthcdr 3 source-def))))))))

(defun unlog (&rest symbols)
  "Remove a function from the *logged-functions* table
and reset it to its old definition (prior to transformation to include logging)."
  (loop for symbol in symbols
     do (progn
	  (let ((orig-source (gethash symbol *logged-functions*)))
	    (eval `(ibcl:defun ,(second orig-source) ,(third orig-source) ,@(nthcdr 3 orig-source))))
	  (remhash symbol *logged-functions*))))

(defun log-all-within (&rest symbols)
  "Turn logging on for all user-defined top-level functions within FUNCTION-NAME."
  (loop for function-name in symbols
     do (let ((function-symbols (get-functions-in-definition function-name)))
	  (apply #'log function-symbols))))

(defun unlog-all ()
  "Unlog all functions in *logged-functions*."
  (loop for key being the hash-keys of *logged-functions*
     do (unlog key))
  (print "All logged functions cleared."))

(defun select-logs (function-name)
  "Extract all user-defined top-level functions within FUNCTION-NAME and render Autologger's Emacs Menu Buffer."
  (let ((function-symbols (get-functions-in-definition function-name)))
    (let ((log-symbols-list (log-symbols-list function-name function-symbols)))
      (print log-symbols-list)
      (swank::eval-in-emacs `(autologger-create-menu-buffer ',log-symbols-list)))))

(defun all-logs ()
  "Extract all logged functions found in *logged-functions* and render Autologger's Emacs Menu Buffer."
  (let ((log-symbols-list (log-symbols-list "all-logs" (hash-keys *logged-functions*))))
    (swank::eval-in-emacs `(autologger-create-menu-buffer ',log-symbols-list))))



;;; 3.0 Internal Functions
;;; *************************************************************************

(defun contains-lambda-list-keywords (source-def)
  "Returns T if function's lambda list contains lambda list keywords and prints this to screen."
  (when (loop for item in (third source-def)
	   thereis (find item '(&key &rest &optional) :test #'symbol-name-equal-p))
    (format t "Cannot log ~s due to presence of lambda-list keywords (&key, &rest and/or &optional)~%" (second source-def))
    t))

(defun check-null (source-def)
  "Returns T if function source definition cannot be found in IBCL."
  (when (null source-def)
    (format t "Cannot log ~s as unable to find source definition in IBCL~%" (second source-def))
    t))

(defun reset-log-data ()
  "Reset global variables used for logging to their starting values."
  (setf *flat-list* nil)
  (setf *counter* 0)
  (setf *levels* (make-array 1 :fill-pointer 0 :adjustable t)))

(defun get-functions-in-definition (function-symbol)
  (let ((function-definition
	 (or (gethash function-symbol *logged-functions*)
	     (source function-symbol :function))))
    (let ((collected-functions nil))
      (declare (special collected-functions))
      (recursive-collect-functions function-definition)
      (keep-only-user-functions collected-functions))))

(defun keep-only-user-functions (collected-functions)
  "Remove all symbols that are not user-defined top-level functions and also any duplicates."
  (remove-duplicates (remove-if-not #'(lambda (a)
					(and (fboundp a)
					     (not (macro-function a))))
				    collected-functions)))

(defun recursive-collect-functions (fn-list)
  (declare (special collected-functions))
  (cond ((null fn-list) nil)
	((atom fn-list) nil)
	((listp (car fn-list))(recurse-collect-functions fn-list))
	((higher-order-cl-function-p (car fn-list))
	 (cond ((listp (second fn-list))
		(recurse-collect-functions-cdr fn-list))
	       ((cl-function-p (second fn-list))
		(recurse-collect-functions-cddr fn-list))
	       (t (push (cadr fn-list) collected-functions)
		  (recurse-collect-functions-cddr fn-list))))
	((cl-function-p (car fn-list))
	 (recurse-collect-functions-cdr fn-list))
	(t
	 (push (car fn-list) collected-functions)
	 (recurse-collect-functions-cdr fn-list))))

(defun recurse-collect-functions (fn-list)
  "Recurse RECURSIVE-COLLECT-FUNCTION across all items in FN-LIST.
When the CAR of FN-LIST is a list itself, then we treat all items of
FN-LIST as forms that potentially contain functions and hence we recurse
across all items of FN-LIST."
  (declare (special collected-functions))
  (loop for item in fn-list do (recursive-collect-functions item)))

(defun recurse-collect-functions-cdr (fn-list)
  "Recurse RECURSIVE-COLLECT-FUNCTION across all items in FN-LIST except first.
When the CAR of FN-LIST is an atom, we analyse it for whether it is a user-defined
top-level funciton, and (regardless of the outcome of that analysis) recurse down
the remaining items of FN-LIST which are the arguments to the CAR of the FN-LIST
and themselves may be forms containing functions."
  (declare (special collected-functions))
  (loop for item in (cdr fn-list) do (recursive-collect-functions item)))

(defun recurse-collect-functions-cddr (fn-list)
  "Recurse RECURSIVE-COLLECT-FUNCTION across all items in FN-LIST except first two.
When the CAR of FN-LIST is a higher order function, we analyse the second item of
FN-LIST. If that is an atom, we need to recurse down all the items of FN-LIST after
the second."
  (declare (special collected-functions))
  (loop for item in (cddr fn-list) do (recursive-collect-functions item)))

(defun higher-order-cl-function-p (symbol)
  "Returns true/false on whether SYMBOL represents a standard CL function."
  (find symbol *higher-order-cl-functions* :test #'symbol-name-equal-p))

(defun cl-function-p (symbol)
  "Returns true/false on whether SYMBOL represents a standard CL function."
  (find symbol *cl-functions* :test #'symbol-name-equal-p))

(defun log-symbols-list (function-name function-symbols)
  "Destructures a list of function symbols associated with a function
(see GET-FUNCTIONS-IN-DEFINITION) into strings that can be parsed by Emacs Autologger Buffer."
  (cons (write-to-string function-name)
	(loop for symbol in function-symbols
	   collect (cons (destructure-symbol symbol)
			 (hash-exists symbol *logged-functions*)))))

(defun destructure-symbol (symbol)
  "Destructure SYMBOL into a list of SYMBOL-NAME, PACKAGE-NAME and STATUS."
  (list (symbol-name symbol)
	(package-name (symbol-package symbol))
	(multiple-value-bind (name status)
	    (find-symbol (symbol-name symbol) (symbol-package symbol))
	  (declare (ignore name))
	  (write-to-string status))))

(defun hash-exists (key table)
  "Returns true/false on whether a hash entry exists for KEY in TABLE."
  (when (gethash key table) t))

;; We keep this internal as we only call it from Emacs and users
;; shouldn't have a reason to call it directly

(defun set-function-logs (emacs-symbol-data-list)
  "Modify logging state for functions supplied by Emacs Autologger Menu Buffer."
  (print emacs-symbol-data-list)
  (loop for item in emacs-symbol-data-list
     do (destructuring-bind ((name package) . log-status) item
	  (let ((function-symbol (find-symbol name package)))
	    (if (equal log-status ":yes-log")
		(unless (gethash function-symbol *logged-functions*)
		  (when (fboundp function-symbol) (log function-symbol)))
		(when (gethash function-symbol *logged-functions*)
		  (unlog function-symbol)))))))



;;; 4.0 Helper Functions
;;; *************************************************************************

(defun symbol-name-equal-p (a b)
  "Test if A and B have the same symbol name."
  (when (and (symbolp a) (symbolp b))
    (equal (symbol-name a) (symbol-name b))))

(defun vector-copy (vector)
  "Make a copy of VECTOR."
  (map-into
   (make-array (array-dimensions vector) :adjustable (adjustable-array-p vector)
               :fill-pointer (fill-pointer vector))
   #'copy-tree vector))

(defun vector-to-list (vector)
  "Convert VECTOR to a list."
  (coerce vector 'list))

(defun hash-keys (hash-table)
  "Return the keys of HASH-TABLE."
  (loop for key being the hash-keys of hash-table
     collect key))



