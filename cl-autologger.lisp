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

(defvar *cl-functions* nil
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
	    (unless (contains-lambda-list-keywords source-def)
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

(defun contains-lambda-list-keywords (function-definition)
  "Returns T if function's lambda list contains lambda list keywords and prints this to screen."
  (when (loop for item in (third function-definition)
	   thereis (find item '(&key &rest &optional) :test #'symbol-name-equal-p))
    (format t "Cannot log ~s due to presence of lambda-list keywords (&key, &rest and/or &optional)~%" (second function-definition))
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



;;; 5.0 CL Function Symbols
;;; *************************************************************************

;; List of all CL Standard Function Symbols

;; Created with the following:
(defun get-all-symbols (&optional package)
  (let ((lst ())
        (package (find-package package)))
    (do-all-symbols (s lst)
      (when (fboundp s)
        (if package
            (when (eql (symbol-package s) package)
              (push s lst))
            (push s lst))))
    lst))


(let ((macros 0) (funs 0) (specops 0)) 
           (do-external-symbols (s "CL")
             (cond ((special-operator-p s) (incf specops))
                   ((macro-function s)     (incf macros))
                   ((fboundp s)            (incf funs))))
           (list :macros macros :functions funs :special-operators specops))

(defparameter *cl-function-symbols* (get-all-symbols :cl))
;;
;; https://stackoverflow.com/questions/1511981/how-to-examine-list-of-defined-functions-from-common-lisp-repl-prompt

(defparameter *cl-functions* '(UNION VALUES * FLOAT CHAR FUNCTION COMPUTE-APPLICABLE-METHODS REMOVE-METHOD
			       MAKE-INSTANCE ENSURE-GENERIC-FUNCTION CLASS-NAME ALLOCATE-INSTANCE
			       METHOD-QUALIFIERS ADD-METHOD FUNCTION QUOTE CLASS-NAME METHOD-QUALIFIERS
			       NSUBST COPY-PPRINT-DISPATCH GENTEMP CONTINUE SYMBOL-FUNCTION BIT-AND
			       LISTP CHAR-GREATERP MAKE-DISPATCH-MACRO-CHARACTER LAST SOFTWARE-TYPE
			       DOTIMES INVOKE-DEBUGGER PPRINT-FILL DESTRUCTURING-BIND
			       MAKE-BROADCAST-STREAM PEEK-CHAR MACRO-FUNCTION MACROEXPAND-1
			       SLOT-UNBOUND NOT FROUND PPRINT-DISPATCH THIRD 1- STRING<= CAADAR CADDR
			       PSETF PATHNAME-NAME AREF SHIFTF CAAADR CDADDR PRINC-TO-STRING CLRHASH
			       FUNCALL BIT-ANDC1 NBUTLAST MAP-INTO GET-MACRO-CHARACTER + DEFMETHOD READ
			       MAPCON SQRT ADJOIN ALPHA-CHAR-P RETURN TRUNCATE MAKE-HASH-TABLE
			       ENSURE-DIRECTORIES-EXIST FUNCTION PARSE-INTEGER AND SUBSTITUTE-IF-NOT
			       SET-DIFFERENCE MACROEXPAND GET-UNIVERSAL-TIME SLOT-VALUE PRIN1-TO-STRING
			       EQ FLOAT MAKE-STRING SIMPLE-BIT-VECTOR-P MULTIPLE-VALUE-PROG1
			       DEFINE-CONDITION POSITION-IF SUBSEQ KEYWORDP SHORT-SITE-NAME CDAR
			       UPGRADED-ARRAY-ELEMENT-TYPE LABELS MISMATCH ARRAY-HAS-FILL-POINTER-P
			       COPY-LIST HASH-TABLE-TEST GENSYM UNEXPORT CHANGE-CLASS GETHASH CONJUGATE
			       CONSP STRING-RIGHT-TRIM WITH-SIMPLE-RESTART RASSOC ASINH FIND-CLASS
			       REMOVE-IF-NOT ASH CHAR>= TAN CDDDAR ROOM SECOND PPRINT-INDENT
			       NSET-EXCLUSIVE-OR MAKE-TWO-WAY-STREAM ASIN LET* NSTRING-CAPITALIZE
			       WITH-OUTPUT-TO-STRING SYMBOL-NAME MAKE-SEQUENCE NCONC STRING-NOT-EQUAL
			       PACKAGE-SHADOWING-SYMBOLS UNREAD-CHAR MACHINE-VERSION ARRAY-DIMENSION
			       DESCRIBE-OBJECT VECTORP VECTOR SIN SBIT SETQ STRING<
			       PRINT-NOT-READABLE-OBJECT LIST* NSET-DIFFERENCE
			       PPRINT-EXIT-IF-LIST-EXHAUSTED EXP TAGBODY COPY-SYMBOL CHAR-UPCASE
			       FUNCTION-LAMBDA-EXPRESSION INVOKE-RESTART-INTERACTIVELY NTHCDR
			       STORE-VALUE FILL-POINTER LOGCOUNT MAKUNBOUND GET ERROR
			       DIRECTORY-NAMESTRING SCHAR SYNONYM-STREAM-SYMBOL LIST = THE
			       REMOVE-METHOD STRING-UPCASE FBOUNDP WILD-PATHNAME-P DELETE-FILE
			       CTYPECASE REM LOGEQV SHADOW CAADDR FFLOOR READTABLE-CASE NULL EXPT
			       VALUES-LIST USE-VALUE FIFTH CLEAR-INPUT COMPILE IGNORE-ERRORS NUNION
			       CADDAR PRINT-OBJECT * IN-PACKAGE DEFCONSTANT APROPOS-LIST
			       PATHNAME-DEVICE CHAR-INT WRITE-STRING FILE-AUTHOR MAPL FLOAT-PRECISION
			       STRING-DOWNCASE INTERSECTION LET BYTE-POSITION DO NTH CDDR
			       DELETE-DUPLICATES CADR CDAAR LOG ASSOC-IF-NOT BIT-ANDC2
			       WITH-STANDARD-IO-SYNTAX SET-PPRINT-DISPATCH NSUBST-IF PRINT DENOMINATOR
			       DIGIT-CHAR-P WHEN DPB NSUBSTITUTE BIT-EQV WITH-OPEN-FILE
			       STREAM-EXTERNAL-FORMAT MAKE-PACKAGE REVERSE PRIN1 GET-SETF-EXPANSION
			       PACKAGE-USE-LIST PPRINT-NEWLINE STRING MAPCAR CAAAR FLOATP
			       MULTIPLE-VALUE-SETQ BIT-ORC1 PACKAGE-USED-BY-LIST PROG1 CLEAR-OUTPUT
			       CONS SUBST-IF UPDATE-INSTANCE-FOR-DIFFERENT-CLASS >= RETURN-FROM
			       STRING>= CHAR< NSUBLIS MACHINE-TYPE REALPART SET-EXCLUSIVE-OR
			       LISP-IMPLEMENTATION-TYPE DIGIT-CHAR CASE INPUT-STREAM-P
			       WITH-CONDITION-RESTARTS TYPE-OF SVREF STRING/= RENAME-FILE WRITE-LINE
			       FILE-WRITE-DATE CDDADR DRIBBLE CADAR REDUCE CDDAAR LOGORC1 FIRST
			       HASH-TABLE-P ABORT PPRINT-POP CADAAR LOWER-CASE-P EVAL NUMERATOR
			       RATIONAL BUTLAST PROBE-FILE SYMBOL-PACKAGE ATOM PRINT-UNREADABLE-OBJECT
			       COMPILE-FILE-PATHNAME RESTART-BIND FTRUNCATE ECHO-STREAM-INPUT-STREAM
			       GET-DECODED-TIME MAKE-PATHNAME CHAR> GRAPHIC-CHAR-P UNINTERN
			       HASH-TABLE-REHASH-THRESHOLD OR UPPER-CASE-P HANDLER-BIND RATIONALIZE
			       WITH-ACCESSORS FILE-LENGTH FUNCTION-KEYWORDS INTERACTIVE-STREAM-P
			       FIND-METHOD FILE-POSITION REMOVE-IF CHAR-NOT-GREATERP SHADOWING-IMPORT
			       CLASS-NAME CELL-ERROR-NAME DELETE REALP ROTATEF NO-APPLICABLE-METHOD
			       READ-DELIMITED-LIST PATHNAME-MATCH-P POP LDB-TEST ARRAY-ELEMENT-TYPE
			       REQUIRE EQUAL STRING-LESSP ELT DECODE-FLOAT /= MUFFLE-WARNING
			       MAKE-SYMBOL TWO-WAY-STREAM-OUTPUT-STREAM TANH HANDLER-CASE NOTANY OPEN
			       COPY-ALIST STRING-CAPITALIZE REMOVE CLOSE MULTIPLE-VALUE-LIST
			       COPY-STRUCTURE PPRINT-TAB READTABLEP INVALID-METHOD-ERROR CODE-CHAR
			       COMPILED-FUNCTION-P ODDP MAKE-CONCATENATED-STREAM DEFPACKAGE
			       ARRAY-IN-BOUNDS-P STRING= LOGANDC2 VECTOR-PUSH COSH PATHNAME-DIRECTORY
			       DISASSEMBLE FIND-PACKAGE FILE-NAMESTRING FRESH-LINE
			       ENSURE-GENERIC-FUNCTION CHAR-LESSP CHAR-EQUAL LOOP-FINISH LOGXOR REPLACE
			       INITIALIZE-INSTANCE REMOVE-DUPLICATES STRING-LEFT-TRIM SUBSETP POSITION
			       MAPLIST ARRAY-RANK FLOAT-SIGN DIRECTORY LOGICAL-PATHNAME-TRANSLATIONS
			       CHAR-NAME BIT-NAND SUBLIS ECASE ASSOC ZEROP ADD-METHOD ROW-MAJOR-AREF
			       SUBTYPEP COMPUTE-RESTARTS ATAN SYMBOL-PLIST COMPLEMENT SYMBOLP APROPOS
			       STABLE-SORT CLASS-OF CALL-METHOD STREAM-ERROR-STREAM /
			       UPDATE-INSTANCE-FOR-REDEFINED-CLASS IMAGPART BIT-VECTOR-P COPY-SEQ GETF
			       LOGNOT NAMESTRING LOGBITP SYMBOL-VALUE TYPE-ERROR-DATUM RATIONALP
			       DEFMACRO DEFTYPE DECLAIM CHAR-NOT-EQUAL SHARED-INITIALIZE
			       ARRAY-TOTAL-SIZE CDADR PATHNAME < SEARCH TYPE-ERROR-EXPECTED-TYPE
			       FIND-RESTART BROADCAST-STREAM-STREAMS READ-PRESERVING-WHITESPACE
			       FORCE-OUTPUT WITH-INPUT-FROM-STRING NAME-CHAR ARRAY-ROW-MAJOR-INDEX
			       CHECK-TYPE LOGORC2 RPLACA IF LOGANDC1 TAILP DO-EXTERNAL-SYMBOLS
			       FMAKUNBOUND DEFVAR COND NRECONC ARITHMETIC-ERROR-OPERANDS PLUSP
			       LIST-ALL-PACKAGES SIGNAL MAP ASSOC-IF MULTIPLE-VALUE-CALL
			       PACKAGE-ERROR-PACKAGE CAAR MAX COPY-TREE EQL MAKE-SYNONYM-STREAM
			       RENAME-PACKAGE HASH-TABLE-SIZE DO* PROG2 OPEN-STREAM-P
			       MAKE-STRING-OUTPUT-STREAM PPRINT-TABULAR REINITIALIZE-INSTANCE
			       TRANSLATE-PATHNAME STRING-GREATERP LOGAND PRINC RANDOM CDDDDR EXPORT
			       PPRINT-LINEAR FLOOR CADADR FDEFINITION TRUENAME PROG* CDDDR
			       STRING-NOT-GREATERP PATHNAME-TYPE COUNT-IF-NOT METHOD-COMBINATION-ERROR
			       CAAAAR MAPCAN COS UNBOUND-SLOT-INSTANCE SIMPLE-VECTOR-P DO-SYMBOLS
			       ADJUST-ARRAY SOFTWARE-VERSION DEFSTRUCT FOURTH EVAL-WHEN ACOSH
			       STRING-EQUAL READ-FROM-STRING THROW DEFINE-MODIFY-MACRO SLEEP MAPHASH
			       COERCE GET-DISPATCH-MACRO-CHARACTER CONCATENATE CIS ARRAY-DIMENSIONS
			       READ-SEQUENCE HOST-NAMESTRING SET-MACRO-CHARACTER BIT-IOR FLOAT-DIGITS
			       PACKAGE-NICKNAMES ARRAY-DISPLACEMENT CAR LDIFF CERROR CDAAAR
			       MAKE-ECHO-STREAM LDB SLOT-MAKUNBOUND ISQRT CAADR FUNCTIONP
			       WITH-PACKAGE-ITERATOR PATHNAME-HOST STRING> PUSH FIND TYPEP RESTART-CASE
			       DEFINE-COMPILER-MACRO CONSTANTLY FLET PAIRLIS PSETQ CHAR-NOT-LESSP
			       NSUBSTITUTE-IF-NOT CHAR= SOME COUNT-IF RASSOC-IF STANDARD-CHAR-P UNTRACE
			       ABS FORMAT INTEGER-DECODE-FLOAT WARN UNWIND-PROTECT
			       MAKE-STRING-INPUT-STREAM SCALE-FLOAT READ-LINE SIMPLE-STRING-P SXHASH
			       WITH-SLOTS FORMATTER COPY-READTABLE PACKAGEP FIND-SYMBOL READ-BYTE
			       SUBST-IF-NOT TREE-EQUAL INTERN MACROLET LOAD-TIME-VALUE RASSOC-IF-NOT
			       CONSTANTP SYMBOL-MACROLET LOGTEST MAPC WRITE-CHAR EQUALP
			       PPRINT-LOGICAL-BLOCK NSTRING-DOWNCASE PROGV ENDP STRING-NOT-LESSP
			       CHAR-CODE NINTERSECTION CDR SLOT-BOUNDP INVOKE-RESTART METHOD-QUALIFIERS
			       SIXTH GO MAKE-CONDITION VECTOR-POP VECTOR-PUSH-EXTEND IMPORT ACOS
			       ENOUGH-NAMESTRING SIMPLE-CONDITION-FORMAT-ARGUMENTS WITH-OPEN-STREAM
			       LOCALLY TWO-WAY-STREAM-INPUT-STREAM ENCODE-UNIVERSAL-TIME SUBSTITUTE-IF
			       VALUES LCM WRITE-TO-STRING MAKE-LOAD-FORM-SAVING-SLOTS TRACE REMHASH
			       CONCATENATED-STREAM-STREAMS STEP LENGTH LISP-IMPLEMENTATION-VERSION
			       FIND-ALL-SYMBOLS MOD SET EVERY STREAM-ELEMENT-TYPE STRING-TRIM IDENTITY
			       DOCUMENTATION RANDOM-STATE-P RPLACD REVAPPEND CHARACTERP MIN FIND-IF
			       EIGHTH TIME SUBST COMPILER-MACRO-FUNCTION SET-DISPATCH-MACRO-CHARACTER
			       PACKAGE-NAME BOUNDP ACONS FILE-ERROR-PATHNAME GET-PROPERTIES
			       SIMPLE-CONDITION-FORMAT-CONTROL MAKE-LOAD-FORM LOOP DEFGENERIC Y-OR-N-P
			       WITH-COMPILATION-UNIT LONG-SITE-NAME DELETE-IF
			       COMPUTE-APPLICABLE-METHODS GET-OUTPUT-STREAM-STRING 1+ ALLOCATE-INSTANCE
			       PROG NUMBERP FINISH-OUTPUT LOGNOR TYPECASE MACHINE-INSTANCE COMPILE-FILE
			       SUBSTITUTE YES-OR-NO-P DECODE-UNIVERSAL-TIME GET-INTERNAL-RUN-TIME
			       HASH-TABLE-COUNT BYTE-SIZE LOAD COUNT DELETE-PACKAGE GCD NINTH
			       PATHNAME-VERSION CDDAR SIGNUM MEMBER USE-PACKAGE WRITE-BYTE CATCH
			       USER-HOMEDIR-PATHNAME BIT MAKE-INSTANCES-OBSOLETE GET-INTERNAL-REAL-TIME
			       CADDDR ARRAYP BOOLE DECF EVENP MERGE-PATHNAMES LISTEN POSITION-IF-NOT
			       DEFINE-METHOD-COMBINATION UNION MINUSP SETF WRITE-SEQUENCE PPRINT
			       REMPROP PROGN INTEGER-LENGTH UNUSE-PACKAGE TERPRI DELETE-IF-NOT
			       MAKE-INSTANCE NREVERSE SLOT-EXISTS-P DESCRIBE PROVIDE BIT-NOT COMPLEX ED
			       SPECIAL-OPERATOR-P COMPLEXP ASSERT DO-ALL-SYMBOLS INSPECT BIT-XOR
			       ECHO-STREAM-OUTPUT-STREAM SINH BIT-NOR UNLESS PHASE LOGICAL-PATHNAME
			       LOAD-LOGICAL-PATHNAME-TRANSLATIONS NSTRING-UPCASE RESTART-NAME
			       SET-SYNTAX-FROM-CHAR ARITHMETIC-ERROR-OPERATION MEMBER-IF NOTEVERY
			       NSUBST-IF-NOT FIND-IF-NOT PATHNAMEP PARSE-NAMESTRING BIT-ORC2
			       MEMBER-IF-NOT MAKE-ARRAY WITH-HASH-TABLE-ITERATOR
			       TRANSLATE-LOGICAL-PATHNAME READ-CHAR-NO-HANG SORT DEFSETF APPLY
			       LIST-LENGTH MAKE-LIST APPEND CHAR-DOWNCASE CDAADR <=
			       HASH-TABLE-REHASH-SIZE NTH-VALUE CHAR<= DEPOSIT-FIELD OUTPUT-STREAM-P
			       MULTIPLE-VALUE-BIND FILL ETYPECASE BLOCK WRITE LOGNAND CHAR/=
			       FLOAT-RADIX QUOTE ALPHANUMERICP FILE-STRING-LENGTH STRINGP TENTH DEFUN
			       INTEGERP READ-CHAR DEFCLASS DEFINE-SYMBOL-MACRO CHAR MAKE-RANDOM-STATE
			       NSUBSTITUTE-IF PUSHNEW ATANH - SEVENTH BYTE SLOT-MISSING LOGIOR
			       DEFINE-SETF-EXPANDER BOTH-CASE-P CHARACTER MASK-FIELD STREAMP REMF
			       ADJUSTABLE-ARRAY-P CEILING CDADAR NO-NEXT-METHOD LAMBDA PROCLAIM ROUND
			       CCASE UPGRADED-COMPLEX-PART-TYPE INCF FCEILING DEFPARAMETER REST BREAK >
			       DOLIST MERGE APROPOS-LIST SUBST SUBSTITUTE PPRINT-LINEAR FILE-NAMESTRING
			       WRITE-CHAR DO* SLOT-EXISTS-P FILE-AUTHOR MACRO-FUNCTION RASSOC
			       MAKE-ECHO-STREAM ARITHMETIC-ERROR-OPERATION POSITION-IF-NOT LIST CDADR
			       LISP-IMPLEMENTATION-TYPE VECTOR-PUSH LET LENGTH STRING-UPCASE ADJOIN
			       DIGIT-CHAR STEP MEMBER-IF HANDLER-BIND LOGNOT APPLY GCD SLOT-UNBOUND
			       STRINGP VALUES-LIST STABLE-SORT DECODE-FLOAT MAKE-LIST RPLACA ISQRT
			       EXPORT SYNONYM-STREAM-SYMBOL FUNCTION-KEYWORDS REPLACE TANH MAPHASH
			       CODE-CHAR DECF ARRAY-DISPLACEMENT STRING-NOT-LESSP SLOT-VALUE REMOVE-IF
			       CELL-ERROR-NAME VECTORP CDDDAR TWO-WAY-STREAM-OUTPUT-STREAM
			       PARSE-INTEGER GET-INTERNAL-REAL-TIME FOURTH MAKE-STRING SLOT-MISSING
			       BYTE-SIZE STRING-TRIM NSTRING-DOWNCASE CDADDR < LABELS
			       INTERACTIVE-STREAM-P FIFTH MAX LOGXOR PATHNAME-NAME FUNCTION REALP EQL
			       LOGAND SHORT-SITE-NAME PROG1 USER-HOMEDIR-PATHNAME LIST-ALL-PACKAGES EXP
			       CADAR READ-CHAR-NO-HANG PACKAGE-ERROR-PACKAGE STREAM-EXTERNAL-FORMAT
			       BIT-ANDC2 NSUBSTITUTE-IF MAPCAR COMPLEMENT
			       LOAD-LOGICAL-PATHNAME-TRANSLATIONS PPRINT-NEWLINE ODDP CAAAR
			       DESTRUCTURING-BIND COPY-ALIST ACOS GO BIT-NOR DEFCONSTANT FCEILING TENTH
			       NREVERSE = NUNION SLOT-BOUNDP STRING> COUNT-IF ATOM CHAR= RANDOM-STATE-P
			       ROW-MAJOR-AREF BIT-ANDC1 TRANSLATE-PATHNAME SIMPLE-VECTOR-P COERCE
			       SUBSTITUTE-IF-NOT ZEROP INVALID-METHOD-ERROR COMPILE REALPART
			       REMOVE-IF-NOT PPRINT-TAB HASH-TABLE-REHASH-THRESHOLD INVOKE-RESTART IF
			       COUNT /= DO INITIALIZE-INSTANCE ABS SCHAR
			       SIMPLE-CONDITION-FORMAT-CONTROL DELETE-PACKAGE SUBST-IF LAMBDA
			       HASH-TABLE-COUNT ARRAY-HAS-FILL-POINTER-P BIT WITH-STANDARD-IO-SYNTAX
			       PARSE-NAMESTRING PROCLAIM ARRAY-IN-BOUNDS-P MULTIPLE-VALUE-CALL RPLACD
			       SOME GRAPHIC-CHAR-P READ-FROM-STRING CONSP CADAAR ACONS EVERY
			       MAKE-PATHNAME MASK-FIELD CASE SET-MACRO-CHARACTER BIT-AND RESTART-BIND
			       ECHO-STREAM-INPUT-STREAM COMPILE-FILE FILL-POINTER NUMBERP ACOSH
			       ARRAY-DIMENSIONS DOCUMENTATION MINUSP INSPECT COPY-STRUCTURE
			       INTEGER-LENGTH ENSURE-GENERIC-FUNCTION CHAR>= QUOTE LOGNOR
			       MAKE-TWO-WAY-STREAM IGNORE-ERRORS TAILP WITH-SLOTS FBOUNDP
			       LOGICAL-PATHNAME-TRANSLATIONS EQUAL FLOAT-SIGN SHADOW SLEEP NUMERATOR
			       PROG2 GETF LDB-TEST ROUND LOCALLY ECHO-STREAM-OUTPUT-STREAM LOG
			       GET-MACRO-CHARACTER ALPHANUMERICP FIND-METHOD NINTERSECTION DEFCLASS
			       DEFINE-CONDITION PRINT-UNREADABLE-OBJECT DEFVAR BROADCAST-STREAM-STREAMS
			       FLOATP SUBST-IF-NOT INTEGERP TRANSLATE-LOGICAL-PATHNAME SUBSETP WHEN
			       WRITE-STRING WITH-OPEN-FILE CLRHASH APROPOS INTERN MIN STRING-GREATERP
			       IMPORT NSET-DIFFERENCE PROG INCF BOTH-CASE-P MULTIPLE-VALUE-PROG1
			       CHARACTERP STREAMP DIGIT-CHAR-P RANDOM STRING-LESSP
			       MAKE-STRING-INPUT-STREAM COPY-SYMBOL READ-SEQUENCE LOGCOUNT BIT-NOT
			       BOUNDP ENCODE-UNIVERSAL-TIME THIRD DECLAIM MAP CONS SET-SYNTAX-FROM-CHAR
			       AND CIS SYMBOL-PLIST LOOP-FINISH STANDARD-CHAR-P MULTIPLE-VALUE-BIND
			       ASIN STRING POP COMPLEX FDEFINITION PSETF TYPE-ERROR-DATUM
			       OUTPUT-STREAM-P FLOOR WRITE-LINE <= DEFMACRO RATIONAL HASH-TABLE-TEST
			       WITH-OPEN-STREAM READ-CHAR STRING-CAPITALIZE GET-PROPERTIES Y-OR-N-P
			       USE-PACKAGE REMOVE COMPILER-MACRO-FUNCTION READ PACKAGE-NICKNAMES
			       REMOVE-DUPLICATES MAKE-LOAD-FORM-SAVING-SLOTS DRIBBLE
			       DEFINE-MODIFY-MACRO MAKE-DISPATCH-MACRO-CHARACTER CLOSE COSH OPEN
			       FINISH-OUTPUT STRING-DOWNCASE CAR NSTRING-CAPITALIZE SOFTWARE-TYPE
			       READ-PRESERVING-WHITESPACE CADR FROUND NSUBLIS DEFSETF FIND-ALL-SYMBOLS
			       CHAR> NO-APPLICABLE-METHOD COMPUTE-RESTARTS PATHNAME BIT-ORC2
			       WRITE-SEQUENCE PPRINT-TABULAR SYMBOL-VALUE CHAR-NAME GET-DECODED-TIME
			       FORMATTER BIT-VECTOR-P INTERSECTION PATHNAME-TYPE CLEAR-INPUT
			       CALL-METHOD PRINC-TO-STRING SYMBOLP MAKE-LOAD-FORM NSUBST
			       PPRINT-DISPATCH HANDLER-CASE METHOD-COMBINATION-ERROR PROBE-FILE ATAN
			       STRING< TYPE-ERROR-EXPECTED-TYPE PUSHNEW UNREAD-CHAR PRINT OR
			       WITH-HASH-TABLE-ITERATOR MAKE-SEQUENCE ECASE UNWIND-PROTECT REQUIRE
			       SIXTH GET-DISPATCH-MACRO-CHARACTER CHAR-NOT-LESSP READ-BYTE TAGBODY
			       FILE-ERROR-PATHNAME CATCH RATIONALP CHAR-DOWNCASE CHAR-INT ARRAY-RANK
			       COND LAST MAKE-STRING-OUTPUT-STREAM ARRAY-DIMENSION HOST-NAMESTRING
			       INPUT-STREAM-P DECODE-UNIVERSAL-TIME DEFUN EVAL-WHEN CHAR-CODE
			       PATHNAME-DIRECTORY EVENP SUBSEQ PPRINT FTRUNCATE MAKE-INSTANCE
			       PATHNAME-HOST LOGBITP REMF 1+ COPY-PPRINT-DISPATCH CHAR-UPCASE ERROR
			       READ-LINE SECOND MAKE-PACKAGE DIRECTORY SPECIAL-OPERATOR-P OPEN-STREAM-P
			       RASSOC-IF-NOT CCASE EQUALP SUBSTITUTE-IF * CHAR/= CDR SQRT LCM
			       LOGICAL-PATHNAME EVAL DEFINE-COMPILER-MACRO NSUBSTITUTE-IF-NOT MAPCON
			       IMAGPART SET-EXCLUSIVE-OR SIMPLE-CONDITION-FORMAT-ARGUMENTS EXPT
			       CONCATENATE FILE-POSITION MACROLET KEYWORDP HASH-TABLE-REHASH-SIZE +
			       EIGHTH USE-VALUE CHAR-EQUAL BIT-XOR FORMAT BYTE DOTIMES NAMESTRING
			       CHAR-NOT-EQUAL MULTIPLE-VALUE-LIST ASSERT APPEND NOTANY TYPEP
			       DELETE-FILE MAKUNBOUND CDAAR FILE-WRITE-DATE > CDDDR WRITE-TO-STRING
			       FUNCALL MEMBER-IF-NOT DEFTYPE READTABLE-CASE WITH-ACCESSORS TRUENAME
			       CONSTANTP RASSOC-IF CAAADR TREE-EQUAL NSET-EXCLUSIVE-OR NSUBSTITUTE
			       MAKE-INSTANCES-OBSOLETE PACKAGE-USE-LIST INVOKE-DEBUGGER PROVIDE
			       COUNT-IF-NOT TRACE LOGANDC1 NTHCDR CHAR<= FUNCTIONP WITH-SIMPLE-RESTART
			       SET-DISPATCH-MACRO-CHARACTER LOGORC2 UNEXPORT REST UNBOUND-SLOT-INSTANCE
			       MAKE-HASH-TABLE HASH-TABLE-P REINITIALIZE-INSTANCE NTH DO-SYMBOLS
			       NRECONC MACROEXPAND STORE-VALUE FLOAT-PRECISION REMPROP NTH-VALUE
			       DEFINE-SYMBOL-MACRO UPDATE-INSTANCE-FOR-REDEFINED-CLASS IDENTITY PROGV
			       PROGN RETURN-FROM READTABLEP REM SYMBOL-NAME PSETQ WILD-PATHNAME-P CHAR
			       LIST* CHAR< PLUSP PAIRLIS CDDAR PPRINT-INDENT UNION COMPILED-FUNCTION-P
			       ROTATEF ABORT MACHINE-TYPE CONCATENATED-STREAM-STREAMS STRING-RIGHT-TRIM
			       ENOUGH-NAMESTRING ARITHMETIC-ERROR-OPERANDS CEILING DOLIST DELETE
			       MAKE-CONDITION STRING-LEFT-TRIM INTEGER-DECODE-FLOAT CHECK-TYPE NOTEVERY
			       FUNCTION-LAMBDA-EXPRESSION - MULTIPLE-VALUE-SETQ NAME-CHAR PUSH
			       PPRINT-POP COMPILE-FILE-PATHNAME LIST-LENGTH NSTRING-UPCASE EQ FIND-IF
			       METHOD-QUALIFIERS CAADR CDDR STRING= LET* REMOVE-METHOD PATHNAME-MATCH-P
			       FIND-PACKAGE TRUNCATE CAADDR GET-SETF-EXPANSION LOOP
			       DEFINE-SETF-EXPANDER CADDR PACKAGE-SHADOWING-SYMBOLS FORCE-OUTPUT
			       SLOT-MAKUNBOUND STRING-NOT-GREATERP CDADAR CDAADR LOGANDC2 MAKE-ARRAY
			       MERGE-PATHNAMES SIN 1- MACHINE-VERSION FFLOOR PACKAGEP
			       SET-PPRINT-DISPATCH FLET GENSYM PPRINT-EXIT-IF-LIST-EXHAUSTED COS GET
			       MAPL DELETE-IF WITH-CONDITION-RESTARTS ATANH COPY-LIST FILL
			       CHAR-NOT-GREATERP BIT-ORC1 MOD PACKAGE-USED-BY-LIST WARN ADD-METHOD
			       SIMPLE-STRING-P FIND-RESTART DESCRIBE PATHNAME-VERSION PEEK-CHAR
			       YES-OR-NO-P COMPLEXP AREF NOT POSITION-IF STRING>= DEFSTRUCT FLOAT-RADIX
			       NINTH CAADAR SUBTYPEP SET BUTLAST ALLOCATE-INSTANCE
			       WITH-INPUT-FROM-STRING ASSOC WRITE MAKE-RANDOM-STATE BIT-EQV
			       FLOAT-DIGITS LONG-SITE-NAME WITH-COMPILATION-UNIT DELETE-DUPLICATES
			       MAKE-SYMBOL ROOM CDAR PPRINT-FILL DEFGENERIC MACROEXPAND-1 SCALE-FLOAT
			       CDAAAR UPDATE-INSTANCE-FOR-DIFFERENT-CLASS ARRAY-ROW-MAJOR-INDEX ED
			       FILE-STRING-LENGTH ENSURE-DIRECTORIES-EXIST COPY-READTABLE STRING<=
			       SEVENTH WITH-OUTPUT-TO-STRING SIGNUM ELT UNTRACE NULL DEFPARAMETER BLOCK
			       PRIN1 REVAPPEND GENTEMP CTYPECASE ASH SXHASH LISTP DO-EXTERNAL-SYMBOLS
			       BIT-IOR ETYPECASE SORT CHANGE-CLASS FIND-CLASS ALPHA-CHAR-P MAP-INTO
			       TERPRI DO-ALL-SYMBOLS LDB LOGORC1 SEARCH FMAKUNBOUND LOAD CHARACTER
			       STRING-NOT-EQUAL PATHNAMEP MAKE-BROADCAST-STREAM ARRAYP MAPCAN CERROR
			       INVOKE-RESTART-INTERACTIVELY ASSOC-IF-NOT WITH-PACKAGE-ITERATOR
			       GET-INTERNAL-RUN-TIME READ-DELIMITED-LIST UNLESS LOWER-CASE-P
			       RESTART-NAME / BOOLE DEFMETHOD FLOAT SOFTWARE-VERSION VECTOR-POP
			       VECTOR-PUSH-EXTEND CAAR LDIFF MEMBER FIND-SYMBOL REDUCE SVREF
			       DESCRIBE-OBJECT LOGIOR STRING-EQUAL TYPE-OF POSITION CDDADR
			       PATHNAME-DEVICE GET-OUTPUT-STREAM-STRING SYMBOL-PACKAGE TAN
			       COMPUTE-APPLICABLE-METHODS CDDDDR NSUBST-IF-NOT SUBLIS SET-DIFFERENCE
			       TWO-WAY-STREAM-INPUT-STREAM ADJUSTABLE-ARRAY-P MACHINE-INSTANCE SIGNAL
			       CONJUGATE CAAAAR ENDP LISP-IMPLEMENTATION-VERSION CDDAAR PACKAGE-NAME
			       ADJUST-ARRAY BIT-NAND GETHASH IN-PACKAGE SYMBOL-FUNCTION
			       MAKE-CONCATENATED-STREAM DEFPACKAGE CLASS-OF NO-NEXT-METHOD LOGEQV
			       DEPOSIT-FIELD DISASSEMBLE UNUSE-PACKAGE COPY-TREE FIND ASINH CLASS-NAME
			       RENAME-FILE VALUES PRINT-NOT-READABLE-OBJECT MISMATCH CADADR
			       SHADOWING-IMPORT DELETE-IF-NOT MAPLIST LISTEN RETURN STREAM-ELEMENT-TYPE
			       UNINTERN MERGE MAKE-SYNONYM-STREAM PRIN1-TO-STRING NSUBST-IF
			       BYTE-POSITION PHASE MUFFLE-WARNING REMHASH CONTINUE LOAD-TIME-VALUE
			       HASH-TABLE-SIZE UPGRADED-COMPLEX-PART-TYPE CHAR-LESSP SBIT
			       UPGRADED-ARRAY-ELEMENT-TYPE FILE-LENGTH TYPECASE CADDDR FIRST
			       RATIONALIZE LOGTEST FIND-IF-NOT DPB MAPC SINH CHAR-GREATERP SHIFTF
			       DENOMINATOR GET-UNIVERSAL-TIME NCONC SETF LOGNAND RENAME-PACKAGE
			       PPRINT-LOGICAL-BLOCK BREAK SYMBOL-MACROLET THE FRESH-LINE CLEAR-OUTPUT
			       ASSOC-IF STRING/= PRINC DIRECTORY-NAMESTRING STREAM-ERROR-STREAM
			       ARRAY-ELEMENT-TYPE SETQ COPY-SEQ TIME RESTART-CASE PROG*
			       SHARED-INITIALIZE ARRAY-TOTAL-SIZE SIMPLE-BIT-VECTOR-P
			       DEFINE-METHOD-COMBINATION WRITE-BYTE CONSTANTLY CADDAR PRINT-OBJECT
			       VECTOR THROW REVERSE >= UPPER-CASE-P NBUTLAST))
