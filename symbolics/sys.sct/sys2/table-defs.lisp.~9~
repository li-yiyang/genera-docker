;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: COMMON-LISP-INTERNALS; Lowercase: Yes -*-
;;;>
;;;> *****************************************************************************************
;;;> ** (c) Copyright 1998-1982 Symbolics, Inc.  All rights reserved.
;;;> ** Portions of font library Copyright (c) 1984 Bitstream, Inc.  All Rights Reserved.
;;;>
;;;>    The software, data, and information contained herein are proprietary to,
;;;> and comprise valuable trade secrets of, Symbolics, Inc., which intends 
;;;> to keep such software, data, and information confidential and to preserve them
;;;> as trade secrets.  They are given in confidence by Symbolics pursuant 
;;;> to a written license agreement, and may be used, copied, transmitted, and stored
;;;> only in accordance with the terms of such license.
;;;> 
;;;> Symbolics, Symbolics 3600, Symbolics 3675, Symbolics 3630, Symbolics 3640,
;;;> Symbolics 3645, Symbolics 3650, Symbolics 3653, Symbolics 3620, Symbolics 3610,
;;;> Zetalisp, Open Genera, Virtual Lisp Machine, VLM, Wheels, Dynamic Windows,
;;;> SmartStore, Semanticue, Frame-Up, Firewall, Document Examiner,
;;;> Delivery Document Examiner, "Your Next Step in Computing", Ivory, MacIvory,
;;;> MacIvory model 1, MacIvory model 2, MacIvory model 3, XL400, XL1200, XL1201,
;;;> Symbolics UX400S, Symbolics UX1200S, NXP1000, Symbolics C, Symbolics Pascal,
;;;> Symbolics Prolog, Symbolics Fortran, CLOE, CLOE Application Generator,
;;;> CLOE Developer, CLOE Runtime, Common Lisp Developer, Symbolics Concordia,
;;;> Joshua, Statice, and Minima are trademarks of Symbolics, Inc.
;;;> 
;;;> Symbolics 3670, Symbolics Common Lisp, Symbolics-Lisp, and Genera are registered
;;;> trademarks of Symbolics, Inc.
;;;>
;;;> GOVERNMENT PURPOSE RIGHTS LEGEND
;;;> 
;;;>      Contract No.: various
;;;>      Contractor Name: Symbolics, Inc.
;;;>      Contractor Address: c/o Ropes & Gray
;;;> 			 One International Place
;;;> 			 Boston, Massachusetts 02110-2624
;;;>      Expiration Date: 2/27/2018
;;;>      
;;;> The Government's rights to use, modify, reproduce, release, perform, display or
;;;> disclose this software are restricted by paragraph (b)(2) of the "Rights in
;;;> Noncommercial Computer Software and Noncommercial Computer Software Documentation"
;;;> contained in the above identified contracts.  No restrictions apply after the
;;;> expiration date shown above.  Any reproduction of the software or portions thereof
;;;> marked with this legend must also reproduce the markings.  Questions regarding
;;;> the Government's rights may be referred to the AS&T Contracts Office of the
;;;> National Reconnaissance Office, Chantilly, Virginia 20151-1715.
;;;> 
;;;>      Symbolics, Inc.
;;;>      c/o Ropes & Gray
;;;>      One International Place
;;;>      Boston, Massachusetts 02110-2624
;;;>      781-937-7655
;;;>
;;;> *****************************************************************************************
;;;>

;;; this stuff cribbed from the previous table implementation;
;;; sys;sys2;table-defs...

;;; New hash functions

(defsubst gc-dependence (x)
  (if (sys:%pointer-type-p (sys:%data-type x))
      (if (sys:%ephemeralp x)
	  %gc-dependence-ephemeral
	  (let ((region (sys:%region-number x)))
	    (cond (region
		   (select (level-type (ldb sys:%%region-level (sys:region-bits region)))
		     (%level-type-dynamic %gc-dependence-dynamic)
		     (%level-type-static  %gc-dependence-static)
		     (otherwise		  %gc-dependence-none)))
		  (t %gc-dependence-none))))		;Probably cannot move
      %gc-dependence-none))

;;; Embody some standard calculations.

(defsubst fixnum-eql-hash (x)
  ;; offset it so that 0 doesn't hash to 0
  (si:%32-bit-plus (si:fixnum-multiply-by-prime x) 31))
  
(defsubst bignum-eql-hash (x)
  (logxor (ldb (byte 1 31) x) (ldb (byte 31 0) x)))
	
(defsubst character-hash (x)
  (si:sxhash-character x))

(defsubst string-hash (x)
  (si:sxhash-string x))

;;; Real hash functions.

;; Hash function for EQ.
(defsubst xeqhash (x)
  (let ((gc-dependence (gc-dependence x)))	;Must compute before calling %pointer
    (values (sys:%pointer x) gc-dependence)))

(defsubst xeqhash-no-gc (x)
  (sys:%pointer x))

;;; Function suitable for test of EQL on numbers.  No GC dependence.
(defsubst number-eql-hash (x)
  (if (integerp x)
      (if (fixnump x)
	  (fixnum-eql-hash x)
	  (bignum-eql-hash x))
      (number-eql-hash-1 x)))

;; Hash function for EQL
(defsubst xeqlhash (x)
  (typecase x
    (number
      (values (number-eql-hash x) %gc-dependence-none))
    (character
      (values (character-hash x) %gc-dependence-none))
    (otherwise
      (xeqhash x))))

(defsubst xeqlhash-no-gc (x)
  (typecase x
    (number
      (number-eql-hash x))
    (character
      (character-hash x))
    (otherwise
      (xeqhash-no-gc x))))

(defmacro map-list-accumulating-hash ((list elt-var &key (atom-hash 'xequal-hash) ignore-gc)
				      &body body)
  (let ((list-var (gensym))
	(rot (gensym))
	(hash (gensym))
	(a-hash (gensym))
	(gc-flag (gensym))
	(gc-dependence-var (gensym)))
    (flet ((ehash (form)
	     (if ignore-gc
		 form
		 `(multiple-value-bind (,a-hash ,gc-flag)
		      ,form
		    (when (> ,gc-flag ,gc-dependence-var)
		      (setq ,gc-dependence-var ,gc-flag))
		    ,a-hash))))	     
      `(do ((,rot 4)
	    (,hash 0)
	    (,list-var ,list)
	    ,@(unless ignore-gc
		`((,gc-dependence-var %gc-dependence-none))))
	   ((atom ,list-var)
	    (unless (null ,list-var)
	      (setq ,hash (logxor (rot ,(ehash `(,atom-hash ,list-var))
				       (- ,rot 4))
				  ,hash)))
	    ,(if ignore-gc
		 hash
		 `(values ,hash ,gc-dependence-var)))
	 (let ((,elt-var (pop ,list-var)))
	   (setq ,rot (ldb (byte 5 0) (+ ,rot 7)))	;rot = mod(rot+7,32)
	   (setq ,hash (logxor ,hash
			       (rot ,(ehash `(progn ,@body))
				    ,rot)))
	   )))))

;;; This is just like SI:EQUAL-HASH, but symbols hash on the pointer rather than the
;;; pname string.
(defun xequal-hash (x)
  (declare (values hash dependence-on-address))
  (declare (inline bit-vector-p))
  (typecase x
    (symbol
      (xeqhash x))
    (string
      (values (string-hash x) %gc-dependence-none))
    (number
      (values (number-eql-hash x) %gc-dependence-none))
    (list					;Rotate car by 11. and cdr by 7, but do it efficiently
      (do ((rot 4) (hash 0) (val %gc-dependence-none))
	  ((atom x)
	   (unless (null x)
	     (setq hash (logxor (rot (multiple-value-bind (hash flag)
					 (xequal-hash x)
				       (when (and flag (> flag val))
					 (setq val flag))
				       hash)
				     (- rot 4))
				hash)))
	   (values hash val))
	(let ((y (pop x)))
	  (setq rot (ldb (byte 5 0) (+ rot 7)))	;rot = mod(rot+7,32)
	  (setq hash (logxor (rot (typecase y
				    (symbol
				      (multiple-value-bind (hash flag)
					  (xeqhash y)
					(when (and flag (> flag val))
					  (setq val flag))
					hash))
				    (string
				      (sxhash-string y))
				    (fixnum
				      (%32-bit-plus y 13))
				    (otherwise
				      (multiple-value-bind (hash flag)
					  (xequal-hash y)
					(when (and flag (> flag val))
					  (setq val flag))
					hash)))
				  rot)
			     hash)))))
    (character
      (values (character-hash x) %gc-dependence-none))
    (bit-vector
      (values (bit-vector-hash x) %gc-dependence-none))
    (otherwise
      (xeqhash x))))

(defun xequal-hash-no-gc (x)
  (declare (values hash))
  (declare (inline bit-vector-p))
  (typecase x
    (symbol
      (xeqhash-no-gc x))
    (string
      (string-hash x))
    (number
      (number-eql-hash x))
    (list					;Rotate car by 11. and cdr by 7, but do it efficiently
      (do ((rot 4) (hash 0))
	  ((atom x)
	   (unless (null x)
	     (setq hash (logxor (rot (xequal-hash-no-gc x)
				     (- rot 4))
				hash)))
	   hash)
	(let ((y (pop x)))
	  (setq rot (ldb (byte 5 0) (+ rot 7)))	;rot = mod(rot+7,32)
	  (setq hash (logxor (rot (typecase y
				    (symbol
				      (xeqhash-no-gc y))
				    (string
				      (sxhash-string y))
				    (fixnum
				      (%32-bit-plus y 13))
				    (otherwise
				      (xequal-hash-no-gc y)))
				  rot)
			     hash)))))
    (character
      (character-hash x))
    (bit-vector
      (bit-vector-hash x))
    (otherwise
      (xeqhash-no-gc x))))

;; The out-of-line part of number hashing.  This is for a test of EQL.
(defun number-eql-hash-1 (x)
  (declare (values hash dependence-on-address))
  (etypecase x
    (single-float
      ;; offset it so that 0.0 doesn't hash to 0
      (%32-bit-plus (%fixnum x) 89))
    (double-float
      ;; offset it so that 0.0d0 doesn't hash to 0
      (%32-bit-plus (logxor (double-high x)
			    (double-low x))
		    107))
    #+3600
    (ratio
      (logxor (number-eql-hash (rational-numerator x))
	      (number-eql-hash (rational-denominator x))))
    #+imach
    (big-ratio
      (logxor (number-eql-hash (big-ratio-numerator x))
	      (number-eql-hash (big-ratio-denominator x))))
    #+imach
    (small-ratio
      (logxor (fixnum-eql-hash (small-ratio-numerator x))
	      (fixnum-eql-hash (small-ratio-denominator x))))
    (complex
      ;; do something so it's not commutative and #c(X X) doesn't hash to 0
      (logxor (number-eql-hash (complex-realpart x))
	      (rot (number-eql-hash (complex-imagpart x)) 7)))
    (fixnum
      (fixnum-eql-hash x))
    (integer
      (bignum-eql-hash x))))

;;; Function suitable for test of = on numbers.  No GC dependence.
(defun =-hash (x)
  (if (complexp x)
      (let ((real (complex-realpart x))
	    (imag (complex-imagpart x)))
	(if (floatp imag)
	    (logxor (number-eql-hash (rationalize real))
		    (if (= imag 0.0) 0 (rot (number-eql-hash (rationalize imag)) 7)))
	    (logxor (number-eql-hash real)
		    (rot (number-eql-hash imag) 7))))
      (number-eql-hash (if (floatp x) (rationalize x) x))))

(defun equalp-hash (x)
  (declare (values hash dependence-on-address))
  (typecase x
    (symbol
      (xeqhash x))
    (string
      (values (string-hash x) %gc-dependence-none))
    (number
      (values (=-hash x) %gc-dependence-none))
    (list					;Rotate car by 11. and cdr by 7, but do it efficiently
      (do ((rot 4) (hash 0) (val %gc-dependence-none))
	  ((atom x)
	   (unless (null x)
	     (setq hash (logxor (rot (multiple-value-bind (hash flag)
					 (equalp-hash x)
				       (when (and flag (> flag val))
					 (setq val flag))
				       hash)
				     (- rot 4))
				hash)))
	   (values hash val))
	(let ((y (pop x)))
	  (setq rot (ldb (byte 5 0) (+ rot 7)))	;rot = mod(rot+7,32)
	  (setq hash (logxor (rot (multiple-value-bind (hash flag)
				      (equalp-hash y)
				    (when (and flag (> flag val))
				      (setq val flag))
				    hash)
				  rot)
			     hash)))))
    (character
      (values (character-hash x) %gc-dependence-none))
    (array
      ;; hash on dimensions, (fill-pointer for 1d), first 4 elements, where exist.
      (case (array-rank x)
	(0
	  (equalp-hash (aref x)))
	(1
	  (let* ((x x)
		 (length (length x))
		 (hash length)
		 (gc %gc-dependence-none))
	    (declare (sys:array-register x))
	    (dotimes (i (min 4 length))
	      (multiple-value-bind (h g)
		  (equalp-hash (aref x i))
		(setq hash (rot (logxor h hash) 7))
		(setf gc (max g gc))))
	    (values hash gc)))
	(otherwise
	  (let ((x x)
		(hash (xequal-hash (array-dimensions x)))
		(gc cli::%gc-dependence-none))
	    (declare (sys:array-register-1d x))
	    (dotimes (i (min 4 (array-total-size x)))
	      (multiple-value-bind (h g)
		  (equalp-hash (sys:%1d-aref x i))
		(setq hash (rot (logxor h hash) 7))
		(setf gc (max g gc))))
	    (values hash gc)))))
    (hash-table
      (values (hash-table-count x) %gc-dependence-none))
    ((satisfies structure-object-p)
      (let* ((info (clos-internals::%instance-instance-information x))
	     (size (clos-internals::%instance-information-size info))
	     (hash size)
	     (gc %gc-dependence-none))
	(dotimes (i (1- size))
	  (let ((loc (locf (clos-internals::%structure-ref x i))))
	    (multiple-value-bind (h g)
		(if (location-boundp loc)
		    (equalp-hash (location-contents loc))
		    (values 0 %gc-dependence-none))
	      (setq hash (rot (logxor h hash) 7))
	      (setf gc (max g gc)))))
	(values hash gc)))
    (otherwise
      (xeqhash x))))

(defun structure-object-p (x)
  (declare (notinline typep))
  (typep x 'clos:structure-object))

;;; Other stuff needed by new tables

(defvar hash-table-weak-link-area (make-area :name 'hash-table-weak-link-area
					     '%%region-space-type %region-space-weak))


;;; stub.  remove this (and the call in flavor::bootstrap-flavors) when
;;; install this for real.
(defun bootstrap-tables ()
  ())

;;; ****************************************************************

;;; defgenerics for all the usual table ops.  this is cribbed from the
;;; macroexpansion of the define-protocol form in hte old table system's
;;; defs.

(defgeneric gethash (key table &optional default)
  (:documentation
    "Returns the value of the entry associated with KEY in TABLE or DEFAULT,
a predicate value to indicate whether such an entry was found, and the KEY of this entry
if found."
    )
  (declare (values value found found-key))
  (:dispatch table)
  (declare (optimize speed))
  (declare (side-effects reader)))

(defgeneric puthash (table key value)
  (:documentation "Associates KEY with VALUE in TABLE.  Returns VALUE")
  (declare (values value))
  ;; (:inline-methods :recursive)
  )

(defgeneric remhash (key table)
  (:documentation
    "Removes the entry associated with KEY in TABLE.  Returns T if such an entry was
found, and hence removed, NIL otherwise."
    )
  (declare (values removed))
  ;; (:inline-methods :recursive)
  (:dispatch table)
  (declare (optimize speed)))

(defgeneric modify-hash (table key function)
  (:documentation
    "Calls function with KEY, VALUE and FOUND-P.  The value the call
returns is used as the new value, as if a PUTHASH were done."
    )
  (declare (values new-value key))
  ;; (:inline-methods :recursive)
  (declare (downward-funarg function) (values new-value key)))

(defgeneric hash-table-count (table)
  (:documentation "Returns the number of entries in TABLE.")
  (declare (values filled-elements))
  ;; (:inline-methods :recursive)
  (declare (side-effects reader)))

(defgeneric maphash (function table)
  (:documentation
    "For each entry in TABLE, calls FUNCTION on the key and value of the entry.")
  (declare (values nil))
  (:dispatch table)
  (declare (optimize speed))
  (declare (downward-funarg function)))

(defgeneric maptable (table function result-type)
  (:documentation
    "Calls FUNCTION on the key and value of every entry in TABLE and ARGS.
If RESULT-TYPE is specified, collects and returns a sequence of type RESULT-TYPE
which contains the results calling FUNCTION on all the keys and values of TABLE."
    )
  (declare (values result-sequence))
  (declare (downward-funarg function)))

(defgeneric next-entry (table old-entry)
  (:documentation
    "Returns 3 values: an updated representation-dependent pointer (or nil if no more
elements), the key and the value."
    )
  (declare (values new-entry-or-nil key value))
  (declare (side-effects reader)))

(defgeneric clrhash (table)
  (:documentation "Clears the contents of TABLE")
  (declare (values table))
  (:inline-methods :recursive))

(defgeneric page-in-table (table &key type hang-p)
  (:documentation "Pages in the table, including the internal-representation.")
  (declare (values nil)))

(defgeneric page-out-table (table &key write-modified reuse)
  (:documentation "Pages out the table, including the internal-representation.")
  (declare (values nil)))

(defgeneric test-function (table)
  (:documentation "Returns the table's test function.")
  (declare (values test-function))
  ;; (:inline-methods :recursive)
  )

(defgeneric fast-table-get (table key)
  (:documentation
    "Like GETHASH, but doesn't take a DEFAULT, and only returns the value of the entry.")
  (declare (values value))
  ;; (:inline-methods :recursive)
  (declare (side-effects reader)))

(defgeneric with-table-locked-internal (table function)
  (:documentation "Call the function with the table locked.")
  (declare (values nil))
  (:inline-methods :recursive)
  (declare (downward-funarg function)))

(defgeneric table-number-of-values (table)
  (:documentation "Return number of values stored in each table entry.")
  (declare (values number-of-values))
  (declare (function-parent table define-protocol))
  (:inline-methods :recursive))



(defsetf gethash (key table &optional default) (value)
  default					;Ignored by SETF - may be used by INCF
  `(puthash ,table ,key ,value))

(compiler:defoptimizer (gethash gethash-to-fast-table-get fast-table-get) (form)
  ;; change GETHASH to FAST-TABLE-GET if the default isn't specified or is null,
  ;; and only the first return value is being used
  (block optimizer
    (compiler:matchp form
      (('gethash key table . rest)
       (when (or (null rest)			; default not specified
		 (compiler:matchp rest
		   ((default)
		    (and (compiler:constant-form-p default)
			 ;; null default
			 (null (compiler:constant-evaluator default))))))
	 (when (eql 1 (compiler:destination-n-values compiler:*optimizer-destination*))
	   (return-from optimizer
	     (let ((key-var (make-symbol "KEY"))
		   (table-var (make-symbol "TABLE")))
	       (lt:let-subst `(,key-var ,table-var) `(,key ,table)
			     `(fast-table-get ,table-var ,key-var)
			     compiler:*optimizer-environment*)))))))
    form))

(compiler:defoptimizer (future-common-lisp:gethash ansi-gethash-to-fast-table-get fast-table-get) (form)
  ;; change GETHASH to FAST-TABLE-GET if the default isn't specified or is null,
  ;; and only the first return value is being used
  (block optimizer
    (compiler:matchp form
      (('future-common-lisp:gethash key table . rest)
       (when (or (null rest)			; default not specified
		 (compiler:matchp rest
		   ((default)
		    (and (compiler:constant-form-p default)
			 ;; null default
			 (null (compiler:constant-evaluator default))))))
	 (when (eql 1 (compiler:destination-n-values compiler:*optimizer-destination*))
	   (return-from optimizer
	     (let ((key-var (make-symbol "KEY"))
		   (table-var (make-symbol "TABLE")))
	       (lt:let-subst `(,key-var ,table-var) `(,key ,table)
			     `(fast-table-get ,table-var ,key-var)
			     compiler:*optimizer-environment*)))))))
    form))

;;;
;;; This def cribbed from the old table system.  I can't quite figure
;;; out why it has to make a forward reference to some random DW
;;; function (is there really no other way to clean up arglists???) but
;;; I'm not going to try to change that now.  As the comment internally
;;; says, this forward reference is only used at compile time.
;;;
(macrolet ((define-with-stack-table ()
	    (let ((args '(&key 
			   (name nil)
			   (test ''eql)
			   (size '*default-table-size* size-supplied-p)
			   (hash-function nil hash-function-supplied-p)
			   rehash-before-cold
			   rehash-after-full-gc
			   (number-of-values 1)
			   (store-hash-code nil store-hash-code-supplied-p)
			   (gc-protect-values t)
			   (mutating t)
			   (initial-contents nil initial-contents-supplied-p)
			   (optimizations nil)
			   (locking :process)
			   (ignore-gc nil ignore-gc-supplied-p)
			   (growth-factor '*default-table-growth-factor*)
			   (growth-threshold '*default-table-growth-threshold*)
			   (rehash-size nil rehash-size-supplied-p)
			   (rehash-threshold nil rehash-threshold-supplied-p))))
	      `(defmacro with-stack-table ((var &rest options ,@args) &body body)
		 ;; Yes, this is a forward reference in the inner system, but this function is
		 ;; only called at compile time for this file.
		 ,@(dw::find-lambda-vars args)
		 `(with-data-stack 
		    (let ((,var (make-hash-table :area :stack ,@options)))
		      ,@body))))))
  (define-with-stack-table))

(defmacro with-table-locked ((table) &body body)
  `(with-table-locked-internal ,table #'(lambda () ,@body)))


;;; Generate the sequence of primes used as table sizes.  This code is
;;; used only at compile-time.  cf moon

;;; If this is too slow, make it faster, damn it
;;; But it seems to take under 1/10 second for any reasonable arguments
;;; See choose-new-size, what a crock, doesn't even return a prime!
(DEFUN NEXT-PRIME-PAIR (MIN)
  (DECLARE (VALUES P P+2))
  (LOOP FOR P+2 FROM (LOGIOR MIN 1) BY 2 WITH P = NIL DO
    (WHEN (LOOP FOR FACTOR FROM 3 BY 2 TO (ISQRT P+2)
		NEVER (ZEROP (MOD P+2 FACTOR)))
      (WHEN (EQL P (- P+2 2))
	(RETURN (VALUES P P+2)))
      (SETQ P P+2))))

(defvar *max-defined-prime-for-table* 100000000)	; 100M; ever need bigger table?

;;;
;;; primes for use as sizes of tables.  growing by 2.5 percent yields a
;;; list of 458 pairs, roughly evenly distributed (logarithmically)
;;; between 3 and 100 million.  we collect the *lower* of the two, ie P,
;;; not P+2.
;;;
(defun prime-numbers-for-table-system ()
  (loop for i from 3 with p 
	do (setq p (next-prime-pair i))
	   (setq i (floor (* 1.025 (1+ p))))
	collect p
	while (< i *max-defined-prime-for-table*)))
