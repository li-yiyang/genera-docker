;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
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




(future-common-lisp:defpackage uid
  (:use scl)
  (:export "UNIQUE-ID" "UNIQUE-ID-WORDS" "UNIQUE-ID-INSTANCE")
  )


;;; UFS package
(future-common-lisp:defpackage UFS
  (:use scl)
  (:size 1055)
  (:export
    "CREATE-FILE" "CREATE-FILE-GIVEN-UID" "DESTROY-FILE" "OPEN-FILE" "CLOSE-FILE" "LIST-FILES"
    "CREATE-BLOCK" "CREATE-BLOCKS" "INSERT-BLOCKS" "DESTROY-BLOCK" "DESTROY-BLOCKS"
    "READ-BLOCKS" "WRITE-BLOCKS"
    "FILE-SIZE" "FILE-EXTANT-P" "BLOCK-EXTANT-P" "BLOCK-SIZE" "FILE-SYSTEM-SIZE"
    "INITIALIZE" "VERIFY" "SHUTDOWN" "ENABLE" "DEENCACHE" "ADD-PARTITION" "SHOW-PARTITION-USAGE"
    "DEFAULT-NEW-PARTITION-NAME" "USING-WIRED-DISK-EVENT"

    ;; errors
    "UFS-ERROR" "FILE-SYSTEM-FULL" "DIRECTORY-FULL" "FILE-ERROR" "FILE-ALREADY-EXISTS"
    "FILE-DOES-NOT-EXIST" "FILE-IS-OPEN" "FILE-NOT-OPEN" "BLOCK-EXISTS" "BLOCK-DOES-NOT-EXIST"
    "FILE-SYSTEM-INCONSISTENT" "FILE-SYSTEM-DISABLED" 

    ;; the flavor
    "UFS-FILE-SYSTEM-MIXIN"))


;;; File level package

(future-common-lisp:defpackage DBFS
  (:use scl)
  (:size 1055)
  (:export 
    ;; Transaction interface
    "*CURRENT-TRANSACTION*"			;++ maybe should not be exported?
    "WITH-TRANSACTION"				; Special form to scope a transaction
    "BEGIN-TRANSACTION"				; Start a transaction
    "END-TRANSACTION"				; End and commit a transaction
    "MARK-TRANSACTION"				; Commit a transaction without ending
    "ABORT-TRANSACTION"				; Abort a transaction without committing.
    "VALIDATE-TRANSACTION"			; Ensure the transaction is valid
    "CURRENT-TRANSACTION-ID"			; Returns an ID for the current tran
    
    ;; File interface
    "FILE-ID"					; Flavor
    "LOOKUP-FILE-ID"				; Constructor for file ids
    "BASIC-PORTAL"				; Base flavor
    "PORTAL"					; Instantiable Flavor
    "MAKE-PORTAL"				; Constructor for portals
    "FILE-UNIQUE-ID"				; To a portal
    "CREATE-FILE"				; "
    "DESTROY-FILE"				; "
    "FILE-EXTANT-P"				; "
    "CREATE-PAGE"				; "
    "DESTROY-PAGE"				; "
    "PAGE-EXTANT-P"				; "
    "READ-PAGE"					; "
    "WRITE-PAGE"				; "
    "RETURN-PAGE"				; "
    "FILE-PAGE-SIZE"				; "
    "FILE-PAGE-LENGTH"				; "
    "INVALID-FILE-ID"				; "
    "VALIDATE-PORTAL"				; "
    
    ;; Top level calls
    "SHUTDOWN-DBFS"
    "ENABLE-DBFS"
    "SHUTDOWN-DBFS-FILE-SYSTEM"
    "ENABLE-DBFS-FILE-SYSTEM"
    "SHOW-DBFS-STATUS"
    "SHOW-DBFS-METERS"
    "CREATE-LOCAL-DBFS"
    "SET-LOCAL-DBFS-LOG-SIZE"
    "SET-LOCAL-DBFS-CHECKPOINT-PERCENTAGE"
    "SET-BUFFER-REPLACEMENT-PARAMETERS"    
    
    ;; Options
    "*FULL-VERIFICATION*"
    "*OPTIMISTIC-LOCKING*"
    "*SELECTIVE-PESSIMISM*"
    "*FORCE-OUT-OPTIMISTIC-COMMANDS*"
    "*AUTOMATIC-SELECTIVE-PESSIMISM*"
    "*TRANSACTION-RETRY-LIMIT*"
    "*CHECKPOINT-PRIORITY*"
    "*CHECK-WRITES*"
    "*CHECK-WRITES-RETRY-LIMIT*"
    "*ENABLE-TRACE*"
    "*DBFS-TRACE*"

    ;; Error interface
    "DBFS-ERROR"
    "FILE-SYSTEM-ERROR"
    "NONEXTANT-FILE-SYSTEM"
    "FILE-SYSTEM-ALREADY-EXISTS"
    "INVALID-FILE-SYSTEM"
    "DISABLED"
    "FILE-SYSTEM-DISABLED"
    "DBFS-DISABLED"
    "NO-TRANSACTION"
    "TRANSACTION-RETRY-LIMIT-EXCEEDED"
    "FILE-ERROR"
    "NONEXTANT-FILE"
    "FILE-ALREADY-EXISTS"
    "INVALID-FILE-OPERATION"
    "FILE-ID-INVALID"
    "INVALID-PAGE-OPERATION"
    "NONEXTANT-PAGE"
    "PAGE-ALREADY-EXISTS"
    "TRANSACTION-ABORT"
    "SIMPLE-TRANSACTION-ABORT"
    "SYSTEM-SHUTDOWN-TRANSACTION-ABORT"
    "WARM-BOOT-TRANSACTION-ABORT"
    "NETWORK-HARD-ERROR-TRANSACTION-ABORT"
    "RESTARTABLE-TRANSACTION-ABORT"
    "SIMPLE-RESTARTABLE-TRANSACTION-ABORT"
    "TWO-TRANSACTION-DEADLOCK-RESTARTABLE-TRANSACTION-ABORT"
    "N-TRANSACTION-DEADLOCK-RESTARTABLE-TRANSACTION-ABORT"
    "CACHE-FLUSH-RESTARTABLE-TRANSACTION-ABORT"
    "CACHE-SKEW-RESTARTABLE-TRANSACTION-ABORT"
    "NETWORK-ERROR-RESTARTABLE-TRANSACTION-ABORT"
    "SERVER-RESTART-RESTARTABLE-TRANSACTION-ABORT"
    "PAGE-INVALID-RESTARTABLE-TRANSACTION-ABORT"))

;;; Storage level package

(future-common-lisp:defpackage Statice-Storage
  (:use SCL)
  (:size 1055)
  (:export "READ-RECORD-WORD" "WRITE-RECORD-WORD"
	   "READ-MULTIPLE-RECORD-WORD" "WRITE-MULTIPLE-RECORD-WORD"
	   "READ-MULTIPLE-RECORD-WORD-GIVEN-RID"
	   "MAKE-RECORD" "DELETE-RECORD" "WITH-RECORD" "UPGRADE-RECORD-MODE"
	   "GET-RECORD-SIZE" "SET-RECORD-SIZE" "ADJUST-RECORD-SIZE"
	   "GET-ROOT-RID" "SET-ROOT-RID" "VALIDATE-RID" "GET-ADDRESSOR-RID"
	   "OPEN-FILE" "INIT-FILE" "CLOSE-FILE"
	   "BEGIN-TRANSACTION" "COMMIT-TRANSACTION" "ABORT-TRANSACTION" "MARK-TRANSACTION"
	   "WITH-TRANSACTION-INTERNAL"
	   "MAP-OVER-AREA"
	   "MAKE-BSET-INDEX" "DELETE-BSET-INDEX"
	   "INSERT-BSET-KEY" "DELETE-BSET-KEY" "LOOKUP-BSET-VALUES" "COUNT-BSET-VALUES"
	   "MAP-OVER-BSET" "SCAN-BSET-RANGE" "MAP-BSET-RANGE" "LOOKUP-BSET-VALUE"
	   "MAP-OVER-PAGES-IN-CLUSTER" "CLUSTERED-P"))

;;; B-tree Index package

(future-common-lisp:defpackage Statice-Index
  (:use SCL DBFS)
  (:size 1055)
  ;;Needed for Greenwald's new heap implementation.
  (:shadow make-heap)
  (:import-from "ZL"
   "ARRAY-PUSH" "ARRAY-PUSH-EXTEND")
  (:import-from "STATICE-STORAGE"
   "ALLOCATE-WHOLE-PAGE"
   "DEALLOCATE-WHOLE-PAGE"
   "*B-TREE-HEADER-PAGE-CODE*"
   "*B-TREE-BRANCH-PAGE-CODE*"
   "*B-TREE-LEAF-PAGE-CODE*"
   "*B-TREE-OVERFLOW-PAGE-CODE*")
  (:export
    "MAKE-BTREE-INDEX"				;Create a B* tree index
    "DELETE-BTREE-INDEX"			;Remove a B* tree index
    "INSERT-INDEX-VALUE"			;Add a key/value association
    "DELETE-INDEX-VALUE"			;Remove a key/value association
    "LOOKUP-INDEX-VALUE"			;Finds the one and only value
    "LOOKUP-INDEX-VALUES"			;Finds any and all values
    "LOOKUP-LEAST-INDEX-VALUE"			;Finds the smallest value
    "LOOKUP-GREATEST-INDEX-VALUE"		;Finds the largest valued
    "SCAN-INDEX-RANGE"				;Finds entries in a range
    "COUNT-INDEX-RANGE"				;counts entries in a range
    "COMPUTE-INDEX-DEPTH"))			;finds the depth of an index


;;; Model level package

;;; In order to maintain the same source structure under the Flavors-to-CLOS
;;; conversion of user-visible Statice (only the user-visible parts!) the sources
;;; in the Statice-Model package have to contain a pretty awful mixture of
;;; SCL and Future-CL.  Statice-Model uses Future-CL, but it imports from SCL
;;; where there isn't a conflict.  - jga 14 Sept 92

;;; Also note that there are more exported symbols of Statice-Model than
;;; are mentioned here.  Other symbols want their home package to be
;;; Statice, which hasn't been defined yet.  Those remaining exports are
;;; done after Statice is defined. - jga 25 Nov 92

(future-common-lisp:defpackage Statice-Model
  (:use Future-Common-Lisp Statice-Storage Statice-Index)
  (:size 2151)
  (:import-from "SCL"
   "STRING-CHAR" "STRING-LENGTH" "STRING-SEARCH" "SUBSTRING" "STRING-APPEND" "SEND" "USING-RESOURCE"
   "WITH-STANDARD-IO-ENVIRONMENT" "RECORD-SOURCE-FILE-NAME" "ARGLIST" "LOAD-BYTE"
   "BOOLEAN" "DEFRESOURCE" "MAKE-RASTER-ARRAY" "WITH-STACK-LIST" "WITH-STACK-LIST*"
   "UNWIND-PROTECT-CASE" "CONDITION-BIND" "ERROR-RESTART" "MAXF" "MINF" "SELF" "LET-IF"
   "" "" "CHAR" "CHAR" "STRING" "STRING" "DEFSUBST")
  (:export "MAKE-ENTITY" #+ignore "delete-entity" "DOMAIN-OF"
	  "MAKE-TUPLE" "MAKE-CLUSTERED-TUPLE" "DELETE-TUPLE" "RELATION-OF"
	  "MAKE-ENTITY-AND-TUPLES" "MAKE-CLUSTERED-ENTITY-AND-TUPLES"
	  "GET-VALUE" "GET-VALUE-NULL-P" "NULLP" "SET-VALUE" "NULL-VALUE" "SET-TO-NULL-VALUE"
	  "GET-STRING-LENGTH" "GET-VALUE-INTO-STRING" "GET-VALUE-ARRAY-PORTION" "DO-TEXT-LINES"
	  "FORMAT-VALUE"
	  "GET-ARRAY-LENGTH" "SET-VALUE-ARRAY-PORTION"
	  "MAKE-DOMAIN" "GET-DOMAIN" "DELETE-DOMAIN"
	  "MAKE-RELATION" "GET-RELATION" "DELETE-RELATION"
	  "MAKE-ATTRIBUTE" "GET-ATTRIBUTE" "DELETE-ATTRIBUTE"
	  "WITH-DATABASE" "OPEN-DATABASE" #+ignore "terminate-database" #+ignore "with-current-database"
	  #+ignore "with-transaction" "ABORT-TRANSACTION" "MARK-TRANSACTION" "CHECK-TRANSACTION"
	  #+ignore "with-transaction-if"
	  "MAKE-INDEX" "DELETE-INDEX" "MAKE-GROUP-INDEX" "DELETE-GROUP-INDEX"
	  "MAKE-RELATION-INDEX" "DELETE-RELATION-INDEX" "MAKE-DOMAIN-INDEX" "DELETE-DOMAIN-INDEX"
	  "FIND-AVAILABLE-AREA"
	  "ENTITY-TYPE-OF" "HOME-DATABASE-OF" "VALUE-TYPE-P"
	  "DO-RELATION" "FIND-GREATEST" "FIND-LEAST" "MAP-RELATION-DYNAMIC" "COUNT-RELATION-DYNAMIC"
	  "DO-ATTRIBUTE"
	  "DUMP-DATABASE" "RESTORE-DATABASE"
	  #+ignore "string-prefix" #+ignore "string-prefix-exact" "DOMAIN"	;do-relation spec symbols.
	  "DATABASE"
	  #+ignore "*restart-testing*"
	  ;; Flavors, several for entity-type-of, or for interpreting schema vector.
	  "HANDLE" "DOMAIN-HANDLE" "RELATION-HANDLE" "ATTRIBUTE-HANDLE" "INDEX-HANDLE"
	  "TUPLE-HANDLE" #+ignore "entity-handle" "UNKNOWN-ENTITY-HANDLE" "KNOWN-ENTITY-HANDLE"
	  ;; Type names
	  #+ignore "lisp-object" "ANY-ENTITY" #+ignore "limited-string" #+ignore "image"D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :BOLD NIL) "CPTFONTCB") 0#+ignore "all-but-entity"
	  ;; Error flavors, and accessor functions.
	  "MODEL-ERROR"
	  "UNIQUENESS-VIOLATION" "UNIQUENESS-VIOLATION-RELATION-HANDLE"
	  "UNIQUENESS-VIOLATION-ATTRIBUTE-HANDLE" "UNIQUENESS-VIOLATION-ATTRIBUTE-HANDLES"
	  "FOREIGN-ENTITY-DELETED"
	  #+ignore "entity-handle-deleted"1 0#+ignore "entity-handle-deleted-entity-handle"
	  #+ignore "entity-handle-not-committed"1 0#+ignore "entity-handle-not-committed-entity-handle"
	  "DOMAIN-NOT-FOUND" "DOMAIN-NOT-FOUND-DATABASE" "DOMAIN-NOT-FOUND-NAME"
	  "RELATION-NOT-FOUND" "RELATION-NOT-FOUND-DATABASE" "RELATION-NOT-FOUND-NAME"
	  "ATTRIBUTE-NOT-FOUND" "ATTRIBUTE-NOT-FOUND-RELATION" "ATTRIBUTE-NOT-FOUND-NAME"
	  "INDEX-NOT-FOUND" "INDEX-NOT-FOUND-ATTRIBUTE-HANDLES"
	  "INDEX-NOT-FOUND-CASE-SENSITIVE" "INDEX-NOT-FOUND-RELATION"
	  "DOMAIN-ALREADY-EXISTS" "DOMAIN-ALREADY-EXISTS-DATABASE" "DOMAIN-ALREADY-EXISTS-NAME"
	  "RELATION-ALREADY-EXISTS" "RELATION-ALREADY-EXISTS-DATABASE" "RELATION-ALREADY-EXISTS-NAME"
	  "ATTRIBUTE-ALREADY-EXISTS" "ATTRIBUTE-ALREADY-EXISTS-RELATION"
	  "ATTRIBUTE-ALREADY-EXISTS-NAME"
	  "INDEX-ALREADY-EXISTS" "INDEX-ALREADY-EXISTS-ATTRIBUTE-HANDLES"
	  "INDEX-ALREADY-EXISTS-CASE-SENSITIVE" "INDEX-ALREADY-EXISTS-RELATION"
	  "WRONG-TYPE-VALUE" "WRONG-TYPE-VALUE-VALUE" "WRONG-TYPE-VALUE-TYPE"
	  #+ignore "not-inside-transaction"
	  #+ignore "database-deleted"1 0#+ignore "database-deleted-pathname"
	  #+ignore "database-terminated"1 0#+ignore "database-terminated-function"1 0#+ignore "database-terminated-time"
	  #+ignore "symbol-package-not-found"1 0#+ignore "symbol-package-not-found-package-name"
	  #+ignore "symbol-package-not-found-print-name"))


;;; Model level extended datatype package

(future-common-lisp:defpackage statice-type
  (:use)
  (:import-from "STATICE-MODEL"
    "READ-RECORD-WORD"
    "WRITE-RECORD-WORD"
    "READ-MULTIPLE-RECORD-WORD"
    "WRITE-MULTIPLE-RECORD-WORD"
    "*NULL-VALUE*")
  (:export
    "ENCODE-VALUE"
    "DECODE-VALUE"
    "FIXED-SPACE"
    "SIZE-OF-VALUE"
    "READ-VALUE"
    "VALUE-EQUAL"
    "RECORD-EQUAL"
    "VALUE-NULL-P"
    "WRITE-VALUE"
    "VALUE-COMPARE"
    "RECORD-COMPARE"
    "SIZE-OF-KEY-FROM-RECORD"
    "SIZE-OF-KEY-FROM-VALUE"
    "WRITE-KEY-FROM-RECORD"
    "WRITE-KEY-FROM-VALUE"
    "NEXT-VALUE"
    "DEFINE-VALUE-TYPE"
    "DEFINE-HANDLER-FLAVOR"
    "PRESENTATION-TYPE"
    "ORIGINAL-PRESENTATION-TYPE"
    "NO-NULLS"
    "READ-RECORD-WORD"
    "WRITE-RECORD-WORD"
    "READ-MULTIPLE-RECORD-WORD"
    "WRITE-MULTIPLE-RECORD-WORD"
    "*NULL-VALUE*"
    "DEFINE-HANDLER-CLASS"
    "DEFINE-HANDLER-FLAVOR")) ; remove DEFINE-HANDLER-FLAVOR after 8.3 - jga 21 Oct 92


;;; Function level package

(future-common-lisp:defpackage statice
  (:size 500)
  (:use future-common-lisp statice-model)
  (:shadow with-database open-database make-index delete-index make-entity do-text-lines
	   uniqueness-violation)
  (:import-from "SCL"
   "STRING-APPEND" "DEFPROP" "CONDITION-BIND" "CONDITION-CASE-IF" "COPY-ARRAY-PORTION" "LET-IF"
   "RECORD-SOURCE-FILE-NAME" "SELF" "STRING-LENGTH" "SUBSTRING" "SEND" "SEND-IF-HANDLES")
  (:export "SET-OF" "MAKE-DATABASE" "CURRENT-DATABASE"
	   "WITH-DATABASE" "OPEN-DATABASE" "WITH-CURRENT-DATABASE" "TERMINATE-DATABASE"
	   "WITH-TRANSACTION" "WITH-TRANSACTION-IF" "WITH-CLUSTER"
	   "FOR-EACH" "ASCENDING" "DESCENDING" "STRING-PREFIX" "STRING-PREFIX-EXACT"
	   "DO-TEXT-LINES"
	   "ADD-TO-SET" "DELETE-FROM-SET" "MAKE-ENTITY" "DELETE-ENTITY"
	   "SET-ATTRIBUTE-VALUE-TO-NULL"
	   "MAKE-INDEX" "DELETE-INDEX" "INDEX-EXISTS"
	   "MAKE-INVERSE-INDEX" "DELETE-INVERSE-INDEX" "INVERSE-INDEX-EXISTS"
	   "MAKE-MULTIPLE-INDEX" "DELETE-MULTIPLE-INDEX" "MULTIPLE-INDEX-EXISTS"

	   ;; dynamics
	   "ATTRIBUTE-VALUE" "SET-ATTRIBUTE-VALUE" "ATTRIBUTE-VALUE-NULL-P"
	   "ADD-TO-SET*" "DELETE-FROM-SET*" "FOR-EACH*" "COUNT-ENTITIES*"
	   "DO-TEXT-LINES*" "INVERSE-ATTRIBUTE-VALUE" "DEFAULT-STRING-CREATE-FUNCTION"
	   "ATTRIBUTE-VALUE-LENGTH" "ATTRIBUTE-VALUE-ARRAY-PORTION"
	   "SET-ATTRIBUTE-VALUE-ARRAY-PORTION"

	   ;; Type names, originally from statice-model and re-exported
	   "LISP-OBJECT" "ENTITY-HANDLE" "LIMITED-STRING" "IMAGE" "ALL-BUT-ENTITY"

	   ;; pclsr mode
	   "*RESTART-TESTING*"

	   ;; optimize constructor switch
	   #+ignore
	   "*DISABLE-CONSTRUCTOR-OPTIMIZATION*"

	   ;; Schema examination accessors.
	   "SCHEMA-NAME" "ENTITY-TYPE" "SCHEMA-TYPES"
	   "TYPE-NAME" "TYPE-PARENT-NAMES" "TYPE-ATTRIBUTES" "TYPE-AREA-NAME"
	   "TYPE-SET-EXISTS" "TYPE-MULTIPLE-INDEXES"
	   "ATTRIBUTE-NAME" "ATTRIBUTE-FUNCTION-NAME" "ATTRIBUTE-TYPE" "ATTRIBUTE-VALUE-TYPE"
	   "ATTRIBUTE-VALUE-IS-SET" "ATTRIBUTE-VALUE-FOREIGN" "ATTRIBUTE-UNIQUE" "ATTRIBUTE-READ-ONLY"
	   "ATTRIBUTE-AREA-NAME" "ATTRIBUTE-SET-EXISTS" "ATTRIBUTE-INDEX-EXISTS"
	   "ATTRIBUTE-INDEX-AVERAGE-SIZE" "ATTRIBUTE-INVERSE-INDEX-EXISTS"
	   "ATTRIBUTE-INVERSE-INDEX-EXACT-EXISTS" "ATTRIBUTE-INVERSE-INDEX-AVERAGE-SIZE"
	   "ATTRIBUTE-NO-NULLS" "ATTRIBUTE-INVERSE-FUNCTION-NAME"
	   "MULTIPLE-INDEX-ATTRIBUTE-NAMES" "MULTIPLE-INDEX-UNIQUE" "MULTIPLE-INDEX-CASE-SENSITIVE"
	   "GET-TEMPLATE-SCHEMA" "GET-TEMPLATE-ENTITY-TYPE"

	   ;; Error flavors and associated functions.
	   "FUNCTION-ERROR"
	   "NO-CURRENT-DATABASE"
	   "UNIQUENESS-VIOLATION"
	   "SCHEMA-NOT-LOADED"
	   "SYMBOL-PACKAGE-NOT-FOUND" "SYMBOL-PACKAGE-NOT-FOUND-PACKAGE-NAME"
	   "SYMBOL-PACKAGE-NOT-FOUND-PRINT-NAME"
	   "FUNCTION-UNIQUENESS-VIOLATION" "FUNCTION-UNIQUENESS-VIOLATION-TYPE"
	   "FUNCTION-UNIQUENESS-VIOLATION-FUNCTION"
	   "INDEX-UNIQUENESS-VIOLATION" "INDEX-UNIQUENESS-VIOLATION-TYPE"
	   "INDEX-UNIQUENESS-VIOLATION-FUNCTION"
	   "SCHEMA-NOT-LOADED-PATHNAME" "SCHEMA-NOT-LOADED-SCHEMA-DESCRIPTION-NAME"
	   "WRONG-TYPE-ENTITY"
	   "WRONG-TYPE-ENTITY-ENTITY-HANDLE" "WRONG-TYPE-ENTITY-EXPECTED-TYPE"
	   "ENTITY-NOT-FOUND-IN-SET"
	   "ENTITY-NOT-FOUND-IN-SET-ENTITY-HANDLE" "ENTITY-NOT-FOUND-IN-SET-SET-OF-ENTITIES"
	   "NO-FUNCTION-NAMED" "NO-FUNCTION-NAMED-FUNCTION-NAME"
	   "NO-ENTITY-TYPE-NAMED" "NO-ENTITY-TYPE-NAMED-ENTITY-TYPE-NAME"
	   "VALUE-NOT-A-SET"
	   "NOT-INSIDE-TRANSACTION"
	   "DATABASE-TERMINATED" "DATABASE-TERMINATED-FUNCTION" "DATABASE-TERMINATED-TIME"
	   "DATABASE-DELETED" "DATABASE-DELETED-PATHNAME"
	   "ENTITY-HANDLE-DELETED" "ENTITY-HANDLE-DELETED-ENTITY-HANDLE"
	   "ENTITY-HANDLE-NOT-COMMITTED" "ENTITY-HANDLE-NOT-COMMITTED-ENTITY-HANDLE"

	   ;; For cached objects
	   "CHANGE-VERSION-NUMBER" "ATTRIBUTE-CHANGED" "VALIDATE-CACHE" "LOAD-ENTITY"
	   "UPDATE-TRANSACTION-TAGS" "VERSIONED-ENTITY-MIXIN"

	   ;; From update-schema
	   "GET-REAL-SCHEMA" "GET-REAL-SCHEMA-NAME" "UPDATE-SCHEMA"

	   ;; for the browser
	   "VIEW-ENTITY" "BROWSER-PRESENTATION-TYPE"

           "DEFINE-SCHEMA" "DEFINE-ENTITY-TYPE"))

;;; This code doesn't work in the stricter ANSI-CL world.  It fails when redefining
;;; statice-model for a second time.  Setting the symbol-package of a symbol is
;;; not a good thing to do.  Instead the symbols are defined in statice first, then
;;; imported into and exported from statice-model later. - jga 25 Nov 92
;;;
;;;    ;;; Some symbols are originally interned in statice-model, exported from
;;;    ;;; there, used by the statice package, and exported from the statice
;;;    ;;; package.  These symbols retain the statice-model as their
;;;    ;;; home-package.  Unfortunately, this means their printed
;;;    ;;; representation uses statice-model, and so statice-model shows up in
;;;    ;;; the on-line documentation, even though we don't document that
;;;    ;;; package.  The following kludge changes those symbols to use statice
;;;    ;;; as the home package.
;;;    (defun fixup-home-packages-for-statice-model ()
;;;      (let ((statice (find-package "STATICE"))
;;;	    (statice-model (find-package "STATICE-MODEL")))
;;;	(do-external-symbols (s statice)
;;;	  (when (eq (symbol-package s) statice-model)
;;;	    (setf (symbol-package s) statice))))
;;;      t)
;;;    
;;;    (fixup-home-packages-for-statice-model)

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (let ((symbols-at-home-in-statice '(STATICE:ENTITY-HANDLE-NOT-COMMITTED-ENTITY-HANDLE STATICE:ALL-BUT-ENTITY
				      STATICE:ENTITY-HANDLE-NOT-COMMITTED STATICE:SYMBOL-PACKAGE-NOT-FOUND-PACKAGE-NAME
				      STATICE:SYMBOL-PACKAGE-NOT-FOUND-PRINT-NAME STATICE:DATABASE-TERMINATED
				      STATICE:DATABASE-DELETED STATICE:DATABASE-DELETED-PATHNAME
				      STATICE:DATABASE-TERMINATED-FUNCTION STATICE:ENTITY-HANDLE-DELETED
				      STATICE:ENTITY-HANDLE STATICE:ENTITY-HANDLE-DELETED-ENTITY-HANDLE
				      STATICE:WITH-TRANSACTION STATICE:LISP-OBJECT STATICE:NOT-INSIDE-TRANSACTION
				      STATICE:STRING-PREFIX-EXACT STATICE:STRING-PREFIX STATICE:DELETE-ENTITY
				      STATICE:LIMITED-STRING STATICE:DATABASE-TERMINATED-TIME
				      STATICE:WITH-CURRENT-DATABASE STATICE:SYMBOL-PACKAGE-NOT-FOUND
				      STATICE:WITH-TRANSACTION-IF STATICE:*RESTART-TESTING* STATICE:IMAGE
				      STATICE:TERMINATE-DATABASE)))
    (import symbols-at-home-in-statice "STATICE-MODEL")
    (export symbols-at-home-in-statice "STATICE-MODEL")))

;;; DBFS directory package 

(future-common-lisp:defpackage dbfs-dir
  (:use scl statice)
  (:export "PATHNAME-PORTAL-MIXIN"
	   "INITIALIZE-DIRECTORY-DATABASE"
	   "CREATE-DIRECTORIES"
	   "LOOKUP-DIRECTORY-PORTAL"
	   "NAME-OF-FILE-ID"
	   "NAME-OF-FILE-ID-WORDS"
	   "UPDATE-DBFS-DIRECTORY-FILE-ID"	;For fuzzy-backup

	   "DBFS-FILE-OPERATION-FAILURE"
	   "DBFS-FILE-NOT-FOUND"
	   "DBFS-DIRECTORY-NOT-EMPTY"))

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (import (intern "NAME-OF-FILE-ID" "DBFS-DIR") "DBFS"))

(future-common-lisp:defpackage statice-utilities
  (:use scl statice)
  (:export "ENTITY-NAMED-BY-STRING-ATTRIBUTE"
	   "CREATE-STATICE-FILE-SYSTEM"
	   "COPY-STATICE-DATABASE"))

;;; The statice-tests package is now defined in the statice-tests system

;;; User package just to make playing with statice easier

(future-common-lisp:defpackage statice-user
  (:use future-common-lisp dbfs-dir statice)
  (:nicknames s-u))
