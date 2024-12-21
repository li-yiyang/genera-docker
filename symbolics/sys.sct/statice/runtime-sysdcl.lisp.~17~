;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Lowercase: Yes -*-
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

(defsubsystem Statice-Packages
    (:default-pathname "sys:statice;"
     :maintaining-sites :scrc
     :distribute-sources t
     )
  (:serial "statice-packages"))

(defsubsystem uid
    (:pretty-name "Unique-ID"
     :short-name "UID"
     :default-pathname "sys:statice;model;"
     :maintaining-sites :scrc
     :distribute-sources nil
     )
  (:serial "unique-id"))

;;; File level system kernel declaration
(defsubsystem dbfs-kernel
    (:pretty-name "DBFS Kernel"
     :maintaining-sites :scrc
     :default-pathname "sys:statice;file;"
     :distribute-sources nil
     )
  (:serial "lock-order"
	   (:definitions "defs" 
			 (:serial "locks" "queues" "common" "remote" "client"))))

;;; File level system declaration
(defsubsystem dbfs-client
    (:pretty-name "DBFS Client"
     :maintaining-sites :scrc
     :default-pathname "sys:statice;file;"
     :distribute-sources nil
     )
  (:serial "initialization" "misc"))

;;B* Index system declarations    
(defsubsystem Statice-Index
    (:default-pathname "sys:statice;storage;"
     :maintaining-sites :scrc
     :distribute-sources nil
     )
  (:definitions "storage-defs"			;Storage definitions, constants, and macros
		(:serial "new-heap"		;Greenwald's heaps.
			 "errors"		;Error flavors
			 "tree"			;Index objects
			 "scroll"		;Pagination algorithm
			 "interface")))

;;; System declaration for the "record" portion of the storage level of Statice.
(defsubsystem statice-record
    (:default-pathname "sys:statice;storage;"
     :maintaining-sites :scrc
     :distribute-sources nil
     )
  (:serial "record-defs" "b-set" "record"))

;;; System declaration for the model level of Statice.
(defsubsystem statice-model
    (:default-pathname "sys:statice;model;"
     :maintaining-sites :scrc
     :distribute-sources nil
     )
  (:serial "defs" "datatypes" 
	   (:parallel "utils" "schema" "entrypoints" "other")
	   "query"				;Query depends on entrypoints being loaded.
	   "function-level" "cfm" "versioned-entity" "update-schema"))

;; Directory system declarations
(defsubsystem dbfs-dir
    (:pretty-name "DBFS-DIR"
     :default-pathname "sys:statice;file;"
     :maintaining-sites :scrc
     :distribute-sources nil
     )
  (:serial "dir-defs" "dbfs-pathnames" "directory"))

(defsubsystem statice-runtime-utilities
    (:default-pathname "sys:statice;utilities;"
     :maintaining-sites :scrc
     :distribute-sources nil
     )
  (:serial "runtime-utilities" "model-dumper"
	   "presentation-types"
	   ))

(defsystem Statice-Runtime
    (:default-pathname "sys:statice;"
     :default-module-type :system
     :bug-reports (:mailing-list "Bug-Statice")
     :advertised-in (:herald)
     :distribute-sources nil
     :maintaining-sites :scrc
     :patches-reviewed "System-Changes")
  (:serial "Statice-Packages"
	   "UID"
	   "DBFS-Kernel"
	   "DBFS-Client"
	   "Statice-Record"
	   "Statice-Index"
	   "Statice-Model"
	   "DBFS-DIR"
	   "Statice-Runtime-Utilities"))
