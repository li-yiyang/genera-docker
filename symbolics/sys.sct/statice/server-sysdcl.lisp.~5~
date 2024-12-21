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

;; File level system declarations
(defsubsystem ufs
    (:pretty-name "UFS"
     :maintaining-sites :scrc
     :default-pathname "sys:statice;file;ufs;"
     :distribute-sources nil)
  (:serial (:definitions "defs"
			 (:serial "main" "entrypoints" "cfm"))))

;;; File level system declaration
(defsubsystem dbfs
    (:pretty-name "DBFS"
     :maintaining-sites :scrc
     :default-pathname "sys:statice;file;"
     :distribute-sources nil)
  (:serial "recovery" "local"D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :BOLD NIL) "CPTFONTCB") 0"server" "backup-interface"))

(defsubsystem statice-server-utilities
    (:default-pathname "sys:statice;utilities;"
     :maintaining-sites :scrc
     :distribute-sources nil)
  (:serial "setup-statice"))

(defsubsystem DBFS-Utilities-Packages
    (:pretty-name "DBFS Utilities Packages"
     :default-pathname "sys:statice;file;darc;"
     :maintaining-sites :scrc
     :distribute-sources t)
  (:module packages "utilities-packages" (:type :lisp-read-only))
  (:serial packages))

(defsubsystem DBFS-Utilities
    (:pretty-name "DBFS Utilities"
     :default-pathname "sys:statice;file;darc;"
     :maintaining-sites :scrc
     :distribute-sources nil)
  (:module packages "DBFS-Utilities-Packages" (:type :system))
  (:serial packages
	   (:definitions "tertiary-storage-defs"
			 (:serial "tertiary-storage-errors"
				  "mount-request"
				  "buffer-stream"
				  "tertiary-storage"
				  "tertiary-storage-debug"))
	   "volume-librarian"
	   (:definitions "maint-defs"
			 (:serial "maintenance"
			  "commands"
			  "fuzzy-backup"
			  "backup-utilities"))))

(defsystem Statice-Server
    (:default-pathname "sys:statice;"
     :default-module-type :system
     :bug-reports (:mailing-list "Bug-Statice")
     :advertised-in (:herald)
     :distribute-sources nil
     :maintaining-sites :scrc
     :patches-reviewed "System-Changes"
     :required-systems ("Statice-Runtime"))
  (:serial "UFS"
	   "DBFS"
	   "Statice-Server-Utilities"
	   "DBFS-Utilities"))
