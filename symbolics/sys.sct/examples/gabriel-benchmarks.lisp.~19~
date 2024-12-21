;;; -*- Mode: LISP; Package: (GABRIEL :USE LISP); Syntax:Common-Lisp; Lowercase: Yes -*-
;;;> EXAMPLES-MESSAGE
;;;>
;;;>******************************************************************************************
;;;>
;;;>       Symbolics hereby grants permission to customer to incorporate
;;;>       the examples in this file in any work belonging to customer.
;;;>
;;;>******************************************************************************************


;;; Richard Gabriel benchmark suite, Common Lisp version.

;;; This version was created at Symbolics, Inc.
;;; It is used to produce the official Symbolics benchmark values.
;;; Translated to Common Lisp by Charlie Hornig.
;;; Timing tools by Charlie Hornig and Dan Weinreb.
;;; This file is in the public domain.

;;;   The final arbiter of the usefulness of a Lisp implementation is the
;;;   ease that the user and programmer have with that implementation.
;;;   Performance is an issue, but it is not the only issue.
;;;      -- "Performance and Evaluation of Lisp Systems" by Richard P. Gabriel:

;;;   There is something fascinating about benchmarks.  One gets such
;;;   wholesale returns of conjecture out of such a trifling investment of
;;;   facts.
;;;      -- Mark Twain (paraphrased)

;;; Make a clean package for all of this stuff.
#-Minima (in-package "GABRIEL" :use '("LISP"))

;;; Minima is a draft ANSI CL Lisp.
#+Minima (defpackage "GABRIEL"
	   (:use "COMMON-LISP"))

#+Minima (in-package "GABRIEL")

;;; Timing tools

;;; Useful top level functions are:
;;;  (run-series) which types out results to standard-output.
;;;  (run-series-to-file pathname) which sends it to a file.

;;; Low-level timing functions

(defmacro without-other-processes (&body body)
  #+LispM
  `(progn
     ;; Since the scheduler will be disabled below, let other processes run now.
     (global:process-allow-schedule)
     ;; Make sure that we aren't in the middle of a GC flip.
     (si:gc-reclaim-oldspace)
     ;; Inhibit the scheduler in the most efficient manner consistent with the system version.
     (#+Genera ,(if (>= (sct:get-system-version) 416)
		    'process:without-preemption
		    'global:without-interrupts)
      #-Genera global:without-interrupts
      ,@body))
  #+Minima
  `(progn
     (minima-internals::gc-shutdown)
     (multiple-value-prog1
       (minima:with-no-other-processes
	 ,@body)
       (minima-internals::gc-on)
       (sleep 1)))
  #-(or LispM Minima)
  `(progn ,@body))

;;; Given a function of no arguments, calls the function once with scheduling inhibited,
;;; and returns the real time and run time consumed, in seconds.
(defun timed-duration (fn)
  ;; Turn off the scheduler so we can get repeatable numbers on Lisp Machines.
  ;; Other implementations may want to do something similar.
  (without-other-processes
    ;; The rest of this is pretty straight-forward.
    (let ((start-real (get-internal-real-time))
	  (start-run (get-internal-run-time)))
      (declare (type integer start-real start-run))
      (funcall fn)
      (let ((end-real (get-internal-real-time))
	    (end-run (get-internal-run-time)))
	(values (/ (- end-real start-real) internal-time-units-per-second)
		(/ (- end-run start-run) internal-time-units-per-second))))))

(defparameter *minimum-tests* 3 "Minimum number of times to run each program.")
(defparameter *minimum-duration* 6 "Minimum number of seconds to run each program.")

;;; Given a function of no arguments, run it enough times that it is run
;;; at least *minimum-tests* times, and enough times so that at least
;;; *minimum-duration* seconds are consumed.
;;; Return five values:
;;;  The total number of runs that were done.
;;;  The minimum time consumed per call in seconds
;;;  The minimum cpu time (subtracting page fault time)
;;;  The average time consumed per call in seconds
;;;  The average cpu time (subtracting page fault time)
(defun multiple-timed-duration (fn)
  ;; Run the function once, at least.
  (multiple-value-bind (total-real-time total-run-time)
      (timed-duration fn)
    (let ((min-real-time total-real-time)
	  (min-run-time total-run-time)
	  (min-real-time-2 total-real-time)
	  (min-run-time-2 total-run-time)
	  (repeats 1))
      ;; Run it some more until we've done it enough.
      (loop
	(when (and (>= repeats *minimum-tests*) (>= total-run-time *minimum-duration*))
	  (return))
	(multiple-value-bind (time cpu)
	    (timed-duration fn)
	  (incf total-real-time time)
	  (incf total-run-time cpu)
	  (when (< cpu min-run-time)
	    (setf min-real-time time)
	    (setf min-run-time cpu))
	  (when (< time min-real-time-2)
	    (setf min-real-time-2 time)
	    (setf min-run-time-2 cpu))
	  (incf repeats)))
      (values
	repeats
	min-real-time
	min-run-time
	(/ total-real-time repeats)
	(/ total-run-time repeats)
	min-real-time-2
	min-run-time-2
	))))

;;; DLW's stuff

(defvar *all-timers* nil)

(defmacro define-timer (name documentation &body body)
  (let ((timer (gensym)))
    `(progn (pushnew ',name *all-timers*)
	    (defun ,timer () ,@body)
	    (setf (get ',name 'timing-function) ',timer)
	    (setf (get ',name 'timing-documentation) ,documentation))))

;;; Series of benchmarks, excluding the I/O benchmarks which I'm not interested in.
(defparameter *series*
	      (append
		'(boyer browse destru traverse-init traverse
		  tak stak ctak takl takr
		  deriv dderiv div2-i div2-r
		  #-Minima fft
		  puzzle triang)
		#-Minima
		'(frpoly2r frpoly2r2 frpoly2r3 frpoly5r frpoly5r2 frpoly5r3
		  frpoly10r frpoly10r2 frpoly10r3 frpoly15r frpoly15r2 frpoly15r3)))

(defun describe-implementation (&optional (stream *standard-output*))
  (format stream "~&Lisp Type:~20T~A" (lisp-implementation-type))
  (format stream "~&Lisp Version:~20T~A" (lisp-implementation-version))
  (format stream "~&Software Type:~20T~A" (software-type))
  (format stream "~&Software Version:~20T~A" (software-version))
  (format stream "~&Machine Type:~20T~A" (machine-type))
  (format stream "~&Machine Version:~20T~A" (machine-version))
  #-Minima (format stream "~&Machine Instance:~20T~A" (machine-instance))
  (format stream "~&Site:~20T~A" (long-site-name))
  (format stream "~&Features:~20T~S" *features*))

;;; Minima doesn't provide floating point.
(defun ff (stream value colon-p atsign-p &optional width places &rest args)
  (declare (ignore colon-p atsign-p args))
  (unless width (setq width 1))
  (unless places (setq places 2))
  (let* ((i-part (floor value 1))
	 (d-part (floor (* (- value i-part) 
			   ;; (expt 10 places)
			   ;; expt doesn't exist in minima 
			   (loop with x = 1 repeat places 
				 do (setq x (* x 10)) finally (return x)))
			1))
	 (i-part-string (format nil "~D" i-part))
	 (d-part-string (format nil "~D" d-part)))
    ;; pad
    (loop repeat (- width (length i-part-string) places 1)
	  do (write-char #\space stream))
    (write-string i-part-string stream)
    (write-char #\. stream)
    (loop repeat (- places (length d-part-string))
	  do (write-char #\0 stream))
    (write-string d-part-string stream)))

;;; Given the symbol that names one of the tests, run that test and
;;; print the results to the stream.  This reports minimum times, rather
;;; than average times, because although average times are more
;;; meaningful, it appears that most available figures from other places
;;; are actually minimum times, and so we must use minimum times in
;;; order to allow a more fair and meaningful comparison.  Too bad this
;;; isn't all standardized!
(defun run-one (name &optional (stream *standard-output*))
  (format stream "~&~A: " (get name 'timing-documentation))
  (multiple-value-bind (n-runs real-time cpu-time avg-real avg-cpu real-time-2 cpu-time-2)
      (multiple-timed-duration (get name 'timing-function))
    (format stream
	    #-Minima "CPU: ~7,3F  Page: ~7,3F  Real: ~7,3F  (based on ~D calls)"
	    #+Minima "CPU: ~7,3/GABRIEL:FF/  Page: ~7,3/GABRIEL::FF/  Real: ~7,3/GABRIEL::FF/  (based on ~D calls)"
	    cpu-time-2 (- real-time-2 cpu-time-2) real-time-2 n-runs)
    (values real-time-2 cpu-time-2)))

;;; Run the entire series of benchmarks, and write the results to the
;;; stream, preceeded by the description of the implementation.
(defun run-series (&key (stream *standard-output*) (file-name "GABRIEL"))
  (describe-implementation stream)
  (terpri stream)
  (terpri stream)
  (let ((times (mapcar #'(lambda (name)
			   (multiple-value-bind (real-time cpu-time)
			       (run-one name stream)
			     (cons cpu-time real-time)))
		       *series*)))
    (flet ((dump-series (type accessor)
	     (format stream "~2&(DEFPARAMETER ~A-~A '(" file-name type)
	     (mapc #'(lambda (name time)
		       (format stream
			       #-Minima "~%  (~A ~E)"
			       #+Minima "~%  (~A ~7,7/GABRIEL::FF/)"
			       name (funcall accessor time)))
		   *series* times)
	     (format stream "~%  )~%  "")")))
      (dump-series "CPU" #'car)
      (dump-series "REAL" #'cdr))
    nil))

;;; Do run-series to a file.  Use the FEP file system, because
;;; interrupts are inhibited long enough to break network connections
;;; sometimes, and there might not be a local LMFS.
(defun run-series-to-file
       (&optional (pathname
		    #+Genera (scl:accept 'fs:pathname :default (pathname "FEP:>GABRIEL.TEXT"))
		    #-Genera "GABRIEL.TEXT"))
  (with-open-file (stream pathname :direction :output)
   (format t "~&Writing results to ~A..." (truename stream))
   (run-series :stream stream :file-name (pathname-name pathname))))



;;; Games with declarations.

;;; The effect of type declarations on a Lisp implementation can be explored with the use of
;;; the *IGNORE-DECLARATIONS* compile-time parameter.  It controls whether the MAYBE-DECLARE,
;;; MAYBE-PROCLAIM, and MAYBE-THE forms found in this file expand into the normal declarations
;;; or are ignored.

;;; Minima is draft ANSI CL which doesn't permit macros to expand into declarations.

(eval-when (compile load eval)
(defvar *ignore-declarations* #-Minima nil #+Minima t)
)

(defmacro maybe-declare (&body body)
  (if *ignore-declarations*
      #-Minima `(declare)
      #+Minima nil
      `(declare ,@body)))

(defmacro maybe-proclaim (&body body)
  (if *ignore-declarations*
      nil
      `(proclaim ,@body)))

(defmacro maybe-the (type form)
  (if *ignore-declarations*
      form
      `(the ,type ,form)))



;;; 3.1 TAK

(defun tak (x y z)
  (maybe-declare (type fixnum x y z))
  (if (not (< y x))				;xy
      z
      (tak (tak (maybe-the fixnum (1- x)) y z)
	   (tak (maybe-the fixnum (1- y)) z x)
	   (tak (maybe-the fixnum (1- z)) x y))))

(define-timer tak "Tak" (tak 18. 12. 6.))



;;; 3.2 STAK
;;; TAK using special binding in place of parameter passing.

(defvar *x*)
(defvar *y*)
(defvar *z*)
(maybe-proclaim '(type fixnum *x* *y* *z*))

(defun stak (*x* *y* *z*)
  (stak-aux))

(defun stak-aux ()
  (if (not (< *y* *x*))				;xy
      *z*
      (let ((*x* (let ((*x* (maybe-the fixnum (1- *x*)))
		       (*y* *y*)
		       (*z* *z*))
		   (stak-aux)))
	    (*y* (let ((*x* (maybe-the fixnum (1- *y*)))
		       (*y* *z*)
		       (*z* *x*))
		   (stak-aux)))
	    (*z* (let ((*x* (maybe-the fixnum (1- *z*)))
		       (*y* *x*)
		       (*z* *y*))
		   (stak-aux))))
	(stak-aux))))

(define-timer stak "STak" (stak 18. 12. 6.))



;;; 3.3 CTAK
;;; TAK using CATCH/THROW.
 
(defun ctak (x y z)
  (maybe-declare (type fixnum x y z))
  (catch 'ctak (ctak-aux x y z)))

(defun ctak-aux (x y z)
  (maybe-declare (type fixnum x y z))
  (cond ((not (< y x))				;xy
	 (throw 'ctak z))
	(t (ctak-aux
	     (catch 'ctak
	       (ctak-aux (maybe-the fixnum (1- x))
			 y
			 z))
	     (catch 'ctak
	       (ctak-aux (maybe-the fixnum (1- y))
			 z
			 x))
	     (catch 'ctak
	       (ctak-aux (maybe-the fixnum (1- z))
			 x
			 y))))))

(define-timer ctak "CTak" (ctak 18. 12. 6.))



;;; 3.4 TAKL

(defun listn (n)
  (maybe-declare (type fixnum n))
  (if (not (zerop n))
      (list* n (listn (maybe-the fixnum (1- n))))))

(defvar *l18* (listn 18.))
(defvar *l12* (listn 12.))
(defvar *l6* (listn 6.))
(maybe-proclaim '(type list *l18* *l12* *l6*))

(defun mas (x y z)
  (maybe-declare (type list x y z))
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
	   (mas (cdr y) z x)
	   (mas (cdr z) x y))))

(defun shorterp (x y)
  (maybe-declare (type list x y))
  (and y (or (null x)
	     (shorterp (cdr x)
		       (cdr y)))))

(define-timer takl "TakL" (mas *l18* *l12* *l6*))



;;; 3.5 TAKR
;;; Gross Version to try to trash cache.

(define-timer takr "TakR" (tak0 18. 12. 6.))

(DEFUN TAK0 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK1 (TAK37 (MAYBE-THE FIXNUM (1- X)) Y Z)
		   (TAK11 (MAYBE-THE FIXNUM (1- Y)) Z X)
		   (TAK17 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK1 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK2 (TAK74 (MAYBE-THE FIXNUM (1- X)) Y Z)
		   (TAK22 (MAYBE-THE FIXNUM (1- Y)) Z X)
		   (TAK34 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK2 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK3 (TAK11 (MAYBE-THE FIXNUM (1- X)) Y Z)
		   (TAK33 (MAYBE-THE FIXNUM (1- Y)) Z X)
		   (TAK51 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK3 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK4 (TAK48 (MAYBE-THE FIXNUM (1- X)) Y Z)
		   (TAK44 (MAYBE-THE FIXNUM (1- Y)) Z X)
		   (TAK68 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK4 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK5 (TAK85 (MAYBE-THE FIXNUM (1- X)) Y Z)
		   (TAK55 (MAYBE-THE FIXNUM (1- Y)) Z X)
		   (TAK85 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK5 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK6 (TAK22 (MAYBE-THE FIXNUM (1- X)) Y Z)
		   (TAK66 (MAYBE-THE FIXNUM (1- Y)) Z X)
		   (TAK2 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK6 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK7 (TAK59 (MAYBE-THE FIXNUM (1- X)) Y Z)
		   (TAK77 (MAYBE-THE FIXNUM (1- Y)) Z X)
		   (TAK19 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK7 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK8 (TAK96 (MAYBE-THE FIXNUM (1- X)) Y Z)
		   (TAK88 (MAYBE-THE FIXNUM (1- Y)) Z X)
		   (TAK36 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK8 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK9 (TAK33 (MAYBE-THE FIXNUM (1- X)) Y Z)
		   (TAK99 (MAYBE-THE FIXNUM (1- Y)) Z X)
		   (TAK53 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK9 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK10 (TAK70 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK10 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK70 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK10 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK11 (TAK7 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK21 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK87 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK11 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK12 (TAK44 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK32 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK4 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK12 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK13 (TAK81 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK43 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK21 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK13 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK14 (TAK18 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK54 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK38 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK14 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK15 (TAK55 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK65 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK55 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK15 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK16 (TAK92 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK76 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK72 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK16 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK17 (TAK29 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK87 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK89 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK17 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK18 (TAK66 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK98 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK6 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK18 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK19 (TAK3 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK9 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK23 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK19 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK20 (TAK40 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK20 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK40 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK20 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK21 (TAK77 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK31 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK57 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK21 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK22 (TAK14 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK42 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK74 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK22 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK23 (TAK51 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK53 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK91 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK23 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK24 (TAK88 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK64 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK8 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK24 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK25 (TAK25 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK75 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK25 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK25 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK26 (TAK62 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK86 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK42 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK26 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK27 (TAK99 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK97 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK59 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK27 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK28 (TAK36 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK8 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK76 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK28 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK29 (TAK73 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK19 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK93 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK29 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK30 (TAK10 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK30 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK10 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK30 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK31 (TAK47 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK41 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK27 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK31 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK32 (TAK84 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK52 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK44 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK32 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK33 (TAK21 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK63 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK61 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK33 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK34 (TAK58 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK74 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK78 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK34 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK35 (TAK95 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK85 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK95 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK35 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK36 (TAK32 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK96 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK12 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK36 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK37 (TAK69 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK7 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK29 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK37 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK38 (TAK6 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK18 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK46 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK38 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK39 (TAK43 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK29 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK63 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK39 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK40 (TAK80 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK40 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK80 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK40 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK41 (TAK17 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK51 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK97 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK41 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK42 (TAK54 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK62 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK14 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK42 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK43 (TAK91 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK73 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK31 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK43 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK44 (TAK28 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK84 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK48 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK44 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK45 (TAK65 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK95 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK65 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK45 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK46 (TAK2 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK6 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK82 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK46 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK47 (TAK39 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK17 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK99 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK47 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK48 (TAK76 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK28 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK16 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK48 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK49 (TAK13 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK39 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK33 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK49 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK50 (TAK50 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK50 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK50 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK50 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK51 (TAK87 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK61 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK67 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK51 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK52 (TAK24 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK72 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK84 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK52 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK53 (TAK61 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK83 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK1 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK53 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK54 (TAK98 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK94 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK18 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK54 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK55 (TAK35 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK5 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK35 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK55 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK56 (TAK72 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK16 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK52 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK56 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK57 (TAK9 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK27 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK69 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK57 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK58 (TAK46 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK38 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK86 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK58 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK59 (TAK83 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK49 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK3 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK59 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK60 (TAK20 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK60 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK20 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK60 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK61 (TAK57 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK71 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK37 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK61 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK62 (TAK94 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK82 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK54 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK62 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK63 (TAK31 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK93 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK71 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK63 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK64 (TAK68 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK4 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK88 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK64 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK65 (TAK5 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK15 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK5 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK65 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK66 (TAK42 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK26 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK22 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK66 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK67 (TAK79 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK37 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK39 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK67 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK68 (TAK16 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK48 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK56 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK68 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK69 (TAK53 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK59 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK73 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK69 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK70 (TAK90 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK70 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK90 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK70 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK71 (TAK27 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK81 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK7 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK71 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK72 (TAK64 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK92 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK24 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK72 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK73 (TAK1 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK3 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK41 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK73 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK74 (TAK38 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK14 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK58 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK74 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK75 (TAK75 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK25 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK75 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK75 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK76 (TAK12 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK36 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK92 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK76 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK77 (TAK49 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK47 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK9 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK77 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK78 (TAK86 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK58 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK26 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK78 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK79 (TAK23 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK69 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK43 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK79 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK80 (TAK60 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK80 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK60 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK80 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK81 (TAK97 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK91 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK77 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK81 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK82 (TAK34 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK2 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK94 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK82 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK83 (TAK71 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK13 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK11 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK83 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK84 (TAK8 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK24 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK28 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK84 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK85 (TAK45 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK35 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK45 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK85 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK86 (TAK82 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK46 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK62 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK86 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK87 (TAK19 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK57 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK79 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK87 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK88 (TAK56 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK68 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK96 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK88 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK89 (TAK93 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK79 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK13 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK89 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK90 (TAK30 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK90 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK30 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK90 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK91 (TAK67 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK1 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK47 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK91 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK92 (TAK4 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK12 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK64 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK92 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK93 (TAK41 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK23 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK81 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK93 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK94 (TAK78 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK34 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK98 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK94 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK95 (TAK15 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK45 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK15 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK95 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK96 (TAK52 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK56 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK32 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK96 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK97 (TAK89 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK67 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK49 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK97 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK98 (TAK26 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK78 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK66 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK98 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK99 (TAK63 (MAYBE-THE FIXNUM (1- X)) Y Z)
		    (TAK89 (MAYBE-THE FIXNUM (1- Y)) Z X)
		    (TAK83 (MAYBE-THE FIXNUM (1- Z)) X Y)))))
(DEFUN TAK99 (X Y Z) 
  (MAYBE-DECLARE (TYPE FIXNUM X Y Z))
    (COND ((NOT (< Y X)) Z)
	  (T (TAK0 (TAK0 (MAYBE-THE FIXNUM (1- X)) Y Z)
		   (TAK0 (MAYBE-THE FIXNUM (1- Y)) Z X)
		   (TAK0 (MAYBE-THE FIXNUM (1- Z)) X Y)))))



;;; 3.6 BOYER
;;; The Maclisp Code

(defvar *unify-subst*)
(defvar *temp-temp*)

(DEFUN ADD-LEMMA (TERM)
  (COND ((AND (NOT (ATOM TERM))
	      (EQ (CAR TERM)
		  (QUOTE EQUAL))
	      (NOT (ATOM (CADR TERM))))
	 ;; This change lets you run setup several times.
	 (PUSHNEW TERM (GET (CAR (CADR TERM)) (QUOTE LEMMAS)) :TEST #'EQUAL))
	(T
	 (error "Add lemma did not like term")
	 )))

(DEFUN ADD-LEMMA-LST (LST)
       (COND ((NULL LST)
	      T)
	     (T (ADD-LEMMA (CAR LST))
		(ADD-LEMMA-LST (CDR LST)))))

(DEFUN APPLY-SUBST (ALIST TERM)
  (COND ((ATOM TERM)
	 (COND ((SETQ *TEMP-TEMP* (ASSOC TERM ALIST))
		(CDR *TEMP-TEMP*))
	       (T TERM)))
	(T (CONS (CAR TERM)
		 (APPLY-SUBST-LST ALIST (CDR TERM))))))

(DEFUN APPLY-SUBST-LST (ALIST LST)
       (COND ((NULL LST)
	      NIL)
	     (T (CONS (APPLY-SUBST ALIST (CAR LST))
		      (APPLY-SUBST-LST ALIST (CDR LST))))))

(DEFUN FALSEP (X LST)
       (OR (EQUAL X (QUOTE (F)))
	   (MEMBER X LST :TEST #'EQUAL)))

(DEFUN ONE-WAY-UNIFY (TERM1 TERM2)
       (PROGN (SETQ *UNIFY-SUBST* NIL)
	      (ONE-WAY-UNIFY1 TERM1 TERM2)))

(DEFUN ONE-WAY-UNIFY1 (TERM1 TERM2)
  (COND ((ATOM TERM2)
	 (COND ((SETQ *TEMP-TEMP* (ASSOC TERM2 *UNIFY-SUBST*))
		(EQUAL TERM1 (CDR *TEMP-TEMP*)))
	       (T (SETQ *UNIFY-SUBST* (CONS (CONS TERM2 TERM1)
					    *UNIFY-SUBST*))
		  T)))
	((ATOM TERM1)
	 NIL)
	((EQ (CAR TERM1)
	     (CAR TERM2))
	 (ONE-WAY-UNIFY1-LST (CDR TERM1)
			     (CDR TERM2)))
	(T NIL)))

(DEFUN ONE-WAY-UNIFY1-LST (LST1 LST2)
       (COND ((NULL LST1)
	      T)
	     ((ONE-WAY-UNIFY1 (CAR LST1)
			      (CAR LST2))
	      (ONE-WAY-UNIFY1-LST (CDR LST1)
				  (CDR LST2)))
	     (T NIL)))

(DEFUN REWRITE (TERM)
       (COND ((ATOM TERM)
	      TERM)
	     (T (REWRITE-WITH-LEMMAS (CONS (CAR TERM)
					   (REWRITE-ARGS (CDR TERM)))
				     (GET (CAR TERM)
					      (QUOTE LEMMAS))))))

(DEFUN REWRITE-ARGS (LST)
       (COND ((NULL LST)
	      NIL)
	     (T (CONS (REWRITE (CAR LST))
		      (REWRITE-ARGS (CDR LST))))))

(DEFUN REWRITE-WITH-LEMMAS (TERM LST)
       (COND ((NULL LST)
	      TERM)
	     ((ONE-WAY-UNIFY TERM (CADR (CAR LST)))
	      (REWRITE (APPLY-SUBST *UNIFY-SUBST* (CADDR (CAR LST)))))
	     (T (REWRITE-WITH-LEMMAS TERM (CDR LST)))))

(DEFUN SETUP ()
  (ADD-LEMMA-LST
    (QUOTE ((EQUAL (COMPILE FORM)
		   (REVERSE (CODEGEN (OPTIMIZE FORM)
				     (NIL))))
	    (EQUAL (EQP X Y)
		   (EQUAL (FIX X)
			  (FIX Y)))
	    (EQUAL (GREATERP X Y)
		   (LESSP Y X))
	    (EQUAL (LESSEQP X Y)
		   (NOT (LESSP Y X)))
	    (EQUAL (GREATEREQP X Y)
		   (NOT (LESSP X Y)))
	    (EQUAL (BOOLEAN X)
		   (OR (EQUAL X (T))
		       (EQUAL X (F))))
	    (EQUAL (IFF X Y)
		   (AND (IMPLIES X Y)
			(IMPLIES Y X)))
	    (EQUAL (EVEN1 X)
		   (IF (ZEROP X)
		       (T)
		       (ODD (1- X))))
	    (EQUAL (COUNTPS- L PRED)
		   (COUNTPS-LOOP L PRED (ZERO)))
	    (EQUAL (FACT- I)
		   (FACT-LOOP I 1))
	    (EQUAL (REVERSE- X)
		   (REVERSE-LOOP X (NIL)))
	    (EQUAL (DIVIDES X Y)
		   (ZEROP (remainder Y X)))
	    (EQUAL (ASSUME-TRUE VAR ALIST)
		   (CONS (CONS VAR (T))
			 ALIST))
	    (EQUAL (ASSUME-FALSE VAR ALIST)
		   (CONS (CONS VAR (F))
			 ALIST))
	    (EQUAL (TAUTOLOGY-CHECKER X)
		   (TAUTOLOGYP (NORMALIZE X)
			       (NIL)))
	    (EQUAL (FALSIFY X)
		   (FALSIFY1 (NORMALIZE X)
			     (NIL)))
	    (EQUAL (PRIME X)
		   (AND (NOT (ZEROP X))
			(NOT (EQUAL X (ADD1 (ZERO))))
			(PRIME1 X (1- X))))
	    (EQUAL (AND P Q)
		   (IF P (IF Q (T)
			     (F))
		       (F)))
	    (EQUAL (OR P Q)
		   (IF P (T)
		       (IF Q (T)
			   (F))
		       (F)))
	    (EQUAL (NOT P)
		   (IF P (F)
		       (T)))
	    (EQUAL (IMPLIES P Q)
		   (IF P (IF Q (T)
			     (F))
		       (T)))
	    (EQUAL (FIX X)
		   (IF (NUMBERP X)
		       X
		       (ZERO)))
	    (EQUAL (IF (IF A B C)
		       D E)
		   (IF A (IF B D E)
		       (IF C D E)))
	    (EQUAL (ZEROP X)
		   (OR (EQUAL X (ZERO))
		       (NOT (NUMBERP X))))
	    (EQUAL (PLUS (PLUS X Y)
			 Z)
		   (PLUS X (PLUS Y Z)))
	    (EQUAL (EQUAL (PLUS A B)
			  (ZERO))
		   (AND (ZEROP A)
			(ZEROP B)))
	    (EQUAL (DIFFERENCE X X)
		   (ZERO))
	    (EQUAL (EQUAL (PLUS A B)
			  (PLUS A C))
		   (EQUAL (FIX B)
			  (FIX C)))
	    (EQUAL (EQUAL (ZERO)
			  (DIFFERENCE X Y))
		   (NOT (LESSP Y X)))
	    (EQUAL (EQUAL X (DIFFERENCE X Y))
		   (AND (NUMBERP X)
			(OR (EQUAL X (ZERO))
			    (ZEROP Y))))
	    (EQUAL (MEANING (PLUS-TREE (APPEND X Y))
			    A)
		   (PLUS (MEANING (PLUS-TREE X)
				  A)
			 (MEANING (PLUS-TREE Y)
				  A)))
	    (EQUAL (MEANING (PLUS-TREE (PLUS-FRINGE X))
			    A)
		   (FIX (MEANING X A)))
	    (EQUAL (APPEND (APPEND X Y)
			   Z)
		   (APPEND X (APPEND Y Z)))
	    (EQUAL (REVERSE (APPEND A B))
		   (APPEND (REVERSE B)
			   (REVERSE A)))
	    (EQUAL (TIMES X (PLUS Y Z))
		   (PLUS (TIMES X Y)
			 (TIMES X Z)))
	    (EQUAL (TIMES (TIMES X Y)
			  Z)
		   (TIMES X (TIMES Y Z)))
	    (EQUAL (EQUAL (TIMES X Y)
			  (ZERO))
		   (OR (ZEROP X)
		       (ZEROP Y)))
	    (EQUAL (EXEC (APPEND X Y)
			 PDS ENVRN)
		   (EXEC Y (EXEC X PDS ENVRN)
			 ENVRN))
	    (EQUAL (MC-FLATTEN X Y)
		   (APPEND (FLATTEN X)
			   Y))
	    (EQUAL (MEMBER X (APPEND A B))
		   (OR (MEMBER X A)
		       (MEMBER X B)))
	    (EQUAL (MEMBER X (REVERSE Y))
		   (MEMBER X Y))
	    (EQUAL (LENGTH (REVERSE X))
		   (LENGTH X))
	    (EQUAL (MEMBER A (INTERSECT B C))
		   (AND (MEMBER A B)
			(MEMBER A C)))
	    (EQUAL (NTH (ZERO)
			I)
		   (ZERO))
	    (EQUAL (EXP I (PLUS J K))
		   (TIMES (EXP I J)
			  (EXP I K)))
	    (EQUAL (EXP I (TIMES J K))
		   (EXP (EXP I J)
			K))
	    (EQUAL (REVERSE-LOOP X Y)
		   (APPEND (REVERSE X)
			   Y))
	    (EQUAL (REVERSE-LOOP X (NIL))
		   (REVERSE X))
	    (EQUAL (COUNT-LIST Z (SORT-LP X Y))
		   (PLUS (COUNT-LIST Z X)
			 (COUNT-LIST Z Y)))
	    (EQUAL (EQUAL (APPEND A B)
			  (APPEND A C))
		   (EQUAL B C))
	    (EQUAL (PLUS (REMAINDER X Y)
			 (TIMES Y (QUOTIENT X Y)))
		   (FIX X))
	    (EQUAL (POWER-EVAL (BIG-PLUS1 L I BASE)
			       BASE)
		   (PLUS (POWER-EVAL L BASE)
			 I))
	    (EQUAL (POWER-EVAL (BIG-PLUS X Y I BASE)
			       BASE)
		   (PLUS I (PLUS (POWER-EVAL X BASE)
				 (POWER-EVAL Y BASE))))
	    (EQUAL (REMAINDER Y 1)
		   (ZERO))
	    (EQUAL (LESSP (REMAINDER X Y)
			  Y)
		   (NOT (ZEROP Y)))
	    (EQUAL (REMAINDER X X)
		   (ZERO))
	    (EQUAL (LESSP (QUOTIENT I J)
			  I)
		   (AND (NOT (ZEROP I))
			(OR (ZEROP J)
			    (NOT (EQUAL J 1)))))
	    (EQUAL (LESSP (REMAINDER X Y)
			  X)
		   (AND (NOT (ZEROP Y))
			(NOT (ZEROP X))
			(NOT (LESSP X Y))))
	    (EQUAL (POWER-EVAL (POWER-REP I BASE)
			       BASE)
		   (FIX I))
	    (EQUAL (POWER-EVAL (BIG-PLUS (POWER-REP I BASE)
					 (POWER-REP J BASE)
					 (ZERO)
					 BASE)
			       BASE)
		   (PLUS I J))
	    (EQUAL (GCD X Y)
		   (GCD Y X))
	    (EQUAL (NTH (APPEND A B)
			I)
		   (APPEND (NTH A I)
			   (NTH B (DIFFERENCE I (LENGTH A)))))
	    (EQUAL (DIFFERENCE (PLUS X Y)
			       X)
		   (FIX Y))
	    (EQUAL (DIFFERENCE (PLUS Y X)
			       X)
		   (FIX Y))
	    (EQUAL (DIFFERENCE (PLUS X Y)
			       (PLUS X Z))
		   (DIFFERENCE Y Z))
	    (EQUAL (TIMES X (DIFFERENCE C W))
		   (DIFFERENCE (TIMES C X)
			       (TIMES W X)))
	    (EQUAL (REMAINDER (TIMES X Z)
			      Z)
		   (ZERO))
	    (EQUAL (DIFFERENCE (PLUS B (PLUS A C))
			       A)
		   (PLUS B C))
	    (EQUAL (DIFFERENCE (ADD1 (PLUS Y Z))
			       Z)
		   (ADD1 Y))
	    (EQUAL (LESSP (PLUS X Y)
			  (PLUS X Z))
		   (LESSP Y Z))
	    (EQUAL (LESSP (TIMES X Z)
			  (TIMES Y Z))
		   (AND (NOT (ZEROP Z))
			(LESSP X Y)))
	    (EQUAL (LESSP Y (PLUS X Y))
		   (NOT (ZEROP X)))
	    (EQUAL (GCD (TIMES X Z)
			(TIMES Y Z))
		   (TIMES Z (GCD X Y)))
	    (EQUAL (VALUE (NORMALIZE X)
			  A)
		   (VALUE X A))
	    (EQUAL (EQUAL (FLATTEN X)
			  (CONS Y (NIL)))
		   (AND (NLISTP X)
			(EQUAL X Y)))
	    (EQUAL (LISTP (GOPHER X))
		   (LISTP X))
	    (EQUAL (SAMEFRINGE X Y)
		   (EQUAL (FLATTEN X)
			  (FLATTEN Y)))
	    (EQUAL (EQUAL (GREATEST-FACTOR X Y)
			  (ZERO))
		   (AND (OR (ZEROP Y)
			    (EQUAL Y 1))
			(EQUAL X (ZERO))))
	    (EQUAL (EQUAL (GREATEST-FACTOR X Y)
			  1)
		   (EQUAL X 1))
	    (EQUAL (NUMBERP (GREATEST-FACTOR X Y))
		   (NOT (AND (OR (ZEROP Y)
				 (EQUAL Y 1))
			     (NOT (NUMBERP X)))))
	    (EQUAL (TIMES-LIST (APPEND X Y))
		   (TIMES (TIMES-LIST X)
			  (TIMES-LIST Y)))
	    (EQUAL (PRIME-LIST (APPEND X Y))
		   (AND (PRIME-LIST X)
			(PRIME-LIST Y)))
	    (EQUAL (EQUAL Z (TIMES W Z))
		   (AND (NUMBERP Z)
			(OR (EQUAL Z (ZERO))
			    (EQUAL W 1))))
	    (EQUAL (GREATEREQPR X Y)
		   (NOT (LESSP X Y)))
	    (EQUAL (EQUAL X (TIMES X Y))
		   (OR (EQUAL X (ZERO))
		       (AND (NUMBERP X)
			    (EQUAL Y 1))))
	    (EQUAL (REMAINDER (TIMES Y X)
			      Y)
		   (ZERO))
	    (EQUAL (EQUAL (TIMES A B)
			  1)
		   (AND (NOT (EQUAL A (ZERO)))
			(NOT (EQUAL B (ZERO)))
			(NUMBERP A)
			(NUMBERP B)
			(EQUAL (1- A)
			       (ZERO))
			(EQUAL (1- B)
			       (ZERO))))
	    (EQUAL (LESSP (LENGTH (DELETE X L))
			  (LENGTH L))
		   (MEMBER X L))
	    (EQUAL (SORT2 (DELETE X L))
		   (DELETE X (SORT2 L)))
	    (EQUAL (DSORT X)
		   (SORT2 X))
	    (EQUAL (LENGTH (CONS X1
				 (CONS X2
				       (CONS X3 (CONS X4
						      (CONS X5
							    (CONS X6 X7)))))))
		   (PLUS 6 (LENGTH X7)))
	    (EQUAL (DIFFERENCE (ADD1 (ADD1 X))
			       2)
		   (FIX X))
	    (EQUAL (QUOTIENT (PLUS X (PLUS X Y))
			     2)
		   (PLUS X (QUOTIENT Y 2)))
	    (EQUAL (SIGMA (ZERO)
			  I)
		   (QUOTIENT (TIMES I (ADD1 I))
			     2))
	    (EQUAL (PLUS X (ADD1 Y))
		   (IF (NUMBERP Y)
		       (ADD1 (PLUS X Y))
		       (ADD1 X)))
	    (EQUAL (EQUAL (DIFFERENCE X Y)
			  (DIFFERENCE Z Y))
		   (IF (LESSP X Y)
		       (NOT (LESSP Y Z))
		       (IF (LESSP Z Y)
			   (NOT (LESSP Y X))
			   (EQUAL (FIX X)
				  (FIX Z)))))
	    (EQUAL (MEANING (PLUS-TREE (DELETE X Y))
			    A)
		   (IF (MEMBER X Y)
		       (DIFFERENCE (MEANING (PLUS-TREE Y)
					    A)
				   (MEANING X A))
		       (MEANING (PLUS-TREE Y)
				A)))
	    (EQUAL (TIMES X (ADD1 Y))
		   (IF (NUMBERP Y)
		       (PLUS X (TIMES X Y))
		       (FIX X)))
	    (EQUAL (NTH (NIL)
			I)
		   (IF (ZEROP I)
		       (NIL)
		       (ZERO)))
	    (EQUAL (LAST (APPEND A B))
		   (IF (LISTP B)
		       (LAST B)
		       (IF (LISTP A)
			   (CONS (CAR (LAST A))
				 B)
			   B)))
	    (EQUAL (EQUAL (LESSP X Y)
			  Z)
		   (IF (LESSP X Y)
		       (EQUAL T Z)
		       (EQUAL F Z)))
	    (EQUAL (ASSIGNMENT X (APPEND A B))
		   (IF (ASSIGNEDP X A)
		       (ASSIGNMENT X A)
		       (ASSIGNMENT X B)))
	    (EQUAL (CAR (GOPHER X))
		   (IF (LISTP X)
		       (CAR (FLATTEN X))
		       (ZERO)))
	    (EQUAL (FLATTEN (CDR (GOPHER X)))
		   (IF (LISTP X)
		       (CDR (FLATTEN X))
		       (CONS (ZERO)
			     (NIL))))
	    (EQUAL (QUOTIENT (TIMES Y X)
			     Y)
		   (IF (ZEROP Y)
		       (ZERO)
		       (FIX X)))
	    (EQUAL (GET J (SET I VAL MEM))
		   (IF (EQP J I)
		       VAL
		       (GET J MEM)))))))

(DEFUN TAUTOLOGYP (X TRUE-LST FALSE-LST)
       (COND ((TRUEP X TRUE-LST)
	      T)
	     ((FALSEP X FALSE-LST)
	      NIL)
	     ((ATOM X)
	      NIL)
	     ((EQ (CAR X)
		  (QUOTE IF))
	      (COND ((TRUEP (CADR X)
			    TRUE-LST)
		     (TAUTOLOGYP (CADDR X)
				 TRUE-LST FALSE-LST))
		    ((FALSEP (CADR X)
			     FALSE-LST)
		     (TAUTOLOGYP (CADDDR X)
				 TRUE-LST FALSE-LST))
		    (T (AND (TAUTOLOGYP (CADDR X)
					(CONS (CADR X)
					      TRUE-LST)
					FALSE-LST)
			    (TAUTOLOGYP (CADDDR X)
					TRUE-LST
					(CONS (CADR X)
					      FALSE-LST))))))
	     (T NIL)))

(DEFUN TAUTP (X)
       (TAUTOLOGYP (REWRITE X)
		   NIL NIL))
(DEFUN TEST NIL
       (PROG (ANS TERM)
	     (SETQ TERM
		   (APPLY-SUBST
		     (QUOTE ((X F (PLUS (PLUS A B)
					(PLUS C (ZERO))))
			     (Y F (TIMES (TIMES A B)
					 (PLUS C D)))
			     (Z F (REVERSE (APPEND (APPEND A B)
						   (NIL))))
			     (U EQUAL (PLUS A B)
				(DIFFERENCE X Y))
			     (W LESSP (REMAINDER A B)
				(MEMBER A (LENGTH B)))))
		     (QUOTE (IMPLIES (AND (IMPLIES X Y)
					  (AND (IMPLIES Y Z)
					       (AND (IMPLIES Z U)
						    (IMPLIES U W))))
				     (IMPLIES X W)))))
	     (SETQ ANS (TAUTP TERM))
	     (RETURN ANS)))

#||
;;; These functions are not used.

(DEFUN TRANS-OF-IMPLIES (N)
       (LIST (QUOTE IMPLIES)
	     (TRANS-OF-IMPLIES1 N)
	     (LIST (QUOTE IMPLIES)
		   0 N)))

(DEFUN TRANS-OF-IMPLIES1 (N)
       (COND ((EQUAL N 1)
	      (LIST (QUOTE IMPLIES)
		    0 1))
	     (T (LIST (QUOTE AND)
		      (LIST (QUOTE IMPLIES)
			    (1- N)
			    N)
		      (TRANS-OF-IMPLIES1 (1- N))))))
||#

(DEFUN TRUEP (X LST)
       (OR (EQUAL X (QUOTE (T)))
	   (MEMBER X LST :TEST #'EQUAL)))

(SETUP)

(define-timer boyer "Boyer" (test))



;;; 3.7 BROWSE
;;; Benchmark to create and browse through an AI-like data base of units

;;; n is # of symbols
;;; m is maximum amount of stuff on the plist
;;; npats is the number of basic patterns on the unit
;;; ipats is the instantiated copies of the patterns

(defvar *rand* 21.)

(defmacro char1 (x) `(char (symbol-name ,x) 0))

(defun init (n m npats ipats)
  (maybe-declare (type fixnum n m npats))
  (let ((ipats (copy-tree ipats)))
    (do ((p ipats (cdr p)))
	((null (cdr p)) (rplacd p ipats)))
    (do ((n n (maybe-the fixnum (1- n)))
	 (i m (cond ((= i 0) m)
		    (t (maybe-the fixnum (1- i)))))
	 (name (gensym) (gensym))		;GENTEMP?
	 (a ()))
	((= n 0) a)
      (maybe-declare (type fixnum n i)) 
      (push name a)
      (do ((i i (maybe-the fixnum (1- i))))
	  ((= i 0))
	(maybe-declare (type fixnum i))
	(setf (get name (gensym)) ()))
      (setf (get name 'pattern) 
	    (do ((i npats (maybe-the fixnum (1- i)))
		 (ipats ipats (cdr ipats))
		 (a ()))
		((= i 0) a)
	      (maybe-declare (type fixnum i))
	      (push (car ipats) a)))
      (do ((j (maybe-the fixnum (- m i)) (maybe-the fixnum (1- j))))
	  ((= j 0))
	(maybe-declare (type fixnum j))
	(setf (get name (gensym) ) ())))))  

(defun browse-random () (setq *rand* (mod (maybe-the fixnum (* *rand* 17.)) 251.)))

(defun randomize (l)
  (do ((a ()))
      ((null l) a)
    (let ((n (mod (maybe-the fixnum (browse-random)) (maybe-the fixnum (length l)))))
      (maybe-declare (type fixnum n))
      (cond ((= n 0)
	     (push (car l) a)
	     (setq l (cdr l)))
	    (t 
	     (do ((n n (maybe-the fixnum (1- n)))
		  (x l (cdr x)))
		 ((= n 1)
		  (push (cadr x) a)
		  (rplacd x (cddr x)))
	       (maybe-declare (type fixnum n))))))))

(defun match (pat dat alist)
  (cond ((null pat)
	 (null dat))
	((null dat) ())
	((or (eq (car pat) '\?)
	     (eq (car pat)
		 (car dat)))
	 (match (cdr pat) (cdr dat) alist))
	((eq (car pat) '*)
	 (or (match (cdr pat) dat alist)
	     (match (cdr pat) (cdr dat) alist)
	     (match pat (cdr dat) alist)))
	(t (cond ((atom (car pat))
		  (cond ((eql (char1 (car pat)) #\?)	; long story
			 (let ((val (assoc (car pat) alist)))
			   (cond (val (match (cons (cdr val)
						   (cdr pat))
					     dat alist))
				 (t (match (cdr pat)
					   (cdr dat)
					   (cons (cons (car pat)
						       (car dat))
						 alist))))))
			((eql (char1 (car pat)) #\*)
			 (let ((val (assoc (car pat) alist)))
			   (cond (val (match (append (cdr val)
						     (cdr pat))
					     dat alist))
				 (t 
				  (do ((l () (nconc l (cons (car d) nil)))
				       (e (cons () dat) (cdr e))
				       (d dat (cdr d)))
				      ((null e) ())
				    (cond ((match (cdr pat) d
						  (cons (cons (car pat) l)
							alist))
					   (return t))))))))))
		 (t (and 
		      (not (atom (car dat)))
		      (match (car pat)
			     (car dat) alist)
		      (match (cdr pat)
			     (cdr dat) alist)))))))

(defun browse ()
  (setf *rand* 21)
  (investigate (randomize 
		 (init 100. 10. 4. '((a a a b b b b a a a a a b b a a a)
				     (a a b b b b a a
					(a a)(b b))
				     (a a a b (b a) b a b a))))
	       '((*a \?b *b \?b a *a a *b *a)
		 (*a *b *b *a (*a) (*b))
		 (\? \? * (b a) * \? \?))))

(defun investigate (units pats)
  (do ((units units (cdr units)))
      ((null units))
    (do ((pats pats (cdr pats)))
	((null pats))
      (do ((p (get (car units) 'pattern)
	      (cdr p)))
	  ((null p))
	(match (car pats) (car p) ())))))
       
(define-timer browse "Browse" (browse))



;;; 3.8 DESTRU
;;; Destructive operation benchmark

(defun destructive (n m)
  (maybe-declare (type fixnum n m))
  (let ((l (do ((i 10. (maybe-the fixnum (1- i)))
		(a () (push () a)))
	       ((= (maybe-the fixnum i) 0) a)
	     (maybe-declare (type fixnum i)))))
    (do ((i n (maybe-the fixnum (1- i))))
	((= (maybe-the fixnum i) 0))
      (maybe-declare (type fixnum i))
      (cond ((null (car l))
	     (do ((l l (cdr l)))
		 ((null l))
	       (or (car l) 
		   (rplaca l (cons () ())))
	       (nconc (car l)
		      (do ((j m (maybe-the fixnum (1- j)))
			   (a () (push () a)))
			  ((= (maybe-the fixnum j) 0) a)
			(maybe-declare (type fixnum j)))))) 
	    (t
	     (do ((l1 l (cdr l1))
		  (l2 (cdr l) (cdr l2)))
		 ((null l2))
	       (rplacd (do ((j (floor (maybe-the fixnum (length (car l2))) 2)
			       (maybe-the fixnum (1- j)))
			    (a (car l2) (cdr a)))
			   ((= (maybe-the fixnum j) 0) a)
			 (maybe-declare (type fixnum j))
			 (rplaca a i))
		       (let ((n (floor (maybe-the fixnum (length (car l1))) 2)))
			 (cond ((= (maybe-the fixnum n) 0) (rplaca l1 ())
				(car l1))
			       (t 
				(do ((j n (maybe-the fixnum (1- j)))
				     (a (car l1) (cdr a)))
				    ((= (maybe-the fixnum j) 1)
				     (prog1 (cdr a)
					    (rplacd a ())))
				  (maybe-declare (type fixnum j))
				  (rplaca a i))))))))))))

(define-timer destru "Destruct" (destructive 600. 50.))



;;; 3.9 TRAVERSE
;;; Benchmark to create once and traverse a Structure

(defstruct node
  (parents ())
  (sons ())
  (sn (snb))
  (entry1 ())
  (entry2 ())
  (entry3 ())
  (entry4 ())
  (entry5 ())
  (entry6 ())
  (mark ()))

(defvar *sn* 0)
;(defvar *rand* 21.) already declared above for BROWSE
(defvar *count* 0)
(defvar *marker* nil)
(defvar *root*)

(defun snb () (setq *sn* (maybe-the fixnum (1+ *sn*))))

(defun seed () (setq *rand* 21.))

(defun traverse-random ()
  (setq *rand* (maybe-the fixnum (mod (maybe-the fixnum (* *rand* 17.)) 251.))))

(defun traverse-remove (n q)
  (maybe-declare (type fixnum n))
  (cond ((eq (cdr (car q)) (car q))
	 (prog2 () (caar q) (rplaca q ())))
	((= n 0)
	 (prog2 () (caar q)
		(do ((p (car q) (cdr p)))
		    ((eq (cdr p) (car q))
		     (rplaca q
			     (rplacd p (cdr (car q))))))))
	(t (do ((n n (maybe-the fixnum (1- n)))
		(q (car q) (cdr q))
		(p (cdr (car q)) (cdr p)))
	       ((= n 0) (prog2 () (car q) (rplacd q p)))
	     (maybe-declare (type fixnum n))))))

(defun traverse-select (n q)
  (maybe-declare (type fixnum n))
  (do ((n n (maybe-the fixnum (1- n)))
       (q (car q) (cdr q)))
      ((= n 0) (car q))
    (maybe-declare (type fixnum n))))

(defun add (a q)
  (cond ((null q)
	 `(,(let ((x `(,a)))
	      (rplacd x x) x)))
	((null (car q))
	 (let ((x `(,a)))
	   (rplacd x x)
	   (rplaca q x)))
	(t (rplaca q
		   (rplacd (car q) `(,a .,(cdr (car q))))))))

(defun create-structure (n)
  (maybe-declare (type fixnum n))
  (let ((a `(,(make-node))))
    (do ((m (maybe-the fixnum (1- n)) (maybe-the fixnum (1- m)))
	 (p a))
	((= m 0) (setq a `(,(rplacd p a)))
	 (do ((unused a)
	      (used (add (traverse-remove 0 a) ()))
	      (x) (y))
	     ((null (car unused))
	      (find-root (traverse-select 0 used) n))
	   (setq x (traverse-remove (maybe-the fixnum
					       (rem (maybe-the fixnum (traverse-random)) n))
				    unused))
	   (setq y (traverse-select (maybe-the fixnum
					       (rem (maybe-the fixnum (traverse-random)) n))
				    used))
	   (add x used)
	   (setf (node-sons y) `(,x .,(node-sons y)))
	   (setf (node-parents x) `(,y .,(node-parents x))) ))
      (maybe-declare (type fixnum m))
      (push (make-node) a))))

(defun find-root (node n)
  (maybe-declare (type fixnum n))
  (do ((n n (maybe-the fixnum (1- n))))
      ((= n 0) node)
    (maybe-declare (type fixnum n))
    (cond ((null (node-parents node))
	   (return node))
	  (t (setq node (car (node-parents node)))))))

(defun travers (node mark)
  (cond ((eq (node-mark node) mark) ())
	(t (setf (node-mark node) mark)
	   (setq *count* (maybe-the fixnum (1+ *count*)))
	   (setf (node-entry1 node) (not (node-entry1 node)))
	   (setf (node-entry2 node) (not (node-entry2 node)))
	   (setf (node-entry3 node) (not (node-entry3 node)))
	   (setf (node-entry4 node) (not (node-entry4 node)))
	   (setf (node-entry5 node) (not (node-entry5 node)))
	   (setf (node-entry6 node) (not (node-entry6 node)))
	   (do ((sons (node-sons node) (cdr sons)))
	       ((null sons) ())
	     (travers (car sons) mark)))))

(defun traverse (*root*)
  (let ((*count* 0))
    (travers *root* (setq *marker* (not *marker*)))
    *count*))

(define-timer traverse-init "Traverse, Initialize"
  (seed)
  (setq *root* (create-structure 100.))
  nil)
		 
(define-timer traverse "Traverse, Traverse"
  (do ((i 50. (maybe-the fixnum (1- i))))
      ((= (maybe-the fixnum i) 0))
    (maybe-declare (type fixnum i))
    (traverse *root*)
    (traverse *root*)
    (traverse *root*)
    (traverse *root*)
    (traverse *root*)))



;;; 3.10 DERIV

(DEFUN deriv-aux (A) (LIST '/ (DERIV A) A))

(DEFUN DERIV (A)
  (COND 
    ((ATOM A)
     (COND ((EQ A 'X) 1) (T 0)))
    ((EQ (CAR A) '+)
     (CONS '+ (MAPCAR #'DERIV (CDR A))))
    ((EQ (CAR A) '-) 
     (CONS '- (MAPCAR #'DERIV (CDR A))))
    ((EQ (CAR A) '*)
     (LIST '*
	   A 
	   (CONS '+ (MAPCAR 'deriv-aux (CDR A)))))
    ((EQ (CAR A) '/)
     (LIST '-
	   (LIST '/
		 (DERIV (CADR A)) 
		 (CADDR A))
	   (LIST '/
		 (CADR A) 
		 (LIST '*
		       (CADDR A)
		       (CADDR A)
		       (DERIV (CADDR A))))))
    (T 'ERROR)))

(DEFUN RUN-DERIV ()
  (DO ((I 0 (MAYBE-THE FIXNUM (1+ I))))
      ((= (MAYBE-THE FIXNUM I) 1000.))
    (MAYBE-DECLARE (TYPE FIXNUM I))
    (DERIV '(+ (* 3 X X) (* A X X) (* B X) 5))
    (DERIV '(+ (* 3 X X) (* A X X) (* B X) 5))
    (DERIV '(+ (* 3 X X) (* A X X) (* B X) 5))
    (DERIV '(+ (* 3 X X) (* A X X) (* B X) 5))
    (DERIV '(+ (* 3 X X) (* A X X) (* B X) 5))))

(define-timer deriv "Deriv" (run-deriv))



;;; 3.11 DDERIV

(DEFUN dderiv-aux (A) (LIST '/ (DDERIV A) A))

(DEFUN +DDERIV (A)
  (CONS '+ (MAPCAR #'DDERIV A)))

(DEFUN -DDERIV (A)
  (CONS '- (MAPCAR #'DDERIV A)))

(DEFUN *DDERIV (A)
	(LIST '* (CONS '* A)
		(CONS '+ (MAPCAR #'dderiv-aux A))))

(DEFUN /DDERIV (A)
       (LIST '- 
	     (LIST '/ 
		   (DDERIV (CAR A)) 
		   (CADR A))
	     (LIST '/ 
		   (CAR A) 
		   (LIST '*
			 (CADR A)
			 (CADR A)
			 (DDERIV (CADR A))))))

(DEFUN DDERIV (A)
  (COND 
    ((ATOM A)
     (COND ((EQ A 'X) 1) (T 0)))
    (T (LET ((DDERIV (GET (CAR A) 'DDERIV)))
	 (COND (DDERIV (FUNCALL DDERIV (CDR A)))
	       (T 'ERROR))))))

(defun setup-dderiv ()
  (mapc #'(lambda (op fun)
	   (setf (get op 'dderiv) (symbol-function fun)))
	'(+ - * /)
	'(+dderiv -dderiv *dderiv /dderiv)))

(setup-dderiv)

(DEFUN RUN-dderiv ()
  (DO ((I 0 (MAYBE-THE FIXNUM (1+ I))))
      ((= (MAYBE-THE FIXNUM I) 1000.))
    (MAYBE-DECLARE (TYPE FIXNUM I))
    (DDERIV '(+ (* 3 X X) (* A X X) (* B X) 5))
    (DDERIV '(+ (* 3 X X) (* A X X) (* B X) 5))
    (DDERIV '(+ (* 3 X X) (* A X X) (* B X) 5))
    (DDERIV '(+ (* 3 X X) (* A X X) (* B X) 5))
    (DDERIV '(+ (* 3 X X) (* A X X) (* B X) 5))))

(define-timer dderiv "DDeriv" (run-dderiv))



;;; 3.13 DIV2
;;; Dividing by 2 using lists of n ()'s

(defun create-n (n)
  (maybe-declare (type fixnum n))
  (do ((n n (maybe-the fixnum (1- n)))
       (a () (push () a)))
      ((= (maybe-the fixnum n) 0) a)
    (maybe-declare (type fixnum n))))

(defvar *div2-l* (create-n 200.))

(defun iterative-div2 (l)
  (do ((l l (cddr l))
       (a () (push (car l) a)))
      ((null l) a)))

(defun recursive-div2 (l)
  (cond ((null l) ())
	(t (cons (car l) (recursive-div2 (cddr l))))))

(defun iterative-div2-test (l)
  (do ((i 300. (maybe-the fixnum (1- i))))
      ((= (maybe-the fixnum i) 0))
    (maybe-declare (type fixnum i))
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)))

(defun recursive-div2-test (l)
  (do ((i 300. (maybe-the fixnum (1- i))))
      ((= (maybe-the fixnum i) 0))
    (maybe-declare (type fixnum i))
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)))

(define-timer div2-i "Div2, Iterative" (iterative-div2-test *div2-l*))

(define-timer div2-r "Div2, Recursive" (recursive-div2-test *div2-l*))



;;; 3.14 FFT

;;; FFT	-*- Mode:Lisp; Package:User; Base:10; Lowercase:T; -*-
;;;Barrow FFT
;;;Here is the Barrow FFT benchmark which tests floating operations
;;;of various types, including flonum arrays. (ARRAYCALL FLONUM A I)
;;;accesses the I'th element of the FLONUM array A, where these arrays are
;;;0-based. (STORE (ARRAYCALL FLONUM A I) V) stores the value V in the
;;;I'th element of the FLONUM array A. 

;;;There was a fair amount of FLONUM GC's in the SAIL MacLisp run, which,
;;;when it needed to CORE up during GC, took 4.5 seconds of CPU time for the
;;;computation and 15 seconds for GC. Other configurations of memory required
;;;only 1.5 seconds for GC.

;;;Refer to this as FFT.
;;;			-rpg-

;;; From Rich Duda, by way of Harry Barrow -- 3/26/82 

(defvar *re* (make-array 1025. :element-type 'single-float ':initial-element 0.0))	
(defvar *im* (make-array 1025. :element-type 'single-float ':initial-element 0.0))

(Defun FFT					;Fast Fourier Transform
       (AREAL AIMAG)				;AREAL = real part 
  (MAYBE-DECLARE (TYPE (SIMPLE-ARRAY SINGLE-FLOAT (*)) AREAL AIMAG))
  (PROG						;AIMAG = imaginary part
    ((AR AREAL) (AI AIMAG)
     (I 1) (J 0) (K 0) (M 0) (N 0) (LE 0) (LE1 0) (IP 0) (NV2 0) (NM1 0)
     (UR 0E0) (UI 0E0) (WR 0E0) (WI 0E0) (TR 0E0) (TI 0E0))
    (MAYBE-DECLARE (TYPE FIXNUM I J K N NV2 NM1 M LE LE1 IP))
    (MAYBE-DECLARE (TYPE (SIMPLE-ARRAY SINGLE-FLOAT (*)) AR AI))
    (MAYBE-DECLARE (TYPE SINGLE-FLOAT UR UI WR WI TR TI))
    (SETQ AR AREAL				;Initialize
          AI AIMAG
	  N (ARRAY-DIMENSION AR 0)
	  N (MAYBE-THE FIXNUM (1- N))
	  NV2 (MAYBE-THE (VALUES FIXNUM FIXNUM) (FLOOR N 2))
	  NM1 (MAYBE-THE FIXNUM (1- N))
	  M 0					;Compute M = log(N)
	  I 1)
 L1 (COND ((< I N)
	   (SETQ M (MAYBE-THE FIXNUM (1+ M))
		 I (MAYBE-THE FIXNUM (+ I I)))
	   (GO L1)))
    (COND ((NOT (EQUAL N (MAYBE-THE FIXNUM (EXPT 2 M))))
	   (PRINC "Error ... array size not a power of two.")
	   (READ)
	   (RETURN (TERPRI))))
    (SETQ J 1					;Interchange elements
	  I 1)					;in bit-reversed order
 L3 (COND ((< I J)
	   (SETQ TR (AREF AR J)
		 TI (AREF AI J))
	   (SETF (AREF AR J) (AREF AR I))
	   (SETF (AREF AI J) (AREF AI I))
	   (SETF (AREF AR I) TR)
	   (SETF (AREF AI I) TI)))
    (SETQ K NV2)
 L6 (COND ((< K J) 
	   (SETQ J (MAYBE-THE FIXNUM (- J K))
		 K (MAYBE-THE FIXNUM (FLOOR K 2)))
	   (GO L6)))
    (SETQ J (MAYBE-THE FIXNUM (+ J K))
	  I (MAYBE-THE FIXNUM (1+ I)))
    (COND ((< I N)
	   (GO L3)))
    (DO ((L 1 (MAYBE-THE FIXNUM (1+ (MAYBE-THE FIXNUM L)))))
	((> (MAYBE-THE FIXNUM L) M))			;Loop thru stages
      (MAYBE-DECLARE (TYPE FIXNUM L))
      (SETQ LE (MAYBE-THE FIXNUM (EXPT 2 L))
	    LE1 (MAYBE-THE (VALUES FIXNUM FIXNUM) (FLOOR LE 2))
	    UR 1.0
	    UI 0.0
	    WR (COS (/ (FLOAT PI 0E0) (FLOAT LE1)))
	    WI (SIN (/ (FLOAT PI 0E0) (FLOAT LE1))))
      (DO ((J 1 (MAYBE-THE FIXNUM (1+ J))))
	  ((> (MAYBE-THE FIXNUM J) LE1))		;Loop thru butterflies
	(MAYBE-DECLARE (TYPE FIXNUM J))
	(DO ((I J (+ (MAYBE-THE FIXNUM I) LE)))
	    ((> (MAYBE-THE FIXNUM I) N))		;Do a butterfly
	  (MAYBE-DECLARE (TYPE FIXNUM I))
	  (SETQ IP (MAYBE-THE FIXNUM (+ I LE1))
		TR (- (* (AREF AR IP) UR)
		      (* (AREF AI IP) UI))
		TI (+ (* (AREF AR IP) UI)
		      (* (AREF AI IP) UR)))
	  (SETF (AREF AR IP) (- (AREF AR I) TR))
	  (SETF (AREF AI IP) (- (AREF AI I) TI))
	  (SETF (AREF AR I) (+ (AREF AR I) TR))
	  (SETF (AREF AI I) (+ (AREF AI I) TI))))
      (SETQ TR (- (* UR WR) (* UI WI))
	    TI (+ (* UR WI) (* UI WR))
	    UR TR
	    UI TI))
    (RETURN T)))

(define-timer fft "FFT"
  (do ((ntimes 0 (1+ ntimes)))
      ((= ntimes 10.))
    (fft *re* *im*)))



;;; 3.15 PUZZLE

(defconstant size 511.)
(defconstant classmax 3.)
(defconstant typemax 12.)

(defconstant true t)
(defconstant false ())

(defvar *iii* 0)
(defvar *kount* 0)
(defvar *d* 8.)
(maybe-proclaim '(type fixnum *iii* *kount* *d*))

(defvar *piececount* (make-array (1+ classmax) ':initial-element 0))
(defvar *class* (make-array (1+ typemax) ':initial-element 0))
(defvar *piecemax* (make-array (1+ typemax) ':initial-element 0))
(defvar *puzzle* (make-array (1+ size)))
(defvar *p* (make-array (list (1+ typemax) (1+ size))))

(maybe-proclaim '(type simple-vector *piececount* *class* *piecemax* *puzzle*))

(maybe-proclaim '(type (simple-array t (* *)) *p*))

(defun fit (i j)
  (maybe-declare (type fixnum i j))
  #+Genera (declare (sys:optimize-array-references-in-loops))
  (let ((end (aref *piecemax* i)))
    (maybe-declare (type fixnum end))
    (do ((k 0 (maybe-the fixnum (1+ k))))
	((> (maybe-the fixnum k) end) true)
      (cond ((aref *p* i k)
	     (cond ((aref *puzzle* (maybe-the fixnum (+ j k)))
		    (return false))))))))

 
(defun place (i j)
  (maybe-declare (type fixnum i j))
  #+Genera (declare (sys:optimize-array-references-in-loops))
  (let ((end (aref *piecemax* i)))
    (maybe-declare (type fixnum end))
    (do ((k 0 (maybe-the fixnum (1+ (maybe-the fixnum k)))))
	((> (maybe-the fixnum k) end))
      (maybe-declare (type fixnum k))
      (cond ((aref *p* i k) 
	     (setf (aref *puzzle* (maybe-the fixnum (+ j k))) true))))
    (setf (aref *piececount* (maybe-the fixnum (aref *class* i)))
	  (maybe-the fixnum (- (maybe-the fixnum (aref *piececount* (aref *class* i)))
			 1)))
    (do ((k j (maybe-the fixnum (1+ (maybe-the fixnum k)))))
	((> (maybe-the fixnum k) size)
	 ;;(terpri)
	 ;;(princ "Puzzle filled") 
	 0)
      (maybe-declare (type fixnum k))
      (cond ((not (aref *puzzle* k))
	     (return k))))))

(defun puzzle-remove (i j)
  (maybe-declare (type fixnum i j))
  #+Genera (declare (sys:optimize-array-references-in-loops))
  (let ((end (aref *piecemax* i)))
    (maybe-declare (type fixnum end))
    (do ((k 0 (maybe-the fixnum (1+ (maybe-the fixnum k)))))
	((> (maybe-the fixnum k) end))
      (maybe-declare (type fixnum k))
      (cond ((aref *p* i k) (setf (aref *puzzle* (maybe-the fixnum (+ j k))) false))))
    (setf (aref *piececount* (maybe-the fixnum (aref *class* i)))
	  (maybe-the fixnum (+ (maybe-the fixnum (aref *piececount* (aref *class* i)))
			 1)))))
  
(defun trial (j)
  (maybe-declare (type fixnum j))
  (let ((k 0))
    (maybe-declare (type fixnum k))
    (do ((i 0 (maybe-the fixnum (1+ (maybe-the fixnum i)))))
	((> (maybe-the fixnum i) typemax) (setq *kount* (maybe-the fixnum (1+ *kount*))) false)
      (maybe-declare (type fixnum i))
      (cond ((not (= (maybe-the fixnum (aref *piececount* (maybe-the fixnum (aref *class* i))))
		     0))
	     (cond ((fit i j)
		    (setq k (place i j))
		    (cond ((or (trial k)
			       (= k 0))
			   ;;(format t "~%Piece ~4D at ~4D." (+ i 1) (+ k 1))
			   (setq *kount* (maybe-the fixnum (+ *kount* 1)))
			   (return true))
			  (t (puzzle-remove i j))))))))))

(defun definepiece (iclass ii jj kk)
  (maybe-declare (type fixnum ii jj kk))
  (let ((index 0))
    (maybe-declare (type fixnum index))
    (do ((i 0 (1+ i)))
	((> i ii))
      (maybe-declare (type fixnum i))
      (do ((j 0 (1+ j)))
	  ((> j jj))
	(maybe-declare (type fixnum j))
	(do ((k 0 (1+ k)))
	    ((> k kk))
	  (maybe-declare (type fixnum k))
	  (setq index (+ i (maybe-the fixnum (* *d* (maybe-the fixnum (+ j (maybe-the fixnum (* *d* k))))))))
	  (setf (aref *p* *iii* index) true))))
    (setf (aref *class* *iii*) iclass)
    (setf (aref *piecemax* *iii*) index) 
    (cond ((not (= *iii* typemax))
	   (setq *iii* (maybe-the fixnum (+ *iii* 1)))))))

(defun start ()
  (do ((m 0 (maybe-the fixnum (1+ (maybe-the fixnum m)))))
      ((> (maybe-the fixnum m) size))
    (maybe-declare (type fixnum m))
    (setf (aref *puzzle* m) true))
  (do ((i 1 (maybe-the fixnum (1+ (maybe-the fixnum i)))))
      ((> (maybe-the fixnum i) 5))
    (maybe-declare (type fixnum i))
    (do ((j 1 (maybe-the fixnum (1+ (maybe-the fixnum j)))))
	((> (maybe-the fixnum j) 5))
      (maybe-declare (type fixnum j))
      (do ((k 1 (maybe-the fixnum (1+ (maybe-the fixnum k)))))
	  ((> (maybe-the fixnum k) 5))
	(maybe-declare (type fixnum k))
	(setf (aref *puzzle* (+ i (maybe-the fixnum (* *d* (maybe-the fixnum (+ j (maybe-the fixnum (* *d* k))))))))
	      false))))
  (do ((i 0 (maybe-the fixnum (1+ (maybe-the fixnum i)))))
      ((> (maybe-the fixnum i) typemax))
    (maybe-declare (type fixnum i))
    (do ((m 0 (maybe-the fixnum (1+ (maybe-the fixnum m)))))
	((> (maybe-the fixnum m) size))
      (maybe-declare (type fixnum m))
      (setf (aref *p* i m) false)))
  (setq *iii* 0)
  (definePiece 0 3 1 0)
  (definePiece 0 1 0 3)
  (definePiece 0 0 3 1)
  (definePiece 0 1 3 0)
  (definePiece 0 3 0 1)
  (definePiece 0 0 1 3)
  
  (definePiece 1 2 0 0)
  (definePiece 1 0 2 0)
  (definePiece 1 0 0 2)
  
  (definePiece 2 1 1 0)
  (definePiece 2 1 0 1)
  (definePiece 2 0 1 1)
  
  (definePiece 3 1 1 1)
  
  (setf (aref *piececount* 0) 13.)
  (setf (aref *piececount* 1) 3)
  (setf (aref *piececount* 2) 1)
  (setf (aref *piececount* 3) 1)
  (let ((m (maybe-the fixnum (+ 1 (maybe-the fixnum (* *d* (maybe-the fixnum (+ 1 *d*)))))))
	(n 0)(*kount* 0))
    (maybe-declare (type fixnum m n))
    (cond ((fit 0 m) (setq n (place 0 m)))
	  (t (format t "~%Error.")))
    (cond ((trial n) 
	   #|(format t "~%Success in ~4D trials." *kount*)|#)
	  (t (format t "~%Failure.")))))

(define-timer puzzle "Puzzle" (start))



;;; 3.16 TRIANG

(defvar *board* '#(1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1))
(defvar *sequence* (make-array 14. ':initial-element 0.))
(defvar *a* '#(1 2 4 3 5 6 1 3 6 2 5 4 11. 12. 13. 7 8. 4
		  4 7 11. 8. 12. 13. 6 10. 15. 9. 14. 13. 13. 14. 15. 9. 10. 6 6))
(defvar *b* '#(2 4 7 5 8. 9. 3 6 10. 5 9. 8. 12. 13. 14. 8. 9. 5
		  2 4 7 5 8. 9. 3 6 10. 5 9. 8. 12. 13. 14. 8. 9. 5 5))
(defvar *c* '#(4 7 11. 8. 12. 13. 6 10. 15. 9. 14. 13. 13. 14. 15. 9. 10. 6
		  1 2 4 3 5 6 1 3 6 2 5 4 11. 12. 13. 7 8. 4 4))

(maybe-proclaim '(type simple-vector *board* *sequence* *a* *b* *c*))

(defvar *answer*)
(defvar *final*)

(defun last-position ()
  (do ((i 1 (maybe-the fixnum (1+ (maybe-the fixnum i)))))
      ((= (maybe-the fixnum i) 16.) 0)
    (if (= 1 (maybe-the fixnum (aref *board* i)))
	(return (maybe-the fixnum i)))))

(defun try (i depth)
  (maybe-declare (type fixnum i depth))
  (cond ((= depth 14) 
	 (let ((lp (last-position)))
	   (unless (member lp *final*)
	     (push lp *final*)))
	 (push (cdr (coerce *sequence* 'list)) *answer*) t)
	((and (= 1 (maybe-the fixnum (aref *board* (maybe-the fixnum (aref *a* i)))))
	      (= 1 (maybe-the fixnum (aref *board* (maybe-the fixnum (aref *b* i)))))
	      (= 0 (maybe-the fixnum (aref *board* (maybe-the fixnum (aref *c* i))))))
	 (setf (aref *board* (maybe-the fixnum (aref *a* i))) 0)
	 (setf (aref *board* (maybe-the fixnum (aref *b* i))) 0)
	 (setf (aref *board* (maybe-the fixnum (aref *c* i))) 1)
	 (setf (aref *sequence* depth) i)
	 (do ((j 0 (maybe-the fixnum (1+ j)))
	      (depth (maybe-the fixnum (1+ depth))))
	     ((or (= j 36.)
		  (try j depth)) ())
	   (maybe-declare (type fixnum j)))
	 (setf (aref *board* (maybe-the fixnum (aref *a* i))) 1) 
	 (setf (aref *board* (maybe-the fixnum (aref *b* i))) 1)
	 (setf (aref *board* (maybe-the fixnum (aref *c* i))) 0)
	 ())))

(defun gogogo (i)
  (maybe-declare (type fixnum i))
  (dotimes (j 16)
    (setf (aref *board* j) 1))
  (setf (aref *board* 5) 0)
  (let ((*answer* ())
	(*final* ()))
    (try i 1)))

(define-timer triang "Triang" (gogogo 22.))

(defun triang-test ()
  (dotimes (j 16)
    (setf (aref *board* j) 1))
  (setf (aref *board* 5) 0)
  (let ((*answer* ())
	(*final* ()))
    (try 22. 1)
    (= (length *answer*) 775.)))



;;; 3.17 FPRINT
;;; Benchmark to print to a file.

(defparameter *fprint-test-file*
	      #+Genera "fep:>fprint.test"
	      #-Genera "fprint.test")

(defvar *test-atoms* '(abcdef12 cdefgh23 efghij34 ghijkl45 ijklmn56 klmnop67 
				mnopqr78 opqrst89 qrstuv90 stuvwx01 uvwxyz12 
				wxyzab23 xyzabc34 123456ab 234567bc 345678cd 
				456789de 567890ef 678901fg 789012gh 890123hi))

(defun fprint-init (m n atoms)
       (let ((atoms (copy-tree atoms)))
	    (do ((a atoms (cdr a)))
		((null (cdr a)) (rplacd a atoms)))
	    (fprint-init1 m n atoms)))

(defun fprint-init1 (m n atoms)
       (cond ((= m 0) (pop atoms))
	     (t (do ((i n (- i 2))
		     (a ()))
		    ((< i 1) a)
		    (push (pop atoms) a)
		    (push (fprint-init1 (1- m) n atoms) a)))))

(defvar *test-pattern* (fprint-init 6. 6. *test-atoms*))

(defun fprint ()
  (let ((f (open *fprint-test-file* :direction :output :if-exists :supersede)))
    (print *test-pattern* f)
    (close f)))

#-Minima
(define-timer fprint "FPrint" (fprint))



;;; 3.18 FREAD
;;; Benchmark to read from a file.

(defun fread ()
  (let ((f (open *fprint-test-file*)))
    (read f)
    (close f)))

#-Minima
(define-timer fread "FRead" (fread))



;;; 3.19 TPRINT
;;; Benchmark to print and read to the terminal

#-Minima
(defvar *test-atoms-tprint* '(abc1 cde2 efg3 ghi4 ijk5 klm6 mno7 opq8 qrs9
				   stu0 uvw1 wxy2 xyz3 123a 234b 345c 456d 
				   567d 678e 789f 890g))

#-Minima
(defun tprint-init (m n atoms)
       (let ((atoms (copy-tree atoms)))
	    (do ((a atoms (cdr a)))
		((null (cdr a)) (rplacd a atoms)))
	    (tprint-init1 m n atoms)))

#-Minima
(defun tprint-init1 (m n atoms)
       (cond ((= m 0) (pop atoms))
	     (t (do ((i n (- i 2))
		     (a ()))
		    ((< i 1) a)
		    (push (pop atoms) a)
		    (push (tprint-init1 (1- m) n atoms) a)))))

#-Minima
(defvar *test-pattern-tprint* (tprint-init 6. 6. *test-atoms-tprint*))

#-Minima
(define-timer tprint "TPrint" (print *test-pattern-tprint*))



;;; 3.20 FRPOLY
;;; Franz Lisp benchmark from Fateman
;; test from Berkeley based on polynomial arithmetic.

(defvar *v*)
;(defvar *X*) already declared above for STAK
(defvar *alpha*)
(defvar *l)
(defvar *p)
(defvar q*)
(defvar u*)
(defvar *var)
;(defvar *y*) already declared above for STAK
(defvar *R*)
(defvar *r2*)
(defvar *r3*)

;(declare (localf pcoefadd pcplus pcplus1 pplus ptimes ptimes1
;		 ptimes2 ptimes3 psimp pctimes pctimes1
;		 pplus1))

(defmacro pointergp (x y) `(> (get ,x 'order)(get ,y 'order)))
(defmacro pcoefp (e) `(atom ,e))

(defmacro pzerop (x) 
  (let ( (var (gensym)) )
    `(let ((,var ,x))
       (if (numberp ,var) (zerop ,var)))))	;true for 0 or 0.0
(defmacro pzero () 0)
(defmacro cplus (x y) `(+ ,x ,y))
(defmacro ctimes (x y) `(* ,x ,y))

(defun pcoefadd (e c x)
  (if (pzerop c)
      x
      (cons e (cons c x))))

(defun pcplus (c p)
  (if (pcoefp p)
      (cplus p c)
      (psimp (car p) (pcplus1 c (cdr p)))))

(defun pcplus1 (c x)
       (cond ((null x)
	      (cond ((pzerop c) nil) (t (cons 0 (cons c nil)))))
	     ((pzerop (car x)) (pcoefadd 0 (pplus c (cadr x)) nil))
	     (t (cons (car x) (cons (cadr x) (pcplus1 c (cddr x)))))))
	 
(defun pctimes (c p) (cond ((pcoefp p) (ctimes c p))
			   (t (psimp (car p) (pctimes1 c (cdr p))))))

(defun pctimes1 (c x)
       (cond ((null x) nil)
	     (t (pcoefadd (car x)
			  (ptimes c (cadr x))
			  (pctimes1 c (cddr x))))))

(defun pplus (x y) (cond ((pcoefp x) (pcplus x y))
			 ((pcoefp y) (pcplus y x))
			 ((eq (car x) (car y))
			  (psimp (car x) (pplus1 (cdr y) (cdr x))))
			 ((pointergp (car x) (car y))
			  (psimp (car x) (pcplus1 y (cdr x))))
			 (t (psimp (car y) (pcplus1 x (cdr y))))))

(defun pplus1 (x y)
       (cond ((null x) y)
	     ((null y) x)
	     ((= (car x) (car y))
	      (pcoefadd (car x)
			(pplus (cadr x) (cadr y))
			(pplus1 (cddr x) (cddr y))))
	     ((> (car x) (car y))
	      (cons (car x) (cons (cadr x) (pplus1 (cddr x) y))))
	     (t (cons (car y) (cons (cadr y) (pplus1 x (cddr y)))))))

(defun psimp (var x)
       (cond ((null x) 0)
	     ((atom x) x)
	     ((zerop (car x)) (cadr x))
	      (t (cons var x))))

(defun ptimes (x y) (cond ((or (pzerop x) (pzerop y)) (pzero))
			  ((pcoefp x) (pctimes x y))
			  ((pcoefp y) (pctimes y x))
			  ((eq (car x) (car y))
			   (psimp (car x) (ptimes1 (cdr x) (cdr y))))
			  ((pointergp (car x) (car y))
			   (psimp (car x) (pctimes1 y (cdr x))))
			  (t (psimp (car y) (pctimes1 x (cdr y))))))

(defun ptimes1 (*x* y) (prog (u* *v*)
			       (setq *v* (setq u* (ptimes2 y)))
			  a    (setq *x* (cddr *x*))
			       (cond ((null *x*) (return u*)))
			       (ptimes3 y)
			       (go a)))

(defun ptimes2 (y) (cond ((null y) nil)
			 (t (pcoefadd (+ (car *x*) (car y))
				      (ptimes (cadr *x*) (cadr y))
				      (ptimes2 (cddr y))))))

(defun ptimes3 (y) 
  (prog (e u c) 
     a1 (cond ((null y) (return nil)))
	(setq e (+ (car *x*) (car y)))
	(setq c (ptimes (cadr y) (cadr *x*) ))
	(cond ((pzerop c) (setq y (cddr y)) (go a1))
	      ((or (null *v*) (> e (car *v*)))
	       (setq u* (setq *v* (pplus1 u* (list e c))))
	       (setq y (cddr y)) (go a1))
	      ((= e (car *v*))
	       (setq c (pplus c (cadr *v*)))
	       (cond ((pzerop c) (setq u* (setq *v* (pdiffer1 u* (list (car *v*) (cadr *v*))))))
		     (t (rplaca (cdr *v*) c)))
	       (setq y (cddr y))
	       (go a1)))
     a  (cond ((and (cddr *v*) (> (caddr *v*) e)) (setq *v* (cddr *v*)) (go a)))
	(setq u (cdr *v*))
     b  (cond ((or (null (cdr u)) (< (cadr u) e))
	       (rplacd u (cons e (cons c (cdr u)))) (go e)))
	(cond ((pzerop (setq c (pplus (caddr u) c))) (rplacd u (cdddr u)) (go d))
	      (t (rplaca (cddr u) c)))
     e  (setq u (cddr u))
     d  (setq y (cddr y))
	(cond ((null y) (return nil)))
	(setq e (+ (car *x*) (car y)))
	(Setq c (ptimes (cadr y) (cadr *x*)))
     c  (cond ((and (cdr u) (> (cadr u) e)) (setq u (cddr u)) (go c)))
	(go b)))

;; pdiffer1 is referred to above but not defined.  RPG says it is never called.
(defun pdiffer1 (x y) x y (error "pdiffer2 called"))

(defun pexptsq (p n)
  (do ((n (floor n 2) (floor n 2))
       (s (cond ((oddp n) p) (t 1))))
      ((zerop n) s)
    (setq p (ptimes p p))
    (and (oddp n) (setq s (ptimes s p))) ))

(defun setup-frpoly nil
  (setf (get 'x 'order ) 1)
  (setf (get 'y 'order ) 2)
  (setf (get 'z 'order ) 3)
  (setq *r* (pplus '(x 1 1 0 1) (pplus '(y 1 1) '(z 1 1)))) ; r= x+y+z+1
  (setq *r2* (ptimes *r* 100000.)) ;r2 = 100000*r
  (setq *r3* (ptimes *r* 1.0)); r3 = r with floating point coefficients
  )

(setup-frpoly)

(define-timer frpoly2r "FRPoly, Power = 2, r = x + y + z + 1" (pexptsq *r* 2))
(define-timer frpoly2r2 "FRPoly, Power = 2, r2 = 1000r" (pexptsq *r2* 2))
(define-timer frpoly2r3 "FRPoly, Power = 2, r3 = r in flonums" (pexptsq *r3* 2))

(define-timer frpoly5r "FRPoly, Power = 5, r = x + y + z + 1" (pexptsq *r* 5))
(define-timer frpoly5r2 "FRPoly, Power = 5, r2 = 1000r" (pexptsq *r2* 5))
(define-timer frpoly5r3 "FRPoly, Power = 5, r3 = r in flonums" (pexptsq *r3* 5))

(define-timer frpoly10r "FRPoly, Power = 10, r = x + y + z + 1" (pexptsq *r* 10.))
(define-timer frpoly10r2 "FRPoly, Power = 10, r2 = 1000r" (pexptsq *r2* 10.))
(define-timer frpoly10r3 "FRPoly, Power = 10, r3 = r in flonums" (pexptsq *r3* 10.))

(define-timer frpoly15r "FRPoly, Power = 15, r = x + y + z + 1" (pexptsq *r* 15.))
(define-timer frpoly15r2 "FRPoly, Power = 15, r2 = 1000r" (pexptsq *r2* 15.))
(define-timer frpoly15r3 "FRPoly, Power = 15, r3 = r in flonums" (pexptsq *r3* 15.))
