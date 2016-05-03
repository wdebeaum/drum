;;;;
;;;; defcomp.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 30 Mar 2001
;;;; Time-stamp: <Wed Jan 20 11:51:00 EST 2010 ferguson>
;;;;

(in-package :defcomponent)

(defmacro defcomponent (name &key (class 'trips-agent)
				  (package name)
				  (nicknames nil)
				  (use '(:common-lisp))
				  (export nil)
				  (system))
  "Defines TRIPS component NAME. This does several things:

1. Defines a new package named PACKAGE (defaults to NAME).
   - This package will be defined to use any packages given as USE. The
     default is (:common-lisp), and the use of the DEFCOMPONENT package is
     added automatically regardless of the value of this argument. The
     created package will export those symbols given on EXPORT.

2. Defines a new mk:defsystem named NAME from the specification SYSTEM.
   - If this specification does not contain a :source-pathname specifier,
     one will be provided based on the location of the file containing the
     DEFCOMPONENT form.

3. Defines an instance of CLASS (which should be a subclass of TRIPS-COMPONENT)
   named NAME, with the new system and package as attributes.

4. Defines several functions in the newly-created package for use
   by the component. These typically encapsulate a call to a component
   method with the new component, so that user code doesn't have to
   keep track of the components.

5. Call IN-PACKAGE on the new package and IN-COMPONENT on the new component,
   to simplify code following the DEFCOMPONENT form.

Use DEFCOMPONENT-METHOD to augment or override the standard component
methods (COMPILE-COMPONENT, LOAD-COMPONENT, etc.).

Use DEFCOMPONENT-HANDLER to add message handlers for the component."
  ;; Create package, system, and component
  (let* (#+allegro (*print-readably* nil)
	 (package
          (eval `(defpackage ,package (:nicknames ,@nicknames) (:use :defcomponent ,@use) (:export ,@export))))
	 (foo1 (format *trace-output* "~&;; defined package ~S~%" package))
	 (system (eval `(warp-defsystem-spec ',system)))
         (system
	  (eval `(mk:defsystem ,name ,@system)))
	 (foo2 (format *trace-output* "~&;; defined system ~S~%" system))
         (component
	  (eval `(new-component ',class
		   :name (string ,name) :package ,package :system ,system)))
	 (foo3 (format *trace-output* "~&;; defined component ~S~%" component)))
    (declare (ignore foo1 foo2 foo3))
    ;; Return from defcomponent:
    `(progn
       ;; Do in-package to simplify following code
       (in-package ,(package-name package))
       ;; Do in-component for the same reason
       (in-component ,name)
       ;; Return new component from DEFCOMPONENT
       ',component)))

(defun warp-defsystem-spec (spec)
  "Adds a :source-pathname item to the mk:defsystem SPEC if one is not
present already."
  (if (find :source-pathname spec)
      spec
    (let* ((this-directory (directory-namestring *load-truename*)))
      (setq spec (append spec (list :source-pathname this-directory))))))

;;;
;;; To allow the user to override/augment a component method:
;;;
(defmacro defcomponent-method (method-name qualifier args &body body)
  "Allows a user to override or augment the standard methods for TRIPS
components. This macro defines a method named METHOD-NAME (which should be
one of the standard component methods) for the value of *COMPONENT* that takes
arguments ARGS. QUALIFIER is a CLOS method combination qualifier (e.g.,
:before, :after, or :around, or NIL if no qualifier is wanted). The rest of
the form is the body of the method. Use CALL-NEXT-METHOD to invoke the
standard component method from an :around method."
  (format *trace-output* ";; defining ~S method ~S for component ~S~%" qualifier method-name *component*)
  `(defmethod ,method-name ,qualifier ((m (eql ,*component*)) ,@args)
     ,@body))

