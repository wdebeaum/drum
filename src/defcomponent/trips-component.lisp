;;;;
;;;; trips-component.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 30 Mar 2001
;;;; Time-stamp: <Fri Jan 15 14:11:55 EST 2010 ferguson>
;;;;

(in-package :defcomponent)

(defclass trips-component ()
  ((name :initform nil :initarg :name)
   (package :initform nil :initarg :package)
   (system :initform nil :initarg :system))
  (:documentation "Base class for TRIPS components."))

(defmethod print-object ((x trips-component) stream)
  "Pretty-prints a TRIPS-COMPONENT"
  (with-slots (name) x
    (format stream "#<~A ~S>" (class-name (class-of x)) name)))

(defvar *defined-components* nil
  "List of components defined by DEFCOMPONENT.")

(defun new-component (&rest args)
  "Apply MAKE-INSTANCE to ARGS, store the result on *DEFINED-COMPONENTS*,
and then return it."
  (let ((this (apply #'make-instance args)))
    (push this *defined-components*)
    this))

(defun defined-components ()
  "Return the a list of defined components."
  *defined-components*)

(defun find-component (name)
  "Return the component named NAME, or NIL if not found. If name is a
TRIPS-COMPONENT already, then it is returned, otherwise NAME is coerced
to a string."
  (if (typep name 'trips-component)
      name
    (find (string name) *defined-components*
	  :key #'(lambda (x) (slot-value x 'name))
	  :test #'string-equal)))

(defun find-component-or-die (name)
  "Call FIND-COMPONENT and signal an error if it returns NIL."
  (or (find-component name)
      (error "Can't find component ~S" name)))

(defvar *component* nil
  "The current component, set by IN-COMPONENT (and DEFCOMPONENT).")

(defun in-component (c)
  "Sets *COMPONENT* to the the given component (which can be a component
or the name of a component."
  (setq *component* (find-component-or-die c)))

;;;
;;; Methods for TRIPS-COMPONENTs
;;;

(defgeneric compile-component (m))

(defmethod compile-component ((m trips-component))
  "Default COMPILE-COMPONENT method: Calls TRIPS:COMPILE-SYSTEM on the
component's system."
  ;; Need to pass system name not system itself to mk:compile-system
  (with-slots (name) m
    (mk:compile-system name)))

(defmethod compile-component ((name t))
  "Looks up the component named NAME and calls COMPILE-COMPONENT on it.
Signals an error if the component is not found."
  (compile-component (find-component-or-die name)))

(defgeneric load-component (m))

(defmethod load-component ((m trips-component))
  "Default LOAD-COMPONENT method: Calls TRIPS:LOAD-SYSTEM on the component's
system."
  ;; Need to pass system name not system itself to mk:load-system
  (with-slots (name) m
    (mk:load-system name)))

(defmethod load-component ((name t))
  "Looks up the component named NAME and calls LOAD-COMPONENT on it.
Signals an error if the component is not found."
  (load-component (find-component-or-die name)))

