;;;;
;;;; parser.lisp : Load the TRAINS/TRIPS parser
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 13 Dec 1995
;;;; Time-stamp: <Thu Jun 12 15:20:11 EDT 2003 ferguson>
;;;;
 
(unless (boundp 'user::*TRIPS_BASE*)
  (load (make-pathname :directory '(:relative :up "config" "code")
		       :name "trips")))

(setq user::*default-scenario-directory*
  (directory-namestring
   (translate-logical-pathname "TRIPS:Scenario;trains;")))

(load "TRIPS:Parser;defsys")

(setq parser::*default-KR-in-lexicon* nil)
(setq parser::*default-KR-boost* 1)
(setq parser::*use-sem* nil)

(PARSER::load-system)

(load "trips-parser/extrainterface")
