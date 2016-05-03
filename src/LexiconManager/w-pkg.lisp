;;;;
;;;; File: LexiconManager/w-pkg.lisp
;;;; Creator: George Ferguson
;;;; Created: Thu Oct 29 16:07:32 2009
;;;; Time-stamp: <Thu Oct 29 16:07:52 EDT 2009 ferguson>
;;;;

;; This is a package for all words and lexcal features
;; Anyone who uses it will have to load this defsys
;; We share +, - with cl-user to make it simpler
;; We also export %, ? and $ to other packages
;; because these are crucial in correct lexicon handling
(defpackage :W
  (:use)
  (:import-from "COMMON-LISP-USER" "+" "-" "*" "**" "***")
  (:export "&" "%" "?" "$" "<NOT>" "<OR>" "<SIL>" )
  )
