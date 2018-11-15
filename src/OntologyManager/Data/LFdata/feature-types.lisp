(in-package :om)

(define-feature-list-type F::sem
    :features (f::type) ;;(type)
    :defaults () ;;((type ANY-type) )
    )

(define-feature-list-type F::Phys-obj
    :features (F::object-function F::origin F::form F::mobility F::group F::spatial-abstraction F::intentional F::information F::container F::kr-type f::type F::trajectory F::tangible)
    :defaults (
	       (F::type ont::phys-object)
	       ;;(F::object-function F::ANY-object-function)
	       (F::origin F::any-origin) (F::Form F::ANY-form)
	       (F::mobility F::ANY-Mobility) (F::group -)
	       (F::Spatial-abstraction (? sab F::spatial-point F::spatial-region))
	       (F::intentional -) (F::trajectory -)
	       (F::information -) (F::container -)
	       (F::tangible +)
	       ))
  
(define-feature-list-type F::Situation
    :features (F::aspect F::time-span F::cause F::trajectory F::locative F::intentional F::information F::container F::kr-type F::type f::origin f::iobj)
    :defaults (
	       (F::type ont::situation) 	       
	       (F::intentional -)
	       (F::information F::mental-construct) (F::container -)
	       (F::Aspect F::ANY-aspect) (F::Cause F::ANY-Cause)
	       (F::Time-span F::ANY-Time-span) (F::Trajectory -)
	       (F::Locative -)
	       (f::origin f::any-origin)
	       (f::iobj -)
	       )) 

;; swier added gradability feature
;; swift added orientation and intensity
(define-feature-list-type F::Abstr-obj
    :features (F::measure-function F::scale F::intentional F::information F::container F::gradability F::kr-type f::type f::object-function f::origin f::intensity f::orientation F::tangible)
    :defaults (
	       (F::type ont::abstract-object)
	       (F::gradability -)
	       (F::orientation -)
	       (F::intensity -)
	       (F::intentional -)
	       (F::information -) (F::container -)
	       (F::Measure-function -) (F::scale -)
	       ;;(F::object-function -)
	       (f::origin f::any-origin)
	       (F::tangible -)
	       ))

(define-feature-list-type F::Proposition
    :features (F::intentional F::information F::container F::gradability F::kr-type f::type f::origin)
    :defaults (
	       (F::type ont::any-sem)
	       (F::gradability -)
	       (F::intentional -)
	       (F::information F::information-content) (F::container -)
	       (f::origin f::any-origin)
	       ))
 

(define-feature-list-type F::time
  :features (F::time-function f::scale ;F::time-scale
			      F::kr-type f::type)
    :defaults (
	       (F::type ont::any-time-object)
	       (F::time-function F::Any-time-function)
	       ;(F::time-scale F::point)
	       (F::SCALE ONT::TIME-LOC-SCALE) ;(f::scale -)
	       ;;ont::time-measure-scale) ;; a default -- we need f::scale to allow role restriction sharing between durations such as abstract quantities w/ times, like five minutes, which are (f::scale f::duration), and ont::time-intervals
	       )
    )
