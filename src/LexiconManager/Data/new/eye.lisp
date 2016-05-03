;;;;
;;;; W::EYE
;;;;

(define-words :pos W::n
 :tags (:base500)
 :words (
;; physical systems, digestive, reproductive,. ...
;; those are adjectives
;; external
  (W::EYE
  (senses((LF-PARENT ONT::external-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::n
 :words (
  ((W::EYE W::OF W::ROUND)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
  (W::eye
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("sight-30.2") :wn ("eye%2:39:00"))
     (LF-PARENT ONT::active-perception)
     (TEMPL agent-neutral-templ) ; like observe,view,watch
     )
    )
   )
))

