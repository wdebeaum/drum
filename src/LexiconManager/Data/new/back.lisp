;;;;
;;;; w::back
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::BACK
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("back%1:06:00" "back%1:15:02"))
     (LF-PARENT ONT::object-dependent-location)
     (example "the back of the truck")
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :tags (:base500)
 :words (
;; physical systems, digestive, reproductive,. ...    
;; those are adjectives 
;; external
  (w::back
  (senses((LF-PARENT ONT::external-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((W::back w::breaking)
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090422 :change-date 20090821 :comments nil :wn ("difficult%3:00:00"))
     (EXAMPLE "It's tiresome [for him]")
     (lf-parent ont::difficult)
     (TEMPL adj-content-affected-optional-xp-templ)
     )
    ((meta-data :origin cardiac :entry-date 20090422 :change-date 20090821 :comments nil :wn ("difficult%3:00:00"))
     (EXAMPLE "it's tiresome to do")
     (lf-parent ont::difficult)
     (TEMPL adj-expletive-content-xp-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::BACK
   (SENSES
    ((LF-PARENT ONT::DIRECTION-backward)
     ;(TEMPL PRED-S-POST-TEMPL)
     (TEMPL PRED-S-VP-templ)
     (example "move back")
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::BACK W::AND W::FORTH)
   (SENSES
    ((LF-PARENT ONT::DIRECTION-wrt-entity)
     ;(TEMPL PRED-S-POST-TEMPL)
     (TEMPL PRED-S-VP-templ)
     )
    )
   )
))

(define-words :pos W::V
 :words (
  ((W::BACK W::AWAY)
   (SENSES
    ((LF-PARENT ONT::RENEGE)
     (TEMPL AGENT-neutral-XP-TEMPL (xp (% w::pp (w::ptype w::from))))
     )
    )
   )
))
