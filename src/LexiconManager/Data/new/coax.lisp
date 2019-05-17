;;;;
;;;; W::coax
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::coax
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL agent-affected-theme-objcontrol-optional-templ)  ; like dare
     (example "He coaxed him [to run for office]")   
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("tease%2:32:00" "tease%2:37:00"))
     (LF-PARENT ont::provoke)
     (example "he coaxed a promise [from them]")
     (templ agent-affected-xp-templ)
     ;(TEMPL agent-effect-affected-optional-templ (xp (% w::pp (w::ptype (? pt w::from))))) ; like annoy,bother,concern,hurt   
      )
    )
   )
))

#|
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
(w::coax
    (SENSES
     ((LF-PARENT ONT::Physical-discrete-domain)
      (meta-data :origin calo :entry-date 20050418 :change-date nil :comments projector-purchasing)
      )
     )
    )
))
|#

