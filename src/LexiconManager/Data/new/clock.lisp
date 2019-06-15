;;;;
;;;; W::clock
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::clock
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060123 :change-date nil :wn ("clock%1:06:00") :comment caloy3)
     (LF-PARENT ONT::DEVICE)
     (example "the clock on the wall")
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::CLOCK W::SPEED)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::clock W::speeds))))
   (SENSES
    ((LF-PARENT ONT::CLOCK-SPEED-scale)
     (TEMPL reln-subcat-of-units-TEMPL)
     )
    ((LF-PARENT ONT::CLOCK-SPEED-scale)
     (TEMPL other-reln-TEMPL)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::clock
   (SENSES
    ((LF-PARENT ONT::register)
     (TEMPL agent-neutral-xp-templ)
     (example "He clocked the runners")
     (meta-data :origin trips :entry-date 20090910 :change-date nil :comments nil :vn ("register-54.1"))
     )
    ((LF-PARENT ONT::register)
     (TEMPL AGENT-NEUTRAL-FORMAL-2-XP1-3-XP2-TEMPL)
     (example "He clocked the runners at different speeds")
     (meta-data :origin trips :entry-date 20090910 :change-date nil :comments nil :vn ("register-54.1"))
     )
    ((LF-PARENT ONT::register)
     (TEMPL neutral-extent-xp-templ (xp (% W::PP (W::ptype W::at))))
     (example "He clocked at 85km/hr")
     (meta-data :origin trips :entry-date 20090910 :change-date nil :comments nil :vn ("register-54.1"))
     )
    )
   )
))

