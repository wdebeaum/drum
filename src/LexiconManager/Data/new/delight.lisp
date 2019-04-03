;;;;
;;;; W::delight
;;;;

(define-words :pos W::V 
 :words (
  (W::delight
   (SENSES
    ((example "these colors delight the senses")
     (meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("delight%2:37:00" "delight%2:37:02"))
     (LF-PARENT ONT::evoke-joy)
     (TEMPL agent-affected-xp-templ)
     )
;    ((LF-PARENT ONT::evoke-joy)
;     (TEMPL affected-agent-as-comp-templ (xp (% W::pp (W::ptype W::in)))))

    ((example "he delights in his granddaughter")
     (LF-PARENT ONT::appreciate)
     (TEMPL experiencer-neutral-xp-templ  (xp (% W::pp (W::ptype W::in))))
    )

    )
   )
))

