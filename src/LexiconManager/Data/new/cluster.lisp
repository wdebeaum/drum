;;;;
;;;; W::cluster
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::cluster
   (wordfeats (W::morph (:forms (-vb) :past W::clustered :ing W::clustering)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("shake-22.3-2"))
     (LF-PARENT ONT::joining)
 ; like gather,collect
     )
    ((EXAMPLE "cluster the red one with those orange ones")
     (LF-PARENT ONT::joining)
     (TEMPL AGENT-affected2-TEMPL (xp (% W::PP (W::ptype W::with))))
     )

    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("herd-47.5.2") :wn ("cluster%2:35:00" "cluster%2:38:00"))
     (LF-PARENT ONT::meet)
     (TEMPL agent-plural-templ) ; like congregate,assemble,gather
     )
    )
   )
))

