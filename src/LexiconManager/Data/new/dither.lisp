;;;;
;;;; W::dither
;;;;

(define-words :pos W::v 
 :words (
  (W::dither
   (wordfeats (W::morph (:forms (-vb) :past W::dithered :ing W::dithering)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("linger-53.1") :wn ("dither%2:37:01"))
     (LF-PARENT ONT::state-of-worrying)
     ;(TEMPL agent-time-duration-templ) ; like pause
     (TEMPL experiencer-Theme-XP-TEMPL (xp (% W::PP (W::ptype (? p W::about)))))
     )

    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("linger-53.1") :wn ("dither%2:37:01"))
     (LF-PARENT ONT::state-of-worrying)
     ;(TEMPL agent-time-duration-templ) ; like pause
     (TEMPL experiencer-templ)
     )
    
    )
   )
))

