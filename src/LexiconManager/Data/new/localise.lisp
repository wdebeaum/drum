;;;;
;;;; w::localise
;;;;

; note: similar file for "localize"

(define-words :pos W::v 
 :words (
  (w::localise
   (wordfeats (W::morph (:forms (-vb) :nom w::localisation )))
   (senses

    ( 
     (LF-PARENT ONT::sit) ;be-at-loc)
     (TEMPL NEUTRAL-LOCATION-XP-TEMPL (xp (% W::pp (W::ptype (? xxx W::to w::on w::in w::into w::at)))))
     (example "the protein localises in/to the nucleus")
     )

    ( 
     (LF-PARENT ONT::sit) ;be-at-loc)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::pp (W::ptype (? xxx W::with)))))
     (example "the protein localises with the other protein")
     )

    ( 
     (LF-PARENT ONT::sit) ;be-at-loc)
     (TEMPL NEUTRAL-NEUTRAL1-LOCATION-2-XP1-3-XP-TEMPL (xp (% W::pp (W::ptype (? xxx W::to w::on w::in w::into w::at)))))
     (example "some property localises the protein to the nucleus")
     )

    )
   )
))
