;;;;
;;;; w::localize
;;;;

; note: similar file for "localise"

(define-words :pos W::v 
 :words (
  (w::localize
   (wordfeats (W::morph (:forms (-vb) :nom w::localization )))
   (senses

    ( 
     (LF-PARENT ONT::sit) ;be-at-loc)
     (TEMPL NEUTRAL-LOCATION-XP-TEMPL (xp (% W::pp (W::ptype (? xxx W::to w::on w::in w::into w::at)))))
     (example "the protein localizes in/to the nucleus")
     )

    ( 
     (LF-PARENT ONT::sit) ;be-at-loc)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::pp (W::ptype (? xxx W::with)))))
     (example "the protein localizes with the other protein")
     )

    ( 
     (LF-PARENT ONT::sit) ;be-at-loc)
     (TEMPL NEUTRAL-NEUTRAL1-LOCATION-2-XP1-3-XP-TEMPL (xp (% W::pp (W::ptype (? xxx W::to w::on w::in w::into w::at)))))
     (example "some property localizes the protein to the nucleus")
     )

    )
   )
))
