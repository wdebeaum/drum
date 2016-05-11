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
     (LF-PARENT ONT::be-at-loc)
     (TEMPL neutral-LOCATION-TEMPL (xp (% W::pp (W::ptype (? xxx W::to w::on w::in w::into w::at)))))
     (example "the protein localises in/to the nucleus")
     )

    ( 
     (LF-PARENT ONT::be-at-loc)
     (TEMPL neutral-neutral-xp-TEMPL (xp (% W::pp (W::ptype (? xxx W::with)))))
     (example "the protein localises with the other protein")
     )

    ( 
     (LF-PARENT ONT::be-at-loc)
     (TEMPL neutral-neutral-xp-location-templ (xp (% W::pp (W::ptype (? xxx W::to w::on w::in w::into w::at)))))
     (example "some property localises the protein to the nucleus")
     )

    )
   )
))
