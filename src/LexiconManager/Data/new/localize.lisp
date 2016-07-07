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
     (LF-PARENT ONT::be-at-loc)
     (TEMPL neutral-TEMPL (xp (% W::pp (W::ptype (? xxx W::to w::on w::in w::into w::at)))))
     (example "the protein localizes in/to the nucleus")
     )

    ( 
     (LF-PARENT ONT::be-at-loc)
     (TEMPL neutral-neutral-xp-TEMPL (xp (% W::pp (W::ptype (? xxx W::with)))))
     (example "the protein localizes with the other protein")
     )

    ( 
     (LF-PARENT ONT::be-at-loc)
     (TEMPL neutral-neutral-xp-location-templ (xp (% W::pp (W::ptype (? xxx W::to w::on w::in w::into w::at)))))
     (example "some property localizes the protein to the nucleus")
     )

    )
   )
))
