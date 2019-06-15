;;;;
;;;; W::download
;;;;

(define-words :pos W::v 
 :words (
  (W::download
   (SENSES
    ((EXAMPLE "download all of the images from my camera")
     (LF-PARENT ONT::acquire)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::trajectory -))
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? pt W::at W::from)))))
     )
    
    )
   )))

