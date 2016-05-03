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
     (TEMPL agent-affected-SOURCE-optional-templ (xp (% W::PP (W::ptype (? pt W::at W::from)))))
     )
    
    )
   )))

