;;;;
;;;; W::hang
;;;;

(define-words :pos W::v 
 :words (
  ((W::hang (W::out))
   (wordfeats (W::morph (:forms (-vb) :past W::hung :pastpart W::hung)))   
   (SENSES
    ((LF-PARENT ONT::WAIT)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::hang
   (wordfeats (W::morph (:forms (-vb) :past W::hung :pastpart W::hung)))   
   (SENSES
    ((EXAMPLE "he was hanging from the tree")
     (LF-PARENT ONT::BE-AT-LOC)
     (SEM (F::Aspect F::Stage-level) (F::Time-span F::Extended))
     (TEMPL neutral-templ)
     )
    )
   )
))

