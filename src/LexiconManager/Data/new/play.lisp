;;;;
;;;; W::play
;;;;

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::play
   (SENSES
    ((EXAMPLE "play the voice note")
     ; changed from ont::execute to ont::play for asma
     (meta-data :origin boudreaux :entry-date 20060424 :change-date 20111004 :comments asma)
     (LF-PARENT ONT::play)
     (templ agent-affected-xp-templ)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    ((EXAMPLE "they are playing")
     (meta-data :origin boudreaux :entry-date 20060424 :change-date 20111004 :comments asma)
     (LF-PARENT ONT::play)
     (templ agent-templ)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     
    )
    )
   )))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::play
   (senses
    ((LF-PARENT ONT::performance-play)
     (example "I saw the play at the theatre tonight")
     (meta-data :origin calo-ontology :entry-date 20060615 :change-date nil :wn ("concert%1:10:00") :comments nil)
     )
    )
   )
))

