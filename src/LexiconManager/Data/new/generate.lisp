;;;;
;;;; W::generate
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTEDR-XP-TEMPL
 :words (
  (W::generate
     (wordfeats (W::morph (:forms (-vb) :nom w::generation)))
   (SENSES
    ((LF-PARENT ONT::CREATE)
     (example "generate an android")
     (meta-data :origin calo :entry-date 20050418 :change-date nil :comments projector-purchasing)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

