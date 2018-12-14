;;;;
;;;; W::goose-step
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  ((W::goose w::punc-minus w::step)
   (wordfeats (W::morph (:forms (-vb)
                                 :3s (W::goose w::punc-minus w::steps)
                                 :past (W::goose w::punc-minus w::stepped)
                                 :pastpart (W::goose w::punc-minus w::stepped)
                                 :ing (W::goose w::punc-minus w::stepping)
				 :nom (w::goose w::punc-minus w::step))))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("goose_step%2:38:00"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

