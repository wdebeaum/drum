;;;;
;;;; W::create
;;;;

(define-words :pos W::v :templ agent-affected-create-templ
 :words (
  (W::create
     (wordfeats (W::morph (:forms (-vb) :nom w::creation)))
   (SENSES
    ((LF-PARENT ONT::CREATE)
     (example "create a plan")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((LF-PARENT ONT::CREATE)
     (example "an open switch creates a gap" "it created fanatics in Afganistan")
     ;; note that create-26.4 is approximate as VN does not allow for a cause
     (meta-data :origin beetle2-onr2 :entry-date 20071123 :change-date nil :comments nil :vn ("create-26.4"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-create-templ)
     )    
    )
   )
))

