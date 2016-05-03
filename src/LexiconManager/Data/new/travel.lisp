;;;;
;;;; W::travel
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::travel
   (SENSES
    ((meta-data :origin calo :entry-date 20050216 :change-date nil :wn ("travel%1:04:00") :comments caloy2)
     (example "the projector needs to be light because I want it for travel")
     (LF-PARENT ont::travel)
     (TEMPL BARE-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :tags (:base500)
 :words (
  (W::travel
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("travel%2:38:00" "travel%2:38:02"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     (PREFERENCE 0.96)
     )
    ((meta-data :origin general :entry-date 20110127 :change-date nil :comments jansen :vn ("run-51.3.2") :wn ("travel%2:38:00" "travel%2:38:02"))
     (LF-PARENT ONT::self-locomote)
     (example "he travelled the route")
     (TEMPL agent-neutral-templ) ; like stroll,walk
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::MOVE)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::Extended))
     (TEMPL AFFECTED-TEMPL)
     (example "the truck travelled to Avon")
     (PREFERENCE 0.95) ;; prefer agent interpretation
     )
    )
   )
))

