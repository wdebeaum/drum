;;;;
;;;; W::tear
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::tear
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090130 :change-date nil :comments nil)
     (LF-PARENT ONT::bodily-fluid)
     (TEMPL count-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::tear
   (wordfeats (W::morph (:forms (-vb) :past W::tore :pastpart W::torn :ing W::tearing)))
   (SENSES
   
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("tear%2:38:00"))
     (LF-PARENT ONT::move-rapidly)
     (example "he tore through the streets")
     (TEMPL agent-templ) ; like stroll,walk
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::BREAK-OBJECT)
     (meta-data :origin task-learning :entry-date 20050829 :change-date nil :comments nil :vn ("break-45.1") :wn ("tear%2:35:00" "tear%2:35:01"))
     (example "the shirt tore")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AFFECTED-TEMPL)
     )
    #||((LF-PARENT ONT::BREAK-OBJECT)   ;; subsumed by cause role
     (meta-data :origin task-learning :entry-date 20050829 :change-date nil :comments nil :vn ("break-45.1") :wn ("tear%2:35:00" "tear%2:35:01"))
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he tore the paper")
     )||#
    ((LF-PARENT ont::break-object)
     (meta-data :origin task-learning :entry-date 20050829 :change-date nil :comments nil)
     (SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "it tore the paper")
     )
    )
   )
))

