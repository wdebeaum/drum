;;;;
;;;; W::isolate
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 (W::isolate
   (wordfeats (W::morph (:forms (-vb) :nom w::isolation)))
   (SENSES
    ((meta-data :origin plow :entry-date 20060615 :change-date nil :comments pq)
     (LF-PARENT ont::separation)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::from)))))
     (EXAMPLE "isolate several dnas from a blood cell library")
     )
    ((meta-data :origin beetle :entry-date 20080716 :change-date nil :comments nil :vn ("separate-23.1-2"))
     (LF-PARENT ont::separation)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? t W::from)))))
     (EXAMPLE "a gap isolates one side from the other")
     )
    )
   )
))

