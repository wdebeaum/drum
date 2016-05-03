;;;;
;;;; W::decline
;;;;


(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  (W::decline
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090407 :change-date nil :comments weather :wn ("decrease%1:04:00"))
     (LF-PARENT ONT::adjust)
     (example "he experienced a decline")
     (templ other-reln-affected-templ  (xp (% W::pp (W::ptype (? pt w::in)))))
     )
    )
   )
))


(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::decline
   (wordfeats (W::morph (:forms (-vb) :nom w::decline :nomobjpreps (w::of w::in))))
   (SENSES
    ((lf-parent ONT::decrease)
     (templ affected-templ)
     (example "the market declined")
     (meta-data :origin "wordnet-3.0" :entry-date 20090504 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::decline
   (SENSES
    ((LF-PARENT ONT::reject)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (EXAMPLE "decline the cookie")
     (meta-data :origin task-learning :entry-date 20050825 :change-date 20090508 :comments nil)
     )
    )
   )
))
