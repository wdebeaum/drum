;;;;
;;;; W::CHOP
;;;;

(define-words :pos W::n
 :words (
  (W::CHOP
  (senses
	   ((LF-PARENT ONT::MEAT)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::chop (W::up))
   (wordfeats (W::morph (:forms (-vb) :ing W::chopping)))
   (SENSES
    ((LF-PARENT ONT::cut)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::extended))
     )
    )
   )
))

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::chop
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::cut)
   (example "chop the carrots")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

