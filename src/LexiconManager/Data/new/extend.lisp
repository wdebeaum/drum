;;;;
;;;; w::extend
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(w::extend
 (senses
  ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("future_having-13.3") :wn ("extend%2:40:05"))
   (LF-PARENT ONT::giving)
   (example "extend him an offer")
   (TEMPL agent-affected-recipient-alternation-templ) ; like grant,offer
     (PREFERENCE 0.96)
   )
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments html-processing-corpus)
   (LF-PARENT ONT::increase)
   (example "extend the deadline [to midnight]")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   )
  ((example "the changes extended the process")
   (sem (f::aspect f::dynamic))
   (templ agent-affected-xp-templ)
   (meta-data :origin step :entry-date 20080626 :change-date 20090504 :comments nil)
   (LF-PARENT ONT::increase)
   )
  ((example "the line extends here")
   (templ neutral-templ)
   (meta-data :origin gloss :entry-date 20110330 :change-date nil :comments nil)
   (LF-PARENT ONT::span)
   )
  )
 )
))

