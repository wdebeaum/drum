;;;;
;;;; W::don
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::don
   (wordfeats (W::morph (:forms (-vb) :past W::donned :ing W::donning)))
   (SENSES
    ((meta-data :origin boudreaux :entry-date 20060414 :change-date nil :comments nil :vn ("simple_dressing-41.3.1") :wn ("don%2:29:00"))
     (EXAMPLE "I am starting to don the suit")
     (LF-PARENT ONT::put-on)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

