;;;;
;;;; W::injure
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::injure ; like hurt
    (wordfeats (W::morph (:forms (-vb) :past W::injured :ing W::injuring :nom w::injury)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date 20090512 :comments nil :vn ("hurt-40.8.3-2") :wn ("injure%2:29:00"))
     (LF-PARENT ONT::evoke-injury)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::injury
  (senses
	   ((meta-data :wn ("injury%1:26:00"))
            (LF-PARENT ONT::injury)
	    (TEMPL count-pred-TEMPL)
	    )
	   )
)
))


