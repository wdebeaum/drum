;;;;
;;;; W::alert
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::alert
   (wordfeats (W::morph (:forms (-vb) :nom W::alert)))
   (SENSES
    #||((EXAMPLE "alert the people")    ;; associated-info is now compositional
     (LF-PARENT ONT::WARN)
     (TEMPL AGENT-ADDRESSEE-ASSOCIATED-INFORMATION-TEMPL (xp (% w::PP (w::ptype (? ptp w::about w::to)))))
     )||#
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("advise-37.9") :wn ("alert%2:32:00"))
     (LF-PARENT ONT::WARN)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL (xp (% w::cp (w::ctype w::s-finite)))) ; like notify
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    (W::alert
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080828 :change-date nil :comments nil)
     (lf-parent ont::alert-val)
     (example "he is alert")
     )
    )
   )
))

