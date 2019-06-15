;;;;
;;;; W::CREDIT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    ((W::CREDIT w::CARD)
     (SENSES
    ((meta-data :origin calo :entry-date 20040205 :change-date nil :wn ("credit_card%1:21:00") :comments y1v5)
     (LF-PARENT ONT::credit-card)
     (templ other-reln-templ)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::credit
   (wordfeats (W::morph (:forms (-vb) :past W::credited :ing W::crediting)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090513 :comments nil :vn ("fulfilling-13.4.1-1"))
     (LF-PARENT ONT::giving)
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-WITH-1-OPTIONAL-TEMPL)
     (example "We credit your account with $100")
     )
    ((meta-data :origin "wordnxset-3.0" :entry-date 20090513 :change-date nil :comments nil)
     (LF-PARENT ONT::judgement)
     ;;(TEMPL agent-affected-theme-optional-templ  (xp (% W::PP (W::ptype (? pt w::with W::for)))))
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype (? pt w::for w::with)))))
     (example "We credited her for saving our jobs")
     )
    )
   )
))

