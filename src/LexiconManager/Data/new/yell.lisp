;;;;
;;;; W::yell
;;;;

(define-words :pos W::v 
 :words (
   (W::yell
   (wordfeats (W::morph (:forms (-vb) :past W::yelled :ing w::yelling)))
   (SENSES
    ((LF-PARENT ONT::manner-say)
     (meta-data :origin step :entry-date 20080711 :change-date nil :comments nil)
     (example "He yelled that three teams are going to Delta")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-formal-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-to W::s-finite)))))
     )
    ((LF-PARENT ONT::NONVERBAL-EXPRESSION)
     (example "he yelled")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )
    ((LF-PARENT ONT::manner-say)
     (example "he yelled go over there")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-formal-XP-TEMPL (xp (% w::utt)))
     )
        )
   )
))

