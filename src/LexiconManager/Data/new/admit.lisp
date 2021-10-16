;;;;
;;;; W::admit
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	 (W::admit
	  (wordfeats (W::morph (:forms (-vb) :nom W::admission)))
	  (SENSES
	   ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("confess-37.10"))
	    ;;(LF-PARENT ONT::announce)
	    (lf-parent ont::assert)
	    (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::cp (w::ctype (? c w::s-that))))) ; like acknowledge
	    )
	   ((lf-parent ont::admit) ; 20120524 GUM change new parent
	    (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
	    (example "he admitted him to the building")
	    (meta-data :origin cardiac :entry-date 20090422 :change-date nil :comments nil)
	    ;(templ agent-affected-goal-optional-templ (xp (% W::ADVBL (w::lf (% w::prop (w::class (?x ONT::SITUATED-IN ONT::GOAL-RELN)))))))
	    (templ AGENT-AFFECTED-XP-NP-TEMPL)
	    )
	   )
	  )))

