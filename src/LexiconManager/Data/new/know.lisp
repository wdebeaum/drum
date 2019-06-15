;;;;
;;;; W::know
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::know
   (wordfeats (W::morph (:forms (-vb) :past W::knew :pastpart W::known :nom w::knowledge)))
   (SENSES
    ;;;; I know (that) he...
    ((LF-PARENT ONT::KNOW)
      (meta-data :origin trips :entry-date unknown :change-date nil :comments nil :wn ( "know%2:31:02"))
     (TEMPL experiencer-theme-xp-templ (xp (% W::cp (w::ctype w::s-that) (w::wh w::-))))
     )
     ((LF-PARENT ONT::know)
      (meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("consider-29.9-2") :wn ("believe%2:31:04:"))
      (example "they know her to have cancer")
      (SEM (F::Time-span F::extended))
      (TEMPL EXPERIENCER-NEUTRAL-FORMAL-CP-OBJCONTROL-B-TEMPL)
      )
    ((LF-PARENT ONT::KNOWIF)
     (example "I know whether he left")
     (meta-data :origin csli-ts :entry-date 20070323 :change-date nil :comments nil :wn ("know%2:31:01"))
     (TEMPL experiencer-theme-xp-templ (xp (% W::cp (w::ctype w::s-if) 
					   (w::wh w::-))))
     )
    ((LF-PARENT ONT::KNOWIF)
     (example "I know whether to leave")
      (meta-data :origin csli-ts :entry-date 20070323 :change-date nil :comments nil :wn ("know%2:31:01"))
      (TEMPL EXPERIENCER-FORMAL-SUBJCONTROL-TEMPL (xp (% W::cp (w::ctype w::s-to) (w::condition (:* ont::pos-condition ?x))
					   (w::wh w::-))))
     )
;;;; ::::::: 12/5/07  I'm reinstating this rule to get "I know what city it is in "  to work
;;;; :::::::          and I tightened the restrictions on the rule after to eliminate the WH complements
;;;; :::::::          The problem was that the WH feature was propagating up and the sentence came out as a question
;;;; :::::::          and the WH-TERM was not built.
    ;;;; I know how to.../ I know where he is
   (;(LF-PARENT ONT::FAMILIAR)
    (LF-PARENT ONT::know)
    (TEMPL experiencer-theme-xp-templ (xp (% W::NP (W::sort W::wh-desc))))
    )
   
    ((LF-PARENT ONT::FAMILIAR)
     ;(SEM (F::Aspect F::Stage-level))
     (example "I know him ")
     (TEMPL experiencer-neutral-TEMPL )
     )

    ((LF-PARENT ONT::know)
     (example "I know the answer ") 
     (TEMPL experiencer-neutral-TEMPL )
     )
    ((LF-PARENT ONT::KNOW)
     (example "I know")
     (TEMPL experiencer-templ)
     (preference .96)
     )

    ((LF-PARENT ONT::know)
     (example "I know about the party.") 
     (TEMPL experiencer-neutral-xp-TEMPL (xp (% W::PP (W::ptype (? p W::of W::about)))))
     )
    
    )
   )
))


