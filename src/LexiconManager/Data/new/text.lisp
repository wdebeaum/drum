;;;;
;;;; W::TEXT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::TEXT
   (SENSES
    ((LF-PARENT ONT::text-representation)
     (meta-data :origin calo :entry-date 20040406 :change-date 20050325 :wn ("text%1:10:00") :comments y1v5)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::text
   (SENSES
    ((LF-PARENT ONT::nonverbal-say)
     (meta-data :origin asma :entry-date 20110921 :change-date nil :comments nil)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     (EXAMPLE "he texted this morning")
     (PREFERENCE 0.98) ;; prefer transitive sense
     )
    ((LF-PARENT ONT::nonverbal-say)
     (meta-data :origin asma :entry-date 20110921 :change-date nil :comments nil)
     (example "text me a text")
     (TEMPL AGENT-FORMAL-AFFECTED-TEMPL)
     )
    ((LF-PARENT ONT::nonverbal-say)
     (meta-data :origin asma :entry-date 20110921 :change-date nil :comments nil)
     (example "text me about the problem")
     (TEMPL AGENT-AGENT1-FORMAL-XP-PP-ABOUT-TEMPL)
     )
    ((LF-PARENT ONT::nonverbal-say)
     (meta-data :origin asma :entry-date 20110921 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     (EXAMPLE "he texted that he couldn't come")
     )
    )
   )
))

