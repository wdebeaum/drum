;;;;
;;;; W::explore
;;;;

(define-words :pos W::v 
 :words (
 (W::explore
     (wordfeats (W::morph (:forms (-vb) :nom w::exploration)))
   (SENSES
    ((meta-data :origin calo :entry-date 20040402 :change-date 20061005 :comments html-purchasing-corpus :vn ("investigate-35.4") :wn ("explore%2:31:02"))
     (LF-PARENT ONT::physical-scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "explore the region")
     )

    ((EXAMPLE "explore the options")
     (LF-PARENT ONT::scrutiny)
     (TEMPL agent-neutral-xp-templ)
     )

    )
   )
))

