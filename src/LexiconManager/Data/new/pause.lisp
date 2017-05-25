;;;;
;;;; W::pause
;;;;

(define-words :pos W::v 
 :words (
 (W::pause
  (wordfeats (W::morph (:forms (-vb) :nom w::pause)))
   (SENSES
    
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus :vn ("linger-53.1"))
     (LF-PARENT ONT::PAUSE)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     ;(TEMPL AGENT-TIME-DURATION-TEMPL)
     (TEMPL AGENT-TEMPL)
     (example "I paused for five minutes")
     )

    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus :vn ("linger-53.1"))
     (LF-PARENT ONT::PAUSE)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     ;(TEMPL AGENT-TIME-DURATION-TEMPL)
     (TEMPL AGENT-AFFECTED-XP-TEMPL)
     (example "I paused the program.")
     )
    
    )
   )
))

