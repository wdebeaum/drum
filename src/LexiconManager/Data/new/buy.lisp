;;;;
;;;; W::buy
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::buy
   (wordfeats (W::morph (:forms (-vb) :past W::bought :ing W::buying)))
   (SENSES
    #|
    ((EXAMPLE "I need to buy a computer")
     (meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus :vn ("get-13.5.1") :wn ("buy%2:40:00" "buy%2:42:00"))
     (LF-PARENT ONT::purchase)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    |#
        
    ((lf-parent ont::purchase)
     (example "buy the book from amazon dot com")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::trajectory -))
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

