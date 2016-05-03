;;;;
;;;; W::exclude
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(W::exclude
   (wordfeats (W::morph (:forms (-vb) :nom w::exclusion)))
 (SENSES
  ((EXAMPLE "the battery is in a path which excludes the bulb" "the offer excludes previous purchases")
   (meta-data :origin beetle :entry-date 20092002 :change-date nil :comments beetle-pilots)	    
   (LF-PARENT ONT::EXCLUDE)
   (SEM (F::Aspect F::static) (F::Time-span F::extended))
   
   )
  )
 )
))

