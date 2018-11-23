;;;;
;;;; W::eat
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::eat
   (wordfeats (W::morph (:forms (-vb) :past W::ate :pastpart W::eaten :ing W::eating)))
   (SENSES
    ((EXAMPLE "Eat a meal" "Eat calcium for your bones")
     (LF-PARENT ONT::EAT)
     (TEMPL AGENT-AFFECTED-XP-TEMPL)
     )
    ((EXAMPLE "Eat!")
     (LF-PARENT ONT::EAT)
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

