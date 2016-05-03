;;;;
;;;; W::treat
;;;;

(define-words :pos W::v 
 :words (
  (W::treat
    (wordfeats (W::morph (:forms (-vb) :nom w::treatment :nomobjpreps (w::for w::of))))
    (SENSES
     ((EXAMPLE "The doctor treated me/my arthritis; The doctor treated me for my arthritis")
      (LF-PARENT ONT::CONTROL-MANAGE)
      (TEMPL AGENT-AFFECTED-THEME-OPTIONAL-TEMPL (xp (% W::pp (W::ptype (? xxx W::for)))))
      )
    
     )
   )
))

