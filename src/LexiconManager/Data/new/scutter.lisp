;;;;
;;;; W::scutter
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::scutter
   (wordfeats (W::morph (:forms (-vb) :past W::scuttered :ing W::scuttering)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

