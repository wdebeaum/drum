;;;;
;;;; W::undertake
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::undertake
   (wordfeats (W::morph (:forms (-vb) :past W::undertook :pastpart W::undertaken :ing W::undertaking)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("begin-55.1-1"))
     (LF-PARENT ONT::appropriate)
 ; like begin
     (templ agent-effect-xp-templ)
     )
    )
   )
))

