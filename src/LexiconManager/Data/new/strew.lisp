;;;;
;;;; W::strew
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::strew
     (wordfeats (W::morph (:forms (-vb) :pastpart W::strewn)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::filling)
 ; like spray
     )
    )
   )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  ;; alternate past
  (W::strew
   (wordfeats (W::morph (:forms NIL)) (W::vform W::past))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1"))
     (LF-PARENT ONT::filling)
 ; like spray
     )
    )
   )
))

