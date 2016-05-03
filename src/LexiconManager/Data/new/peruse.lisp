;;;;
;;;; W::peruse
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::peruse
   (wordfeats (W::morph (:forms (-vb) :nom w::perusal)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("sight-30.2"))
     (LF-PARENT ONT::scrutiny)
     (TEMPL agent-neutral-xp-templ) ; like regard
     )
    )
   )
))

