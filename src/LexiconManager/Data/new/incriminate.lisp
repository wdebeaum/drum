;;;;
;;;; W::incriminate
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::incriminate
   (wordfeats (W::morph (:forms (-vb) :nom w::incrimination)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::indict)
     (TEMPL agent-addressee-templ) ; like thank
     )
    )
   )
))

