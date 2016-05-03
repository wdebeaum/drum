;;;;
;;;; W::forgive
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::forgive
   (wordfeats (W::morph (:forms (-vb) :past W::forgave :pastpart W::forgiven :ing W::forgiving)))
   (SENSES
     ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("forgive%2:32:00"))
     (LF-PARENT ONT::forgive)
     (TEMPL agent-addressee-templ) ; like thank
     )
    )
   )
))

