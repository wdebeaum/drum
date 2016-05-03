;;;;
;;;; W::chasten
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::chasten
   (wordfeats (W::morph (:forms (-vb) :past W::chastened :ing W::chastening)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("judgement-33") :wn ("chasten%2:32:00"))
     (LF-PARENT ONT::criticize)
     (TEMPL agent-addressee-templ) ; like thank
     )
    )
   )
))

