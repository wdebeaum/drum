;;;;
;;;; W::backbite
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::backbite
   (wordfeats (W::morph (:forms (-vb) :past W::backbit :pastpart W::backbitten :ing W::backbiting)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("backbite%2:32:00"))
     (LF-PARENT ont::insult)
     (TEMPL agent-addressee-templ) ; like thank
     )
    )
   )
))

