;;;;
;;;; W::ATTEND
;;;;


(define-words :pos W::V :templ AGENT-neutral-XP-TEMPL
 :words (
  (W::attend
   (wordfeats (W::morph (:forms (-vb) :past w::attended :ing W::attending)))
   (SENSES
    ((LF-PARENT ONT::participate-attend)
     (EXAMPLE "He attended the university" "She attends class regularly" "I rarely attend services at my church.")
     (meta-data :wn ("attend%2:42:00"))
    )
    )
   )
))
