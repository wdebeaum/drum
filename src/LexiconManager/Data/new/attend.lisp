;;;;
;;;; W::ATTEND
;;;;


(define-words :pos W::V :templ AGENT-neutral-XP-TEMPL
 :words (
  (W::attend
   (wordfeats (W::morph (:forms (-vb) :past w::attended :ing W::attending)))
   (SENSES
    ((meta-data :wn ("attend%2:42:00"))
     (LF-PARENT ONT::participate-attend)
     (EXAMPLE "I rarely attend services at my church" "He attended the university" "She attends class regularly")
     )
    )
   )
))
