;;;;
;;;; W::BLOCK
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::BLOCK
     (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    ((LF-PARENT ONT::hindering)
     (meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (example "the intrusion detector blocked the hacker/signal" "he blocked the door")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     )
    ((meta-data :origin "gloss-training" :entry-date 20100217 :change-date nil :comments nil)
     (LF-PARENT ONT::hindering)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype w::from))))
     (example "It blocks him from doing something")
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::block
   (SENSES
    ((meta-data :origin fruitcarts :entry-date 20050225 :change-date nil :wn ("box%1:06:00") :comments nil)
     ;(LF-PARENT ONT::small-container)
     (lf-parent ont::block)
     (example "paint the block")
     )
   ))
))
