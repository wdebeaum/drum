;;;;
;;;; W::leave
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
(W::leave
   (SENSES
    (  (meta-data :origin calo :entry-date 20031211 :comments calo-y1script)
     (LF-PARENT ONT::vacation)
     (example "he is on leave this month")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::leave
   (wordfeats (W::morph (:forms (-vb) :past W::left)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("future_having-13.3") :wn ("leave%2:31:05" "leave%2:40:01" "leave%2:40:02"))
     (LF-PARENT ONT::future-giving)
     (example "leave him the truck")
      (TEMPL AGENT-RECIPIENT-affected-TEMPL )
     )
    ;;;; swier -- the truck will leave from Boston for Rochester
    ((LF-PARENT ONT::DEPART)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL GO-FROM-TO-TEMPL (xp1 (% W::PP (W::ptype W::from))) (xp2 (% W::PP (W::ptype W::for))))
     (preference .98)
     )
    ((LF-PARENT ONT::DEPART)
     (meta-data :origin asma :entry-date 20111004)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "he left the house")   
     )
    ((LF-PARENT ONT::DEPART)
     (meta-data :origin asma :entry-date 20111004)
     (templ agent-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "he left"))
     
;    ((LF-PARENT ONT::DEPART)
;     (example "the truck left atlanta for rochester")
;     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
;     (TEMPL GO-FROM-TO-TEMPL (xp2 (% W::PP (W::ptype W::for))))
;     )
    ((LF-PARENT ONT::LEAVE-behind)
     (example "he left his dog at the house")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ agent-affected-xp-templ)
     )    
    ((LF-PARENT ONT::LEAVE-TIME)
     (SEM (F::Aspect F::stage-level))
     (example "The solution leaves us 5 hours")
     (TEMPL neutral-AFFECTED-DURATION-TEMPL)
     )
    ((LF-PARENT ONT::LEAVE-TIME)
     (SEM (F::Aspect F::stage-level))
     (example "it leaves us 5 hours to complete the plan")
     (TEMPL AFFECTED-DURATION-EXPLETIVE-TEMPL)
     )
    ))
))

(define-words :pos W::v 
 :words (
  ((W::leave (w::out))
     (wordfeats (W::morph (:forms (-vb) :past W::left)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060710 :change-date nil :comments nil)
     (LF-PARENT ONT::omit)
     (example "leave this paragraph out")
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

