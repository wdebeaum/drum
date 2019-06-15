;;;;
;;;; W::BROWN
;;;;

(define-words :pos W::n
 :words (
  ((W::BROWN W::MUSHROOM)
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::brown
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3") :wn ("brown%2:30:00"))
     (LF-PARENT ONT::cook-result)
 ; like bake,blanch,boil,braise,cook,fry
     )
    )
   )
))

(define-words :pos w::adj :templ central-adj-templ
 :tags (:base500)
 :words (
   (W::brown
    (SENSES
    ((meta-data :origin joust :entry-date 20091027 :change-date 20091027 :comments nil :wn ("brown%3:00:01:chromatic:00"))
     (LF-PARENT ONT::brown)
     (SYNTAX (W::morph (:forms (-er))))
     (templ central-adj-templ)
     )
    )
   ) 
))

