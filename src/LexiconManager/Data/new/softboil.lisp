;;;;
;;;; W::softboil
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::softboil
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3"))
     (LF-PARENT ONT::cook-boil) ;boil) ; like bake,blanch,boil,braise,cook,fry
     )
    )
   )
))


(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::soft w::boil)
   (wordfeats (W::morph (:forms (-vb)
                                 :3s (W::soft w::boils)
                                 :past (W::soft w::boiled)
                                 :pastpart (W::soft w::boiled)
                                 :ing (W::soft w::boiling))))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3"))
     (LF-PARENT ONT::cook-boil) ;boil) ; like bake,blanch,boil,braise,cook,fry                                                                                                                  
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::soft w::punc-minus w::boil)
   (wordfeats (W::morph (:forms (-vb)
                                 :3s (W::soft w::punc-minus w::boils)
                                 :past (W::soft w::punc-minus w::boiled)
                                 :pastpart (W::soft w::punc-minus w::boiled)
                                 :ing (W::soft w::punc-minus w::boiling))))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3"))
     (LF-PARENT ONT::cook-boil) ;boil) ; like bake,blanch,boil,braise,cook,fry                                                                                                                  
     )
    )
   )
))
