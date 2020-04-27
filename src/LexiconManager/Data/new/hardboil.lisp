;;;;
;;;; W::hardboil
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::hardboil
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3"))
     (LF-PARENT ONT::cook-boil) ;boil)
 ; like bake,blanch,boil,braise,cook,fry
     )
    )
   )
))


(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::hard w::boil)
   (wordfeats (W::morph (:forms (-vb)
                                 :3s (W::hard w::boils)
                                 :past (W::hard w::boiled)
                                 :pastpart (W::hard w::boiled)
                                 :ing (W::hard w::boiling))))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3"))
     (LF-PARENT ONT::cook-boil) ;boil)
 ; like bake,blanch,boil,braise,cook,fry                                                                                                                                            
     )
    )
   )
))


(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::hard w::punc-minus w::boil)
   (wordfeats (W::morph (:forms (-vb)
                                 :3s (W::hard w::punc-minus w::boils)
                                 :past (W::hard w::punc-minus w::boiled)
                                 :pastpart (W::hard w::punc-minus w::boiled)
                                 :ing (W::hard w::punc-minus w::boiling))))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("cooking-45.3"))
     (LF-PARENT ONT::cook-boil) ;boil)
 ; like bake,blanch,boil,braise,cook,fry                                                                                                                                            
     )
    )
   )
))
