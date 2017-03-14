;;;;
;;;; W::HOME
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::HOME
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::relative-location)
     (TEMPL BARE-PRED-TEMPL)
     (example "he is at home" "I left home")
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::home W::PAGE)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::home W::pages))))
   (SENSES
    ((LF-PARENT ont::website)
     (meta-data :origin calo :entry-date 20041025 :change-date nil :wn ("home_page%1:10:00") :comments y2)
     )
    )
   )
))

#||
(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::HOME
   (SENSES
    ((LF-PARENT ONT::dwelling)
     (TEMPL PRED-S-VP-TEMPL)
     (example "he went home")
     (meta-data :origin calo-ontology :entry-date 20060418 :change-date nil :comments nil)
     )
    )
   )
))||#

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::home
   (wordfeats (W::ATYPE (? atype W::pre-vp W::post w::pre-vp)))
   (SENSES
    ((LF-PARENT ONT::to-loc)
     (SYNTAX (W::IMPRO-CLASS ONT::relative-location)
     ))
     )
   )
))
