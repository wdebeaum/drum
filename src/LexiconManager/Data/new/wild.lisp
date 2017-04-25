;;;;
;;;; w::wild
;;;; 

(define-words :pos W::n :templ MASS-PRED-TEMPL
 :words (
  ((w::wild w::rice)
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin food-kb :entry-date 20050801 :change-date nil :wn ("wild_rice%1:13:00" "wild_rice%1:20:00") :comments nil)
     (LF-PARENT ont::grains)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::wild
   (wordfeats (W::morph (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am wild / an angry person")
     (LF-PARENT ONT::wild-val)
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am wild about her")
     (LF-PARENT ONT::wild-val)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::about)))))
     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date nil :comments nil :wn ("happy%3:00:00"))
     (example "a wild idea")
     (LF-PARENT ONT::atypical-val)
     (templ central-adj-templ)
     (sem (f::gradability +) (f::intensity ont::med) (f::orientation ont::less))
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (bio)
 :words (
   ((W::wild W::type)
    (SENSES
     ((example "wild type EGFR")
      (LF-PARENT ONT::NATURAL)
;      (templ central-adj-experiencer-templ)
      )))))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (bio)
 :words (
   ((W::wild W::punc-minus W::type)
    (SENSES
     ((example "wild-type EGFR")
      (LF-PARENT ONT::NATURAL)
;      (templ central-adj-experiencer-templ)
      )))))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (bio)
 :words (
   (W::wildtype
    (SENSES
     ((example "wildtype EGFR")
      (LF-PARENT ONT::NATURAL)
;      (templ central-adj-experiencer-templ)
      )))
   ))

(define-words :pos W::n 
 :tags (bio)
 :words (
   ((W::wild W::type)
    (SENSES
     ((example "the EGFR wild type")
      (LF-PARENT ONT::WILDTYPE-OBJ)
      (templ count-pred-templ)
      )))))

(define-words :pos W::n
 :tags (bio)
 :words (
   ((W::wild W::punc-minus W::type)
    (SENSES
     ((example "the EGFR wild-type")
      (LF-PARENT ONT::WILDTYPE-OBJ)
      (templ count-pred-templ)
      )))))

(define-words :pos W::n
 :tags (bio)
 :words (
   (W::wildtype
    (SENSES
     ((example "the EGFR wildtype")
      (LF-PARENT ONT::WILDTYPE-OBJ)
      (templ count-pred-templ)
      )))
   ))
