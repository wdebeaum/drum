;;;;
;;;; W::FRESH
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::FRESH
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("fresh%5:00:00:original:00"))
     (LF-PARENT ONT::novelty-VAL)
     (example "a fresh idea")
     (TEMPL LESS-ADJ-TEMPL)
     )
    ((meta-data :origin step :entry-date 20080721 :change-date nil :comments nil)
     (LF-PARENT ONT::AGE-VAL)
     (example "fresh produce")
     (TEMPL LESS-ADJ-TEMPL)
     )
    )
   )
))

