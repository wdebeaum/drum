;;;;
;;;; W::tired
;;;;

(define-words :pos W::adj :templ ADJ-EXPERIENCER-TEMPL
 :words (
  ;; later - derive from verb
  (W::tired
   (wordfeats (W::morph (:FORMS ( -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("tired%3:00:00"))
     (LF-PARENT ont::physical-symptom-val)
     (templ central-adj-templ)
     )
    )
   )
))

