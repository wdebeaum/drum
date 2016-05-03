;;;;
;;;; w::mobile
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::mobile W::phone)
    (wordfeats (W::morph (:forms (-S-3P) :plur (W::mobile W::phones))))
   (SENSES
    ((LF-PARENT ONT::device)
     (SEM (F::mobility F::non-self-moving))
     (meta-data :origin calo :entry-date 20041206 :change-date nil :wn ("mobile_phone%1:06:00") :comments caloy2)
     (example "can I access it from my mobile phone")
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::mobile
   (SENSES
    ((LF-PARENT ONT::device)
     (SEM (F::mobility F::non-self-moving))
     (meta-data :origin calo :entry-date 20041206 :change-date nil :comments caloy2)
     (example "can I access it from my mobile")
     )
    )
   )
))

