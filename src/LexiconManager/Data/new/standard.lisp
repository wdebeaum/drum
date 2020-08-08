;;;;
;;;; W::standard
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::standard
   (SENSES
    ((LF-PARENT ONT::REQUIREMENTS)
     (TEMPL OTHER-RELN-TEMPL)
     (example "I don't know if it meets the european standard")
     (meta-data :origin calo :entry-date 20050527 :change-date nil :wn ("standard%1:09:00") :comments projector-purchasing)
     )
    ((LF-PARENT ONT::STANDARD)
     (TEMPL OTHER-RELN-TEMPL)
     (example "We live by the standards of the community")
     (meta-data :origin calo :entry-date 20170821 :change-date nil :wn ("standard%1:10:00"))
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::standard
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("standard%5:00:00:common:01") :comments html-purchasing-corpus)
     (EXAMPLE "They are unusual")
     (lf-parent ont::typical-val)
     (sem (f::gradability +) (f::intensity ont::med) (f::orientation F::pos))
     )
    )
   )
))

