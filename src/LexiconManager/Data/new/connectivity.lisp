;;;;
;;;; W::CONNECTIVITY
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CONNECTIVITY
   (SENSES
    ((meta-data :origin calo :entry-date 20050522 :change-date nil :wn ("connectivity%1:07:00") :comments caloy2)
     (LF-PARENT ONT::connection)
     (templ other-reln-theme-templ  (xp (% W::pp (W::ptype (? pt w::to)))))
     )
    )
   )
))

