;;;;
;;;; W::CONNECTION
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
   (W::CONNECTION
   (SENSES
    ((meta-data :origin calo :entry-date 20040421 :change-date nil :comments y1demo)
     (example "I want a fast internet connection")
     (LF-PARENT ONT::connected)
     (templ other-reln-theme-templ  (xp (% W::pp (W::ptype (? pt w::to)))))
     )
    )
   )
))

