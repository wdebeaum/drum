;;;;
;;;; W::score
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
;a written form of a musical composition
 :words (
  (W::SCORE
   (SENSES
    ((LF-PARENT ONT::MUSICAL-DOCUMENT)
     (example "he studied the score of the sonata")
     (meta-data :wn ("score%1:10:00"))
     )
    )
   )
))


(define-words :pos W::v 
 :words (
  (W::score
   (SENSES
    ((meta-data :origin "wordnet-3.0" :entry-date 20090429 :change-date nil :comments nil :vn ("get-13.5.1"))
     (LF-PARENT ONT::acquire)
     (TEMPL agent-affected-source-optional-templ (xp (% w::pp (w::ptype (? pt w::from))))) ; like catch
     )
    )
   )
))

