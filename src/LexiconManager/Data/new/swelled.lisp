;;;;
;;;; W::swelled
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   ((W::swelled w::up)
    (wordfeats (W::morph (:forms NIL)) (W::vform W::pastpart))
   (SENSES
    ((meta-data :origin chf :entry-date 20070810 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("swell%2:30:00" "swell%2:30:02"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (templ affected-templ)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::swelled
   ;; alternate pastpart
   (wordfeats (W::morph (:forms NIL)) (W::vform W::pastpart))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (preference .97)
     (templ affected-templ)
 ; like ferment
     )
    )
   )
))

