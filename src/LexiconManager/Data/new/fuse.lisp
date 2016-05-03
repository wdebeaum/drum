;;;;
;;;; W::fuse
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::fuse
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("mix-22.1-1-1"))
     (LF-PARENT ONT::combine-objects)
     (TEMPL agent-affected2-optional-templ (xp (% w::pp (w::ptype w::with)))) ; like combine
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("mix-22.1-1-1"))
     (LF-PARENT ONT::combine-objects)
 ; like mix
     )
    )
   )
))

