;;;;
;;;; W::DIAGRAM
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::DIAGRAM
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :wn ("diagram%1:06:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::chart)
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
    (w::diagram
     (senses
      ((lf-parent ont::write)
       (example "diagram this object")
       (templ agent-neutral-xp-templ)	     
       (meta-data :origin calo-ontology :entry-date 20060713 :change-date 20090506 :comments caloy3)
       )
      )
     )
))

