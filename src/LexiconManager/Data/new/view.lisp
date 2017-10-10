;;;;
;;;; W::VIEW
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::VIEW
   (SENSES
;    ((meta-data :origin calo :entry-date 20031229 :change-date nil :wn ("view%1:09:01" "view%1:06:00") :comments calo-y1script)
;     (LF-PARENT ONT::IMAGE)
;     (TEMPL OTHER-RELN-TEMPL)
;     )
    ((lf-parent ont::opinion)
     (example "he has strong views about that")
     (meta-data :origin calo-ontology :entry-date 20060215 :change-date nil :wn ("view%1:09:04") :comments caloy3)
     (TEMPL OTHER-RELN-TEMPL)
     )	   
    )
   )
))

(define-words :pos W::v 
 :words (
 (W::view
   (wordfeats (W::morph (:forms (-vb) :nom w::view)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::ACTIVE-PERCEPTION)
;;     (SEM (F::Time-span F::atomic))
     (TEMPL agent-NEUTRAL-TEMPL)
     )
    )
   )
))

