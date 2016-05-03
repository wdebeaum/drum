;;;;
;;;; W::NOTE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::NOTE
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("note%1:10:02" "note%1:10:03" "note%1:10:00"))
     (LF-PARENT ONT::annotation)
     (templ other-reln-theme-templ)
     )
    )
   )
))

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::note
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("sight-30.2") :wn ("note%2:39:00" "note%2:39:02"))
     (LF-PARENT ONT::becoming-aware)
     (TEMPL agent-neutral-xp-templ) ; like regard
;     (PREFERENCE 0.96)
     )
    (
     (LF-PARENT ONT::becoming-aware)
     (TEMPL agent-formal-xp-templ) 
     )
    ((EXAMPLE "note it")
     (LF-PARENT ONT::RECORD)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-xp-templ)
     )
    )
   )
))

