;;;;
;;;; W::record
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::record
   (SENSES
    ((meta-data :origin plow :entry-date 20050928 :change-date nil :wn ("record%1:10:03") :comments naive-subjects)
     (LF-PARENT ONT::chronicle) ;info-medium)
     (example "Keep a record of the proceedings")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-NEUTRAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::record
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("transcribe-25.4") :wn ("record%2:32:01"))
     (EXAMPLE "record the proceedings")
     (LF-PARENT ONT::RECORD)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    )
   )
))

