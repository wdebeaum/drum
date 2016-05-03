;;;;
;;;; W::PHOTOGRAPH
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PHOTOGRAPH
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("photograph%1:06:00"))
     (LF-PARENT ONT::IMAGE)
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::photograph
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("transcribe-25.4") :wn ("photograph%2:32:00"))
     (LF-PARENT ONT::record)
 ; like tape,record
     )
    )
   )
))

