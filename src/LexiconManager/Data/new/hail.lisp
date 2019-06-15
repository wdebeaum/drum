;;;;
;;;; W::hail
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::hail
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("hail%1:19:00"))
     (LF-PARENT ONT::precipitation)
     (TEMPL MASS-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (w::hail
   (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("hail%2:32:02"))
     (LF-PARENT ONT::praise)
     (TEMPL AGENT-AGENT1-NP-TEMPL) ; like thank
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::precipitating)
     (SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL EXPLETIVE-TEMPL)
     (EXAMPLE "it started hailing")
     (meta-data :origin calo-ontology :entry-date 20060124 :change-date nil :comments caloy3)
     )    
    )
   )
))

