;;;;
;;;; W::notice
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::notice
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :wn ("notice%1:10:00" "notice%1:10:01" "notice%1:10:02" "notice%1:10:03") :comments html-purchasing-corpus)
     (LF-PARENT ONT::information)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
 (W::notice
   (SENSES
    ((LF-PARENT ONT::message)
     (templ count-subcat-that-optional-templ)
     (meta-data :origin caloy3 :entry-date 20070330 :change-date nil :comments y3-test-data)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::NOTICE
   (SENSES
    ((LF-PARENT ONT::information-function-object) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
 (W::notice
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("see-30.1-1"):wn ("notice%2:39:00" "notice%2:39:04"))
     (LF-PARENT ONT::becoming-aware)
     (TEMPL AGENT-FORMAL-XP-CP-TEMPL)
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("see-30.1-1") :wn ("notice%2:39:00" "notice%2:39:04"))
     (LF-PARENT ONT::becoming-aware)
     (TEMPL agent-templ) 
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::becoming-aware)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-neutral-XP-TEMPL)
     (example "he perceived the damage")
     (meta-data :origin calo-ontology :entry-date 20051213 :change-date nil :comments Perceive :vn ("see-30.1-1"))
     )
    )
   )
))

