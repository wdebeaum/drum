;;;;
;;;; W::FILE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::FILE
   (SENSES
    ( ;; 20050325 changed from info-function-object to info-medium
     (LF-PARENT ONT::info-medium)
     (meta-data :origin calo :entry-date 20040406 :change-date 20050325 :wn ("file%1:10:00") :comments y1v6)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::file
   (SENSES
    ((LF-PARENT ONT::submit)
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date 20090501 :comments nil)
     (example "file the document")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
     ((LF-PARENT ONT::submit)
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date 20090501 :comments nil)
     (example "file for protection")
     (templ agent-theme-xp-templ (xp (% W::pp (W::ptype W::for))))
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

