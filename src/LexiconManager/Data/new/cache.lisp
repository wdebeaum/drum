;;;;
;;;; W::CACHE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CACHE
   (SENSES
    ((LF-PARENT ont::internal-computer-storage) (TEMPL count-pred-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("cache%1:06:01")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::cache
   (wordfeats (W::morph (:forms (-vb) :nom w::cache)))
   (SENSES
    ((EXAMPLE "cache the webpage")
     (meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil)
     (LF-PARENT ONT::put-away)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

