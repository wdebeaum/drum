;;;;
;;;; W::BOOST
;;;;

#|
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BOOST
   (SENSES
    ((LF-PARENT ONT::adjust) (TEMPL other-reln-theme-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))
|#

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(w::boost
   (wordfeats (W::morph (:forms (-vb) :nom w::boost)))
 (senses
  ;; need the agent templ for the imperative
  #|
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments calo-y1script)
   (LF-PARENT ONT::increase)
   (example "boost the dose [to 3K]")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
   )
  |#
  ((meta-data :origin cardiac :entry-date 20081223 :change-date 20090504 :comments LM-vocab)
   (LF-PARENT ONT::increase)
   (example "it boosted the pressure")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
   )
  )
  )
 ))

