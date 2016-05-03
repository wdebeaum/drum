;;;;
;;;; w::translate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(w::translate
  (wordfeats (W::morph (:forms (-vb) :nom W::translation)))
 (senses
   ((meta-data :origin task-learning :entry-date 20050829 :change-date nil :comments nil)
   (LF-PARENT ONT::change-format)
   (example "translate the code to java")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? pt w::into W::to)))))
   )

   ((meta-data :origin BOB :entry-date 20141212 :change-date nil :comments nil)
   (LF-PARENT ONT::GENE-TRANSLATION)
   (example "the ribosome translates the mRNA (into polypeptide)")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-RESULT-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? pt w::into W::to)))))
   )

   ))
))


