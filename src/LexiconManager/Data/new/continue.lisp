;;;;
;;;; W::continue
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-NP-TEMPL
 :words (
  (W::continue
   (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    ((LF-PARENT ONT::GO-ON)
     (example "continue to Delta/the truck continued to delta")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-TEMPL)
         ;;; Myrosia 20040805 lowered the preference so that this sense doesn't interfere unless main selectional restrictions
     ;;; so move sense should only show up in boosted versions of the domain
     (preference 0.95)
     )
    ((lf-parent ont::activity-ongoing)
     (SEM (F::Locative F::Located) (F::Time-span F::extended))
     (example "continue walking")
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% w::VP (w::vform w::ing))))
     (meta-data :origin bee :entry-date 20040805 :change-date 20090220 :comments portability-followup)
     )    
    ((lf-parent ont::activity-ongoing)
     (SEM (F::Locative F::Located) (F::Time-span F::extended))
     (example "they will continue to light up")
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL)
     (meta-data :origin bee :entry-date 20080923 :change-date 20090220 :comments pilot4)
     )
    ((lf-parent ont::activity-ongoing)
     (SEM (F::Locative F::Located) (F::Time-span F::extended))
     (example "Continue with your work")
     (TEMPL AGENT-FORMAL-XP-OPTIONAL-TEMPL (xp (% w::PP (w::pform w::with))))
     (meta-data :origin bee :entry-date 20040805 :change-date 20090220 :comments portability-followup)
     )
    ((lf-parent ont::activity-ongoing)
     (example "he continued the patient on the therapy" "the patient was continued on the drug")
     (TEMPL AGENT-AFFECTED-FORMAL-XP-PP-WITH-TEMPL (xp (% W::PP (W::ptype (? pt W::with W::on)))))
     (meta-data :origin cernl :entry-date 20110210 :change-date nil :comments ticket-242)
     )
#|
    ;;; Myrosia 20040805 lowered the preference so that this sense doesn't interfere unless main selectional restrictions
    ((LF-PARENT ONT::TALK)
     (example "continue with the story")
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-XP-TEMPL (xp (% W::pp (W::ptype W::with))))
     (preference 0.95)
     )
|#
    ))
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::continue W::on)
   (SENSES
    ;;;; Continue on to Delta/ The truck continued on to Delta
    ((LF-PARENT ONT::GO-ON)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL affected-TEMPL)
              ;;; Myrosia 20040805 lowered the preference so that this sense doesn't interfere unless main selectional restrictions
     ;;; so move sense should only show up in boosted versions of the domain
     (preference 0.95)
     )
    ;;;; continue on with the plan
    ((LF-PARENT ONT::ACTIVITY-ONGOING)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-NEUTRAL-XP-OPTIONAL-TEMPL (xp (% W::pp (W::ptype W::with))))
     (meta-data :origin bee :entry-date ? :change-date 20040805 :comments portability-followup)
     )
    ))
))

