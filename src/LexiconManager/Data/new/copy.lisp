;;;;
;;;; W::copy
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::copy
    (wordfeats (W::morph (:forms (-vb) :ing w::copying :nom W::copy)))
    (SENSES
     (
      (meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("transcribe-25.4") :wn ("copy%2:36:01" "copy%2:36:05"))
      (LF-PARENT ONT::duplicate)
      (preference .98)   ;; prefer the other sense that requires the goal/recipient
      (TEMPL agent-neutral-xp-templ)
      )
     ((lf-parent ont::sendcopy)
      (SEM (F::aspect F::bounded) (F::time-span F::atomic))
      (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-NP-TEMPL (xp (% W::PP (W::ptype w::on))))
      (example "copy my group on the email")
      (meta-data :origin plow :entry-date 20051004 :change-date nil :comments nil :vn ("send-11.1-1"))
     )
     ((lf-parent ont::sendcopy)
      (SEM (F::aspect F::bounded) (F::time-span F::atomic))
      (TEMPL AGENT-AFFECTED-RESULT-ADVBL-OBJCONTROL-TEMPL)
      (example "copy that to me")
      (meta-data :origin plow :entry-date 20051004 :change-date nil :comments nil :vn ("send-11.1-1"))
      )
     )
    )
   ))

