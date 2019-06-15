;;;;
;;;; w::belong
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (w::belong
   (senses
    ((meta-data :origin plow :entry-date 20060113 :change-date nil :comments task-learning)
     (EXAMPLE "this belongs here; in/on/under the box")
     (LF-PARENT ONT::should-be-at)
     (TEMPL NEUTRAL-LOCATION-XP-TEMPL)
     )

    (
     (EXAMPLE "this belongs to me")
     (LF-PARENT ONT::possess)
     (TEMPL NEUTRAL1-NEUTRAL-XP-TEMPL (xp (% W::PP (W::ptype W::to))))  ;; note neutral roles in opposite order to match "I own this"
     )

    (
     (EXAMPLE "I belong to the club")
     (LF-PARENT ONT::membership)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::PP (W::ptype W::to))))
     )
    ))))

