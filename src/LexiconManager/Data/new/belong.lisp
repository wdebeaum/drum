;;;;
;;;; w::belong
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (w::belong
   (senses
    ((meta-data :origin plow :entry-date 20060113 :change-date nil :comments task-learning)
     (EXAMPLE "this belongs here; in/on/under the box")
     (LF-PARENT ONT::should-be-at)
     (TEMPL neutral-LOCATION-TEMPL)
     )

    (
     (EXAMPLE "this belongs to me")
     (LF-PARENT ONT::possess)
     (TEMPL neutral1-neutral-TEMPL (xp (% W::PP (W::ptype W::to))))  ;; note neutral roles in opposite order to match "I own this"
     )

    (
     (EXAMPLE "I belong to the club")
     (LF-PARENT ONT::membership)
     (TEMPL neutral-neutral-TEMPL (xp (% W::PP (W::ptype W::to))))
     )
    ))))

