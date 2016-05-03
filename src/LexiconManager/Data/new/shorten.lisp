;;;;
;;;; W::shorten
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(W::shorten
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050919 :change-date 20090504 :comments nil)
     (EXAMPLE "names in your address book are shortened to the name only, without the email address")
     (LF-PARENT ONT::decrease)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

