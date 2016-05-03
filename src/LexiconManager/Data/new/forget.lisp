;;;;
;;;; W::forget
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::forget
   (wordfeats (W::morph (:forms (-vb) :ing W::forgetting :past W::forgot :pastpart W::forgotten)))
   (SENSES
    ((LF-PARENT ONT::FORGET)
     (example "he forgot to take aspirin")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-theme-subjcontrol-templ)
     ((meta-data :origin "trips" :entry-date 20060315 :change-date nil :comments nil :wn ( "forget%2:31:03")))
     )
    ((LF-PARENT ONT::FORGET)
     (example "he forgot that he was coming")
     ((meta-data :origin "trips" :entry-date 20060315 :change-date nil :comments nil :wn ("forget%2:31:00" "forget%2:31:01"  "forget%2:31:02")))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-theme-xp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((LF-PARENT ONT::FORGET)
     (example "he forgot the recipe")
     ((meta-data :origin "trips" :entry-date 20060315 :change-date nil :comments nil :wn ("forget%2:31:00" "forget%2:31:01"  "forget%2:31:02")))
     (TEMPL agent-neutral-xp-templ)
     )
    ((LF-PARENT ONT::FORGET)
     (meta-data :origin chf :entry-date 20070904 :change-date nil :comments nil)
     (example "I forget")
     ((meta-data :origin "trips" :entry-date 20060315 :change-date nil :comments nil :wn ("forget%2:31:00" "forget%2:31:01"  "forget%2:31:02"  "forget%2:31:03")))
     (TEMPL agent-templ)
     (preference .97)
     )
    ((LF-PARENT ONT::FORGET)
     (example "he forgot about the recipe")
     ((meta-data :origin "trips" :entry-date 20060315 :change-date nil :comments nil :wn ("forget%2:31:00" "forget%2:31:01"  "forget%2:31:02")))
     (TEMPL agent-theme-xp-templ  (xp (% W::pp (W::ptype W::about))))
     )
    ;; this sense only works with "actions"
    ((LF-PARENT ONT::CANCEL)
     (example "forget the problem")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-neutral-XP-TEMPL)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::forget W::it)
   (SENSES
    ((LF (W::FORGET-IT))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_REJECT))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::forget W::that)
   (SENSES
    ((LF (W::FORGET-IT))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_REJECT))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::forget W::about W::it)
   (SENSES
    ((LF (W::FORGET-IT))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_REJECT))
     )
    )
   )
))

