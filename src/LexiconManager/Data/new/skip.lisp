;;;;
;;;; W::skip
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::skip
   (wordfeats (W::morph (:forms (-vb) :ing W::skipping :past W::skipped)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("skip%2:35:00" "skip%2:38:00"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::SKIP-ACTION)
     (example "he skipped a dose")
     )
    )
   )
))

