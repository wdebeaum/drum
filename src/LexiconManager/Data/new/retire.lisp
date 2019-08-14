;;;;
;;;; w::retire
;;;;

;; ;; 20121212 GUM change delete type and associated words

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (w::retire
    (SENSES
    ((LF-PARENT ONT::retire)
     (meta-data :origin cause-result-relations :entry-date 20180907 :change-date nil :comments nil)
     (TEMPL agent-templ)
     (example "John retired (at the age of 65)")
     )
    ((LF-PARENT ONT::retire)
     (meta-data :origin cause-result-relations :entry-date 20180907 :change-date nil :comments nil)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "John was retired after the scandal")
     )
    )
   )
))
