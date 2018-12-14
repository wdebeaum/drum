;;;;
;;;; w::retire
;;;;

;; ;; 20121212 GUM change delete type and associated words
;(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
; :words (
;   (w::retire
;    (SENSES
;    ((LF-PARENT ONT::prepare-for-sleep)
;     (meta-data :origin cardiac :entry-date 20080828 :change-date nil :comments nil)
;     (TEMPL agent-templ)
;     (example "he usually retires early")
;     )
;    )
;   )
;))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
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
     (TEMPL agent-affected-xp-templ)
     (example "John was retired after the scandal")
     )
    )
   )
))
