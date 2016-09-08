;;;;
;;;; W::TIME
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::TIME
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("time%1:28:06"))
;     (LF-PARENT ONT::TIME-point)
     (LF-PARENT ONT::TIME-MEASURE-SCALE)
     (example "what time did it arrive")
     (templ other-reln-templ)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("time%1:28:05" "time%1:28:00" "time%1:28:03" "time%1:28:01"))
     (LF-PARENT ONT::TIME-INTERVAL)
     (example "how much time does it take")
     (SEM (F::time-scale F::interval))
     (TEMPL MASS-PRED-TEMPL)
     )
    ;; what's the example for this one?
;    ((LF-PARENT ONT::TIME-INTERVAL)
;     (SEM (F::time-scale F::interval))
    ; (example "a time of five minutes")
;     (TEMPL reln-subcat-of-units-TEMPL)
;     )

    ((LF-PARENT ONT::multiple)
     (example "increase by two times")
     (TEMPL SUBSTANCE-UNIT-TEMPL)
     )

    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::time
   (SENSES
    ((LF-PARENT ONT::register)
     (TEMPL agent-theme-xp-templ)
     (example "He timed the runners")
     (meta-data :origin trips :entry-date 20090910 :change-date nil :comments nil :vn ("register-54.1"))
     )
    ((LF-PARENT ONT::register)
     (TEMPL agent-neutral-extent-templ)
     (example "He timed the runner at 1.64")
     (meta-data :origin trips :entry-date 20090910 :change-date nil :comments nil :vn ("register-54.1"))
     )
    ((LF-PARENT ONT::register)
     (TEMPL neutral-extent-xp-templ (xp (% W::PP (W::ptype W::at))))
     (example "he timed at 1.64 in the 10-yd dash")
     (meta-data :origin trips :entry-date 20090910 :change-date nil :comments nil :vn ("register-54.1"))
     )
    )
   )
))

