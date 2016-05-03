;;;;
;;;; W::cleanse
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::cleanse
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090601 :comments nil :vn ("cheat-10.6") :wn ("cleanse%2:30:05"))
     (LF-PARENT ONT::remove-parts)
     (TEMPL agent-affected-theme-optional-templ (xp (% w::pp (w::ptype w::of))))
     )
    )
   )
))

