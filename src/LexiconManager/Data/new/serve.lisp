;;;;
;;;; W::serve
;;;;

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::serve
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090501 :comments nil :vn ("fulfilling-13.4.1-1"))
     (LF-PARENT ONT::supply)
     (example "that company only serves paying customers")
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-affected-recipient-alternation-templ) ; like supply
     )
    ((EXAMPLE "The knife serves to open the letter")
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     (lf-parent ont::cause-effect) ;;  20121028 GUM change new parent
     (TEMPL AGENT-THEME-SUBJCONTROL-TEMPL)
     )

    )
   )
))

