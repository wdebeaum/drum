;;;;
;;;; w::subtract
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(w::subtract
 (senses
  ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090527 :comments nil :vn ("remove-10.1") :wn ("subtract%2:31:00"))
   (LF-PARENT ONT::cause-out-of)
   (TEMPL agent-affected-source-optional-templ)
   (example "subtract the wireless card [from the order]")
   (PREFERENCE 0.96)
   )
  ((meta-data :origin calo :entry-date 20050324 :change-date 20090522 :comments caloy2)
   (LF-PARENT ONT::calc-subtract)
   (example "subtract five dollars [from the price]" "subtract 5 [ from sin(x) ]")
   (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL agent-theme-theme-optional-templ (xp (% W::PP (W::ptype W::from))))
   )
  #||((meta-data :origin lam :entry-date 20050707 :change-date 20090522 :comments nil) 
   ;;seems to be valid only in a negative context
   (LF-PARENT ONT::calc-subtract)
   (TEMPL agent-templ)
   (example "you are not subtracting")
   )||#
  )
 )
))

