;;;;
;;;; W::promise
;;;;

#|
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::promise
   (SENSES
    ((LF-PARENT ONT::promise)
     (TEMPL COUNT-PRED-TEMPL)
     (example "he made a promise")
     (META-DATA :ORIGIN plow :ENTRY-DATE 20060628 :CHANGE-DATE nil :wn ("promise%1:10:00") :COMMENTS nil))
    )
   )
))
|#

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::promise
    (wordfeats (W::morph (:forms (-vb) :nom W::promise)))
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20060120 :comments nil :vn ("future_having-13.3") :wn ("promise%2:32:01"))
     (lf-parent ont::promise) ;;  ;; 20120524 GUM change new type
     (TEMPL agent-affected-iobj-theme-templ) ; like grant,offer
     )
    (
     (lf-parent ont::promise) ;; 
     (example "he promised to go")
     (TEMPL AGENT-THEME-SUBJCONTROL-TEMPL))

    (
     (lf-parent ont::promise) ;;  
     (example "he promised me to go")
     (TEMPL agent-affected-theme-subjcontrol-templ))

    (
     (lf-parent ont::promise) ;;  
     (example "he promised me to go")
      (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL (xp (% W::cp (W::ctype W::s-finite)))))
    )
   )
))

