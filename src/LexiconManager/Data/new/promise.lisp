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

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::promise
    (wordfeats (W::morph (:forms (-vb) :nom W::promise)))
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20060120 :comments nil :vn ("future_having-13.3") :wn ("promise%2:32:01"))
     (lf-parent ont::promise) ;;  ;; 20120524 GUM change new type
     (TEMPL AGENT-FORMAL-AFFECTED-TEMPL) ; like grant,offer
     )
    (
     (lf-parent ont::promise) ;; 
     (example "he promised to go")
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL))

    (
     (lf-parent ont::promise) ;;  
     (example "he promised me to go")
     (TEMPL AGENT-AFFECTED-FORMAL-CP-SUBJCONTROL-TEMPL))

    (
     (lf-parent ont::promise) ;;  
     (example "he promised me to go")
      (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL (xp (% W::cp (W::ctype W::s-finite)))))
    )
   )
))

