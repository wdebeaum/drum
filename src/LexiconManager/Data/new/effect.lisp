;;;;
;;;; W::effect
;;;;

(define-words :pos W::n 
 :words (
  (W::effect
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::caused-event)
     (TEMPL BARE-PRED-TEMPL)
     (preference 0.96) ;; less likely synonym to "side effect"
     )
    )
   )
))

(define-words :pos W::V 
 :words (
  (w::effect
   (wordfeats (W::morph (:forms (-vb) :nom W::effect :nomsubjpreps (w::of) :nomobjpreps (w::on))))
   (senses
    ((lf-parent ont::objective-influence)
     (TEMPL agent-affected-xp-templ) 
     (example "to effect a change")
     )
    ))
  ))
