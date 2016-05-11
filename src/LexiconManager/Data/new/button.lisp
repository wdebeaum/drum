;;;;
;;;; W::BUTTON
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BUTTON
   (SENSES
    ((meta-data :origin plow :entry-date 20050318 :change-date nil :comments nil)
     (example "click on the button in the corner of the browser")
     (LF-PARENT ONT::icon)
     )
    ((meta-data :origin caet :entry-date 20110803 :change-date nil :comments nil)
     (example "press the button to turn it on")
 ;    (LF-PARENT ONT::operating-switch)
     (lf-parent ont::button)
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
   (W::button
    (wordfeats (W::morph (:forms (-vb) :past W::buttoned :ing W::buttoning)))
   (SENSES
    ((meta-data :origin trips :entry-date 20090401 :change-date nil :comments nil)
     (LF-PARENT ONT::attach)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

