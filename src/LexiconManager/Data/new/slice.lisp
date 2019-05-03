;;;;
;;;; w::slice
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::slice
    (senses
     ((lf-parent ont::sheet)
      (example "a slice of pie") 
     
      (meta-data :origin plow :entry-date 20060802 :change-date nil :wn ("slice%1:13:00") :comments weather)
      )
     )
    )
))

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
(w::slice
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::cut)
   (example "slice the carrots")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))

