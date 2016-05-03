;;;;
;;;; W::prediction
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::prediction
   (SENSES
    ((LF-PARENT ONT::predict)
     (example "click here for the forecast")
     (meta-data :origin task-learning :entry-date 20051109 :change-date nil :wn ("prediction%1:10:00") :comments zipcode-dialog)
     (templ other-reln-theme-templ  (xp (% W::pp (W::ptype (? pt w::about)))))
     )
    )
   )
))

