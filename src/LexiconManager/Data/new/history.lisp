;;;;
;;;; W::history
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::history
   (SENSES
    ;; should be ont::time-span?
    ((LF-PARENT ONT::information-function-object)
     (EXAMPLE "in all of human history" )
     (meta-data :origin cernl :entry-date 20110706 :change-date nil :wn ("history%1:28:00") :comments nil)
;     (TEMPL mass-pred-templ)
     )
    #|
    ((LF-PARENT ONT::information-function-object)
     (EXAMPLE  "a history of success")
     (meta-data :origin task-learning :entry-date 20050815 :change-date nil :wn ("history%1:10:00") :comments nil)
     (TEMPL other-reln-associated-info-count-templ (xp (% W::pp (W::ptype (? pt W::of w::with)))))
     (preference .98)
     )
    |#
    )
   )
))

