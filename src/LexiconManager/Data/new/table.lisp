;;;;
;;;; w::table
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (w::table
   (SENSES
    ((meta-data :origin caloy2 :entry-date 20050404 :change-date nil :wn ("table%1:06:01") :comments projector-purchasing)
  ;   (LF-PARENT ont::furnishings)
     (lf-parent ont::table)
     )
    ((meta-data :origin plow :entry-date 20060303 :change-date nil :wn ("table%1:14:00") :comments pq)
     (LF-PARENT ONT::chart)
     (preference .98)
;;     (templ other-reln-templ) let it be a mods
     (example "here is the table of results")
     )
    )
   )
))

