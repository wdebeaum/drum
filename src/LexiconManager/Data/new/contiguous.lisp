;;;;
;;;; W::contiguous
;;;;

(define-words :pos W::adj 
  :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::contiguous
    ;; can't auto-generate adv with theme templ
 ;;  (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((LF-PARENT ONT::CONNECTED-VAL)
     (meta-data :origin task-learning :entry-date 20051028 :change-date nil :wn ("contiguous%5:00:01:connected:00") :comments nil)
     (EXAMPLE "combine contiguous cells")
     (TEMPL adj-theme-templ)
     )
    )
   )
))

