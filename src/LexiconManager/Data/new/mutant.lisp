;;;;
;;;; w::mutant
;;;;

(define-words :pos W::adj 
 :tags (bio)
 :words (
   ((W::mutant)
    (SENSES
     ((lf-parent ont::strange)
      (templ CENTRAL-ADJ-TEMPL)
      (example "mutant EGFR")
      )))))


(define-words :pos W::n 
 :tags (bio)
 :words (
   ((W::mutant)
    (SENSES
     ((LF-PARENT ONT::MUTANT-OBJ)
      (templ count-pred-templ)
      (example "the EGFR mutant")
      )))))
