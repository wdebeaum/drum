;;;;
;;;; W::finding
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::finding
   (SENSES
    ((meta-data :origin calo :entry-date 20110311 :change-date nil :comments cernl :wn ("outcome%1:11:00"))
     ;(LF-PARENT ONT::clinical-finding)
     (LF-PARENT ONT::outcome)
     (TEMPL OTHER-RELN-TEMPL)
     (preference .98) ;; prefer verb
     )
    )
   )
))

