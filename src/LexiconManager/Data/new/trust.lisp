;;;;
;;;; W::trust
;;;;

#|
(define-words :pos W::n 
 :words (
   (W::trust
   (SENSES
    ((LF-PARENT ONT::confidence-scale)
     (meta-data :origin task-learning :entry-date 20080206 :change-date nil :wn ("confidence%1:12:00") :comments nil)
     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype (? pt w::in W::about)))))
     )
    )
   )
))
|#


(define-words :pos W::v 
 :words (
 (W::trust
   (wordfeats (W::morph (:forms (-vb) :nom w::trust :nomsubjpreps (w::of w::by) :nomobjpreps (w::in))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::TRUST)
     (SEM (F::Aspect F::Indiv-level))
     (TEMPL EXPERIENCER-NEUTRAL-XP-TEMPL)
     )    
    )
   )
))

