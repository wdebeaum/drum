;;;;
;;;; W::signal
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::signal
   (SENSES
    (;(LF-PARENT ONT::message)
     (LF-PARENT ONT::information)
     (meta-data :origin calo :entry-date 20050405 :change-date nil :wn ("signal%1:10:00") :comments projector-purchasing)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
   (W::signal
    (wordfeats (W::morph (:forms (-vb) :past w::signaled :ing w::signaling :nomsubjpreps (w::by w::of) :nomobjpreps (w::nil))))
;    (wordfeats (W::morph (:forms (-vb) :nom w::signaling :nomsubjpreps (w::by w::of) )))
    (SENSES
     ((LF-PARENT ONT::nonverbal-say)
      (templ agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::n 
 :words (
  ((W::SIGNALING W::PATHWAY)
   (SENSES
    (
     (LF-PARENT ONT::SIGNALING-PATHWAY)
     (TEMPL COUNT-PRED-TEMPL)
     )
    ))
  ))

(define-words :pos W::n 
 :words (
  ((W::SIGNALING W::CASCADE)
   (SENSES
    (
     (LF-PARENT ONT::SIGNALING-PATHWAY)
     (TEMPL COUNT-PRED-TEMPL)
     )
    ))
  ))
