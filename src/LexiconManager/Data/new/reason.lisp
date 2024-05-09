;;;;
;;;; W::REASON
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::REASON
   (SENSES
    ;; should be an AGENTnom
    ((LF-PARENT ONT::cause-effect)
     (example "the reason that he left")
     (templ count-subcat-that-optional-templ)
     (meta-data :origin trips :entry-date unknown :change-date 20041201 :wn ("reason%1:16:00") :comments caloy2)
     (preference .98) ;; prefer subcat for
     )
     ((LF-PARENT ONT::motive)
     (example "the reason for the appointment")
;     (templ count-subcat-purpose-templ)
     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype (? pt W::for W::of)))))
     (meta-data :origin plot :entry-date 20081204 :change-date nil :wn ("reason%1:16:00") :comments caloy2)
     )
    )
   )
))

; a better way would be to treat "why ..." as a CP and make a grammar rule similar to -s-that-overt
; (but then we will need to add an s-why feature?)
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::REASON W::WHY)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::reasons W::why))))
   (SENSES
    ;; should be an AGENTnom
    ((LF-PARENT ONT::cause-effect)
     (example "the reason why he left")
     (templ count-subcat-that-optional-templ)
     (meta-data :origin trips :entry-date unknown :change-date 20041201 :wn ("reason%1:16:00") :comments caloy2)
     (preference .98) ;; prefer subcat for
     )
    )
   )
))

