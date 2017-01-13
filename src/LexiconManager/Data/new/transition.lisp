;;;;
;;;; W::TRANSITION
;;;;

#|
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;      :COMMENTS HTML-PURCHASING-CORPUS))))
  (W::TRANSITION
   (SENSES
    ((LF-PARENT ONT::event) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("transition%1:11:00")
      :COMMENTS HTML-PURCHASING-CORPUS))
    ))
  ))
|#

(define-words :pos W::v :templ COUNT-PRED-TEMPL
 :words (
;      :COMMENTS HTML-PURCHASING-CORPUS))))
  (W::TRANSITION
   (wordfeats (W::morph (:forms (-vb) :nom w::transition :nomobjpreps (w::from w::of))))
   (SENSES
    ((LF-PARENT ONT::event-of-change) 
     (TEMPL AGENT-affected-RESULT-TEMPL (xp (% w::pp (w::ptype (? tt w::to w::into)))))
     )
    ((LF-PARENT ONT::change)
     (templ affected-theme-xp-optional-templ  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
     )

    ))
  ))
