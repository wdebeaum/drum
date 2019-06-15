;;;;
;;;; W::call
;;;;

#|
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::call
   (SENSES
    ((LF-PARENT ONT::conversation)
     (example "give him a call at the office")
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date 20090505 :wn ("call%1:10:01") :comments Office)
     )
    )
   )
))
|#

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::call
   (wordfeats (W::morph (:forms (-vb) :nom w::call)))
   (SENSES
    ((EXAMPLE "Call the doctor")
     (LF-PARENT ONT::establish-communication)
     (TEMPL AGENT-AGENT1-NP-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090501 :comments nil :vn ("dub-29.3") :wn ("call%2:32:00" "call%2:32:02" "call%2:41:14"))
     (EXAMPLE "call me ishmael")
     (LF-PARENT ONT::naming)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-NP-TEMPL)
     )
    ((EXAMPLE "he calls for change")
     (LF-PARENT ONT::appeal-apply-demand)
     (TEMPL AGENT-FORMAL-XP-PP-ABOUT-TEMPL  (xp (% W::PP (W::ptype W::for))))
     (meta-data :origin step :entry-date 20080629 :change-date 20090508 :comments nil)
     )
    ))))

