;;;;
;;;; W::conflict
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::conflict
   (SENSES
    (
     (lf-parent ont::transgress)
     (TEMPL reln-agent-affected-optional-templ (xp2 (% W::PP (W::ptype (? pp W::with)))))
     )
   ))
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::conflict  ; should be stative?
   ;(wordfeats (W::morph (:forms (-vb) :nom w::conflict)))
   (SENSES
    (;(LF-PARENT ONT::HINDERING)
     (LF-PARENT ONT::transgress)
     (Example "a plan conflicts with another")
     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-expt)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL (xp (% W::PP (W::ptype W::with))))
     )    
    (;(LF-PARENT ONT::HINDERING)
     (LF-PARENT ONT::transgress)
      (example "2 plans conflict")
      (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-expt)
      (TEMPL AFFECTED-NP-PLURAL-TEMPL)
      )
     ))
))

