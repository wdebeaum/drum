;;;;
;;;; W::RATE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::RATE
   (SENSES
    #||((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::RATE)
     (TEMPL other-reln-templ)
     )||#
    ((example "the rate of decline")
     (LF-PARENT ONT::rate-scale)
     (TEMPL other-reln-theme-templ)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::rate-scale)
     (example "he drove at a rate of 5 mph")
     (TEMPL reln-subcat-of-units-TEMPL)
     )
    ))
   ))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::rate
   (wordfeats (W::morph (:forms (-vb) :nom w::rating)))
   (SENSES
    ((LF-PARENT ONT::ordering)
     (TEMPL agent-neutral-xp-templ)
     (example "how is this model rated?")
     (meta-data :origin calo :entry-date 20041122 :change-date 20090501 :comments caloy2)
     )
    )
   )
))

