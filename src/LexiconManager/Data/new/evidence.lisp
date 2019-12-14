;;;;
;;;; W::evidence
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::evidence
   (SENSES
    ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s14 :wn ("evidence%1:10:00"))
     (LF-PARENT ONT::evidence) ;information)
     (templ count-subcat-that-optional-templ)
     (example "the evidence that the dog ate the cat")
     )
    ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s14 :wn ("evidence%1:10:00"))
     (LF-PARENT ONT::evidence) ;information)
     (templ other-reln-templ (xp (% w::pp (w::ptype (? ptp w::of w::for)))))
     (example "the evidence of this")
     )
    )
   )
))

