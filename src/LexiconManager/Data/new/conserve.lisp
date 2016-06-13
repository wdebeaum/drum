;;;;
;;;; W::conserve
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::conserve
    (wordfeats (W::morph (:forms (-vb) :nom w::conservation)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("get-13.5.1"))
     (LF-PARENT ONT::retain)
     (TEMPL agent-affected-xp-templ) ; like reserve,book
     )
    )
   )
))

