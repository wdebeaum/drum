;;;;
;;;; W::expose
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::expose
    (wordfeats (W::morph (:forms (-vb) :nom w::exposure)))
   (SENSES
    ((LF-PARENT ONT::expose)
     (EXAMPLE "he exposed the truth")
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date 20090506 :comments nil :vn ("reflexive_appearance-48.1.2"))
     )
    )
   )
))

