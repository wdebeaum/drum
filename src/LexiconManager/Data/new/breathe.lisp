;;;;
;;;; W::breathe
;;;;

(define-words :pos W::v :templ affected-affected-templ
 :words (
  (W::breathe
   (wordfeats (W::morph (:forms (-vb) :nom w::breath)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::breathe)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::breathe)
     (TEMPL affected-templ) 
     )
    )
   )
))

(define-words :pos W::v :templ affected-affected-templ
 :words (
  ((W::breathe (w::in))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::inhale)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::inhale)
     (TEMPL affected-templ) ; like bleed
     )
    )
   )
))

(define-words :pos W::v :templ affected-affected-templ
 :words (
    ((W::breathe (w::out))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::exhale)
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::exhale)
     (TEMPL affected-templ) ; like bleed
     )
    )
   )
))

