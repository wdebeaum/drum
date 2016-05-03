;;;;
;;;; W::breathe
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::breathe
   (wordfeats (W::morph (:forms (-vb) :nom w::breath)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::bodily-process)
     (TEMPL agent-affected-xp-templ) ; like vomit
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::bodily-process)
     (TEMPL agent-templ) 
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  ((W::breathe (w::in))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::bodily-process)
     (TEMPL agent-affected-xp-templ) ; like vomit
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::bodily-process)
     (TEMPL agent-templ) ; like bleed
     )
    )
   )
))

(define-words :pos W::v 
 :words (
    ((W::breathe (w::out))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::bodily-process)
     (TEMPL agent-affected-xp-templ) ; like vomit
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("breathe-40.1.2") :wn ("breathe%2:29:00" "breathe%2:32:00"))
     (LF-PARENT ONT::bodily-process)
     (TEMPL agent-templ) ; like bleed
     )
    )
   )
))

