;;;;
;;;; W::SPEED
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::SPEED
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("speed%1:28:00"))
     (LF-PARENT ONT::RATE)
     (TEMPL other-reln-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("speed%1:28:00"))
     (LF-PARENT ONT::RATE)
     (TEMPL reln-subcat-of-units-TEMPL)
     )
    )
   )
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  ((W::speed (w::up))
   (wordfeats (W::morph (:forms (-vb) :past W::sped :nom (w::speed w::up))))
   (SENSES
    ((meta-data :origin coordops :entry-date 20070514 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::increase)
     (TEMPL agent-templ)
     (example "speed up")
     )
    ((meta-data :origin calo :entry-date 20070514 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::increase-speed)
     (TEMPL agent-affected-xp-templ)
     (example "speed up the process")
     )
    )
   )
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::speed
   (wordfeats (W::morph (:forms (-vb) :past W::sped :nom w::speed)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("speed%2:30:00" "speed%2:30:02" "speed%2:38:00" "speed%2:38:03"))
     (LF-PARENT ONT::move-rapidly)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

