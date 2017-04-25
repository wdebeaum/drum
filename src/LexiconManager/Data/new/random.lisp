;;;;
;;;; w::random
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((w::random w::access w::memory)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::random w::access W::memories))))
   (SENSES
    ((meta-data :origin calo :entry-date 20030605 :change-date nil :wn ("random_access_memory%1:06:00") :comments calo-y1script)
     (LF-PARENT ont::internal-computer-storage)
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::RANDOM
   (wordfeats (W::comp-op -) (w::morph (:forms (-ly))))
   (SENSES
    ((meta-data :origin monroe :entry-date 20031223 :change-date nil :wn ("random%3:00:00" "random%5:00:00:unselected:00") :comments s7)
     (lf-parent ont::random-val)
     )
    )
   )
))

