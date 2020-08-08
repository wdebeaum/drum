;;;;
;;;; W::prominent
;;;;

#|
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::prominent
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin task-learning :entry-date 20050824 :change-date nil :wn ("prominent%5:00:00:conspicuous:00" "prominent%5:00:02:conspicuous:00") :comments nil)
     (LF-PARENT ONT::STATUS-val)
     (EXAMPLE "include a prominent notice")
     )
    )
   )
))
|#

(define-words :pos W::adj
 :words (
  (w::prominent
  (senses
   ((LF-PARENT ONT::noticeable)
    (TEMPL central-adj-templ)
     (EXAMPLE "include a prominent notice")
    (sem (f::gradability +) (f::intensity ont::med) (f::orientation F::pos))
    (meta-data :origin cardiac :entry-date 20090129 :change-date 20090804 :comments nil  :wn ("prominent%5:00:00:conspicuous:00" "prominent%5:00:02:conspicuous:00") )
    (SYNTAX (W::morph (:forms (-ly))))
    )
   )
)
))

