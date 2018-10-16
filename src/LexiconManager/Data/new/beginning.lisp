;;;;
;;;; W::BEGINNING
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BEGINNING
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     ;(LF-PARENT ont::pos-start-of-trajectory) ;ONT::LINE-DEPENDENT-LOCATION)
     (LF-PARENT ONT::STARTPOINT)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    #|
    ((meta-data :origin plow :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::BEGINNING)
     (example "the beginning of the range ")
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     (preference 0.97)
     )
    |#
    ; nominalization of the verb
;    ((LF-PARENT ONT::TIME-POINT)
;     (example "the beginning of the meeting")
;     (meta-data :origin step :entry-date 20080623 :change-date nil :comments nil :wn ("beginning%1:28:00"))
;     (TEMPL GEN-PART-OF-RELN-ACTION-TEMPL)
;     )
;    ((LF-PARENT ONT::TIME-POINT)
;     (example "the beginning of the year")
;     (meta-data :origin step :entry-date 20080623 :change-date nil :comments nil :wn ("beginning%1:28:00"))
;     (TEMPL GEN-PART-OF-RELN-INTERVAL-TEMPL)
;     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::beginning
   (SENSES
    (
     (LF-PARENT ONT::start-time)
     (example "Beginning Monday I work.")
     (TEMPL binary-constraint-S-templ (xp (% W::NP (W::case (? cas W::obj -)) (w::lf (% w::description (w::class ont::time-loc))))))
     )
    )
   )
))

