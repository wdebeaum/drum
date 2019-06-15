;;;;
;;;; W::press
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::press
   (SENSES
    ((meta-data :origin calo :entry-date 20050325 :change-date nil :wn ("press%1:10:00") :comments caloy2)
     (LF-PARENT ONT::info-medium)
     (example "he saw a press release")
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::press
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL)  ; like dare
     (example "Napoleon pressed him to become the Emporer's painter")
     (preference .98) ;; prefer physical sense
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("push-12-1-1"))
     (LF-PARENT ONT::press)
     (example "he pressed the door closed")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like push
     )
        )
   )
))
