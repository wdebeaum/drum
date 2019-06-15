;;;;
;;;; w::fancy
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::fancy
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("want-32.1") :wn ("fancy%2:37:00"))
     (LF-PARENT ONT::want)
     (TEMPL experiencer-neutral-templ) ; like desire
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::fancy
   (SENSES
    ((meta-data :origin calo :entry-date 20041122 :change-date nil :wn ("fancy%3:00:00") :comments caloy2)
     (EXAMPLE "I don't need anything fancy")
     (lf-parent ont::not-plain-val)
     )
    )
   )
))

