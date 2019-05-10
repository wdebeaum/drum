;;;;
;;;; w::nervous
;;;;

(define-words :pos W::adj
 :words (
  (w::nervous
  (senses
   
;   ((LF-PARENT ONT::uneasy)
;    (TEMPL central-adj-experiencer-templ)
;    (example "I am nervous")
;    )

    (
     (LF-PARENT ONT::uneasy)
     (templ central-adj-templ)
;     (templ central-adj-content-templ)
     (example "a nervous night")
     )
    
    (
     (LF-PARENT ONT::uneasy)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::about)))))
     (example "I am nervous about it")
     )

   ((LF-PARENT ONT::body-system-val)
    (TEMPL central-adj-templ)
    (example "nervous system; nervous disorder")
    (meta-data :origin cardiac :entry-date 20090408 :change-date nil :comments nil)
    )
    
   )
)
))

