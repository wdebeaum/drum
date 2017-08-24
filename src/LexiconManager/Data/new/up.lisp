;;;;
;;;; W::UP
;;;;

(define-words :pos W::ADV
 :words (
  ((W::UP w::to)
   (SENSES
    ((LF-PARENT ONT::qmodifier)
     (LF-FORM W::up-to)
     (TEMPL number-operator-TEMPL)
     (example "I can spend up to five dollars")
     (meta-data :origin calo :entry-date 20040507 :change-date nil :comments calo-y1variants)
     )
    ((LF-PARENT ONT::UP)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (meta-data :origin plot :entry-date 20080609 :change-date nil :comments nil)
     (example "let's practice up to here" "all the way up to my shins")
     )

    
;; 3/2011 removing sit-val role for event-time-rel
;    ((LF-PARENT ONT::event-time-rel)
;     (example "practice up to this step")
;     (meta-data :origin plot :entry-date 20080613 :change-date nil :comments nil)
;     (TEMPL binary-constraint-SIT-VAL-NP-TEMPL)
;     )
    )
   )
))

#|
(define-words :pos W::ADV
 :words (
    ((W::up w::from)
   (SENSES
    ((LF-PARENT ONT::qmodifier)
     (LF-FORM W::down-to)
     (TEMPL number-operator-TEMPL)
     (example "up from five pounds")
     (meta-data :origin calo :entry-date 20040507 :change-date nil :comments calo-y1variants)
     )
    ((LF-PARENT ONT::up)
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     (meta-data :origin plot :entry-date 20080609 :change-date nil :comments nil)
     (example "all the way up from my toes")
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
    ((W::up w::of)
   (SENSES
    ((LF-PARENT ONT::up)
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     (meta-data :origin plot :entry-date 20080609 :change-date nil :comments nil)
     (example "it's up of that")
     )
    )
   )   
))
|#

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::UP
   (SENSES
    ((LF-PARENT ONT::DIRECTION-UP-GROUND)
     (example "the swelling is moving up his leg")
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     )
    ((LF-PARENT ONT::DIRECTION-UP)
     (example "pan the camera UP")
     (TEMPL PARTICLE-TEMPL)
     )
    
    ((LF-PARENT ONT::COMPLETELY)
     (TEMPL PARTICLE-MANNER-TEMPL)
     (meta-data :origin plot :entry-date 20080609 :change-date nil :comments nil)
     (example "let's practice up to here" "all the way up to my shins")
     )
    )
   )
))

(define-words :pos W::adj
 :tags (:base500)
 :words (
  (w::up
  (senses
   ((LF-PARENT ONT::in-working-order-val)
    (TEMPL central-adj-templ)
    )
   ((LF-PARENT ONT::DIRECTION-UP)
     (TEMPL PREDICATIVE-ONLY-ADJ-TEMPL)
     (example "his weight / the temperature is up (X)")
     (SYNTAX (W::ALLOW-DELETED-COMP +) (w::degree-adv +))
     (meta-data :origin cardiac :entry-date 20080428 :change-date nil :comments nil)
     ) 
   )
)
))

#|
(define-words :pos W::ADV
 :words (
  ((W::up w::of)
   (SENSES
    ((LF-PARENT ONT::spatial-loc)
     (example "to the left and up of ocean view terrace")
     (meta-data :origin fruitcarts :entry-date 20050428 :change-date nil :comments nil)
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
   ((W::up w::from)
   (SENSES
    ((LF-PARENT ONT::spatial-loc)
     (example "to the left and up from ocean view terrace")
     (meta-data :origin fruitcarts :entry-date 20050428 :change-date nil :comments nil)
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     )
    )
   )
))
|#

