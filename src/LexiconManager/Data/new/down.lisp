;;;;
;;;; W::DOWN
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::DOWN
   (SENSES
    ((meta-data :origin monroe :entry-date 20060824 :change-date nil :comments nil :wn ("down%3:00:00"))
     (LF-PARENT ONT::inoperable)
     (example "the lines are down")
     (templ predicative-only-adj-templ)
     )
    ((meta-data :origin trips :entry-date 20101231 :change-date nil :comments nil)
     (LF-PARENT ONT::inoperable)
     (example "they have lines down")
     (preference .98)
     (TEMPL postpositive-ADJ-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
   ((W::down w::to)
   (SENSES
    ((LF-PARENT ONT::qmodifier)
     (LF-FORM W::down-to)
     (TEMPL number-operator-TEMPL)
     (example "down to five pounds")
     (meta-data :origin calo :entry-date 20040507 :change-date nil :comments calo-y1variants)
     )
    ((LF-PARENT ONT::down)
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     (meta-data :origin plot :entry-date 20080609 :change-date nil :comments nil)
     (example "let's go down to the beach" "all the way down to my toes")
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::DOWN
   (SENSES
    ((LF-PARENT ONT::direction)
     (example "he walked down the road/ down the wall")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    ((LF-PARENT ONT::DIRECTION)
     (example "pan the camera down" "pan down the camera")
     (TEMPL PARTICLE-TEMPL)
     )
    ;;  this is definitely not a binary SCALE-RELATION -- thinking just using DIRECTION sense for now
    #||((LF-PARENT ONT::SCALE-RELATION)
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     (example "his weight / the temperature is down")
     (SYNTAX (W::ALLOW-DELETED-COMP +) (w::degree-adv +))
     (meta-data :origin cardiac :entry-date 20080428 :change-date nil :comments nil)
     )||#
    )
   )
))

(define-words :pos W::ADV
 :words (
 ((W::DOWN w::of)
   (SENSES
    ((LF-PARENT ONT::DOWN)
     (example "to the left and down of ocean view terrace")
     (meta-data :origin fruitcarts :entry-date 20050428 :change-date nil :comments fruitcart-11-2)
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     (preference .97) ;; infrequent usage
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
 ((W::down w::from)
   (SENSES
    ((LF-PARENT ONT::DOWN)
     (example "to the left and down from ocean view terrace")
     (meta-data :origin fruitcarts :entry-date 20050428 :change-date nil :comments nil)
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     )
    )
   )
))

