;;;;
;;;; W::STOP
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::STOP
   (SENSES
    ((meta-data :origin boudreaux :entry-date 20031024)
     (LF-PARENT ONT::LOCATION)
     (example "go to this stop")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::STOP
   (wordfeats (W::morph (:forms (-vb) :nom w::stop)))
   (SENSES
    ((EXAMPLE "He stopped rioting; the computer stopped working")
     (LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% w::vp (w::vform w::ing))))
     )
    ((EXAMPLE "he/the truck stopped")
     (LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ affected-templ)
     (meta-data :origin fruit-carts :entry-date 20050308 :change-date nil :comments nil)
     )
   
    ((LF-PARENT ONT::STOP)
     (example "the computers/managers stopped the men working")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% w::vp (w::vform w::ing))))
     )
    ((LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-nogerund-TEMPL)
     (EXAMPLE "He/The storm stopped the fair/the truck")
     )
    #|
    ((LF-PARENT ONT::STOP)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-TEMPL)
     (EXAMPLE "The rioting stopped")
     )
    |#
     ((meta-data :origin wordnet-3.0 :entry-date 20090528 :change-date nil :comments nil)
     (LF-PARENT ONT::stop)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype w::from))))
     (example "The hospital stops visitors from smoking")
     )
    )
   )
))

