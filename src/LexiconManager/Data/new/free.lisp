;;;;
;;;; W::free
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :tags (:base500)
 :words (
  (W::free
   (wordfeats (W::morph (:forms (-vb) :past W::freed :ing W::freeing)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("free-78-1"))
     (LF-PARENT ONT::releasing)
 ; like release
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090508 :comments nil :vn ("cheat-10.6-1-1"))
     (LF-PARENT ONT::pardon)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::FREE
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil)
     (EXAMPLE "that's free [for him]")
     (LF-PARENT ONT::AVAILABLE)
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("free%3:00:00"))
     (EXAMPLE "you're free to go")
     (LF-PARENT ONT::AVAILABLE)
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::CP (W::CTYPE W::s-to))))
     )
    ((example "we have the second ambulance free")
     (LF-PARENT ONT::AVAILABLE)
     (SEM (F::GRADABILITY F::-))
     (TEMPL postpositive-adj-optional-xp-templ)
     (meta-data :origin monroe :entry-date 20050404 :change-date 20090731 :wn ("free%5:00:02:unoccupied:00") :comments s12)
     )
    ((meta-data :origin calo :entry-date 20060615 :change-date nil :wn ("free%5:00:00:unpaid:00") :comments pq)
     (LF-PARENT ONT::no-COST-val)
     (example "a free breakfast")
     (SEM (F::GRADABILITY F::+))
     )
    ((LF-PARENT ONT::free-val)
     (example "after the Civil War he was a free man")
     (meta-data :origin adjective-reorganization :entry-date 20170317 :change-date nil :comments nil)
     )
    )
   )
))

