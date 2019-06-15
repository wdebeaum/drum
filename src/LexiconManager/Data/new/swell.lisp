;;;;
;;;; W::swell
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::swell
   (wordfeats (W::morph (:forms (-vb) :past W::swelled :pastpart W::swollen :ing W::swelling)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("swell%2:30:00" "swell%2:30:02"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (templ affected-unaccusative-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::size-scale))
 ; like ferment
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::swell)
     (preference .97) ;; this is a rare usage; prefer intransitive
     (example "the medication swelled his ankles")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::size-scale))

 ; like ferment
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::swell w::up)
   (wordfeats (W::morph (:forms (-vb) :past W::swelled :pastpart W::swollen :ing W::swelling)))
   (SENSES
    ((meta-data :origin chf :entry-date 20070810 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("swell%2:30:00" "swell%2:30:02"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (templ affected-unaccusative-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::size-scale))
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::swell)
     (preference .98) ;; prefer intransitive
     (example "the medication swelled up his ankles")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::size-scale))
 ; like ferment
     )
    )
   )
))

