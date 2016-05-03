;;;;
;;;; W::swell
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::swell
   (wordfeats (W::morph (:forms (-vb) :past W::swelled :pastpart W::swollen :ing W::swelling)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("swell%2:30:00" "swell%2:30:02"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (templ affected-unaccusative-templ)
 ; like ferment
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::swell)
     (preference .97) ;; this is a rare usage; prefer intransitive
     (example "the medication swelled his ankles")
     (templ agent-affected-xp-templ)
 ; like ferment
     )
    )
   )
))

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  ((W::swell w::up)
   (wordfeats (W::morph (:forms (-vb) :past W::swelled :pastpart W::swollen :ing W::swelling)))
   (SENSES
    ((meta-data :origin chf :entry-date 20070810 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("swell%2:30:00" "swell%2:30:02"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (templ affected-unaccusative-templ)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::swell)
     (preference .98) ;; prefer intransitive
     (example "the medication swelled up his ankles")
     (templ agent-affected-xp-templ)
 ; like ferment
     )
    )
   )
))

