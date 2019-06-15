;;;;
;;;; W::UPSET
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::UPSET
   (wordfeats (W::morph (:forms (-vb) :past W::upset)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090515 :comments nil :vn ("amuse-31.1") :wn ("upset%2:30:00"))
     (EXAMPLE "Aspirin upsets my stomach" "The horror movie upset me")
     (LF-PARENT ONT::evoke-upset)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090512 :change-date nil :comments nil :vn ("amuse-31.1") :wn ("upset%2:37:00" "upset%2:37:01"))
     (EXAMPLE "The horror movie upset me")
     (LF-PARENT ONT::evoke-upset)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj :templ ADJ-EXPERIENCER-TEMPL
 :words (
  (W::UPSET
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("upset%5:00:00:ill:01"))
     (LF-PARENT ONT::AILING-val)
     (templ central-adj-templ)
     )
    )
   )
))

