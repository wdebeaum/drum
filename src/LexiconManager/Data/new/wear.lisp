;;;;
;;;; W::wear
;;;;

(define-words :pos W::v 
 :words (
  ((W::wear W::off)
   (wordfeats (W::morph (:forms (-vb) :pastpart w::worn :past W::wore)))
   (SENSES
    ((LF-PARENT ONT::STOP)
     (example "the effect wore off")
     (TEMPL affected-TEMPL)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::wear
   (wordfeats (W::morph (:forms (-vb) :past W::wore)))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090401 :change-date 20090910 :comments nil)
     (EXAMPLE "he was wearing a hat")
     (LF-PARENT ONT::wear)
     (TEMPL agent-neutral-XP-TEMPL)
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  ((W::wear (w::out))
   (wordfeats (W::morph (:forms (-vb) :past W::wore :pastpart W::worn :ing W::wearing)))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090511 :comments LM-vocab)
     (LF-PARENT ONT::cause-body-effect)
     (syntax (w::resultative +)) 
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     )
    )
   )
))

