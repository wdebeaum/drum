;;;;
;;;; W::lay
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::lay
   (wordfeats (W::morph (:forms (-vb) :past W::laid :pastpart W::laid :ing W::laying)))
   (SENSES
    ((EXAMPLE "You had better lie down")
     (LF-PARENT ONT::BODY-MOVEMENT-PLACE)
          (LF-FORM W::lie)
     ;; need this sense for sloppy speakers
     (TEMPL AGENT-TEMPL)
     (preference .98)
     )
   #| ((EXAMPLE "lay down your weapons")
     (LF-PARENT ONT::PUT)
     ) |#
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::lay
   (wordfeats (W::morph (:forms (-vb) :past W::laid :pastpart W::laid :ing W::laying)))
   (SENSES
    ((EXAMPLE "lay an egg on the table")
     (EXAMPLE "lay down your weapons")
     (LF-PARENT ONT::PUT)
     )
    )
   )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  ((W::lay (w::off))
   (wordfeats (W::morph (:forms (-vb) :past W::laid)))
   (SENSES
    ((LF-PARENT ONT::terminate)
     (example "Abrams laid off a programmer")
     (meta-data :origin csli-ts :entry-date 20070313 :change-date 20090508 :comments nil :wn nil)
     )
    )
   )
))

