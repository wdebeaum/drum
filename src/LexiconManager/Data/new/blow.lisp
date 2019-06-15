;;;;
;;;; W::blow
;;;;

(define-words :pos W::v 
 :words (
   ((W::blow w::up)
     (wordfeats (W::morph (:forms (-vb) :past W::blew :pastpart w::blown)))
    (SENSES
     ((meta-data :origin step :entry-date 20080705 :change-date 20090504 :comments step5)
      (EXAMPLE "the bomb blew up")
      (LF-PARENT ONT::explode)
      (TEMPL affected-TEMPL)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      )
     )
    )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::blow
   (wordfeats (W::morph (:forms (-vb) :past W::blew :pastpart W::blown :ing W::blowing)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("blow%2:43:00"))
     (LF-PARENT ONT::atmospheric-event)
     (example "it blew")
     (TEMPL expletive-templ) ; like rain
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("blow%2:43:00"))
     (LF-PARENT ONT::blow)
     (example "he blew (on the fire)")
     (TEMPL agent-templ) 
     )
    )
   )
))

