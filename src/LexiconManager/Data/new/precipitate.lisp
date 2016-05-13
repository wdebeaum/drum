;;;;
;;;; W::precipitate
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::precipitate
   (wordfeats (W::morph (:forms (-vb) :nom w::precipitation)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("precipitate%2:43:00"))
     (LF-PARENT ont::precipitating)
     (TEMPL expletive-templ) ; like rain
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (W::precipitate
   (SENSES
    (
     (LF-PARENT ONT::parts-removed)
     (TEMPL COUNT-PRED-TEMPL)
     (EXAMPLE "The precipitate of these molecules")
     )
    )
   )
))


(define-words :pos W::v 
 :words (
  (W::precipitate
   (wordfeats (W::morph (:forms (-vb) :nom w::precipitation :nomsubjpreps (w::of) :nomobjpreps (w::with))))
   (SENSES
    (
     (LF-PARENT ONT::parts-removed)
     (TEMPL AFFECTED-affected-as-comp-TEMPL (xp (% W::pp (W::ptype (? xx w::with)))))
     (EXAMPLE "This protein co-immunoprecipitated with that protein.")
     )
    (
     (LF-PARENT ONT::parts-removed)
     (TEMPL AGENT-AFFECTED-XP-TEMPL)
     (EXAMPLE "We immunoprecipitated these proteins.")
     )
    (
     (LF-PARENT ONT::parts-removed)
     (TEMPL AFFECTED-TEMPL)
     (EXAMPLE "These proteins coprecipitated.")
     )
    )
   )
))
