;;;;
;;;; W::SPELL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::SPELL
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("spell%1:28:01"))
     (LF-PARENT ONT::time-interval)
     (TEMPL other-reln-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
 (W::spell
  (SENSES
    ((meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil)
     (LF-PARENT ONT::EVENT)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (w::spell
   (wordfeats (W::morph (:forms (-vb) :nom w::spelling)))
   (senses
    ((lf-parent ont::locution)
     (example "spell the word for me")
     (templ agent-theme-affected-optional-templ (xp1 (% w::NP)) (xp2 (% w::pp (w::ptype w::for))))	     
     (meta-data :origin task-learning :entry-date 20050815 :change-date nil :comments nil)
    )
   ))
))

