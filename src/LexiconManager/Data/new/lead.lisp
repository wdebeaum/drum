;;;;
;;;; w::lead
;;;;

(define-words :pos w::N 
 :tags (:base500)
 :words (
;; Various electrical parts
  (w::lead
  (senses((LF-parent ONT::Device-component) 
	    (templ part-of-reln-templ)
	    (meta-data :origin bee :entry-date 20040407 :change-date 20040607 :comments (test-s portability-experiment))
	    ))
  ;; Myrosia added terminal and holder for portability experiment
)
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
  (W::lead
   (wordfeats (W::morph (:forms (-vb) :past W::led :ing W::leading :nom w::lead)))
   (SENSES
    ((meta-data :origin step :entry-date 20080724 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::cause-effect)
     (example "it has led him to find a solution")
     (templ agent-effect-affected-objcontrol-templ)
     )
    ((meta-data :origin step :entry-date 20080724 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::cause-effect)
     (example "it led to disaster")
     ;(templ agent-effect-xp-templ (xp (% w::PP (w::ptype w::to))))
     (TEMPL agent-affected-xp-templ (xp (% w::PP (w::ptype w::to))))
     )
    ((meta-data :origin coordops :entry-date 20070511 :change-date nil :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::guiding)
     (example "team alpha will lead the activity")
     (TEMPL agent-affected-xp-templ)
     )
    ((meta-data :origin coordops :entry-date 20070511 :change-date nil :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::guiding)
     (example "team alpha will lead")
     (TEMPL agent-templ)
     )
    )
   )
))

