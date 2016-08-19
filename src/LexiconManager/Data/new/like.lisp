;;;;
;;;; W::LIKE
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::LIKE
   (SENSES
    ;;;; NOTE: This is the only sense of "LIKE" meaning "want"
    ;;;; Swier -- Having just crossed Death Valley, I would like to have some water.
    ((LF-PARENT ONT::want)
     (SEM (F::Aspect F::indiv-level))
     (TEMPL experiencer-action-SUBJCONTROL-TEMPL)
     (example "I would like to have some water")
     )
    ((LF-PARENT ONT::appreciate)
     (SEM (F::Aspect F::indiv-level))
     (meta-data :origin cardiac :entry-date 20081005 :change-date 200905008 :comments nil)
     (TEMPL neutral-action-SUBJCONTROL-TEMPL  (xp (% W::VP (W::vform W::ing))))
     (example "He likes drinking water after a hike")
     )
    ((LF-PARENT ONT::appreciate)
     (SEM (F::Aspect F::indiv-level))
     (example "I like oranges")
     (TEMPL experiencer-neutral-xp-templ)
     )    
    ((LF-PARENT ONT::want)
     (SEM (F::Aspect F::indiv-level))
     (example "I like him to dance")
     (TEMPL experiencer-action-objcontrol-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::LIKE
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("like%3:00:00"))
     (EXAMPLE "it is like that")
     (LF-PARENT ONT::SIMILAR)
     (templ adj-co-theme-templ (xp (% w::np)))
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::LIKE
   (SENSES
    ((LF-PARENT ONT::similarity)
     (LF-FORM W::LIKE)
     ;; Myrosia put in a really low preference: don't put this in
     ;; unless there's no other possible interpretation
     (preference 0.92)
     (example "he solved it like this")
     (TEMPL binary-constraint-s-or-np-TEMPL)
     )
    ((LF-PARENT ONT::MODIFIER)
     (LF-FORM W::LIKE)
     (example "give me like 10 of them")
     (TEMPL NUMBER-OPERATOR-TEMPL)
     (preference .97)
     )
    )
   )
))

(define-words :pos w::adv
 :tags (:base500)
 :words (
;; Parentheticals
  (w::like
  (senses
	   ((TEMPL BINARY-CONSTRAINT-NP-TEMPL)
	    (LF-PARENT ONT::PARENTHETICAL)
	    (meta-data :origin beetle :entry-date 20090116 :change-date nil :comments nil)
	    (preference 0.98) ;; lower preference so that other senses are considered first, since this sense is very underconstrained
	    )
	   )
)
))

