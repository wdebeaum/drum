;;;;
;;;; W::known
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ;; later- derive from verb
   (W::known
   (SENSES
    ((meta-data :origin cernl :entry-date 20100501 :change-date nil :comments nil)
     (EXAMPLE "a well-known person")
     (lf-parent ont::familiar-val)
     (TEMPL central-adj-TEMPL)
     )
    ((meta-data :origin cernl :entry-date 20100501 :change-date nil :comments nil)
     (EXAMPLE "a person well-known to him")
     (lf-parent ont::familiar-val)
     (TEMPL adj-affected-xp-TEMPL (xp (% PP (w::ptype w::to))))
     )
    )
   )
))

;;   This is here top support the expletive reading whoich doesn't work for the verb "KNOW"
(define-words :pos W::v
  :words (
	  (W::known
	   (wordfeats (W::VFORM W::PASSIVE) (W::MORPH (:forms NIL)))
	   (SENSES
	    ((EXAMPLE "It is known (that)...")
	     (LF-PARENT ONT::know)
	     (TEMPL EXPLETIVE-FORMAL-1-XP1-2-XP2-TEMPL )
	     )
	    )
	   ))
  )
