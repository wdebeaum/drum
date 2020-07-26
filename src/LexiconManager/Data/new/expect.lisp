;;;;
;;;; W::expect
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::expect
   (wordfeats (W::morph (:forms (-vb) :nom w::expectation)))
   (SENSES
    ((LF-PARENT ONT::EXPECTATION)
     (example "what side effects should I expect")
     (TEMPL experiencer-neutral-xp-templ)
     )
    ((LF-PARENT ONT::expectation)
     (example "he expects to have a degree next year")
     (meta-data :origin calo :entry-date 20040907 :change-date nil :comments caloy2 :vn ("wish-62") :wn ("expect%2:31:00"))
     (TEMPL EXPERIENCER-FORMAL-SUBJCONTROL-TEMPL)
     )
    ((LF-PARENT ONT::EXPECTATION)
     (example "she expects him to have a degree next year")
     (meta-data :origin calo :entry-date 20040907 :change-date nil :comments caloy2 :vn ("wish-62")  :wn ("expect%2:31:00"))
     (TEMPL EXPERIENCER-NEUTRAL-FORMAL-CP-OBJCONTROL-A-TEMPL)
     )
    ((meta-data :origin calo-ontology :entry-date 20060315 :change-date nil :comments nil  :wn ("expect%2:31:00"))
     (LF-PARENT ONT::expectation)
     (example "he expects that he will do it")
     (TEMPL experiencer-formal-xp-templ (xp (% w::cp (w::ctype (? cp w::s-finite)))))
     )
    )
   )
))

(define-words :pos W::v
  :tags (:base500)
  :words (
	  (W::expected
	   (wordfeats (W::VFORM W::PASSIVE) (W::MORPH (:forms NIL)))
	   (SENSES
	    ((EXAMPLE "It is expected (that)...")
	     (LF-PARENT ONT::expectation)
	     ;(TEMPL EXPLETIVE-FORMAL-TEMPL (xp1 (% W::NP (W::lex W::it))) )
	     (TEMPL EXPLETIVE-FORMAL-1-XP1-2-XP2-TEMPL )
	     )
	    )
	   ))
  )
