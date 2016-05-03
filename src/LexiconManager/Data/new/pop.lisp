;;;;
;;;; w::pop
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::pop w::up W::MENU)
    (wordfeats (W::MORPH (:forms (-S-3P) :plur (w::pop w::up w::menus))))
   (SENSES
    ((LF-PARENT ONT::symbolic-representation) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN plow :ENTRY-DATE 20050310 :CHANGE-DATE NIL
      :COMMENTS nil))))
))

(define-words :pos W::n
 :words (
  (w::pop
  (senses
	   ((LF-PARENT ONT::SODA)
	    (TEMPL COUNT-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::pop
   (SENSES
    ((LF-PARENT ONT::protocol)
     (EXAMPLE "pop is the post office protocol")
     (meta-data :origin task-learning :entry-date 20050824 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::pop
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::PUT)
     (SEM (F::cause F::agentive) (F::aspect F::bounded) (F::time-span F::atomic))
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

