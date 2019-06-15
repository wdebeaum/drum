;;;;
;;;; W::PROGRAM
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  (W::PROGRAM
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date 20050816 :comments lf-restructuring)
     (LF-PARENT ONT::algorithm)
     )
;    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus :wn ("program%1:04:00"))
;     (example "execute the program")
;     (LF-PARENT ONT::ACTION)
;     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::program
   (wordfeats (W::morph (:forms (-vb) :nom W::program)))
   (SENSES
    ((LF-PARENT ONT::planning)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-NEUTRAL-FORMAL-CP-OBJCONTROL-TEMPL)
     (example "program the vcr to start at 10 pm")
     (meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090515 :comments nil :vn ("create-26.4-1"))
     )
    )
   )
))

