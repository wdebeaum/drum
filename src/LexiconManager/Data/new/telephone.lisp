;;;;
;;;; W::telephone
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::telephone
   (SENSES
    ((LF-PARENT ONT::device)
     (SEM (F::mobility F::non-self-moving))
     (meta-data :origin calo :entry-date 20041206 :change-date nil :wn ("telephone%1:06:00") :comments caloy2)
     (example "this company offers telephone support")
;     (preference .98) ;; prefer compounds for plot
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::telephone
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060606 :change-date 20090508 :comments nil :vn ("instr_communication-37.4") :wn ("telephone%2:32:00"))
     (EXAMPLE "telephone the doctor")
     (LF-PARENT ONT::establish-communication)
     (TEMPL AGENT-AGENT1-NP-TEMPL)
     (preference .97) ;; prefer noun sense
     )
    )
   )
))

