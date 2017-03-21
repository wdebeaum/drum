;;;;
;;;; w::push
;;;;

(define-words :pos W::n
 :words (
  ((w::push w::up)
  (senses
   ((LF-PARENT ONT::working-out)
    (meta-data :origin cardiac :entry-date 20090129 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::push
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("59-force"))
     (LF-PARENT ont::provoke)
     (TEMPL AGENT-EFFECT-AFFECTED-OBJCONTROL-TEMPL)  ; like dare
     (example "he pushed him to do it")
     (PREFERENCE 0.96)
     )
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::PUSH)
     (SEM (F::cause F::agentive) (F::aspect F::unbounded) (F::time-span F::extended))
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

