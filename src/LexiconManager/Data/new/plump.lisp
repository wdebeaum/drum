;;;;
;;;; w::plump
;;;; 

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(w::plump
 (senses
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments calo-y1script)
   (LF-PARENT ONT::swell)
   (example "plump the pillows")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-XP-TEMPL)
   )
  )
 )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
((w::plump (w::up))
 (senses
  ((meta-data :origin calo :entry-date 20040112 :change-date 20090504 :comments calo-y1script)
   (LF-PARENT ONT::swell)
   (example "plump up the pillows")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   (TEMPL AGENT-affected-XP-TEMPL)
   )
  )
 )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
    (W::plump
    (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090731 :comments LM-vocab)
     (lf-parent ont::fat-val)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation ont::more))
     (example "a plump cat" "a plump line")
     )
    )
   )
))

