;;;;
;;;; w::peek
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(w::peek
   (wordfeats (W::morph (:forms (-vb) :nom W::peek)))
 (senses
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("peer-30.3") :wn ("peek%2:39:00" "peek%2:39:02"))
     (LF-PARENT ONT::scrutiny)
     (TEMPL agent-templ) ; like look
     (PREFERENCE 0.96)
     )
  ((EXAMPLE "peek at something")
   (LF-PARENT ONT::scrutiny)
   (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
   (TEMPL agent-neutral-XP-TEMPL  (xp (% W::pp (W::ptype W::at))))
   (meta-data :origin fruitcarts :entry-date 20060215 :change-date nil :comments nil)
   )
  )
 )
))

