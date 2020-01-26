;;;;
;;;; w::read
;;;; 

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::read w::only w::memory)
     (wordfeats (W::morph (:forms (-S-3P) :plur (W::read w::only W::memories))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031201 :change-date nil :comments caloy2)
     (LF-PARENT ont::internal-computer-storage)
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
(W::read
   (wordfeats (W::morph (:forms (-vb) :past W::read :ing W::reading)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date 20090508 :comments html-purchasing-corpus :wn ("read%2:31:00" "read%2:31:04""read%2:31:09""read%2:31:01" ))
     (EXAMPLE "read the letter")
     (LF-PARENT ONT::read)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-TEMPL)
     )
    ((LF-PARENT ONT::read)
     (meta-data :origin calo :entry-date 20031230 :change-date 20090508 :comments html-purchasing-corpus :wn ("read%2:31:00" "read%2:31:04""read%2:31:09""read%2:31:01" ))
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-templ)
     )
    ((example "he read that the meeting was tomorrow")
     (meta-data :origin calo :entry-date 20031230 :change-date 20090508 :comments html-purchasing-corpus :wn ("read%2:31:00" "read%2:31:04""read%2:31:09""read%2:31:01" ))
     (LF-PARENT ONT::read)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    
    )
   )
))

(define-words :pos W::adj
 :words (
  ((w::read w::only)
  (senses
   ((lf-parent ont::r-able-val)
    (TEMPL central-adj-templ)
    (meta-data :origin calo :entry-date 20060824 :change-date nil  :wn ("readable%5:00:00:legible:00") :comments nil)
    )
   )
)
))

