;;;;
;;;; W::come
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::come
   (wordfeats (W::morph (:forms (-vb) :past W::came :pastpart W::come)))
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date nil :comments nil :vn ("appear-48.1.1") :wn ("come%2:30:01" "come%2:42:13"))
     (LF-PARENT ONT::occurring)
     (TEMPL neutral-templ )
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("escape-51.1-2"))
     (LF-PARENT ONT::COME)
     (example "the cargo/truck came yesterday")
     (SEM (F::trajectory +) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL affected-TEMPL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (PREFERENCE 0.98)
     )
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s15)
     (lf-parent ont::become)
     (example "Her wish came true")
     (templ affected-pred-templ))
    ))))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 ((W::come (w::to))
    (wordfeats (W::morph (:forms (-vb) :past W::came :pastpart W::come :ing W::coming)))
   (SENSES
     ((LF-PARENT ONT::reviving)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ affected-templ)
     (example "he came to")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 ((W::come w::out)
    (wordfeats (W::morph (:forms (-vb) :past W::came :pastpart W::come :ing W::coming)))
   (SENSES
     ((example "the truth came out")
     (LF-PARENT ONT::appear)
     (meta-data :wn ("come_out%2:32:00"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (templ affected-templ)
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  (w::coming
  (senses
   ((LF-PARENT ONT::event-time-rel)
    (TEMPL central-adj-templ)
    (meta-data :origin calo :entry-date 20060824 :change-date nil  :wn ("readable%5:00:00:legible:00") :comments nil)
    )
   )
)
))
