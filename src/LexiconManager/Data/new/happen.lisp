;;;;
;;;; W::happen
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::happen
   (wordfeats (W::morph (:forms (-vb) :past W::happened :ing W::happening :nom w::happening)))
   (SENSES    ;;;; swier -- shit happens. (Ha!)
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("occurrence-48.3") :wn ("happen%2:30:00" "happen%2:30:01"))
     (LF-PARENT ONT::occurring)
     ;;(SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL neutral-TEMPL)
     (SYNTAX (w::exclude-passive +))
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("occurrence-48.3") :wn ("happen%2:30:00" "happen%2:30:01"))
     (LF-PARENT ONT::undergo-action)
     (EXAMPLE "what happened to you")
     ;;(SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL neutral-affected-xp-TEMPL)
     (SYNTAX (w::exclude-passive +))
     )
    ((LF-PARENT ONT::HAPPEN)
     (example "it happened [to be true]")
     ;;(SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL neutral-theme-subjcontrol-templ  (xp (% W::cp (W::ctype W::s-to))))
     (SYNTAX (w::exclude-passive +))
     )
    )
   )
))

