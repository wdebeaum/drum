;;;;
;;;; W::run
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :tags (:base500)
 :words (
  (W::run
   (wordfeats (W::morph (:forms (-vb) :ing W::running :past W::ran :pastpart W::run)))
   (SENSES
    ;;;; swier -- missing sense "Let's run through the plan" Should get LF_REVIEW or something
    ;;;; swier -- also need "I-90 runs through Boston." LF_INTERSECT maybe?
    ;;;; swier -- also need "run into" which is certainly LF_INTERSECT.
    ;;;; swier -- added word (run through), below.
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("run%2:38:00" "run%2:38:04" "run%2:38:05" "run%2:38:10"))
     (LF-PARENT ONT::move-rapidly)
     (example "he runs in the park every day")
     (TEMPL AGENT-TEMPL)
     )

    ((LF-PARENT ONT::FUNCTION)
     (example "a computer running at 2 point 4 ghz")
     (SEM (F::Time-span F::extended))
     (TEMPL neutral-extent-xp-templ (xp (% W::pp (W::ptype W::at))))
     )
    
    ((LF-PARENT ONT::execute)
     (example "run the script")
     (templ agent-neutral-xp-templ)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::extended))
     )
    
    (;;(LF-PARENT ONT::managing)
     (lf-parent  ont::manage) ;; 20120521 GUM change new parent 
     (example "the piston runs the drive shaft")
     (templ agent-affected-xp-templ)
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil)
     (SEM (F::Aspect F::dynamic) (F::Time-span F::extended))
     )
    )
   )
))

