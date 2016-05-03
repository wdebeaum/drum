;;;;
;;;; W::have
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::have
   (wordfeats (W::morph (:forms (-vb) :3s W::has :past W::had)))
   (SENSES
    ((EXAMPLE "I have to go")
     (LF-PARENT ONT::NECESSITY)
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-theme-subjcontrol-templ)
     ) 
    ((LF-PARENT ONT::HAVE)
     (example "I have a truck")
     (TEMPL neutral-neutral-xp-templ)
          )
  
    ((LF-PARENT ONT::CONSUME)
     (TEMPL AGENT-AFFECTED-XP-TEMPL)
     (meta-data :origin medadvisor :entry-date 20011121 :change-date nil :comments nil)
     (example "I like to have an antacid at bedtime")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (preference .98)
     )
    ((LF-PARENT ONT::MAKE-IT-SO)
     (meta-data :origin trains :entry-date unknown :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "have the trucks go there")
     (TEMPL agent-effect-affected-objcontrol-templ (xp (% W::VP (W::vform (? x W::base W::passive)))))
     )
    ((LF-PARENT ONT::have-experience)
     (TEMPL neutral-neutral-templ)
     (example "he has a headache")
     (meta-data :origin cardiac :entry-date 20080217 :change-date nil :comments nil)
     )
    ;;;; auxiliary have in perfect construction
    ((LF-PARENT ONT::PERFECTIVE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (TEMPL PERFECTIVE-TEMPL)
     (example "they have gone")
     (SYNTAX (W::auxname W::perf) (w::changesem +))
     )
    ;;;; auxiliary have in perfect construction with main V elided
    ((LF-PARENT ONT::PERFECTIVE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (TEMPL AUX-NOCOMP-TEMPL)
     (example "they have")
     (SYNTAX (W::auxname W::perf) (W::changesem +))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::have w::a w::good w::night)
   (SENSES
    ((LF (W::GOODBYE))
     (meta-data :origin asma :entry-date 20110921 :change-date nil :comments nil)
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_CLOSE))
     )
    )
   )
))

