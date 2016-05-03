;;;;
;;;; W::CENTER
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::CENTER
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::CENTER)
     (TEMPL PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::center
   (wordfeats (W::morph (:forms (-vb) :ing w::centering :past W::centered)))   
   (SENSES
;    ((LF-PARENT ONT::computer-action)
;     (SEM (F::Aspect F::bounded) (F::Time-span F::Atomic))
;     (TEMPL AGENT-TEMPL)
;     (meta-data :origin ralf :entry-date 20040621 :change-date nil :comments ralf.txt)
;     (example "center on Atlanta")
;     (preference 0.98)
;     )
    ((LF-PARENT ONT::arranging)
     (SEM (F::Aspect F::bounded) (F::Time-span F::Atomic))
     (TEMPL AGENT-THEME-XP-TEMPL)
     (meta-data :origin task-learning :entry-date 20050923 :change-date 20090507 :comments nil)
     (example "center the image within the window")
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
   (w::center
     (senses
      ((lf-parent ont::location-val)
       (meta-data :origin step :entry-date 20080705 :change-date nil :comments step5)
       )
      ))
))

