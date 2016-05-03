;;;;
;;;; W::MAY
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::MAY
   (SENSES
    ;;;; I may drive a truck
    ((LF-PARENT ONT::POSSIBILITY)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::may)
     (TEMPL COND-PRES-TEMPL)
     )
    ;;;; I may have driven a truck
    ((LF-PARENT ONT::POSSIBILITY)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::may)
     (TEMPL COND-PAST-TEMPL)
     )
    ((LF-PARENT ONT::POSSIBILITY)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::may)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::VFORM W::PRES) (W::changesem +))
     )
    )
   )
))

(define-words :pos W::name :boost-word t
 :words (
  (W::MAY
  (senses
   ((LF-PARENT ONT::MONTH-NAME)
;    (TEMPL value-templ)
    (templ nname-templ)
    )
   )
)
))

(define-words :pos W::name :boost-word t
 :tags (:base500)
 :words (
  (W::may
  (senses
   ((LF-PARENT ONT::MONTH-NAME)
;    (TEMPL value-templ)
    (templ nname-templ)
    )
   )
)
))

(define-words :pos W::name :boost-word t
 :words (
  ((W::may  w::punc-period)
  (senses
   ((LF-PARENT ONT::MONTH-NAME)
;    (TEMPL value-templ)
    (templ nname-templ)
    )
   )
)
))

