;;;;
;;;; W::should
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::should
   (SENSES
    ;;;; I should drive a truck
    ((LF-PARENT ONT::should)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::should)
     (TEMPL COND-PRES-TEMPL)
     )
    ;;;; I should have driven a truck
    ((LF-PARENT ONT::should)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::should)
     (TEMPL COND-PAST-TEMPL)
     )
    ((LF-PARENT ONT::should)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::should)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::VFORM W::PRES) (W::changesem +)
	     )
     )
    )
   )
))

