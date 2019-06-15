;;;;
;;;; w::had
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((w::had w::better)
   (wordfeats (W::morph (:forms NIL)))
   (SENSES
    ((LF-PARENT ONT::should)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (TEMPL COND-PRES-TEMPL)
     (example "he had better hire him")
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     )
    ((LF-PARENT ONT::should)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (example "he had better")
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     (SYNTAX (W::VFORM W::PRES) (W::changesem +))
     )
    )
   )
))

