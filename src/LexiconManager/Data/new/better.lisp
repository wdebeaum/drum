;;;;
;;;; w::better
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (w::better
   (wordfeats (W::morph (:forms NIL))
	      (w::contraction +) ;; MD -- this is not quite a contraction, but we mark it so that it does not go into nocomp rules and other rules that disallow contractions
	      )
   (SENSES
    ((LF-PARENT ONT::should)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (TEMPL COND-PRES-TEMPL)
     (example "he better hire him")
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     (preference .97)
     )
    ((LF-PARENT ONT::should)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (example "he better")
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     (SYNTAX (W::VFORM W::PRES) (W::changesem +))
     (preference .97)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
    (W::BETTER
   (wordfeats (W::COMPARATIVE +)); (W::FUNCTN ONT::ACCEPTABILITY-VAL))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("better%3:00:00"))
     (LF-PARENT ONT::MORE-VAL)
     (lf-form w::good)
     (TEMPL COMPAR-TEMPL)
     (SEM (f::orientation ont::more) (f::intensity ont::med) (F::SCALE ONT::ACCEPTABILITY-VAL))
     )
    )
   )
))

