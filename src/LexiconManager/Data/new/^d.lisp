;;;;
;;;; w::^d
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((w::^d w::better)   
   (wordfeats (W::morph (:forms NIL))
	      (w::contraction +)
	      )
   (SENSES
    ((LF-PARENT ONT::should)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (TEMPL COND-PRES-TEMPL)
     (example "he'd better hire him")
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     )
    ((LF-PARENT ONT::should)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (example "he'd better")
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     (SYNTAX (W::VFORM W::PRES) (W::changesem +))
     )
    )
   )
))

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::^d
   (wordfeats (W::morph (:forms NIL)) 
	      (W::agr (? vf W::1s W::2s W::3s W::1p W::2p W::3p))
	      (w::contraction +)
	      )
   (SENSES
    ;;;; I'd like to be under the sea
    ((LF-PARENT ONT::CONDITIONAL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::would)
     (TEMPL COND-PRES-TEMPL)
     )
    ;;;; I'd have been under the sea
    ((LF-PARENT ONT::CONDITIONAL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::would)
     (TEMPL COND-PAST-TEMPL)
     )
    ;;;; swift 03/21/02
    ;;;; auxiliary have in perfect construction -- I/you/she/we/they'd gone
    ((LF-PARENT ONT::PERFECTIVE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (TEMPL PERFECTIVE-TEMPL)
     (SYNTAX (W::vform W::past))
     )
    )
   )
))

