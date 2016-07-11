;;;;
;;;; W::^ve
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::^ve
   (wordfeats (W::morph (:forms NIL)) (W::vform W::pres) 
	      (W::agr (? vf W::1s W::2s W::1p W::2p W::3p))
	      (w::contraction +)
   )
   (SENSES
    ;;;; auxiliary have in perfect construction
    ((LF-PARENT ONT::PERFECTIVE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM ONT::have)
     (TEMPL PERFECTIVE-TEMPL)
     (example "they've gone")
     (SYNTAX (W::auxname W::perf) (w::changesem +))
     )
    )
   )
))

