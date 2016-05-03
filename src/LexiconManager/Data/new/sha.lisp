;;;;
;;;; W::sha
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::sha
   (wordfeats (w::contraction +))    
   (SENSES
    ;;;; I shan't drive a truck
    ((LF-PARENT ONT::FUTURE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::shall)
     (TEMPL AUX-FUTURE-TEMPL)
     (SYNTAX (W::VFORM W::FUT)
	     )
     )
    )
   )
))

