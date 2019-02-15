;;;;
;;;; w::darken
;;;;                                                                                                                                                                                                                                        


(define-words :pos W::V
  :templ agent-affected-xp-templ
 :words (
(w::darken
 (senses
  ((meta-data :origin cause-result-relations :entry-date 20190108 :change-date nil :comments nil)
   (LF-PARENT ONT::darken)
   (example "The curtains darkened the room.")
    )
    ((meta-data :origin cause-result-relations :entry-date 20190108 :change-date nil :comments nil)
     (EXAMPLE "The room darkened.")
     (LF-PARENT ONT::darken)
     (TEMPL affected-unaccusative-templ)
     )
  )
 )
))
