;;;;
;;;; W::backpack
;;;;

(define-words :pos w::N :templ count-pred-templ
 :words (
;; allison's additions for calo
  (W::backpack
   (SENSES
    ((meta-data :origin allison :entry-date 20041114 :change-date nil :wn ("backpack%1:06:00") :comments caloy2 :wn-sense (1))
     (LF-PARENT ONT::small-container)
     (TEMPL count-pred-templ)
     (example "they fit in your backpack")
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::backpack
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("backpack%2:38:00"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

