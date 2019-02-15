;;;;
;;;; W::ease
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
  (W::ease
   (SENSES
    ((meta-data :origin "wordnet-3.0" :entry-date 20090512 :change-date nil :comments nil)
     (LF-PARENT ONT::evoke-calm)
     (example "The news eased my conscience")
     (TEMPL agent-affected-xp-templ)
     )
    ((meta-data :origin "wordnet-3.0" :entry-date 20090515 :change-date nil :comments nil)
     (LF-PARENT ONT::evoke-comfort)
     (example "Ease the pain in your legs")
     (TEMPL agent-affected-xp-templ)
     )
    ((meta-data :origin motion-reorganization :entry-date 20190117 :change-date nil :comments nil)
     (LF-PARENT ONT::facilitate)
     (example "You can ease the process by sharing your knowledge.")
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::EASE
   (SENSES
    ((meta-data :origin domain-reorganization :entry-date 20170810 :change-date nil :comments nil :wn ("ease%1:07:00"))
     (LF-PARENT ONT::easy-scale)
     (TEMPL other-reln-TEMPL)
     )
    )
   )
))