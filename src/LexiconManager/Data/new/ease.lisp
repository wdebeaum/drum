;;;;
;;;; W::ease
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::ease
   (SENSES
    ((meta-data :origin "wordnet-3.0" :entry-date 20090512 :change-date nil :comments nil)
     (LF-PARENT ONT::evoke-relief)
     (example "The news eased my conscience" "Ease the pain in your legs")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((meta-data :origin motion-reorganization :entry-date 20190117 :change-date nil :comments nil)
     (LF-PARENT ONT::facilitate)
     (example "You can ease the process by sharing your knowledge.")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
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