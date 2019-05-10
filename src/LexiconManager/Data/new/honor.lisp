;;;;
;;;; W::honor
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::honor
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("honor%2:41:00" "honor%2:41:01"))
     (LF-PARENT ONT::praise)
     (TEMPL agent-addressee-templ) ; like thank
     )
    ;; prefer vn sense
    ((LF-PARENT ONT::praise)
     (TEMPL agent-theme-xp-templ)
     (EXAMPLE "honor the achievement")
     (meta-data :origin cardiac :entry-date 20090121 :change-date 20090508 :comments nil)
     )
    )
   )
))

#|

(define-words :pos W::V 
 :words (
  (W::honor
   (wordfeats (W::morph (:forms (-vb) :past W::honored :ing W::honoring :nom w::honor)))
   (SENSES
    ((meta-data :origin step :entry-date 20080630 :change-date 20090511 :comments nil)
     (LF-PARENT ONT::appreciate)
     (example "they honor him")
     (TEMPL experiencer-neutral-templ)
     )
    )
   )
))

|#