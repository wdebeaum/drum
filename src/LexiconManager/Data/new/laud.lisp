;;;;
;;;; W::laud
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::laud
   (SENSES
     ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::praise)
     (example "he lauded it" "it was lauded by him")
     (TEMPL agent-theme-xp-templ) 
     )
     ((meta-data :origin ptb :entry-date 2010617 :change-date nil :comments nil)
      (LF-PARENT ONT::praise)
      (example "the company lauded the innovation as a new way of making money")
      (TEMPL agent-neutral-as-theme-templ)
      )
    )
   )
))

