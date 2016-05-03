;;;;
;;;; W::list
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::list
   (SENSES
    ((meta-data :origin calo :entry-date 20040107 :change-date 20070521 :wn ("list%1:10:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::list)
     (example "here is the list of results")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::list
   (SENSES
    ((example "why isn't there a Mac listed")
     (meta-data :origin calo :entry-date 20031206 :change-date nil :comments calo-y1script)
     (LF-PARENT ONT::listing)
     )
    )
   )
))

