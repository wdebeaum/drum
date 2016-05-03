;;;;
;;;; W::title
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::title
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((EXAMPLE "what is the title of the book")
     (meta-data :origin calo :entry-date 20040622 :change-date nil :wn ("title%1:10:01" "title%1:10:00") :comments y2)
     (LF-PARENT ONT::TITLE)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

