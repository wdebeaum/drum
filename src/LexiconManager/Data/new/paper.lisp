;;;;
;;;; W::paper
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::paper w::clip)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::paper W::clips))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("paper_clip%1:06:00") :comments Office)
     (LF-PARENT ONT::device)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::PAPER
   (SENSES
    ((LF-PARENT ONT::substance) (TEMPL mass-PRED-TEMPL)
     (example "a paper airplane")
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("paper%1:27:00")
      :COMMENTS HTML-PURCHASING-CORPUS))
    ((LF-PARENT ONT::info-medium) ;; like newspaper
     (EXAMPLE "i read it in the paper")
     (meta-data :origin calo-ontology :entry-date 20060423 :change-date nil :wn ("paper%1:10:03") :comments nil)
     )
    ))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::PAPER
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :wn ("paper%1:27:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::material)
     (TEMPL MASS-PRED-TEMPL)
     )
    ((meta-data :origin calo :entry-date 20050325 :change-date nil :comments caloy2)
     (LF-PARENT ONT::info-medium)
     (example "they wrote a paper about it")
     )
    )
   )
))

