;;;;
;;;; w::markup
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  ((w::markup w::language)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::markup W::languages))))
   (SENSES
    ((LF-PARENT ONT::computer-language)
     (EXAMPLE "Hypertext Markup Language, a language used for creating documents for the World Wide Web")
     (meta-data :origin task-learning :entry-date 20050923 :change-date nil :wn ("markup_language%1:10:00") :comments nil)
     )
    )
   )
))

