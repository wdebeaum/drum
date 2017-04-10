;;;;
;;;; W::LOOSE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::LOOSE
   (wordfeats (W::MORPH (:FORMS (-ly -ER))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("loose%3:00:01") :comments html-purchasing-corpus)
     (EXAMPLE "a tight fit")
     (LF-PARENT ONT::LOOSE-VAL)
     )
    ((LF-PARENT ont::free-val)
     (example "dogs are loose on the streets")
     (meta-data :origin adjective reorganization :entry-date 20170317 :change-date nil :comments nil)
     )
    ((LF-PARENT ont::not-affixed-val)
     (example "loose gravel")
     (meta-data :origin adjective reorganization :entry-date 20170317 :change-date nil :comments nil)
     )
    )
   )
))

