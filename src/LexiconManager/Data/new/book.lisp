;;;;
;;;; W::BOOK
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::BOOK
   (SENSES
    ((LF-PARENT ONT::book)
;     (templ count-pred-subcat-originator-optional-templ)
     (TEMPL COUNT-PRED-TEMPL)
     (example "a book by hemingway")
     (meta-data :origin calo :entry-date 20040716 :change-date 20070517 :wn ("book%1:06:00" "book%1:10:00") :comments y2)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::book
   (senses
    ((lf-parent ont::reserve)
     (templ agent-affected-xp-templ)
     (example "please book a room for me")
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :comments nil :vn ("get-13.5.1") :wn ("book%2:41:01"))
     )
    )
   ))
)
