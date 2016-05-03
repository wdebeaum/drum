;;;;
;;;; W::SIMPLE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::SIMPLE
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090821 :wn ("simple%3:00:02" "simple%5:00:00:easy:01") :comments html-purchasing-corpus)
     (LF-PARENT ONT::easy)
     (TEMPL adj-content-affected-optional-xp-templ)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090821 :comments nil :wn ("simple%3:00:02" "simple%5:00:00:easy:01"))
     (EXAMPLE "it's simple to do")
     (LF-PARENT ONT::easy)
     (TEMPL adj-expletive-content-xp-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

