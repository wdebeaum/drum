
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((w::high w::school)
   (SENSES
    ((LF-PARENT ONT::academic-institution)
     (EXAMPLE "send email from school")
     (meta-data :origin asma :entry-date 20111003 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((W::high w::end)
    (SENSES
     ((meta-data :origin calo :entry-date 20041116 :change-date 20061106 :comments caloy2 :comlex nil)
      (example "a good book")
      (lf-parent ont::superior-val)
      (TEMPL central-adj-templ)
      )
     )
    )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::high
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("high%3:00:01"))
     (EXAMPLE "a high mountain" "a five foot high building")
     (LF-PARENT ONT::high-val)
     )
 #||   ((EXAMPLE "I need a higher resolution")
     (LF-PARENT ONT::intense)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation ont::more))
     (TEMPL LESS-ADJ-TEMPL)
     (meta-data :origin boudreaux :entry-date 20031024 :change-date 20090731 :wn ("high%3:00:02") :comments nil)
     )||#
    )
   )
))

