;;;;
;;;; W::e
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   ((W::e w::punc-period w::g w::punc-period)
   (SENSES
    ((LF-PARENT ONT::qualification)
     (example "e.g. if x=5, y=7")
     (TEMPL disc-pre-templ)
     (meta-data :origin lam :entry-date 20050422 :change-date nil :comments lam-initial)
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (W::E
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

