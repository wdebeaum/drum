;;;;
;;;; W::CARD
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CARD
   (SENSES
    ((meta-data :origin calo :entry-date 20030605 :change-date nil :wn ("card%1:06:01") :comments calo-y1script)
     (LF-PARENT ONT::COMPUTER-CARD)
     (preference .98)
     )
    ((meta-data :origin calo :entry-date 20040607 :change-date  20060222  :comments y1v5)
     (LF-PARENT ONT::credit-card)
     (templ other-reln-templ)
     (preference .98)
     )
    ((meta-data :origin asma :entry-date 20111004)
     (LF-PARENT ONT::card)
     (example "playing cards")
     )
    )
   )
))

