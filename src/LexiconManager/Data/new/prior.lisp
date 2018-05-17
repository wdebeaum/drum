;;;;
;;;; W::PRIOR
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::PRIOR
   (SENSES
    ((meta-data :origin calo :entry-date 20040414 :change-date nil :wn ("prior%5:00:00:antecedent:00") :comments caloy1v6)
     (lf-parent ont::previous-val)
     (TEMPL attributive-only-adj-templ)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::PRIOR w::to)
   (SENSES
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL binary-constraint-S-ing-TEMPL)
     (example "prior to leaving she checked her watch")
     )
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL binary-constraint-S-TEMPL)
     (example "prior to the meeting she checked her watch")
     )
    ((LF-PARENT ONT::event-time-rel)
     (meta-data :origin cernl :entry-date 20110223 :change-date nil :comments bionlp)
      (example "show me departures prior to 5 pm / the meeting")
     (TEMPL binary-constraint-np-TEMPL)
     )
    #|
    ((LF-PARENT ONT::event-time-rel)
     (TEMPL BINARY-CONSTRAINT-adj-postpos-TEMPL)
     (meta-data :origin cernl :entry-date 20110223 :change-date nil :comments bionlp)
     (example "the device placed prior to that")
     (preference .98)
     )
    |#
    )
   )
))

