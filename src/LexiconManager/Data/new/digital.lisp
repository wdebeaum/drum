;;;;
;;;; w::digital
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((w::digital w::video w::disk)
    (wordfeats (W::morph (:forms (-S-3P) :plur (W::digital w::video w::disks))))
   (SENSES
    ( (meta-data :origin calo :entry-date 20030605 :change-date nil :comments calo-y1script)
     (LF-PARENT ont::data-storage-medium)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ;; what about the digital age, the digital divide?
  (W::digital
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20041119 :change-date nil :wn ("digital%3:00:00") :comments caloy2)
     (lf-parent ont::digital-val)
     (example "a digital circuit")
     )
    )
   )
))

