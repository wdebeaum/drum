;;;;
;;;; W::rom
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::rom
   (SENSES
    ((meta-data :origin calo :entry-date 20031201 :change-date nil :wn ("rom%1:06:00") :comments caloy2)
     (LF-PARENT ont::internal-computer-storage)
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::ROM
   (SENSES
    ((LF-PARENT ont::internal-computer-storage) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("rom%1:06:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

