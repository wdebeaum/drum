;;;;
;;;; W::DISK
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::DISK W::DRIVE)
    (wordfeats (W::morph (:forms (-S-3P) :plur (W::disk W::drives))))
   (SENSES
    ((meta-data :origin calo :entry-date 20030605 :change-date nil :wn ("disk_drive%1:06:00") :comments calo-y1script)
     (LF-PARENT ont::io-device)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::DISK W::SPACE)
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((meta-data :origin calo :entry-date 20040421 :change-date nil :wn ("disk_space%1:15:00") :comments calo-y1script)
     (LF-PARENT ont::internal-computer-storage)
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::DISK
   (SENSES
    ((LF-PARENT ONT::internal-computer-storage) (TEMPL COUNT-PRED-TEMPL)
     (PREFERENCE 0.96) ; prefer compound if it's hard disk
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("disk%1:06:03")
      :COMMENTS HTML-PURCHASING-CORPUS)))
   )
))

