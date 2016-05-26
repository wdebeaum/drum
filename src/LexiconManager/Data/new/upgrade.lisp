;;;;
;;;; W::UPGRADE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::UPGRADE
   (SENSES
    ((LF-PARENT ONT::MANUFACTURED-OBJECT) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("upgrade%1:10:00" "upgrade%1:06:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::UPGRADE
   (SENSES
    ((LF-PARENT ONT::computer-hardware) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("upgrade%1:06:00")
      :COMMENTS HTML-PURCHASING-CORPUS))
    ((LF-PARENT ONT::software-application) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("upgrade%1:10:00")
      :COMMENTS HTML-PURCHASING-CORPUS))
    ))
))

(define-words :pos W::v 
 :words (
(w::upgrade
 (senses
  ((LF-PARENT ONT::device-adjust)
    (example "you can upgrade your computer")
    (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
    (templ agent-affected-xp-templ)
    (meta-data :origin calo :entry-date 20041122 :change-date 20090504 :comments caloy2)
    )
   ))
))

