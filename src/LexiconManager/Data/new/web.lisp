;;;;
;;;; W::WEB
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::WEB
   (SENSES
    ((LF-PARENT ONT::COMPUTER-NETWORK) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("web%1:06:02")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((w::web W::PAGE)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::web W::pages))))
   (SENSES
    ((LF-PARENT ont::website)(TEMPL COUNT-PRED-TEMPL)
     (meta-data :origin calo :entry-date 20041025 :change-date nil :wn ("web_page%1:10:00") :comments y2)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((w::web W::site)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::web W::sites))))
   (SENSES
    ((LF-PARENT ont::website)
     (meta-data :origin calo :entry-date 20041025 :change-date nil :wn ("web_site%1:10:00") :comments y2)
     )
    )
   )
))

