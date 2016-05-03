;;;;
;;;; w::popup
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::popup W::MENU)
    (wordfeats (W::MORPH (:forms (-S-3P) :plur (w::popup w::menus))))
   (SENSES
    ((LF-PARENT ONT::symbolic-representation) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN plow :ENTRY-DATE 20050310 :CHANGE-DATE NIL
      :COMMENTS nil))))
))

