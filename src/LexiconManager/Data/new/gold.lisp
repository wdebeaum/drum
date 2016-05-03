;;;;
;;;; W::GOLD
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::GOLD
   (SENSES
    ((LF-PARENT ONT::substance) (TEMPL mass-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("gold%1:27:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos w::adj :templ central-adj-templ
 :tags (:base500)
 :words (
   (W::gold
    (SENSES
    ((meta-data :origin joust :entry-date 20091027 :change-date 20091027 :comments nil :wn ("gold%3:00:01:chromatic:00"))
     (LF-PARENT ONT::gold)
     (SYNTAX (W::morph (:forms (-er))))
     (templ central-adj-templ)
     )
    )
   )
))

