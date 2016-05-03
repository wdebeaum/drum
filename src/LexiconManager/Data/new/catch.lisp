;;;;
;;;; W::CATCH
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CATCH
   (SENSES
    ((LF-PARENT ONT::event) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::V 
 :words (
  (W::catch
   (wordfeats (W::morph (:forms (-vb) :past W::caught :ing w::catching :nom w::catch)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090430 :comments nil :vn ("get-13.5.1") :wn ("catch%2:35:00" "catch%2:35:01" "catch%2:35:03"))
     (lf-parent ont::capture)
     (example "he caught the ball")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::trajectory -))
     (TEMPL agent-affected-source-optional-templ (xp (% W::PP (W::ptype (? pt W::from)))))
     )
   )
)
))

