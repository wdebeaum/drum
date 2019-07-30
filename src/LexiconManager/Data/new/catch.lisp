;;;;
;;;; W::CATCH
;;;;

#|
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CATCH
   (SENSES
    ((LF-PARENT ONT::event)
     (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))
|#

(define-words :pos W::V 
 :words (
  (W::catch
   (wordfeats (W::morph (:forms (-vb) :past W::caught :ing w::catching :nom w::catch)))
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090430 :comments nil :vn ("get-13.5.1") :wn ("catch%2:35:00" "catch%2:35:01" "catch%2:35:03"))
     (lf-parent ont::capture)
     (example "he caught the ball")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic) (F::trajectory -))
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype (? pt W::from)))))
     )
   )
)
))

(define-words :pos W::v
 :words (
  ((W::catch W::it)
   (wordfeats (W::morph (:forms (-vb) :past W::caught)))
   (SENSES
    ((example "I really caught it the other day" "we really caught it from the storm last week")
     (LF-PARENT ONT::RECEIVE-PUNISHMENT)
     (meta-data :wn ("catch_it%2:41:00"))
     (templ affected-SOURCE-XP-OPTIONAL-TEMPL)
     )
    )
   )
))
