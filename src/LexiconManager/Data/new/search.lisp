;;;;
;;;; W::SEARCH
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::SEARCH W::ENGINE)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::search W::engines))))
   (SENSES
    ((LF-PARENT ONT::software-application) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20050318 :CHANGE-DATE 20050824 :wn ("search_engine%1:10:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::search
    (wordfeats (W::morph (:forms (-vb) :nom w::search)))
   (SENSES
    ((EXAMPLE "search the lane")
     (LF-PARENT ONT::physical-scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-THEME-XP-TEMPL)
     (meta-data :origin lou :entry-date 20040311 :change-date nil :comments lou-sent-entry :vn ("search-35.2") :wn ("search%2:35:00" "search%2:35:01" "search%2:39:00"))
     )
    )
   )
))

