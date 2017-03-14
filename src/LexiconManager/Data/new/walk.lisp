;;;;
;;;; W::WALK
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::WALK w::THROUGH)
    (wordfeats (W::morph (:forms (-S-3P) :plur (W::walk W::throughs))))
   (SENSES
    ((LF-PARENT ONT::INFORMATION-FUNCTION-OBJECT) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::walk
    (wordfeats (W::morph (:forms (-vb) :nom W::walk)))
   (SENSES
    ;;;; Walk to Avon
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("?walk%2:38:02" "walk%2:38:00" "walk%2:38:01" "walk%2:38:03" "walk%2:38:05"))
     (LF-PARENT ONT::SELF-LOCOMOTE)
     (TEMPL AGENT-TEMPL)
     )
    ((meta-data :origin chf :entry-date 20070904 :change-date nil :comments nil)
     (LF-PARENT ONT::WALKING)
     (example "walk the dog")
     (preference .98)
     (TEMPL AGENT-affected-XP-TEMPL)
     )
     ((meta-data :origin general :entry-date 20110127 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("travel%2:38:00" "travel%2:38:02"))
     (LF-PARENT ONT::self-locomote)
     (example "walk the line")
     (TEMPL agent-neutral-templ) ; like stroll,walk
     (PREFERENCE 0.96)
     )
    )
   )
))
