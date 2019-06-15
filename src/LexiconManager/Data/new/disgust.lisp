;;;;
;;;; W::disgust
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::disgust
   (wordfeats (W::morph (:forms (-vb) :past W::disgusted :ing W::disgusting)))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090512 :comments LM-vocab)
     (LF-PARENT ONT::evoke-disgust)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like annoy,bother,concern,hurt
     (syntax (w::resultative +))
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::disgust
   (SENSES
    ((LF-PARENT ONT::not-attractive-scale) (TEMPL mass-PRED-TEMPL)
     (META-DATA :ORIGIN domain-reorganization :ENTRY-DATE 20170821 :CHANGE-DATE NIL)
     )))
))

#||
(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::disgust
   (wordfeats (W::morph (:forms (-vb) :past W::disgusted :ing W::disgusting)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("amuse-31.1") :wn ("sadden%2:37:01"))
     (LF-PARENT ont::cause-body-effect)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     )
    )
   )
))||#

