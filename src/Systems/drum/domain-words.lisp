(in-package :lxm)

(define-words :pos W::adj  
:words (
 (w::WT
   (SENSES
    ((LF-PARENT ONT::NATURAL)
     (LF-FORM W::WILDTYPE)
     (TEMPL central-adj-templ)
     )
    )
   )
))

(define-words :pos W::n 
:words (
 (w::WT
   (SENSES
    ((LF-PARENT ONT::WILDTYPE-OBJ)
     (LF-FORM W::WILDTYPE)
     (TEMPL count-pred-templ)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (W::target
   (wordfeats (W::morph (:forms (-vb) :past w::targeted :ing w::targeting)))
   (SENSES
    ((lf-parent ont::TRANSPORT)
     (EXAMPLE "protein targeting")
     (TEMPL agent-affected-GOAL-templ (xp (% W::ADVBL (w::lf (% w::prop (w::class (? x ont::goal-reln ont::source-reln)))))))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::sort
   (SENSES
    ((lf-parent ont::TRANSPORT)
     (example "protein sorting")
     ;(TEMPL agent-affected-xp-templ)
     (TEMPL agent-affected-GOAL-templ (xp (% W::ADVBL (w::lf (% w::prop (w::class (? x ont::goal-reln ont::source-reln)))))))
     )

    )
   )
))

;;;;;;;;;;;;;;;;;;;;;;;;
; TFTA
;;;;;;;;;;;;;;;;;;;;;;;;

(define-words :pos W::name
:words (
 (w::Reactome
   (SENSES
    ((LF-PARENT ONT::DATABASE)
     (TEMPL name-templ)
     )
    )
   )
))

(define-words :pos W::name
:words (
 (w::KEGG
   (SENSES
    ((LF-PARENT ONT::DATABASE)
     (TEMPL name-templ)
     )
    )
   )
))

;;;;;;;;;;;;;;;;;;;;;;;;
; for paper 1
;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define-words :pos W::n 
:words (
 (w::RTK
   (SENSES
    ((LF-PARENT ONT::PROTEIN)
     (TEMPL count-pred-templ)
     )
    )
   )
))

; put here because there should be a more principled way to handle adjectives
(define-words :pos W::adj 
:words (
 ((w::short w::lived)
   (SENSES
    ((LF-PARENT ONT::SHORT)
     (TEMPL CENTRAL-ADJ-TEMPL)
     )
    )
   )
))

; copied from "resulting"
(define-words :pos W::adj 
 :words (
   (W::ensuing
   (SENSES
    (
     (LF-PARENT ONT::outcome-val)
     (TEMPL CENTRAL-ADJ-TEMPL)
     (example "the ensuing chaos")  
     )
    )
   )
))

|#
