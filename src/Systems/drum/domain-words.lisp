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

;;;;;
; barley paper

#|
(define-words :pos W::n 
:words (
 ((w::Cly)
   (SENSES
    ((LF-PARENT ONT::GENE)
     (TEMPL count-pred-templ)
     )
    )
   )
))

(define-words :pos W::n 
:words (
 (w::lodicule
   (SENSES
    ((LF-PARENT ONT::PLANT-PART)
     (TEMPL count-pred-templ)
     )
    )
   )
))

(define-words :pos W::n 
:words (
 (w::cleistogamy
   (SENSES
    ((LF-PARENT ONT::PROCESS)
     (TEMPL count-pred-templ)
     )
    )
   )
))

(define-words :pos W::n 
:words (
 ((w::HvAP)
   (SENSES
    ((LF-PARENT ONT::MOLECULE)
     (TEMPL count-pred-templ)
     )
    )
   )
))

(define-words :pos W::n 
:words (
 ((w::HvAG)  ; Hv: Hordeum vulgare (barley); AG: AGAMOUS gene 
   (SENSES
    ((LF-PARENT ONT::GENE)
     (TEMPL count-pred-templ)
     )
    )
   )
))

|#
