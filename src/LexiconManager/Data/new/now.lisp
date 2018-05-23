;;;;
;;;; W::NOW
;;;;

#|
(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::NOW
   (SENSES
    ;; Myrosia added 11/13/03, because it is really not a time with imperatives
    ;; how about "send a truck to barnacle now"?
    ((EXAMPLE "Now send a truck to Barnacle")
     (LF-PARENT ONT::SEQUENCE-POSITION)
     )
    )
   )
))
|#

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::NOW W::THAT)
   (SENSES
    ((LF-PARENT ONT::Conjunct )
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin beetle2 :entry-date 20070609 :change-date nil :comments sentential-conjunction-cleanup)
     )
    ))
))

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::NOW
   (wordfeats (W::ATYPE (? atype W::pre W::post)))
   (SENSES
    (;(LF-PARENT ONT::EVENT-TIME-REL)
     (LF-PARENT ONT::EVENT-TIME-REL-NOW)
;     (SYNTAX (W::IMPRO-CLASS (:* ONT::TIME-LOC W::NOW)))
     (SYNTAX (W::IMPRO-CLASS ONT::TIME-LOC))
     (TEMPL pred-s-vp-templ)
     )
    ))
))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::NOW
   (SENSES
    ((LF-PARENT ONT::time-loc)
     (PREFERENCE 0.97)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::now w::and w::then)
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090319 :change-date nil :comments nil)
;     (LF-PARENT ONT::FREQUENCY)
     (LF-PARENT ONT::sometimes)
     (lf-form w::sometimes)
     (TEMPL pred-s-post-TEMPL)
     )
    )
   )
))

