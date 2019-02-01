;;;;
;;;; W::transcriptional
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::transcriptional
   (SENSES
    (
     (LF-PARENT ONT::property-val) ; dummy
     (SYNTAX (w::pertainym (:* ONT::GENE-TRANSCRIPTION W::transcription)))
     (TEMPL CENTRAL-ADJ-TEMPL)
     )
    )
   )
))
