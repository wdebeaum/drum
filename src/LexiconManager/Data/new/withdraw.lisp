;;;;
;;;; W::withdraw
;;;;

(define-words :pos W::v
 :words (
  (W::withdraw
   (wordfeats (W::morph (:forms (-vb) :past W::withdrew :pastpart W::withdrawn :ing W::withdrawing)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090527 :comments nil :vn ("remove-10.1") :wn ("withdraw%2:30:01" "withdraw%2:40:00" "withdraw%2:40:01"))
     (LF-PARENT ONT::cause-out-of)
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL)
     )
    ((lf-parent ont::move-back)
     (example "he withdrew from the field")
     (TEMPL AGENT-TEMPL)
    )
    )
   )
))

