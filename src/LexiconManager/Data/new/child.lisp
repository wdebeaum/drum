;;;;
;;;; W::child
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::child
   (wordfeats (W::morph (:forms (-S-3P) :plur w::children)))
    (SENSES
     ((LF-PARENT ONT::child)
      (meta-data :origin plow :entry-date 20050928 :change-date nil :comments naive-subjects)
      (templ kinship-reln-templ)
      )
     )
    )
))

