;;;;
;;;; W::migrate
;;;;

 (define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::migrate
   (wordfeats (W::morph (:forms (-vb) :nom w::migration)))
   (SENSES
     ((LF-PARENT ONT::migrate)
     (example "they migrated to ethiopia")
     (TEMPL agent-templ)
     )
    )
   )
))

