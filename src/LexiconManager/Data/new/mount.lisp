;;;;
;;;; W::mount
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::mount
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("put-9.1") :wn ("mount%2:30:01" "mount%2:35:00"))
     (LF-PARENT ONT::put)
     (example "he mounted a picture on the wall")
 ; like insert,position
     )
      ((meta-data :origin text-processing :entry-date 20091204 :change-date nil :comments TimeBank)
     (LF-PARENT ONT::increase)
     (example "the death toll mounted")
     (TEMPL AFFECTED-TEMPL)
     )
    )
   )
))

