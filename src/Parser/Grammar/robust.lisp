;;;;
;;;; robust.lisp
;; This contains rules that are useful when processing complex text
;;   mainly used to produce more informative fragments
;;;;
;;;; Time-stamp: <Tue Jan 19 15:50:37 EST 2016 jallen>
;;;;

(in-package :W)

(parser::augment-grammar
 '((headfeatures
    (isolated-cp vform var neg sem subjvar dobjvar cont  transform subj-map subj))

 ;;   ISOLATED RELATIVE CLAUSES

   ;; with robust parsing, long sentences are often broken into fragments.
   ;;  this rule is just like -rel1> except that we bind thr pro form to the arg, allowing
   ;;  reference resolution to try to reattach it in its proper place.
   ;; TEST: which chased the cat.
   ((ISOLATED-CP (ctype relc) 
     ;;(ignore (% *PRO* (status pro) (var ?prov) (class ?class) (sem ?argsem) (constraint (& (proform ?plex) (relative-pro +)))))
     (argsem ?argsem) (var ?v) (gap -) ;; (gap ?g)
;;;     (lf (% prop (var ?v) (arg ?arg) (class ?c) 
;;;	    (constraint ?con) (sem ?sem) (transform ?transform)
;;;	    (tma ?tma)
;;;	    )
     (lf ?newlf)
     ;; aug-trips
     (lex ?vlex) (headcat ?hcat)
     ;; (lex ?plex) (headcat ?vcomp)  ;; non-aug-trips settings   
     )
    -isolated-rel1> 
    (pro (wh (? wh R)) (lf ?class) (sem ?argsem) (lex ?plex) (var ?prov)
     (agr ?agr) (CASE SUB) (headcat ?vcomp))
    (head (vp (subj (% np (var ?prov) (sem ?argsem) (sem ($ ?!type))))
	      (subjvar ?prov)
	      (subjvar ?prov)
	      (agr ?agr) 
	      (gap -)  ;; my guess is there are some eliiptical glosses that motivated allowing a gap here, but I'm deleting it for now(gap ?g) 
	      (WH -)
	      (class ?c) (constraint ?con) (vform fin) (tma ?tma)
	      (sem ?sem) (lf ?lf)
	      (transform ?transform) (lex ?vlex)
	      (advbl-needed -)
	      (headcat ?hcat) ;; aug-trips
	      ))
    (add-to-conjunct (val (focus (% *PRO* (status pro) (var ?prov) (class ?class) (sem ?argsem) (constraint (& (proform ?plex) (relative-pro +))))))
     (old ?con) (new ?newcon))
    (change-feature-values (old ?lf) (new ?newlf) (newvalues ((constraint ?newcon)))))

))




















