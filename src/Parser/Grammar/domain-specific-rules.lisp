;;;;
;;;; domain-specific-rules.lisp
;;;;
;;;;
;;;; These are all the rules moved from different files that look
;;;; like domain hacks for me
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; phrase-grammar.lisp
;;
;;

(in-package :W)


;; none of these rules are used anymore -- swift Feb 12 04
;;(cl:setq *grammar-N1-domain*
;      '((headfeatures
;         (N1 VAR AGR MASS SEM Changeagr LEX QUANTITY transform)
;         (QUAL ARG ARGSEM VAR transform)
;         (ADJP VAR ARGSEM ATYPE transform)
;         (COMPARATIVE VAR ARGSEM ATYPE transform)
;         (SUPERLATIVE VAR ARGSEM ATYPE transform))
;        ;;  FUNC with to-complement
;
;        ;; commenting this out because we don't use SORT FUNC anymore
;        ((N1 (VAR ?v) (SORT PRED) (CLASS ?lf) 
;             (RESTR (?lf ?v ?v1)))
;         -N1-to-comp>
;         ;;      (head (n (SORT FUNC) (LF ?lf) (ARGSEM ACTION)))
;         ;; !!!!!
;         (head (n (SORT FUNC) (LF ?lf) (ARGSEM ($ situation (F::cause F::agentive)))))
;         (s (stype to) (var ?v1)))
;        
;;;	;;  identification of problems
;;;	;;  e.g., the city with congestion
;;;	((N1 (RESTR ?new) (SORT ?sort) (CLASS ?c) (RELN ?reln))
;;;         -N1-with1>
;;;	 (head (N1 (RESTR ?r) (VAR ?v) (SEM ($ phys-obj (object-function F_location)))
;;;		   (SORT ?sort) (class ?c) (RELN ?reln)))
;;;	 (PP (ptype with) (VAR ?l1) (LF ?pred-name) (SEM ($ situation)))
;;;         (add-to-conjunct (val (HAS-PROBLEM ?v ?l1)) (old ?r) (new ?new)))
	
;;;        ;; FUNC with PATH complement.  e.g., distance from A to B
;;;	;; Shouldn't there be any unification between head ARGSEM and the path sem???
;;;	((N1 (VAR ?v) (SORT PRED) (CLASS ?lf) 
;;;	  (RESTR (?lf ?v ?v1)))
;;;	 -N1-path>
;;;	 (head (n (SORT FUNC) (LF ?lf) (ARGSEM ($ abstr-obj (Functn F_path)))))
;;;	 (path (var ?v1) (LF ?lf2)))

	;; 08/08/00 Myrosia commented out as using obsolete types
	
	;; oranges for OJ, the train for the oranges, the train for the avon route
	;;	((N1 (RESTR (& (ASSOC-WITH ?v1 ?v2) ?r)) (CLASS ?c) (SORT ?sort))
	;;	 -N1-assoc2>
	;; (head (N1 (VAR ?v1) (SEM (? sem TRANS-AGENT COMMODITY))
	;;	(RESTR ?r) (CLASS ?c) (SORT ?sort)))
	;; (pp (ptype for) (VAR ?v2) (SEM (? sem1 PHYS-OBJ ROUTE))))

	;; keyboard/map display/screen/window
	
;;;        ((N1 (RESTR (& (SHOWING  ?v2 ?lf) ?r)) (CLASS ?c) (SORT ?sort))
;;;	 -N-n-display>
;;;	 (n (SEM (? sem DISPLAY)) (LF ?lf) (VAR ?v1))
;;;	 (head (N1 (VAR ?v2) (SEM (? s DISPLAY)) 
;;;		   (RESTR ?r) (CLASS ?c) (SORT ?sort))))


;;))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Intersections etc
;;
(parser::augment-grammar
;;(cl:setq *Grammar-intersections*
  '((headfeatures
     (NP CASE MASS agr PRO CLASS Changeagr wh transform)
     )
    ;; Myrosia 02/26/00
    ;; so far we restrict these constructions to "geographical line at geographical line"
    ;; Uncommented 2003/11/06 to help in dealing with coercion
    ;; This is a (relatively) low priority, for point contexts only,
    ;; such as "go to Clinton and Goodman"
    ((np (LF (% description (status definite) (var *) (SORT individual) 
		(class LF::Intersection)
		(CONSTRAINT (& (LF::OF (LF::SET-OF ?v1 ?v2))))
		(sem ($ f::phys-obj (f::spatial-abstraction f::spatial-point) 
			(f::form F::geographical-object)))
		))
      (var *)
      (sem ($ f::phys-obj (f::spatial-abstraction f::spatial-point) 
	      (f::form F::geographical-object)))
      (sort pred))
     -np-line-at-line-intersection> 0.96
     (head (np (wh -) (var ?v1) 
	    ;; only street names here - don't allow sets ets, that not right
	    (lf (% description (status (? sts1 definite name gname number)) (sort (? srt1 individual pred))))
	    (sem ($ f::phys-obj (f::spatial-abstraction (? sa1 f::line f::strip))
		    (f::form F::geographical-object)))	    
	    ))
     (conj (lex and))
     (np (wh -) (var ?v2) 
      ;; only street names here - don't allow sets ets, that not right
      (lf (% description (status (? sts2 definite name gname number)) (sort (? srt2 individual pred))))
      (sem ($ f::phys-obj (f::spatial-abstraction (? sa2 f::line f::strip))
	      (f::form F::geographical-object)))	    
      )     
     )
    ))
;;(parser::augment-grammar *Grammar-intersections*) 

;; deleted for now - why do we need this?
;; various misc rules for collocations
;(cl:setq *special-name-rules*
;  '((headfeatures
;     (ADV var transform)
;     (conj var transform)
;     (quan var transform)
;     (adj var transform)
;     (pause var lex transform)
;     (N1 VAR AGR MASS SEM Changeagr QUANTITY transform)
;     )             
;    ;; Myrosia 2/12/99: changed the rule so that class in LF comes from class
;    ;; NP -> the NAME
;    ;; we do not want it to apply if there are other options, like N-N modification
;    ((NP (SORT DESCR) (var ?v) (Class ?c) (sem ?s) (agr ?agr)
;      (LF (% Description (Status Name) (var ?v) (Sort Individual)
;             (Class ?c) (lex ?l))))
;     -np-the-name> 0.9
;     (art (lex the))
;     (head (name (lex ?l) (sem ?s) (var ?v) (agr ?agr) (lf ?c)
;            )))
;        
;
;  ))
;
;; 
;;(parser::augment-grammar *special-name-rules*) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adverbial-grammar.lisp
;;

;(parser::augment-grammar
;  '((headfeatures
;     (PP VAR KIND CASE MASS NAME agr SEM SORT PRO SPEC CLASS transform)
;     (ADVBLS FOCUS VAR SEM SORT ATYPE ARG ARGCAT SEM ARGSEM NEG TO QTYPE transform)
;     (ADVBL VAR ARGCAT ARGSEM ARGSORT ATYPE SEM SUBCATSEM transform)
;     (ADV SORT ATYPE DISC-FUNCTION SA-ID PRED NEG TO LEX transform)
;     )                  
;
;;;;    ;; Myrosia 12/05/00 Commented out
;;;    ;; Now these cases should be handled through "roles" feature
;;;    ;; ===============================
;;;    ;;   Mode adverbials
;;;    ;;   e.g.,  travel BY TRAIN
;;;    ;; !!!!!!!!!! was argsem go!
;;;    ((ADVBL (LF (?lf ?a ?v1)) (ARG ?a) (ARGSEM ($ situation)) 
;;;      (LFPRED instrument) (lfarg ?v1) (SORT CONSTRAINT))
;;;     -advbl-mode1>
;;;     (head (adv (SEM ($ abstr-obj (type F_TRAVEL-MODE))) (LF ?lf)))
;;;     ;; !!! war Sem Trans-Agent Travel-Mode
;;;     (np (sem ($ phys-obj (Functn F_vehicle))) (GAP -) (VAR ?v1) (class ?c)))

    ;; Myrosia 08/08/00 commentned out since it is not functional due to incorrect types anyway
    ;;    ;; hack rule to convert "by road" -> "by a vehicle that travels by road"
    ;;    ((np (lf (% description (type ?c) (class ?c) (status indefinite)
    ;;		(var ?v) (sort individual))) 
    ;;      (class ?c) (var ?v)
    ;;   (sem TRAVEL-MODE))
    ;; -np-mode1> .9
    ;;   (n1 (sem (? sem TRANS-AGENT TRAVEL-MODE)) (VAR ?v) (class ?c)))  
;;))

;;  PATH DETECTION
;; these rule construct special forms of adverbials that are
;;;  subcategorized for by verbs: PATH and LOCATIONS
;
;(parser::augment-grammar
; '((headfeatures
;     (adv var lf lex subcat argsem argsort atype agr sort sem lf transform))
;   
;;;   ;;  any PATH adverbial, e.g., "to avon"
;;;   ((path (var ?v) (sem ?s) 
;;;     (gap ?gap) (lf (% Path (var ?v) (constraint ?lf))))
;;;    -advbl-to-path>
;;;    (advbls (arg ?v) (sem ?s) (sem ($ abstr-obj (Functn path)))
;;;     (gap ?gap) (var ?v) (lf ?lf)))
;;;   
;;;   ;;  Once the path is built, convert back to an adverbial
;;;   ;; Myrosia changed to 2 rules !!!!!!!!!
;;;   ((ADVBLS (ARG ?arg) (VAR *) (ATYPE PRE) (SORT CONSTRAINT) (gap ?gap)
;;;           (ARGCAT S) (ARGSEM ($ situation (trajectory +))) (LF (ROUTE ?arg ?v))) ;;  NB: "*" creates new constant
;;;    -path-to-advbl-pre1>
;;;    (head (path (ARGSEM ?argsem) (VAR ?v) (GAP ?gap))))
;;;   
;;;      ((ADVBLS (ARG ?arg) (VAR *) (ATYPE PRE) (SORT CONSTRAINT) (gap ?gap)
;;;           (ARGCAT S) (ARGSEM ($ situation (trajectory +))) (LF (ROUTE ?arg ?v))) ;;  NB: "*" creates new constant
;;;    -path-to-advbl-pre2>
;;;    (head (path (ARGSEM ?argsem) (VAR ?v) (GAP ?gap))))
;;;   
;;;   ;; for NP cats, there are 3 possible  usages:
;;;   ;; 1. a river from Avon to Bath, a route along the coast - basically, the geographical lines
;;;   ;; that connect things
;;;   ;; 2. A bus/vehicle from avon (to Bath) - there are more controversial cases here
;;;   ;; but essentially anything that can function as a vehicle does it
;;;   ;; 3. a path from Avon to Bath - an abstract path used as such situations
;;;   ;; the last one is questionable, since so far we do not have those in the system
;;;   ;; but we may end up introducing them
;;;
;;;   ((ADVBLS (ARG ?arg) (VAR *) (ATYPE POST) (SORT CONSTRAINT) (ARGCAT NP) 
;;;     (ARGSEM ($ phys-obj (form geographical-object) 
;;;		(spatial-abstraction (? sa line strip)))) (GAP ?gap)
;;;            (LF (ROUTE ?arg ?v))) ;;  NB: "*" creates new constant
;;;    -path-to-advbl-post1-1>
;;;    (head (path (ARGSEM ?argsem) (VAR ?v) (GAP ?gap))))
;;;   
;;;   ((ADVBLS (ARG ?arg) (VAR *) (ATYPE POST) (SORT CONSTRAINT) (ARGCAT NP) 
;;;     (ARGSEM ($ phys-obj (form object) (Functn vehicle)))
;;;     (GAP ?gap)
;;;     (LF (ROUTE ?arg ?v))) ;;  NB: "*" creates new constant
;;;    -path-to-advbl-post1-2>
;;;    (head (path (ARGSEM ?argsem) (VAR ?v) (GAP ?gap))))
;;;   
;;;   ((ADVBLS (ARG ?arg) (VAR *) (ATYPE POST) (SORT CONSTRAINT) (ARGCAT NP) 
;;;     (ARGSEM ($ abstr-obj (Functn path))) (GAP ?gap)
;;;     (LF (ROUTE ?arg ?v))) ;;  NB: "*" creates new constant
;;;    -path-to-advbl-post1-3>
;;;    (head (path (ARGSEM ?argsem) (VAR ?v) (GAP ?gap))))
;;;   
;;;   ;; for situation types, the usage is easy - only (trajectory +)
;;;   ;; situations can be modified by paths
;;;   ((ADVBLS (ARG ?arg) (VAR *) (ATYPE POST) (SORT CONSTRAINT) (ARGCAT (? c VP NP)) 
;;;              (ARGSEM ($ situation (trajectory +))) (GAP ?gap)
;;;            (LF (ROUTE ?arg ?v))) ;;  NB: "*" creates new constant
;;;    -path-to-advbl-post2> 1.1
;;;    (head (path (ARGSEM ?argsem) (VAR ?v) (GAP ?gap))))
;;;   
;;;   ;; columbus to raleigh  (implicit FROM)
;;;   ((path  (var ?v) (sem ($ abstr-obj (Functn path)))
;;;     (lf (% Path (var ?v) (constraint (& (from ?v ?v1) ?lf1)))))
;;;    -path4> .7
;;;     (np (name +) (SEM CITY) (var ?v1))
;;;    (advbls (arg ?v) (var ?v) (gap -) (sem ($ abstr-obj (Functn path))) 
;;;     (lf ?lf1)))
;;;   
;;;   ;;  this rule forces a wide scoping of directly
;;;   
;;;   ;;((path (var ?v) (sem path) (lf (% Path (var ?v) (constraint (directly ?lf)))))
;;;   ;; -path5>
;;;   ;; (word (lex (? x directly straight)))
;;;   ;; (advbls (arg ?v) (sem trajectory) (var ?v) (lf ?lf)))
;;;   
;;;   
;;;    ;;   ??????????  When needed??
;;;   ((Loc (var ?v) (sem ($ abstr-obj (Functn path)))
;;;     (lf (% Loc (var ?v) (constraint ?lf))))
;;;    -loc1>
;;;    (advbls (sem ($ abstr-obj (Functn location))) (sort SETTING) (ARG ?v) (VAR ?v) (LF ?lf)))
   
;;   ))

;; Special rules that violate head features

(parser::augment-grammar
   '((headfeatures
      )

   ;; MINIMIZING scales: e.g., as quickly/cheaply as possible
    ;;   These work only for adverbials with the COMP-OP feature set.

    ((ADVBL (ARG ?arg) 
            (ARGCAT S)
            (ARGSEM ($ situation)) 
            (LF (MANNER ?arg (MIN ?pred))) 
            (ATYPE (? atype PRE POST)))
     -as..as1>
     (word (lex as))
     (head (adv (COMP-OP LESS) (PRED ?pred) (LF ?lf)))
     (word (lex as))
     (word (lex possible)))
    
    ;; MAXIMIZING SCALES: e.g., as slowly/expensively as possible
    ((ADVBL (ARG ?arg) 
            (ARGCAT S)
            (ARGSEM ($ situation))
            (LF (MANNER ?arg (MAX ?pred))) 
            (ATYPE (? atype PRE POST)))
     -as..as2>
     (word (lex as))
     (head (adv (COMP-OP MORE) (PRED ?pred)))
     (word (lex as))
     (word (lex possible)))

;;;     ;; !!! is this really actiojn here?
;;;      ;;  Purpose adverbials: e.g., to get the people at Avon
;;;     ((ADVBL (ARG ?arg) (ARGSEM ($ situation (aspect F_dynamic) (cause F_Agentive))) 
;;;       (LF (Purpose ?arg ?svar)) (ATYPE (? atype PRE POST)))
;;;     -advbl-purpose>
;;;     (head (s (stype to) (var ?svar)))
;;;     )
    
      ;; e.g., via/by/using  the shortest/quickest route   - eventually needs to be replaces by a more general rule
    ((ADVBL (ARG ?arg) (ARGSEM ($ situation)) 
	    (LF (:PREFERENCE ?cname)) (ATYPE (? atype PRE POST)))
     -via-preference>
     (word (lex (? s via by using)))
     (np (lf (% description (class (? x route path)) (constraint (?cname ?carg))))))
    )
   )

;; don't think we use this anymore -- swift Feb 12 2004
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special "be" rules
;;

;
;(parser::augment-grammar
; '((headfeatures
;    (vp vform var neg sem iobj dobj comp3 part cont transform)
;    )
;   ;; Myrosia 07/23/00 added the rule for cases like "this can be the cheapest route"
;   ;;  the issue is that infinite BE does not have a AGR feature
;   ((vp (subj (% np (agr ?agr) (sem ?subjsem) (var ?subjvar))) (main +) (gap -)
;     (sort PRED) (var ?v) (subjvar ?subjvar) (agr ?agr)
;     (class (BE EQUAL)) (constraint (& (LSUBJ ?subjvar) (LOBJ ?npvar)
;                                       (?lsubj-map ?subjvar)(?dobj-map ?npvar)  
;                                       ))
;     (postadvbl -)
;     )
;    -vp-be2-infinite>
;    (head (v (lf ?c) (agr -) (sem ($ situation (F::type F::BE))) (DOBJ (% np))
;           (subj-map ?lsubj-map) (dobj-map ?dobj-map) 
;           ))
;    (NP (sem ?subjsem) (agr ?agr) (var ?npvar) 
;     (lf (% description (status (? x definite name pro))))))   
;   ))
;








