;;
;; Nate Chambers
;;
;; test suite for lf-public and kr-public functions
;;

(print (list
	
; ;; lf-parent
; (mapcar (lambda (in out) (equal out (funcall 'lf-parent in)))
; 	'(lf_move lf_thing lf_equipment lf_grade-modifier lf_adsf) '(lf_motion lf_any-sem lf_manufactured-object lf_modifier nil))
; ;; lf-parent
(mapcar (lambda (in out) (equal out (funcall 'get-parent in)))
	'(lf_move lf_thing lf_equipment lf_grade-modifier lf_adsf) '(lf_motion lf_any-sem lf_manufactured-object lf_modifier nil))

; ;; lf-subtype
; (mapcar (lambda (sub super out) (equal out (funcall 'lf-subtype sub super)))
; 	'(lf_situation-root lf_body-process lf_qmodifier) '(lf_body-process lf_situation-root lf_modifier) '(nil lf_body-process lf_qmodifier))
;; lf-subtype
(mapcar (lambda (sub super out) (equal out (funcall 'subtype sub super)))
	'(lf_situation-root lf_body-process lf_qmodifier) '(lf_body-process lf_situation-root lf_modifier) '(nil lf_body-process lf_qmodifier))

; ;; lf-more-specific
; (mapcar (lambda (sub super out) (equal out (funcall 'lf-more-specific sub super)))
; 	'(lf_topic-signal lf_situation-root lf_body-process lf_qmodifier) '(lf_restriction lf_body-process lf_situation-root lf_modifier) '(nil lf_body-process lf_body-process lf_qmodifier))
;; lf-more-specific
(mapcar (lambda (sub super out) (equal out (funcall 'more-specific sub super)))
	'(lf_topic-signal lf_situation-root lf_body-process lf_qmodifier) '(lf_restriction lf_body-process lf_situation-root lf_modifier) '(nil lf_body-process lf_body-process lf_qmodifier))

; ;; lf-common-ancestor
; (mapcar (lambda (sub super out) (equal out (funcall 'lf-common-ancestor sub super)))
; 	'(lf_machine lf_situation-object-modifier lf_restriction) '(lf_predicate lf_manner lf_topic) '(lf_any-sem lf_situation-object-modifier lf_predicate))
;; lf-common-ancestor
(mapcar (lambda (sub super out) (equal out (funcall 'common-ancestor sub super
						    :ontology 'lf)))
	'(lf_machine lf_situation-object-modifier lf_restriction) '(lf_predicate lf_manner lf_topic) '(lf_any-sem lf_situation-object-modifier lf_predicate))

; ;; lf-feature-subtype
; (mapcar (lambda (x y out) (equal out (funcall 'lf-feature-subtype x y)))
; 	'(f_bounded f_force f_mental f_atomic f_plant) '(f_dynamic f_stimulating f_located f_atomic f_natural) '(f_bounded nil nil f_atomic f_plant))
;; lf-feature-subtype
(mapcar (lambda (x y out) (equal out (funcall 'subtype x y)))
	'(f_bounded f_force f_mental f_atomic f_plant) '(f_dynamic f_stimulating f_located f_atomic f_natural) '(f_bounded nil nil f_atomic f_plant))

;; lf-sem
(mapcar (lambda (x out) (equal out (funcall 'lf-sem x)))
	'(lf_move lf_trajectory) 
	'((FT_SITUATION
 (:REQUIRED (TIME-SPAN F_ANY-TIME-SPAN) (TYPE F_GO) (TRAJECTORY +)
  (LOCATIVE -) (CONTAINER -) (ASPECT F_DYNAMIC) (CAUSE (? C F_FORCE -))
  (INTENTIONAL -) (INFORMATION F_PROPOSITION)))
	  (FT_ABSTR-OBJ
 (:REQUIRED (SCALE -) (MEASURE -) (CONTAINER -) (INFORMATION -)
  (INTENTIONAL -) (GRADABILITY -) (TYPE F_ANY-TYPE)))
	  )	
	)

;; lf-arguments
(mapcar (lambda (x out) (equal out (funcall 'lf-arguments x)))
	'(lf_land-vehicle lf_human-object lf_body-process)
	'(
	  ((:OPTIONAL LF_SPATIAL-LOC (FT_PHYS-OBJ) (:IMPLEMENTS LF_SPATIAL-LOC)))
	  ((:OPTIONAL LF_SPATIAL-LOC (FT_PHYS-OBJ) (:IMPLEMENTS LF_SPATIAL-LOC)))
	  ((:OPTIONAL CO-THEME (FT_PHYS-OBJ (FORM F_SUBSTANCE)) (:IMPLEMENTS CO-THEME))
	   (:REQUIRED THEME (FT_PHYS-OBJ (ORIGIN F_LIVING)) (:IMPLEMENTS THEME)))

	  )
	)

;; lf-GCB
(mapcar (lambda (x y out) (equal out (funcall 'lf-GCB x y)))
	'(
	  (ft_Situation (:required (intentional -)) (:default (origin f_living)))
	  (ft_phys-obj (group +) (origin f_human) (intentional +) (mobility F_movable))
	  (ft_Abstr-obj (CONTAINER -) (INFORMATION -) (INTENTIONAL -) (SCALE F_POINT))
	  )
	'(
	  (ft_Situation (:required (intentional -) (origin f_living)))
	  (ft_phys-obj (group +) (origin f_natural) (intentional +) (mobility F_self-moving))
	  (ft_Abstr-obj (CONTAINER -) (INFORMATION -) (INTENTIONAL -) (SCALE F_VALUE))
	  )
	'(
	  nil
	  (FT_PHYS-OBJ (MOBILITY F_SELF-MOVING) (INTENTIONAL +) (ORIGIN F_HUMAN)
 (GROUP +))
	  (ft_Abstr-obj (SCALE nil) (INTENTIONAL -) (INFORMATION -) (CONTAINER -))
	  )
	)

;; lf-feature-pattern-match
(mapcar (lambda (x y out) (equal out (funcall 'lf-feature-pattern-match x y)))
	'(
	  (ft_Situation (:required (intentional -)))
	  (ft_Situation (:required (intentional -)) (:default (origin f_living)))
	  (ft_phys-obj (group +) (origin f_natural) (intentional +) (mobility F_movable))
	  (ft_Abstr-obj (CONTAINER -) (INFORMATION -) (INTENTIONAL -) (SCALE F_POINT))
	  )
	'(
	  (ft_Situation (:required (intentional -) (origin f_living)))
	  (ft_Situation (:required (intentional -)))
	  (ft_phys-obj (group +) (origin f_human) (intentional +) (mobility F_self-moving))
	  (ft_Abstr-obj (CONTAINER -) (INFORMATION -) (INTENTIONAL -) (SCALE F_VALUE))
	  )
	'(
	  t t t nil
	  )
	)

;; lf-feature-set-merge
(mapcar (lambda (x y out) (equal out (funcall 'lf-feature-set-merge x y)))
	'(
	  (ft_Abstr-obj (INTENTIONAL -) (SCALE F_POINT))
	  (ft_phys-obj (origin f_human))
	  )
	'(
	  (ft_Abstr-obj (CONTAINER -) (INFORMATION -))
	  (ft_phys-obj (origin f_human) (group -))
	  )
	'(
	  (FT_ABSTR-OBJ
	   (:REQUIRED (TYPE F_ANY-TYPE) (GRADABILITY -) (MEASURE -) (SCALE F_POINT)
		      (INTENTIONAL -) (CONTAINER -) (INFORMATION -)))
	  (FT_PHYS-OBJ
 (:REQUIRED (TYPE F_ANY-TYPE) (OBJECT-FUNCTION F_ANY-OBJECT-FUNCTION)
  (MOBILITY F_ANY-MOBILITY)
  (SPATIAL-ABSTRACTION (? SAB F_SPATIAL-POINT F_SPATIAL-REGION))
  (TRAJECTORY -) (INFORMATION -) (CONTAINER -) (GROUP -) (ORIGIN F_HUMAN)
  (INTENTIONAL +) (FORM F_SOLID-OBJECT)))
	  )
	)

;; lf-more-specific-feature-value
; (mapcar (lambda (x y out) (equal out (funcall 'lf-more-specific-feature-value x y)))
; 	'(f_dynamic
; 	  f_atomic
; 	  f_plant
; 	  f_gas
; 	  )
; 	'(
; 	  f_unbounded
; 	  f_extended
; 	  f_living
; 	  f_substance
; 	  )
; 	'(
; 	  f_unbounded
; 	  nil
; 	  f_plant
; 	  f_gas
; 	  )
	
	
; 	)
; ))
(mapcar (lambda (x y out) (equal out (funcall 'more-specific x y
					      :ontology 'lf)))
	'(f_dynamic
	  f_atomic
	  f_plant
	  f_gas
	  )
	'(
	  f_unbounded
	  f_extended
	  f_living
	  f_substance
	  )
	'(
	  f_unbounded
	  nil
	  f_plant
	  f_gas
	  )
	
	
	)
))

(print (list
; ;; kr-subclass
; (mapcar (lambda (x y out) (equal out (funcall 'kr-subclass x y)))
; 	'(
; 	  objective
; 	  intentional-object
; 	  fly
; 	  )
; 	'(
; 	  intentional-object
; 	  objective
; 	  move
; 	  )
; 	'( objective nil fly )
; 	)
;; kr-subclass
	(mapcar (lambda (x y out) (equal out (funcall 'subtype x y
						      :ontology 'kr)))
	'(
	  objective
	  intentional-object
	  fly
	  )
	'(
	  intentional-object
	  objective
	  move
	  )
	'( objective nil fly )
	)

; ;; kr-subpred
; (mapcar (lambda (x y out) (equal out (funcall 'kr-subpred x y)))
; 	'(
; 	  along
; 	  trajectory
; 	  ahead
; 	  )
; 	'(
; 	  trajectory
; 	  along
; 	  geo-loc-predicate
; 	  )
; 	'( along nil ahead )
; 	)
;; kr-subpred
(mapcar (lambda (x y out) (equal out (funcall 'subtype x y
					      :ontology 'kr)))
	'(
	  along
	  trajectory
	  ahead
	  )
	'(
	  trajectory
	  along
	  geo-loc-predicate
	  )
	'( along nil ahead )
	)

; ;; kr-suboperator
; (mapcar (lambda (x y out) (equal out (funcall 'kr-suboperator x y)))
; 	'(
; 	  location-operator
; 	  root-operator
; 	  near
; 	  )
; 	'(
; 	  root-operator
; 	  location-operator
; 	  root-operator
; 	  )
; 	'( location-operator nil near)	
; 	)
;; kr-suboperator
(mapcar (lambda (x y out) (equal out (funcall 'subtype x y
					      :ontology 'kr)))
	'(
	  location-operator
	  root-operator
	  near
	  )
	'(
	  root-operator
	  location-operator
	  root-operator
	  )
	'( location-operator nil near)	
	)

; ;; kr-more-specific-class
; (mapcar (lambda (x y out) (equal out (funcall 'kr-more-specific-class x y)))
; 	'(
; 	  fire-engine
; 	  trans-agent
; 	  plane
; 	  )
; 	'( 
; 	  trans-agent
; 	  fire-engine
; 	  bus
; 	  )
; 	'(
; 	  fire-engine
; 	  fire-engine
; 	  nil
; 	  )
; 	)
;; kr-more-specific-class
(mapcar (lambda (x y out) (equal out (funcall 'more-specific x y 
					      :ontology 'kr)))
	'(
	  fire-engine
	  trans-agent
	  plane
	  )
	'( 
	  trans-agent
	  fire-engine
	  bus
	  )
	'(
	  fire-engine
	  fire-engine
	  nil
	  )
	)

; ;; kr-common-ancestor-class
; (mapcar (lambda (x y out) (equal out (funcall 'kr-common-ancestor-class x y)))
; 	'(
; 	  truck
; 	  show
; 	  shows
; 	  )
; 	'(
; 	  car
; 	  expect
; 	  show
; 	  )
; 	'(
; 	  vehicle
; 	  action
; 	  nil
; 	  )
; 	)
;; kr-common-ancestor-class
(mapcar (lambda (x y out) (equal out (funcall 'common-ancestor x y
					      :ontology 'kr)))
	'(
	  truck
	  show
	  shows
	  )
	'(
	  car
	  expect
	  show
	  )
	'(
	  vehicle
	  action
	  nil
	  )
	)

; ;; kr-parentclass
; (mapcar (lambda (x out) (equal out (funcall 'kr-parentclass x)))
; 	'(
; 	  stop
; 	  expect
; 	  truck
; 	  )
; 	'(
; 	  action-theme
; 	  action
; 	  vehicle
; 	  )
; 	)

; ;; kr-parentpred
; (mapcar (lambda (x out) (equal out (funcall 'kr-parentpred x)))
; 	'(
; 	  south-of
; 	  approx-at-loc
; 	  kr-pred-root
; 	  )
; 	'(
; 	  rel-loc
; 	  at-loc
; 	  nil
; 	  )
; 	)

; ;; kr-parentop
; (mapcar (lambda (x out) (equal out (funcall 'kr-parentop x)))
; 	'(
; 	  location-operator
; 	  root-operator
; 	  near
; 	  )
; 	'(
; 	  root-operator
; 	  nil
; 	  location-operator
; 	  )
; 	)
;; kr-parentclass
(mapcar (lambda (x out) (equal out (funcall 'get-parent x :ontology 'kr)))
	'(
	  stop
	  expect
	  truck
	  )
	'(
	  action-theme
	  action
	  vehicle
	  )
	)

;; kr-parentpred
(mapcar (lambda (x out) (equal out (funcall 'get-parent x :ontology 'kr)))
	'(
	  south-of
	  approx-at-loc
	  kr-pred-root
	  )
	'(
	  rel-loc
	  at-loc
	  nil
	  )
	)

;; kr-parentop
(mapcar (lambda (x out) (equal out (funcall 'get-parent x :ontology 'kr)))
	'(
	  location-operator
	  root-operator
	  near
	  )
	'(
	  root-operator
	  nil
	  location-operator
	  )
	)

;; kr-slots
(mapcar (lambda (x out) (equal out (funcall 'kr-slots x)))
	'(
	  cargo
	  truck
	  accident
	  happen
	  )
	'(
	  nil
	  ((AT-LOC))
	  ((REASON-FOR KR-ROOT) (AT-TIME TIME-LOC) (AT-LOC GEO-LOC)
	   (APPROX-AT-TIME TIME-LOC))
	  ((REASON-FOR KR-ROOT) (AT-TIME TIME-LOC) (AT-LOC GEO-LOC)
	   (APPROX-AT-TIME TIME-LOC) (EVENTUALITY EVENT))
	  )
	)
	
	))
