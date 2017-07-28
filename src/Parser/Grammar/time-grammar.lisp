;;;
;;;; time-grammar.lisp
;;;;
;;;;
;;;; Myrosia 02/14/00
;;;; This is a grammar to handle times.
;;;; I put it in a separate file to make it easier to see things.


;; Myrosia 01/25/02
;; New time grammar using time semantics

;; Myrosia  07/06/00;; An attempt to make a new version of the grammar
;; rather than do explicit conversion most of the time
;; introduce a feature "time-converted"
;; and make it "+" for those values that have gone throught the conversion and shouldn't again
(in-package :W)

;; First the basic value grammar
;; wherever we can, we fill in sem values defined on words, but when we can't, we will add them later

;;(cl:setq *grammar-time-Values*
(parser::augment-grammar
  '((headfeatures
     (NP VAR KIND NAME PRO SPEC ATTACH Changeagr transform lex headcat)
     (PP VAR KIND CASE MASS NAME agr SEM PRO SPEC QUANT ATTACH transform lex headcat)
     (ADVBL SEM transform lex headcat)
     (VALUE VAR transform lex headcat)
     (time-value var transform headcat)
     )			

    ;;   BASIC TIME-OF-DAY VALUES
    
   ;;  e.g., 5
    ((time-value 
      (Hour ?r) (am-pm ?x) (minute ?m) 
      (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time) (f::time-scale f::point)))
      )
     -time0>  .97  ;; we reduce the weight to prefer straight number interpretations
     (head (NUMBER (VAL ?r) (NTYPE (? n W::HOUR12))))
     )

    ;;  e.g., 5:30
    ((time-value 
      (Hour ?r) (Minute ?r2) (am-pm ?x) (time-converted +)
      (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time) (f::time-scale f::point)))
      )
     -time1> 1.0
     (head (NUMBER (VAL ?r) (NTYPE (? n W::HOUR12 W::HOUR24))))
     (punc (lex punc-colon))
     (NUMBER (VAL ?r2) (NTYPE W::MINUTE)))
    
    ;;  E.G., FIVE THIRTY
    ((time-value 
      (Hour ?r) (Minute ?r2) (am-pm ?x)
      (sem ($ f::time (f::intentional -) (f::information -) (f::time-function f::clock-time) (f::time-scale f::point)))
      )
     -time1A> .98 ;; don't prefer times over plain numbers  -> "one fifty" should be first a number, not a time
     (head (NUMBER (VAL ?r) (NTYPE (? n w::HOUR12 w::HOUR24))))
     (NUMBER (VAL ?r2) (NTYPE w::MINUTE)))

    ;;  E.G., FIVE oh three
    ((time-value 
      (Hour ?r) (Minute ?r2) (am-pm ?x)
      (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time)))
      )
     -time1B>
     (head (NUMBER (VAL ?r) (NTYPE (? n w::HOUR12 w::HOUR24))))
     (NUMBER (LEX oh))
     (NUMBER (VAL ?r2) (NTYPE w::DIGIT)))

    ;;  E.G., ten minutes to eight
    ((time-value
      (Hour ?adjhour) (Minute ?adjminute) (am-pm ?x);;))))
      (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time)))
      )
     -time-minutes-to-hour> 1.0
     (NUMBER (VAL ?minute) (NTYPE (? n w::digit w::MINUTE)))
     (word (lex w::minutes))
     (PREP (lex w::to))
     (head (NUMBER (VAL ?hour) (NTYPE w::HOUR12)))
     (compute-val-and-ntype (expr (- ?hour 1)) (newval ?adjhour) (ntype ?n1))
     (compute-val-and-ntype (expr (- 60 ?minute)) (newval ?adjminute) (ntype ?n2)))

    ;;  E.G., ten to eight
    ((time-value
      (Hour ?adjhour) (Minute ?adjminute) (am-pm ?x);;))))
      (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time)))
      )
     -time-minute-to-hour> 1.0
     (NUMBER (VAL ?minute) (NTYPE (? n w::digit w::MINUTE)))
     (PREP (lex w::to))
     (head (NUMBER (VAL ?hour) (NTYPE w::HOUR12)))
     (compute-val-and-ntype (expr (- ?hour 1)) (newval ?adjhour) (ntype ?n1))
     (compute-val-and-ntype (expr (- 60 ?minute)) (newval ?adjminute) (ntype ?n2)))

    ;;  E.G., ten past/after eight
    ((time-value
      (Hour ?hour) (Minute ?minute) (am-pm ?x);;))))
      (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time)))
      )
     -time-minute-past-hour> 1.0
     (NUMBER (VAL ?minute) (NTYPE (? n w::digit w::MINUTE)))
     (word (lex (? ww w::past w::after)))
     (head (NUMBER (VAL ?hour) (NTYPE w::HOUR12)))
     )

    ;; Half/quarter past twelve

     ((time-value ;;(LF (% time-description
		  ;; (sem ($ f::time (f::intentional -) (f::information -) (f::time-function f::clock-time)))
		  ;; (Constraint (& 
       (Hour ?hour) (Minute ?minute) (am-pm ?am);;))))
       (AGR 3s)  
       (time-converted +)
       (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time)))
       )
     -time-half-past> 1.0
      (ordinal (lex (? x w::half w::quarter)) (lf (nth ?n)))
      (adv (lex past))
      (head (time-value (hour ?hour) (minute -) (am-pm ?am)))
      ;;(head (NUMBER (VAL ?hour) (NTYPE w::HOUR12)))
      (compute-val-and-ntype (expr (/ 60 ?n)) (newval ?minute) (ntype ?n2))
      )

    ;; half/quarter past noon/midnight
    ((time-value ;;(LF (% time-description
		 ;;  (sem ($ f::time (f::intentional -) (f::information -) (f::time-function f::clock-time)))
		 ;;  (Constraint (& 
      (Hour 12) (Minute ?minute) (am-pm ?ref);;))))
      (AGR 3s)  
      (time-converted +)
      (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time)))
      )
     -time-half-past1> 1.0
      (ordinal (lex (? x w::half w::quarter)) (lf (nth ?n)))
      (adv (lex past))
      ;;(head (NUMBER (VAL ?hour) (NTYPE w::HOUR12)))
     (head (pro (lf (:* ONT::TIME-OBJECT (? ref W::NOON W::MIDNIGHT)))))
     (compute-val-and-ntype (expr (/ 60 ?n)) (newval ?minute) (ntype ?n2))
      )

    ;; quarter to six
    ((time-value ;;(LF (% time-description
      ;;(sem ($ f::time (f::intentional -) (f::information -) (f::time-function f::clock-time)))
      ;;(Constraint (& 
      (Hour ?adjhour) (Minute ?minute) (am-pm ?am);;))))
      (AGR 3s)  
      (time-converted +)
      (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time)))
      )
     -time-quarter-to> 1.0
     (ordinal (lex (? x w::half w::quarter)) (lf (nth ?n)))
     (prep (lex w::to))
     (head (NUMBER (VAL ?hour) (NTYPE w::HOUR12)))
     (compute-val-and-ntype (expr (- ?hour 1)) (newval ?adjhour) (ntype ?n1))
     (compute-val-and-ntype (expr (- 60 (/ 60 ?n))) (newval ?minute) (ntype ?n2))
     )
    
    ;;  e.g.,  5 am, 5 pm
    ((time-value ;;(LF (% time-description (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time) (f::time-scale f::point)))
             ;;(constraint ?newcon)))
      (Hour ?hour) (Minute ?minute) (am-pm (:* ont::TIME-OBJECT (? x AM PM)))
      (AGR 3s) 
      (sem ($ f::time (f::time-function f::clock-time) (f::intentional -) (f::information -) (f::time-scale f::point)))
      )

     -time2> 1.0
     ;;(head (NUMBER (VAL ?n) (NTYPE w::HOUR12)))
     (head (time-value (date-added -)
		       (Hour ?hour) (Minute ?minute) (am-pm -)))
		 ;; (LF (% time-description (sem ?sem) (constraint ?con)))))
     (N (SORT PRED)      
      (LF (:* ont::TIME-OBJECT (? x AM PM))))
      )
     ;;(add-to-conjunct (val (am-pm ?lf)) (old ?con) (new ?newcon)))

    ;;  noon, midnight
    ((time-value
      (hour 12) (am-pm ?ref)
      (var ?v)
      (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time) (f::time-scale f::point)))
      )
     -noon1> 1.0
     (head (pro (lf (:* ONT::TIME-OBJECT (? ref W::NOON W::MIDNIGHT)))))
     )
    
    ;; Time + temporal pro  e.g., 12 noon, ...
    ((time-value
      (hour 12) (am-pm ?ref)
      (var ?v)
      (sem ($ f::time (f::time-function f::clock-time) (f::intentional -) (f::information -) (f::time-scale f::point)))
      )
     -time-noon> 1.0
     (head (time-value 
			(hour 12)
			;;(sem ?sem) (lf ?lf)
			;;(lf (% time-description (constraint ?c1)))
	      ))
     (pro (lf (:* ONT::TIME-OBJECT (? ref W::NOON W::MIDNIGHT))))
     ;;(add-to-conjunct (val (am-pm ?ref)) (old ?c1) (new ?newc))
     )

     ;;  e.g., 5 o'clock
    ((time-value
      (Hour ?n) (am-pm ?x)
      (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time) (f::time-scale f::point)))
      )
     -time3>
     (head (NUMBER (VAL ?n) (NTYPE w::HOUR12)))
     (word (lex w::O^clock))
     )

    ;; time + date: 9AM May 4th,  the day must be specified
     ((time-value
       (Hour ?hour) (Minute ?min) (am-pm ?x) (year ?year) (dow ?dow) (day ?day) (month ?month) (century ?century) (era ?era)
       (date-added +)
       (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time) (f::time-scale f::point)))
       )
     -time-date>
      (head (time-value (date-added -)
			(Hour ?hour) (Minute ?min) (am-pm ?x)))

      (date (int +) (year ?year) (dow ?dow) (day ?day) (month ?month) (century ?century) (era ?era) (day-specified +))
      )
     
     ;; date + time: May 4th 9AM,  the day must be specified
    ((time-value
      (Hour ?hour) (Minute ?min) (am-pm ?x) (year ?year) (dow ?dow) (day ?day) (month ?month) (century ?century) (era ?era)
      (date-added +)
      (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::clock-time) (f::time-scale f::point)))
      )
     -time-date1>
     (date (int +) (year ?year) (dow ?dow) (day ?day)
      (month ?month) (century ?century) (era ?era) (day-specified +))
     (head (time-value (date-added -)
		       (Hour ?hour) (Minute ?min) (am-pm ?x)))
     )
    
    ;; e.g., military speak:  e.g., day 5; business speak: week 2

    ((value (LF (% time-description (sem ?sem) (class ?lf)
		   (constraint (& (?lx ?n)))))
      (sem ?sem)
      (AGR 3s) 
      (time-converted +)
      )
     -day-n>
     (head (N (lex (? lx day week month year)) (sem ?sem) (lf ?lf)))
     (number (VAL ?n)))
    
    ;; e.g., between day 5 and day 7
    
    ((value (LF (% time-description (start ?date1) (end ?date2)))
      (AGR 3s) 
      (time-converted +)
      (sem ($ f::time (f::intentional -) (f::information -)(f::time-function f::time-interval) (f::time-scale f::interval)))
      )
     -interval1>
     ;; Myrosia 03/08/00 Matching class values avoid cases like
     ;; "Between 10 am and Jul 25"
     (head (value 
	    (time-converted +)
	    (sem ?sem)
	    ;;	    (sem ($ f::time (f::time-function (? fun f::date f::time-of-day))))
	    (LF ?date1)
	    (LF (% time-description))
	    )
      )
     (conj (LF ont::and))
     (value 
      (time-converted +)
      (sem ?sem)
      (LF ?date2)
      (LF (% time-description))
      ))
    
    #||;;  iteration: (he spends three dollars) a day
    ((ADVBL (ARG ?arg) 
      (SORT BINARY-CONSTRAINT) (var *)
      (LF (% PROP (var *) (CLASS ONT::ITERATION-PERIOD) 
	     (Constraint (& (FIGURE ?arg) (GROUND ?v)))))
      (ATYPE w::post) (focus ?v)
      (lex ?hlex) (headcat ?hcat)
      (ARGUMENT (% (? x W::VP W::S)))
      (SEM ?sem))
     -a-day-advbl> .98
     (head (np (agr 3s) (var ?v)
	       (sem ($ f::time (f::time-scale f::interval)))
	       (lf (% description (status ont::indefinite) (class ont::time-interval) (constraint (& (scale f::time-measure-scale)))))
	       ;; exclude things like "lunch"
	       (mass count)
	       )))||#

    ((ADVBL (ARG ?arg) 
      (SORT BINARY-CONSTRAINT) (var *)
      (LF (% PROP (var *) (CLASS ONT::ITERATION-PERIOD) 
	     (Constraint (& (FIGURE ?arg) (GROUND ?v)))))
      (ATYPE w::post) (focus ?v)
      (lex ?hlex) (headcat ?hcat)
      (SUBJVAR ?subjvar)
      (ARGUMENT (% (? x W::VP W::S) (sem ?sss) (subjvar ?subj)))
      (SEM ?sem))
     -rate-advbl> .98
     (head (np (agr 3s) (var ?v)
	       ;;(sem ($ f::abstr-obj (f::type f::rate)))
	       (lf (% description (status ont::indefinite) (class ont::rate))
	      	       )))
    
    )
    ))

(parser::augment-grammar
  '((headfeatures
     (VALUE transform)
     )		
   
    ;; rate expressions
    ;; 7 miles per hour; 7 degrees per second
    ((np (LF (% description (var ?v) (class ont::rate) (status ont::indefinite) (constraint (& (repeats ?v1) (over-period ?per)))))
            (var ?v) (case (? case sub obj)) (SORT PRED) (AGR 3s)
            (time-converted +)
            (sem ($ f::abstr-obj (f::intentional -) (f::information -) (f::mobility -)
		    (f::type ont::rate) (f::scale ont::rate-scale)))
            )
     -units-per-period> 
     (head (np (lf ?lf) (sort unit-measure) (wh -) (var ?v1)
	       ))
     (advbl (var ?v) (lf (% prop (class ont::iteration-period)))
		(focus ?per)	    ;;(constraint (& (val ?per))))))
      ))

    ;; $125 a share
    ((np (LF (% description (var *) (class ont::rate) (status ont::indefinite) (constraint (& (repeats ?v1) (over-period ?per)))))
            (var *) (case (? case sub obj)) (SORT PRED) (AGR 3s)
            (time-converted +)
            (sem ($ f::abstr-obj (f::intentional -) (f::information -) (f::mobility -) 
		    (f::type ont::rate) (f::scale ont::rate-scale)))
            )
     -units-per-period2> .98
     (head (np (lf ?lf) (sort unit-measure) (wh -) (var ?v1)
	       ))
     (head (np (agr 3s) (var ?per) (lf (% description (status ont::indefinite))) (mass count)
	       )))


     ;; m/s = meters per second
      ((np (LF (% description (var ?v) (class ont::rate) (status indefinite) (constraint (& (repeats ?v1) (over-period ?per)))))
            (var ?v) (case (? case sub obj)) (SORT UNIT-MEASURE) (AGR 3s)
            (time-converted +)
            (sem ($ f::abstr-obj (f::intentional -) (f::information -) (f::mobility -)
		    (f::type ont::rate) (f::scale ont::rate-scale)))
	)
       -units-slash-time>
       (head (np (lf ?lfd) (sort unit-measure) (wh -) (var ?v1)
	          ))
       (punc (lex (? l slash punc-slash)))
       (n (w::agr w::3s) (var ?v)	(LF ?per) (mass count)
	(sem ($ f::time (f::scale ont::duration-scale)))
	))
  
    ;; e.g., the gdp / gtp ratio

    ((n1 (sort pred) (var ?v) (class (:* ONT::RATIO ?x)) (agr ?agr) (CASE (? case SUB OBJ))
      (sem ?sem) (lex ?lex) (subcat -) (restr (& (figure ?v1) (ground ?v2))))
     -ratio1> 1
     (head (np (lf ?num) (bare-np +) (wh -) (var ?v1) (agr 3s)))
     (punc (lex (? l slash punc-slash)))
     (np (w::agr w::3s) (bare-np +) (var ?v2) (LF ?denom))
     (n (LF (:* ONT::RATIO ?x)) (var ?v) (sem ?sem) (agr ?agr) (lex ?lex))
     )

    ((n1 (sort pred) (var ?v) (class (:* ONT::RATIO ?x)) (agr ?agr) (CASE (? case SUB OBJ))
      (sem ?sem) (lex ?lex) (subcat -) (restr (& (figure ?v1) (ground ?v2))))
     -ratio2> 1.1  ;; override parser's reluctance to make bare NP's
     (head (np (lf ?num) (wh -) (bare-np +) (var ?v1) (agr 3s)))
     (np (w::agr w::3s) (var ?v2) (bare-np +) (agr ?agr) (LF ?denom))
     (n (LF (:* ONT::RATIO ?x)) (var ?v) (sem ?sem) (lex ?lex))
     )

    #|
    ((adj (gap ?gap) (var ?v) (lex (?n ?w))
      (input (?n ?w)) (LF (:* ont::quantitive-relation (?n ?w)))
      (sem ($ f::abstr-obj))
      (coerce -) (arg ?arg)
      (atype w::central) (sort pred) (template lxm::central-adj-templ)
      (argument-map ont::FIGURE) (argument (% NP (sem ($ f::situation (f::type ont::change-magnitude))))))
     -n-fold>
     (number (var ?v) (val ?n))
     (word (lex (? w w::fold w::times))))
    |#
    
))


;;======================================================================================
;;
;;     TIME AND LOCATION PHRASES
;;
;;======================================================================================

;;(cl:setq *grammar-TIME-LOC*
(parser::augment-grammar
  '((headfeatures
     (advbl headcat lex)
     (np pred  Changeagr transform headcat))

    
    ;;  5 a.m., 5 o'clock, 5:30, plus times with dates, and dates, all end up here
   
      ((name 
	(class ONT::TIME-LOC) (name +) 	(LF ONT::TIME-LOC) 
	(restr (&  (Hour ?hour) (Minute ?min) (am-pm ?x) (year ?year) (dow ?dow) (day ?day) (month ?month) (century ?century) (era ?era)))
	(sem ?sem) (time-converted +)
	(var ?v) (CASE (? case sub obj)) (SORT PRED) (AGR 3s)
	(lex ?hl) (headcat ?hc)
	)
       -time-value-np1> .98 ; only use these for times -- check "one fifty" if you change this -- it should be a number 
       (head (time-value (var ?v)
	      (sem ?sem)
	      (Hour ?hour) (Minute ?min) (am-pm ?x) (year ?year) (dow ?dow) (day ?day) (month ?month) (century ?century) (era ?era)
	      (lex ?hl) (headcat ?hc)
	      )))
       
    ;; for the remaining VALUES generated 
     ((np (lf (% description 
		  (status direct)
		  (var ?v)
		  (sort Individual)
		  (class ONT::TIME-LOC)
		  (constraint ?con)
		  (sem ?sem)
		  ))
	(sem ?sem)
	(var ?v) (CASE (? case sub obj)) (SORT PRED) (AGR 3s)
	(lex ?hl) (headcat ?hc)
	)
       -value-np1> .98 
       (head (value  (time-converted +) 
	      (sem ?sem)
	      (LF ?lf)
	      (lf (% time-description (constraint ?con)))
		  (lex ?hl) (headcat ?hc)
	      ))
      )       

    ;;  with ADVBL modifier, e.g., 10 o'clock in the morning
    ((time-value
      (Hour ?hour) (Minute ?min) (am-pm ?val) (year ?year) (dow ?dow) (day ?day) (month ?month) (century ?century) (era ?era)
	      
	(sem ?sem)
	(var ?v) (CASE (? case sub obj)) (SORT PRED) (AGR 3s)
	(lex ?hl) (headcat ?hc)
	)
     -value-mod-np1> 
     (head (time-value (var ?v)
		   (sem ?sem)
		   (Hour ?hour) (Minute ?min) (am-pm -) (year ?year) (dow ?dow) (day ?day) (month ?month) (century ?century) (era ?era)
		   ))
     (advbl (VAR ?mod) (WH -) (LF (% prop (class ont::time-span-rel))) (focus ?val)
	    (ATYPE POST) (ARG ?v) (GAP -) 
      )
    
     )
      
    ;;  e.g., Tuesday morning, Monday a.m., Monday early afternoon
    ((time-value
      (Hour ?hour) (Minute ?min) (am-pm ?v1) (dow ?dow) (day ?day)
      (SEM ($ f::time (F::Time-Function (? xx F::day-period F::day-part F::day-point))))
      (VAR ?v)
      )
     -time-np3> 1.0
     (date
      (dow ?dow) (day -) (month -) (century -) (era -)
      (VAR ?v)
      )
     (head (np (SORT pred) 
	       (SEM ($ f::time (F::Time-Function (? xx F::day-period F::day-part F::day-point))))
	       (var ?v1)
	       (Lex ?lf2)))
     )

        
    ;; do it 3 times
    ((advbl (sort constraint)
      (argument (% S (var ?argvar) (sem ($ f::situation)) ))
      (subcatsem ?valsem) (generated +)
      (role ont::frequency)
      (var *)
      ;;(var ?var)
      (lf (% PROP (class ONT::repetition) (var *) (constraint (& (FIGURE ?argvar) (GROUND ?!val)))))
      (atype post)
      (arg ?argvar)
      )
     -repetition-number-advbll>
     ;;(head (number (val ?val)))
     (head (cardinality (var ?val) (agr 3p)))
     (word (lex (? t W::time W::times)))
     )

    ;; do it many times
    ;; 2011/09/19 this rule was commented out -- reinstating it so that "many times" parses as an adverbial
    ;; as in "he woke up many times during the night"
    ((advbl (sort constraint)
      (argument (% S (var ?argvar) (sem ($ f::situation)) ))
      (subcatsem ?valsem) (generated +)
      (role ont::frequency)
      (var *)
      ;;(var ?var)
      (lf (% PROP (class ONT::repetition) (var *) (constraint (& (FIGURE ?argvar) (GROUND ?!card)))))
      (atype post)
      (arg ?argvar)
      )
     -repetition-quan-advbll>
     (head (spec (mass count) (lf ont::indefinite-plural)))
     (word (lex (? t W::time W::times)))
     )

    ;; every/each/some/many/ day/morning/year

    ((advbl (sort constraint)
	    (argument (% S (var ?argvar) (sem ($ f::situation)) ))
	    (subcatsem ?valsem) (bare-advbl +)
	    (role ont::frequency)
	    (var *) (case ?case)
	    ;;(var ?var)
	    (lf (% PROP (class ONT::iteration-period) (var *) (constraint (& (FIGURE ?argvar) (GROUND ?valvar)))))
	    (atype post)
	    (arg ?argvar)
	    )
     -period-value-advbl1> .98
     (head (np (sem ?valsem) (var ?valvar)
	       (sem ($ f::time (f::time-scale F::INTERVAL))) (headless -)
	       (LF (% DESCRIPTION (status (? xx ont::quantifier)))) ;; indefinite))))   
	       ))
     )

     ;; 30 miles per hour
     ((np (sort unit-measure)
       (var ?v) (frequency +) (case ?case)
       (lf (% description (class ?class) (var ?v) (constraint ?constr) (status ont::indefinite)))
       (sem ?sem)
      )
     -frequency-value-np> 
     (head (value (dow -)
		  (lf (% description (class ?class) (var ?v) (constraint ?constr) (status value)))
		  (sem ($ f::abstr-obj (f::scale ont::rate-scale)))
		  (sem ?sem)))
     )

     ;; this morning, this year, that week, next week, ...
      ((advbl (sort constraint)
	    (argument (% S (var ?argvar) (sem ($ f::situation)) ))
	    (subcatsem ?valsem)  (bare-advbl +)
	    (var *)
	    (lf (% PROP (class ONT::event-time-rel) (var *) (constraint (& (FIGURE ?argvar) (GROUND ?valvar)))))
	    (atype (? atp pre post))
	    (arg ?argvar)
	    )
     -deictic-time-advbl> 1
     (head (np (sem ?valsem) (var ?valvar) (headless -) (coerced -)
	       (sem ($ f::time (f::time-scale F::INTERVAL)))
	       (LF (% DESCRIPTION (status ont::definite) (class ont::time-object);;(CONSTRAINT (& (proform (? cr W::THIS W::THAT W::THOSE W::THESE)))))) ;;(? cr this that those these))))))
	       ))
     )))

    ;;  Special construction for last year/next week/ etc which doesn't seem to generalize to non-temporal
    ((np (var ?v) (sort pred) (agr 3s) 
      (LF (% description (var ?v) (status ont::definite)
	     (class ont::time-loc) (constraint (& (proform ?lex) (extent ?class))) (sem ($ f::time (f::time-scale F::INTERVAL)))))
      (sem ($ f::time (f::time-scale F::INTERVAL))))
      -next-last-time1> 1
      (adjp (lex (? x next last)) (lex ?lex)
       (var ?adjv) (arg ?v))
      (Head (n1 (sem ?valsem) (var ?v) (class ?class) (restr ?r)
	       (sem ($ f::time (f::time-scale F::INTERVAL)))
	       )))

    ;; E.G., last February 15th, next Tuesday
      
    ((np (var ?v) (sort pred)  (LF (% description (var ?v) (status ont::definite)
		(class ONT::TIME-LOC) (constraint ?new) (sem ($ f::time (f::time-scale F::INTERVAL)))))
      (sem ($ f::time (f::time-scale F::INTERVAL))))
     -next-last-date> 1
     (adjp (lex (? x next last)) (lex ?lex)
      (var ?adjv) (arg ?v))
     (head (DATE (var ?v) (month ?m) (year -) (day ?d) (dow ?dow)))
     (append-conjuncts (conj1 (& (month ?m) (day ?d) (dow ?dow) (mods ?adjv)))
      (conj2 ?r) (new ?new))
     )


    ;;  Special construction for the last three hours/two days/...

    ((np (sort pred) (agr 3s) (var *) (LF (% description (var *) (status ont::definite)
		(class ONT::TIME-LOC) (constraint (& (proform ?lex) (extent ?v)))
		(sem ($ f::time (f::time-scale F::INTERVAL)))))
      (sem ($ f::time (f::time-scale F::INTERVAL))))
      -the-last-dur> 1.0
     (art (lex the))
     (adjp (lex (? lex next last))
       (var ?adjv) (arg ?valvar))
     (Head (np (sem ?valsem) (var ?v) (sort unit-measure) (restr ?r)
	       (sem ($ f::abstr-obj (f::scale ont::DURATION-SCALE)))
	       ))
     
     )
    
    ;;  Special construction for quantifications of dates: every Monday, each june 1st, this friday, 
    
    ((np (var ?v) (LF (% description (var ?v) (status ont::quantifier)
		(class ONT::TIME-LOC) (constraint ?new) (sem ($ f::time (f::time-scale F::INTERVAL)))))
      (sem ($ f::time (f::time-scale F::INTERVAL))))
     -quant-date>
     (spec (pred (? x ont::every ont::each)) (restr ?r) (arg ?v))
     (head (DATE (var ?v) (month ?m) (year -) (day ?d) (dow ?dow)))
     (append-conjuncts (conj1 (& (month ?m) (day ?d) (dow ?dow)))
      (conj2 ?r) (new ?new))
     )

   
    
    ))


;; Myrosia 05/18/00 changed the entire rule format to get the name structure right
;; Myrosia 04/22/02 removed lex as a head feature. In our grammar, we use lex from names as their main ID
;; therefore, we can't make lex a head feature in things like "engine e2", it has to be complex

(parser::augment-grammar
  '((headfeatures 
     (name var agr transform headcat)
     (N1 var lex transform sem quantity subcat argument indef-only headcat)
     (N var lex transform lf sem agr headcat)
     (NP headcat lex)
     (number-sequence headcat lex)
     (mixed-sequence headcat lex)
     (nname headcat)
     (rnumber headcat lex restr var)
     (number headcat)
     )    

    ;; Basic Number Expressions
    ;;  e.g., thirty one, twenty seven 
    ((number (VAL ?newval) (agr 3p) (lex (?l1 ?l2)) (ntype ?ntype)
      (var *) (LF ?lf) (coerce ?coerce) (sem ?sem)
      (nobarespec ?nbs)
	     )
     -tenty-digit>
     (head (number (nobarespec ?nbs) (lf ?lf) (val ?!v1) (lex ?l1) (sem ?sem)
		   (NTYPE w::TENS) (coerce ?coerce) (digits -)))
     (number (val ?!v2) (lex ?l2) (NTYPE w::DIGIT) (coerce ?coerce) (digits -))
     (compute-val-and-ntype (expr (+ ?!v1 ?!v2)) (newval ?newval) (ntype ?ntype)))

    ;; Written numbers with commas, e.g., 1,939 - we used to remove the commas in tokenization, but this
    ;;   leads to unfortunate parser like 1,939 parsing as a year!
    ((number (VAL ?newval) (agr 3p) (lex (?l1 ?l2)) (ntype ?ntype)
      (var *) (LF ?lf) (coerce ?coerce) (sem ?sem) (comma +) (digits ?newdigits)
      (nobarespec ?nbs)
	     )
     -comma-number> 1
     (head (number (nobarespec ?nbs) (lf ?lf) (val ?!v1) (lex ?l1) (sem ?sem)
		   (coerce ?coerce) (digits ?!digits1)))
     (punc (lex W::punc-comma))
     (number (val ?!v2) (lex ?l2) (number-digits 3)
      ;;(ntype (? x w::threedigit w::zero W::twodigit)) 
      (coerce ?coerce) (digits ?!digits2))
     (compute-val-and-ntype (expr (w::stringappend ?!digits1 ?!digits2)) (newval ?newdigits))
     (compute-val-and-ntype (expr (w::string-to-number ?newdigits)) (newval ?newval) (ntype ?ntype)))

    ;; allowing agr to be 3s OR 3p -- 0.1 meter(s) per second
    ;; decimal digit e.g., one . seven, one point seven 
    ((number (VAL ?newval) (agr (? ag 3s 3p)) (lex (?l1 ?l2)) 
      (var *) (LF ?lf) (ntype w::decpoint) (coerce ?coerce)  (sem ?sem)
      (nobarespec +) ;; impossible to have this as a bare specifier
	     )
     -decimal-digit>
     (head (number (lf ?lf) (val ?!v1) (lex ?l1) (sem ?sem) (fraction -)  ;;(NTYPE (? ntt1 w::DIGIT w::ZERO))
		   (coerce ?coerce)))
     (punc (lex (? l point punc-period)))
     (number (val ?!v2) (lex ?l2) (NTYPE (? ntt2 w::DIGIT w::TWODIGIT w::THREEDIGIT w::ZERO)) (coerce ?coerce))
     (compute-val-and-ntype (expr (decimal-point ?!v1 ?!v2)) (newval ?newval) (ntype ?ntype)))

    ;; fractions expressed with ordinals: two thirds
    ((number (VAL ?newval) (agr ?agr) (lex (?l1 ?l2)) 
      (var *) (LF ?lf) (ntype w::fraction) (coerce ?coerce) (sem ?sem) (fraction +)
      (nobarespec +) ;; impossible to have this as a bare specifier
	     )
     -ordinal-fraction>
     (head (number (lf ?lf) (val ?!v1) (lex ?l1) (agr ?agr) (coerce ?coerce) (sem ?sem) (fraction -) ))
     (ordinal (lf (nth ?!ordnum)) (lex ?l2) (agr ?agr) (NTYPE w::fraction) (coerce ?coerce))
     (compute-val-and-ntype (expr (fraction ?!v1 ?!ordnum)) (newval ?newval) (ntype ?ntype)))
    
    ;; fraction expressed with a slash: 1 slash 3
    ((number (VAL ?newval) (agr ?agr) (lex (?l1 ?l2)) 
      (var *) (LF ?lf) (ntype w::fraction) (coerce ?coerce) (sem ?sem) (fraction +)
      (nobarespec +) ;; impossible to have this as a bare specifier
	     )
     -slash-fraction>
     (head (number (lf ?lf) (val ?!v1) (agr ?agr) (lex ?l1) (coerce ?coerce) (sem ?sem) (fraction -)))
     (punc (lex (? l slash punc-slash)))
     (number (val ?!v2) (lex ?l2) (NTYPE (? ntt2 w::DIGIT w::TWODIGIT w::THREEDIGIT)) (coerce ?coerce) (fraction -))
     (compute-val-and-ntype (expr (fraction ?!v1 ?!v2)) (newval ?newval) (ntype ?ntype)))

  ;; special case of 'a' plus ordinal, where 'a' = one  e.g., a third

    ((number (VAL ?newval) (agr 3s) (lex ?l2) ;;(lex (?l1 ?l2)) ;;me
			 (ntype w::fraction) (unit +)
      (var *) (LF ?lf) (coerce ?coerce) (nobarespec ?nbs) (sem ?sem)
      )
     -a-ordinal>
     (word (lex a))
     (head (ordinal (lf (nth ?!ordnum)) (lex ?l2) (ntype w::fraction) (digits -) (sem ?sem)))
     (compute-val-and-ntype (expr (fraction 1 ?!ordnum)) (newval ?newval) (ntype ?ntype)))

    ;; fractions: I cut the pizza into fifths.
   #|| ((number (VAL ?newval) (agr 3p) (lex ?l2) ;;(lex (?l1 ?l2)) ;;me
			 (ntype w::fraction) (unit +)
      (var *) (LF ?lf) (coerce ?coerce) (nobarespec ?nbs) (sem ?sem)
      )||#
    ((NP (SORT PRED)
	 (var ?v) (class ?c) ;(Class ?lf) ;(sem ?sem)
	 (agr 3p) ;(agr ?agr)
	 (case (? cas sub obj -)) (sem ?xx) ;(sem (? xx ($ F::phys-obj)))
      (LF (% Description (Status Ont::indefinite-plural) (var ?v) ;(Sort Individual)
	     (class (? c ont::PHYS-OBJECT)) (lex ?l2) (sem (? xx ($ F::phys-obj)))
	     (transform ?transform)  (generated ?gen)
	     (constraint (& (quan ?newval) (refset (% *PRO* (VAR *)
						      ;(CLASS ONT::PHYS-OBJECT) (sem (? xx ($ F::phys-obj)))
						      (CLASS ?c) (sem ?xx) 
						      ))
	     ))))
      (mass count) ;(name +)
      (simple +) (time-converted ?tc) (generated ?gen)
      (postadvbl ?gen) ;; swift -- setting postadvl to gen as part of eliminating gname rule but still allowing e.g. truck 1
      )
     -ordinal-bare-plural>
     (head (ordinal (lf (nth ?!ordnum)) (var ?v) (lex ?l2) (ntype w::fraction) (digits -) (sem ?sem) (agr 3p)))
     (compute-val-and-ntype (expr (fraction 1 ?!ordnum)) (newval ?newval) (ntype ?ntype)))
    
    
   ;; decimal digit less than one e.g., . seven, point seven five
    ((number (VAL ?newval) (agr 3p) (lex (?l1)) 
      (var *) (LF ?lf) (ntype w::decpoint) (coerce ?coerce)  (sem ?sem)
      (nobarespec +) ;; impossible to have this as a bare specifier
	     )
     -point-decimal-digit>
     (punc (lex (? l point punc-period)))
     (head (number (lf ?lf) (val ?!v1) (lex ?l1) (coerce ?coerce) (sem ?sem)
		   (NTYPE (? ntt1 w::DIGIT w::TWODIGIT w::THREEDIGIT w::ZERO))))
     (compute-val-and-ntype (expr (decimal-point 0 ?!v1)) (newval ?newval) (ntype ?ntype)))
    
    ;; number unit  e.g., fifteen hundred, three hundred thousand

    ((number (VAL ?newval) (agr 3p) (lex ?l2) ;;(lex (?l1 ?l2)) ;;me
			 (ntype ?ntype) (unit +)
      (var *) (LF ?lf) (coerce ?coerce) (nobarespec ?nbs) (sem ?sem)
      )
     -number-unit>
     (number (lf ?lf) (val ?!v1) (lex ?l1) (coerce ?coerce) (nobarespec ?nbs) ;;(has-digits -)  ;; commented out as it excludes "100 million"   JFA 4/09
      (sem ?sem))
     (head (number-unit (val ?!v2) (lex ?l2) (digits -)))
     (less-than (val1 ?!v1) (val2 ?!v2))    ;; eliminate two thousand hundred, allows two hundred thousand
     (compute-val-and-ntype (expr (W::TIMES* ?!v1 ?!v2)) (newval ?newval) (ntype ?ntype)))

    ;; explicit unit number followed by a lower values: e.g., two thousand three hundred, three hundred thirty four
    ;;   Note that two hundred three thousand should NOT work here, it has an interpretation via -number-unit> producing 203000
    ;;  NB: This does overgenerate in that it parses "three thousand two thousand" as 5000! If we want to eliminate this,
    ;;   we simply have to extract the units themselves and make sure they are descending.

    ((number (VAL ?newval) (agr 3p) (lex (?l1 ?l2)) (ntype ?ntype)
	     (var *) (LF ?lf) (nobarespec ?nbs) (sem ?sem)
	     )
     -number-unit-number>
     (head (number (lf ?lf) (val ?!v1) (lex ?l1) (unit +) (digits -) (sem ?sem)))
     (number (lf ?lf2) (val ?!v2) (lex ?l2) (nobarespec ?nbs) (digits -))
     (Less-than (val1 ?!v2) (val2 ?!v1))
     (compute-val-and-ntype (expr (+ ?!v1 ?!v2)) (newval ?newval) (ntype ?ntype)))

    ;; Special case of above: number UNIT AND number < 100
    ;;   e.g., two thousand and thirty five, two hundred and one, ...

    ((number (VAL ?newval) (agr 3p) (lex (?l1 ?l2)) (ntype ?ntype)
	     (var *) (LF ?lf) (nobarespec ?nbs) (sem ?sem)
	     )
     -number-unit-small-number>
     (head (number (lf ?lf) (val ?!v1) (lex ?l1) (unit +) (nobarespec ?nbs) (digits -) (sem ?sem)))
     (word (lex and))
     (number (lf ?lf2) (val ?!v2) (lex ?l2) (ntype (? x w::twodigit w::digit)) (digits -))
     (compute-val-and-ntype (expr (+ ?!v1 ?!v2)) (newval ?newval) (ntype ?ntype)))

    ;; Special case of above: number AND fraction
    ;;   e.g., one and two thirds; five and a half

    ((number (VAL ?newval) (agr 3p) (lex (?l1 ?l2)) (ntype ?ntype)
	     (var *) (LF ?lf) (nobarespec ?nbs) (sem ?sem)
	     )
     -number-unit-fraction>
     (head (number (lf ?lf) (val ?!v1) (lex ?l1)  (ntype (? x w::threedigit w::twodigit w::digit))
		   (unit -) (nobarespec ?nbs) (digits -) (sem ?sem)))
     (word (lex and))
     (number (lf ?lf2) (val ?!v2) (lex ?l2) (ntype w::fraction) (digits -))
     (compute-val-and-ntype (expr (+ ?!v1 ?!v2)) (newval ?newval) (ntype ?ntype)))

    ;; special case of 'a' plus number unit, where 'a' = one  e.g., a hundred

    ((number (VAL ?newval) (agr 3p) (lex ?l2) ;;(lex (?l1 ?l2)) ;;me
			 (ntype ?ntype) (unit +) (sem ?sem)
      (var *) (LF ?lf) (coerce ?coerce) (nobarespec ?nbs)
      )
     -a-number-unit>
     (word (lex a))
     (head (number-unit (val ?!v2) (lex ?l2) (digits -) (sem ?sem)))
     (compute-val-and-ntype (expr (W::TIMES* 1 ?!v2)) (newval ?newval) (ntype ?ntype)))  ;; we do this to compute the ntype

    ;; for robustness -- allow "hundred and fifty", w/o the "a"
     ((number (VAL ?newval) (agr 3p) (lex ?l2) ;;(lex (?l1 ?l2)) ;;me
			 (ntype ?ntype) (unit +) (sem ?sem)
      (var *) (LF ?lf) (coerce ?coerce) (nobarespec ?nbs)
      )
     -number-unit-bare> .9
     (head (number-unit (val ?!v2) (lex ?l2) (digits -) (sem ?sem)))
     (compute-val-and-ntype (expr (W::TIMES* 1 ?!v2)) (newval ?newval) (ntype ?ntype)))
		
    ;; Myrosia 05/14/00
    ;; e.g. three thirty five
    ;; This is a slightly lower priority to ensure left-to-right addition
    ((number (val ?newval) (agr 3p) (lex (?l1 ?l2)) (sem ?sem)
	     (var *);; making up a new var for this number
	     (lf ?lf) (coerce ?coerce) (ntype ?ntype) (nobarespec +) 
      )
     -three-digit>
     (head (number (lf ?lf) (val ?!v1) (lex ?l1) (NTYPE w::DIGIT) (coerce ?coerce) (sem ?sem) (digits -)))
     (number (val ?!v2) (lex ?l2) (NTYPE w::twodigit) (coerce ?coerce) (digits -))
     (compute-val-and-ntype (expr (+ (times* ?!v1 100) ?!v2)) (newval ?newval) (ntype ?ntype))
     )


    ;; Myrosia 10/01/2004
    ;; plus/minus numbers
    ;; THIS TIME WE ID THIS AS POSITIVE BECAUSE WE NEED TO KNOW THAT IT WAS SIGNED
    ((number (val ?v1) (agr ?agr) (lex (?l1 ?l2)) (NTYPE w::POSITIVE)
      (var *);; making up a new var for this number
      (lf ?lf) (coerce ?coerce) (nobarespec +)
      )
     -signed-plus>
     (punc (lex punc-plus))
     (head (number (agr ?agr) (lf ?lf) (val ?!v1) (lex ?l1)
		   (NTYPE (? NTT w::DIGIT w::TWODIGIT w::THREEDIGIT w::DECPOINT)) (coerce ?coerce)))
     ;;     (compute-val-and-ntype (expr ?v1) (newval ?newval) (ntype ?ntype))
     )


    ((number (val ?newval) (agr 3p) (lex (?l1 ?l2));;(NTYPE threedigit)
	     (var *);; making up a new var for this number
      (lf ?lf) (coerce ?coerce) (ntype ?ntype) (nobarespec +)
      )
     -signed-minus>
     (punc (lex punc-minus))
     (head (number (lf ?lf) (val ?!v1) (lex ?l1)
		   (NTYPE (? NTT w::DIGIT w::TWODIGIT w::THREEDIGIT w::DECPOINT)) (coerce ?coerce)))
     (compute-val-and-ntype (expr (times* -1 ?!v1)) (newval ?newval) (ntype ?ntype))
     )

    ;; number ranges
    ;;  10 to 20
    ((number (RESTR (& (min ?!v1) (max ?!v2))) (agr 3p) (lex (?l1 ?l2)) (ntype W::RANGE) (range +)
      (var *) (LF ?lf) (coerce ?coerce) (sem ?sem)
      (nobarespec ?nbs)
	     )
     -range1>
     (head (number (nobarespec ?nbs) (lf ?lf) (val ?!v1) (lex ?l1)))
     (word (lex to))
     (number (val ?!v2) (lex ?l2)))

    ;; number range with hyphen
    ((number (RESTR (& (min ?!v1) (max ?!v2))) (agr 3p) (lex (?l1 ?l2)) (ntype W::RANGE) (range +)
      (var *) (LF ?lf) (coerce ?coerce) (sem ?sem)
      (nobarespec ?nbs)
	     )
     -range-hyphen> 1
     (head (number (nobarespec ?nbs) (lf ?lf) (val ?!v1) (lex ?l1)))
     (punc (lex  w::punc-minus))
     (number (val ?!v2) (lex ?l2)))

    ;; from 20 to 35
    ((number (RESTR (& (min ?!v1) (max ?!v2))) (agr 3p) (lex (?l1 ?l2)) (ntype ?ntype) (range +)
      (var *) (LF ?lf) (coerce ?coerce) (sem ?sem)
      (nobarespec ?nbs)
	     )
     -range2>
     (word (lex from))
     (head (number (nobarespec ?nbs) (lf ?lf) (val ?!v1)))
     (word (lex to))
     (number (val ?!v2) (lex ?l2)))

    ;; between 20 and 35
    ((number (RESTR (& (min ?!v1) (max ?!v2))) (agr 3p) (lex (?l1 ?l2)) (ntype ?ntype) (range +)
      (var *) (LF ?lf) (coerce ?coerce) (sem ?sem)
      (nobarespec ?nbs)
	     )
     -range3>
     (word (lex between))
     (head (number (nobarespec ?nbs) (lf ?lf) (val ?!v1)))
     (word (lex and))
     (number (val ?!v2) (lex ?l2)))

    ;; 10 or 15
    ((number (RESTR (& (sequence (?!v1 ?!v2)))) (agr 3p) (lex (?l1 ?l2)) (ntype ?ntype) 
      (var *) (LF ?lf) (coerce ?coerce) (sem ?sem)
      (nobarespec ?nbs)
	     )
     -uncertain-number>
     (head (number (nobarespec ?nbs) (lf ?lf) (val ?!v1)))
     (word (lex or))
     (number (val ?!v2) (lex ?l2)))

    ;;   e.g., seven mph
    
    ((value (LF (% RATE (UNIT ?lf) (VAL ?n))) (SORT RATE)
	    (PRED ?pred) (SEM ?sem)  (AGR ?a))
     -unit5>
     (number (Agr ?a) (VAL ?n))
     (head (n (SORT RATE) (SEM ?sem) (LF ?lf) (PRED ?pred) (AGR ?a))))

    ;; NUMBER SEQUENCES (e.g., phone numbers, serial numbers, names (e.g., delta three five seven)
    ;;  We basically allow an arbitrary sequence of numbers, EXCEPT we eliminate +UNIT numbers
    ;;       e.g., Two thousand thirty five cannot be interpreted as the sequence (2000 35)
    ;; These rules have a slightly low score to compensate for build up of weight due to number and unit boosting
    
    ((number-sequence (val ?newlist) (agr 3p) (unit ?unit) (numb +)
      (var *);; making up a new var for this sequence
      (sem ($ F::abstr-obj (F::INFORMATION F::INFORMATION-CONTENT)))
      (multi-element-sequence +)
      )
     -number-sequence1> 
     (head (number-sequence (val ?v1) (unit -)))
     (number (val ?v2) (unit ?unit) (ntype (? !nt w::negative w::positive)) (premod -) (range -))
     (add-to-end-of-list (list ?v1) (val ?v2) (newlist ?newlist)))

    ((number-sequence  (agr 3p) (val (?l1 ?l2)) (unit ?unit)
      (var *);; making up a new var for this sequence
      (sem ($ F::abstr-obj (F::INFORMATION F::INFORMATION-CONTENT)))
      ;; (sem ?sem)
      (multi-element-sequence +)
      )
     -number-sequence2> .97    ;; need at least two consecutive numbers to form a sequence
     (head (number (val ?l1) (range -) (unit ?unit) (premod -) ))
     (number (val ?l2) (unit ?unit) (premod -) (range -) )
     )

    ;;  Letter/number sequences
    ;; On mixed sequences we define a special feature called multi-element-sequence
    ;; It's set to + for anything that has more than 1 element
    ;; RNUMBER rule then uses it to label something as a BARE-SEQUENCE +
    ;; And conjunction rules use BARE-SEQUENCE + to prevent bare sequences from appearing in super-ambiguous constructions like (a b) c and d

    ((mixed-sequence (agr 3p) (val (?l2)) (unit ?unit)
      (var *);; making up a new var for this sequence
      (sem ($ F::abstr-obj (F::INFORMATION F::INFORMATION-CONTENT)))  ;;(sem ?sem)
      (multi-element-sequence -)
      )
     -mixed-sequence-letter1> .98 
     (head (value (LF (? l ont::greek-letter-symbol ONT::LETTER-SYMBOL)) (lex ?l2)))
     )
   
    ((mixed-sequence (agr 3p) (unit ?unit) ;;(lex (?l1 ?l2)) 
      (var *)
      (sem ($ F::abstr-obj (F::INFORMATION F::INFORMATION-CONTENT)))  ;;(sem ($ ?anysem))
      (val ?newlist)
      (multi-element-sequence +)
      )
     -mixed-sequence1> .98  
     (head (mixed-sequence (val ?lf) (unit -))) ;; (lex ?l1) 
     (value (LF (? l ONT::LETTER-SYMBOL ont::greek-letter-symbol)) (lex ?l2))
     (add-to-end-of-list (list ?lf) (val ?l2) (newlist ?newlist)))
    
    ((mixed-sequence (agr 3p) (unit ?unit) ;;(lex (?l1 ?l2)) 
      (var *)
      (sem ($ F::abstr-obj (F::INFORMATION F::INFORMATION-CONTENT)))  ;;(sem ($ ?anysem))
      (val ?newlist)
      (multi-element-sequence +)
      )
      -mixed-sequence2> .98  
      (head (mixed-sequence (val ?lf) (unit -))) ;; (lex ?l1) 
      (number (val ?l2) (unit ?unit) (range -) (premod -) (ntype (? !nt w::negative w::positive)))
      (add-to-end-of-list (list ?lf) (val ?l2) (newlist ?newlist)))

    ;; last case, a mixed sequence may start with a sequence of numbers
    ((mixed-sequence  (agr 3p) (val ?newlist) (unit ?unit)
      (var *);; making up a new var for this sequence
      (sem ($ F::abstr-obj (F::INFORMATION F::INFORMATION-CONTENT)))
      ;; (sem ?sem)
      (multi-element-sequence +)
      )
     -mixed-sequence3> .98 
     (number-sequence (val ?v2) (unit ?unit) (ntype (? !nt w::negative w::positive)))
     (head (value (LF (? l ONT::LETTER-SYMBOL ont::greek-letter-symbol)) (lex ?l2)))
     (add-to-end-of-list (list ?v2) (val ?l2) (newlist ?newlist))
     )
    

    
    ;;  Using numbers SEQUENCES in names    ;; NB: I've changed these from numbers of number sequences
    ;;       which better matching the data - e.g., Delta three hundred fifty seven, Delta Fifty three twenty two, ...
    ;;   RNUMBER allows a  number sequences ,e.g., (1) or (1 2), or the phrase NUMBER.
    
    ;; We define a feature on RNUMBER called
    ;; BARE-NUMBER, which is set to "+" on cases like "1" and "number
    ;; 1". These cause ambiguity and should not go through a
    ;; NP-SEQUENCE rule later
    
    ;; We also define a feature BARE-SEQUENCE set to "+" on sequences
    ;; without a preceding word or NP. These would be disallowed from
    ;; participating in conjunctions, since they cause ambiguity that
    ;; can never be resolved

    ;; So a conjunction will always be interpreted as a set of
    ;; separate numbers, and it's the responsibility of the caller to
    ;; figure it out if there should be any sequences inside
    
    
    ((RNUMBER (LF ?lf) (agr ?a) (number-only -) (val (?val)))
     -rn1>
     (word (lex w::number))
     (head (NUMBER (val ?lf) (agr ?a) (val ?val) 
	    (ntype (? !nt decpoint negative positive))
	    (premod -)
	    ))
     )
    
    ;; This covers delta three hundred fifty seven etc.
    ((RNUMBER (LF ?lf) (agr ?a) (number-only -) (val ?lf))
     -rn1a> 0.96
     (word (lex w::number))
     (head (NUMBER-SEQUENCE (val ?lf) (agr ?a) (ntype (? !nt decpoint negative positive))))
     )


    ;; This covers "1" since number-sequence has to have at least 2 numbers
    ((RNUMBER (LF ?lf) (val (?!lf)) (agr ?a) (coerce ?coerce) (sem ?sem) (lex ?lex) (number-only +) (bare-number +))
     -rn2>
     (head (NUMBER (val ?!lf) (ntype (? !nt w::decpoint w::negative w::positive w::fraction)) 
	    (agr ?a) (sem ?sem) (coerce ?coerce) (lex ?lex)
	    (premod -)
	    ))
     )
    
    ((RNUMBER (LF ?lf) (val ?lf) (agr ?a) (coerce ?coerce) (sem ?sem) (lex ?lex) (number-only +) (bare-sequence +))
     -rn2a>
     (head (NUMBER-SEQUENCE (val ?lf) (ntype (? !nt w::decpoint w::negative w::positive w::fraction)) (agr ?a) (sem ?sem) (coerce ?coerce) (lex ?lex)))
     )
    
    

     ((RNUMBER (LF ?lf) (val ?lf) (agr ?a) (coerce ?coerce) (lex ?lex) (number-only -) (bare-sequence ?ms))
     -rn3>
      (head (MIXED-SEQUENCE (val ?lf) (ntype (? !nt w::decpoint w::negative w::positive)) (agr ?a) (coerce ?coerce) (lex ?lex)
	     (multi-element-sequence ?ms)
	     ))
     )
    

    ;;  e.g., ENGINE E 1
   
    ((name (lex ?seq) (lf ?cl) (SEM ?sem) (agr 3s) (name +) (generated +)
      )
     -noun-nname2> 0.98 ;0.96  ; increased to 0.98 so "block 1 and block 3" would parse
     ;; Myrosia 10/26/03 added (name -) to prevent cases like "aspirin 7" or "pittsford 8"
     ;; also lowered the probability considerably to avoid overgeneration
     ;; swift 09/22/11 removing the sem restriction to allow "unit 1" "scenario 2" etc.
     (head (n (name -) (one -) (SEM ?sem) (SEM ($ (? xx f::PHYS-OBJ f::abstr-obj) (f::scale -)))   ;; don't want scales like "S" for seconds, etc
	      (WH -) (lf ?cl) 
	    (LF (:* ?lfparent ?lfform))
	    (lex ?lex))) 
     (BOUND (arg1 ?cl))
     ;;     (nname (lex ?name))
     (rnumber (val ?name))
     (simple-cons (in1 ?lfform) (in2 ?name) (out ?seq))
     )
    

    ;;  e.g., bulbs 1 and 3
    
    ((name (lex ?lex) (lf ?cl) (SEM ?sem) (agr 3p) (name +) (generated +) 
	   (restr (& (name-modifiers ?m))))
     -noun-nname-plural> 0.94
     (head (n (name -) (SEM ?sem) ;(SEM ($ f::PHYS-OBJ)) 
	      (WH -) (lf ?cl) 
	      (agr 3p)
	      (LF (:* ?lfparent ?lfform))
	      (lex ?lex))) 
     (BOUND (arg1 ?cl))
     ;;     (nname (lex ?name))
     (np (complex +) (generated +) (lf (% description (constraint (& (sequence ?m))))))
     )

    
    ;; rule to allow names to be used as common nouns, e.g. I want a mac/ dell
    ;; sometimes they behave like common nouns and take modifiers/specifiers, e.g. I'd like a g 4
    ;; so we need to convert the names to n.
    ((n1 (sort pred) (class ?lf) (mass count) (agr 3s) (CASE (? case SUB OBJ))
        (sem ?sem) (lex ?lex)(RESTR (& (name-of ?lex))))
     -nname-bare> 0.98
     (head (name (SEM ?sem) (nname +) (title -) 
	    ;(SEM ($ f::PHYS-OBJ)) 
	    (WH -) (lf ?lf) (lex ?lex))) 
     )

    ;; rule to allow names to combine with numbers, e.g. DELTA 567 or HP 21.
    ((name (lex ?seq) (lf ?cl) (SEM ?sem) (agr 3s) (name +) (generated +)
      )
     -nname-n2> .96
     ;; Myrosia 10/26/03 added (name -) to prevent cases like "aspirin 7" or "pittsford 8"
     ;; also lowered the probability considerably to avoid overgeneration
     ;; swift 09/22/11 removing the sem restriction to allow "unit 1" "scenario 2" etc.
     (head (name (name +) (nname +) (SEM ?sem) ; (SEM ($ f::PHYS-OBJ))
	      (WH -) (lf ?cl) 
	    (LF (:* ?lfparent ?lfform))
	    (lex ?lex))) 
     (BOUND (arg1 ?cl))
     ;;     (nname (lex ?name))
     (rnumber (val ?name))
     (simple-cons (in1 ?lfform) (in2 ?name) (out ?seq))
     )
#|| 
    ;; rule to allow names to combine with numbers, e.g. DELTA 567 or HP 21.
    ;; sometimes they behave like common nouns and take modifiers/specifiers, e.g. I'd like a g 4
    ;; so we need to convert the names to n.
    ((n (sort pred) (class ?lf) (mass count) (agr 3s) (CASE (? case SUB OBJ))
        (sem ?sem) (lex ?seq)(RESTR (& (name-of ?seq))))
     -nname-n> 0.98
     (head (name (SEM ?sem) (nname +) ;(SEM ($ f::PHYS-OBJ)) 
		 (WH -) (lf ?cl) (lex ?lex))) 
     ;;     (nname (lex ?name))
     (rnumber (val ?name))
     (simple-cons (in1 ?lex) (in2 ?name) (out ?seq))
     )
||#

   #|| ;;  title + name constructions -- semantics needs to be improved eventually

    ((name (lex ?n) (var ?v) (lf ?cl) (SEM ?sem) (agr 3s) (name +) (generated +) (restr (& (title ?tlex))))
     -title-name>
     (name (title +) (lex ?tlex))
     (head (name (lex ?n) (var ?v) (lf ?cl) (SEM ?sem) (agr 3s) (name +) (generated -)))
     )||#

    ;; Myrosia 2008/04/30
    ;; A rule that allows "an open switch Z" as well as "open switch Z" without an article
    ;; It's similar to the nname-bare, but requires a generated name with headcat N to disallow "the open A"
    ((n (sort pred) (class ?lf) (mass (? mm bare count )) (agr 3s) (CASE (? case SUB OBJ))  (generated +)
        (sem ?sem) (lex ?lex)(RESTR (& (name-of ?lex))))
     -nname-bare-generated> 0.96
     (head (name (SEM ?sem) (generated +) (headcat N)
	    (SEM ($ f::PHYS-OBJ)) (WH -) (lf ?cl) (lex ?lex))) 
     )
    

    ;;; Myrosia 2008/04/30 commented out because this is now covered by nname-bare-generated 
;;;    ;; e.g the ENGINE E 1
;;;    
;;;    ((name (lex ?seq) (lf ?cl) (SEM ?sem) (agr 3s) (name +) (generated +))
;;;     -name-nname3> .98
;;;     (art (lex the))
;;;     (head (n (SEM ?sem) (SEM ($ f::PHYS-OBJ)) (lex ?lex) (lf ?cl)
;;;	    (lf (:* ?lftype ?lfform))
;;;	    ))
;;;     (BOUND (arg1 ?cl))
;;;     ;;     (nname (Lex ?name))
;;;     (rnumber (val ?name))
;;;     (simple-cons (in1 ?lfform) (in2 ?name) (out ?seq))
;;;     )


 ;; e.g the number one
    
    ((N1 (CLASS ?lf) (sort PRED) (agr ?agr)  (case ?case)
      (POSTADVBL -) (QUAL -) (RESTR ?newr) (subcat ?subcat)) 
     -N1_number> 1.01
     (head (n1 (SORT PRED) (CLASS (:* ONT::NUMBER-PROTOTYPE ?x)) (CLASS ?lf) (RESTR ?r)
	       (subcat ?subcat) (complex -) (agr ?agr) (case ?case)
	       )
      )
     (number (lf ?val) (premod -))
     (add-to-conjunct (val (:name-of ?val)) (old ?r) (new ?newr)))


    ;; Allow a letter_symbol to be an N1
    ;; for example, "I don't see the H on the map" where H marks the hospital
    ((NAME (SORT PRED) (mass count) (agr 3s) (CASE (? case SUB OBJ))
         (POSTADVBL -) (QUAL -) (sem ?sem) (lex ?let)  (lf ont::letter-symbol) (name +) (name-or-bare +))
     -N1_SYM> .98
     (head (value (lf ont::letter-symbol) (sem ?sem) (lex ?let)))
     )
    
    ;; I see three h's on the map
      ((NAME (SORT PRED) (mass count) (agr 3p) (CASE (? case SUB OBJ))
	(POSTADVBL -) (QUAL -) (sem ?sem) (lex ?let)  (lf ont::letter-symbol) (name +) (name-or-bare +))
      -N1_SYM-PL> .98
       (head (value (lf ont::letter-symbol) (sem ?sem) (lex ?let)))
       (word (lex ^s) )
     )


    ;; this uses declared coercions on names 
					
    ((np (sort pred) (agr 3s) (Class ?lf-new)
      (SEM ?newsem) (var ?var) ;; duplicate (sort individual)
      (transform ?op) (mass count) (case (? case sub obj -))
      (lf (% description 
	     (status ont::gname) (class ?lf-new) (var ?var)
	     (sem ?newsem)  (lex ?lex)
	    ;; (constraint (& (NAME-OF ?lex)))  not necesary, LEX does the job
	     ))
      (name +) (generated +)
      )
     -number-name-coerce> 0.97 ;; I prefer to see cardinality here
     (head (nname (lf ?lf-old) (var ?var)
	    (COERCE ((% coerce (KR-TYPE ?kr) (Operator ?op) (sem ?newsem) (LF ?lf-new)) ?cc))
	    ))
     )
    
 ;; construction for naming subregions, e.g., southern israel
    ((NP (var *) (sem ?nsem) (sort pred)
      (LF (% Description (status ont::definite) (var *) (class ONT::GEOGRAPHIC-REGION) (sem ?nsem)
	     (constraint (& (mod ?sub))))))

      -sublocation>
      (ADJP (atype (? at attributive-only central)) (var ?sub) (LF (? qual ont::spatial)) (ARG ?v) (VAR ?adjv) (WH -)
	    (argument (% NP (sem ?nsem))) (COMPLEX -) (comparative ?com) (Set-modifier -)
	    (post-subcat -)
	    )
     (head (NP (name +) (sem ?nsem) (sem ($ F::PHYS-OBJ (f::spatial-abstraction F::spatial-region)))
		(var ?v) (class ?c) (lex ?l))))

    ;; today, tomorrow, ...
    ((DATE (var *) (INT +) (LF ONT::DAY-NAME) (DAY ?day) (lex ?hlex) (headcat ?hcat) (day-specified +))
     -dt-pro-day> 1.0
     (head (NP (LF (% description (CLASS ONT::DATE-OBJECT))) (PRO +) (var ?day)
	       (lex ?hlex) (headcat ?hcat)
	       )))
    
    ))


;;  DATES
;; lex and headcat added for aug-trips
(parser::augment-grammar
  '((headfeatures 
     (DATE var lex headcat)
     (number ntype var lex headcat)
     )

  ;; 2004
  ((DATE (INT +) (YEAR ?N) (dow -) (day -) (month -) (at-least-year +)
	(lex ?hlex) (headcat ?hcat) )
   -dt-year>
   (HEAD (Number (VAL ?n) (NTYPE w::YEAR) (range -) (lex ?hlex) (headcat ?hcat) (comma -))))

 ;;  Year in words, e.g., ninteen thirty one
    ((DATE (INT +) (YEAR ?newval) (at-least-year +)
      (var ?v))
     -year1> 1.0
     (head (number (nobarespec ?nbs) (lf ?lf) (val ?!v1) (lex ?l1) (sem ?sem) (var ?v)
		   (NTYPE w::TWODIGIT) (coerce ?coerce) (digits -)))
     (number (val ?!v2) (lex ?l2) (NTYPE w::TWODIGIT) (coerce ?coerce) (digits -))
     (compute-val-and-ntype (expr (+ (W::TIMES* ?!v1 100) ?!v2)) (newval ?newval) (ntype ?ntype)))


    ;; another variation '96

     ((DATE (INT +) (YEAR ?N) (dow -) (day -) (month -) (at-least-year +)
	(lex ?hlex) (headcat ?hcat) )
      -dt-year2>
      (word (lex w::^))
      (HEAD (Number (VAL ?n) (NTYPE w::TWODIGIT) (range -) (lex ?hlex) (headcat ?hcat) (comma -))))
    
    ;; July
    ((DATE (INT +) (MONTH ?M) (lex ?hlex) (headcat ?hcat))
     -dt-month> 1.0
     (head (name
	    (lf ?M) (LF ONT::MONTH-NAME) (lex ?hlex) (headcat ?hcat)
	    )))

  ;; Monday
  ((DATE (INT +) (LF ONT::DAY-NAME) (DOW ?dow) (lex ?hlex) (headcat ?hcat) (day-specified +))
   -dt-dow> 1.0
   (head (Name (LF ONT::DAY-NAME)    
	    (lf ?dow)  (lex ?hlex) (headcat ?hcat)
	    )))

    ;; 7/31/2007 (31 July 2007)  or 7-31-2007
  ((DATE (INT +) (Year ?y) (Month ?m) (day ?d) (lex ?hlex) (headcat ?hcat) (day-specified +))
   -dt-month-slash-day-slash-year> 1.0
   (head (date (year -) (month ?m) (day ?d) (lex ?hlex) (headcat ?hcat)))
   (punc (lex (? ll w::punc-slash w::punc-minus)))
   (number (VAL ?y) (number-digits (? x 2 4))))

 ;; 2007-7-31 (31 July 2007)  ;; I don't think this works with the slash!
  ((DATE (INT +) (Year ?y) (Month ?m) (day ?d) (lex ?hlex) (headcat ?hcat) (day-specified +))
   -dt-year-hyphen-month-day> 1.0
   (number (VAL ?y) (number-digits 4))
   (punc (lex (? ll w::punc-minus)))
   (head (date (year -) (month ?m) (day ?d) (lex ?hlex) (headcat ?hcat))))
   
   ;; 7/2007 (July 2007)  and 07/07.
  ((DATE (INT +) (dow -) (day -) (Year ?y) (Month ?m) (lex ?hlex) (headcat ?hcat)  (day-specified +))
   -dt-month-slash-year> 1.0
   (head (number (VAL ?m) (NTYPE w::month)(lex ?hlex) (headcat ?hcat))) 
   (punc (lex (? ll w::punc-slash w::punc-minus)))
   (number (VAL ?y) (number-digits (? x 2 4)))) ;; (NTYPE w::year)))

  ;; 7/31 (July 31)
  ((DATE (INT +)  (DAY ?d) (Month ?m)  (lex ?hlex) (headcat ?hcat) (day-specified +))
   -dt-month-slash-day> 1.0
   (head (number (VAL ?m) (NTYPE w::month) (lex ?hlex) (headcat ?hcat)))
   (punc (lex  (? ll w::punc-slash w::punc-minus)))
   (number (val ?d) (ntype w::day))
   )

   ;; July 31
  ((DATE (INT +) (Month ?m) (DAY ?n)(lex ?hlex) (headcat ?hcat) (day-specified +))
   -dt-month-day> 1.0
   (head (name (LF ONT::MONTH-NAME)
		(lf ?M) (lex ?hlex) (headcat ?hcat)
		))
   (number (VAL ?n) (NTYPE w::DAY)))

  ;; July 31st
  ((DATE (INT +) (Month ?m) (DAY ?n)(lex ?hlex) (headcat ?hcat) (day-specified +))
   -dt-month-day-ord> 1.0
   (head (name (LF ONT::MONTH-NAME)
		(lf ?M)  (lex ?hlex) (headcat ?hcat)
		))
   (ordinal (LF (NTH ?n)) (NTYPE w::DAY)
	   ))

      ;; 31st July
  ((DATE (INT +) (Month ?m) (DAY ?n)(lex ?hlex) (headcat ?hcat) (day-specified +))
   -dt-ord-month> 1.0
   (ordinal (LF (NTH ?n)) (NTYPE w::DAY))
   (head (name (LF ONT::MONTH-NAME)
		(lf ?M)  (lex ?hlex) (headcat ?hcat)
		))
   
   )

    ;; July the 31st
  ((DATE (INT +) (Month ?m) (DAY ?n)(lex ?hlex) (headcat ?hcat) (day-specified +))
   -dt-month-day-the-ord> 1.01
   (head (name (LF ONT::MONTH-NAME)
		(lf ?M)  (lex ?hlex) (headcat ?hcat)
		))
   (ART (lex the))
   (ordinal (LF (NTH ?n)) (NTYPE w::DAY)
	   ))
    

   ;; The 31st of July
  ((DATE (INT +) (Month ?m) (DAY ?n)
	 (lex ?hlex) (headcat ?hcat) (day-specified +))
   -dt-ord-month1> 1.01
   (ART (LEX THE))
   (ordinal (LF (NTH ?n)) (NTYPE w::DAY))
   (prep (Lex of))
   (head (name (LF ONT::MONTH-NAME)
		(lf ?M) (lex ?hlex) (headcat ?hcat)
		))
   )

     ;; The 31st
    ((DATE (INT +) (Month ?m) (DAY ?n) (var ?v)
      (lex ?hlex) (headcat ?hcat) (day-specified +))
     -dt-ord> 1.0
     (ART (LEX THE))
     (head (ordinal (LF (NTH ?n)) (NTYPE w::DAY) (var ?v))))
     
    ;; The 2nd century
    ((DATE (INT +) (Century ?n) (at-least-year +)
      (lex ?hlex) (headcat ?hcat))
     -dt-ord-century> 1.0
     (ART (LEX THE))
     (ordinal (LF (NTH ?n)) (NTYPE w::DAY))
     (head (N (LF (:* ONT::TIME-INTERVAL W::CENTURY))))
     )

    ;; 1900 AD, the second century BC, ....
    ((DATE (INT +) (year ?y) (Century ?n) (era ?era)
	 (lex ?hlex) (headcat ?hcat))
     -dt-era> 1.01   ;; boost to compensate for length of rule
     (head (DATE (INT +) (year ?y) (Century ?n) (at-least-year +)
	     (lex ?hlex) (headcat ?hcat)))
     (value (LF ONT::ERA) (lex ?era)))
;     (word (lex (? era w::ad W::bc))))
    
   ;; Monday July 31, Tuesday May 3rd  but not  Monday, July
   ((DATE (INT +) (dow ?!dow) (DAY ?!day) (month ?m)  (year ?year) (lex ?hlex) (headcat ?hcat) (day-specified +))
    -dt-day-date> 1.01
    (head (DATE (LF ONT::DAY-NAME) (dow ?!dow)   
	    (month -) (year -) (day -)
	    ))
    (DATE (day ?!day) (dow -) (year ?year) (month ?m)))

    ;; Monday afternoon
    ;;  note there is also a time-value rule like this -- we need this one for the ADVBL use - should clean up sometime
    ((DATE (INT +) (dow ?!dow) (DAY ?!day) (month ?m) (am-pm ?v1) (year ?year) (lex ?hlex) (headcat ?hcat) (day-specified +))
     -dt-day-period-date> 1.01
     (head (DATE (LF ONT::DAY-NAME) (dow ?!dow)   
		 (month -) (year -) (day -)
		 ))
      (np (SORT pred) 
	       (SEM ($ f::time (F::Time-Function (? xx F::day-period F::day-part F::day-point))))
	       (var ?v1)
	       (Lex ?lf2))
     )
     
     

   ;; July 2004, July first 2004, Monday July second 2004, but not  Monday 2004*
   ((DATE (INT +) (dow ?dow) (DAY ?d) (year ?!y) (month ?!m)
	  (lex ?hlex) (headcat ?hcat))
    -dt-date-year> 1.01
    (head (DATE (month ?!m) (year -) (day ?d) (dow ?dow) (lex ?hlex) (headcat ?hcat)))
    (DATE (month -) (year ?!y)))

    ((DATE (INT +) (dow ?dow) (DAY ?d) (year ?!y) (month ?!m)
	  (lex ?hlex) (headcat ?hcat))
    -dt-date-comma-year> 1.01
     (head (DATE (month ?!m) (year -) (day ?d) (dow ?dow) (lex ?hlex) (headcat ?hcat)))
     (punc (lex w::punc-comma))
     (DATE (month -) (year ?!y)))

    ;;  July of 2004, august in 1987
    ((DATE (INT +) (dow ?dow) (DAY ?d) (year ?!y) (month ?!m)
       (lex ?hlex) (headcat ?hcat))
    -dt-date-year-of> 1.0
     (head (DATE (month ?!m) (year -) (day ?d) (dow ?dow) (lex ?hlex) (headcat ?hcat)))
     (ADV (LEX (? w w::of w::in)))
     (DATE (month -) (year ?!y)))

   ;; late june, mid 06/2010, early 2006

     ((date (day ?d) (dow ?dow) (month ?month) (year ?year) (phase (? c ont::stage-val  ont::scheduled-time-modifier))
       (var ?v))
      -mid-month-year> 1.0
      (adjp (var ?adjv) (LF (% PROP (class (? c ont::stage-val  ont::scheduled-time-modifier)))) (arg ?v))
      (head (date (INT +) (hour -) (minute -) (day ?d) (dow ?dow) (month ?month) (year ?year)))
      
      )


   ;;  Dates as adverbials
   ;; Those with a day of the week, e.g.,  Monday I go
   ((ADVBL (ARG ?arg) (ROLE (:* ONT::EVENT-TIME-REL W::DATE))
	   (SORT BINARY-CONSTRAINT)
	   (LF (% PROP (VAR ?v) (CLASS (:* ONT::EVENT-TIME-REL W::DATE))
		  (CONSTRAINT (& (FIGURE ?arg) (GROUND (% *PRO* (VAR *)
						       (CLASS ONT::TIME-LOC)
						       (CONSTRAINT (& (DAY ?day) (Month ?m) (DAY-OF-WEEK ?!dow) (YEAR ?y) (AM-pm ?ampm) (phase ?phase)))))))))
	   (VAR ?v) (ATYPE (? x W::PRE W::POST))  (bare-advbl +)
	   (lex ?hlex) (headcat ?hcat)
	   (ARGUMENT (% (? ARGCAT8043 W::S
				     W::NP
				     W::VP)
			(SEM (? SEM8044 ($ F::SITUATION (F::ASPECT ( ? ASP8042 F::DYNAMIC
								       F::STAGE-LEVEL))))))))
    -date-advbl1>
    (DATE (var ?v) (DAY ?day) (Month ?m) (DOW ?!dow) (Year ?y) (phase ?phase) (AM-pm ?ampm)
	  (lex ?hlex) (headcat ?hcat)))

   ;; this one covers the other case, e.g., I go July third
   ((ADVBL (ARG ?arg) (ROLE (:* ONT::EVENT-TIME-REL W::DATE))
	   (SORT BINARY-CONSTRAINT)  (bare-advbl +)
	   (LF (% PROP (VAR ?v) (CLASS (:* ONT::EVENT-TIME-REL W::DATE))
		  (CONSTRAINT (& (FIGURE ?arg) (GROUND (% *PRO* (VAR *) (STATUS ont::definite)
						       (CLASS ONT::TIME-LOC)
						       (CONSTRAINT (& (DAY ?!day) (Month ?m) (YEAR ?y) (phase ?phase)))))))))
	   (VAR ?v) (ATYPE (? x W::PRE W::POST))
	   (lex ?hlex) (headcat ?hcat)
	   (ARGUMENT (% (? ARGCAT8043 W::S
				     W::NP
				     W::VP)
			(SEM (? SEM8044 ($ F::SITUATION (F::ASPECT ( ? ASP8042 F::DYNAMIC
										       F::STAGE-LEVEL))))))))
    -date-advbl2> .98
    (DATE (var ?v) (DAY ?!day) (Month ?m) (DOW -) (Year ?y)
	  (lex ?hlex) (headcat ?hcat) (phase ?phase)))

    ;;  three days before yesterday, two hours before noon, a few minutes after noon, 5 days ago
    ;;  create an NP here that can then either be the object of a prep (until 5 days ago) or made into an ADVBL
    
    ((NP (LF  (% DESCRIPTION (VAR *) (STATUS ont::DEFINITE)
		 (CLASS ONT::TIME-LOC) 
		 (CONSTRAINT (& (MODS (% *PRO* (status ont::F) (CLASS (? ev ONT::EVENT-TIME-REL)) (var ?v)
					(constraint ?newc)))))))
      (SEM ($ F::TIME (F::TIME-FUNCTION F::YEAR-NAME) (F::TIME-SCALE F::INTERVAL)))
      (VAR *) (sort pred) (NAME +)
      (lex ?hlex) (headcat ?hcat))
     -durational-date> 1.0
     (NP (SORT W::UNIT-MEASURE) (SEM (? s ($ ?any (F::SCALE ont::TIME-MEASURE-scale))))
      (var ?vdur) (generated -))
     (head (ADVBL (durational -) (bare-advbl -) (var ?v) 
		  (lf (% PROP (CLASS (? ev ONT::EVENT-TIME-REL)) (var ?v)
			 (constraint ?constr)))
		  (atype ?atype) (arg *) (argument ?arg1) (sem ?sem)))
     (add-to-conjunct (val (extent ?vdur)) (old ?constr) (new ?newc)))

    ;; 2 days ago as an ADVBL
     ((ADVBL (LF (% PROP (CLASS (? ev ONT::EVENT-TIME-REL)) (var ?v)
			 (constraint ?newc)))
      (SEM ($ F::TIME (F::TIME-FUNCTION F::YEAR-NAME) (F::TIME-SCALE F::INTERVAL)))
      (VAR ?v) (sort pred) (NAME +) (atype ?atype) (arg ?arg) (argument (% S (var ?arg) (sem ($ f::situation)) ))
      (lex ?hlex) (headcat ?hcat))
     -durational-date-constraint> 1.0
     (NP (SORT W::UNIT-MEASURE) (SEM (? s ($ ?any (F::SCALE ont::TIME-MEASURE-scale))))
      (var ?vdur) (generated -))
     (head (ADVBL (durational -) (bare-advbl -) (var ?v) 
		  (lf (% PROP (CLASS (? ev ONT::EVENT-TIME-REL)) (var ?v)
			 (constraint ?constr)))
		  (arg ?arg) (argument ?arg1) (sem ?sem)))
     (add-to-conjunct (val (extent ?vdur)) (old ?constr) (new ?newc)))
    
    ;; Durational adverbials, e.g., he ran three hours

    ((ADVBL (ARG ?arg) 
      (SORT BINARY-CONSTRAINT) (var *)
      (LF (% PROP (var *) (CLASS ONT::TIME-DURATION-REL) (Constraint (& (:FIGURE ?arg) (:GROUND ?v)))))
      (ATYPE ?atype) (npadvbl +)
      (lex ?hlex) (headcat ?hcat)
      (ARGUMENT (% (? x W::VP W::S)))
      (SEM ?sem))
    -bare-duration-advbl> 
     (NP (SORT W::UNIT-MEASURE) (spec ont::indefinite)   ;; must be indefinite, e.g., "three months", not "this month"
      (LF (% PROP (CLASS ont::duration-scale)))
      (SEM ($ ?any (F::SCALE ont::duration-scale)))
      (var ?v) (generated -))
      )
    
   ;;  Date NPs, typically as arguments to ON (for days) and IN

   ;;  Monday, Monday July 4, Monday July 4 2003
   
   ((NP (LF  (% DESCRIPTION (VAR ?v) (STATUS ont::definite)
		(CLASS ONT::TIME-LOC) (CONSTRAINT (& (DAY-OF-WEEK ?!dow) (DAY ?day) (Month ?m) (YEAR ?y)))))
	(SEM ($ F::TIME (F::TIME-FUNCTION F::DAY-OF-WEEK) (F::TIME-SCALE F::INTERVAL)))
	(VAR ?v) (NAME +) (Sort pred)
	(lex ?hlex) (headcat ?hcat))
    -NP-date1>
    (DATE (DOW ?!dow) (var ?v) (DAY ?day) (Month ?m)  (Year ?y)
	  (lex ?hlex) (headcat ?hcat)))

   ;; July third, etc, no day of the week
    ((NP (LF  (% DESCRIPTION (VAR ?v) (STATUS ont::definite)
		(CLASS ONT::TIME-LOC) (CONSTRAINT (& (DAY ?!day) (Month ?m) (YEAR ?y)))))
	(SEM ($ F::TIME (F::TIME-FUNCTION F::DAY-OF-WEEK) (F::TIME-SCALE F::INTERVAL)))
	(Sort pred)
      (VAR ?v) (NAME +)
	(lex ?hlex) (headcat ?hcat))
    -Np-date2> 1.0
    (DATE (DOW -) (var ?v) (DAY ?!day) (Month ?m)  (Year ?y)
	 (lex ?hlex) (headcat ?hcat) ))

    ;;  The next rules go with IN (and do not mention a day)
    ;;  e.g.,  July, and July 2004
    ((NP (LF  (% DESCRIPTION (VAR ?v) (STATUS ont::DEFINITE) (NAME +)
		(CLASS ONT::TIME-LOC) (CONSTRAINT (& (Month ?!m) (YEAR ?y) (phase ?phase))))) (sort pred) 
	(SEM ($ F::TIME (F::TIME-FUNCTION F::MONTH-NAME) (F::TIME-SCALE F::INTERVAL)))
	(VAR ?v) (lex ?hlex) (headcat ?hcat))
     -NP-date3> 1.0
     (DATE (DOW -) (var ?v) (DAY -) (Month ?!m)  (Year ?y) 
	   (lex ?hlex) (headcat ?hcat) (phase ?phase)))
    

    ;; 2004,  the second century, ...
     ((NP (LF  (% DESCRIPTION (VAR ?v) (STATUS ont::DEFINITE)
		(CLASS ONT::TIME-LOC) (CONSTRAINT (& (YEAR ?y) (century ?c) (era ?e)(phase ?phase)))))
	(SEM ($ F::TIME (F::TIME-FUNCTION F::YEAR-NAME) (F::TIME-SCALE F::INTERVAL)))
	(VAR ?v) (sort pred) (NAME +)
	(lex ?hlex) (headcat ?hcat))
     -NP-date4> 1.0
     (DATE (DOW -) (var ?v) (DAY -) (Year ?y) (month -) (century ?c) (era ?e) (phase ?phase)
	  (lex ?hlex) (headcat ?hcat)))

    ;; Time expression ranges
     
     ;; dates: march 23 (2004) - april 8 (2004); 11/28/2004 - 12/28/2004
     ;; times: 2 - 3pm
    ((NP (LF  (% DESCRIPTION (VAR *) (status ?st)
		 (CLASS ONT::TIME-RANGE)(CONSTRAINT (& (to ?v2) (from ?v1)))))
      (SEM ($ F::TIME (F::TIME-FUNCTION ?tf1)))
      (VAR *) (headcat ?hcat))
      -date-range>
     (NP (LF  (% DESCRIPTION (VAR ?v1) (status ?st)))
      (SEM ($ F::TIME (F::TIME-FUNCTION ?tf)))
      (VAR ?v1) (headcat ?hcat))
     (punc (lex (? lx punc-tilde punc-minus))) ;; currently - defined only as punc-minus in parser
     (head
      (NP (LF  (% DESCRIPTION (VAR ?v2) (status ?st)))
	  (SEM ($ F::TIME (F::TIME-FUNCTION ?tf)))
	  (VAR ?v2))))
   
    ;; e.g., "the 1980s" "the 40s" 
    ;;  we can't build an N1 here as then it would become a SET through the INDV-PLURAL rules.
    ((NP (LF (% DESCRIPTION (VAR ?v) (status ?spec) (CLASS ONT::TIME-RANGE) (CONSTRAINT (& (decade ?n) (poss ?poss)))))
      (SEM ($ F::TIME (F::TIME-FUNCTION ?tf1))) (SORT PRED)
      (VAR ?v) (headcat ?hcat))
      -decade1>
     (SPEC (LF ?spec) (ARG ?v) (mass count) (CARD ?CARD)
      (wh ?w) (WH-VAR ?whv) (poss ?poss)
      (agr 3s) (RESTR ?restr) (NOSIMPLE -))
     (Number (val ?n) (var ?v) (ntype W::end-zero))
     (word (lex ^s))
     )

    ;; "the early 80s"
    ((NP (LF (% DESCRIPTION (VAR ?v) (status ?spec) (CLASS ONT::TIME-RANGE) (CONSTRAINT (& (decade ?n) (poss ?poss) (phase (? phase ont::stage-val  ont::scheduled-time-modifier))))))
      (SEM ($ F::TIME (F::TIME-FUNCTION ?tf1))) (SORT PRED)
      (VAR ?v) (headcat ?hcat))
     -decade-modified1>
     (SPEC (LF ?spec) (ARG ?v) (mass count) (CARD ?CARD)
      (wh ?w) (WH-VAR ?whv) (poss ?poss)
      (agr 3s) (RESTR ?restr) (NOSIMPLE -))
     (adjp (arg ?v) (LF (% PROP (class (? phase ont::stage-val ont::scheduled-time-modifier)))) (var ?adjv))
     (Number (val ?n) (var ?v) (ntype W::end-zero))
     (word (lex ^s))
     )
    

    ;;  Grammar of Names
    ;; Jack Smack, 47, ... 
    ((name (lex ?l) (lf ?cl) (var ?v) (SEM ?sem) (agr 3s) (name +) (restr (& (age ?!v1))) (generated +))
     -name-age>
     (head (name (lex ?l) (var ?v) (lf ?cl) (SEM ?sem) (agr 3s) (time-converted -)))
     (punc (lex w::punc-comma))
     (Number (lf ?lf) (val ?!v1) (lex ?l1)
      (NTYPE (? NTT w::DIGIT w::TWODIGIT w::THREEDIGIT)))
     (punc (lex ?anything)))

    ;; titles:  President Smith
    ((name (lex ?l) (lf ?cl) (var ?v) (SEM ?sem) (agr 3s) (name +) (restr (& (title ?title))) (generated +))
     -name-title>
     (name (lex ?ltitle) (title +) (var ?vt) (lf ?title) (agr 3s))
     (head (name (lex ?l) (lf ?cl) (var ?v) (agr 3s) (sem ($ PHYS-OBJ (F::ORIGIN F::HUMAN))) (sem ?sem))))

    ;; special constructions on web pages and documents
    ;;  "sq ft: 5000", "population: 1000"
    ((NP (LF (% description (STATUS ONT::INDEFINITE)
		(VAR ?v) (SORT unit-measure)
		(CLASS (:* ONT::quantity ?sc))
		(CONSTRAINT ?constr) (argument ?argument)
		(sem ?sem) (unit-spec +)
		))
	      (SPEC ont::INDEFINITE) (VAR ?v) (SORT unit-measure))
         -unit-colon-value>
     (head (N1 (VAR ?v) (SORT unit-measure) (INDEF-ONLY -) (CLASS ?c) (MASS ?m)
		   (KIND -) (agr ?agr) (sem ?sem) (sem ($ f::abstr-obj (f::scale ?sc)))
		   (argument ?argument) (RESTR ?restr)
		   (post-subcat -)
		))
     (punc (lex w::punc-colon))
     (NUMBER (val ?num) (VAR ?nv) (AGR ?agr) (restr ?r))
         (add-to-conjunct (val (& (value ?num))) (old ?r) (new ?newr))
	 (add-to-conjunct (val (& (amount (% *PRO* (status ont::indefinite) (class ont::NUMBER) (VAR ?nv) (constraint ?newr)))
				  (unit ?c))) (old ?restr) (new ?constr))
	 )
    
  ))

  
