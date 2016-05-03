;;;;
;;;; runtest.lisp
;;;;
;;;; Time-stamp: <Fri Dec 10 08:30:09 EST 2010 jallen>
;;;;

;;  This File tests the grammar and lexicon  - mostly obsolete and will need modifying
;;

(in-package "PARSER")
     

(defun test-sentence (sentence sa structure-ans)
  "Checks the speech act and bracketing on the first interpretation"
  ;;  (format t "~& Testing ~S" sentence)
  (let* ((analysis (parse-sentence sentence))
         (sa1 (caar analysis))
         (structure (extract-words (find-arg (find-arg (cdar analysis) :syntax) :tree))))
    (format t ".")
    (cond ((not (eq sa sa1))
           (format t "~% Bad speech act id for ~S. Got ~S, expected ~S~%" sentence sa1 sa)
	   sentence
	   )
          ((AND ;;Structure-ans 
                (not (equal structure structure-ans)))
           (Format t "~% Bad Parse for ~S~%   Got      ~S~%   Expected ~S~%" sentence structure structure-ans)
           sentence
           )
	  (t nil)
	  )
    ))


(defun test-sentence1 (sentence sa structure-ans)
  "Checks the speech act and bracketing on the first interpretation"
  (let* ((analysis (parse-sentence sentence))
         (sa1 (caar analysis))
         (structure (extract-words (find-arg (find-arg (cdar analysis) :syntax) :tree))))
    (cond ((not (eq sa sa1))
	   sentence
	   )
	  ;;	  ((AND Structure-ans (not (equal structure structure-ans)))
	  ;;   sentence
	  ;;	   )
	  (t nil)
	  )
    ))

(defun extract-words (tree)
  "takes an abbreviated tree and extracts bracketing and words"
  (cond 
   ((eq (second tree) :input)
    (car (third tree)))
   ((consp (second tree))
    (remove-unary-lists (Mapcar #'extract-words (cdr tree))))
   ((eq (second tree) :var)
    (remove-unary-lists (mapcar #'extract-words (cdddr tree))))
   ))

(defun remove-unary-lists (ll)
  "takes the car of ll until it is not a list of length one"
  (if (and (consp ll) (eq (list-length ll) 1))
      (remove-unary-lists (car ll))
    ll))

;; code to be used to recompute a test file from an old test file.
;;(defun test-sentence (s a b)
;;  (gen-test-case s))

(defun gen-test-case (sentence)
  (let* ((analysis (parse-sentence sentence))
         (sa (caar analysis))
         (structure (find-arg (find-arg (cdar analysis) :syntax) :tree)))
    (format t "~%(test-sentence ~S '~S ~%      '~S)" sentence sa (extract-words structure))))

(defun sa-match (ans ref)
  (if (symbolp ref)
      (eq (car ans) ref)))

(defun parse-sentence (x)
  (parse :user '(start-sentence))
  (parse :user (list 'word x))
  (parse :user '(end-sentence)))

(defun runtest (data)
  (let ((savedvalue *standalone*))
    (setq *standalone* t)    ;; disable messages sucha s TAKE-TURN
    (Format t "Starting Test of grammar")
    (let ((result (remove-if #'null
			     (mapcar (lambda (x) (test-sentence (first x) (second x) (third x))) data)
			     )))
      (format t "Test completed")
      (setq *standalone* savedvalue)
      )
    
    ))

(defun runtest1 (data)
  (let ((result (remove-if #'null
			   (mapcar (lambda (x) (test-sentence1 (first x) (second x) (third x))) data)
			   )))
    result
    ))

(defun new-test-datum (x)
  (list (first x) (right-var (second x) 'SA_ nil) (third x)))


(defun remove-from-test-data (slist datalist)
  (remove-if (lambda (x) (member (first x) slist :test #'equal)) datalist))
;;(setf *test-result (runtest *test-data*))


(setq *monroe-data*
  '(
      ("hello" 
       SA_GREET
       (HELLO END-OF-UTTERANCE))
      ("show me a map of monroe county"
        SA_REQUEST
        ((SHOW ME (A (MAP (OF (MONROE COUNTY))))) END-OF-UTTERANCE))
      ("where are the people"
       SA_WH-QUESTION
       ((WHERE (ARE (THE PEOPLE))) END-OF-UTTERANCE))
      ("where are the transports"
       SA_WH-QUESTION
       ((WHERE (ARE (THE TRANSPORTS))) END-OF-UTTERANCE))
      
      ("use an ambulance to get the people at the pittsford fire station to strong"
       SA_REQUEST
      (((USE (AN AMBULANCE)) (TO ((GET (THE (PEOPLE (AT (THE (PITTSFORD FIRE STATION)))))) (TO STRONG)))) END-OF-UTTERANCE))
      ("how long will that take"
       SA_WH-QUESTION
       (((HOW LONG) (WILL THAT TAKE)) END-OF-UTTERANCE))
      ;; or what if we went along 15A instead
      ("what if we went along 390 instead"
       SA_WHAT-IF-QUESTION
       ((WHAT IF (((WE WENT) (ALONG 390)) INSTEAD)) END-OF-UTTERANCE))
      ("forget it"
       SA_REQUEST
       ((FORGET IT) END-OF-UTTERANCE))
      ("now use the other ambulance to get the person at the irondequoit police station to strong hospital"
       SA_REQUEST
       ((NOW ((USE (THE (OTHER AMBULANCE))) (TO ((GET (THE (PERSON (AT (THE (IRONDEQUOIT POLICE STATION)))))) (TO (STRONG HOSPITAL)))))) END-OF-UTTERANCE)
       )
      ("how long will that take"
       SA_WH-QUESTION
       (((HOW LONG) (WILL THAT TAKE)) END-OF-UTTERANCE))
      ("let's use the helicopter instead"
       SA_REQUEST
       ((LET ^S ((USE (THE HELICOPTER)) INSTEAD)) END-OF-UTTERANCE))
      ("now send bus one to marketplace mall"
       SA_REQUEST
       ((NOW ((SEND (BUS ONE)) (TO (MARKETPLACE MALL)))) END-OF-UTTERANCE))
      ("pick up the people there"
       SA_REQUEST
       (((PICK UP (THE PEOPLE)) THERE) END-OF-UTTERANCE))
      ("go on to the university of rochester"
       SA_REQUEST
       (((GO ON) (TO (THE (UNIVERSITY OF ROCHESTER)))) END-OF-UTTERANCE))
      ("and take the people there to the downtown bus terminal"
       SA_REQUEST
       ((AND ((TAKE (THE (PEOPLE THERE))) (TO (THE (DOWNTOWN BUS TERMINAL))))) END-OF-UTTERANCE))
      ("that looks good"
       SA_TELL
       ((THAT (LOOKS GOOD)) END-OF-UTTERANCE))
      ("mount hope at the inner loop is out"
       SA_TELL
       ((((MOUNT HOPE) AT (THE (INNER LOOP))) (IS OUT)) END-OF-UTTERANCE))
      ("send the bus up plymouth instead"
       SA_REQUEST
       ((((SEND (THE BUS)) (UP PLYMOUTH)) INSTEAD) END-OF-UTTERANCE))
      ("I'm done"
       SA_CLOSE
       ((I ^M DONE) END-OF-UTTERANCE))))









(defparameter *test-data*
    '(
      ("we need to clear this airfield as soon as possible" 
       SA_TELL
       (((WE (NEED (TO (CLEAR (THIS AIRFIELD))))) (AS SOON AS POSSIBLE)) END-OF-UTTERANCE))
      
      ("could we fly the combat units to the airport" SA_YN-QUESTION 
       ((COULD WE ((FLY (THE (COMBAT UNITS))) (TO (THE AIRPORT)))) END-OF-UTTERANCE))
      
      ("hold that option" SA_REQUEST 
       (( ((HOLD (THAT OPTION))) END-OF-UTTERANCE)))
      
      ("what if we send all the units by road" SA_WHAT-IF-QUESTION
       ((WHAT IF ((WE (SEND (ALL (THE UNITS)))) (BY ROAD))) END-OF-UTTERANCE))
      
      ("what rate of march did you use" SA_WH-QUESTION 
       (((WHAT (RATE OF MARCH)) (DID YOU USE)) END-OF-UTTERANCE))

      ("what if we are delayed in the City" SA_WHAT-IF-QUESTION 
       ((WHAT IF ((WE (ARE DELAYED)) (IN (THE CITY)))) END-OF-UTTERANCE))

      ("5 mph" IDENTIFY
       (( ((5 MPH)) END-OF-UTTERANCE)))

      ("can we bypass the city" SA_YN-QUESTION 
       (( (CAN (WE) (BYPASS (THE CITY))) END-OF-UTTERANCE)))

      ("what if we divert just the combat engineering detactments" SA_WHAT-IF-QUESTION
       (( (WHAT IF (WE) (DIVERT (JUST (THE COMBAT ENGINEERING DETACTMENTS)))) END-OF-UTTERANCE)))

      ("what if we fly the combat engineers and their humvees to here" SA_WHAT-IF-QUESTION
       (( (WHAT IF (WE) ((FLY ((THE COMBAT ENGINEERS) AND (THEIR HUMVEES))) TO (HERE))) END-OF-UTTERANCE)))

      ("will the train arrive at 5:30" SA_YN-QUESTION 
       (((WILL (THE TRAIN) ARRIVE) (AT (5 PUNC-COLON 30))) END-OF-UTTERANCE))

      ("There are oranges at This city" SA_TELL 
       ((THERE ARE (ORANGES (AT (THIS CITY)))) END-OF-UTTERANCE))

      ("Is there a train in That village" SA_YN-QUESTION 
       (((IS THERE (A TRAIN)) (IN (THAT VILLAGE))) END-OF-UTTERANCE))

      ("From the city to the town" SPEECH-ACT 
       (( (FROM (the city) TO (the town)) END-OF-UTTERANCE)))

      ("Is the engine coming from This city?" SA_YN-QUESTION 
       (((IS (THE ENGINE) (COMING (FROM (THIS CITY)))) PUNC-QUESTION-MARK) END-OF-UTTERANCE))

      ("The engine is coming from This city?" SA_YN-QUESTION 
       ((((THE ENGINE) ((IS COMING) (FROM (THIS CITY)))) PUNC-QUESTION-MARK) END-OF-UTTERANCE))

      ("The boxcar?" SA_YN-QUESTION 
       ((( (THE BOXCAR) PUNC-QUESTION-MARK) END-OF-UTTERANCE)))

      ("From the city to This city?" SA_YN-QUESTION 
       ((( FROM (the city) TO (This city) PUNC-QUESTION-MARK) END-OF-UTTERANCE)))

      ("I'm done" SA_CLOSE 
       ( (I ^M DONE) END-OF-UTTERANCE))

      ("I'm done, please" SA_CLOSE 
       (((I ^M DONE) (PUNC-COMMA PLEASE)) END-OF-UTTERANCE))

      ("Hello" SA_GREET 
       (HELLO END-OF-UTTERANCE))

      ("How about from the city to This city?" SA_SUGGEST 
       (( ((HOW ABOUT FROM (the city) TO (This city)) PUNC-QUESTION-MARK) END-OF-UTTERANCE)))
      ("Why would that route be faster" SA_WHY-QUESTION 
       (( ((WHY) WOULD (THAT ROUTE) (BE FASTER)) END-OF-UTTERANCE)))
      ("Why not three boxcars of oranges?" SA_WHY-QUESTION 
       (((WHY NOT (THREE (BOXCARS (OF ORANGES)))) PUNC-QUESTION-MARK) END-OF-UTTERANCE))
      ("When will the engine arrive at This village" SA_WH-QUESTION 
       (( ((WHEN) WILL (THE ENGINE) ((ARRIVE) AT (This village))) END-OF-UTTERANCE)))
      ("When" SA_WH-QUESTION 
       (WHEN END-OF-UTTERANCE))
      ("What engine did we send to the town" SA_WH-QUESTION 
       (((WHAT ENGINE) (DID WE (SEND (TO the town)))) END-OF-UTTERANCE)
       )
      
      ("The boxcar is at the town" SA_TELL 
       (((THE BOXCAR) (IS (AT the town))) END-OF-UTTERANCE) 
       )
      
      ("At what time will the train arrive" SA_WH-QUESTION 
       (( (AT (WHAT TIME) WILL (THE TRAIN) (ARRIVE)) END-OF-UTTERANCE))
       )
      ("Where is the train going" SA_WH-QUESTION 
       (( ((WHERE) IS (THE TRAIN) (GOING)) END-OF-UTTERANCE))
       )

      ("We sent the boxcar where" SA_WH-QUESTION 
       (( ((WE) (SENT (THE BOXCAR)) (WHERE)) END-OF-UTTERANCE)))

      ("The train that went to This city arrived" SA_TELL 
       (( ((THE TRAIN THAT ((WENT) TO (This city))) (ARRIVED)) END-OF-UTTERANCE)))
      ("The train is late" SA_TELL 
       (((THE TRAIN) (IS LATE)) END-OF-UTTERANCE)
       )
      ("the city is a city" SA_TELL 
       ((the city (IS (A CITY))) END-OF-UTTERANCE)
       )

      ("The engine is in This village" SA_TELL 
       (((THE ENGINE) (IS (IN This village))) END-OF-UTTERANCE)
       )
      ("Is the train arriving at 5:30" SA_YN-QUESTION 
       (( (IS (THE TRAIN) ((ARRIVING) AT (5 PUNC-COLON 30))) END-OF-UTTERANCE))
       )

      ("Will the train arrive tomorrow" SA_YN-QUESTION 
       (( (WILL (THE TRAIN) (ARRIVE) (TOMORROW)) END-OF-UTTERANCE))
       )

      ("Is the engine at That city" SA_YN-QUESTION 
       ((IS (THE ENGINE) (AT That city)) END-OF-UTTERANCE)
       )

      ("Was the train late?" SA_YN-QUESTION 
       (((WAS (THE TRAIN) LATE) PUNC-QUESTION-MARK) END-OF-UTTERANCE)
       )

      ("Is New york a city" SA_YN-QUESTION 
       (( (IS (NEW YORK) (A CITY)) END-OF-UTTERANCE))
       )

      ("Can we load up the oranges" SA_YN-QUESTION 
       ((CAN WE (LOAD UP (THE ORANGES))) END-OF-UTTERANCE)
       )

      ("The engine was taken to the town" SA_TELL 		 
       (( ((THE ENGINE) (WAS ((TAKEN) TO (the town)))) END-OF-UTTERANCE))
       )

      ("TELL him to go to the city" SA_REQUEST 
       ((SA_TELL HIM (TO (GO (TO the city)))) END-OF-UTTERANCE)
       )
      ("Go to the city instead" SA_REQUEST 
       (((GO (TO the city)) INSTEAD) END-OF-UTTERANCE)
       )
      ("That's OK" SA_TELL
       (( (THAT ^S OK) END-OF-UTTERANCE))
       )
      ("OK" CONFIRM 
       (OK END-OF-UTTERANCE)
       )
      ("Maybe" MAYBE 
       (MAYBE END-OF-UTTERANCE)
       )
      ("Who did you see in the train?" SA_WH-QUESTION 
       (( (((WHO) DID (YOU) (SEE) IN (THE TRAIN)) PUNC-QUESTION-MARK) END-OF-UTTERANCE))
       )

      ("They went to the city maybe" SA_TELL 
       (( ((THEY) (((WENT) TO (the city)) MAYBE)) END-OF-UTTERANCE))
       )
      ("From That city, go to the large town via the car city" SA_REQUEST 
       (((FROM (That city PUNC-COMMA)) ((GO (TO the large town)) (VIA the car city))) END-OF-UTTERANCE)
       )
      ("In That city, the train arrived" SA_TELL 
       (( (IN ((That city) PUNC-COMMA) (THE TRAIN) (ARRIVED)) END-OF-UTTERANCE))
       )
      
      ("The train is not in That city" SA_TELL 
       (((THE TRAIN) (IS NOT (IN That city))) END-OF-UTTERANCE)
       )
      ("Don't go through the large town" SA_REQUEST 
       (( (((DO N^T (GO)) THROUGH (the large town))) END-OF-UTTERANCE))
       )
      ("SA_TELL me the plan" SA_REQUEST 
       ((SA_TELL ME (THE PLAN)) END-OF-UTTERANCE)
       )
      ("Let's go to the town" SA_REQUEST 
       ((LET ^S (GO (TO the town))) END-OF-UTTERANCE)
       )
      ("Have the train sent to the city" SA_REQUEST 
       (( ((HAVE (THE TRAIN) ((SENT) TO (the city)))) END-OF-UTTERANCE))
       )
      ("The train was sent from the town" SA_TELL 
       (( ((THE TRAIN) (WAS ((SENT) FROM (the town)))) END-OF-UTTERANCE))
       )
      ("Let's get the train to the town" SA_REQUEST 
       ((LET ^S ((GET (THE TRAIN)) (TO the town))) END-OF-UTTERANCE)
       )
      ("When does the train get to the town" SA_WH-QUESTION 
       (( ((WHEN) DOES (THE TRAIN) (GET TO (the town))) END-OF-UTTERANCE))
       )
      ("will the train arrive at 5:30" SA_YN-QUESTION 
       (( (WILL (THE TRAIN) ((ARRIVE) AT (5 PUNC-COLON 30))) END-OF-UTTERANCE))
       )
      ("how about the western city to the south city" SA_SUGGEST 
       (( (HOW ABOUT (the western city) TO (the south city)) END-OF-UTTERANCE))
       )
      ("how far is it from the city to the town" SA_WH-QUESTION 
       (( ((HOW FAR) IS IT FROM (the city) TO (the town)) END-OF-UTTERANCE))
       )
      ("highlight the trains" SA_REQUEST 
       ((HIGHLIGHT (THE TRAINS)) END-OF-UTTERANCE)
       )
      ("show me the trains" SA_REQUEST 
       ((SHOW ME (THE TRAINS)) END-OF-UTTERANCE)
       )
      ("where else do i need to go" SA_WH-QUESTION 
       (((WHERE ELSE) (DO I (NEED (TO GO)) )) END-OF-UTTERANCE)
       )
      
      ("where is the train that went to the city" SA_WH-QUESTION 
       ((WHERE (IS (THE (TRAIN (THAT (WENT (TO the city))))))) END-OF-UTTERANCE)
       )
      ("I believe that the train is late" SA_TELL 
       ((I (BELIEVE (THAT ((THE TRAIN) (IS LATE))))) END-OF-UTTERANCE)
       )
      ("what is the shortest route from the city to the town" SA_WH-QUESTION 
       (( ((WHAT) (IS (THE SHORTEST ROUTE FROM (the city) TO (the town)))) END-OF-UTTERANCE))
       )
      ("what should i do now" SA_WH-QUESTION 
       ((WHAT ((SHOULD I DO) NOW)) END-OF-UTTERANCE)
       )
      ("how do I move the trains around" HOW-QUESTION 
       (( ((HOW) DO (I) ((MOVE (THE TRAINS)) AROUND)) END-OF-UTTERANCE))
       )
      ("how many trains are at the city" SA_WH-QUESTION 
       ((((HOW MANY) TRAINS) (ARE (AT the city))) END-OF-UTTERANCE)
       )
      ("what's the distance between some city and the steel city" SA_WH-QUESTION 
       (( ((WHAT) (^S (THE DISTANCE BETWEEN ((some city) AND (the steel city))))) END-OF-UTTERANCE))
       )
      ("go to to the city" SA_REQUEST 
       (( (((GO) TO TO (the city))) END-OF-UTTERANCE))
       )
      ("I want to go to the city and" COMPOUND-COMMUNICATIONS-ACT 
       (((I) (WANT TO ((GO) TO (the city)))) AND)
       )
      ("cancel the whole route" SA_REQUEST 
       (( ((CANCEL (THE WHOLE ROUTE))) END-OF-UTTERANCE))
       )
      ("show me the trains" SA_REQUEST 
       ((SHOW ME (THE TRAINS)) END-OF-UTTERANCE)
       )
      
      ("i'm done" SA_CLOSE 
       ((I ^M DONE) END-OF-UTTERANCE)
       )
      ("is there a faster route" SA_YN-QUESTION 
       ((IS THERE (A (FASTER ROUTE))) END-OF-UTTERANCE)
       )
      ("go to the town via the shortest route" SA_REQUEST 
       (((GO (TO the town)) (VIA (THE (SHORTEST ROUTE)))) END-OF-UTTERANCE)
       )
      ("Is there a faster route" SA_YN-QUESTION 
       ((IS THERE (A (FASTER ROUTE))) END-OF-UTTERANCE)
       )
      ("Is there a faster route than that" SA_YN-QUESTION 
       ((IS THERE (A (FASTER ROUTE THAN THAT))) END-OF-UTTERANCE)
       )
      ("What about going through the city and the town instead" SA_SUGGEST 
       (( (WHAT ABOUT ((GOING) THROUGH ((the city) AND (the town))) INSTEAD) END-OF-UTTERANCE))
       )
      ("What supplies are in the large city" SA_WH-QUESTION 
       (((WHAT SUPPLIES) (ARE (IN the large city))) END-OF-UTTERANCE)
       )
      ("Are the goods loaded" SA_YN-QUESTION 
       (( (ARE (THE GOODS) (LOADED)) END-OF-UTTERANCE))
       )
      ("Will the snowstorm stop soon" SA_YN-QUESTION 
       (( (WILL (THE SNOWSTORM) ((STOP) SOON)) END-OF-UTTERANCE))
       )
      ("How much will that cost" SA_WH-QUESTION 
       (((HOW MUCH) (WILL THAT COST)) END-OF-UTTERANCE)
       )
      ("I have how much money" SA_WH-QUESTION 
       (( ((I) (HAVE (HOW MUCH MONEY))) END-OF-UTTERANCE))
       )
      ("Is there a delay involved" SA_YN-QUESTION 
       (( (IS THERE (A DELAY (INVOLVED))) END-OF-UTTERANCE))
       )
      ("WHAT DID YOU SAY" SA_WH-QUESTION
       ((WHAT (DID YOU SAY)) END-OF-UTTERANCE)
       )
      
      ("how long will it take to get to the city" SA_WH-QUESTION 
       (( ((HOW LONG) WILL (IT) (TAKE TO (GET TO (the city)))) END-OF-UTTERANCE))
       )
      ("how much time will it take" SA_WH-QUESTION 
       (( ((HOW MUCH TIME) WILL (IT) (TAKE)) END-OF-UTTERANCE))
       )
      ("how long will it take for the train to arrive" SA_WH-QUESTION 
       (((HOW LONG) (WILL IT (TAKE (FOR (THE TRAIN) TO ARRIVE)))) END-OF-UTTERANCE)
       )
      
      ("how long is that route" SA_WH-QUESTION 
       (((HOW LONG) (IS (THAT ROUTE))) END-OF-UTTERANCE)
       )
      ("what is the cost to travel from the city to the town" SA_WH-QUESTION 
       ((WHAT (IS (THE (COST (TO ((TRAVEL (FROM the city)) (TO the town))))))) END-OF-UTTERANCE)
       )  
      ("The distance of the route is seven miles" SA_TELL 
       (( ((THE DISTANCE OF (THE ROUTE)) (IS (SEVEN MILES))) END-OF-UTTERANCE))
       )
      ("The route is seven miles long" SA_TELL 
       (( ((THE ROUTE) (IS (SEVEN MILES) LONG)) END-OF-UTTERANCE))
       )
      ("The route is how long" SA_WH-QUESTION 
       (((THE ROUTE) (IS (HOW LONG))) END-OF-UTTERANCE)
       )
      
      ("What train do i need to see" SA_WH-QUESTION 
       (( ((WHAT TRAIN) DO (I) (NEED TO (SEE))) END-OF-UTTERANCE))
       )
      ("this is the train which is currently at the city of love" SA_TELL 
       (( ((THIS) (IS (THE TRAIN WHICH (IS CURRENTLY) AT (the city of love)))
		  )
	 END-OF-UTTERANCE))
       )
      ("show me how to get to the city" SA_REQUEST 
       (( ((SHOW (ME) (HOW TO (GET TO (the city))))) END-OF-UTTERANCE))
       )
      ("he asked me what to do" SA_TELL 
       ((HE (ASKED ME (WHAT (TO DO)))) END-OF-UTTERANCE)
       )
      ("is it fast to go through the city" SA_YN-QUESTION 
       (( (IS (IT) FAST TO ((GO) THROUGH (the city))) END-OF-UTTERANCE))
       )
      ("It is fastest to go through the city" SA_TELL 
       ((IT (IS (FASTEST (TO (GO (THROUGH the city)))))) END-OF-UTTERANCE)
       )

      ("Is it faster to go through the city instead of the town" SA_YN-QUESTION 
       (( ((IS (IT) FASTER TO ((GO) THROUGH (the city))) INSTEAD OF (the town)) END-OF-UTTERANCE))
       )

      ("where are the people" SA_WH-QUESTION 
       ((WHERE (ARE (THE PEOPLE))) END-OF-UTTERANCE)
       )
      ("are there any people at a city" SA_YN-QUESTION 
       (( (ARE THERE (ANY PEOPLE AT (a city))) END-OF-UTTERANCE))
       )
      ("show me all the vehicles" SA_REQUEST 
       ((SHOW ME (ALL (THE VEHICLES))) END-OF-UTTERANCE)
       )
      
      ("how long does that take" SA_WH-QUESTION 
       (((HOW LONG) (DOES THAT TAKE)) END-OF-UTTERANCE)
       )
      
      ("let's take helicopter one from a city to that town" SA_REQUEST 
       ((LET ^S (((TAKE (HELICOPTER ONE)) (FROM a city)) (TO that town))) END-OF-UTTERANCE)
       )

      ("pick up a group and take it back to a city" COMPOUND-COMMUNICATIONS-ACT 
       (((PICK UP (A GROUP))) ((AND (((TAKE (IT)) BACK TO (a city)))) END-OF-UTTERANCE))
       )

      ("use truck one to evacuate the remaining group at that town" SA_REQUEST 
       (( (((USE (TRUCK ONE)) TO (EVACUATE (THE REMAINING GROUP AT (that town))))) END-OF-UTTERANCE))
       )
      
      ("now let's use the helicopter to evacuate one of the groups at somewhere" SA_REQUEST
       (( (NOW (LET (^S) ((USE (THE HELICOPTER)) TO (EVACUATE (ONE OF (THE GROUPS AT (somewhere))))))) END-OF-UTTERANCE))
       )
      ("what if we used both trucks to get the remaining groups at here" SA_WHAT-IF-QUESTION 
       (( (WHAT IF (WE) ((USED (BOTH TRUCKS)) TO (GET (THE REMAINING GROUPS AT (here))))) END-OF-UTTERANCE))
       )
      ("would it be faster to take the trucks along the coastal road" SA_YN-QUESTION 
       (( (WOULD (IT) (BE FASTER TO ((TAKE (THE TRUCKS)) ALONG (THE COASTAL ROAD)))) END-OF-UTTERANCE))
       )

      ("my goal is to rescue the people" SA_TELL 
       (( ((MY GOAL) (IS TO (RESCUE (THE PEOPLE)))) END-OF-UTTERANCE))
       )
      ("use the inland road" SA_REQUEST 
       ((USE (THE (INLAND ROAD))) END-OF-UTTERANCE)
       )
      ("now use the helicopter in place of the trucks for this town and that town" SA_REQUEST 
       (( ((NOW ((USE (THE HELICOPTER)))) IN PLACE OF (THE TRUCKS FOR ((this town) AND (that town)))) END-OF-UTTERANCE))
       )

      ("scratch the plan involving a truck and this town" SA_REQUEST 
       (( ((SCRATCH (THE PLAN (INVOLVING ((A TRUCK) AND (this town)))))) END-OF-UTTERANCE))
       )
      ("use the helicopter to retrieve that group" SA_REQUEST 
       (( (((USE (THE HELICOPTER)) TO (RETRIEVE (THAT GROUP)))) END-OF-UTTERANCE))
       )
      ("what is this" SA_WH-QUESTION 
       ((WHAT (IS THIS)) END-OF-UTTERANCE)
       )
      ("what is this here" SA_WH-QUESTION 
       (( ((WHAT) (IS (THIS (HERE)))) END-OF-UTTERANCE))
       )
      ("do this with truck two instead" SA_REQUEST 
	  (( (((DO (THIS WITH (TRUCK TWO)))) INSTEAD) END-OF-UTTERANCE))
	)
      ("i meant the coastal road from here to a city" SA_TELL 
       (( ((I) (MEANT (THE COASTAL ROAD FROM (here) TO (a city)))) END-OF-UTTERANCE))
       )
      ("we need to clear the airfield of mines" SA_TELL 
       (( ((WE) (NEED TO (CLEAR (THE AIRFIELD) OF (MINES)))) END-OF-UTTERANCE))
       )
      ("where are the available engineering units" SA_WH-QUESTION 
       ((WHERE (ARE (THE (AVAILABLE (ENGINEERING UNITS))))) END-OF-UTTERANCE)
       )
      ("let's fly the light unit in by helicopter" SA_REQUEST
       (( (LET (^S) ((FLY (THE LIGHT UNIT)) IN) BY (HELICOPTER)) END-OF-UTTERANCE))
       )

      ("let's send both units in by road instead" SA_REQUEST 
       (( ((LET (^S) ((SEND (BOTH UNITS)) IN) BY (ROAD)) INSTEAD) END-OF-UTTERANCE))
       )

      ("Can we avoid the city" SA_YN-QUESTION 
       (( (CAN (WE) (AVOID (THE CITY))) END-OF-UTTERANCE))
       )
      ("can we fly the light unit to here" SA_YN-QUESTION 
       (( (CAN (WE) ((FLY (THE LIGHT UNIT)) TO (HERE))) END-OF-UTTERANCE))
       )

      ("and have them walk in" SA_REQUEST 
       (( (AND ((HAVE (THEM) (WALK)) (IN))) END-OF-UTTERANCE))
       )

      ("can we get them some vehicles" SA_YN-QUESTION 
       (( (CAN (WE) (GET (THEM) (SOME VEHICLES))) END-OF-UTTERANCE))
       )
      ("let's do that" SA_REQUEST
       ((LET ^S (DO THAT)) END-OF-UTTERANCE)
       )
      ("take the people off the truck" SA_REQUEST
       ((TAKE (THE PEOPLE) (OFF (THE TRUCK))) END-OF-UTTERANCE)
       )

      ;;  CAMP demo sentences
      ("good morning" SA_GREET
       ((GOOD MORNING) END-OF-UTTERANCE)
       )
      ("what's new" SA_WH-QUESTION
       ((WHAT (^S NEW)) END-OF-UTTERANCE)
       )
      ("let's work on the man's plan" SA_REQUEST
       (( (LET (^S) (WORK ON (THE (GEORGES) PLAN))) END-OF-UTTERANCE))
       )
      ("show the lift REQuirements" SA_REQUEST
       (( ((SHOW (THE LIFT SA_REQUIREMENTS))) END-OF-UTTERANCE))
       )
      ("estimate resources needed" SA_REQUEST
       (( ((ESTIMATE (RESOURCES (NEEDED)))) END-OF-UTTERANCE))
       )
      ("estimate resource needs" SA_REQUEST
       (( ((ESTIMATE (RESOURCE NEEDS))) END-OF-UTTERANCE))
       )


      ("where can we get them" SA_WH-QUESTION
       (( ((WHERE) CAN (WE) (GET (THEM))) END-OF-UTTERANCE))
       )

      ("what if we only use planes from east bases" SA_WHAT-IF-QUESTION
       ((WHAT IF (WE (ONLY (USE (PLANES (FROM (EAST BASES))))))) END-OF-UTTERANCE)
       )

      ("use Charleston as an intermediate stop" SA_REQUEST
       ((USE CHARLESTON (AS (AN (INTERMEDIATE STOP)))) END-OF-UTTERANCE)
       )

       ))
  

(defvar *camps-test*
    '(
      
      ("are there any one forty ones available between day 5 and day 7" SA_YN-QUESTION
									(( (ARE THERE (ANY ONE FORTY ONES AVAILABLE BETWEEN (DAY 5 AND DAY 7))
										)
									   END-OF-UTTERANCE))
									)
      ("let's use the c five from travis also" SA_REQUEST
       (( (LET (^S) ((USE (THE C FIVE FROM (TRAVIS))) ALSO)) END-OF-UTTERANCE))
       )
      
      
      ("use the one available on day 6" SA_REQuest
       (( (((USE (THE ONE AVAILABLE)) ON (DAY 6))) END-OF-UTTERANCE))
       )
      
      ("confirm this with the barrel" SA_REQuest
       (( ((CONFIRM (THIS) WITH (THE BARREL))) END-OF-UTTERANCE))
       )
      
      ("where are the refueling ports" SA_WH-question
       (( ((WHERE) ARE (THE REFUELING PORTS)) END-OF-UTTERANCE))
       )
      ))



(defvar *basic-bad*
    '("hold that option"
      "what rate of march did you use"
      "5 mph"
      "what if we divert just the combat engineering detactments"
      "what if we fly the combat engineers and their humvees to here"
      "From the city to the town"
      "The boxcar?"
      "From the city to This city?"
      "How about from the city to This city?"
      "Is New york a city"
      "Have the train sent to the city"
      "how about the western city to the south city"
      "how far is it from avon to the town"
      "what is the shortest route from avon to the town"
      "what's the distance between some city and the steel city"
      "go to to avon"
      "I want to go to Avon and"
      "cancel the whole route"
      "go to the town via the shortest route"
      "What about going through avon and the town instead"
      "how much time will it take"
      "how long will it take for the train to arrive"
      "what is the cost to travel from avon to the town"
      "The distance of the route is seven miles"
      "The route is seven miles long"
      "The route is how long"
      "What train do i need to see"
      "is it fast to go through avon"
      "Is it faster to go through Avon instead of the town"
      "pick up a group and take it back to a city"
      "my goal is to rescue the people"
      "now use the helicopter in place of the trucks for this town and that town"
      "scratch the plan involving a truck and this town"
      "i meant the coastal road from here to a city"
      "let's fly the light unit in by helicopter"
      "let's send both units in by road instead"
      "Can we avoid the city"
      "can we get them some vehicles"
      "let's work on the georges plan"
      "show the lift SA_REQuirements"
      "estimate resources needed"
      "estimate resource needs"
))

(defvar *kr-bad*
    '("could we fly the combat units to the airport" 
      "what if we send all the units by road" 
      "Avon is a city" 
      "The engine was taken to the town"
      "Let's get the train to the town" 
      "how do I move the trains around" 
      "use truck one to evacuate the remaining group at that town"
      "now let's use the helicopter to evacuate one of the groups at somewhere" 
      "would it be faster to take the trucks along the coastal road"
      "use the helicopter to retrieve that group" 
      "can we fly the light unit to here" 
      "take the people off the truck")
  )

(defparameter *test-sentences* (mapcar #'first *test-data*))
(defparameter *new-test-data* (mapcar #'new-test-datum *test-data*))

(defparameter *basic-good-test-data* (remove-from-test-data *basic-bad* *new-test-data*))
(defparameter *good-test-data* 
    (remove-from-test-data *kr-bad* *basic-good-test-data*))

(defvar *lattice-input* 
  '((word "<GAP>" :frame (1 66) :score 1.000000 :uttnum 2) 
    (word "AND" :frame (67 93) :score 0.600000 :uttnum 2) 
    (word "<GAP>" :frame (94 106) :score 0.200000 :uttnum 2) 
    (word "IT" :frame (107 124) :score 0.100000 :uttnum 2) 
    (word "<GAP>" :frame (94 105) :score 0.300000 :uttnum 2) 
    (word "AND" :frame (106 147) :score 0.200000 :uttnum 2) 
    (word "AND" :frame (67 94) :score 0.400000 :uttnum 2) 
    (word "<GAP>" :frame (95 107) :score 0.400000 :uttnum 2) 
    (word "THAT" :frame (108 146) :score 0.100000 :uttnum 2) 
    (word "THEN" :frame (108 144) :score 0.200000 :uttnum 2) 
    (word "<GAP>" :frame (94 123) :score 0.100000 :uttnum 2) 
    (word "UP" :frame (124 147) :score 0.100000 :uttnum 2) 
    (word "LAKE" :frame (108 147) :score 0.100000 :uttnum 2) 
    (word "<GAP>" :frame (148 226) :score 0.100000 :uttnum 2) 
    (word "A" :frame (227 229) :score 0.100000 :uttnum 2) 
    (word "AT" :frame (107 146) :score 0.100000 :uttnum 2) 
    (word "<GAP>" :frame (145 146) :score 0.100000 :uttnum 2) 
    (word "THE" :frame (147 228) :score 0.100000 :uttnum 2) 
    (word "AN" :frame (106 124) :score 0.100000 :uttnum 2)))
