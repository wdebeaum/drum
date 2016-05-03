;; nomlex-verbnoms.lisp
;;
;; Time-stamp: <Fri Dec  5 09:25:52 EST 2014 jallen>
;;
;; Author: Lucian Galescu <lgalescu@ihmc.us>,  4 Dec 2014
;;

;; examples of usage:
;; (nomlex-lookup-nom "commission")
;; ==> ((NOM :NOUN "commission" :VERB "commission" :NOM-TYPE ((OBJECT) (COMP))
;;       :PREPS ("by") :SOURCE NOMLEX)
;;      (NOM :NOUN "commission" :VERB "commit" :NOM-TYPE ((VERB-NOM)) :PREPS
;;       ("of" "by") :SOURCE NOMLEX))
;; (nomlex-lookup-nom "commission" :type 'VERB-NOM)
;; ==> (NOM :NOUN "commission" :VERB "commit" :NOM-TYPE ((VERB-NOM)) :PREPS
;;      ("of" "by") :SOURCE NOMLEX))
;; (nomlex-lookup-nom "clampdown" :type 'VERB-PART)
;; ==> ((NOM :NOUN "clampdown" :VERB "clamp" :NOM-TYPE ((VERB-PART :ADVAL ("down")))
;;       :PREPS ("by") :SOURCE NOMLEX))
;; (nomlex-lookup-nom "clampdown" :type 'VERB-NOM)
;; ==> NIL
(in-package "LEXICONMANAGER")

(defun nomlex-lookup-nom (noun &key type)
  (loop
     for entry in *nomlex-verb-nominalizations*
     for enoun = (get-keyword-arg entry :NOUN)
     for nomtypes = (mapcar #'car (get-keyword-arg entry :NOM-TYPE))
     when (and (equalp noun enoun)
	       (or (null type) (member type nomtypes)))
     collect entry)
  )

(defun filter-nomlex nil
  (mapcar #'test-lexicon *nomlex-verb-nominalizations*))

(defun test-lexicon (entry)
  (when (equal (get-keyword-arg entry :nom-type) '((VERB-NOM)))
    (let* ((nstring (get-keyword-arg entry :noun))
	   (vstring (get-keyword-arg entry :verb))
	   (n (if (stringp nstring) (parser::tokenize nstring)))
	   (v (if (stringp vstring)(parser::tokenize vstring))))
    (when (and n v)
      (if (retrieve-from-lex (car v))
	  (if (retrieve-from-lex (car n))
	    (if (equal n v)
		;; check that there's both an N and a V entry
		(let ((es (retrieve-from-lex (car v))))
		  (if (not (and (contains-an-entry es 'W::V) (contains-an-entry es 'W::N)))
		      (if (contains-an-entry es 'W::V)
			  (format t "~%~S missing nom with same name" v)
			  (format t "~%~S exists as a noun, but no verb" v))))
		T) ;;(format t "~%~S exists already with nominalization ~S" v n))
	    (format t "~%~S exists and missing nominlaization ~S" v n))
	  t) ;;(format t "~%~S is missing" v))
      ))))

(defun contains-an-entry (entries pos)
  (find-if #'(lambda (x) (eq (car (lex-entry-description x)) pos))
	   entries))

(setq *nomlex-verb-nominalizations* 
      '((NOM :NOUN "abandonment" :VERB "abandon" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "abasement" :VERB "abase" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "abatement" :VERB "abate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "abbreviation" :VERB "abbreviate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "abdication" :VERB "abdicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "abduction" :VERB "abduct" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "abductor" :VERB "abduct" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "abhorrence" :VERB "abhor" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "abjuration" :VERB "abjure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "abolishment" :VERB "abolish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "abolition" :VERB "abolish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "abomination" :VERB "abominate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "abortion" :VERB "abort" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "for") :SOURCE NOMLEX)
	(NOM :NOUN "abrasion" :VERB "abrade" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "abrasive" :VERB "abrade" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "abridgement" :VERB "abridge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "abrogation" :VERB "abrogate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "abscess" :VERB "abscess" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "absentee" :VERB "absent" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "absorbent" :VERB "absorb" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "absorber" :VERB "absorb" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "absorption" :VERB "absorb" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "abstainer" :VERB "abstain" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "abstention" :VERB "abstain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "abstract" :VERB "abstract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "abstraction" :VERB "abstract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "abuse" :VERB "abuse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "abuser" :VERB "abuse" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "abutment" :VERB "abut" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "acceleration" :VERB "accelerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of") :SOURCE NOMLEX)
	(NOM :NOUN "accelerator" :VERB "accelerate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "accent" :VERB "accent" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "accentuation" :VERB "accentuate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "acceptance" :VERB "accept" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "acceptation" :VERB "accept" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "access" :VERB "access" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "accession" :VERB "ascend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "acclaim" :VERB "acclaim" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "acclimation" :VERB "acclimate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "acclimatization" :VERB "acclimatize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "accommodation" :VERB "accommodate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "accompaniment" :VERB "accompany" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "accompanist" :VERB "accompany" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "accomplishment" :VERB "accomplish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "accord" :VERB "accord" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "accordance" :VERB "accord" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "account" :VERB "account" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "accreditation" :VERB "accredit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "accretion" :VERB "accrete" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "accrual" :VERB "accrue" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "accumulation" :VERB "accumulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "accumulator" :VERB "accumulate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "accusation" :VERB "accuse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("against" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "accuser" :VERB "accuse" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "achievement" :VERB "achieve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "acknowledgement" :VERB "acknowledge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "acknowledgment" :VERB "acknowledge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "acquaintance" :VERB "acquaint" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "acquiescence" :VERB "acquiesce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "acquirement" :VERB "acquire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "acquirer" :VERB "acquire" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "acquisition" :VERB "acquire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "acquittal" :VERB "acquit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "act" :VERB "act" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "acting" :VERB "act" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "action" :VERB "act" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "activation" :VERB "activate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "actor" :VERB "act" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "actress" :VERB "act" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "ad" :VERB "advertisement" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "adaptation" :VERB "adapt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "adapter" :VERB "adapt" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "adaptor" :VERB "adapt" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "addict" :VERB "addict" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "addiction" :VERB "addict" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "addition" :VERB "add" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "address" :VERB "address" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "addressee" :VERB "address" :NOM-TYPE ((OBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "addressee2" :VERB "address" :NOM-TYPE ((IND-OBJ :PVAL ("to")))
	 :PREPS ("of") :SOURCE NOMLEX)
	(NOM :NOUN "adherence" :VERB "adhere" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "adherent" :VERB "adhere" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "adhesion" :VERB "adhere" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "adhesive" :VERB "adhere" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "adjournment" :VERB "adjourn" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "adjudication" :VERB "adjudicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "adjudicator" :VERB "adjudicate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "adjuration" :VERB "adjure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "adjuster" :VERB "adjust" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "adjustment" :VERB "adjust" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "administration" :VERB "administer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "administrator" :VERB "administrate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "admiration" :VERB "admire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "for") :SOURCE NOMLEX)
	(NOM :NOUN "admirer" :VERB "admire" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "admission" :VERB "admit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "admittance" :VERB "admit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "admonition" :VERB "admonish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "adoption" :VERB "adopt" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "adoration" :VERB "adore" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "adorer" :VERB "adore" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "adornment" :VERB "adorn" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "adulterant" :VERB "adulterate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "adulteration" :VERB "adulterate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "advance" :VERB "advance" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "advancement" :VERB "advance" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "advancer" :VERB "advance" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "adventurer" :VERB "adventure" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "advertisement" :VERB "advertise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "advertiser" :VERB "advertise" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "advice" :VERB "advise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "adviser" :VERB "advise" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "advisor" :VERB "advise" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "advisory" :VERB "advice" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "advocacy" :VERB "advocate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "advocate" :VERB "advocate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "aeration" :VERB "aerate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "affect" :VERB "affect" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "affectation" :VERB "affect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "affection" :VERB "affect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "affiliate" :VERB "affiliate" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "affiliation" :VERB "affiliate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "affirmation" :VERB "affirm" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "affliction" :VERB "afflict" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "afforestation" :VERB "afforest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "agglomeration" :VERB "agglomerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "aggrandizement" :VERB "aggrandize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "aggravation" :VERB "aggravate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "aggregate" :VERB "aggregate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "between" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "aggregation" :VERB "aggregate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "between" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "aggression" :VERB "agress" :NOM-TYPE ((VERB-NOM)) :PREPS ("by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "agitation" :VERB "agitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "agitator" :VERB "agitate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "agreement" :VERB "agree" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "among" "amongst" "between" "by") :SOURCE NOMLEX)
	(NOM :NOUN "aid" :VERB "aid" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "ailment" :VERB "ail" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "aim" :VERB "aim" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "air" :VERB "air" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "alarm" :VERB "alarm" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("over" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "alarmist" :VERB "alarm" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "alert" :VERB "alert" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "alienation" :VERB "alienate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "alignment" :VERB "align" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "allegation" :VERB "allege" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "alleviation" :VERB "alleviate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "alliance" :VERB "ally" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "allocation" :VERB "allocate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "allocator" :VERB "allocate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "allotment" :VERB "allot" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "allowance" :VERB "allow" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "allure" :VERB "allure" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "allurement" :VERB "allure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "allusion" :VERB "allude" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ally" :VERB "ally" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "alteration" :VERB "alter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "alternation" :VERB "alternate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "among" "between" "by") :SOURCE NOMLEX)
	(NOM :NOUN "amalgamation" :VERB "amalgamate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "amazement" :VERB "amaze" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "by" "over" "of") :SOURCE NOMLEX)
	(NOM :NOUN "amelioration" :VERB "ameliorate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "amendment" :VERB "amend" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "amortization" :VERB "amortize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "amplification" :VERB "amplify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "amplifier" :VERB "amplify" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "amputation" :VERB "amputate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "amusement" :VERB "amuse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "over" "by" "in" "of") :SOURCE NOMLEX)
	(NOM :NOUN "anaesthetist" :VERB "anaesthetize" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "analysis" :VERB "analyze" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "analyst" :VERB "analyze" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "analyzer" :VERB "analyze" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "anesthetist" :VERB "anesthetize" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "anger" :VERB "anger" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "among" "in" "of") :SOURCE NOMLEX)
	(NOM :NOUN "angler" :VERB "angle" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "animadversion" :VERB "animadvert" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "animation" :VERB "animate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "annexation" :VERB "annex" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "annihilation" :VERB "annihilate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "annotation" :VERB "annotate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "announcement" :VERB "announce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "announcer" :VERB "announce" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "annoyance" :VERB "annoy" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("over" "to" "at" "with") :SOURCE NOMLEX)
	(NOM :NOUN "annulment" :VERB "annul" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "annunciation" :VERB "annunciate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "anointment" :VERB "anoint" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "answer" :VERB "answer" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "antagonism" :VERB "antagonize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "antagonist" :VERB "antagonize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "anticipation" :VERB "anticipate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "apologist" :VERB "apologize" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "apology" :VERB "apologize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "apparition" :VERB "appear" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "appeal" :VERB "appeal" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "appearance" :VERB "appear" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "appeasement" :VERB "appease" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "applause" :VERB "applaud" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "appliance" :VERB "apply" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "applicant" :VERB "apply" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "application" :VERB "apply" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "appointee" :VERB "appoint" :NOM-TYPE ((OBJECT)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "appointment" :VERB "appoint" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "apportionment" :VERB "apportion" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "appraisal" :VERB "appraise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "appraiser" :VERB "appraise" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "appreciation" :VERB "appreciate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "on") :SOURCE NOMLEX)
	(NOM :NOUN "apprehension" :VERB "apprehend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "approach" :VERB "approach" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "on" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "approbation" :VERB "approve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "appropriation" :VERB "appropriate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "appropriator" :VERB "appropriate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "approval" :VERB "approve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "approximation" :VERB "approximate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "arbitration" :VERB "arbitrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "arbitrator" :VERB "arbitrate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "argument" :VERB "argue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "with" "between" "amongst" "by" "among") :SOURCE NOMLEX)
	(NOM :NOUN "armor" :VERB "arm" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "arraignment" :VERB "arraign" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "arrangement1" :VERB "arrange" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "arrangement2" :VERB "arrange" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "arrest" :VERB "arrest" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "arrester" :VERB "arrest" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "arrival" :VERB "arrive" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "articulation" :VERB "articulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "ascendant" :VERB "ascend" :NOM-TYPE ((P-OBJ :PVAL ("to"))) :PREPS
	 ("by") :SOURCE NOMLEX)
	(NOM :NOUN "ascendency" :VERB "ascend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "ascendent" :VERB "ascend" :NOM-TYPE ((P-OBJ :PVAL ("to"))) :PREPS
	 ("by") :SOURCE NOMLEX)
	(NOM :NOUN "ascension" :VERB "ascend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "ascent" :VERB "ascend" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ascription" :VERB "ascribe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "aspersion" :VERB "asperse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "asphyxiation" :VERB "asphyxiate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "aspirant" :VERB "aspirate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "aspiration1" :VERB "aspire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "aspiration2" :VERB "aspirate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "assailant" :VERB "assail" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "assassination" :VERB "assassinate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "assault" :VERB "assault" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "assemblage" :VERB "assemble" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "assembly" :VERB "assemble" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "assent" :VERB "assent" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "assertion" :VERB "assert" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "assessment" :VERB "assess" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "assessor" :VERB "assess" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "asseveration" :VERB "asseverate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "assignation" :VERB "assign" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "assignment" :VERB "assign" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "assimilation" :VERB "assimilate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "assistance" :VERB "assist" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "assistant" :VERB "assist" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "associate" :VERB "associate" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "association" :VERB "associate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "assumption" :VERB "assume" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "assurance" :VERB "assure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to" "from") :SOURCE NOMLEX)
	(NOM :NOUN "astonishment" :VERB "astonish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "by" "over" "of") :SOURCE NOMLEX)
	(NOM :NOUN "atomizer" :VERB "atomize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "atonement" :VERB "atone" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "attachment" :VERB "attach" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "attack" :VERB "attack" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "on" "against" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "attacker" :VERB "attack" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "attainment" :VERB "attain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "attempt" :VERB "attempt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "at" "of") :SOURCE NOMLEX)
	(NOM :NOUN "attendance" :VERB "attend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "at" "of") :SOURCE NOMLEX)
	(NOM :NOUN "attendant" :VERB "attend" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "attendee" :VERB "attend" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "at")
	 :SOURCE NOMLEX)
	(NOM :NOUN "attention" :VERB "attend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "attenuation" :VERB "attenuate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "attraction" :VERB "attract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "attribution" :VERB "attribute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "auction" :VERB "auction" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "auctioneer" :VERB "auction" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "audit" :VERB "audit" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "audition" :VERB "audition" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "auditor" :VERB "audit" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "augmentation" :VERB "augment" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "augury" :VERB "augur" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "authentication" :VERB "authenticate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "authority" :VERB "authorize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "authorization" :VERB "authorize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("under" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "autograph" :VERB "autograph" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "automation" :VERB "automate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "avenger" :VERB "avenge" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "average" :VERB "average" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "aversion" :VERB "avert" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "avoidance" :VERB "avoid" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "avowal" :VERB "avow" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "award" :VERB "award" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "babbler" :VERB "babble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "babysitter" :VERB "babysit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "back-up" :VERB "back" :NOM-TYPE
	 ((VERB-PART :ADVAL ("up")) (OBJECT-PART :ADVAL ("up"))) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "backbiter" :VERB "backbite" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "backer" :VERB "back" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "backlog" :VERB "backlog" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bagger" :VERB "bag" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bail" :VERB "bail" :NOM-TYPE ((VERB-PART :ADVAL ("out"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "bailment" :VERB "bail" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bailor" :VERB "bail" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bailout" :VERB "bail" :NOM-TYPE ((VERB-PART :ADVAL ("out")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "baker" :VERB "bake" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "balance" :VERB "balance" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ban" :VERB "ban" :NOM-TYPE ((VERB-NOM)) :PREPS ("from" "of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bang" :VERB "bang" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "banishment" :VERB "banish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "banker" :VERB "bank" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "banking" :VERB "bank" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "banner" :VERB "ban" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "banter" :VERB "banter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "baptism" :VERB "baptize" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "barbarism" :VERB "barbarize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "bargain" :VERB "bargain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "between" "among" "amongst" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "barker" :VERB "bark" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "barnstormer" :VERB "barnstorm" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "barrier" :VERB "bar" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "barter" :VERB "barter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "barterer" :VERB "barter" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "base" :VERB "base" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "on" "to") :SOURCE NOMLEX)
	(NOM :NOUN "basis" :VERB "base" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "to" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bather" :VERB "bathe" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "batter" :VERB "bat" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "batting" :VERB "bat" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "battle" :VERB "battle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "by" "against" "of" "between") :SOURCE NOMLEX)
	(NOM :NOUN "bearer" :VERB "bear" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bearing" :VERB "bear" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "beater" :VERB "beat" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "beatification" :VERB "beatify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "bedevilment" :VERB "bedevil" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "begetter" :VERB "beget" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "beggary" :VERB ("beg" "beggar") :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "beginner" :VERB "begin" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "behavior" :VERB "behave" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "in") :SOURCE NOMLEX)
	(NOM :NOUN "beholder" :VERB "behold" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "being" :VERB "be" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "belch" :VERB "belch" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "belief" :VERB "believe" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "believer" :VERB "believe" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bender" :VERB "bend" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "benefactor" :VERB "benefit" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "beneficiary" :VERB "benefit" :NOM-TYPE ((OBJECT)) :PREPS
	 ("of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "benefit" :VERB "benefit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("such as" "for" "of" "by" "to" "from" "with") :SOURCE NOMLEX)
	(NOM :NOUN "bequest" :VERB "bequeath" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "bereavement" :VERB "bereave" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "besieger" :VERB "besiege" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bestowal" :VERB "bestow" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bet" :VERB "bet" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "for" "to") :SOURCE NOMLEX)
	(NOM :NOUN "betrayal" :VERB "betray" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "betrayer" :VERB "betray" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "betrothal" :VERB "betroth" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "betterment" :VERB "better" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bettor" :VERB "bet" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bewilderment" :VERB "bewilder" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "bias" :VERB "bias" :NOM-TYPE ((VERB-NOM)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bid" :VERB "bid" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "bidder" :VERB "bid" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "bidding" :VERB "bid" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "to" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "bifurcation" :VERB "bifurcate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "bill" :VERB "bill" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "billing" :VERB "bill" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "bind" :VERB "bind" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "binder" :VERB "bind" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "binge" :VERB "binge" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "birth" :VERB "bear" :NOM-TYPE ((VERB-NOM)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bisection" :VERB "bisect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "bite" :VERB "bite" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "blackmailer" :VERB "blackmail" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "blame" :VERB "blame" :NOM-TYPE ((VERB-NOM)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "blasphemer" :VERB "blaspheme" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "blast" :VERB "blast" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "blazer" :VERB "blaze" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "blazonry" :VERB "blazon" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bleach" :VERB "bleach" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "blemish" :VERB "blemish" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "blend" :VERB "blend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "among" "between" "by") :SOURCE NOMLEX)
	(NOM :NOUN "blighter" :VERB "blight" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "blink" :VERB "blink" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "blip" :VERB "blip" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "blockade" :VERB "blockade" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "blockage" :VERB "block" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "blocker" :VERB "block" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "blocking" :VERB "block" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bloomer" :VERB "bloom" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "blotter" :VERB "blot" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "blow" :VERB "blow" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "blower" :VERB "blow" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bluffer" :VERB "bluff" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "blunder" :VERB "blunder" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "blunderer" :VERB "blunder" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "boarder" :VERB "board" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "boast" :VERB "boast" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "boaster" :VERB "boast" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "boater" :VERB "boat" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "boiler" :VERB "boil" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bombardment" :VERB "bombard" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "bomber" :VERB "bomb" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bond" :VERB "bond" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "booking" :VERB "book" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "boom" :VERB "boom" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by" "in")
	 :SOURCE NOMLEX)
	(NOM :NOUN "boost" :VERB "boost" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "booster" :VERB "boost" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bootlegger" :VERB "bootleg" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "boozer" :VERB "booze" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "borderer" :VERB "border" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "boredom" :VERB "bore" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "borer" :VERB "bore" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "borrower" :VERB "borrow" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "borrowing" :VERB "borrow" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "botcher" :VERB "botch" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bottler" :VERB "bottle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bounce" :VERB "bounce" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bounder" :VERB "bound" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bowler" :VERB "bowl" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "boxer" :VERB "box" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "boxing" :VERB "box" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bracket" :VERB "bracket" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bravery" :VERB "brave" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "brawl" :VERB "brawl" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "brawler" :VERB "brawl" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "breach" :VERB "breach" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "against") :SOURCE NOMLEX)
	(NOM :NOUN "break" :VERB "break" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "break-up" :VERB "break" :NOM-TYPE ((VERB-PART :ADVAL ("up")))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "breakage" :VERB "break" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "breakdown" :VERB "break" :NOM-TYPE ((VERB-PART :ADVAL ("down")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "breaker" :VERB "break" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "breakup" :VERB "break" :NOM-TYPE ((VERB-PART :ADVAL ("up")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "breath" :VERB "breath" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "breather" :VERB "breathe" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "breathing" :VERB "breathe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "breeder" :VERB "breed" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "breeding" :VERB "breed" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "brewer" :VERB "brew" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bribe" :VERB "bribe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "bribery" :VERB "bribe" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bridge" :VERB "bridge" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "brief" :VERB "brief" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "briefing" :VERB "brief" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "brightener" :VERB "brighten" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "broadcast" :VERB "broadcast" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "broadcaster" :VERB "broadcast" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "broiler" :VERB "broil" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "broker" :VERB "broker" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "brokerage" :VERB "broker" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bruiser" :VERB "bruise" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "brush" :VERB "brush" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "budget" :VERB "budget" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "buffer" :VERB "buff" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bugger" :VERB "bug" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "buggery" :VERB "bugger" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "build-up" :VERB "build" :NOM-TYPE ((VERB-PART :ADVAL ("up")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "builder" :VERB "build" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "buildup" :VERB "build" :NOM-TYPE ((VERB-PART :ADVAL ("up")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "bulldozer" :VERB "bulldoze" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bumper" :VERB "bump" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bungler" :VERB "bungle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "bunt" :VERB "bunt" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "burden" :VERB "burden" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "burglary" :VERB "burgle" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "burial" :VERB "bury" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "burn" :VERB "burn" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "burner" :VERB "burn" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "burst" :VERB "burst" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "by" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "buster" :VERB "bust" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bustle" :VERB "bustle" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "butchery" :VERB "butcher" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "buy" :VERB "buy" :NOM-TYPE ((VERB-NOM) (OBJECT)) :PREPS
	 ("from" "for" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "buy-back" :VERB "buy" :NOM-TYPE ((VERB-PART :ADVAL ("back")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "buy-out" :VERB "buy" :NOM-TYPE ((VERB-PART :ADVAL ("out"))) :PREPS
	 ("of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "buyer" :VERB "buy" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "buzzer" :VERB "buzz" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "bystander" :VERB "stand" :NOM-TYPE ((SUBJECT-PART :ADVAL ("by")))
	 :PREPS ("by") :SOURCE NOMLEX)
	(NOM :NOUN "cackler" :VERB "cackle" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "cadger" :VERB "cadge" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "cage" :VERB "cage" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "cajolery" :VERB "cajole" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "calcination" :VERB "calcine" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "calculation" :VERB "calculate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "calculator" :VERB "calculate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "calibration" :VERB "calibrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "call" :VERB "call" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "caller" :VERB "call" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "campaign" :VERB "campaign" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "campaigner" :VERB "campaign" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "camper" :VERB "camp" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "canalization" :VERB "canalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cancellation" :VERB "cancel" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cannibalism" :VERB "cannibalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "canoeist" :VERB "canoe" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "canonization" :VERB "canonize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cant" :VERB "cant" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "canter" :VERB "cant" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "cantor" :VERB "cant" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "capitalist" :VERB "capitalize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "capitalization" :VERB "capitalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "capitulation" :VERB "capitulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "captain" :VERB "captain" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "caption" :VERB "caption" :NOM-TYPE ((P-OBJ :PVAL ("with"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "captive" :VERB "capture" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "captor" :VERB "capture" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "capture" :VERB "capture" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "carbonization" :VERB "carbonize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "care" :VERB "care" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "caricature" :VERB "caricature" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "caricaturist" :VERB "caricature" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "caroller" :VERB "carol" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "carousal" :VERB "carouse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "carrier" :VERB "carry" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "carry-forward" :VERB "carry" :NOM-TYPE
	 ((VERB-PART :ADVAL ("forward"))) :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "carryover" :VERB "carry" :NOM-TYPE ((VERB-PART :ADVAL ("over")))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "carter" :VERB "cart" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "cartoonist" :VERB "cartoon" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "carver" :VERB "carve" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "cast" :VERB "cast" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "caster" :VERB "cast" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "castigation" :VERB "castigate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "casting" :VERB "cast" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "castor" :VERB "cast" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "castration" :VERB "castrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "catalysis" :VERB "catalyze" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "catalyst" :VERB "catalyze" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "catch" :VERB "catch" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "catcher" :VERB "catch" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "catechism" :VERB "catechize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "caterer" :VERB "cater" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "causation" :VERB "cause" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cause" :VERB "cause" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "for")
	 :SOURCE NOMLEX)
	(NOM :NOUN "caution" :VERB "caution" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("on" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cave-in" :VERB "cave" :NOM-TYPE ((VERB-PART :ADVAL ("in"))) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "celebrant" :VERB "celebrate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "celebration" :VERB "celebrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "censer" :VERB "censure" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "censor" :VERB "censure" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "censorship" :VERB "censor" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "census" :VERB "census" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "centralization" :VERB "centralize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "certification" :VERB "certify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cessation" :VERB "cease" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "cession" :VERB "cede" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "chagrin" :VERB "chagrin" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "chair" :VERB "chair" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "challenge" :VERB "challenge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("with" "on" "of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "challenger" :VERB "challenge" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "champ" :VERB "champion" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "champion" :VERB "champion" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "championship" :VERB "champion" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "chance" :VERB "chance" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "change" :VERB "change" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("through" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "changeover" :VERB "change" :NOM-TYPE ((VERB-PART :ADVAL ("over")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "chant" :VERB "chant" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "characterization" :VERB "characterize" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "charge" :VERB "charge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "charge-off" :VERB "charge" :NOM-TYPE ((VERB-PART :ADVAL ("off")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "charger" :VERB "charge" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "charm" :VERB "charm" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "charmer" :VERB "charm" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "chart" :VERB "chart" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "charter" :VERB "chart" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "chase" :VERB "chase" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "chaser" :VERB "chase" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "chastisement" :VERB "chastise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "chat" :VERB "chat" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cheater" :VERB "cheat" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "check" :VERB "check" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "checker" :VERB "check" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "cheer" :VERB "cheer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("on" "by" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "chiseler" :VERB "chisel" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "chiseller" :VERB "chisel" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "chlorination" :VERB "chlorinate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "choice" :VERB "choose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "choker" :VERB "choke" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "chopper" :VERB "chop" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "chronicler" :VERB "chronicle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "circulation" :VERB "circulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "circumcision" :VERB "circumcise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "circumnavigation" :VERB "circumnavigate" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "circumscription" :VERB "circumscribe" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "circumvention" :VERB "circumvent" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "citation" :VERB "cite" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "civilization" :VERB "civilize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "claim" :VERB "claim" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "claimant" :VERB "claim" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "clamor" :VERB "clamor" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "clamp" :VERB "clamp" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "clampdown" :VERB "clamp" :NOM-TYPE ((VERB-PART :ADVAL ("down")))
	 :PREPS ("by") :SOURCE NOMLEX)
	(NOM :NOUN "clanger" :VERB "clang" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "clangor" :VERB "clang" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "clapper" :VERB "clap" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "clarification" :VERB "clarify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "clash" :VERB "clash" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "among" "by" "between") :SOURCE NOMLEX)
	(NOM :NOUN "classification" :VERB "classify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cleaner" :VERB "clean" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "cleanser" :VERB "cleanse" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "cleanup" :VERB "clean" :NOM-TYPE ((VERB-PART :ADVAL ("up")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "clearance" :VERB "clear" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "cleaver" :VERB "cleave" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "clerk" :VERB "clerk" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "climax" :VERB "climax" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "climb" :VERB "climb" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "climber" :VERB "climb" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "clincher" :VERB "clinch" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "clinker" :VERB "clink" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "clipper" :VERB "clip" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "clipping" :VERB "clip" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "cloak" :VERB "cloak" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "clone" :VERB "clone" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "close" :VERB "close" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "closure" :VERB "close" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "co-operation" :VERB "co-operate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "co-optation" :VERB "co-opt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "coach" :VERB "coach" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "coagulation" :VERB "coagulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "coalescence" :VERB "coalesce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "coat" :VERB "coat" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "coating" :VERB "coat" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "cobbler" :VERB "cobble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "codification" :VERB "codify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "coercion" :VERB "coerce" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "coexistence" :VERB "coexist" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cogeneration" :VERB "cogenerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cogitation" :VERB "cogitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cohabitation" :VERB "cohabit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "coherence" :VERB "cohere" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cohesion" :VERB "cohere" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "coil" :VERB "coil" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "coinage" :VERB "coin" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "coincidence" :VERB "coincide" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "between" "among" "amongst" "by" "about") :SOURCE NOMLEX)
	(NOM :NOUN "coiner" :VERB "coin" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "collaboration" :VERB "collaborate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "collaborator" :VERB "collaborate" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "collapse" :VERB "collapse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("on" "by" "of" "in") :SOURCE NOMLEX)
	(NOM :NOUN "collar" :VERB "collar" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "collation" :VERB "collate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "collection" :VERB "collect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "collectivization" :VERB "collectivize" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "collector" :VERB "collect" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "collision" :VERB "collide" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "collocation" :VERB "collocate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "collusion" :VERB "collude" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "among" "amongst" "of") :SOURCE NOMLEX)
	(NOM :NOUN "colonialist" :VERB "colonize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "colonist" :VERB "colonize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "colonization" :VERB "colonize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "colonizer" :VERB "colonize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "colorization" :VERB "color" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "combat" :VERB "combat" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "combatant" :VERB "combat" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "combination" :VERB "combine" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "combustion" :VERB "combust" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "comeback" :VERB "come" :NOM-TYPE ((VERB-PART :ADVAL ("back")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "comer" :VERB "come" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "comfort" :VERB "comfort" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "comforter" :VERB "comfort" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "command" :VERB "command" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "from" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "commander" :VERB "command" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "commandment" :VERB "command" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "commemoration" :VERB "commemorate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "commencement" :VERB "commence" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "commendation" :VERB "commend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "comment" :VERB "comment" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "commentary" :VERB "comment" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "commentator" :VERB "commentate" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "commercialization" :VERB "commercialize" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "commiseration" :VERB "commiserate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "commission" :VERB "commission" :NOM-TYPE ((OBJECT) (COMP)) :PREPS
	 ("by") :SOURCE NOMLEX)
	(NOM :NOUN "commission" :VERB "commit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "commissioner" :VERB "commission" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "commital" :VERB "commit" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "commitment" :VERB "commit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "communicant" :VERB "communicate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "communication" :VERB "communicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "among" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "communion" :VERB "commune" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "communique" :VERB "communication" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "commutation" :VERB "commute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "commute" :VERB "commute" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "commuter" :VERB "commute" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "comparison" :VERB "compare" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "compensation" :VERB "compensate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "competition" :VERB "compete" :NOM-TYPE ((VERB-NOM) (SUBJECT))
	 :PREPS ("of" "for" "in" "among" "between" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "competitiveness" :VERB "compete" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "competitor" :VERB "compete" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "compilation" :VERB "compile" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "compiler" :VERB "compile" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "complainant" :VERB "complain" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "complaint" :VERB "complain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "complaisance" :VERB "comply" :NOM-TYPE ((VERB-NOM)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "complement" :VERB "complement" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "complementation" :VERB "complement" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "completion" :VERB "complete" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "compliance" :VERB "comply" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "complication" :VERB "complicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "compliment" :VERB "compliment" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "comportment" :VERB "comport" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "composer" :VERB "compose" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "composition" :VERB "compose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "composure" :VERB "compose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "comprehension" :VERB "comprehend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "compression" :VERB "compress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "compressor" :VERB "compress" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "compromise" :VERB "compromise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "between" "by") :SOURCE NOMLEX)
	(NOM :NOUN "computation" :VERB "compute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "computer" :VERB "compute" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "con" :VERB "con" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "concealment" :VERB "conceal" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "conceiver" :VERB "conceive" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "concentrate" :VERB "concentrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "concentration" :VERB "concentrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "concept" :VERB "conceive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conception" :VERB "conceive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conceptualization" :VERB "conceptualize" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "concern" :VERB "concern" :NOM-TYPE ((VERB-NOM) (SUBJECT)) :PREPS
	 ("about" "over" "by" "with" "for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "concession" :VERB "concede" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conciliation" :VERB "conciliate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conclusion" :VERB "conclude" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "concoction" :VERB "concoct" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "concurrence" :VERB "concur" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "concussion" :VERB "concuss" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "condemnation" :VERB "condemn" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "condensation" :VERB "condense" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "condenser" :VERB "condense" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "condescension" :VERB "condescend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "conditioner" :VERB "condition" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("for") :SOURCE NOMLEX)
	(NOM :NOUN "condonation" :VERB "condone" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conduct" :VERB "conduct" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "conduction" :VERB "conduct" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conductor" :VERB "conduct" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "confabulation" :VERB "confabulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 NIL :SOURCE NOMLEX)
	(NOM :NOUN "confederation" :VERB "confederate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "conferee" :VERB "confer" :NOM-TYPE ((OBJECT) (SUBJECT)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conference" :VERB "confer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "conferment" :VERB "confer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "confession" :VERB "confess" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "confessor" :VERB "confess" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "confidant" :VERB "confide" :NOM-TYPE ((P-OBJ :PVAL ("in" "to")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "confidence" :VERB "confide" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "confidence-crusher" :VERB "crush" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "configuration" :VERB "configure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "confine" :VERB "confine" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "confinement" :VERB "confine" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "confirmation" :VERB "confirm" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "confiscation" :VERB "confiscate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conflation" :VERB "conflate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conflict" :VERB "conflict" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "conformation" :VERB "conform" :NOM-TYPE ((VERB-NOM)) :PREPS ("by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "conformist" :VERB "conform" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "confrontation" :VERB "confront" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "with" "by") :SOURCE NOMLEX)
	(NOM :NOUN "confusion" :VERB "confuse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "confutation" :VERB "confute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conglomeration" :VERB "conglomerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "congratulation" :VERB "congratulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "congregation" :VERB "congregate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "conjugation" :VERB "conjugate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conjunction" :VERB "conjoin" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conjuration" :VERB "conjure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "conjurer" :VERB "conjure" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "conjuror" :VERB "conjure" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "connection" :VERB "connect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "connector" :VERB "connect" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "connivance" :VERB "connive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "connotation" :VERB "connote" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conqueror" :VERB "conquer" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "conquest" :VERB "conquer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conscription" :VERB "conscript" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "consecration" :VERB "consecrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "consent" :VERB "consent" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "conservancy" :VERB "conserve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conservation" :VERB "conserve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "consideration" :VERB "consider" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "consignee" :VERB "consign" :NOM-TYPE ((IND-OBJ :PVAL ("to")))
	 :PREPS ("of" "for") :SOURCE NOMLEX)
	(NOM :NOUN "consigner" :VERB "consign" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "consignment" :VERB "consign" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "consignor" :VERB "consign" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "consistence" :VERB "consist" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "consolation" :VERB "console" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of" "from" "to") :SOURCE NOMLEX)
	(NOM :NOUN "consolidation" :VERB "consolidate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "conspiracy" :VERB "conspire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "between" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "conspirator" :VERB "conspire" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "constipation" :VERB "constipate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "constitution" :VERB "constitute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "constraint" :VERB "constrain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "constriction" :VERB "constrict" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "construction" :VERB "construct" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "constructor" :VERB "construct" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "consultant" :VERB "consult" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "consultation" :VERB "consult" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "to" "of" "between" "by") :SOURCE NOMLEX)
	(NOM :NOUN "consumer" :VERB "consume" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "consummation" :VERB "consummate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "consumption" :VERB "consume" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "contact" :VERB "contact" :NOM-TYPE ((VERB-NOM) (OBJECT)) :PREPS
	 ("between" "by" "of" "with") :SOURCE NOMLEX)
	(NOM :NOUN "container" :VERB "contain" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "containment" :VERB "contain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "contaminant" :VERB "contaminate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "contamination" :VERB "contaminate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("with" "of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "contemplation" :VERB "contemplate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "contender" :VERB "contend" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "content" :VERB "contain" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "contention" :VERB "contend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "contentment" :VERB "content" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "contest" :VERB "contest" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "contestant" :VERB "contest" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "continuance" :VERB "continue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "continuation" :VERB "continue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "contortion" :VERB "contort" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "contract" :VERB "contract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "with" "from" "between" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "contraction" :VERB "contract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "contractor" :VERB "contract" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "contradiction" :VERB "contradict" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("with" "of" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "contrast" :VERB "contrast" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "contravention" :VERB "contravene" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "contribution" :VERB "contribute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "contributor" :VERB "contribute" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "contrivance" :VERB "contrive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "contriver" :VERB "contrive" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "control" :VERB "control" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "controller" :VERB "control" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "contusion" :VERB "contuse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "convalescence" :VERB "convalesce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "convalescent" :VERB "convalesce" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "convener" :VERB "convene" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "convention" :VERB "convene" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "convergence" :VERB "converge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "conversation" :VERB "converse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "of" "between" "amongst") :SOURCE NOMLEX)
	(NOM :NOUN "conversion" :VERB "convert" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "convert" :VERB "convert" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "converter" :VERB "convert" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "conveyance" :VERB "convey" :NOM-TYPE ((VERB-NOM) (INSTRUMENT))
	 :PREPS ("of") :SOURCE NOMLEX)
	(NOM :NOUN "conveyer" :VERB "convey" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "conveyor" :VERB "convey" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "conviction" :VERB "convict" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "convocation" :VERB "convoke" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "convulsion" :VERB "convulse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cook" :VERB "cook" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "cooker" :VERB "cook" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "cooking" :VERB "cook" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "coolant" :VERB "cool" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "cooler" :VERB "cool" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "cooperation" :VERB "cooperate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "among" "by" "of" "from" "around") :SOURCE NOMLEX)
	(NOM :NOUN "cooperative" :VERB "cooperate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cooperator" :VERB "cooperate" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "coordination" :VERB "coordinate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "coordinator" :VERB "coordinate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "copier" :VERB "copy" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "copulation" :VERB "copulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "copy" :VERB "copy" :NOM-TYPE ((P-OBJ :PVAL ("to")) (OBJECT))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "copyist" :VERB "copy" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "corker" :VERB "cork" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "correction" :VERB "correct" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "correlation" :VERB "correlate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "correspondence" :VERB "correspond" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "among" "by" "between") :SOURCE NOMLEX)
	(NOM :NOUN "correspondent" :VERB "correspond" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "corroboration" :VERB "corroborate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "corrosion" :VERB "corrode" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "corrugation" :VERB "corrugate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "corruption" :VERB "corrupt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cost" :VERB "cost" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("with" "on" "from" "to" "for" "in" "of") :SOURCE NOMLEX)
	(NOM :NOUN "counseling" :VERB "counsel" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "counsellor" :VERB "counsel" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "counselor" :VERB "counsel" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "count" :VERB "count" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "countdown" :VERB "count" :NOM-TYPE ((VERB-PART :ADVAL ("down")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "countenance" :VERB "look" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "counter" :VERB "count" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "counteraction" :VERB "counteract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "counterfeiter" :VERB "counterfeit" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "countertrader" :VERB "countertrade" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "courser" :VERB "course" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "courtship" :VERB "court" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "among" "amongst" "by" "between") :SOURCE NOMLEX)
	(NOM :NOUN "cover" :VERB "cover" :NOM-TYPE ((INSTRUMENT) (VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "coverage" :VERB "cover" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "crack" :VERB "crack" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "crackdown" :VERB "crack" :NOM-TYPE ((VERB-PART :ADVAL ("down")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cracker" :VERB "crack" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "crammer" :VERB "cram" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "crash" :VERB "crash" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "crashlet" :VERB "crash" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "craving" :VERB "crave" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "crawler" :VERB "crawl" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "creation" :VERB "create" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "creator" :VERB "create" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "credential" :VERB "credit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "credit" :VERB "credit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "with" "by") :SOURCE NOMLEX)
	(NOM :NOUN "creditor" :VERB "credit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "creeper" :VERB "creep" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "cremation" :VERB "cremate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "crier" :VERB "cry" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "critic" :VERB "criticize" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "criticism" :VERB "criticize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "among" "on" "of" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "crooner" :VERB "croon" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "cropper" :VERB "crop" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "cross" :VERB "cross" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "between" "by" "amongst" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cross-examination" :VERB "cross-examine" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cross-examiner" :VERB "cross-examine" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "cross-fertilization" :VERB "cross-fertilize" :NOM-TYPE
	 ((VERB-NOM)) :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cruise" :VERB "cruise" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "cruiser" :VERB "cruise" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "crusade" :VERB "crusade" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "crusader" :VERB "crusade" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "cry" :VERB "cry" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "by" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "crystallization" :VERB "crystallize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cue" :VERB "cue" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "culmination" :VERB "culminate" :NOM-TYPE
	 ((P-OBJ :PVAL ("with" "in"))) :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cultivation" :VERB "cultivate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cultivator" :VERB "cultivate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "curb" :VERB "curb" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "cure" :VERB "cure" :NOM-TYPE ((INSTRUMENT)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "curler" :VERB "curl" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "curse" :VERB "curse" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "curtailment" :VERB "curtail" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cushion" :VERB "cushion" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "cut" :VERB "cut" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "under" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cutback" :VERB "cut" :NOM-TYPE ((VERB-PART :ADVAL ("back")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "cutoff" :VERB "cut" :NOM-TYPE ((VERB-PART :ADVAL ("off"))) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cutout" :VERB "cut" :NOM-TYPE ((OBJECT-PART :ADVAL ("out")))
	 :PREPS ("by" "from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "cutter" :VERB "cut" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "cutting" :VERB "cut" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dabbler" :VERB "dabble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dalliance" :VERB "dally" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dam" :VERB "dam" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "damage" :VERB "damage" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "due to" "to" "of" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "damnation" :VERB "damn" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "damper" :VERB "dampen" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "dance" :VERB "dance" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dancer" :VERB "dance" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "dancing" :VERB "dance" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dash" :VERB "dash" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dauber" :VERB "daub" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "dawdler" :VERB "dawdle" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "de-emphasis" :VERB "de-emphasize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "de-escalation" :VERB "de-escalate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "deal" :VERB "deal" :NOM-TYPE ((VERB-NOM)) :PREPS ("to" "of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dealer" :VERB "deal" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "dealership" :VERB "deal" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dealing" :VERB "deal" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "death" :VERB "die" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "for" "by" "of" "from" "against") :SOURCE NOMLEX)
	(NOM :NOUN "debarkation" :VERB "debark" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "debasement" :VERB "debase" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "debate" :VERB "debate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "between" "of" "on" "in connection with" "with regard to" "concerning"
	  "about" "regarding" "with respect to" "in regard to" "over" "by" "among"
	  "amongst" "within")
	 :SOURCE NOMLEX)
	(NOM :NOUN "debater" :VERB "debate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "debauchee" :VERB "debauch" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "debauchery" :VERB "debauch" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "debut" :VERB "debut" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "decanter" :VERB "decant" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "decapitation" :VERB "decapitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "decay" :VERB "decay" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "decease" :VERB "decease" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "deceiver" :VERB "deceive" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "decentralization" :VERB "decentralize" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deception" :VERB "deceive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "decimalization" :VERB "decimalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "decision" :VERB "decide" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "by" "to" "for") :SOURCE NOMLEX)
	(NOM :NOUN "declaration" :VERB "declare" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "declassification" :VERB "declassify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "declination" :VERB "decline" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "decline" :VERB "decline" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("on" "in" "by" "of" "from" "for") :SOURCE NOMLEX)
	(NOM :NOUN "decliner" :VERB "decline" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "decoder" :VERB "decode" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "decolonization" :VERB "decolonize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "decomposition" :VERB "decompose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "decompression" :VERB "decompress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "decontamination" :VERB "decontaminate" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "decoration" :VERB "decorate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "decorator" :VERB "decorate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "decrease" :VERB "decrease" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "decree" :VERB "decree" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dedication" :VERB "dedicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deduction1" :VERB "deduct" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deduction2" :VERB "deduce" :NOM-TYPE ((VERB-NOM)) :PREPS ("by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "deed" :VERB "do" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "defacement" :VERB "deface" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "defamation" :VERB "defame" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "default" :VERB "default" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "defaulter" :VERB "default" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "defeat" :VERB "defeat" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "defecation" :VERB "defecate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "defection" :VERB "defect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "defector" :VERB "defect" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "defendant" :VERB "defend" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "defender" :VERB "defend" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "defense" :VERB "defend" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "deference" :VERB "defer" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "deferment" :VERB "defer" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "deferral" :VERB "defer" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "defiance" :VERB "defy" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "defilement" :VERB "defile" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "definition" :VERB "define" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deflation" :VERB "deflate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "deflator" :VERB "deflate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "deflection" :VERB "deflect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "defoliant" :VERB "defoliate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "defoliation" :VERB "defoliate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "defrayal" :VERB "defray" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "defrayment" :VERB "defray" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "defroster" :VERB "defrost" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "degeneration" :VERB "degenerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "degradation" :VERB "degrade" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deification" :VERB "deify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "delay" :VERB "delay" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "of" "on") :SOURCE NOMLEX)
	(NOM :NOUN "delegation" :VERB "delegate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deletion" :VERB "delete" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "deliberation" :VERB "deliberate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "delight" :VERB "delight" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "in") :SOURCE NOMLEX)
	(NOM :NOUN "delimitation" :VERB "delimit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "delineation" :VERB "delineate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deliverance" :VERB "deliver" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "deliverer" :VERB "deliver" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "delivery" :VERB "deliver" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "delusion" :VERB "delude" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "demagnetization" :VERB "demagnetize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "demand" :VERB "demand" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "by" "for" "from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "demarcation" :VERB "demarcate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "demeanor" :VERB "demean" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "demo" :VERB "demonstrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "demobilization" :VERB "demobilize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "democratization" :VERB "democratize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "demolition" :VERB "demolish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "demonetization" :VERB "demonetize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "demonstration" :VERB "demonstrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "demonstrator" :VERB "demonstrate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "demoralization" :VERB "demoralize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "demotion" :VERB "demote" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "denationalization" :VERB "denationalize" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "denial" :VERB "deny" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "denier" :VERB "deny" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of" "to" "for") :SOURCE NOMLEX)
	(NOM :NOUN "denigration" :VERB "denigrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "denomination" :VERB "denominate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "denominator" :VERB "denominate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "dent" :VERB "dent" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "denudation" :VERB "denude" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "denunciation" :VERB "denounce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "against" "by") :SOURCE NOMLEX)
	(NOM :NOUN "departure" :VERB "depart" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "for") :SOURCE NOMLEX)
	(NOM :NOUN "dependant" :VERB "depend" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "dependence" :VERB "depend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dependency" :VERB "depend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dependent" :VERB "depend" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "depiction" :VERB "depict" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "depletion" :VERB "deplete" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deployment" :VERB "deploy" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "depopulation" :VERB "depopulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "deportation" :VERB "deport" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deportee" :VERB "deport" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "deportment" :VERB "deport" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deposit" :VERB "deposit" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "deposition" :VERB "depose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "deposition2" :VERB "deposit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "depositor" :VERB "deposit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "deprecation" :VERB "deprecate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "depreciation" :VERB "depreciate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "depressant" :VERB "depress" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "depression" :VERB "depress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "regarding" "with respect to" "in regard to" "over" "on"
	  "in connection with" "with regard to" "concerning" "about" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "deprivation" :VERB "deprive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deputation" :VERB "depute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "derailment" :VERB "derail" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "derangement" :VERB "derange" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deregulation" :VERB "deregulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deregulaton" :VERB "deregulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "derision" :VERB "deride" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "derivation" :VERB "derive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "derivative" :VERB "derive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "derogation" :VERB "derogate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "desalination" :VERB "desalinate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "desalinization" :VERB "desalinize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "descendant" :VERB "descend" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "descent" :VERB "descend" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "description" :VERB "describe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "desecration" :VERB "desecrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "desegregation" :VERB "desegregate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "desensitization" :VERB "desensitize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deserter" :VERB "desert" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "desertion" :VERB "desert" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "desiccant" :VERB "desiccate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "design" :VERB "design" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "designation" :VERB "designate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "designee" :VERB "designate" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "designer" :VERB "design" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "desire" :VERB "desire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "desolation" :VERB "desolate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "desperation" :VERB "despair" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "destination" :VERB "destine" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "destiny" :VERB "destine" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "destroyer" :VERB "destroy" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "destruction" :VERB "destroy" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "detachment" :VERB "detach" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "detainee" :VERB "detain" :NOM-TYPE ((OBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "detection" :VERB "detect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "detective" :VERB "detect" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "detector" :VERB "detect" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "detention" :VERB "detain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "deterioration" :VERB "deteriorate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "determinant" :VERB "determine" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "determination" :VERB "determine" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "determiner" :VERB "determine" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "deterrence" :VERB "deter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "deterrent" :VERB "deter" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "detestation" :VERB "detest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dethronement" :VERB "dethrone" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "detonation" :VERB "detonate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "detonator" :VERB "detonate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "detoxification" :VERB "detoxify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "detraction" :VERB "detract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "detractor" :VERB "detract" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "detribalization" :VERB "detribalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "devaluation" :VERB "devaluate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "devastation" :VERB "devastate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "developer" :VERB "develop" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "development" :VERB "develop" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "in" "by" "from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "deviant" :VERB "deviate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "deviation" :VERB "deviate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "for") :SOURCE NOMLEX)
	(NOM :NOUN "devisee" :VERB "devise" :NOM-TYPE ((OBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "devitalization" :VERB "devitalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "devotee" :VERB "devote" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "devotion" :VERB "devote" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "diagnosis" :VERB "diagnose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dictation" :VERB "dictate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dictator" :VERB "dictate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "difference" :VERB "differ" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "among" "by" "between") :SOURCE NOMLEX)
	(NOM :NOUN "differential" :VERB "differ" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "among" "by" "between") :SOURCE NOMLEX)
	(NOM :NOUN "differentiation" :VERB "differentiate" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "diffraction" :VERB "diffract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "diffusion" :VERB "diffuse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dig" :VERB "dig" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "digestion" :VERB "digest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "digger" :VERB "dig" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "digression" :VERB "digress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dilation" :VERB "dilate" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dilution" :VERB "dilute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "diminution" :VERB "diminish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "diner" :VERB "dine" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "dip" :VERB "dip" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "dipper" :VERB "dip" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "direction" :VERB "direct" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "directive" :VERB "direct" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "director" :VERB "direct" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "disablement" :VERB "disable" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disagreement" :VERB "disagree" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "by" "between" "amongst") :SOURCE NOMLEX)
	(NOM :NOUN "disappearance" :VERB "disappear" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "disappointment" :VERB "disappoint" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "over" "by" "with" "in" "to") :SOURCE NOMLEX)
	(NOM :NOUN "disapprobation" :VERB "disapprove" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disapproval" :VERB "disapprove" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "disarmament" :VERB "disarm" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disarrangement" :VERB "disarrange" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disavowal" :VERB "disavow" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disbandment" :VERB "disband" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disbelief" :VERB "disbelieve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "disbursement" :VERB "disburse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "discernment" :VERB "discern" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "discharge" :VERB "discharge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disclaimer" :VERB "disclaim" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "disclosure" :VERB "disclose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "discoloration" :VERB "discolor" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "discolouration" :VERB "discolour" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "discombobulation" :VERB "discombobulate" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "discomfiture" :VERB "discomfit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "discomfort" :VERB "discomfit" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "discomposure" :VERB "discompose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "discontent" :VERB "discontent" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "discontinuance" :VERB "discontinue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "discontinuation" :VERB "discontinue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "discount" :VERB "discount" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "discounter" :VERB "discount" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "discouragement" :VERB "discourage" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "by" "over" "of") :SOURCE NOMLEX)
	(NOM :NOUN "discourse" :VERB "discourse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "discoverer" :VERB "discover" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "discovery" :VERB "discover" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "discrimination" :VERB "discriminate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "discussion" :VERB "discuss" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("with" "of" "amongst" "among" "between" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disembarkation" :VERB "disembark" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "disembarrassment" :VERB "disembarrass" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disenchantment" :VERB "disenchant" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "disengagement" :VERB "disengage" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "disentanglement" :VERB "disentangle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disestablishment" :VERB "disestablish" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disfavor" :VERB "disfavor" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disfigurement" :VERB "disfigure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disfranchisement" :VERB "disfranchise" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disgorgement" :VERB "disgorge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "disgrace" :VERB "disgrace" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disgust" :VERB "disgust" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("with" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "disillusion" :VERB "disillusion" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "disillusionment" :VERB "disillusion" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("with" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disinclination" :VERB "disincline" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disinfectant" :VERB "disinfect" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "disinfection" :VERB "disinfect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disinfestation" :VERB "disinfest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disinheritance" :VERB "disinherit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "disintegration" :VERB "disintegrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "disinterment" :VERB "disinter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dislike" :VERB "dislike" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dislocation" :VERB "dislocate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "dislodgement" :VERB "dislodge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dismantlement" :VERB "dismantle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dismay" :VERB "dismay" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dismemberment" :VERB "dismember" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dismissal" :VERB "dismiss" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disobedience" :VERB "disobey" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "disorder" :VERB "disorder" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disorganization" :VERB "disorganize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disparagement" :VERB "disparage" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dispatch" :VERB "dispatch" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dispatcher" :VERB "dispatch" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dispensary" :VERB "dispense" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dispensation" :VERB "dispense" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dispenser" :VERB "dispense" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dispersal" :VERB "disperse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dispersion" :VERB "disperse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "displacement" :VERB "displace" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "display" :VERB "display" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "displeasure" :VERB "displease" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "disposal" :VERB "dispose" :NOM-TYPE ((VERB-NOM)) :PREPS ("by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "disposition" :VERB "dispose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dispossession" :VERB "dispossess" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disputant" :VERB "dispute" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "disputation" :VERB "dispute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dispute" :VERB "dispute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "by" "over" "in regard to" "with respect to" "regarding" "of"
	  "about" "concerning" "with regard to" "in connection with" "on" "between"
	  "within")
	 :SOURCE NOMLEX)
	(NOM :NOUN "disqualification" :VERB "disqualify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "disregard" :VERB "disregard" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disruption" :VERB "disrupt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dissatisfaction" :VERB "dissatisfy" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "with" "by" "in" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dissection" :VERB "dissect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dissembler" :VERB "dissemble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dissemination" :VERB "disseminate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dissension" :VERB "dissent" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dissent" :VERB "dissent" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dissenter" :VERB "dissent" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "dissimulation" :VERB "dissimulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dissipation" :VERB "dissipate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dissociation" :VERB "dissociate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dissolution" :VERB "dissolve" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dissuasion" :VERB "dissuade" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "distension" :VERB "distend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "distillation" :VERB "distill" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "distiller" :VERB "distill" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "distinction" :VERB "distinguish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "distortion" :VERB "distort" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "distraction" :VERB "distract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "distress" :VERB "distress" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "distributer" :VERB "distribute" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "distribution" :VERB "distribute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "distributor" :VERB "distribute" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "distributorship" :VERB "distribute" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "distrust" :VERB "distrust" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "disturbance" :VERB "disturb" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "divagation" :VERB "divagate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dive" :VERB "dive" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by" "in")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dive-bomber" :VERB "dive-bomb" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "diver" :VERB "dive" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "divergence" :VERB "diverge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "by" "amongst" "among" "of" "between") :SOURCE NOMLEX)
	(NOM :NOUN "diversification" :VERB "diversify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "diversion" :VERB "divert" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "divestiture" :VERB "divest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "divestment" :VERB "divest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "divide" :VERB "divide" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "divination" :VERB "divine" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "diviner" :VERB "divine" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "division" :VERB "divide" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "among" "between" "by") :SOURCE NOMLEX)
	(NOM :NOUN "divisor" :VERB "divide" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "divorcee" :VERB "divorce" :NOM-TYPE ((OBJECT)) :PREPS
	 ("by" "from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "divulgence" :VERB "divulge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "do-gooder" :VERB "do" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "for")
	 :SOURCE NOMLEX)
	(NOM :NOUN "docker" :VERB "dock" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "documentation" :VERB "document" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dodderer" :VERB "dodder" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "dodger" :VERB "dodge" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "doer" :VERB "do" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "for") :SOURCE
	 NOMLEX)
	(NOM :NOUN "domestication" :VERB "domesticate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dominance" :VERB "dominate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "over" "of") :SOURCE NOMLEX)
	(NOM :NOUN "domination" :VERB "dominate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dominion" :VERB "dominate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "donation" :VERB "donate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("out of" "through" "by" "from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "donor" :VERB "donate" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "doubt" :VERB "doubt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("about" "concerning" "with regard to" "in connection with" "on" "over"
	  "in regard to" "with respect to" "regarding" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dower" :VERB "dower" :NOM-TYPE ((P-OBJ :PVAL ("with"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "downfall" :VERB "fall" :NOM-TYPE ((VERB-PART :ADVAL ("down")))
	 :PREPS ("by") :SOURCE NOMLEX)
	(NOM :NOUN "downgrade" :VERB "downgrade" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "downing" :VERB "down" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "downsizing" :VERB "downsize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dowry" :VERB "dower" :NOM-TYPE ((P-OBJ :PVAL ("with"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dowser" :VERB "dowse" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "draftee1" :VERB "draft" :NOM-TYPE ((OBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "draftee2" :VERB "draft" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "drag" :VERB "drag" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "drain" :VERB "drain" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "drainage" :VERB "drain" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dramatist" :VERB "dramatize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dramatization" :VERB "dramatize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "draper" :VERB "drape" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "dread" :VERB "dread" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dream" :VERB "dream" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dreamer" :VERB "dream" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "dredger" :VERB "dredge" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "drenching" :VERB "drench" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "dresser" :VERB "dress" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "dribbler" :VERB "dribble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "drift" :VERB "drift" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "drifter" :VERB "drift" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "drink" :VERB "drink" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "drinker" :VERB "drink" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "drinking" :VERB "drink" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "drive" :VERB "drive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "driveller" :VERB "drivel" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "driver" :VERB "drive" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "drop" :VERB "drop" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "of" "to" "by" "for" "in") :SOURCE NOMLEX)
	(NOM :NOUN "drop-off" :VERB "drop" :NOM-TYPE ((VERB-PART :ADVAL ("off")))
	 :PREPS ("by") :SOURCE NOMLEX)
	(NOM :NOUN "dropout" :VERB "drop" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dropper" :VERB "drop" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "drubbing" :VERB "drub" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "drudgery" :VERB "drudge" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "drummer" :VERB "drum" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "dry-cleaner" :VERB "dry-clean" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "dryer" :VERB "dry" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "duel" :VERB "duel" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "duelist" :VERB "duel" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "duellist" :VERB "duel" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "dumper" :VERB "dump" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "duplication" :VERB "duplicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "duplicator" :VERB "duplicate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "duster" :VERB "dust" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "dweller" :VERB "dwell" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "dwelling" :VERB "dwell" :NOM-TYPE
	 ((VERB-NOM) (P-OBJ :PVAL ("in"))) :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "dyer" :VERB "dye" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "earner" :VERB "earn" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "eater" :VERB "eat" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "eavesdropper" :VERB "eavesdrop" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "ebb" :VERB "ebb" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "echo" :VERB "echo" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "edification" :VERB "edify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "edition" :VERB "edit" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "editor" :VERB "edit" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "education" :VERB "educate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "educator" :VERB "educate" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "eduction" :VERB "educate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "effacement" :VERB "efface" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "effect" :VERB "affect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "on" "of") :SOURCE NOMLEX)
	(NOM :NOUN "effervescence" :VERB "effervesce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "effrontery" :VERB "affront" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "ejaculation" :VERB "ejaculate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "ejection" :VERB "eject" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ejector" :VERB "eject" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "elaboration" :VERB "elaborate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "elation" :VERB "elate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "election" :VERB "elect" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "elector" :VERB "elect" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "electorate" :VERB "elect" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "electrification" :VERB "electrify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "electrocution" :VERB "electrocute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "electrolysis" :VERB "electrify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "elevation" :VERB "elevate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "elevator" :VERB "elevate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "elicitation" :VERB "elicit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "elimination" :VERB "eliminate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "elision" :VERB "elide" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "elongation" :VERB "elongate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "elopement" :VERB "elope" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "elucidation" :VERB "elucidate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "emaciation" :VERB "emaciate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "emanation" :VERB "emanate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "emancipation" :VERB "emancipate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "emasculation" :VERB "emasculate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "embalmment" :VERB "embalm" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "embargo" :VERB "embargo" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "embarkation" :VERB "embark" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "embarrassment" :VERB "embarrass" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "with" "by" "over" "to") :SOURCE NOMLEX)
	(NOM :NOUN "embellishment" :VERB "embellish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "embezzlement" :VERB "embezzle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "embitterment" :VERB "embitter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "embodiment" :VERB "embody" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "embroidery" :VERB "embroider" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "emendation" :VERB "emend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "emergence" :VERB "emerge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "emigrant" :VERB "emigrate" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "emigration" :VERB "emigrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "emission" :VERB "emit" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "emphasis" :VERB "emphasize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "in") :SOURCE NOMLEX)
	(NOM :NOUN "employee" :VERB "employ" :NOM-TYPE ((OBJECT)) :PREPS
	 ("to" "at" "of" "with" "from" "in") :SOURCE NOMLEX)
	(NOM :NOUN "employer" :VERB "employ" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "employment" :VERB "employ" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "emulation" :VERB "emulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "enactment" :VERB "enact" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "encampment" :VERB "encamp" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "enchanter" :VERB "enchant" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "enchantment" :VERB "enchant" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "encirclement" :VERB "encircle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "enclosure" :VERB "enclose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "encounter" :VERB "encounter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "encouragement" :VERB "encourage" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "encroachment" :VERB "encroach" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "end" :VERB "end" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "endangerment" :VERB "endanger" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "endearment" :VERB "endear" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "endeavor" :VERB "endeavor" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "ending" :VERB "end" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "endorsement" :VERB "endorse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "endowment" :VERB "endow" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "endurance" :VERB "endure" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "enforcement" :VERB "enforce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "enforcer" :VERB "enforce" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "enfranchisement" :VERB "enfranchise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "engagement" :VERB "engage" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "engorgement" :VERB "engorge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "engraver" :VERB "engrave" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "enhancement" :VERB "enhance" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "enjoyment" :VERB "enjoy" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "enlargement" :VERB "enlarge" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "enlightenment" :VERB "enlighten" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "enlistment" :VERB "enlist" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "ennoblement" :VERB "ennoble" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "enquirer" :VERB "enquire" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "enquiry" :VERB "enquire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "enrichment" :VERB "enrich" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "enrollee" :VERB "enroll" :NOM-TYPE ((OBJECT)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "enrollment" :VERB "enroll" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "enrolment" :VERB "enroll" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "enslavement" :VERB "enslave" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "entanglement" :VERB "entangle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "entertainer" :VERB "entertain" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "entertainment" :VERB "entertain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "enthronement" :VERB "enthrone" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "enticement" :VERB "entice" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "entitlement" :VERB "entitle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "entrance" :VERB "enter" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "entrant" :VERB "enter" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "entreaty" :VERB "entreat" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "entrenchment" :VERB "entrench" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "entry" :VERB "enter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "by" "of" "into") :SOURCE NOMLEX)
	(NOM :NOUN "enumeration" :VERB "enumerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "enunciation" :VERB "enunciate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "envelopment" :VERB "envelope" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "envy" :VERB "envy" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "to" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "equal" :VERB "equal" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "equality" :VERB "equal" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "of" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "equalization" :VERB "equalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "equalizer" :VERB "equalize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "equation" :VERB "equate" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "equipment" :VERB "equip" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "eradication" :VERB "eradicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "eraser" :VERB "erase" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "erasure" :VERB "erase" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "erection" :VERB "erect" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "erosion" :VERB "erode" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "error" :VERB "err" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by" "on")
	 :SOURCE NOMLEX)
	(NOM :NOUN "eruption" :VERB "erupt" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "escalation" :VERB "escalate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "escalator" :VERB "escalate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "escape" :VERB "escape" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "escapee" :VERB "escape" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "escapement" :VERB "escape" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "escort" :VERB "escort" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "escrow" :VERB "escrow" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "espionage" :VERB "spy" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "espousal" :VERB "espouse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "establishment" :VERB "establish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "esteem" :VERB "esteem" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "estimate" :VERB "estimate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "estimation" :VERB "estimate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "estimator" :VERB "estimate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "estrangement" :VERB "estrange" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "etcher" :VERB "etch" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "eulogist" :VERB "eulogize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "evacuation" :VERB "evacuate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "evacuee" :VERB "evacuate" :NOM-TYPE ((OBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "evader" :VERB "evade" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "evaluation" :VERB "evaluate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "evaporation" :VERB "evaporate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "evasion" :VERB "evade" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "eviction" :VERB "evict" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "evidence" :VERB "evidence" :NOM-TYPE ((INSTRUMENT)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "evolution" :VERB "evolve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exacerbation" :VERB "exacerbate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exaction" :VERB "exact" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "exaggeration" :VERB "exaggerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exaltation" :VERB "exalt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "examination" :VERB "examine" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "examiner" :VERB "examine" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "example" :VERB "exemplify" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "exasperation" :VERB "exasperate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "with") :SOURCE NOMLEX)
	(NOM :NOUN "excavation" :VERB "excavate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "excavator" :VERB "excavate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "excellence" :VERB "excel" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "exception" :VERB "except" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "excess" :VERB "exceed" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "exchange" :VERB "exchange" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("with" "of" "by" "between") :SOURCE NOMLEX)
	(NOM :NOUN "exchanger" :VERB "exchange" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "excision" :VERB "excise" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "excitement" :VERB "excite" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "exclamation" :VERB "exclaim" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exclusion" :VERB "exclude" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "excogitation" :VERB "excogitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "excommunication" :VERB "excommunicate" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "excoriation" :VERB "excoriate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "excretion" :VERB "excrete" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "excuse" :VERB "excuse" :NOM-TYPE ((VERB-NOM)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "execration" :VERB "execrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "executant" :VERB "execute" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "execution" :VERB "execute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "executor" :VERB "execute" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "exemplification" :VERB "exemplify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "exemption" :VERB "exempt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exercise" :VERB "exercise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "exertion" :VERB "exert" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "exhalation" :VERB "exhale" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exhaustion" :VERB "exhaust" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exhibit" :VERB "exhibit" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "exhibition" :VERB "exhibit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exhibitor" :VERB "exhibit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "exhilaration" :VERB "exhilarate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exhortation" :VERB "exhort" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exhumation" :VERB "exhume" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exile" :VERB "exile" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "existence" :VERB "exist" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "exit" :VERB "exit" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by" "for")
	 :SOURCE NOMLEX)
	(NOM :NOUN "exoneration" :VERB "exonerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exorcism" :VERB "exorcise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "expansion" :VERB "expand" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "expansionism" :VERB "expand" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "expectancy" :VERB "expect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "expectation" :VERB "expect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "expedition" :VERB "expedite" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "expenditure" :VERB "spend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "expense" :VERB "expend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "at" "of" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "experience" :VERB "experience" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "experiment" :VERB "experiment" :NOM-TYPE ((VERB-NOM)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "experimentation" :VERB "experiment" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "experimenter" :VERB "experiment" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "expiation" :VERB "expiate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "expiration" :VERB "expire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "expiry" :VERB "expire" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "explanation" :VERB "explain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exploitation" :VERB "exploit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exploration" :VERB "explore" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "explorer" :VERB "explore" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "explosion" :VERB "explode" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "explusion" :VERB "expel" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "export" :VERB "export" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exportation" :VERB "export" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "exporter" :VERB "export" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "exposition" :VERB "expose" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "expostulation" :VERB "expostulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "exposure" :VERB "expose" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "expression" :VERB "express" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "expropriation" :VERB "expropriate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "expulsion" :VERB "expel" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "expurgation" :VERB "expurgate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "extension" :VERB "extend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "extent" :VERB "extend" :NOM-TYPE ((P-OBJ :PVAL ("to"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "extenuation" :VERB "extenuate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "extermination" :VERB "exterminate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "extinguisher" :VERB "extinguish" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "extirpation" :VERB "extirpate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "extortion" :VERB "extort" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "extraction" :VERB "extract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "extradition" :VERB "extradite" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "extrapolation" :VERB "extrapolate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "on" "of") :SOURCE NOMLEX)
	(NOM :NOUN "extrication" :VERB "extricate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "extrusion" :VERB "extrude" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "exultation" :VERB "exult" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "fabrication" :VERB "fabricate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "fabricator" :VERB "fabricate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "facer" :VERB "face" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "facing" :VERB "face" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "failure" :VERB "fail" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fall" :VERB "fall" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by" "in")
	 :SOURCE NOMLEX)
	(NOM :NOUN "falloff" :VERB "fall" :NOM-TYPE ((VERB-PART :ADVAL ("off")))
	 :PREPS ("in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "falsification" :VERB "falsify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "familiarization" :VERB "familiarize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "fancier" :VERB "fancy" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "farmer" :VERB "farm" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "fascination" :VERB "fascinate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "by" "in" "with" "of" "for") :SOURCE NOMLEX)
	(NOM :NOUN "fastener" :VERB "fasten" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fate" :VERB "fate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fault" :VERB "fault" :NOM-TYPE ((VERB-NOM)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "favorite" :VERB "favor" :NOM-TYPE ((OBJECT)) :PREPS
	 ("of" "by" "from" "with") :SOURCE NOMLEX)
	(NOM :NOUN "favoritism" :VERB "favor" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "fax" :VERB "fax" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "to" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fear" :VERB "fear" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "federation" :VERB "federate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "feeder" :VERB "feed" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "feel" :VERB "feel" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "feeler" :VERB "feel" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "feeling" :VERB "feel" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "of" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "felicitation" :VERB "felicitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "fencer" :VERB "fence" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "fender" :VERB "fend" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "fermentation" :VERB "ferment" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "fertilization" :VERB "fertilize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "fertilizer" :VERB "fertilize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "feud" :VERB "feud" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "between" "among" "amongst" "with" "by") :SOURCE NOMLEX)
	(NOM :NOUN "fibber" :VERB "fib" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "fiddler" :VERB "fiddle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fielder" :VERB "field" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "fight" :VERB "fight" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "with" "amongst" "among" "between" "by") :SOURCE NOMLEX)
	(NOM :NOUN "fighter" :VERB "fight" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "fighting" :VERB "fight" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "figure" :VERB "figure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "file" :VERB "file" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "filers" :VERB "file" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "filibuster" :VERB "filibuster" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "filling" :VERB "fill" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "film" :VERB "film" :NOM-TYPE ((VERB-NOM)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "filter" :VERB "filter" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "filtration" :VERB "filter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "finance" :VERB "finance" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "financer" :VERB "finance" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "financier" :VERB "finance" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "find" :VERB "find" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "finder" :VERB "find" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "for")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fine" :VERB "fine" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "to" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "finish" :VERB "finish" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fire" :VERB "fire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "firing" :VERB "fire" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fisher" :VERB "fish" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "fisherman" :VERB "fish" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "fishing" :VERB "fish" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fission" :VERB "divide" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fit" :VERB "fit" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "fitment" :VERB "fit" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fitter" :VERB "fit" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "fix" :VERB "fix" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "for" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fixation" :VERB "fixate" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "flagellant" :VERB "flagellate" :NOM-TYPE ((OBJECT) (SUBJECT))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "flagellation" :VERB "flagellate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "flap" :VERB "flap" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "flapper" :VERB "flap" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "flash" :VERB "flash" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "flatterer" :VERB "flatter" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "flattery" :VERB "flatter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "flaw" :VERB "flaw" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "flicker" :VERB "flick" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "flier" :VERB "fly" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "flight" :VERB "fly" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "flipper" :VERB "flip" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "flirtation" :VERB "flirt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "floatation" :VERB "float" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "flotation" :VERB "float" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "flow" :VERB "flow" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fluctuation" :VERB "fluctuate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "in") :SOURCE NOMLEX)
	(NOM :NOUN "fluoridation" :VERB "fluoridate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "fluoridization" :VERB "fluoridize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "fluting" :VERB "flute" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "flutist" :VERB "flute" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "flyer" :VERB "fly" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "focus" :VERB "focus" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "for") :SOURCE NOMLEX)
	(NOM :NOUN "follow-up" :VERB "follow" :NOM-TYPE ((VERB-PART :ADVAL ("up")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "follower" :VERB "follow" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fomentation" :VERB "foment" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "food" :VERB "feed" :NOM-TYPE ((OBJECT)) :PREPS
	 ("by" "to" "from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "footing" :VERB "foot" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "footslogger" :VERB "footslog" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "foray" :VERB "foray" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "forbearance" :VERB "forbear" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "forecast" :VERB "forecast" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "forecaster" :VERB "forecast" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "foreclosure" :VERB "foreclose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "forfeiture" :VERB "forfeit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "forger" :VERB "forge" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "forgery" :VERB "forge" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "forging" :VERB "forge" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "forgiveness" :VERB "forgive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "formalism" :VERB "formalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "formation" :VERB "form" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "formulation" :VERB "formulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "fornication" :VERB "fornicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "fortification" :VERB "fortify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "fossilization" :VERB "fossilize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "foundation" :VERB "found" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "founder" :VERB "found" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "foundry" :VERB "founder" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "foxhunter" :VERB "foxhunt" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "fragmentation" :VERB "fragment" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "framer" :VERB "frame" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "franchise" :VERB "franchise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "franchisee" :VERB "franchise" :NOM-TYPE ((IND-OBJ)) :PREPS
	 ("for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "franchiser" :VERB "franchise" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fraternization" :VERB "fraternize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "freedom" :VERB "free" :NOM-TYPE ((VERB-NOM)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "freeze" :VERB "freeze" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "freezer" :VERB "freeze" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fructification" :VERB "fructify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "frustration" :VERB "frustrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("over" "by" "with" "of") :SOURCE NOMLEX)
	(NOM :NOUN "fryer" :VERB "fry" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "fucker" :VERB "fuck" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "fuel" :VERB "fuel" :NOM-TYPE ((OBJECT)) :PREPS ("by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "fulfillment" :VERB "fulfill" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "fulfilment" :VERB "fulfill" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "fulmination" :VERB "fulminate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "fumbler" :VERB "fumble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "fumigation" :VERB "fumigate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "function" :VERB "function" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "fund" :VERB "fund" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "fusion" :VERB "fuse" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "gain" :VERB "gain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "in" "by" "from" "for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "gainer" :VERB "gain" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("to" "of" "for") :SOURCE NOMLEX)
	(NOM :NOUN "galvanism" :VERB "galvanize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "gamble" :VERB "gamble" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "gambler" :VERB "gamble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "gardener" :VERB "garden" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "gasification" :VERB "gasify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "gatecrasher" :VERB "gatecrash" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "gatherer" :VERB "gather" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "gathering" :VERB "gather" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "gauge" :VERB "gauge" :NOM-TYPE ((INSTRUMENT)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "generalization" :VERB "generalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "generation" :VERB "generate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "generator" :VERB "generate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "genuflection" :VERB "genuflect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "germination" :VERB "germinate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "gesticulation" :VERB "gesticulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "gesture" :VERB "gesture" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "get-together" :VERB "get" :NOM-TYPE
	 ((VERB-PART :ADVAL ("together"))) :PREPS ("by") :SOURCE NOMLEX)
	(NOM :NOUN "getter" :VERB "get" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of" "for")
	 :SOURCE NOMLEX)
	(NOM :NOUN "gift" :VERB "give" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "gin" :VERB "gin" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "giveaway" :VERB "give" :NOM-TYPE ((VERB-PART :ADVAL ("away")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "giver" :VERB "give" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "glamorization" :VERB "glamorize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "glance" :VERB "glance" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "glare" :VERB "glare" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "gleaner" :VERB "glean" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "glider" :VERB "glide" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "glimmer" :VERB "glimmer" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "glimpse" :VERB "glimpse" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "globalization" :VERB "globalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "glorification" :VERB "glorify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "glue" :VERB "glue" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "gobbler" :VERB "gobble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "goer" :VERB "go" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "golfer" :VERB "golf" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "gossip" :VERB "gossip" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "governance" :VERB "govern" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "government" :VERB "govern" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "governor" :VERB "govern" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "grab" :VERB "grab" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "grabber" :VERB "grab" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "graduate" :VERB "graduate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "graduation" :VERB "graduate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "grant" :VERB "grant" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "to" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "grasp" :VERB "grasp" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "grater" :VERB "grate" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "gratification" :VERB "gratify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "gravitation" :VERB "gravitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "grease" :VERB "grease" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "greaser" :VERB "grease" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "greeting" :VERB "greet" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "greif" :VERB "grieve" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "grief" :VERB "grieve" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "grievance" :VERB "grieve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "grinder" :VERB "grind" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "grip" :VERB "grip" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "gripe" :VERB "gripe" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "groover" :VERB "groove" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ground" :VERB "ground" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "grounding" :VERB "ground" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "groveler" :VERB "grovel" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "groveller" :VERB "grovel" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "grower" :VERB "grow" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "growler" :VERB "growl" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "growth" :VERB "grow" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by" "from" "due to") :SOURCE NOMLEX)
	(NOM :NOUN "grumbler" :VERB "grumble" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "guarantee" :VERB "guarantee" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "guarantor" :VERB "guarantee" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "guard" :VERB "guard" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "guardian" :VERB "guard" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "guess" :VERB "guess" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "guesswork" :VERB "guess" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "guidance" :VERB "guide" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "on" "for" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "guide" :VERB "guide" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "guideline" :VERB "guide" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "guidepost" :VERB "guide" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "gusher" :VERB "gush" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "guzzler" :VERB "guzzle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "gyration" :VERB "gyrate" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "habitation" :VERB "inhabit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "habituation" :VERB "habituate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "hacker" :VERB "hack" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "halt" :VERB "halt" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "handler" :VERB "handle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "handout" :VERB "hand" :NOM-TYPE ((VERB-PART :ADVAL ("out")))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "handover" :VERB "hand" :NOM-TYPE ((VERB-PART :ADVAL ("over")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "hanger" :VERB "hang" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "hangover" :VERB "hang" :NOM-TYPE ((SUBJECT-PART :ADVAL ("over")))
	 :PREPS NIL :SOURCE NOMLEX)
	(NOM :NOUN "happening" :VERB "happen" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "harangue" :VERB "harangue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "harassment" :VERB "harass" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "harm" :VERB "harm" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "harmonization" :VERB "harmonize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "harper" :VERB "harp" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "harpist" :VERB "harp" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "harrier" :VERB "harry" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "harvest" :VERB "harvest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "harvester" :VERB "harvest" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hatred" :VERB "hate" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "haulage" :VERB "haul" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hauler" :VERB "haul" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "hawker" :VERB "hawk" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "head" :VERB "head" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "header" :VERB "head" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "healer" :VERB "heal" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hearer" :VERB "hear" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "heat" :VERB "heat" :NOM-TYPE ((VERB-NOM)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "heater" :VERB "heat" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "heating" :VERB "heat" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "heckler" :VERB "heckle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hedge" :VERB "hedge" :NOM-TYPE ((INSTRUMENT)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "heist" :VERB "heist" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "help" :VERB "help" :NOM-TYPE ((VERB-NOM) (SUBJECT)) :PREPS
	 ("to" "from" "by" "for" "of" "on" "toward") :SOURCE NOMLEX)
	(NOM :NOUN "helper" :VERB "help" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "heritage" :VERB "inherit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "hesitance" :VERB "hesitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "hesitation" :VERB "hesitate" :NOM-TYPE ((VERB-NOM)) :PREPS ("on")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hewer" :VERB "hew" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "hibernation" :VERB "hibernate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "hideaway" :VERB "hide" :NOM-TYPE
	 ((P-OBJ-PART :ADVAL ("away") :PVAL ("in"))) :PREPS ("by") :SOURCE NOMLEX)
	(NOM :NOUN "hideout" :VERB "hide" :NOM-TYPE
	 ((P-OBJ-PART :ADVAL ("out") :PVAL ("in"))) :PREPS ("by") :SOURCE NOMLEX)
	(NOM :NOUN "hider" :VERB "hide" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "highlight" :VERB "highlight" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hijacker" :VERB "hijack" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hike" :VERB "hike" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hiker" :VERB "hike" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "hindrance" :VERB "hinder" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hindrances" :VERB "hinder" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "hint" :VERB "hint" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "hiring" :VERB "hire" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hit" :VERB "hit" :NOM-TYPE ((VERB-NOM)) :PREPS ("from" "by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hitchhiker" :VERB "hitchhike" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "hitter" :VERB "hit" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "hoard" :VERB "hoard" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hoarder" :VERB "hoard" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "hoaxer" :VERB "hoax" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hog" :VERB "hog" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "hoist" :VERB "hoist" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hold" :VERB "hold" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "holder" :VERB "hold" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "holding" :VERB "hold" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "holdout" :VERB "hold" :NOM-TYPE ((SUBJECT-PART :ADVAL ("out")))
	 :PREPS NIL :SOURCE NOMLEX)
	(NOM :NOUN "holdup" :VERB "hold" :NOM-TYPE ((VERB-PART :ADVAL ("up"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "honor" :VERB "honor" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "honoree" :VERB "honor" :NOM-TYPE ((OBJECT)) :PREPS ("by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "hook-up" :VERB "hook" :NOM-TYPE ((VERB-PART :ADVAL ("up"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "hooter" :VERB "hoot" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "hope" :VERB "hope" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by" "in" "for") :SOURCE NOMLEX)
	(NOM :NOUN "hopper" :VERB "hop" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "hospitalization" :VERB "hospitalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "host" :VERB "host" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "howl" :VERB "howl" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "howler" :VERB "howl" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "hug" :VERB "hug" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "among" "between" "amongst" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "humiliation" :VERB "humiliate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "hunger" :VERB "hunger" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hunt" :VERB "hunt" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hunter" :VERB "hunt" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "hunting" :VERB "hunt" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hurdler" :VERB "hurdle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hurry" :VERB "hurry" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hustler" :VERB "hustle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "hydrogenation" :VERB "hydrogenate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "hypnotism" :VERB "hypnotize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "hypnotist" :VERB "hypnotize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "idealism" :VERB "idealize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "idealist" :VERB "idealize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "idealization" :VERB "idealize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "identification" :VERB "identify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "identity" :VERB "identify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "idler" :VERB "idle" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "idolization" :VERB "idolize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "ignition" :VERB "ignite" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ill-treatment" :VERB "ill-treat" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "ill-usage" :VERB "ill-use" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "illumination" :VERB "illuminate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "illustration" :VERB "illustrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "illustrator" :VERB "illustrate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "image" :VERB "imagine" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "imagination" :VERB "imagine" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "imitation" :VERB "imitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "imitator" :VERB "imitate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "immersion" :VERB "immerse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "immigrant" :VERB "immigrate" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "immigration" :VERB "immigrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by") :SOURCE NOMLEX)
	(NOM :NOUN "immobilization" :VERB "immobilize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "immolation" :VERB "immolate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "immunization" :VERB "immunize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "impact" :VERB "impact" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "on" "from") :SOURCE NOMLEX)
	(NOM :NOUN "impairment" :VERB "impair" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "impalement" :VERB "impale" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "impeachment" :VERB "impeach" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "impediment" :VERB "impede" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "impeller" :VERB "impel" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "impersonation" :VERB "impersonate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "impersonator" :VERB "impersonate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "impingement" :VERB "impinge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "implant" :VERB "implant" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "implantation" :VERB "implant" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "implementation" :VERB "implement" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "implication" :VERB "implicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "implosion" :VERB "implode" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "importation" :VERB "import" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "importer" :VERB "import" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "imposition" :VERB "impose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "impoverishment" :VERB "impoverish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "imprecation" :VERB "imprecate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "impression" :VERB "impress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "imprisonment" :VERB "imprison" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "improvement" :VERB "improve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "improver" :VERB "improve" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "improvisation" :VERB "improvise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "improviser" :VERB "improvise" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "imputation" :VERB "impute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inactivation" :VERB "deactivate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inauguration" :VERB "inaugurate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "incarceration" :VERB "incarcerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "incarnation" :VERB "incarnate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "incineration" :VERB "incinerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "incinerator" :VERB "incinerate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "incision" :VERB "incise" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "incitement" :VERB "incite" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inclination" :VERB "incline" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inclosure" :VERB "inclose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inclusion" :VERB "include" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inconvenience" :VERB "inconvenience" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "incorporation" :VERB "incorporate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "between" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "increase" :VERB "increase" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "in" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "incrimination" :VERB "incriminate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "incubation" :VERB "incubate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "incubator" :VERB "incubate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "incursion" :VERB "incur" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "indemnification" :VERB "indemnify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "indemnity" :VERB "indemnify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "indentation" :VERB "indent" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "indenture" :VERB "indenture" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "index" :VERB "index" :NOM-TYPE ((VERB-NOM)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "indexation" :VERB "index" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "indexer" :VERB "index" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "indication" :VERB "indicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "indicative" :VERB "indicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "indicator" :VERB "indicate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "indictment" :VERB "indict" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "indoctrination" :VERB "indoctrinate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inducement" :VERB "induce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "for" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "induction" :VERB "induce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "indulgence" :VERB "indulge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "industrialization" :VERB "industrialize" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "inebriation" :VERB "inebriate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "infatuation" :VERB "infatuate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "infection" :VERB "infect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "inference" :VERB "infer" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "infestation" :VERB "infest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "in" "on" "by") :SOURCE NOMLEX)
	(NOM :NOUN "infiltration" :VERB "infiltrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "inflammation" :VERB "inflame" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inflation" :VERB "inflate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inflection" :VERB "inflect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "infliction" :VERB "inflict" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "influence" :VERB "influence" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "on" "by") :SOURCE NOMLEX)
	(NOM :NOUN "informant" :VERB "inform" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "information" :VERB "inform" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "through" "of" "to" "out of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "informer" :VERB "inform" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "infringement" :VERB "infringe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "against") :SOURCE NOMLEX)
	(NOM :NOUN "infusion" :VERB "infuse" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ingestion" :VERB "ingest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inhabitant" :VERB "inhabit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "inhalation" :VERB "inhale" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "inhaler" :VERB "inhale" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "inheritance" :VERB "inherit" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "inheritor" :VERB "inherit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "inhibition" :VERB "inhibit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inhibitor" :VERB "inhibit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "initiation" :VERB "initiate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "initiator" :VERB "initiate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "injection" :VERB "inject" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "injury" :VERB "injure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "innovation" :VERB "innovate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "innovator" :VERB "innovate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "inoculation" :VERB "inoculate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "input" :VERB "input" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "inquirer" :VERB "inquire" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "inquiry" :VERB "inquire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "inquisition" :VERB "inquire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "inscription" :VERB "inscribe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "insemination" :VERB "inseminate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "insert" :VERB "insert" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "insertion" :VERB "insert" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "insinuation" :VERB "insinuate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "insistence" :VERB "insist" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "inspection" :VERB "inspect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inspector" :VERB "inspect" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "inspiration" :VERB "inspire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "installation" :VERB "install" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "installment" :VERB "install" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "instigation" :VERB "instigate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "instigator" :VERB "instigate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "instillation" :VERB "instill" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "institution" :VERB "institute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "instruction" :VERB "instruct" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "instructions" :VERB "instruct" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "instructor" :VERB "instruct" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "insulation" :VERB "insulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "insulator" :VERB "insulate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "insult" :VERB "insult" :NOM-TYPE ((VERB-NOM)) :PREPS ("to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "insurance" :VERB "insure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "insurer" :VERB "insure" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "integration" :VERB "integrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "intensification" :VERB "intensify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "intensifier" :VERB "intensify" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "intent" :VERB "intend" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "intention" :VERB "intend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "interaction" :VERB "interact" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "interception" :VERB "intercept" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "interceptor" :VERB "intercept" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "intercession" :VERB "intercede" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "intercommunication" :VERB "intercommunicate" :NOM-TYPE
	 ((VERB-NOM)) :PREPS ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "interconnection" :VERB "interconnect" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "interdiction" :VERB "interdict" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "interest" :VERB "interest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("on" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "interference" :VERB "interfere" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "interjection" :VERB "interject" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "intermarriage" :VERB "intermarry" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "interment" :VERB "inter" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "internationalization" :VERB "internationalize" :NOM-TYPE
	 ((VERB-NOM)) :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "internee" :VERB "intern" :NOM-TYPE ((OBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "internment" :VERB "intern" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "interpellation" :VERB "interpellate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "interpolation" :VERB "interpolate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "interposition" :VERB "interpose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "interpretation" :VERB "interpret" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("on" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "interpreter" :VERB "interpret" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "interrelation" :VERB "interrelate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "interrogation" :VERB "interrogate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "interrogator" :VERB "interrogate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "interrupter" :VERB "interrupt" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "interruption" :VERB "interrupt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "intersection" :VERB "intersect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "between" "by") :SOURCE NOMLEX)
	(NOM :NOUN "intervention" :VERB "intervene" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "interview" :VERB "interview" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "by" "with" "of") :SOURCE NOMLEX)
	(NOM :NOUN "interviewee" :VERB "interview" :NOM-TYPE ((OBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "interviewer" :VERB "interview" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "intimation" :VERB "intimate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "intimidation" :VERB "intimidate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "intonation" :VERB "intone" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "intoxicant" :VERB "intoxicate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "intoxication" :VERB "intoxicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "introduction" :VERB "introduce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "introspection" :VERB "introspect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "introversion" :VERB "introvert" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "intruder" :VERB "intrude" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "intrusion" :VERB "intrude" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "intuition" :VERB "intuit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "regarding" "with respect to" "in regard to" "over" "on"
	  "in connection with" "with regard to" "concerning" "about" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "inundation" :VERB "inundate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "invader" :VERB "invade" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "invalidation" :VERB "invalidate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "invasion" :VERB "invade" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "invention" :VERB "invent" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "inventor" :VERB "invent" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "inversion" :VERB "invert" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "investigation" :VERB "investigate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "investigator" :VERB "investigate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "investment" :VERB "invest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "investor" :VERB "invest" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "invitation" :VERB "invite" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "invitational" :VERB "invite" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "involution" :VERB "involve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "involvement" :VERB "involve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "ionization" :VERB "ionize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "irradiation" :VERB "irradiate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "irrigation" :VERB "irrigate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "irritant" :VERB "irritate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "irritation" :VERB "irritate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "with" "by" "over" "of") :SOURCE NOMLEX)
	(NOM :NOUN "isolation" :VERB "isolate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "issuance" :VERB "issue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "issue" :VERB "issue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "issuer" :VERB "issue" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "iteration" :VERB "iterate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "jab" :VERB "jab" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "jabber" :VERB "jabber" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "jabberer" :VERB "jabber" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "jailer" :VERB "jail" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "jailor" :VERB "jail" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "jam" :VERB "jam" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "jaunt" :VERB "jaunt" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "jaywalker" :VERB "jaywalk" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "jest" :VERB "joke" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "jester" :VERB "jest" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "jobber" :VERB "job" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "jogger" :VERB "jog" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "joiner" :VERB "join" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "jointure" :VERB "joint" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "joke" :VERB "joke" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "joker" :VERB "joke" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "jolt" :VERB "jolt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "jotter" :VERB "jot" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "journey" :VERB "journey" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "judgement" :VERB "judge" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "judgment" :VERB "judge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "juggler" :VERB "juggle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "jump" :VERB "jump" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by" "in")
	 :SOURCE NOMLEX)
	(NOM :NOUN "jumper" :VERB "jump" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "junction" :VERB "adjoin" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "between" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "junket" :VERB "junket" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "justification" :VERB "justify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "juxtaposition" :VERB "juxtapose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "between" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "keeper" :VERB "keep" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "kidnapper" :VERB "kidnap" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "killer" :VERB "kill" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "kisser" :VERB "kiss" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "knitter" :VERB "knit" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "knock" :VERB "knock" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "knocker" :VERB "knock" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "know-how" :VERB "know" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "knowledge" :VERB "know" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "known" :VERB "know" :NOM-TYPE ((OBJECT)) :PREPS ("from" "by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "labor" :VERB "labor" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "laborer" :VERB "labor" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "laceration" :VERB "lacerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "lack" :VERB "lack" :NOM-TYPE ((VERB-NOM)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "lagging" :VERB "lag" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lament" :VERB "lament" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lamentation" :VERB "lament" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "lancer" :VERB "lance" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "landing" :VERB "land" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "lapse" :VERB "lapse" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "laugh" :VERB "laugh" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "launch" :VERB "launch" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "launcher" :VERB "launch" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "launderer" :VERB "launder" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lawsuit" :VERB "sue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "on" "to" "of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "layer" :VERB "lay" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "layoff" :VERB "lay" :NOM-TYPE ((VERB-PART :ADVAL ("off"))) :PREPS
	 ("by" "of" "in") :SOURCE NOMLEX)
	(NOM :NOUN "lead" :VERB "lead" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "leader" :VERB "lead" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "leakage" :VERB "leak" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "leap" :VERB "leap" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by" "in")
	 :SOURCE NOMLEX)
	(NOM :NOUN "learner" :VERB "learn" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "lease" :VERB "lease" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "leave" :VERB "leave" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "lecture" :VERB "lecture" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "lecturer" :VERB "lecture" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "legalization" :VERB "legalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "legislation" :VERB "legislate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "legislator" :VERB "legislate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lender" :VERB "lend" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lessee" :VERB "lease" :NOM-TYPE ((IND-OBJ :PVAL ("to"))) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "leveler" :VERB "level" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "leveller" :VERB "level" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "leverage" :VERB "leverage" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "levitation" :VERB "levitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "levy" :VERB "levy" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "libel" :VERB "libel" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("against" "by") :SOURCE NOMLEX)
	(NOM :NOUN "liberalization" :VERB "liberalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "liberation" :VERB "liberate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "liberator" :VERB "liberate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "license" :VERB "license" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "licensee" :VERB "license" :NOM-TYPE ((IND-OBJ :PVAL ("to")))
	 :PREPS ("of") :SOURCE NOMLEX)
	(NOM :NOUN "life" :VERB "live" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lift" :VERB "lift" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lift-off" :VERB "lift" :NOM-TYPE ((VERB-PART :ADVAL ("off")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "liftoff" :VERB "lift" :NOM-TYPE ((VERB-PART :ADVAL ("off")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "lighter" :VERB "light" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "like" :VERB "like" :NOM-TYPE ((OBJECT)) :PREPS ("of" "by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "limit" :VERB "limit" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "limitation" :VERB "limit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "upon" "on" "by") :SOURCE NOMLEX)
	(NOM :NOUN "line-up" :VERB "line" :NOM-TYPE ((VERB-PART :ADVAL "up")) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "liner" :VERB "line" :NOM-TYPE ((INSTRUMENT)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lineup" :VERB "line" :NOM-TYPE ((VERB-PART :ADVAL "up")) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "lingerer" :VERB "linger" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "link" :VERB "link" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "between" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "linkage" :VERB "link" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "liquidation" :VERB "liquidate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "liquidator" :VERB "liquidate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "liquidizer" :VERB "liquidize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "list" :VERB "list" :NOM-TYPE ((VERB-NOM)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "listener" :VERB "listen" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "listing" :VERB "list" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "litigant" :VERB "litigate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "litigation" :VERB "litigate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "between" "by") :SOURCE NOMLEX)
	(NOM :NOUN "loader" :VERB "load" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "loafer" :VERB "loaf" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "loan" :VERB "loan" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "between" "to" "of" "at" "by") :SOURCE NOMLEX)
	(NOM :NOUN "loathing" :VERB "loathe" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lobbyist" :VERB "lobby" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "localization" :VERB "localize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "location" :VERB "locate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lock" :VERB "lock" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "locker" :VERB "lock" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "lockup" :VERB "lock" :NOM-TYPE ((VERB-PART :ADVAL ("up"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "lodgement" :VERB "lodge" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lodger" :VERB "lodge" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "loiterer" :VERB "loiter" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "longing" :VERB "long" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "look" :VERB "look" :NOM-TYPE ((VERB-NOM)) :PREPS ("by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "looker" :VERB "look" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "lookout" :VERB "look" :NOM-TYPE ((VERB-PART :ADVAL ("out")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "looter" :VERB "loot" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "lord" :VERB "lord" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "loser" :VERB "lose" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "loss" :VERB "lose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "by" "for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "lounger" :VERB "lounge" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "love" :VERB "love" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lover" :VERB "love" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "lubricant" :VERB "lubricate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "lubrication" :VERB "lubricate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "lunch" :VERB "lunch" :NOM-TYPE ((VERB-NOM)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "lurcher" :VERB "lurch" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "lure" :VERB "lure" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "luxuriance" :VERB "luxuriate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "machination" :VERB "machinate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "magnetization" :VERB "magnetize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "magnification" :VERB "magnify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "magnifier" :VERB "magnify" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mail" :VERB "mail" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("within" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mailing" :VERB "mail" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "maintainence" :VERB "maintain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "maintenance" :VERB "maintain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "maker" :VERB "make" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "makeup" :VERB "makeup" :NOM-TYPE ((VERB-PART :ADVAL ("up")))
	 :PREPS ("of") :SOURCE NOMLEX)
	(NOM :NOUN "malfunction" :VERB "malfunction" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "malingerer" :VERB "malinger" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "maltreatment" :VERB "maltreat" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "management" :VERB "manage" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "manager" :VERB "manage" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mandate" :VERB "mandate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "maneuver" :VERB "maneuver" :NOM-TYPE ((VERB-NOM)) :PREPS ("by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "manicurist" :VERB "manicure" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "manifestation" :VERB "manifest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "manipulation" :VERB "manipulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "manipulator" :VERB "manipulate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "manufacture" :VERB "manufacture" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "manufacturer" :VERB "manufacture" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "manumission" :VERB "manumit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "manure" :VERB "manure" :NOM-TYPE ((P-OBJ :PVAL ("with"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "map" :VERB "map" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "marauder" :VERB "maraud" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "march" :VERB "march" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "marcher" :VERB "march" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "mark" :VERB "mark" :NOM-TYPE ((OBJECT)) :PREPS ("by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "markdown" :VERB "mark" :NOM-TYPE ((VERB-PART :ADVAL ("off")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "marker" :VERB "mark" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "marketer" :VERB "market" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "marketization" :VERB "market" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "marriage" :VERB "marry" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "masher" :VERB "mash" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "massacre" :VERB "massacre" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "massage" :VERB "massage" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "master" :VERB "master" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "mastermind" :VERB "mastermind" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mastery" :VERB "master" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mastication" :VERB "masticate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "masturbation" :VERB "masturbate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "match" :VERB "match" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "materialization" :VERB "materialize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "matriculation" :VERB "matriculate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "matter" :VERB "matter" :NOM-TYPE ((P-OBJ :PVAL ("to"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "maturation" :VERB "mature" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "maturity" :VERB "mature" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "maximization" :VERB "maximize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "measure" :VERB "measure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "of" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "measurement" :VERB "measure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mechanization" :VERB "mechanize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "meddler" :VERB "meddle" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "mediation" :VERB "mediate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mediator" :VERB "mediate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "medication" :VERB "medicate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "meditation" :VERB "meditate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "meeting" :VERB "meet" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "of" "amongst" "among" "with" "by") :SOURCE NOMLEX)
	(NOM :NOUN "melioration" :VERB "meliorate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "meltdown" :VERB "melt" :NOM-TYPE ((VERB-NOM)) :PREPS ("by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "menace" :VERB "menace" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mender" :VERB "mend" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "menstruation" :VERB "menstruate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mention" :VERB "mention" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mentor" :VERB "mentor" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "merger" :VERB "merge" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "merit" :VERB "merit" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mesmerism" :VERB "mesmerize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mesmerist" :VERB "mesmerize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mess" :VERB "mess" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "migrant" :VERB "migrate" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "migration" :VERB "migrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "miller" :VERB "mill" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "mimicry" :VERB "mimic" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mincer" :VERB "mince" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "minder" :VERB "mind" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "mine" :VERB "mine" :NOM-TYPE ((VERB-NOM)) :PREPS ("for" "of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "miner" :VERB "mine" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "miniaturization" :VERB "miniaturize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "ministry" :VERB "minister" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mirror" :VERB "mirror" :NOM-TYPE ((INSTRUMENT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "misapplication" :VERB "misapply" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "misapprehension" :VERB "misapprehend" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "misappropriation" :VERB "misappropriate" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "misbehavior" :VERB "misbehave" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "miscalculation" :VERB "miscalculate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "miscarriage" :VERB "miscarry" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "misconception" :VERB "misconceive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "misdirection" :VERB "misdirect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "misgovernment" :VERB "misgovern" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "misinformation" :VERB "misinform" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "misinterpretation" :VERB "misinterpret" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "misjudgment" :VERB "misjudge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mismanagement" :VERB "mismanage" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mismatch" :VERB "mismatch" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "between" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "misquotation" :VERB "misquote" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "misrepresentation" :VERB "misrepresent" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "misstatement" :VERB "misstate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mistranslation" :VERB "mistranslate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mistrust" :VERB "mistrust" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "misunderstanding" :VERB "misunderstand" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "misuse" :VERB "misuse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "mitigation" :VERB "mitigate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mix" :VERB "mix" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "mixer" :VERB "mix" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "mixture" :VERB "mix" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "mob" :VERB "mob" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "mobilization" :VERB "mobilize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "mocker" :VERB "mock" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "mockery" :VERB "mock" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "modeler" :VERB "model" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "modeller" :VERB "model" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "modelling" :VERB "model" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "moderation" :VERB "moderate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "moderator" :VERB "moderate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "modernization" :VERB "modernize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "modification" :VERB "modify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "modifier" :VERB "modify" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "modulation" :VERB "modulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "moisturizer" :VERB "moisturize" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "molestation" :VERB "molest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mollification" :VERB "mollify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "monetization" :VERB "monetize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "monitor" :VERB "monitor" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "monopolist" :VERB "monopolize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "monopolization" :VERB "monopolize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "monopoly" :VERB "monopolize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "mop-up" :VERB "mop" :NOM-TYPE ((VERB-PART :ADVAL ("up"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mortgage" :VERB "mortgage" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("on" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mortgagee" :VERB "mortgage" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mortgagor" :VERB "mortgage" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mortification" :VERB "mortify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "motion1" :VERB "move" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "motion2" :VERB "motion" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "motivation" :VERB "motivate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mourner" :VERB "mourn" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "mouser" :VERB "mouse" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "move" :VERB "move" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "by" "for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "movement" :VERB "move" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mover" :VERB "move" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "mower" :VERB "mow" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "muffler" :VERB "muffle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mugger" :VERB "mug" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "multiple" :VERB "multiply" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "multiplication" :VERB "multiply" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "multiplier" :VERB "multiply" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mummification" :VERB "mummify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "munition" :VERB "munition" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "murder" :VERB "murder" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "murderer" :VERB "murder" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mutation" :VERB "mutate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mutilation" :VERB "mutilate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "mutterer" :VERB "mutter" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "mystification" :VERB "mystify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "nagger" :VERB "nag" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "narration" :VERB "narrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "narrator" :VERB "narrate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "nationalization" :VERB "nationalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "naturalization" :VERB "naturalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "navigation" :VERB "navigate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "navigator" :VERB "navigate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "necessity" :VERB "need" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "need" :VERB "need" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "for" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "negation" :VERB "negate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "neglect" :VERB "neglect" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "negligence" :VERB "neglect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "negotiation" :VERB "negotiate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "between" "with" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "negotiator" :VERB "negotiate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "neighbor" :VERB "neighbor" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "neutralization" :VERB "neutralize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "nickname" :VERB "nickname" :NOM-TYPE ((COMP)) :PREPS ("for" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "nipper" :VERB "nip" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "nod" :VERB "nod" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "nomination" :VERB "nominate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "nominee" :VERB "nominate" :NOM-TYPE ((OBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "normalization" :VERB "normalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "notice" :VERB "notice" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "notification" :VERB "notify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "nourishment" :VERB "nourish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "nullification" :VERB "nullify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "obedience" :VERB "obey" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "obeisance" :VERB "obey" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "obfuscation" :VERB "obfuscate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "objection" :VERB "object" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "objector" :VERB "object" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "obligation" :VERB "oblige" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("under" "by" "of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "obliteration" :VERB "obliterate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "observance" :VERB "observe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("across" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "observation" :VERB "observe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "observer" :VERB "observe" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "obsession" :VERB "obsess" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "obstruction" :VERB "obstruct" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "occasion" :VERB "occasion" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "occupancy" :VERB "occupy" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "occupant" :VERB "occupy" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "occupation" :VERB "occupy" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "with") :SOURCE NOMLEX)
	(NOM :NOUN "occupier" :VERB "occupy" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "occurrence" :VERB "occur" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "offender" :VERB "offend" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "offense" :VERB "offend" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "offer" :VERB "offer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "oiler" :VERB "oil" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "omission" :VERB "omit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "open" :VERB "open" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "opener" :VERB "open" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "operation" :VERB "operate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "operator" :VERB "operate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "opinion" :VERB "opine" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "opponent" :VERB "oppose" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "opposition" :VERB "oppose" :NOM-TYPE ((SUBJECT) (VERB-NOM)) :PREPS
	 ("from" "of" "to" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "oppression" :VERB "oppress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "oppressor" :VERB "oppress" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "optimization" :VERB "optimize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "option" :VERB "opt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "oration" :VERB "orate" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "orator" :VERB "orate" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "oratory" :VERB "orate" :NOM-TYPE ((VERB-NOM)) :PREPS ("by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "orchestration" :VERB "orchestrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "order" :VERB "order" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "for" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "organization" :VERB "organize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "organizer" :VERB "organize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "orientation" :VERB "orientate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "origin" :VERB "originate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "origination" :VERB "originate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "originator" :VERB "originate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ornament" :VERB "ornament" :NOM-TYPE ((P-OBJ :PVAL ("with")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "ornamentation" :VERB "ornament" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "oscillation" :VERB "oscillate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "oscillator" :VERB "oscillate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ossification" :VERB "ossify" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ostracism" :VERB "ostracize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "ouster" :VERB "oust" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "outbreak" :VERB "break" :NOM-TYPE ((VERB-PART :ADVAL ("out")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "outcry" :VERB "cry" :NOM-TYPE ((VERB-PART :ADVAL ("out"))) :PREPS
	 ("by") :SOURCE NOMLEX)
	(NOM :NOUN "outfitter" :VERB "outfit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "outflow" :VERB "flow" :NOM-TYPE ((VERB-PART :ADVAL ("out")))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "outlawry" :VERB "outlaw" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "outlay" :VERB "lay" :NOM-TYPE ((VERB-PART :ADVAL ("out"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "outline" :VERB "outline" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "output" :VERB "put" :NOM-TYPE ((VERB-PART :ADVAL ("out"))) :PREPS
	 ("of" "per" "at" "by" "from" "in" "for" "to") :SOURCE NOMLEX)
	(NOM :NOUN "outrage" :VERB "outrage" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "by" "over") :SOURCE NOMLEX)
	(NOM :NOUN "outrider" :VERB "outride" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "outset" :VERB "set" :NOM-TYPE ((VERB-PART :ADVAL ("out"))) :PREPS
	 ("by") :SOURCE NOMLEX)
	(NOM :NOUN "overcharge" :VERB "overcharge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "overexertion" :VERB "overexert" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "overexposure" :VERB "overexpose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "overextension" :VERB "overextend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "overhaul" :VERB "overhaul" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "overindulgence" :VERB "overindulge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "overlay" :VERB "overlay" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "overpayment" :VERB "overpay" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "overproduction" :VERB "overproduce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "overrun" :VERB "overrun" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "overseer" :VERB "oversee" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "oversight" :VERB "oversee" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "overstatement" :VERB "overstate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "overuse" :VERB "overuse" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "owner" :VERB "own" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "ownership" :VERB "own" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "by" "of" "for") :SOURCE NOMLEX)
	(NOM :NOUN "oxidation" :VERB "oxidize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "oxidization" :VERB "oxidize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pace" :VERB "pace" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pacification" :VERB "pacify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "packer" :VERB "pack" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "pain" :VERB "pain" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "to" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "painter" :VERB "paint" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "pall" :VERB "pall" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "palliation" :VERB "palliate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "palmer" :VERB "palm" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "palpitation" :VERB "palpitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "panic" :VERB "panic" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("over" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "parachutist" :VERB "parachute" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "parallel" :VERB "parallel" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "parallelism" :VERB "parallel" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "paralysis" :VERB "paralyze" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pardon" :VERB "pardon" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pardoner" :VERB "pardon" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "parking" :VERB "park" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "parodist" :VERB "parody" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "parody" :VERB "parody" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "parolee" :VERB "parole" :NOM-TYPE ((OBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "participant" :VERB "participate" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "participation" :VERB "participate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "partition" :VERB "partition" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "party" :VERB "participate" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "pass" :VERB "pass" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "passage" :VERB "pass" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "passing" :VERB "pass" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pasteurization" :VERB "pasteurize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "patent" :VERB "patent" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "patentee" :VERB "patent" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "patrol" :VERB "patrol" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "patron" :VERB "patronize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "patter" :VERB "patter" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pauperization" :VERB "pauperize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pause" :VERB "pause" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "in") :SOURCE NOMLEX)
	(NOM :NOUN "pavement" :VERB "pave" :NOM-TYPE ((P-OBJ :PVAL ("with"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pay" :VERB "pay" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "to" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "payee" :VERB "pay" :NOM-TYPE ((IND-OBJ :PVAL ("to"))) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "payer" :VERB "pay" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "payment" :VERB "pay" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "payout" :VERB "pay" :NOM-TYPE ((VERB-PART :ADVAL ("out"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "payroll" :VERB "pay" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "peak" :VERB "peak" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "peal" :VERB "peal" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pecker" :VERB "peck" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "peculation" :VERB "peculate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "peddler" :VERB "peddle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "peek" :VERB "peek" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "peeler" :VERB "peel" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "peeper" :VERB "peep" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "penalization" :VERB "penalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "penetration" :VERB "penetrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "pension" :VERB "pension off" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "pensioner" :VERB "pension off" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "perambulation" :VERB "perambulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "perambulator" :VERB "perambulate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "perceiver" :VERB "perceive" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "perception" :VERB "perceive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "percolator" :VERB "percolate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "perfection" :VERB "perfect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "perforation" :VERB "perforate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "performance" :VERB "perform" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "performer" :VERB "perform" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "perfumer" :VERB "perfume" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "peril" :VERB "imperil" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "perjurer" :VERB "perjure" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "perjury" :VERB "perjure" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "permeation" :VERB "permeate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "permission" :VERB "permit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by" "from" "with") :SOURCE NOMLEX)
	(NOM :NOUN "permit" :VERB "permit" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "permutation" :VERB "permute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "perpetration" :VERB "perpetrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "perpetrator" :VERB "perpetrate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "perpetuation" :VERB "perpetuate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "persecution" :VERB "persecute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "persecutor" :VERB "persecute" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "perseverance" :VERB "persevere" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "persistence" :VERB "persist" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "personation" :VERB "personate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "personification" :VERB "personify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "perspiration" :VERB "perspire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "persuasion" :VERB "persuade" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "perturbation" :VERB "perturb" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "perusal" :VERB "peruse" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pervasion" :VERB "pervade" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "perversion" :VERB "pervert" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "petition" :VERB "petition" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "petitioner" :VERB "petition" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "phase-out" :VERB "phase" :NOM-TYPE ((VERB-PART :ADVAL ("out")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "philanderer" :VERB "philander" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "photocopier" :VERB "photocopy" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "photographer" :VERB "photograph" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "phrase" :VERB "phrase" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "pick" :VERB "pick" :NOM-TYPE ((OBJECT)) :PREPS ("for" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "picker" :VERB "pick" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pickup" :VERB "pick" :NOM-TYPE ((VERB-PART :ADVAL ("up"))) :PREPS
	 ("in" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "picture" :VERB "picture" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pileup" :VERB "pile" :NOM-TYPE ((VERB-PART :ADVAL ("up"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pilferage" :VERB "pilfer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "pilferer" :VERB "pilfer" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pillager" :VERB "pillage" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pioneer" :VERB "pioneer" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "piper" :VERB "pipe" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "piracy" :VERB "pirate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pirate" :VERB "pirate" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "pitch" :VERB "pitch" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pitcher" :VERB "pitch" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "pity" :VERB "pity" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "placement" :VERB "place" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("through" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "plagiarism" :VERB "plagiarize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "plagiarist" :VERB "plagiarize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "plan" :VERB "plan" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "planner" :VERB "plan" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "plantation" :VERB "plant" :NOM-TYPE ((P-OBJ :PVAL ("on" "in")))
	 :PREPS ("of") :SOURCE NOMLEX)
	(NOM :NOUN "planter" :VERB "plant" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "plasterer" :VERB "plaster" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "plateau" :VERB "plateau" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "play" :VERB "play" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "player" :VERB "play" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "pleasure" :VERB "please" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "from" "with" "in" "of") :SOURCE NOMLEX)
	(NOM :NOUN "plodder" :VERB "plod" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "plot" :VERB "plot" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "plotter" :VERB "plot" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "plug" :VERB "plug" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "plummet" :VERB "plummet" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "plunderer" :VERB "plunder" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "plunge" :VERB "plunge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "by" "of" "on") :SOURCE NOMLEX)
	(NOM :NOUN "plunger" :VERB "plunge" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "poacher" :VERB "poach" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "pointer" :VERB "point" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "poisoner" :VERB "poison" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "poker" :VERB "poke" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "polarization" :VERB "polarize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "polisher" :VERB "polish" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "poll" :VERB "poll" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pollination" :VERB "pollinate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pollutant" :VERB "pollute" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pollution" :VERB "pollute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "popularization" :VERB "popularize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "population" :VERB "populate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "portent" :VERB "portend" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "porter" :VERB "port" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "portion" :VERB "portion" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "portrayal" :VERB "portray" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "poser" :VERB "pose" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "position" :VERB "position" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "possession" :VERB "possess" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "possessor" :VERB "possess" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "postponement" :VERB "postpone" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "posture" :VERB "posture" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "potterer" :VERB "potter" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pounder" :VERB "pound" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "powder" :VERB "powder" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "practice" :VERB "practice" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "practitioner" :VERB "practice" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "praise" :VERB "praise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("on" "of" "to" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "prattle" :VERB "prattle" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "prattler" :VERB "prattle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "prayer" :VERB "pray" :NOM-TYPE ((SUBJECT) (VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "pre-emption" :VERB "pre-empt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pre-existence" :VERB "pre-exist" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "preacher" :VERB "preach" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "prearrangement" :VERB "prearrange" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "precaution" :VERB "caution" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "precedence" :VERB "precede" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "precedent" :VERB "precede" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "precession" :VERB "precede" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "precipitation" :VERB "precipitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "preclearance" :VERB "clear" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "preclusion" :VERB "preclude" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "predecessor" :VERB "precede" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "predestination" :VERB "predestinate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "predetermination" :VERB "predetermine" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "prediction" :VERB "predict" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "predictor" :VERB "predict" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "predisposition" :VERB "predispose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "predominance" :VERB "predominate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "in" "of") :SOURCE NOMLEX)
	(NOM :NOUN "prefabrication" :VERB "prefabricate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "preface" :VERB "preface" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "preference" :VERB "prefer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "preferment" :VERB "prefer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "prejudgement" :VERB "prejudge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "prejudice" :VERB "prejudice" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "prelude" :VERB "prelude" :NOM-TYPE ((VERB-NOM) (SUBJECT)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "premeditation" :VERB "premeditate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "premiere" :VERB "premiere" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "premise" :VERB "premise" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "preoccupation" :VERB "preoccupy" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("with" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "preparation" :VERB "prepare" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "preparer" :VERB "prepare" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "prepayment" :VERB "prepay" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "preponderance" :VERB "preponderate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "prepossession" :VERB "prepossess" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "prescription" :VERB "prescribe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "present" :VERB "present" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "presentation" :VERB "present" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "presenter" :VERB "present" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "preservation" :VERB "preserve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "preserver" :VERB "preserve" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pressure" :VERB ("pressure" "press") :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "presumption" :VERB "presume" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "in") :SOURCE NOMLEX)
	(NOM :NOUN "presupposition" :VERB "presuppose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "pretender" :VERB "pretend" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pretense" :VERB "pretend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "pretension" :VERB "pretend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "prevalence" :VERB "prevail" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "prevarication" :VERB "prevaricate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "prevention" :VERB "prevent" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "preview" :VERB "preview" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "prey" :VERB "prey" :NOM-TYPE ((P-OBJ :PVAL ("on" "upon"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "price" :VERB "price" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "on" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pricker" :VERB "prick" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "primer" :VERB "prime" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "printer" :VERB "print" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "privatization" :VERB "privatize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "probation" :VERB "probate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "probe" :VERB "probe" :NOM-TYPE ((VERB-NOM) (SUBJECT)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "proceeding" :VERB "proceed" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "processor" :VERB "process" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "proclamation" :VERB "proclaim" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "procrastination" :VERB "procrastinate" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "procreation" :VERB "procreate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "procurement" :VERB "procure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "procurer" :VERB "procure" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "producer" :VERB "produce" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "product" :VERB "produce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "production" :VERB "produce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by" "with") :SOURCE NOMLEX)
	(NOM :NOUN "profanation" :VERB "profane" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "profession" :VERB "profess" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "professor" :VERB "profess" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "profile" :VERB "profile" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "profit" :VERB "profit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "prognostication" :VERB "prognosticate" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "programmer" :VERB "program" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "progress" :VERB "progress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "in") :SOURCE NOMLEX)
	(NOM :NOUN "progression" :VERB "progress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "prohibition" :VERB "prohibit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "on" "upon" "against" "by") :SOURCE NOMLEX)
	(NOM :NOUN "projection" :VERB "project" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "projector" :VERB "project" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "proliferation" :VERB "proliferate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "in") :SOURCE NOMLEX)
	(NOM :NOUN "prolongation" :VERB "prolong" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "promise" :VERB "promise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "promoter" :VERB "promote" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "promotion" :VERB "promote" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "prompter" :VERB "prompt" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "promulgation" :VERB "promulgate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pronouncement" :VERB "pronounce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pronunciation" :VERB "pronounce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "proof" :VERB "prove" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "proofreader" :VERB "proofread" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "propagandist" :VERB "propagandize" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "propagation" :VERB "propagate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "propagator" :VERB "propagate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "propellant" :VERB "propel" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "propellent" :VERB "propel" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "propeller" :VERB "propel" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "propitiation" :VERB "propitiate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "proportion" :VERB "proportion" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "proposal" :VERB "propose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "proposer" :VERB "propose" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "proposition" :VERB "propose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "prorogation" :VERB "prorogue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "proscription" :VERB "proscribe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "prosecution" :VERB "prosecute" :NOM-TYPE ((VERB-NOM) (SUBJECT))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "prosecutor" :VERB "prosecute" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "prospector" :VERB "prospect" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "prosperity" :VERB "prosper" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "prostitution" :VERB "prostitute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "prostration" :VERB "prostrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "protection" :VERB "protect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("on" "under" "by" "from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "protectionism" :VERB "protect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "protector" :VERB "protect" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "protest" :VERB "protest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "at" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "protestation" :VERB "protest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "protester" :VERB "protest" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "protraction" :VERB "protract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "protractor" :VERB "protract" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "protrusion" :VERB "protrude" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "provider" :VERB "provide" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "provision" :VERB "provide" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "provocation" :VERB "provoke" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "prowl" :VERB "prowl" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "prowler" :VERB "prowl" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "psychoanalyst" :VERB "psychoanalyze" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "publication" :VERB "publish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "publicist" :VERB "publicize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "publisher" :VERB "publish" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "puddler" :VERB "puddle" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "pull-out" :VERB "pull" :NOM-TYPE ((VERB-PART :ADVAL ("out")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pullback" :VERB "pull" :NOM-TYPE ((VERB-PART :ADVAL ("back")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pullout" :VERB "pull" :NOM-TYPE ((VERB-PART :ADVAL ("out")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pulsation" :VERB "pulsate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "pump" :VERB "pump" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "pumping" :VERB "pump" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "punctuation" :VERB "punctuate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "punishment" :VERB "punish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "punter" :VERB "punt" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "purchase" :VERB "purchase" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "by" "for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "purchaser" :VERB "purchase" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "purgation" :VERB "purge" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "purification" :VERB "purify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "purifier" :VERB "purify" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pursuance" :VERB "pursue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "pursuer" :VERB "pursue" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pursuit" :VERB "pursue" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "purveyance" :VERB "purvey" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "purveyor" :VERB "purvey" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "push" :VERB "push" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "to" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "pusher" :VERB "push" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "putterer" :VERB "putter" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "puzzlement" :VERB "puzzle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "puzzler" :VERB "puzzle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "qualification" :VERB "qualify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "qualifier" :VERB "qualify" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "quantification" :VERB "quantify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "quarantine" :VERB "quarantine" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "quarrel" :VERB "quarrel" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("amongst" "of" "among" "between") :SOURCE NOMLEX)
	(NOM :NOUN "query" :VERB "query" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "quest" :VERB "quest" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "question" :VERB "question" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "to" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "questioner" :VERB "question" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "queue" :VERB "queue" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "quibbler" :VERB "quibble" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "quip" :VERB "quip" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "quitter" :VERB "quit" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "quotation" :VERB "quote" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "quote" :VERB "quote" :NOM-TYPE ((VERB-NOM)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "race" :VERB "race" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "with" "against" "between" "by") :SOURCE NOMLEX)
	(NOM :NOUN "racer" :VERB "race" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "radiance" :VERB "radiate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "radiation" :VERB "radiate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "radiator" :VERB "radiate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "radiopasteurization" :VERB "radiopasteurize" :NOM-TYPE
	 ((VERB-NOM)) :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rafter" :VERB "raft" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "rage" :VERB "rage" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "raid" :VERB "raid" :NOM-TYPE ((VERB-NOM)) :PREPS ("on" "of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "raider" :VERB "raid" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "raillery" :VERB "rail" :NOM-TYPE ((VERB-NOM)) :PREPS ("by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rain" :VERB "rain" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "raise" :VERB "raise" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "raiser" :VERB "raise" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "rally" :VERB "rally" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rambler" :VERB "ramble" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "ramification" :VERB "ramify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "rampage" :VERB "rampage" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rancher" :VERB "ranch" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "range" :VERB "range" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rank" :VERB "rank" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ranker" :VERB "rank" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "ransom" :VERB "ransom" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ranter" :VERB "rant" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "rap" :VERB "rap" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rape" :VERB "rape" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rapist" :VERB "rape" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "ratification" :VERB "ratify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rationalization" :VERB "rationalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "rattle" :VERB "rattle" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "rattler" :VERB "rattle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ravage" :VERB "ravage" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "raver" :VERB "rave" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "ravishment" :VERB "ravish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "razor" :VERB "raze" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "re-election" :VERB "re-elect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "re-enactment" :VERB "re-enact" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "re-entry" :VERB "re-enter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "re-formation" :VERB "re-form" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reach" :VERB "reach" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "for") :SOURCE NOMLEX)
	(NOM :NOUN "reaction" :VERB "react" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "of" "in") :SOURCE NOMLEX)
	(NOM :NOUN "reactor" :VERB "react" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "reader" :VERB "read" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "readjustment" :VERB "readjust" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "readmission" :VERB "readmit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "realignment" :VERB "realign" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "between" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "realization" :VERB "realize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of" "around") :SOURCE NOMLEX)
	(NOM :NOUN "reaper" :VERB "reap" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "reappearance" :VERB "reappear" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reappraisal" :VERB "reappraise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rearmament" :VERB "rearm" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rearrangement" :VERB "rearrange" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reasoning" :VERB "reason" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reassessment" :VERB "reassess" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reassignment" :VERB "reassign" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "reassurance" :VERB "reassure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reauthorization" :VERB "reauthorize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rebate" :VERB "rebate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rebellion" :VERB "rebel" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rebound" :VERB "rebound" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rebuff" :VERB "rebuff" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rebuttal" :VERB "rebut" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "recalculation" :VERB "recalculate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "recall" :VERB "recall" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "recantation" :VERB "recant" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "recapitalization" :VERB "recapitalize" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "recapitulation" :VERB "recapitulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "receipt" :VERB "receive" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "receiver" :VERB "receive" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reception" :VERB "receive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "receptor" :VERB "receive" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "recession" :VERB "recess" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "recipient" :VERB "receive" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reciprocation" :VERB "reciprocate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "recital" :VERB "recite" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "recitation" :VERB "recite" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reckoner" :VERB "reckon" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reckoning" :VERB "reckon" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "recognition" :VERB "recognize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("with" "of" "by" "among") :SOURCE NOMLEX)
	(NOM :NOUN "recognizance" :VERB "recognize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "recollection" :VERB "recollect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "recombination" :VERB "recombine" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "recommendation" :VERB "recommend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reconciliation" :VERB "reconcile" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "between" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reconnaissance" :VERB "reconnoiter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reconsideration" :VERB "reconsider" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reconstruction" :VERB "reconstruct" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "record" :VERB "record" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "recorder" :VERB "record" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "recording" :VERB "record" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "recovery" :VERB "recover" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "at" "of" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "recreation" :VERB "recreate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "recrimination" :VERB "recriminate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "recruit" :VERB "recruit" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "recruiter" :VERB "recruit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "recruitment" :VERB "recruit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rectification" :VERB "rectify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "rectifier" :VERB "rectify" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "recuperation" :VERB "recuperate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "recurrence" :VERB "recur" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "redaction" :VERB "redact" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "redeemer" :VERB "redeem" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "redefinition" :VERB "redefine" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "redemption" :VERB "redeem" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "redeployment" :VERB "redeploy" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "redevelopment" :VERB "redevelop" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rediscovery" :VERB "rediscover" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "redistribution" :VERB "redistribute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "redistributionism" :VERB "redistribute" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reduction" :VERB "reduce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reduplication" :VERB "reduplicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reefer" :VERB "reef" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "reevaluation" :VERB "reevaluate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "referee1" :VERB "referee" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "referee2" :VERB "refer" :NOM-TYPE ((IND-OBJ :PVAL ("to"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reference" :VERB "refer" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "referral" :VERB "refer" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "refinement" :VERB "refine" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "refiner" :VERB "refine" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "refinery" :VERB "refine" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reflation" :VERB "reflate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reflection" :VERB "reflect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reflector" :VERB "reflect" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reforestation" :VERB "reforest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reform" :VERB "reform" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reformation" :VERB "reform" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reformer" :VERB "reform" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reformist" :VERB "reform" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reformulation" :VERB "reformulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "refraction" :VERB "refract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "refresher" :VERB "refresh" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "refreshment" :VERB "refresh" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "refrigeration" :VERB "refrigerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "refrigerator" :VERB "refrigerate" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "refund" :VERB "refund" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "refurbishment" :VERB "refurbish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "refusal" :VERB "refuse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "refuse" :VERB "refuse" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "to" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "refutation" :VERB "refute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "regard" :VERB "regard" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "regeneration" :VERB "regenerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "regimentation" :VERB "regiment" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "registration" :VERB "register" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "registry" :VERB "register" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "regression" :VERB "regress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "regret" :VERB "regret" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "regularization" :VERB "regularize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "regulation" :VERB "regulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "regulator" :VERB "regulate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rehabilitation" :VERB "rehabilitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rehearsal" :VERB "rehearse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reign" :VERB "reign" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reimbursement" :VERB "reimburse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "rein" :VERB "rein" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reincarnation" :VERB "reincarnate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reinforcement" :VERB "reinforce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reinstatement" :VERB "reinstate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reinsurance" :VERB "reinsure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reinsurer" :VERB "reinsure" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reinterpretation" :VERB "reinterpret" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reinvestment" :VERB "reinvest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reiteration" :VERB "reiterate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "rejection" :VERB "reject" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rejuvenation" :VERB "rejuvenate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "relation" :VERB "relate" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "relationship" :VERB "relate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "relaxation" :VERB "relax" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "relay" :VERB "relay" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "release" :VERB "release" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "relegation" :VERB "relegate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reliance" :VERB "rely" :NOM-TYPE ((VERB-NOM)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "relief" :VERB "relieve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("about" "by" "from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "relinquishment" :VERB "relinquish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "relish" :VERB "relish" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "relocation" :VERB "relocate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "remain" :VERB "remain" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "remainder" :VERB "remain" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "remark" :VERB "remark" :NOM-TYPE ((VERB-NOM)) :PREPS ("by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "remarriage" :VERB "remarry" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "remedy" :VERB "remedy" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "remembrance" :VERB "remember" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "remilitarization" :VERB "remilitarize" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reminder" :VERB "remind" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reminiscence" :VERB "reminisce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "remission" :VERB "remit" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "remittance" :VERB "remit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "remonstrance" :VERB "remonstrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "removal" :VERB "remove" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "remover" :VERB "remove" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "remuneration" :VERB "remunerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rendering" :VERB "render" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rendezvous" :VERB "rendezvous" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by") :SOURCE NOMLEX)
	(NOM :NOUN "renegotiation" :VERB "renegotiate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "between" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "renewal" :VERB "renew" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "renovation" :VERB "renovate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "renovator" :VERB "renovate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rent" :VERB "rent" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rental" :VERB "rent" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "renter" :VERB "rent" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "renunciation" :VERB "renounce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reorganization" :VERB "reorganize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reorientation" :VERB "reorientate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rep" :VERB "repute" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "repair" :VERB "repair" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "repairer" :VERB "repair" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reparation" :VERB "repair" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "repatriation" :VERB "repatriate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "repayment" :VERB "repay" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "repeal" :VERB "repeal" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "repeat" :VERB "repeat" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "repeater" :VERB "repeat" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "repellent" :VERB "repel" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "repentance" :VERB "repent" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "repetition" :VERB "repeat" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "replacement" :VERB "replace" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "replay" :VERB "replay" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "replenishment" :VERB "replenish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reply" :VERB "reply" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "report" :VERB "report" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "from" "to") :SOURCE NOMLEX)
	(NOM :NOUN "reportage" :VERB "report" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reporter" :VERB "report" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "representation" :VERB "represent" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "representative" :VERB "represent" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "repression" :VERB "repress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reprieve" :VERB "reprieve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reprobation" :VERB "reprobate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "reproducer" :VERB "reproduce" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reproduction" :VERB "reproduce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reproval" :VERB "reprove" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "repudiation" :VERB "repudiate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "repulsion" :VERB "repel" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "towards" "toward" "of") :SOURCE NOMLEX)
	(NOM :NOUN "repurchase" :VERB "repurchase" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reputation" :VERB "repute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "request" :VERB "request" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "requirement" :VERB "require" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "under" "by") :SOURCE NOMLEX)
	(NOM :NOUN "requisition" :VERB "requisition" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "requital" :VERB "requite" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "rerun" :VERB "rerun" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rescission" :VERB "rescind" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rescue" :VERB "rescue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("through" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rescuer" :VERB "rescue" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "research" :VERB "research" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "on" "into" "of") :SOURCE NOMLEX)
	(NOM :NOUN "researcher" :VERB "research" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "resemblance" :VERB "resemble" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "resentment" :VERB "resent" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reservation" :VERB "reserve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "resettlement" :VERB "resettle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reshuffle" :VERB "reshuffle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "residence" :VERB "reside" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "resident" :VERB "reside" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "resignation" :VERB "resign" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "resistance" :VERB "resist" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "of" "to" "by" "from" "for") :SOURCE NOMLEX)
	(NOM :NOUN "resister" :VERB "resist" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "resistor" :VERB "resist" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "resolution" :VERB "resolve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "resolve" :VERB "resolve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "resonance" :VERB "resonate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "resonator" :VERB "resonate" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "respect" :VERB "respect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "around" "by") :SOURCE NOMLEX)
	(NOM :NOUN "respectability" :VERB "respect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "respecter" :VERB "respect" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "respiration" :VERB "respire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "respondent" :VERB "respond" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "response" :VERB "respond" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "of" "for") :SOURCE NOMLEX)
	(NOM :NOUN "restarter" :VERB "restart" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "restatement" :VERB "restate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "restitution" :VERB "restitute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of") :SOURCE NOMLEX)
	(NOM :NOUN "restoration" :VERB "restore" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "restorer" :VERB "restore" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "restraint" :VERB "restrain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "restriction" :VERB "restrict" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "result" :VERB "result" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "to" "from" "by" "of" "due to" "for" "in") :SOURCE NOMLEX)
	(NOM :NOUN "resumption" :VERB "resume" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "resurrection" :VERB "resurrect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "resuscitation" :VERB "resuscitate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "retailer" :VERB "retail" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "retainer" :VERB "retain" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "retaliation" :VERB "retaliate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by") :SOURCE NOMLEX)
	(NOM :NOUN "retardant" :VERB "retard" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "retardation" :VERB "retard" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "retention" :VERB "retain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rethink" :VERB "rethink" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reticulation" :VERB "reticulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "retiree" :VERB "retire" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "retirement" :VERB "retire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "retort" :VERB "retort" :NOM-TYPE ((VERB-NOM)) :PREPS ("from")
	 :SOURCE NOMLEX)
	(NOM :NOUN "retraction" :VERB "retract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "retreader" :VERB "retread" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "retreat" :VERB "retreat" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "retrenchment" :VERB "retrench" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "retrieval" :VERB "retrieve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "retriever" :VERB "retrieve" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "retrogression" :VERB "retrogress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "return" :VERB "return" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reunification" :VERB "reunify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reunion" :VERB "reunite" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "revaluation" :VERB "revaluate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "revel" :VERB "revel" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "revelation" :VERB "reveal" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reveler" :VERB "revel" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "reveller" :VERB "revel" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "revelry" :VERB "revel" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "revenge" :VERB "revenge" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reverberation" :VERB "reverberate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reverence" :VERB "revere" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reversal" :VERB "reverse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reversion" :VERB "reverse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "review" :VERB "review" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "reviewer" :VERB "review" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "reviser" :VERB "revise" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "revision" :VERB "revise" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "revitalization" :VERB "revitalize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "revival" :VERB "revive" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "revolution" :VERB "revolt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "reward" :VERB "reward" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rewrite" :VERB "rewrite" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "riddance" :VERB "rid" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ride" :VERB "ride" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rider" :VERB "ride" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "ridership" :VERB "ride" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ridicule" :VERB "ridicule" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rifle" :VERB "rifle" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rigger" :VERB "rig" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "ringer" :VERB "ring" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "riot" :VERB "riot" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rioter" :VERB "riot" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "ripple" :VERB "ripple" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "rise" :VERB "rise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "riser" :VERB "rise" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "risk" :VERB "risk" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "of" "by" "to" "in") :SOURCE NOMLEX)
	(NOM :NOUN "rival" :VERB "rival" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "rivalry" :VERB "rival" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "among" "by" "between") :SOURCE NOMLEX)
	(NOM :NOUN "riveter" :VERB "rivet" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "roar" :VERB "roar" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "roaster" :VERB "roast" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "robber" :VERB "rob" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "robbery" :VERB "rob" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rocker" :VERB "rock" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "roll" :VERB "roll" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "roll-out" :VERB "roll" :NOM-TYPE ((VERB-PART :ADVAL ("out")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "roller" :VERB "roll" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "rollover" :VERB "roll" :NOM-TYPE ((VERB-PART :ADVAL ("over")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "romper" :VERB "romp" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "roomer" :VERB "room" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "rooter" :VERB "root" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "rope" :VERB "rope" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rotation" :VERB "rotate" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rotter" :VERB "rot" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "rout" :VERB "rout" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rover" :VERB "rove" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "rower" :VERB "row" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "rubber" :VERB "rub" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "ruination" :VERB "ruin" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rule" :VERB "rule" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "under" "of") :SOURCE NOMLEX)
	(NOM :NOUN "ruler" :VERB "rule" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ruling" :VERB "rule" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ruminant" :VERB "ruminate" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "rumination" :VERB "ruminate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "rumor" :VERB "rumor" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "run-up" :VERB "run" :NOM-TYPE ((VERB-PART :ADVAL ("up"))) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "runaway" :VERB "run" :NOM-TYPE ((SUBJECT-PART :ADVAL ("away")))
	 :PREPS ("by") :SOURCE NOMLEX)
	(NOM :NOUN "runner" :VERB "run" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "rush" :VERB "rush" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "rustler" :VERB "rustle" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "sabotage" :VERB "sabotage" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sack" :VERB "sack" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sacking" :VERB "sack" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sacrifice" :VERB "sacrifice" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "saddler" :VERB "saddle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "safeguard" :VERB "safeguard" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sailing" :VERB "sail" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sailor" :VERB "sail" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sale" :VERB "sell" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "in" "through" "to" "of" "per" "at" "by") :SOURCE NOMLEX)
	(NOM :NOUN "salesman" :VERB "sell" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "salutation" :VERB "salute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "salute" :VERB "salute" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "salvage" :VERB "salvage" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "salvation" :VERB "save" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "for" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "salver" :VERB "salve" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sampler" :VERB "sample" :NOM-TYPE ((VERB-NOM) (SUBJECT)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sanctification" :VERB "sanctify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sanction" :VERB "sanction" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sandwich" :VERB "sandwich" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sapper" :VERB "sap" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "satirist" :VERB "satirize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "satisfaction" :VERB "satisfy" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "in" "of" "with" "to") :SOURCE NOMLEX)
	(NOM :NOUN "saturation" :VERB "saturate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("with" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "saunterer" :VERB "saunter" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "savagery" :VERB "savage" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "saver" :VERB "save" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "savior" :VERB "save" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "savor" :VERB "savor" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "saying" :VERB "say" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "scam" :VERB "scam" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "scanner" :VERB "scan" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "scare" :VERB "scare" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "scavenger" :VERB "scavenge" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "schedule" :VERB "schedule" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "scheme" :VERB "scheme" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "schemer" :VERB "scheme" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "scintillation" :VERB "scintillate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "scoffer" :VERB "scoff" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "scorcher" :VERB "scorch" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "score" :VERB "score" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "for") :SOURCE NOMLEX)
	(NOM :NOUN "scorer" :VERB "score" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "scourer" :VERB "scour" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "scourge" :VERB "scourge" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "scout" :VERB "scout" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "scramble" :VERB "scramble" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "among") :SOURCE NOMLEX)
	(NOM :NOUN "scrambler" :VERB "scramble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "scraper" :VERB "scrape" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "scribbler" :VERB "scribble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "scrounger" :VERB "scrounge" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "scrutiny" :VERB "scrutinize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sculler" :VERB "scull" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sculptor" :VERB "sculpt" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sculpture" :VERB "sculpt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "seal" :VERB "seal" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sealer" :VERB "seal" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "search" :VERB "search" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "searcher" :VERB "search" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "seater" :VERB "seat" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "secession" :VERB "secede" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "seclusion" :VERB "seclude" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "seconder" :VERB "second" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "secretion" :VERB "secrete" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "security" :VERB "secure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sedation" :VERB "sedate" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "seducer" :VERB "seduce" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "seduction" :VERB "seduce" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "seeker" :VERB "seek" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "seer" :VERB "see" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "segment" :VERB "segment" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "segmentation" :VERB "segment" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "segregation" :VERB "segregate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "seige" :VERB "besiege" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "seizure" :VERB "seize" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "selection" :VERB "select" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "selector" :VERB "select" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sell" :VERB "sell" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "to" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sell-off" :VERB "sell" :NOM-TYPE ((VERB-PART :ADVAL ("off")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "seller" :VERB "sell" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sender" :VERB "send" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sensation" :VERB "sense" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sense" :VERB "sense" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sensitization" :VERB "sensitize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sensor" :VERB "sense" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sentence" :VERB "sentence" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sentimentalist" :VERB "sentimentalize" :NOM-TYPE ((SUBJECT))
	 :PREPS ("of") :SOURCE NOMLEX)
	(NOM :NOUN "separation" :VERB "separate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "separator" :VERB "separate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sequester" :VERB "sequester" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sequestration" :VERB "sequestrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "servant" :VERB "serve" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "server" :VERB "serve" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "service" :VERB "serve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "of" "by" "from" "for") :SOURCE NOMLEX)
	(NOM :NOUN "setback" :VERB "set" :NOM-TYPE ((VERB-PART :ADVAL ("back")))
	 :PREPS ("from" "for" "of" "by" "in") :SOURCE NOMLEX)
	(NOM :NOUN "setter" :VERB "set" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "settlement" :VERB "settle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "by" "of" "at" "between") :SOURCE NOMLEX)
	(NOM :NOUN "settler" :VERB "settle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "setup" :VERB "set" :NOM-TYPE ((VERB-PART :ADVAL ("up"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "severance" :VERB "sever" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shake" :VERB "shake" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shake-up" :VERB "shake" :NOM-TYPE ((VERB-PART :ADVAL ("up")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "shakeout" :VERB "shake" :NOM-TYPE ((VERB-PART :ADVAL ("out")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "shaker" :VERB "shake" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "shame" :VERB "shame" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shape" :VERB "shape" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "share" :VERB "share" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "for" "by" "of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "sharpener" :VERB "sharpen" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shaver" :VERB "shave" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "shelter" :VERB "shelter" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shield" :VERB "shield" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shift" :VERB "shift" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shipment" :VERB "ship" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "shipper" :VERB "ship" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shirker" :VERB "shirk" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "shiver" :VERB "shiver" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shock" :VERB "shock" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shocker" :VERB "shock" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "shoot" :VERB "shoot" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shooter" :VERB "shoot" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "shoplifter" :VERB "shoplift" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shopper" :VERB "shop" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "shopping" :VERB "shop" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "for") :SOURCE NOMLEX)
	(NOM :NOUN "shouting" :VERB "shout" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "show" :VERB "show" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "to" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shower" :VERB "shower" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shriek" :VERB "shriek" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shrinkage" :VERB "shrink" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "shuffle" :VERB "shuffle" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shuffler" :VERB "shuffle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "shunter" :VERB "shunt" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "shutdown" :VERB "shut" :NOM-TYPE ((VERB-PART :ADVAL ("down")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "shutter" :VERB "shut" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "shuttle" :VERB "shuttle" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sifter" :VERB "sift" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sigh" :VERB "sigh" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sight" :VERB "sight" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sighting" :VERB "sight" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sign" :VERB "sign" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "signal" :VERB "signal" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "signaller" :VERB "signal" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "signature" :VERB "sign" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "significance" :VERB "signify" :NOM-TYPE ((OBJECT)) :PREPS
	 ("in" "for" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "signification" :VERB "signify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "signor" :VERB "sign" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "silencer" :VERB "silence" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "simplification" :VERB "simplify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "simulation" :VERB "simulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "simulator" :VERB "simulate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sin" :VERB "sin" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "singer" :VERB "sing" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sinker" :VERB "sink" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sinking" :VERB "sink" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sinner" :VERB "sin" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "sitter" :VERB "sit" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "situation" :VERB "situate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "skater" :VERB "skate" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sketch" :VERB "sketch" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sketcher" :VERB "sketch" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "skewer" :VERB "skew" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "skid" :VERB "skid" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by" "on")
	 :SOURCE NOMLEX)
	(NOM :NOUN "skier" :VERB "ski" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "skimmer" :VERB "skim" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "skipper" :VERB ("skipper" "skip") :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "skirmish" :VERB "skirmish" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "skirmisher" :VERB "skirmish" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "skulker" :VERB "skulk" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "slacker" :VERB "slack" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "slanderer" :VERB "slander" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "slap" :VERB "slap" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "slash" :VERB "slash" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "slaughter" :VERB "slaughter" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "slaughterer" :VERB "slaughter" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "slaver" :VERB "enslave" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "slavery" :VERB "enslave" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "slayer" :VERB "slay" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sleep" :VERB "sleep" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sleeper" :VERB "sleep" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "slide" :VERB "slide" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "slinger" :VERB "sling" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "slip" :VERB "slip" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by" "in") :SOURCE NOMLEX)
	(NOM :NOUN "slippage" :VERB "slip" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by" "in") :SOURCE NOMLEX)
	(NOM :NOUN "slogger" :VERB "slog" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "slot" :VERB "slot" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "slowdown" :VERB "slow" :NOM-TYPE ((VERB-PART :ADVAL ("down")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "slumberer" :VERB "slumber" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "slump" :VERB "slump" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "slur" :VERB "slur" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "smasher" :VERB "smash" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "smelter" :VERB "smelt" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "smile" :VERB "smile" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "smoke" :VERB "smoke" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "smoker" :VERB "smoke" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "smuggler" :VERB "smuggle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "snag" :VERB "snag" :NOM-TYPE ((SUBJECT)) :PREPS ("over" "in" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "snatcher" :VERB "snatch" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "snicker" :VERB "snicker" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sniper" :VERB "snipe" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sniveler" :VERB "snivel" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "sniveller" :VERB "snivel" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "snooper" :VERB "snoop" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "snorer" :VERB "snore" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "snorter" :VERB "snort" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "soaker" :VERB "soak" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "socialization" :VERB "socialize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "softener" :VERB "soften" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sojourner" :VERB "sojourn" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "solace" :VERB "solace" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "soldiery" :VERB "soldier" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "solemnization" :VERB "solemnize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "solicitation" :VERB "solicit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "solicitor" :VERB "solicit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "solidification" :VERB "solidify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "soloist" :VERB "solo" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "solution" :VERB "solve" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of" "to" "for") :SOURCE NOMLEX)
	(NOM :NOUN "solvent" :VERB "dissolve" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sorter" :VERB "sort" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sound" :VERB "sound" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sounding" :VERB "sound" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sower" :VERB "sow" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "span" :VERB "span" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spanking" :VERB "spank" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spanner" :VERB "span" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sparkle" :VERB "sparkle" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sparkler" :VERB "sparkle" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "spasm" :VERB "spasm" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spatter" :VERB "spatter" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "speaker" :VERB "speak" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "spearhead" :VERB "spearhead" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "specialism" :VERB "specialize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "specialist" :VERB "specialize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "specialization" :VERB "specialize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "specialty" :VERB "specialize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "specification" :VERB "specify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "speculation" :VERB "speculate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "among") :SOURCE NOMLEX)
	(NOM :NOUN "speculator" :VERB "speculate" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "speech" :VERB "speak" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "speedup" :VERB "speed" :NOM-TYPE ((VERB-PART :ADVAL ("up")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "speller" :VERB "spell" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "spender" :VERB "spend" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "spice" :VERB "spice" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spill" :VERB "spill" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spillage" :VERB "spill" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spin-drier" :VERB "spin-dry" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spin-off" :VERB "spin" :NOM-TYPE ((VERB-PART)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spinner" :VERB "spin" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spinoff" :VERB "spin" :NOM-TYPE ((VERB-PART)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spinout" :VERB "spin" :NOM-TYPE ((VERB-PART)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spiral" :VERB "spiral" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spiritualization" :VERB "spiritualize" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "splicer" :VERB "splice" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "split" :VERB "split" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "sponger" :VERB "sponge off" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sponsor" :VERB "sponsor" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sponsorship" :VERB "sponsor" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "spotter" :VERB "spot" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sprayer" :VERB "spray" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "spread" :VERB "spread" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spreader" :VERB "spread" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sprinkler" :VERB "sprinkle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sprinter" :VERB "sprint" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spur" :VERB "spur" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "spurt" :VERB "spurt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "spy" :VERB "spy" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "squabble" :VERB "squabble" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "squatter" :VERB "squat" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "squawker" :VERB "squawk" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "squeaker" :VERB "squeak" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "squealer" :VERB "squeal" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "squeeze" :VERB "squeeze" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "squeezer" :VERB "squeeze" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stabber" :VERB "stab" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "stabilization" :VERB "stabilize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "stabilizer" :VERB "stabilize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "staffer" :VERB "staff" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "stager" :VERB "stage" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "staggerer" :VERB "stagger" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stagnation" :VERB "stagnate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "stalker" :VERB "stalk" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "stammerer" :VERB "stammer" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stampede" :VERB "stampede" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "standardization" :VERB "standardize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "stapler" :VERB "staple" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "star" :VERB "star" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "start" :VERB "start" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "start-up" :VERB "start" :NOM-TYPE ((VERB-PART :ADVAL ("up")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "starter" :VERB "start" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "starvation" :VERB "starve" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "statement" :VERB "state" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "station" :VERB "station" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stay" :VERB "stay" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stayer" :VERB "stay" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "steamer" :VERB "steam" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "steerage" :VERB "steer" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "step" :VERB "step" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stereotype" :VERB "stereotype" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sterilization" :VERB "sterilize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sticker" :VERB "stick" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "stiffener" :VERB "stiffen" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stimulant" :VERB "stimulate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stimulation" :VERB "stimulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "stimulator" :VERB "stimulate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stinger" :VERB "sting" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "stink" :VERB "stink" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stinker" :VERB "stink" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "stipulation" :VERB "stipulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "stockist" :VERB "stock" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stockpile" :VERB "stockpile" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "stoker" :VERB "stoke" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "stomach" :VERB "stomach" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stonewaller" :VERB "stonewall" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stooge" :VERB "stooge" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stop" :VERB "stop" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "to" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "stoppage" :VERB "stop" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "stopper" :VERB "stop" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "storage" :VERB "store" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "store" :VERB "store" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "storing" :VERB "store" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "straggler" :VERB "straggle" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "strainer" :VERB "strain" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stratification" :VERB "stratify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "stress" :VERB "stress" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stressor" :VERB "stress" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stretch" :VERB "stretch" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stretcher" :VERB "stretch" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stridulation" :VERB "stridulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "strike" :VERB "strike" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "striker" :VERB "strike" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stripper" :VERB "strip" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "striver" :VERB "strive" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "stroke" :VERB "stroke" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stroller" :VERB "stroll" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "struggle" :VERB "struggle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "by" "of" "among") :SOURCE NOMLEX)
	(NOM :NOUN "student" :VERB "study" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "study" :VERB "study" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stultification" :VERB "stultify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "stumble" :VERB "stumble" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stumper" :VERB "stump" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "stunner" :VERB "stun" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "stutterer" :VERB "stutter" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "stylist" :VERB "stylize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "stylization" :VERB "stylize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "subcontractor" :VERB "subcontract" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of" "for") :SOURCE NOMLEX)
	(NOM :NOUN "subdivision" :VERB "subdivide" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "subeditor" :VERB "subedit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "subject" :VERB "subject" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "subjection" :VERB "subject" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "subjugation" :VERB "subjugate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sublimation" :VERB "sublimate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "submergence" :VERB "submerge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "submission" :VERB "submit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "subordinate" :VERB "subordinate" :NOM-TYPE ((OBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "subordination" :VERB "subordinate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "subornation" :VERB "suborn" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "subpoena" :VERB "subpoena" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "subscriber" :VERB "subscribe" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "subscription" :VERB "subscribe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "subsidence" :VERB "subside" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "subsidization" :VERB "subsidize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "subsidy" :VERB "subsidize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("through" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "subsistence" :VERB "subsist" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "substantiation" :VERB "substantiate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "substitute" :VERB "substitute" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "substitution" :VERB "substitute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "subtraction" :VERB "subtract" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "subversion" :VERB "subvert" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "success" :VERB "succeed" :NOM-TYPE ((VERB-NOM) (SUBJECT)) :PREPS
	 ("with" "of" "for") :SOURCE NOMLEX)
	(NOM :NOUN "succession" :VERB "succeed" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "successor" :VERB "succeed" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sucker" :VERB "suck" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sufferance" :VERB "suffer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "sufferer" :VERB "suffer" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "suffocation" :VERB "suffocate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "suffusion" :VERB "suffuse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sugar" :VERB "sugar" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "suggestion" :VERB "suggest" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "suit" :VERB "sue" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "to" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "suitor" :VERB "suit" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "summary" :VERB "summarize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "summation" :VERB "sum" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "superannuation" :VERB "superannuate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "superintendence" :VERB "superintend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "superintendent" :VERB "superintend" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "supersession" :VERB "supersede" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "supervision" :VERB "supervise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "supervisor" :VERB "supervise" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "supper" :VERB "sup" :NOM-TYPE ((P-OBJ :PVAL ("on"))) :PREPS ("by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "supplanter" :VERB "supplant" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "supplement" :VERB "supplement" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "suppliant" :VERB "supplicate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "supplicant" :VERB "supplicate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "supplication" :VERB "supplicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "supplier" :VERB "supply" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "supply" :VERB "supply" :NOM-TYPE ((VERB-NOM)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "support" :VERB "support" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "from" "of" "on" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "supporter" :VERB "support" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "supposition" :VERB "suppose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "suppressant" :VERB "suppress" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "suppression" :VERB "suppress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "suppressor" :VERB "suppress" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "suppuration" :VERB "suppurate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "surge" :VERB "surge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "surprise" :VERB "surprise" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "surrender" :VERB "surrender" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "surtax" :VERB "surtax" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "surveillance" :VERB "survey" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "survey" :VERB "survey" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "surveyor" :VERB "survey" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "survival" :VERB "survive" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "survivor" :VERB "survive" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "suspect" :VERB "suspect" :NOM-TYPE ((OBJECT)) :PREPS
	 ("by" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "suspender" :VERB "suspend" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "suspense" :VERB "suspend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "to" "of") :SOURCE NOMLEX)
	(NOM :NOUN "suspension" :VERB "suspend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "suspicion" :VERB "suspect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "over" "in regard to" "with respect to" "regarding" "about"
	  "concerning" "with regard to" "in connection with" "on" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sustenance" :VERB "sustain" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "swaggerer" :VERB "swagger" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "swap" :VERB "swap" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "of" "amongst" "among" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "swat" :VERB "swat" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sway" :VERB "sway" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "swearer" :VERB "swear" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "swearing" :VERB "swear" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "swearing-in" :VERB "swear" :NOM-TYPE ((VERB-PART :ADVAL ("in")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "sweat" :VERB "sweat" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "sweep" :VERB "sweep" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "sweeper" :VERB "sweep" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "sweetener" :VERB "sweeten" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "swell" :VERB "swell" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "swig" :VERB "swig" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "swimmer" :VERB "swim" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "swindler" :VERB "swindle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "swing" :VERB "swing" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "in") :SOURCE NOMLEX)
	(NOM :NOUN "swirl" :VERB "swirl" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "switch" :VERB "switch" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "switcher" :VERB "switch" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "swoon" :VERB "swoon" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "syllabication" :VERB "syllabicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "syllabification" :VERB "syllabify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "symbol" :VERB "symbolize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "symbolism" :VERB "symbolize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "symbolization" :VERB "symbolize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "sympathizer" :VERB "sympathize" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "sympathy" :VERB "sympathize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "sync" :VERB "synchronize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "synchronization" :VERB "synchronize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "syncopation" :VERB "syncopate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "syndication" :VERB "syndicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "syndicator" :VERB "syndicate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "synthesis" :VERB "synthesize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "synthesizer" :VERB "synthesize" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "systematization" :VERB "systematize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "table" :VERB "reverse" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tabulation" :VERB "tabulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "tabulator" :VERB "tabulate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tag" :VERB "tag" :NOM-TYPE ((VERB-NOM)) :PREPS ("for" "of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tailor" :VERB "tailor" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "taint" :VERB "taint" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "take-off" :VERB "take" :NOM-TYPE ((VERB-PART)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "take-out" :VERB "take" :NOM-TYPE ((VERB-PART)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "takeoff" :VERB "take" :NOM-TYPE ((VERB-PART)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "takeout" :VERB "take" :NOM-TYPE ((VERB-PART)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "takeover" :VERB "take" :NOM-TYPE ((VERB-PART :ADVAL ("over")))
	 :PREPS ("of" "by" "from" "for") :SOURCE NOMLEX)
	(NOM :NOUN "taker" :VERB "take" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "talk" :VERB "talk" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "by" "with" "from") :SOURCE NOMLEX)
	(NOM :NOUN "talker" :VERB "talk" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tally" :VERB "tally" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "tamer" :VERB "tame" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tanner" :VERB "tan" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tape" :VERB "tape" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "taper" :VERB "tape" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "target" :VERB "target" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("for" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "taste" :VERB "taste" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "taster" :VERB "taste" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tattler" :VERB "tattle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tax" :VERB "tax" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "on" "of" "for" "to") :SOURCE NOMLEX)
	(NOM :NOUN "taxation" :VERB "tax" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "on" "by") :SOURCE NOMLEX)
	(NOM :NOUN "teacher" :VERB "teach" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "teaser" :VERB "tease" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "telecast" :VERB "telecast" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "telegraph" :VERB "telegraph" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "telegrapher" :VERB "telegraph" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "telegraphist" :VERB "telegraph" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "temperance" :VERB "temper" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "temptation" :VERB "tempt" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "tempter" :VERB "tempt" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tendency" :VERB "tend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "among") :SOURCE NOMLEX)
	(NOM :NOUN "tender" :VERB "tend" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tension" :VERB "tense" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "termination" :VERB "terminate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "terrorism" :VERB "terrorize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "terrorist" :VERB "terrorize" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "test" :VERB "test" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "testament" :VERB "testify" :NOM-TYPE ((VERB-NOM)) :PREPS ("by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "testimonial" :VERB "testify" :NOM-TYPE ((VERB-NOM)) :PREPS ("by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "testimony" :VERB "testify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "thank" :VERB "thank" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "thatcher" :VERB "thatch" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "thaw" :VERB "thaw" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "theft" :VERB "thieve" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "theorist" :VERB "theorize" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "thief" :VERB "thieve" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "thievery" :VERB "thieve" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "thinker" :VERB "think" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "thinking" :VERB "think" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "thirst" :VERB "thirst" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "thought" :VERB "think" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "threat" :VERB "threaten" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "against" "by" "from") :SOURCE NOMLEX)
	(NOM :NOUN "thrill" :VERB "thrill" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "thriller" :VERB "thrill" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "thrower" :VERB "throw" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "thrust" :VERB "thrust" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "thruster" :VERB "thrust" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "ticker" :VERB "tick" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tickler" :VERB "tickle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tie" :VERB "tie" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "among" "amongst" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "tie-up" :VERB "tie" :NOM-TYPE ((VERB-PART :ADVAL ("up"))) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "tightener" :VERB "tighten" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tiller" :VERB "till" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tilt" :VERB "tilt" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tilth" :VERB "till" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "timer" :VERB "time" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tinge" :VERB "tinge" :NOM-TYPE ((VERB-NOM)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tip" :VERB "tip" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tip-off" :VERB "tip" :NOM-TYPE ((VERB-PART :ADVAL ("off"))) :PREPS
	 ("of" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "tipper" :VERB "tip" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tippler" :VERB "tipple" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tipster" :VERB "tip" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "titillation" :VERB "titillate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "toast" :VERB "toast" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "toaster" :VERB "toast" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "toiler" :VERB "toil" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "tolerance" :VERB "tolerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for") :SOURCE NOMLEX)
	(NOM :NOUN "toleration" :VERB "tolerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "toner" :VERB "tone" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "topper" :VERB "top" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tormentor" :VERB "torment" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "torturer" :VERB "torture" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tosser" :VERB "toss" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "total" :VERB "total" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "touch" :VERB "touch" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tour" :VERB "tour" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tourism" :VERB "tour" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tourist" :VERB "tour" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tracer" :VERB "trace" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tracker" :VERB "track" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "trade" :VERB "trade" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "trade-in" :VERB "trade" :NOM-TYPE ((OBJECT-PART :ADVAL ("in")))
	 :PREPS ("by") :SOURCE NOMLEX)
	(NOM :NOUN "trademark" :VERB "trademark" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "trader" :VERB "trade" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "traducer" :VERB "traduce" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "traffic" :VERB "traffic" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "trafficker" :VERB "traffic" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "trailer" :VERB "trail" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "trainee" :VERB "train" :NOM-TYPE ((OBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "trainer" :VERB "train" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tranquillizer" :VERB "tranquillize" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "transaction" :VERB "transact" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "of" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transcendence" :VERB "transcend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transcript" :VERB "transcribe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transcription" :VERB "transcribe" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transfer" :VERB "transfer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("between" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transferee" :VERB "transfer" :NOM-TYPE
	 ((IND-OBJ :PVAL ("between" "from" "off" "into" "to"))) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "transference" :VERB "transfer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transfiguration" :VERB "transfigure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transformation" :VERB "transform" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transformer" :VERB "transform" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "transfusion" :VERB "transfuse" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "transgression" :VERB "transgress" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "transgressor" :VERB "transgress" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "translation" :VERB "translate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "translator" :VERB "translate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "transliteration" :VERB "transliterate" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transmission" :VERB "transmit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transmitter" :VERB "transmit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "transmogrification" :VERB "transmogrify" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "transmutation" :VERB "transmute" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "transpiration" :VERB "transpire" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transplant" :VERB "transplant" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transplantation" :VERB "transplant" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "transport" :VERB "transport" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "transportation" :VERB "transport" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "transporter" :VERB "transport" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "transposition" :VERB "transpose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "trap" :VERB "trap" :NOM-TYPE ((INSTRUMENT)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "trapper" :VERB "trap" :NOM-TYPE ((SUBJECT)) :PREPS ("of" "to")
	 :SOURCE NOMLEX)
	(NOM :NOUN "trauma" :VERB "traumatize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "travel" :VERB "travel" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "for") :SOURCE NOMLEX)
	(NOM :NOUN "traveler" :VERB "travel" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "traveller" :VERB "travel" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "treasure" :VERB "treasure" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "treatment" :VERB "treat" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("at" "of" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "trencher" :VERB "trench" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "trend" :VERB "trend" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "among" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "trespass" :VERB "trespass" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "against") :SOURCE NOMLEX)
	(NOM :NOUN "trespasser" :VERB "trespass" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "trial" :VERB "try" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("to" "by" "of" "before") :SOURCE NOMLEX)
	(NOM :NOUN "trick" :VERB "trick" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "trickery" :VERB "trick" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "trier" :VERB "try" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "trifler" :VERB "trifle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "trigger" :VERB "trigger" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "trimmer" :VERB "trim" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tripper" :VERB "trip" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "triumph" :VERB "triumph" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "trotter" :VERB "trot" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "trouble" :VERB "trouble" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "in" "on" "over" "with" "of") :SOURCE NOMLEX)
	(NOM :NOUN "trudge" :VERB "trudge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "for") :SOURCE NOMLEX)
	(NOM :NOUN "trumpeter" :VERB "trumpet" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "trust" :VERB "trust" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "trustee" :VERB "entrust" :NOM-TYPE ((IND-OBJ :PVAL ("to"))) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "try" :VERB "try" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tryout" :VERB "try" :NOM-TYPE ((VERB-PART :ADVAL ("out"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "tucker" :VERB "tuck" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "tumble" :VERB "tumble" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tumbler" :VERB "tumble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "tuner" :VERB "tune" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "turn" :VERB "turn" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "turn-on" :VERB "turn" :NOM-TYPE ((VERB-PART :ADVAL ("on"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "turnaround" :VERB "turn" :NOM-TYPE ((VERB-PART :ADVAL ("around")))
	 :PREPS ("for" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "turner" :VERB "turn" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "twist" :VERB "twist" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "twister" :VERB "twist" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "typist" :VERB "type" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "ulceration" :VERB "ulcerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "ululation" :VERB "ululate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "underestimation" :VERB "underestimate" :NOM-TYPE ((VERB-NOM))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "underexposure" :VERB "underexpose" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "underpayment" :VERB "underpay" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "understatement" :VERB "understate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "undertaker" :VERB "undertake" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "undervaluation" :VERB "undervalue" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "underwriter" :VERB "underwrite" :NOM-TYPE ((SUBJECT)) :PREPS
	 ("of") :SOURCE NOMLEX)
	(NOM :NOUN "undulation" :VERB "undulate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "unification" :VERB "unify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "unifier" :VERB "unify" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "unity" :VERB "unite" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "among" "between" "of") :SOURCE NOMLEX)
	(NOM :NOUN "unsettlement" :VERB "unsettle" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "update" :VERB "update" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "upgrade" :VERB "upgrade" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "upholsterer" :VERB "upholster" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "uprising" :VERB "rise" :NOM-TYPE ((VERB-PART :ADVAL ("up")))
	 :PREPS ("by") :SOURCE NOMLEX)
	(NOM :NOUN "upset" :VERB "upset" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "urbanization" :VERB "urbanize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "urge" :VERB "urge" :NOM-TYPE ((VERB-NOM)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "urging" :VERB "urge" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "usage" :VERB "use" :NOM-TYPE ((VERB-NOM)) :PREPS ("for" "of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "use" :VERB "use" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by") :SOURCE
	 NOMLEX)
	(NOM :NOUN "user" :VERB "use" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "usurpation" :VERB "usurp" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "usurper" :VERB "usurp" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "utilization" :VERB "utilize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "utterance" :VERB "utter" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "vacation" :VERB "vacation" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "for") :SOURCE NOMLEX)
	(NOM :NOUN "vacationer" :VERB "vacation" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "vacationist" :VERB "vacation" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "vaccination" :VERB "vaccinate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "vacillation" :VERB "vacillate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "valuation" :VERB "value" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "for" "by") :SOURCE NOMLEX)
	(NOM :NOUN "valuer" :VERB "value" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "valve" :VERB "control" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "vaporization" :VERB "vaporize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "variance" :VERB "vary" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "variant" :VERB "vary" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "variation" :VERB "vary" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "variety" :VERB "vary" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "vaulter" :VERB "vault" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "vaunter" :VERB "vaunt" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "vegetation" :VERB "vegetate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "vendee" :VERB "vend" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "vender" :VERB "vend" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "vendor" :VERB "vend" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "veneration" :VERB "venerate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "vengeance" :VERB "revenge" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "ventilation" :VERB "ventilate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "ventilator" :VERB "ventilate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "venture" :VERB "venture" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "verge" :VERB "verge" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "verification" :VERB "verify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "vestment" :VERB "vest" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "veto" :VERB "veto" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "vexation" :VERB "vex" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "vibration" :VERB "vibrate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "vibrator" :VERB "vibrate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "victimization" :VERB "victimize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "view" :VERB "view" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "on" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "viewer" :VERB "view" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "vilification" :VERB "vilify" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "vindication" :VERB "vindicate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "violation" :VERB "violate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("on" "of" "in" "by") :SOURCE NOMLEX)
	(NOM :NOUN "violator" :VERB "violate" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "visit" :VERB "visit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "to" "by" "between") :SOURCE NOMLEX)
	(NOM :NOUN "visitant" :VERB "visit" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "visitation" :VERB "visit" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "visitor" :VERB "visit" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "visualization" :VERB "visualize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "vituperation" :VERB "vituperate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "vivisection" :VERB "vivisect" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "vociferation" :VERB "vociferate" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "voice" :VERB "voice" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "volley" :VERB "volley" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "volunteer" :VERB "volunteer" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "volunteerism" :VERB "volunteer" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "vote" :VERB "vote" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("in" "by" "for" "of" "from") :SOURCE NOMLEX)
	(NOM :NOUN "voter" :VERB "vote" :NOM-TYPE ((SUBJECT)) :PREPS ("for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "voucher" :VERB "vouch" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "vow" :VERB "vow" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "voyage" :VERB "voyage" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "voyager" :VERB "voyage" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "vulcanization" :VERB "vulcanize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "vulgarization" :VERB "vulgarize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "wader" :VERB "wade" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "waffle" :VERB "waffle" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "wager" :VERB "wager" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "amongst" "between" "among" "of") :SOURCE NOMLEX)
	(NOM :NOUN "waiter" :VERB "wait" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "waiver" :VERB "waive" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "walk" :VERB "walk" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "walker" :VERB "walk" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "walkout" :VERB "walk" :NOM-TYPE ((VERB-PART :ADVAL ("out")))
	 :PREPS ("by") :SOURCE NOMLEX)
	(NOM :NOUN "wanderer" :VERB "wander" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "war" :VERB "war" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "among" "by" "between" "with") :SOURCE NOMLEX)
	(NOM :NOUN "warbler" :VERB "warble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "warder" :VERB "ward" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "warfare" :VERB "war" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "among" "amongst" "by" "between") :SOURCE NOMLEX)
	(NOM :NOUN "warm-up" :VERB "warm" :NOM-TYPE ((VERB-PART :ADVAL ("up"))) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "warmer" :VERB "warm" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "warning" :VERB "warn" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "by" "for" "of" "to") :SOURCE NOMLEX)
	(NOM :NOUN "warrant" :VERB "warrant" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by" "to") :SOURCE NOMLEX)
	(NOM :NOUN "warrantee" :VERB "warrant" :NOM-TYPE ((OBJECT)) :PREPS
	 ("from" "by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "warrantor" :VERB "warrant" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "warranty" :VERB "warrant" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "wash" :VERB "wash" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "washer" :VERB "wash" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "wastage" :VERB "waste" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "waste" :VERB "waste" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("among" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "waster" :VERB "waste" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "watch" :VERB "watch" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "watcher" :VERB "watch" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "waverer" :VERB "waver" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "wearer" :VERB "wear" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "weaver" :VERB "weave" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "wedding" :VERB "wed" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "amongst" "between" "among" "by") :SOURCE NOMLEX)
	(NOM :NOUN "welcome" :VERB "welcome" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "welder" :VERB "weld" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "well-wisher" :VERB "wish" :NOM-TYPE ((SUBJECT)) :PREPS ("to" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "welsher" :VERB "welsh" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "westernization" :VERB "westernize" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "whacker" :VERB "whack" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "whaler" :VERB "whale" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "whimper" :VERB "whimper" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "whiner" :VERB "whine" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "whipping" :VERB "whip" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "whisker" :VERB "whisk" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "whisper" :VERB "whisper" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "whisperer" :VERB "whisper" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "whitewash" :VERB "whitewash" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "will" :VERB "will" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "for" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "win" :VERB "win" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "for" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "winner" :VERB "win" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "wiper" :VERB "wipe" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "wiretap" :VERB "wiretap" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "wish" :VERB "wish" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "to" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "withdrawal" :VERB "withdraw" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "witness" :VERB "witness" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "wobbler" :VERB "wobble" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "womanizer" :VERB "womanize" :NOM-TYPE ((SUBJECT)) :PREPS NIL
	 :SOURCE NOMLEX)
	(NOM :NOUN "wonder" :VERB "wonder" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "wonderment" :VERB "wonder" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "wooer" :VERB "woo" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "work" :VERB "work" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("on" "of" "by" "with" "to") :SOURCE NOMLEX)
	(NOM :NOUN "worker" :VERB "work" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "workout" :VERB "work" :NOM-TYPE ((VERB-PART :ADVAL ("out")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "worry" :VERB "worry" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("of" "to" "by") :SOURCE NOMLEX)
	(NOM :NOUN "worship" :VERB "worship" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "worshipper" :VERB "worship" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "worth" :VERB "worth" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "wound" :VERB "wound" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("from" "of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "wrapper" :VERB "wrap" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "wreck" :VERB "wreck" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "wreckage" :VERB "wreck" :NOM-TYPE ((VERB-NOM)) :PREPS ("of" "by")
	 :SOURCE NOMLEX)
	(NOM :NOUN "wrecker" :VERB "wreck" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "wrestler" :VERB "wrestle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "wriggler" :VERB "wriggle" :NOM-TYPE ((SUBJECT)) :PREPS ("of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "wringer" :VERB "wring" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "write-down" :VERB "write" :NOM-TYPE ((VERB-PART :ADVAL ("down")))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "write-off" :VERB "write" :NOM-TYPE ((VERB-PART :ADVAL ("off")))
	 :PREPS ("of" "by") :SOURCE NOMLEX)
	(NOM :NOUN "writedown" :VERB "write" :NOM-TYPE ((VERB-PART :ADVAL ("down")))
	 :PREPS ("by" "of") :SOURCE NOMLEX)
	(NOM :NOUN "writer" :VERB "write" :NOM-TYPE ((SUBJECT)) :PREPS ("of") :SOURCE
	 NOMLEX)
	(NOM :NOUN "wrongdoing" :VERB "do" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("by" "for" "of") :SOURCE NOMLEX)
	(NOM :NOUN "yearning" :VERB "yearn" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "yield" :VERB "yield" :NOM-TYPE ((VERB-NOM)) :PREPS
	 ("on" "in" "between" "to" "of" "for" "from" "by") :SOURCE NOMLEX)
	(NOM :NOUN "yodeler" :VERB "yodel" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "yodeller" :VERB "yodel" :NOM-TYPE ((SUBJECT)) :PREPS NIL :SOURCE
	 NOMLEX)
	(NOM :NOUN "zigzag" :VERB "zigzag" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX)
	(NOM :NOUN "zone" :VERB "zone" :NOM-TYPE ((VERB-NOM)) :PREPS ("by" "of")
	 :SOURCE NOMLEX))
      )
