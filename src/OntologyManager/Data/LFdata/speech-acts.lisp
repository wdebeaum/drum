(in-package :om)

(define-type ONT::SPEECH-ACT :parent ONT::ANY-SEM
	     :arguments ((:required content))
	     )

(define-type ONT::SA_EVALUATION :parent ONT::SPEECH-ACT)

(define-type ONT::SA_GREET :parent ONT::SPEECH-ACT)

(define-type ONT::SA_CLOSE :parent ONT::SPEECH-ACT)

(define-type ONT::SA_REJECT :parent ONT::SPEECH-ACT)

(define-type ONT::SA_APOLOGIZE :parent ONT::SPEECH-ACT)

(define-type ONT::SA_CONFIRM :parent ONT::SPEECH-ACT)

(define-type ONT::SA_THANK :parent ONT::SPEECH-ACT)

(define-type ONT::SA_WELCOME :parent ONT::SPEECH-ACT)

(define-type ONT::SA_DISCOURSE-MANAGE :parent ONT::SPEECH-ACT)

(define-type ONT::SA_GO-ONLINE :parent ONT::SPEECH-ACT)

(define-type ONT::SA_GO-OFFLINE :parent ONT::SPEECH-ACT)

(define-type ONT::SA_ACK :parent ONT::SPEECH-ACT)

(define-type ONT::SA_EXPRESSIVE :parent ONT::SPEECH-ACT)

(define-type ONT::SA_NOLO-COMPRENDEZ :parent ONT::SPEECH-ACT)

(define-type ONT::SA_DISCOURSE-SIGNAL :parent ONT::SPEECH-ACT)

(define-type ONT::SA_TELL :parent ONT::SPEECH-ACT)

(define-type ONT::GOODBYE :parent ONT::SPEECH-ACT)

(define-type ONT::SA_SUGGEST :parent ONT::SPEECH-ACT)

(define-type ONT::SA_REQUEST :parent ONT::SPEECH-ACT)

(define-type ONT::SA_QUESTION :parent ONT::SPEECH-ACT)

(define-type ONT::SA_YN-QUESTION :parent ONT::SA_QUESTION)

(define-type ONT::SA_TAG-QUESTION :parent ONT::SA_QUESTION)

(define-type ONT::SA_WHAT-IF-QUESTION :parent ONT::SA_QUESTION)

(define-type ONT::SA_WHY-QUESTION :parent ONT::SA_QUESTION)

(define-type ONT::SA_HOW-QUESTION :parent ONT::SA_QUESTION)

(define-type ONT::SA_WH-QUESTION :parent ONT::SA_QUESTION)

(define-type ONT::SA_QUERY :parent ONT::SA_QUESTION) ;; fragments like what?

(define-type ont::definiteness :parent ont::any-sem)

(define-type ont::definiteQ :parent ont::definiteness)

(define-type ont::definite :parent ont::definiteQ)

(define-type ont::indefiniteQ :parent ont::definiteness)

(define-type ont::indefinite :parent ont::indefiniteQ)

(define-type ont::the :parent ont::definiteQ)

(define-type ont::the-set :parent ont::definiteQ)

(define-type ont::pro :parent ont::definiteQ)

(define-type ont::pro-set :parent ont::definiteQ)

(define-type ont::a :parent ont::indefiniteQ)

(define-type ont::indef-set :parent ont::indefiniteQ)

(define-type ont::sm :parent ont::indefiniteQ)

(define-type ont::bare :parent ont::indefiniteQ)

(define-type ont::quantifier :parent ont::indefiniteQ)

(define-type ont::wh-term :parent ont::definiteQ)

(define-type ont::wh :parent ont::definiteQ)

(define-type ont::implicitQ :parent ont::definiteness)

(define-type ont::impro :parent ont::implicitQ)

(define-type ont::impro-set :parent ont::implicitQ)








