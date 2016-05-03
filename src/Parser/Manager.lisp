;;
;; The Top level code for managing interpretation


(in-package :IM)

(defun handle-message (SA)
  "Top level content handling"
  (case (car SA)
    (new-speech-act
     ;; Receiving new speech act from parser, call reference
     (send-with-continuation
      `(request :receiver REFERENCE
		:content (resolve-utterance ,SA))
      #'(lambda (c)
	  (continue-processing-speech-act SA c))))
    (otherwise
     (im-warn "Uninterpretable message context ~S:" SA)))
  (values))

(defun continue-processing-speech-act (SA results)
  "Continue interpreting new speech act, now with reference info"
  (let* ((newSA (install-reference-results SA results))
	 (CPSAct (interp-LF newSA)))
    (im-trace-log "CPSA Act is ~S"CPSAct)))

(defun extract-LFS-and-install-reference-results (SA results)
  ;; We extract the LF terms from the SA, substituting referential terms when possible
  ;;  current code just takes the best interpretation
  (let ((ref-results (remove-if #'null
				(mapcar #'(lambda (result)
					    (if (eq (car result) 'success)
						(cons (find-arg-in-act result :var)
						      (find-arg-in-act (car (find-arg-in-act result :VALUES)) :SPEC))))
			     (third results)))))
    (mapcar #'(lambda (x)
		(let*
		    ((lf (find-arg-in-act x :lf))
		     (var (second lf)))
		  (or (cdr (assoc var ref-results)) lf)))
	    LFs)))
		  
    
  


(defun im-warn (&rest args)
  (apply #'format (cons t args)))
  
  
