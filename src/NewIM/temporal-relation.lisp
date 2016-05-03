;; 
;; Compute temporal relation for events and states
;;

(in-package :IM)

;;
;; temporal relation is computed using Markov Logic Network by module "MLNSolver"
;;
(defun compute-temporal-relation-for-extractions (rec)
  (if (consp (utt-record-extractions rec))
      (let* ((context (make-lf-list rec))
	     (extractions (build-extractions (utt-record-extractions rec)  context))
	     (reduced-ex (elim-dups extractions))
	     (results (send-and-wait `(request :content 
					       (compute-temporal-relation
						:value ,reduced-ex
						:uttnum ,(utt-record-uttnum rec)))))
	     (relation (convert-to-package (get-keyword-arg results :content) 'ont)))
	(if relation
	    (progn
	      ;; update the extractions part of the current utterance record
	      (trace-msg 3 "temporal ordering results are ~S" relation)
	      (setf (utt-record-extractions rec) (get-expanded-extractions-with-temporal-relation (utt-record-extractions rec) relation))
	      (trace-msg 3 "revised extractions are ~S" (utt-record-extractions rec))

	      ;; update the referring expressions part (referent structures) of the current utterance record
	      (update-referring-expressions-with-temporal-relation (utt-record-referring-expressions rec) relation)
	      (trace-msg 3 "revised referring expressions are ~S" (utt-record-referring-expressions rec)))
	    (trace-msg 3 "No temporal relation results received???")))
      (progn
	(trace-msg 3 "No temporal ordering computation when there's no extractions"))))

;;
;; expand an extraction when there's a matching temporal relation
;;
(defun get-expanded-extractions-with-temporal-relation (extractions relation)
  (when (and extractions relation)
    (let ((extraction (car extractions)))
      ;;(trace-msg 3 "extraction to extend: ~S" extraction)
      (mapcar (lambda (rel)
		(if (and
		     ;; valid relation info
		     (listp rel)
		     (< 2 (list-length rel))
		     (eql (first rel) :temporal-relation)
		     ;; if the relation is relevant to the current extraction
		     (get-def-from-akrl-context (second rel) extraction))
		    ;; then update the extraction using the relation info
		    (setf extraction (add-role-to-akrl-context (first rel) (second rel) (third rel) extraction))))
	      relation)
      (trace-msg 3 "extended extraction: ~S" extraction)
      (append (list extraction)
	      (get-expanded-extractions-with-temporal-relation (cdr extractions) relation)))))

;;
;; expand referring expressions when there's a matching temporal relation
;; NOTE:: only the "lf" element of the "referent" structure gets updated because the "lf" part is used in "build-and-send-extractions" (when building a context using "make-lf-list)
;;
(defun update-referring-expressions-with-temporal-relation (referring-expressions relation)
  (when (and referring-expressions relation)
    (trace-msg 3 "expressions to extend: ~S" referring-expressions)
    (mapcar (lambda (expr)
	      (mapcar (lambda (rel)
			(if (eql (second rel) (referent-id expr))
			    (progn
			      (trace-msg 3 "update referent's LF: ~S" (referent-lf expr))
			      (setf (referent-lf expr) (append (referent-lf expr) (list (first rel) (third rel)))))))
		      relation))
	    referring-expressions)))
