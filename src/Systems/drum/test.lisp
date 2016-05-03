;;;;
;;;; File: Systems/drum/test.lisp
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

;;;
;;; Load our TRIPS system
;;;
(defvar *no-im* nil)
(load (if *no-im*
	#!TRIPS"src;Systems;drum;system-no-im"
	#!TRIPS"src;Systems;drum;system"))

;;;
;;; In USER for convenience of testing
;;;
(in-package :common-lisp-user)

;;;
;;; Enable TextTagger
;;;
(setf *use-texttagger* t)

;;;
;;; Sample dialogues
;;;
(defvar *sample-dialogues* nil)
(load #!TRIPS"src;Systems;drum;test-sasaki.lisp")
(load #!TRIPS"src;Systems;drum;test-montero-conde.lisp")
(load #!TRIPS"src;Systems;drum;test-turke.lisp")

;;;
;;; Load core testing code (this needs to be the last load so the "type (run)"
;;; message appears right before the prompt)
;;;
(load #!TRIPS"src;Systems;core;test")

;; Default sample dialogue for this domain
(setf *test-dialog*
  (cdr (assoc 'sasaki-1 *sample-dialogues* :test #'eql)))

(defun ptest (key)
  "Make the sample dialogue given by KEY the global *TEST-DIALOG*, then
call TEST. Reports available KEYs on error."
  (let ((dialogue (cdr (assoc key *sample-dialogues* :test #'eql))))
    (cond
     ((not dialogue)
      (format t "~&ptest: unknown sample dialogue: ~S~%" key)
      (format t "~&ptest: possible values: ~S~%" (mapcar #'car *sample-dialogues*)))
     (t
      (setf *test-dialog* dialogue)
      (test)))))

(defun dtest (text)
  "Run TEXT through the DRUM system. Unlike the core TEST function, which sends the text to TEXTTAGGER, this one sends it to the DRUM module, which, at the end will also generate an EKB."
  (let ((content `(request :sender test :receiver DRUM :content (run-text :text ,text))))
    (test content)
    ))

(defun run-im-only ()
  (dfc:run-component :im))

