;;;;
;;;; File: Systems/core/test.lisp
;;;; Creator: George Ferguson
;;;; Created: Thu Jul 12 15:48:37 2007
;;;; Time-stamp: <Fri Feb  3 09:40:00 CST 2017 lgalescu>
;;;;
;;;; Based on Test/test-socket.lisp from 14 Jun 1999
;;;;
;;;; Load this file after loading your TRIPS system in order to
;;;; run in test mode (with the function TEST and relatives).
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

;; Force use of socket messaging
(COMM:initialize :transport :socket)
(io:initialize :transport :socket)

;; Break on errors for testing
(setq dfc::*break-on-error* t)

;; Don't need tracing in Lisp since the Facilitator does it...
;;(setq comm::*verbose* nil)

;; These functions in USER package for ease of use
(in-package :common-lisp-user)

(defvar *test-dialog* nil
  "List of utterances making up the test dialog.")

(defvar *sample-dialogues* nil
  "Association list of test dialogues, keyed by dialogue ids.")

(defvar *use-texttagger* nil
  "Set to t to make test-utterance (and test, next, etc.) use TextTagger to send stuff to the parser rather than sending it directly.")

(defvar *texttagger-split-mode* :split-clauses
  "Set to :split-clauses or :split-sentences to make test-utterance tell TextTagger to split the string into utterances as clauses or sentences, or set it to nil to keep the string as one big utterance.")

(defun run ()
  "Run the TRIPS system for purposes of testing."
  ;; Initialize for TEST
  (COMM:connect 'test)
  (COMM:send 'test '(register :name test))
  (COMM:send 'test '(tell :content (module-status ready)))
  (COMM:send 'test '(request :content (define-group system)))
  (sleep 1)
  ;; Start all components (of only previously defined trip system)
  (trips:run-trips-system)
  ;; Let modules all get registered
  (format t "Waiting for modules to register ")
  (loop for i from 1 to 3
      do (format t ".")
	 (sleep 1))
  #||
  ;; Send start-conversation messages
  (start-conversation)
  ||#
  (format t "~%System started.~%")
  (help)
  )

;;;
;;; Help
;;;

(defun help ()
  "Show a few hints of what one can do"
  (format t "Use (test \"...\") to test single sentences.~%")
  (format t "Or use (test) followed by (next), (again), or (start-over)~%")
  (format t "to test the default sample dialogue for this scenario.~%")
  (format t "Use (ptest 'dkey) to test the sample dialogue associated with the key \"dkey\". ~%")
  (format t "To see what sample dialogues keys there are use (show-sample-dialogues).~%")
  (format t "Use (help) to see again these hints.~%")
  )

(defun show-sample-dialogues ()
  "Show keys for sample dialogues"
  (format t "Sample dialogues: ~{~a~^, ~}"
	  (sort (mapcar #'car *sample-dialogues*) #'string-lessp)))


;;;
;;; Various useful functions
;;;

(defvar *last-utt-tested* nil
  "Records the last utt tested using TEST or a TEST-DIALOG command.")

(defun test (&optional s &rest features)
  "If parameter S is given, test that sentence by calling TEST-UTTERANCE,
otherwise call TEST-DIALOG to start testing the sample dialog for this
scenario."
  (if s
      (test-utterance s features)
    (test-dialog)))

(defvar *uttnum* 0)

(defun test-utterance (s features)
  "Test a sentence S by sending it to the parser (from whence its
interpretation will get sent to the IM, and so on, and so on...)."
  (setf *last-utt-tested* s)
  (cond
    ((functionp s) ;; we invoke as a 0 parameter function
     (apply s nil))
    ((and (listp s) (listp (first s))) ; a list of messages to send
      (dolist (msg s)
	(COMM:send 'test msg)))
    ;; Non-strings should be messages as if from outside
    ((not (stringp s))
      (COMM:send 'test s))
    (*use-texttagger*
     (let ((tag-request `(tag :text ,s
				      :imitate-keyboard-manager t
				      ,@(when *texttagger-split-mode*
					      (list *texttagger-split-mode* t)))
	     ))
      (COMM:send 'test `(request :content 
				 ,(if features
				     (append tag-request features)
				     tag-request
				      ))))
     ;; display this utterance in the keyboard manager
      (COMM:send 'test `(request :receiver keyboard :content (add-text :text ,s))))
    (t
      (let ((channel 'desktop)
	    (mode 'text)
	    (end (length s)))
	(incf *uttnum*)
	(dolist (content
		 `((started-speaking :channel ,channel :direction input :mode ,mode :channel ,channel)
	           (word ,s :frame (0 ,end) :channel ,channel :direction input)
		   (stopped-speaking :channel ,channel :direction input :mode ,mode :channel ,channel)
		   (utterance :channel ,channel :direction input :mode ,mode :text ,s :channel ,channel :uttnum ,*uttnum*)))
	  (COMM:send 'test `(tell :sender test :content ,content)))))
    )
  t)

;;;
;;; Test by stepping through a dialog
;;;

(defvar *test-uttnum* -1
  "Next utterance to send during testing (actually, 1- that number).")

(defun test-dialog ()
  "This function starts the sample dialog defined by *TEST-DIALOG*.
Use the functions NEXT, AGAIN, and START-OVER to proceed with testing
after each utterance is processed."
  (setf *test-uttnum* -1)
  (next))

(defun next (&optional (count 1))
  "Process the next COUNT (default 1) utterances of the sample dialog."
  (loop for i from 1 to count
	do (next1)))

(defun next1 ()
  "Process the next utterance of the sample dialog."
  (cond ((eql (1+ *test-uttnum*) (length *test-dialog*))
	 (format t "Sample dialogue has ended. Use (START-OVER) to start over.~%"))
	(t
	 (incf *test-uttnum*)
	 (test-one (elt *test-dialog* *test-uttnum*))
	 (when (eql (1+ *test-uttnum*) (length *test-dialog*))
	   (COMM::send 'test `(TELL :content (component-status :who TEST :what (OK TEST-FINISHED))))
	   ))))

 ;; NEXT3 and NEXT4 here for historical reasons...
(defun next3 ()
  (next 3))

(defun next4 ()
  (next 4))

(defun skip (&optional (count 1))
  "Skip the next COUNT (default 1) utterances of the sample dialog."
  (setf *test-uttnum* (mod (+ *test-uttnum* count) (length *test-dialog*)))
  (format t "~&;; skipped ~D utterance~:P; next utterance: ~D~%" count (1+ *test-uttnum*))
  (values))

(defun start-over (&optional dialogue)
  "Restart the sample dialogue at the first utterance."
  (if dialogue (setf *test-dialog* dialogue))
  (setf *test-uttnum* -1)
  ;; Used to be able to just ``say'' this as if the user said it
  ;(test-one "start over")
  ;; Now have to do it ourselves
  (COMM:send 'test '(request :receiver speech-out :content (say "I'm starting over.")))
  (COMM:send 'test '(broadcast :content (request :content (restart))))
  ;; And apparently people didn't like doing NEXT automagically
  ;(next)
)

(defun test-one (s)
  "Sends sentence S to the TEST function."
  (format t ";; -----------------------------------------------------------~%")
  (format t ";; ~A~%" s)
  (format t "~%")
  (setq *last-utt-tested* s)
  (test s))

(defun again ()
  "Process the current utterance of the sample dialogue again.
Also works to repeat something tested with (TEST \"...\")."
  (test-one *last-utt-tested*))

(defun test-all ()
  "Invoke TEST on all utterances of *TEST-DIALOG* in order.
This function does not pause between utterance, wait for results to be
finished, or any other smart thing. It simply pumps the messages in using
TEST."
  (loop for x in *test-dialog*
     do (test x)))

(defun test-all-of (key)
  "Set *TEST-DIALOG* to the dialog identified by KEY on *SAMPLE-DIALOGUES*,
then invoke TEST-ALL to test all its utterances."
  (setf *test-dialog* (cdr (assoc key *sample-dialogues*)))
  (test-all))

(defun test-lines-from-file (filename)
  "Read lines from the named file into *test-dialog*, and then invoke test-all
   to test all of the lines as utterances."
  (with-open-file (f filename :direction :input)
    (setf *test-dialog*
          (loop for line = (read-line f nil nil)
                while line
                collect line
                ))
    )
  (test-all)
  )

(defun test-whole-file (filename)
  "Read the whole named file into one string in *test-dialog*, and then call test on it."
  (with-open-file (f filename :direction :input)
    ;; from http://www.ymeme.com/slurping-a-file-common-lisp-83.html
    (let ((str (make-array (file-length f) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer str) (read-sequence str f))
      (test str))))

(defun start-conversation ()
  ;; Send start-conversation messages
  (format t "~%Starting conversation~%")
  (COMM:send 'test
	     '(broadcast :content (tell :content (start-conversation))))
  (COMM::send 'test `(TELL :content (component-status :who TEST :what (OK))))
  ;; Wait to print listener prompt
  (sleep 2)
  )


;;;
;;; Tell the user what to do next
;;;
(format t "~%Start the TRIPS facilitator if you haven't done so already.~%")
(format t "Then enable any desired tracing and type (run) to start the system.~%")
