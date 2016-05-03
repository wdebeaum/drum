;;;;
;;;; logging.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 17 Aug 1999
;;;; Time-stamp: <Tue Feb 10 14:54:27 EST 2004 ferguson>
;;;;

(in-package :logging2)

(defvar *log-filename* "module.log"
  "Name of the file used to log messages to and from this module.")

(defvar *log-stream* nil
  "Stream for logging i/o to/from the planner. This is nil when running
within Lisp (meaning logging is effectively disabled), or is opened by the
restart function when running from a dumped Lisp.")

(defun initialize (filename)
  "Initialize the logging facility and opens the first log as FILENAME in
the current directory."
  (setq *log-filename* filename)
  (chdir nil))

(defun log-message-received (msg)
  "Logs the receipt of message MSG in the module's log file."
  (log-message :receive msg))

(defun log-message-sent (msg)
  "Logs the sending of message MSG in the module's log file."
  (log-message :send msg))

(defun log-message (what msg)
  "Logs message MSG in the module's log file. Argument WHAT should be a
keyword like :SEND, :RECEIVE, or :WARN."
  (format *log-stream* "<~A T=\"~A\">~%  ~S~%</~A>~%~%"
	  what (timestamp) msg what)
  (finish-output *log-stream*))

(defun chdir (dir)
  "Closes the current module log and reopens it in the directory named DIR."
  (let* ((logfile (make-pathname :directory `(:relative ,dir) ;; NC - explicit
				 :name *log-filename*)))
    (when *log-stream*
      (format *log-stream* "</LOG>~%")
      (close *log-stream*))
    (setq *log-stream* (open logfile :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create))
    (format *log-stream* "<LOG DATE=\"~A\" TIME=\"~A\" FILE=\"~A\">~%~%"
	    (datestamp) (timestamp) (truename logfile))
    (finish-output *log-stream*)))

(defun timestamp ()
  "Returns an HH:MM:SS timestamp."
  (multiple-value-bind (sec min hour date month year dow dstp tz)
      (get-decoded-time)
    (declare (ignore date month year dow dstp tz))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))

(defun datestamp ()
  "Returns an MM/DD/YYYY datestamp."
  (multiple-value-bind (sec min hour date month year dow dstp tz)
      (get-decoded-time)
    (declare (ignore sec min hour dow dstp tz))
    (format nil "~2,'0D/~2,'0D/~4,'0D" date month year)))

