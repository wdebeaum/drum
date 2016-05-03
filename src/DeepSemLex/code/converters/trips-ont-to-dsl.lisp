;;;; trips-ont-to-dsl.lisp - load the entire old TRIPS ontology into the DSL library and write it back out in Lisp DSL format

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up :up "config" "lisp")
                       :name "trips")))
(load #!TRIPS"src;DeepSemLex;code;lib;defsys")
(dfc:load-component :deepsemlex)

(in-package :dsl)

(defun cl-user::run ()
  ;; load all TRIPS ontology files
  (require-resource-version :ont)
  (let* (
         ;; get all concept names in the ONT package
         (ont-pkg (find-package :ont))
         (ont-names
	   (loop for name being the hash-keys of (concepts *db*)
	         when (eql ont-pkg (symbol-package name))
		   collect name))
	 ;; set up environment for printing
         (*package* (find-package :ld))
         *current-provenance*)
    ;; set the current provenance and print it first so we don't print it for
    ;; each concept
    (ld::provenance ld::TRIPS)
    (format t "~s~%~%" *current-provenance*)
    ;; sort concepts alphabetically (why not?)
    (setf ont-names (sort ont-names #'string< :key #'symbol-name))
    ;; print all the concepts
    (dolist (name ont-names)
      (format t "~s~%~%" (listify (gethash name (concepts *db*)))))
    )
  (trips:exit 0))

