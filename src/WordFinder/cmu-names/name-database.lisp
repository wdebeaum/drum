(in-package :wordfinder)

; The hashtable to contain all the names
(defvar name-hash-table (make-hash-table :test #'equal))

(defun build-name-hashtable (filename)
  "Must be run before is-proper-name? or convert-cmunames-entries will work."
  (with-open-file (name-file filename :direction :input)
                  (dolist (name (read name-file))
                          (setf (gethash (string name) name-hash-table) t)))
  name-hash-table)
                                
(defun is-proper-name? (name)
  "Returns whether name is a proper name in the CMU Names database."
  (if (gethash (string-upcase name) name-hash-table)
      name
      nil))

(defun convert-cmunames-entries (name)
  "Returns a list of entries to be returned to the parser given a name."
  (if (is-proper-name? name)
      ; we return a list since all converters are expected to return
      ; multiple entries.
      ; Regarding the probability:
      ; Entries from this database are not "trusted" as well as entries
      ; from the others.  A name should probably only be a name if it
      ; didn't show up in another database.
      ;; for now treat names as ONT::referential-sem; can be places, people
      ;; but mostly they're people
      
      (list (build-entry name :constit-type 'w::NAME :lfs '(ONT::referential-sem)
                         :prob 0.6 :source :cmunames))))
