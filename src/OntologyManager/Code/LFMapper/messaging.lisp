;; Nate Chambers
;;
;; Functions that must make message calls to access some information.
;; Typically they do formatting so they can call real message passing
;; functions in messages.lisp
;;

(in-package "ONTOLOGYMANAGER")


;; This would be used if we had access to the KR's ontology 
(defun query-kr (kr-type argument)
  "@parameter type A KR type.
   @parameter argument A valid argument within the type.
   @desc This function is called to get the type constraints for the type of
         object that can be in the argument position of the given kr-type.
         Transform rules can explicitly set this in the :defaults key if
         we do not have access to the kr.
   @returns A *single* kr-type atom.  No support for multiple types."
  (declare (ignore kr-type argument)) ;; for now
  nil
  )
