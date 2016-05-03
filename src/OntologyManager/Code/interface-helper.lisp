;;
;; Nate Chambers
;;
;; Auxiliary functions for public.lisp
;;

(in-package "ONTOLOGYMANAGER")

(defun debug-om (level str &rest rest)
  "Just output everything for now..."
  (if (<= level *debug-level*) (apply #'format (append (list 't str) rest))))

(defun GCB-value (v1 v2)
  "v1      : feature value  i.e. f_dynamic
v2      : feature value
returns : returns the more specific of the feature values
        : if values are unrelated, returns nil"
  (cond
   ((eq v1 v2) v1)
   ;;   (t (lf-more-specific-feature-value v1 v2))))
   (t (more-specific v1 v2))))

(defun listify_sem-argument (arg)
  "arg     : sem-argument structure
returns : list form of the structure in the same format as the data file
          ...see Data/LFData/"
  (let ((feat-list (sem-argument-restriction arg)))
    (list
     (sem-argument-optionality arg)
     (sem-argument-role arg)
     (cons (feature-list-type feat-list)
	   (feature-list-features feat-list))
     (list :implements 
	   (sem-argument-implements arg))
     )))

(defun listify_feature-list (fl)
  "fl      : feature-list structure
returns : list form of the structure, (ft_situation (container -) ( ) ...)"
  (let ((result (list (feature-list-type fl))))
    (when (feature-list-features fl)
      (setq result (append result 
			   (list (cons :required (feature-list-features fl))))))    
    (when (feature-list-defaults fl)
      (setq result (append result (list (cons :default (feature-list-defaults fl))))))
    result))
  
