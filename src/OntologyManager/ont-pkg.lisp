;; pull this out of defsys so that other modules (e.g., KM)
;; can load just the KR package if they want, instead of having to
;; load the whole OntologyManager -- just put the following in your defsys.lisp
;;   (unless (find-package :ont)
;;     (load #!TRIPS"src;OntologyManager;ont-pkg"))

(defpackage :ont
  (:use)
  (:import-from "COMMON-LISP-USER" "+" "-")
  )
