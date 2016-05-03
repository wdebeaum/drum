(load "defsys")

;;(in-package "USER")

(unless (find-package "LAUNCHER")
  (load "TRIPS:LispLauncher;defsys"))

;;(in-package "PARSER")

(excl:defsystem :launcher-modules ()
  (:module parser ("TRIPS:Parser;parser-medadvisor-kr"))
  )


