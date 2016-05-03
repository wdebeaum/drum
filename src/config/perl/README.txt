
File: README.txt
Creator: George Ferguson
Created: Fri May  4 08:57:01 2007
Time-stamp: <Fri May  4 09:00:22 EDT 2007 ferguson>

4 May 2007

- Updated perl configure/install after many years of disuse

- The basic approach is to put the files associated with a module into
  TRIPS_BASE/etc/MODULE, then invoke perl with

    perl -I$TRIPS_BASE/etc/MODULE -I$TRIPS_BASE/etc

  This way the module's own files can be picked up without any prefix,
  and other modules (eg., perl libraries such as KQML.pm) can be found
  with a prefix (e.g., KQML::KQML).

  For testing, the "run" target in prog.mk passes -I.. in order to
  find other components prior to installing them.

- Current install might not be gracious with installing subdirs (for
  example if one has packages to install)
