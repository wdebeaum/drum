This is a library for representing the Trips Ontology in OWL and also representing
AKRL and/or LF structures in OWL as well.

INSTRUCTIONS FOR SETUP: 

These classes use the Protege-OWL API from the Protege project
(http://protege.stanford.edu/).  The Protege-OWL API is part of the
Protege 3.x OWL package.  (NB: Protege 4.x uses a different API, which
is not compatible.)

Get the API by downloading the latest Protege 3.x.y runtime.  On a Mac,
(need a more general config to handle other domains, TBD...) this will
install to /Applications/Protege_3.X.Y

To compile the lisp ontology to owl, run compileOntology.sh, which will
produce ontology.owl 

NOTES:

When you use this in a java runtime, you need to include a ton of .jar files.
Look in convertTripsOntology.sh to see which ones.  Also, that
script also runs java with the log4j.properties file, which turns off a lot
of the junk that Pellet prints out while it is running.

If you want to run TripsProtegeBridge with the Protege GUI, run Protege and
open the ontology.pprj file (installed in $TRIPS_BASE/etc/OWL/). This will load
ontology.owl in the same directory, and the TRIPS tab, which will connect to
the Facilitator immediately (so you need to run that first). You can also open
the pprj file from the command line using the run_protege.sh script in the main
Protege directory:

cd /Applications/Protege_3.4.*/
./run_protege.sh $TRIPS_BASE/etc/OWL/ontology.pprj

Only Protege 3 can use SQWRL and TripsProtegeBridge, but Protege 4 seems to be better for viewing instances. One strategy for dealing with this is to save the ontology from TRIPS (using the save-ontology message to ProtegeTripsModule) and load it into Protege 4 for viewing. Thereafter, if you save over the same file, Protege 4 will notice and ask you if you want to reload it. But if you want to change the ontology while TRIPS is running, you still have to use Protege 3.
