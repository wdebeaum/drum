Instructions for using TRIPS on Windows:

This document describes how to build and run TRIPS (specifically PLOT) on
Windows XP (using Cygwin and Allegro CL).

-----------------------------
Building TRIPS
-----------------------------
wdebeaum recommends working in a new directory in the root of the Windows
drive you're working on, e.g. "C:\plot". This avoids any hassle with long
directory names, or those with spaces in them.

* Install Cygwin.
  http://cygwin.com/
  This allows us to use the same configure and makefiles that already exist.  
  Make sure you install the following cygwin packages, along with the base
  package (this list is probably incomplete):
  * autoconf (get the base autoconf which will install the other stuff
    automatically)
  * automake (same instructions as autoconf)
  * curl
  * cvs (if you want direct cvs access)
  * gcc (get base)
  * libtool
  * make
  * openssh (if you want direct cvs access)
  * perl
  * procps
  * tcl/tk libs
  * libiconv
  (Nate also recommends:
    * diffutils
    * pine
    * rxvt (nicer terminal)
    * unzip
    * util-linux
    * vim
    * zip
    )

* Install a JDK for Java 1.5 or higher
  http://java.sun.com/javase/downloads/index_jdk5.jsp
  (I used "JDK 5.0 Update 16" --wdebeaum)
  Windows comes with a JRE for 1.6, but being a JRE, it has no javac.

* Install Allegro CL
  Run the install program, and put your license file in the main directory the
  install created, e.g. C:\Program Files\acl81\

* Make sure the Java binaries are visible from your path.
  Either:
  * Put the Windows path to the JDK's bin\ directory in the Windows %PATH%
    environment variable,
  Or:
  * Put the Cygwin path to the JDK's bin/ directory in the Cygwin (bash) $PATH
    environment variable.
  Either way, make sure that the java bin dir comes *before* the Windows
  system dir (which has a java).  Check using 'which java' in Cygwin.

* Make sure the Allegro binaries are visible from your path.
  Same choices as Java, and use that same acl main directory (it contains the
  ".exe"s).

* Fix the tcl lib if it's broken:
  cd /usr/lib
  ls tcl84.dll
  if it's not there, make a link to where it is:
  ln -s ../bin/tcl84.dll

* Check out TRIPS from CVS:
  cvs -d username@trips.cs.rochester.edu:/p/cvs/trips checkout -P plow
  (change "plow" to whatever is appropriate for your desired system)

* Follow the instructions in src/TextTagger/README for getting the resources
  it needs. You can put them in /usr/local/share/ and configure will find
  them.

From here these instructions parallel those in building-trips.html, but
they're complicated by the fact that Java and Lisp on Windows don't understand
symlinks.

* Configure:
  cd src
  ./configure --with-lisp="build -I alisp8" --with-runtime-lisp=alisp8
  We need to use different lisp invocations for configuring/building and
  running. "build -I alisp8" is the console version of Allegro on Windows,
  which allows the configure and build scripts to work, however it doesn't
  handle multithreaded IO well. "alisp8" handles IO correctly (allowing the
  run scripts to work), but it opens its own window instead of communicating
  via stdin/stdout.

* Compile and install the Lisp modules:
  make compile-lisp
  make install-lisp
  Note: don't just do make install in Systems/plot/. Somehow that causes lisp
  to segfault when you load the dumped image. If you compile and install from
  the top level, the dump works correctly. I don't know why.

* Change the name of the top level directory from "src" to "TRIPS", and make
  a symlink from "src":
  mv src TRIPS
  ln -s TRIPS src
  (You'll have to switch this back and forth again if you want to recompile
  lisp stuff and then java stuff.)

* Compile and install the Java modules:
  make compile-java
  make install-java
  * (somehow this includes TextTagger and TextPP, which are Perl modules)

* ... and install supporting java code (compilation happened above):
  cd util/ ; make -f Makefile-java install ; cd ..
  cd KQML/ ; make install ; cd ..
  cd TripsModule/ ; make install ; cd ..

* Install the language models so we don't mess up generation:
  cd SpeechLM/ ; make install ; cd ..

----------------------------
Running TRIPS
----------------------------
wdebeaum recommends running trips-plot from a directory you make called
$TRIPS_BASE/logs, so that the log directories don't clutter up the bin/
directory.

* In a Cygwin terminal window:
  cd logs
  ../bin/trips-plot -nospeech
  [I don't know whether -nospeech is actually necessary, but I haven't tested
  without it --wdebeaum]

Note 1: If you intend to connect to the real CHCS system (as opposed to
FakeCHCS), you should pass the option "-emulator VT320" to trips-plot.

Note 2: the recommended way to quit the whole system is to quit the
Facilitator using the menu (Control > Quit TRIPS). Unfortunately that doesn't
always (ever?) work for the lisp components. You can use Ctrl-Z to stop the
trips-plot job and kill -9 %<job number> to kill it.

----------------------------
General notes about the problems with running on Windows
----------------------------

* The Windows native directory name separator is \, while elsewhere it's /.
 * This interacts with the use of \ as an escape character.

* The Windows native path name separator is ;, while elsewhere it's :.
 * This interacts with the use of ; as a command separator in bash.
   Interestingly, using \; can work.

* Windows paths start with a drive letter, e.g. "C:", which Cygwin maps to
  "/cygdrive/c". (Note: that's "cygdrive", not "cygpath" or "cygwin". This has
  bitten me a couple times --wdebeaum)

* Java programs can be run from Cygwin, but any paths you give them must be in
  Windows format (including CLASSPATH and file arguments).

* cygpath is a useful program for dealing with the above issues, but it
  doesn't exist on non-cygwin systems, so you still have to test whether
  you're on Cygwin any time you want to use it.

* Java doesn't know about symlinks made in Cygwin, so you have to make sure
  any paths you pass to java (and javac etc.) don't traverse any symlinks.
  ABCL has the same problem. This is why we have to switch between TRIPS and
  src for the real name of the top-level source directory.

* Windows, unlike Unix, won't allow you to remove a file when it's being
  "used" by another program. This includes moving a directory when another
  instance of bash is using it as its current working directory.

* Backspace doesn't work in vim in Cygwin :( Use delete instead.
