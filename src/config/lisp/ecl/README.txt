
Time-stamp: <Wed Nov  1 21:07:42 EST 2006 ferguson>

1 Nov 2006:

- As of 16 Jun 2006, you need to have configured ecl with
  ``--with-tcp'' to enable socket support for ``(require 'socket)''

- It seems that ecl-0.9h works fine, but ecl-0.9i has screwed up
  pathname handling or defaults or something so that our bootstrapping
  (the loading of "../config/lisp/trips") fail, perhaps due to a
  missing filetype (of "lisp" or something).

- The ecl website also has the following to say about OSX/Intel, as of
  2006-04-24:

  The only caveat is that the Boehm-Weiser garbage collector shipped
  with ECL (i.e. version 6.5) has some problems with threads in
  OSX. It is thus not recommended to use the configuration flag
  --enable-threads on this platform until these and other issues are
  solved.

  http://sourceforge.net/forum/forum.php?forum_id=565026
