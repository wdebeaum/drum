
Time-stamp: <Wed Oct 22 14:49:29 EDT 2003 ferguson>

- gf: 22 Oct 2003: As of this writing, the new InterpretationManager
  (IM) uses some functions from the Parser. So in this defsys.lisp,
  it loads the Parser's defsys. This means that it is probably
  meaningless to try and load/run/test just the IM. In fact,
  IM::LOAD-SYSTEM loads the parser.


