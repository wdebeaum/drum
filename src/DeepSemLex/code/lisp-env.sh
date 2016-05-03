# lisp-env.sh - source this file with TRIPS_BASE set in order to get
# environment variables useful for constructing lisp-flavor-independent command
# lines

CONFIGDIR="$TRIPS_BASE/src/config"

# get environment from a defs.mk file
mk2env() {
  `awk 'BEGIN { print "export" } /^([A-Z_]+) = ([^[:space:]]+)$/ { print " " $1 "=" $3 }' <$1`
}

# get environment from lisp/defs.mk
mk2env $CONFIGDIR/lisp/defs.mk

# someday I'd like these to be in lisp/defs.mk, but that day hasn't come yet
BATCH=--batch
LOAD=--load
EVAL=--eval
QUIT="--eval (quit)"

case "$LISP_FLAVOR" in
  sbcl)
    BATCH="--noinform --noprint --disable-debugger"
    QUIT="--eval (sb-ext:quit)"
    ;;
  ccl)
    LOAD=-l
    EVAL=-e
    QUIT="-e (quit)"
    ;;
  cmucl)
    BATCH=-batch
    LOAD=-load
    EVAL=-eval
    QUIT="-eval (quit)"
    ;;
  allegro)
    BATCH=-batch -q
    LOAD=-L
    EVAL=-e
    QUIT="-e (exit)"
    ;;
  *)
    echo "Warning: unknown lisp flavor $LISP_FLAVOR" >&2
esac

