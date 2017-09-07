#!/bin/bash

# parameter-tuning-test.sh - try a bunch of different settings of parser parameters on the same data

# see also ../{gloss,parser}/parallel-batch.sh

. $TRIPS_BASE/src/Systems/drum/parallel-settings.sh

meta_logdir=${TRIPS_LOGDIR:-${TRIPS_LOGS:-`pwd`}/`date '+%Y%m%dT%H%M'`}
port=$PORT_BASE

# if we exit this program, kill all the subprocesses
trap "for pid in \`ps -o pid,command |grep 'trips-drum\|DRUM' |fgrep -v grep |perl -n -e \"print \\\"\$&\\n\\\" if (/\\d+/);\"\` ; do kill \$pid ; done" EXIT

    # parameters to tune
for beam_width in 10 20 30 ; do
  for skeleton_boost_factor in 1 1.02 1.05 1.1 ; do
    for barrier_penalty in 0.8 0.9 0.95 1 ; do

      # wait for room for a new job
      while [ "`ps -o command |fgrep 'DRUM' |fgrep -c -v fgrep`" -ge $THREADS ]
      do sleep 30
      done

      # set up logdir and batch.lisp
      logdir=$meta_logdir/$port
      mkdir -p $logdir
      cat >$logdir/batch.lisp <<EOLISP
	(setf
	  parser::*beam-width* $beam_width
	  parser::*skeleton-boost-factor* $skeleton_boost_factor
	  (parser::barrier-penalty parser::*chart*) $barrier_penalty
	  )
	;; example for how to run a text file through	
	;;(test-whole-file "$TRIPS_BASE/etc/Data/dryrun1412/dr1_model1_p1.txt")
EOLISP
      
      # start the new job
      # note: need setpgrp so that trips-drum can kill its children by
      # killing processes whose group IDs are the same as its own process ID
      perl -e " \
	setpgrp(0,0); \
	exec(\"$TRIPS_BASE/bin/trips-drum \
	  -nouser -logdir $logdir -port $port \
	  -batch $logdir/batch.lisp \
	\");" &

      # next port number
      let "port = $port + 1"

      # wait for new job to start before we check for room to start another
      sleep 1

    done
  done
done

# wait for last jobs to finish
wait

# get rid of trap from before
trap - EXIT

