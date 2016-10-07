#!/usr/bin/ruby

# batch.rb - process a big batch of XML papers in parallel
# 2016-10-06
# William de Beaumont
#
# USAGE: set TRIPS_BASE to a reasonable value, and run this program from a
# directory where you want the batch???/ logdirs to show up:
#   caffeinate -s script batch.log $TRIPS_BASE/src/Systems/drum/batch.rb
# some parameters are defined as constants below:

#INPUT_DIR='/Users/lgalescu/work/drum/Data/PMC/ppp'
#PMCIDS_FILE='/Users/lgalescu/work/drum/Data/PMC/runs/eval2016-batch4.pmcids'
INPUT_DIR='/Users/lgalescu/work/drum/RAS_Machine/data.161006'
NUM_TRIPSES=5
BATCH_SIZE=10 # papers
PORT_BASE=6260

raise "TRIPS_BASE environment variable unset" unless (ENV.key?('TRIPS_BASE'))
$: << ENV['TRIPS_BASE'] + '/etc'

require 'benchmark'
require 'thwait'
require 'TripsModule/trips_module'

class DrumParseFiles
  include TripsModule

  attr_reader :thread

  def initialize(logdir, port, input_files)
    @alive = true
    @logdir = logdir
    $stderr.puts "#{logdir} starting on port #{port}"
    @trips_pid = fork {
      # suppress console output (it all goes in the logs too anyway)
      $stdout.reopen('/dev/null','w')
      $stderr.reopen($stdout)
      #exec(ENV['TRIPS_BASE'] + '/bin/trips-drum', *%w{-nouser -mode eval -port}, port.to_s, '-logdir', logdir)
      exec(ENV['TRIPS_BASE'] + '/bin/trips-drum', *%w{-nouser -port}, port.to_s, '-logdir', logdir)
    }
    raise "Failed to start TRIPS" if (@trips_pid.nil?)
    sleep 5 # wait for Facilitator to be up before trying to connect
    super(['-connect', "127.0.0.1:#{port}"])
    @name = "DRUMPARSEFILES"
    init()
    @input_files = input_files
    # wait for TT to be ready before we start sending load-file messages
    add_handler(KQML.from_s('(tell &key :content (module-status . *) :sender TEXTTAGGER)'), method(:do_batch))
  end

  def alive? ; @alive ; end

  def die
    @alive = false
    # tell TRIPS to exit, and wait for it to do so
    begin
      send_msg(KQML.from_s('(request :receiver facilitator :content (exit))'))
    rescue => e
      $stderr.puts "failed to send exit message to facilitator (#{@logdir}): #{e.message}"
    end
    Process.wait(@trips_pid)
    $stderr.puts "#{@logdir} finished"
  end

  def do_batch(msg)
    begin
      @input_files.each { |file|
	absolute_path = File.expand_path(file)
	$stderr.puts "starting #{absolute_path}..."
	times = Benchmark.measure {
	  # TODO detect when TRIPS is stuck and die
	  send_and_wait(KQML[:request, :receiver => :drum, :content =>
#	    KQML[:"load-file", :folder => File.dirname(absolute_path),
#			       :file => File.basename(absolute_path)]
#	    KQML[:"run-pmcid", :folder => File.dirname(absolute_path),
#			       :pmcid => File.basename(absolute_path),
#			       :"reply-when-done" => true]
	    KQML[:"run-all-files", :folder => absolute_path,
				   :select => ".*\\.txt",
				   :"single-ekb" => true,
				   :"reply-when-done" => true]
	  ])
	}
	$stderr.puts "processing #{absolute_path} took #{times.real} seconds"
      }
    ensure
      die
    end
  end

  def run_in_background
    @thread = Thread.new { self.run }
  end
end

port2module = Array.new(NUM_TRIPSES)

papers = Dir[INPUT_DIR + '/*']
#papers =
#  File.open(PMCIDS_FILE,'r').
#  each_line.
#  collect { |l|
#    "#{INPUT_DIR}/#{l.chomp}"
#  }.
#  select { |p| File.directory?(p) }
# sort papers by numeric value
papers.sort_by! { |p| p.sub(/.*\//,'').to_i }
NUM_BATCHES = (papers.size * 1.0 / BATCH_SIZE).ceil

tw = ThreadsWait.new

NUM_BATCHES.times { |batch_num|
  logdir = "batch%03d" % [batch_num]
  unless (File.exists?(logdir)) # already did this one
#    # get all the xml paragraph input files for this batch of papers
#    input_files = []
#    papers[batch_num * BATCH_SIZE, BATCH_SIZE].each { |p|
#      input_files += Dir[p + '/*.xml']
#    }
    # get the paper input directories for this batch of papers
    input_files = papers[batch_num * BATCH_SIZE, BATCH_SIZE]
    # try to start this batch by assigning it to the first available
    # thread/module/TRIPS instance/port
    started = false
    begin
      port2module.each_with_index { |mod, port|
	if (mod.nil? or not mod.alive?)
	  port2module[port] =
	    DrumParseFiles.new(logdir, port + PORT_BASE, input_files)
	  port2module[port].run_in_background
	  tw.join_nowait(port2module[port].thread)
	  started = true
	  break
	end
      }
      unless (started)
        # all are busy
	# wait for the next thread to finish
	$stderr.puts "waiting for next thread to finish"
        tw.next_wait
      end
    end until (started)
  end
}
# wait for remaining threads
tw.all_waits

