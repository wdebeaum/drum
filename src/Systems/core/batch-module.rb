#
# TripsModule for connecting to each TRIPS instance (and starting/stopping it)
#

# NOTE: This assumes it is being required by batch.rb, which defines Options.

require 'benchmark'
require 'timeout'
$: << ENV['TRIPS_BASE'] + '/etc'
require 'TripsModule/trips_module'

class BatchModule
  include TripsModule
  include Timeout

  attr_reader :thread

  def initialize(logdir, port, text_units)
    @alive = true
    @logdir = logdir
    $stderr.puts "#{logdir} starting on port #{port}"
    @trips_pid = fork {
      # suppress console output (it all goes in the logs too anyway)
      $stdout.reopen('/dev/null','w')
      $stderr.reopen($stdout)
      exec(ENV['TRIPS_BASE'] + '/bin/' + Options.trips_exe, *Options.trips_argv, '-port', port.to_s, '-logdir', logdir)
    }
    raise "Failed to start TRIPS" if (@trips_pid.nil?)
    sleep 5 # wait for Facilitator to be up before trying to connect
    super(['-connect', "127.0.0.1:#{port}"])
    @name = "BATCH"
    init()
    @text_units = text_units
    @last_story_id = nil
    # wait for the slowest module to start
    add_handler(KQML.from_s("(tell &key :content (module-status . *) :sender #{Options.wait_for})"), method(:do_batch))
  end

  def alive? ; @alive ; end

  def die
    # tell TRIPS to exit, and wait for it to do so
    begin
      send_msg(KQML.from_s('(request :receiver facilitator :content (exit))'))
      @socket.shutdown # make it easier for Facilitator to shut down?
    rescue => e
      $stderr.puts "failed to send exit message to facilitator (#{@logdir}): #{e.message}"
    end
    Process.wait(@trips_pid)
    $stderr.puts "#{@logdir} finished"
    begin
      sleep 2 # wait a little more for the port to become available
    end while (system("lsof -i4TCP@127.0.0.1:#{@port} |grep java"))
    @alive = false
  end

  def do_batch(msg)
    @text_units.each { |id, text|
      @last_story_id = id
      $stderr.puts "starting #{id}..."
      if (Options.send_to == :webparser)
	output_file = "#{@logdir}/#{id}.xml"
	query = KQML[:input => text] + Options.web_parser_params
	if (Options.pass_sentences)
	  query[:"tag-type"] = "(or default one_sentence_per_line)"
	end
	times = Benchmark.measure {
	  reply =
	    timeout(Options.timeout) {
	      send_and_wait(KQML[:request, :receiver => :webparser, :content =>
		KQML[:http, :post, Options.web_parser_name, :query => query]
	      ])
	    }
	  raise "expected a reply with :content but received none (check the end of WEBPARSER.log for a SORRY message)" if (reply.nil?)
	  raise "expected :content-type \"text/xml...\"" unless (reply[:"content-type"] =~ /^text\/xml/)
	  raise "expected string in :content" unless (String === reply[:content])
	  File.open(output_file, 'w').print reply[:content]
	}
      # else send to DrumGUI (whatever it calls itself in this system)
      elsif (Options.pass_file_names) # send just the file name to DrumGUI
	# in this case the "text" is the expanded file path (see
	# read_text_units_from_file and read_text_units in batch.rb)
	# for some reason DrumGUI wants this separated into folder and file
	folder = File.dirname(text)
	file = File.basename(text)
	times = Benchmark.measure {
	  send_and_wait(KQML[:request, :receiver => Options.send_to, :content =>
	    KQML[:"run-file", :folder => folder, :file => file,
		 :"reply-when-done" => true]
# historical note: we have also used these requests in the past, with drum's
# old batch.rb
#	    KQML[:"run-pmcid", :folder => File.dirname(absolute_path),
#			       :pmcid => File.basename(absolute_path),
#			       :"reply-when-done" => true]
#	    KQML[:"run-all-files", :folder => absolute_path,
#				   :select => ".*\\.txt",
#				   :"single-ekb" => true,
#				   :"reply-when-done" => true]
	  ])
	}
      else # send the text we read to DrumGUI
	times = Benchmark.measure {
	  send_and_wait(KQML[:request, :receiver => Options.send_to, :content =>
	    KQML[:"run-text", :text => text, :"reply-when-done" => true]
	  ])
	}
      end
      $stderr.puts "processing #{id} took #{times.real} seconds"
    }
    @last_story_id = nil
  rescue => e
    $stderr.puts "while processing #{@last_story_id}: " + e.message, *e.backtrace
  ensure
    die
  end

  def run_in_background
    @thread = Thread.new { self.run }
  end
end

