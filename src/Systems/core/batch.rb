# src/Systems/core/batch.rb - library for making programs that process a big batch of short (sentence- or paragraph-length) text units in parallel
# 2018-11-08
# William de Beaumont
#
# A batch.rb for a particular system should look something like this:
#
# #!/usr/bin/env ruby
# 
# require_relative '../core/batch.rb'
# 
# Options.extra_banner = <<EOB
# This will be added to the end of the help banner text, before the individual
# option help texts.
#
# EOB
#
# Options.foo = 'bar' # set default options like this
#
# Presets.merge!({ # set selectable groups of options like this
#   'presetname' => {
#     :desc => "preset description",
#     :argv => %w{command line options that make up the preset}
#   },
#   # more presets...
# })
#
# main()
#

require 'ostruct'
require 'optparse'
require 'csv'
require 'thwait'

TripsSystemName = File.basename(File.dirname(File.realpath($0)))

#
# Option parsing/checking
#

Infinity = 1.0/0.0 # for unspecified column range ends

def parse_ranges(str)
  str.split(/,/).collect { |range_str|
    case range_str
      when /^\d+$/
	i = range_str.to_i
	i..i
      when /^(\d+)-(\d+)$/
	$1.to_i..$2.to_i
      when /^(\d+)-$/
	$1.to_i..Infinity
      when /^-(\d+)$/
	1..$2.to_i
      else
	raise "bogus range spec in --text-columns: #{range_str}"
    end
  }
end

Options = OpenStruct.new

# defaults for user-settable options
Options.input_format = 'tsv'
Options.output_archive = true
Options.num_tripses = 4
Options.batch_size = 100
Options.port_base = 6230
Options.timeout = 900
# (some options not listed here default to nil)

# system-specific config
Options.extra_banner = ''
Options.trips_exe = "trips-#{TripsSystemName}"
Options.trips_argv = %w{-nouser}
Options.wait_for = 'TEXTTAGGER'
Options.send_to = :webparser
Options.web_parser_name = TripsSystemName
Options.web_parser_params = {}
Options.archive_files = '*.xml'

Presets = {}

# wrap str to 43-character lines, with an optional indent
def wrap_help_text(str, indent='')
  lines = []
  line = ''
  str.split(/\s+/).each { |word|
    if (line == '')
      line = indent + word
    elsif (line.length + word.length + 1 <= 43)
      line = line + ' ' + word
    else
      lines.push(line)
      line = indent + word
    end
  }
  lines.push(line) if (line != '')
  return lines
end

TheOptionParser = OptionParser.new

def init_opts(opts=TheOptionParser)
  opts.banner = <<EOB
Mac usage:
  cd $output_dir
  export TRIPS_BASE=...
  caffeinate -s script batch.log $TRIPS_BASE/src/Systems/#{TripsSystemName}/batch.rb [options]

  (caffeinate prevents the Mac from sleeping until the process is finished, and
  script logs whatever happens on the console to batch.log.)

Linux usage:
  cd $output_dir
  export TRIPS_BASE=...
  script batch.log -c "$TRIPS_BASE/src/Systems/#{TripsSystemName}/batch.rb [options]"

  (Note the different usage of the script command on Linux.)

Run a large number of units of text (sentences, paragraphs, short stories,
etc.) through the #{TripsSystemName} #{Options.send_to}, optionally collect the resulting
#{Options.archive_files} files into a .tar.bz2 archive with the same name as the current
directory, and optionally copy that file to a web directory for others to
download.

Input is processed in batches, and each batch creates a subdirectory named
batchN/ where N is the zero-padded, zero-based batch number. If some of these
subdirectories already exist, those batches will be skipped. This makes it
possible to restart after an incomplete run, without reprocessing
already-completed batches; just remove the incomplete batch subdirectories and
run the same command again. If you move the incomplete batches to a
subdirectory whose name starts with 'old', their #{Options.archive_files} files will not appear in
the output archive.

You should create a new output directory every time you run batch.rb with
different options or different input files.

Exactly one of the options --input-file, --input-directory, or
--input-file-list must be given.

EOB
  opts.banner += Options.extra_banner

  # where the input is

  opts.on('-fFILE', '--input-file=FILE',
    #                                           # text must be between the #s 
    "A single file to take input from.") { |f|
    File.exists?(f) or raise "input file #{f.inspect} does not exist"
    File.directory?(f) and
      raise "input file #{f.inspect} is actually a directory; " +
	    "use --input-directory=#{f.inspect} instead if this is intentional"
    Options.input = f
    Options.input_is_a = :file
  }

  opts.on('-dDIR', '--input-directory=DIR',
    #                                           #
    "A directory full of files to take input",
    "from.") { |d|
    File.exists?(d) or raise "input file #{d.inspect} does not exist"
    File.directory?(d) or
      raise "input directory #{d.inspect} is actually a file; " +
	    "use --input-file=#{d.inspect} instead if this is intentional"
    Options.input = d
    Options.input_is_a = :directory
  }

  opts.on('-FFILE', '--input-file-list=FILE',
    #                                           #
    "A file listing other files to take input",
    "from, one file per line. Blank lines,",
    "leading and trailing whitespace, and",
    "comments starting with # are ignored. ~/",
    "and ~username/ are interpreted as user home",
    "directories. Other special shell characters",
    "like '\\' and '$' are not interpreted.",
    "Relative paths are allowed, and are",
    "relative to the directory the list is in.") { |f|
    File.exists?(f) or raise "input file list #{f.inspect} does not exist"
    File.directory?(f) and
      raise "input file list #{f.inspect} is actually a directory; " +
            "use --input-directory=#{f.inspect} instead if this is intentional"
    Options.input = f
    Options.input_is_a = :file_list
  }

  # format of the input

  opts.on('-eFORMAT', '--input-file-format=FORMAT',
    #                                           #
    "(Mnemonic: Extension) The format of the",
    "input file(s). Must be one of the",
    "following:",
    "  'txt': plain text (may be one big text",
    "    unit or a text unit for each line or",
    "    paragraph),",
    "  'tsv': tab-separated values (table),",
    "  'csv': comma-separated values (may have",
    "    quotes around values, etc.),",
    "  'psv': pipe-separated values.",
    "Default '#{Options.input_format}'.") { |f|
    formats = %w{txt tsv csv psv}
    formats.include?(f) or
      raise "bogus --input-file-format: " +
	    "expected one of #{formats.inspect}, but got #{f.inspect}"
    if (Options.one_text_unit_per_file and f != 'txt')
      raise "--input-file-format=#{f} is incompatible with --one-text-unit-per-file"
    end
    Options.input_format = f
  }

  opts.on('-g', '--drop-headings',
    #                                           #
    "(Mnemonic: headinG) Drop the first row of",
    "each input file. Not valid with",
    #v space prevents this being interpreted as a spelling of the option
    " \b--input-file-format=txt.") {
    # ^^ backspace character erases the space
    Options.drop_headings = true
  }

  opts.on('-iLIST', '--id-columns=LIST',
    #                                           #
    "The (one-based) column(s) to take the text",
    "unit ID from (not valid with",
    " \b--input-file-format=txt). IDs are used",
    "to name the .xml files, so they are",
    "restricted to contain only letters,",
    "numbers, periods, underscores, and hyphens.",
    "Other characters are replaced with",
    "underscores. The default is to construct", # TODO? check this is actually the default dynamically
    "IDs using the individual filename, followed",
    "by a hyphen and a zero-padded, zero-based",
    "line/paragraph number (not counting dropped",
    "headings). Line numbers are not appended in",
    " \b--one-text-unit-per-file mode. This",
    "option can take a list of columns, in the",
    "same format as --text-columns. In that",
    "case, the columns are joined with dashes.") { |str|
    Options.id_column_ranges = parse_ranges(str)
  }

  opts.on('-cLIST', '--text-columns=LIST',
    #                                           #
    "The columns to take the text unit itself",
    "from (not valid with",
    " \b--input-file-format=txt). Column values",
    "are joined with single spaces, and a period",
    "is added to each one that doesn't already",
    "have sentence-final punctuation. This",
    "option works the same as the 'cut'",
    "command's -f option. That is, it's a comma-",
    "separated list of one-based column numbers,",
    "or hyphenated ranges thereof. Omitting one",
    "end of a range means take everything on",
    "that side. So:",
    "  N   means the Nth column only,",
    "  N-M means columns N through M inclusive,",
    "  N-  means the Nth column and everything",
    "      after it, and",
    "  -M  means the Mth column and everything",
    "      before it.",
    "The default is to use all columns that", # TODO? check default
    "aren't ID columns.") { |str|
    Options.text_column_ranges = parse_ranges(str)
  }

  opts.on('-l', '--pass-sentences',
    #                                           #
    "Pass sentence segmentation information to",
    "TRIPS, via OneSentencePerLine. Instead of",
    "joining sentences with spaces, they're",
    "joined with newlines. Sentence-final",
    "punctuation is not added. (Mnemonic:",
    "Lines).") {
    Options.pass_sentences = true
  }

  opts.on('-rREGEXP', '--sentence-split-regexp=REGEXP', Regexp,
    #                                           #
    "Split sentences within single columns on",
    "the given regular expression.") { |re|
    Options.sentence_split_regexp = re
  }

  opts.on('-1', '--one-text-unit-per-file',
    #                                           #
    "If this option is given, each input file is",
    "treated as one text unit (implies",
    " \b--input-file-format=txt). By default, each",
    "line of each input file is a text unit.") {
    Options.one_text_unit_per_file = true
    Options.input_format = 'txt'
  }

  opts.on('-2', '--split-text-on-blank-lines',
    #                                           #
    "If this option is given, instead of",
    "splitting text units on single newlines,",
    "text units are split on sequences of two or",
    "more newlines (ignoring other whitespace),",
    "i.e. blank lines (implies",
    " \b--input-file-format=txt).") {
    Options.split_text_on_blank_lines = true
    Options.input_format = 'txt'
  }

  unless (Presets.empty?)
    preset_help_lines = [
      #                                           #
      "Apply input format option presets",
      "corresponding to specific data sets:"
    ]
    Presets.each_pair { |name, desc|
      help = "'#{name}' is #{desc[:desc]}, options are:"
      preset_help_lines.
	concat(wrap_help_text(help, '  ')).
	concat(desc[:argv].map { |arg| '    ' + arg })
    }
    TheOptionParser.on('-sPRESET', '--preset=PRESET', *preset_help_lines) { |p|
      raise "unknown preset #{p.inspect}" unless (Presets.key?(p))
      ARGV.push(*Presets[p][:argv])
    }
  end

  # other options

  opts.on('-a', '--no-output-archive',
    #                                           #
    "If this option is given, the output",
    ".tar.bz2 archive is not made. You can still",
    "get the #{Options.archive_files} files from the batch*/",
    "directories (along with the rest of the",
    "logs).") {
    Options.output_archive = false
  }

  opts.on('-wDIR', '--output-archive-directory=DIR',
    #                                           #
    "(Mnemonic: Web directory) The directory to",
    "copy the output .tar.bz2 archive into. By",
    "default the archive is not copied.") { |d|
    File.exists?(d) or raise "output archive directory does not exist"
    File.directory?(d) or raise "output archive directory is not a directory"
    Options.output_archive_dir = d
  }

  opts.on('-nINTEGER', '--num-tripses=INTEGER', Integer,
    #                                           #
    "The number of parallel TRIPS instances",
    "to run at once. Default #{Options.num_tripses}.") { |n|
    n >= 1 or raise "expected positive integer in --num-tripses, but got #{n}"
    Options.num_tripses = n
  }

  opts.on('-bINTEGER', '--batch-size=INTEGER', Integer,
    #                                           #
    "The number of text units to process in a",
    "single TRIPS instance before restarting it.",
    "Default #{Options.batch_size}.") { |s|
    s >= 1 or raise "expected positive integer in --batch-size, but got #{s}"
    Options.batch_size = s
  }

  opts.on('-pINTEGER', '--port-base=INTEGER', Integer,
    #                                           #
    "The first port number to use for a TRIPS",
    "instance. Ports in the interval [port-base,",
    "port-base + num-tripses) will be used.",
    "Default #{Options.port_base}.") { |p|
    (1024..65535).include?(p) or raise "expected base port number between 1024 and 65535 inclusive, but got #{p}"
    Options.port_base = p
  }

  opts.on('-tINTEGER', '--timeout=INTEGER', Integer,
    #                                           #
    "The number of seconds to wait for a result",
    "for an individual text unit, before giving",
    "up on the rest of the batch and restarting",
    "the (likely stuck) TRIPS instance. Default",
    "#{Options.timeout} seconds (#{Options.timeout/60} minutes).") { |t|
    t >= 1 or raise "expected positive integer in --timeout, but got #{t}"
    Options.timeout = t
  }

  unless (Options.send_to == :webparser)
    opts.on('-N', '--pass-file-names', 
      #                                           #
      "Pass the names of input files instead of",
      "their contents to DrumGUI, using run-file",
      "instead of run-text. Note that this is",
      "incompatible with most of the input format",
      "options, since batch.rb is no longer the",
      "one reading the input files. Also, if you",
      "are passing a large file that DrumGUI will",
      "split into many text units itself, batch.rb",
      "will still see the whole file as one text",
      "unit, so it's recommended to set a low",
      " \b--batch-size and high --timeout to",
      "compensate for that.") {
      Options.pass_file_names = true
    }
  end

  # help

  opts.on('-h', '--help', 'Prints this help and exits.') {
    puts opts
    exit
  }
end

def parse_and_check_options
  TheOptionParser.order! # ordered mode might be necessary for --preset to work?

  # additional sanity checks
  Options.input.nil? and
    raise "one of --input-file, --input-directory, or --input-file-list is required"
  Options.port_base + Options.num_tripses <= 65536 or
    raise "expected port-base + num-tripses <= 65536, but got " +
	  "#{Options.port_base} + #{Options.num_tripses} = " +
	  "#{Options.port_base + Options.num_tripses} > 65536"
  if (Options.input_format == 'txt')
    Options.id_column_ranges.nil? or
      raise "--id-columns is incompatible with --input-file-format=txt"
    Options.text_column_ranges.nil? or
      raise "--text-columns is incompatible with --input-file-format=txt"
    %w{
      drop_headings
      pass_sentences
      sentence_split_regexp
    }.each { |o|
      Options[o] and
        raise "--#{o.gsub(/_/,'-')} is incompatible with --input-file-format=txt"
    }
  end
  if (Options.pass_file_names)
    Options.id_column_ranges.nil? or
      raise "--id-columns is incompatible with --pass-file-names"
    Options.text_column_ranges.nil? or
      raise "--text-columns is incompatible with --pass-file-names"
    %w{
      drop_headings
      one_text_unit_per_file
      pass_sentences
      sentence_split_regexp
      split_text_on_blank_lines
    }.each { |o|
      Options[o] and
        raise "--#{o.gsub(/_/,'-')} is incompatible with --pass-file-names"
    }
  end
  if (Options.output_archive_dir and not Options.output_archive)
    raise "--output-archive-directory is incompatible with --no-output-archive"
  end
  if (Options.one_text_unit_per_file and Options.split_text_on_blank_lines)
    raise "--one-text-unit-per-file is incompatible with --split-text-on-blank-lines"
  end
rescue => e
  $stderr.puts "Error: #{e.message}" # no backtrace
  $stderr.puts "Run '#{$0} --help' for usage information."
  exit 1
end

def check_environment
  ENV.key?('TRIPS_BASE') or raise "TRIPS_BASE environment variable unset"
  File.exists?(ENV['TRIPS_BASE']+'/src/WebParser/') or
    raise "You need to check out src/WebParser/ and rebuild the #{TripsSystemName} lisp image before using this program. (You do not need to 'make install' in src/WebParser/, just check it out.)"
  File.exists?(ENV['TRIPS_BASE']+'/src/config/ruby/') or
    raise "You need to check out src/config/ruby/, (re-)run configure, and 'make install' in src/KQML/ and src/TripsModule/ before using this program."
  File.exists?(ENV['TRIPS_BASE']+'/etc/TripsModule/trips_module.rb') or
    raise "You need to configure and install #{TripsSystemName} before using this program. In particular, the Ruby version of the TripsModule library is not installed. (Did you remember to re-configure and re-install after checking out src/config/ruby/?)"
  File.exists?(ENV['TRIPS_BASE']+'/etc/KQML/kqml.rb') or
    raise "The Ruby version of the KQML library is not installed; you need to do 'make install' in src/KQML/."
rescue => e
  $stderr.puts "Error: #{e.message}" # no backtrace
  $stderr.puts "Run '#{$0} --help' for usage information."
  exit 1
end

#
# Input reading functions
#

module Enumerable
  # join sentence strings with spaces, after adding a period to each sentence
  # that doesn't already end with sentence-ending punctuation
  def join_sentences
    collect { |s|
      s + ((s =~ /[\.\?\!]["']?$/) ? '' : '.')
    }.join(' ')
  end
end

def get_column_ranges(cols, column_ranges)
  column_ranges.collect_concat { |range|
    # Array#[] doesn't accept infinite ranges, so make it finite
    finite_range =
      (range.end == Infinity ? (range.begin..(cols.size-1)) : range)
    cols[finite_range]
  }
end

# return a list of column ranges that select all the columns that column_ranges
# wouldn't
def invert_column_ranges(column_ranges)
  max_finite_column =
    column_ranges.collect { |x| x.end.finite? ? x.end : 0 }.max
  all_finite = column_ranges.all? { |x| x.end.finite? }
  inverse = [0..0]
  max_finite_column.times { |i|
    unless (column_ranges.any? { |x| x.cover?(i) })
      if (i == inverse[-1].end + 1)
	inverse[-1] = inverse[-1].start..i
      else
	inverse.push(i..i)
      end
    end
  }
  inverse.push((max_finite_column+1)..Infinity) if (all_finite)
  return inverse
end

# given the current Options, and the column values of one row, return a text
# unit string, and add an ID to the ids list if appropriate
def row_to_text_unit(cols, ids)
  # prefix nil to account for one-based column indices
  cols = [nil] + cols
  unless (Options.id_column_ranges.nil?)
    # save the ID column
    id = get_column_ranges(cols, Options.id_column_ranges).join('-')
    id.gsub!(/[^\w\.-]/, '_')
    ids << id
  end
  text_column_ranges = (
    Options.text_column_ranges || (
      # get all the non-ID columns as the text unit
      Options.id_column_ranges.nil? ?
        [1..Infinity] :
	invert_column_ranges(Options.id_column_ranges)
    )
  )
  sentences = get_column_ranges(cols, text_column_ranges)
  if (Options.sentence_split_regexp)
    sentences =
      sentences.collect_concat { |col|
        col.split(Options.sentence_split_regexp)
      }
  end
  if (Options.pass_sentences)
    sentences.join("\n")
  else
    sentences.join_sentences
  end
end

# return the list of text units ([id, text] pairs) from a single file, given
# its name
def read_text_units_from_file(filename)
  bn = File.basename(filename)
  return [[bn, filename]] if (Options.pass_file_names)
  File.open(filename, 'r') { |f|
    if (Options.input_format == 'txt')
      if (Options.one_text_unit_per_file)
	return [[bn, f.read]]
      elsif (Options.split_text_on_blank_lines)
        paragraphs = []
	paragraph = '';
	f.each_line { |line|
	  if (line =~ /\S/)
	    paragraph += line
	  elsif (paragraph != '')
	    paragraphs << paragraph
	  end
	}
	paragraphs << paragraph unless (paragraph == '')
	num_digits = Math.log10(paragraphs.size).ceil
	return paragraphs.each_with_index.collect { |p, i|
	  [bn + ("-%0#{num_digits}d" % [i]), p]
	}
      else # one text unit per line
	lines = f.readlines
	num_digits = Math.log10(lines.size).ceil
	return lines.each_with_index.collect { |l, i|
	  [bn + ("-%0#{num_digits}d" % [i]), l]
	}
      end
    else # *sv table
      separator =
        case Options.input_format
	  when 'csv'; ',' # unused
	  when 'psv'; '|'
	  when 'tsv'; "\t"
	  else raise "WTF"
	end
      ids = []
      if (Options.input_format == 'csv')
	csv_options =
	  (Options.drop_headings ?
	    { headers: true, return_headers: false } : {})
	text_units = CSV.new(f, csv_options).collect { |row|
	  row_to_text_unit((Options.drop_headings ? row.fields : row), ids)
	}
      else # if it's not csv, don't do anything fancy, just split
	lines = f.each_line
	lines.next if (Options.drop_headings)
	text_units = lines.collect { |l|
	  row = l.chomp.split(separator)
	  row_to_text_unit(row, ids)
	}
      end
      # make IDs if they're not from a column
      unless (Options.id_column_ranges)
        num_digits = Math.log10(text_units.size).ceil
	ids = text_units.size.times.collect { |i| bn + ("-%0#{num_digits}d" % [i]) }
      end
      return ids.zip(text_units)
    end
  }
end

# return the list of text units ([id, text] pairs) from all input files
def read_text_units
  case Options.input_is_a
    when :directory
      Dir[Options.input + '/*'].collect_concat { |f|
	read_text_units_from_file(f)
      }
    when :file_list
      input_dir = File.dirname(File.expand_path(Options.input))
      File.open(Options.input).each_line.collect_concat { |f|
        # remove leading and trailing whitespace and comments
        f = f.sub(/^\s+/,'').sub(/\s*(#.*)?$/,'')
	if (f.empty?) # otherwise blank line
	  []
	else # line with a file on it
	  f = File.expand_path(f, input_dir)
	  File.exists?(f) or raise "input file #{f.inspect} (listed in #{Options.input.inspect}) does not exist"
	  read_text_units_from_file(f)
	end
      }
    when :file
      read_text_units_from_file(Options.input)
  end
end

#
# Main program
#

def main
  init_opts()
  parse_and_check_options()
  check_environment()
  $stderr.puts "Options = #{Options.inspect}"

  require_relative './batch-module.rb'

  port2module = Array.new(Options.num_tripses)

  text_units = read_text_units()

  num_batches = (text_units.size * 1.0 / Options.batch_size).ceil

  $stderr.puts "read #{text_units.size} text units, will parse in #{num_batches} batches"

  tw = ThreadsWait.new

  num_digits = Math.log10(num_batches).ceil
  num_batches.times { |batch_num|
    logdir = "batch%0#{num_digits}d" % [batch_num]
    unless (File.exists?(logdir)) # already did this one
      batch_text_units =
	text_units[batch_num * Options.batch_size, Options.batch_size]
      # try to start this batch by assigning it to the first available
      # thread/module/TRIPS instance/port
      started = false
      begin
	port2module.each_with_index { |mod, port|
	  if (mod.nil? or not mod.alive?)
	    port2module[port] =
	      BatchModule.new(logdir, port + Options.port_base,
			      batch_text_units)
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

  if (Options.output_archive)
    # pack up the resulting .xml files in a .tar.bz2 file named like this dir,
    # and put it up on the web
    pwd_name = File.basename(Dir.pwd)
    system(
      "cd .. " +
      "&& find #{pwd_name} -name '#{Options.archive_files}' " +
      "| grep -v '/old' " +
      "| tar -jcvf #{pwd_name}.tar.bz2 -T - " +
      (Options.output_archive_dir ?
	"&& cp #{pwd_name}.tar.bz2 #{Options.output_archive_dir}" : "")
    )
  end
end
