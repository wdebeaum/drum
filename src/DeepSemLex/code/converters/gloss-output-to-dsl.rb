#!/usr/bin/ruby

# gloss-output-to-dsl.rb - final stage of converting gloss output facilitator.log to DSL format: correct WN references and fill in definition text

$: << ENV['TRIPS_BASE'] + '/etc/WordNetSQL'
require 'word_net_sql'

current_synset = nil
current_word = nil
current_word_like = nil
begin
  $stdin.each_line { |l|
    if (l =~ /\(CONCEPT WN::\|([^\|]+)\|/)
      before, capture, after = $`, $1, $'
      current_word = nil
      current_word_like = nil
      current_synset = WordNetSQL.sk2ss(capture)
      senses =
	WordNetSQL.query_collect(
	    "SELECT lemma, sense_number FROM senses
	     WHERE ss_type=? AND synset_offset=? ORDER BY word_number;",
	    *current_synset
	  ) { |lemma, sense_number|
	  "(sense WN::#{lemma.gsub(/'/,'^')}.#{current_synset[0]}.#{sense_number})"
	}
      l = before + ("(CONCEPT WN::%s%08d\n " % current_synset) + senses.join("\n ") + after
    end
    comments = ''
    l.gsub!(/\(:\* ONT::\S+ W::([^\)]+)\)|WN::\|([^\|]+)\|/) { |m|
      capture = ($1 || $2)
      if (capture =~ /%/)
	capture += '::' if (capture.count(':') < 4)
	capture.downcase!
	capture.sub!(/\(i?[ap]\)%/,'%')
	# look up the synset for the sense key
	ref_synset = nil
	begin
	  ref_synset = WordNetSQL.sk2ss(capture)
	  comments += ' ' + capture
	rescue
	  $stderr.puts "Warning: bogus sense key #{capture} on line #{$.}"
	end
	if (ref_synset.nil?) # no synset
	  nil
	elsif (current_word.nil?) # no word, just use the synset
	  m = "WN::%s%08d" % ref_synset
	else # we have a word and a sense, try to find a matching sense
	  begin
	    lemma, sense_number =
	      *WordNetSQL.query_first_row(
		"SELECT lemma, sense_number FROM senses
		 WHERE lemma=? AND ss_type=? AND synset_offset=?;",
		current_word, *ref_synset
	      )
	    # use sense number notation
	    m = "WN::#{lemma.gsub(/'/,'^')}.#{ref_synset[0]}.#{sense_number}"
	  rescue # no exact match
	    if (current_word_like.nil?) # no endings to match approximately
	      m = "WN::%s%08d" % ref_synset # fall back to synset
	    else
	      begin
		lemma, sense_number =
		  *WordNetSQL.query_first_row(
		    "SELECT lemma, sense_number FROM senses
		     WHERE lemma LIKE ? AND ss_type=? AND synset_offset=?;",
		    current_word_like, *ref_synset
		  )
		m = "WN::#{lemma.gsub(/'/,'^')}.#{ref_synset[0]}.#{sense_number}"
	      rescue # no match, fall back to synset
		m = "WN::%s%08d" % ref_synset
	      end
	    end
	  end
	end
      else
	current_word = capture.downcase.gsub(/-/,'_').gsub(/\^/,"'")
        # turn common endings into % for LIKE
	current_word_like = current_word.sub(/((i?e)?s|e[dn]|ing|er|est)((?=_)|$)/,'%')
	current_word_like = nil if (current_word_like == current_word)
      end
      m
    }
    l.sub!(/\n$/," ;#{comments}\n") unless (comments == '')
    print l
  }
rescue => e
  $stderr.puts "Error on input line #{$.}:"
  $stderr.puts e
  $stderr.puts e.backtrace
end
