#!/usr/bin/ruby

$: << ENV['TRIPS_BASE'] + '/etc/WordNetSQL'
require 'word_net_sql'

$pos2trips = Hash[*%w{noun n verb v adj adj adv adv}]
# list from lexnames(5WN) manpage
$lexnames = %w{
adj.all
adj.pert
adv.all
noun.Tops
noun.act
noun.animal
noun.artifact
noun.attribute
noun.body
noun.cognition
noun.communication
noun.event
noun.feeling
noun.food
noun.group
noun.location
noun.motive
noun.object
noun.person
noun.phenomenon
noun.plant
noun.possession
noun.process
noun.quantity
noun.relation
noun.shape
noun.state
noun.substance
noun.time
verb.body
verb.change
verb.cognition
verb.communication
verb.competition
verb.consumption
verb.contact
verb.creation
verb.emotion
verb.motion
verb.perception
verb.possession
verb.social
verb.stative
verb.weather
adj.ppl
};

def write_pointer(out, pointer_name, target_synset_offset, target_ss_type, target_word_number)
  begin
    if (pointer_name == 'Pertains to noun / Derived from adjective')
      pointer_name =
	case (target_ss_type)
	  when 'n'; 'Pertains to'
	  when 'a','s'; 'Derived from'
	  else raise "Bogus WN pointer #{pointer_name} #{target_ss_type}%08d" % target_synset_offset
	end
    end 
    out.print("(> WN::#{pointer_name.gsub(/ /,'_')}")
    target_sense_keys = []
    if (target_word_number.nil?)
      out.print(" WN::%s%08d" % [target_ss_type, target_synset_offset])
    else
      target_sense_key =
        begin
	  WordNetSQL.query_first_value(
	    "SELECT sense_key FROM senses
	     WHERE synset_offset=? AND ss_type=? AND word_number=?;",
	    target_synset_offset, target_ss_type, target_word_number
	  )
	rescue
	  # if we didn't find the word_number in senses, it's probably a
	  # case-insensitive duplicate, and thus in the capitalization table
	  # instead
	  WordNetSQL.query_first_value(
	    "SELECT senses.sense_key
	     FROM senses JOIN capitalization USING (synset_offset, ss_type)
	     WHERE synset_offset=? AND ss_type=?
	       AND capitalization.word_number=?;",
	    target_synset_offset, target_ss_type, target_word_number)
	end
      unless (target_sense_keys.include?(target_sense_key))
        out.print(" WN::|#{target_sense_key}|")
	target_sense_keys.push(target_sense_key)
      end
    end
    out.puts(")")
  rescue Exception => e
    raise "Error writing DSL for pointer #{[pointer_name, target_synset_offset, target_ss_type, target_word_number].inspect}\n#{e.message}\n#{e.backtrace.join("\n")}\n"
  end
end

def write_dsl_for_lex_filenum(out, lex_filenum)
  WordNetSQL.db.execute("SELECT ss_type, synset_offset, gloss FROM synsets WHERE lex_filenum=?;", lex_filenum) { |ss_row|
    ss_type, synset_offset, gloss = *ss_row
    begin
      out.puts("(concept WN::%s%08d" % [ss_type, synset_offset])
      # TODO separate examples and definitions, add tags, provenance?
      out.puts("  (definition (text #{gloss.inspect}))")
      WordNetSQL.db.execute("SELECT pointer_name, target_synset_offset, target_ss_type, target_word_number FROM pointers NATURAL JOIN pointer_symbols WHERE source_synset_offset=? AND source_ss_type=? AND source_word_number IS NULL;", synset_offset, ss_type) { |ptr_row|
	out.print("  ")
	write_pointer(out, *ptr_row)
      }
      WordNetSQL.db.execute("SELECT sense_number, word_number, sense_key, lemma FROM senses WHERE synset_offset=? AND ss_type=?;", synset_offset, ss_type) { |sense_row|
	sense_number, word_number, sense_key, lemma = *sense_row
	lemma_symbol = lemma.gsub(/'/, '^')
	word_symbols = lemma_symbol.split(/_/).collect { |ws|
	  ws = "|#{ws.upcase}|" unless (ws =~ /^[A-Za-z^][0-9A-Za-z^\.-]*$/)
	  ws
	}.join(' ')
	out.print <<EOP
  (sense WN::|#{sense_key}|
    (alias WN::#{lemma_symbol}.#{ss_type}.#{sense_number})
    (word (#{word_symbols}))
EOP
	# TODO look for word numbers in capitalization table too?
	WordNetSQL.db.execute("SELECT pointer_name, target_synset_offset, target_ss_type, target_word_number FROM pointers NATURAL JOIN pointer_symbols WHERE source_synset_offset=? AND source_ss_type=? AND source_word_number=?;", synset_offset, ss_type, word_number) { |ptr_row|
	  out.print("    ")
	  write_pointer(out, *ptr_row)
	}
	out.puts "    )"
      }
      out.puts "  )"
      out.puts
    rescue Exception => e
      raise "Error writing DSL for WN::#{ss_type}%08d:\n#{e.message}\n#{e.backtrace.join("\n")}\n" % synset_offset
    end
  }
end

$lexnames.each_with_index { |lex_filename, lex_filenum|
  pos = lex_filename.sub(/\..*/,'')
  File.open("#{lex_filename}.lisp", "w") { |out|
    out.puts ";;;; AUTOMATICALLY GENERATED"
    out.puts %Q{(provenance WordNet (version "3.0") (filename "#{lex_filename}"))}
    out.puts "(pos #{$pos2trips[pos]})"
    write_dsl_for_lex_filenum(out, lex_filenum)
  }
}

