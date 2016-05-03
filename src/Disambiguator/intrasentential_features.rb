require_relative 'config'
require_relative 'corpus'

# given some names of .vocab files, return the set of words in them
def read_vocab_files(*filenames)
  filenames.collect_concat { |file|
    File.open(file,'r').each_line.to_a
  }.collect { |l| l.strip }.to_set
end

def simple_pos(annotation)
  # remove POS if word is in $no_pos_words (loading if necessary)
  unless (Config[:no_pos_words_files].empty?)
    $no_pos_words ||= read_vocab_files(*Config[:no_pos_words_files])
    return nil if ($no_pos_words.include?(annotation.normalized_word))
  end
  # simplify non-nil POS to one of NN, VB, JJ, RB, O
  if (Config[:simplify_pos] and
      not (annotation.penn_pos.nil? or annotation.penn_pos.empty?))
    return (
      annotation.penn_pos[0]. # FIXME might have more than one POS
      to_s.
      sub(/^VB[DGNPZ]$/,'VB').
      sub(/^(RB|JJ)[RS]$/) { |m| $1 }.
      sub(/^NN[PS]*$/,'NN').
      sub(/^(WP|PRP|PO)\$$/,'O').
      sub(/^(CD|DT|PDT|IN|FW|RP|UH|SYM|CC|EX|WP|TO|PO|WRB|PRF|PRP|WDT)$/,'O').
      sub(/^MD$/,'VB').
      intern
    )
  end
  # no simplification happened, just return the original POS
  return annotation.penn_pos[0] # FIXME see above
end

# Turn POS annotations into three corresponding lists: one of word strings,
# one of POS tags, and one of start character indices.
def words_poss_starts(sentence)
  words = []
  poss = []
  starts = []
  sentence.annotations.each { |a|
    if (a.kind_of?(Annotation::PartOfSpeech) and
        (starts.empty? or starts[-1] < a.start))
      words.push(a.normalized_word)
      poss.push(simple_pos(a))
      starts.push(a.start)
    end
  }
  return [words, poss, starts]
end

# get the list of parts of speech to use for making the POS window features
def pos_window(poss, center_index)
  center = poss[center_index]
  left_radius, right_radius = *Config[:pos_window_radii]
  left = []
  i = center_index - 1
  while (i >= 0 and center_index - i <= Config[:word_horizons][0] and
         left.size < left_radius)
    left.unshift(poss[i]) unless (Config[:skip_poss].include?(poss[i]))
    i -= 1
  end
  i = 1
  while (left.size < left_radius)
    left.unshift('BOS' + i.to_s)
    i += 1
  end
  right = []
  i = center_index + 1
  while (i < poss.size and i - center_index <= Config[:word_horizons][1] and
         right.size < right_radius)
    right << poss[i] unless (Config[:skip_poss].include?(poss[i]))
    i += 1
  end
  i = 1
  while (right.size < right_radius)
    right << 'EOS' + i.to_s
    i += 1
  end
  return left + [center] + right
end

# given a list of parts of speech, return the list of features for the POS
# window
def pos_window_features(pos_window)
  if (Config[:join_pos])
    so_far = []
    left =
      (Config[:pos_window_radii][0] - 1).downto(0).
      collect { |i|
        so_far.unshift(pos_window[i])
	so_far.join('_')
      }
    center = pos_window[Config[:pos_window_radii][0]]
    so_far = []
    right =
      (Config[:pos_window_radii][0] + 1).upto(pos_window.size-1).
      collect { |i|
        so_far << pos_window[i]
	so_far.join('_')
      }
    return left + [center] + right
  else
    return pos_window
  end
end

# Return an array of size left_radius + right_radius containing the words whose
# corresponding poss are in selected_poss, and are the first
# (left|right)_radius such words away from the center index in that direction.
def nearby_words_of_pos(selected_poss, center, left_radius, right_radius, words, poss)
  left = []
  i = center - 1
  while (i >= 0 and center - i <= Config[:word_horizons][0] and
         left.size < left_radius)
    left.unshift(words[i]) if (selected_poss.include?(poss[i]))
    i -= 1
  end
  right = []
  i = center + 1
  while (i < words.size and i - center <= Config[:word_horizons][1] and
         right.size < right_radius)
    right.push(words[i]) if (selected_poss.include?(poss[i]))
    i += 1
  end
  return [nil]*(left_radius - left.size) + left + right + [nil]*(right_radius - right.size)
end

DummyPOS = Object.new
class <<DummyPOS
  def include?(pos); true; end
  def [](i); nil; end
end

def adjacent_words(words, center)
  nearby_words_of_pos(
    DummyPOS, center, *Config[:adjacent_words_radii], words, DummyPOS
  )
end

# Enumerate the indices of the selected parts of speech in order from those
# nearest the center index to those farthest away. Prefer the left side in case
# of ties.
def each_nearby_index_of_pos(selected_poss, center, poss)
  return enum_for(__method__, selected_poss, center, poss) unless (block_given?)
  offset = -1
  left_done = false
  right_done = false
  begin
    if (selected_poss.include?(poss[center + offset]))
      yield center + offset
    end
    if (left_done)
      offset += 1
    elsif (right_done)
      offset -= 1
    elsif (offset > 0)
      offset = -(offset+1)
    else
      offset = -offset
    end
    if (center + offset < 0 or -offset > Config[:word_horizons][0])
      left_done = true
      offset = -offset
    end
    if (center + offset >= poss.size or offset > Config[:word_horizons][1])
      right_done = true
      offset = -(offset+1)
    end
  end until (left_done and right_done)
end

# FIXME This should be more configurable, and maybe based on the training data.
# Right now I'm just using lemma.num from Kilgarriff's BNC frequency lists:
# http://www.kilgarriff.co.uk/bnc-readme.html
$verb_to_freq = nil
def verb_freq(verb)
  $verb_to_freq ||=
    Hash[
      File.open(resource_filename('lemma.num'),'r').
      each_line.
      collect { |line| line.chomp.split }.
      select { |rank, freq, word, pos| pos == 'v' }.
      collect { |rank, freq, word, pos| [word, freq.to_i] }
    ]
  return ($verb_to_freq[verb] || 1)
end

module Enumerable
  def stable_sort_by
    each_with_index.sort_by { |e, i| [yield(e), i] }.collect { |e, i| e }
  end
end

# Return an array of feature vectors, one vector for each of the annotations.
def get_intrasentential_features(sentence, annotations)
  all_words, all_poss, starts = words_poss_starts(sentence)
#  $stderr.puts "all_words = #{all_words.inspect}"
#  $stderr.puts "all_poss = #{all_poss.inspect}"
#  $stderr.puts "starts = #{starts.inspect}"
  annotations.collect { |annotation|
    words = nil
    poss = nil
    i = nil
    features = []
    begin
      first_i = nil
      last_i = nil
      starts.each_with_index { |start, start_index|
        first_i = start_index if (annotation.start >= start)
        if (annotation.end >= start)
	  last_i = start_index
	else
	  break
	end
      }
      raise "WTF" if (first_i.nil? or last_i.nil?)
      num_i = last_i + 1 - first_i
      raise "WTF" if (num_i < 1)
#      annotation_indices =
#	starts.each_with_index.select { |start, start_index|
#	  annotation.start <= start and annotation.end > start
#	}.collect { |start, start_index| start_index }
#      num_i = annotation_indices.length
#      first_i = annotation_indices[0]
      if (num_i == 1) # simple case, annotation is for 1 word only
	words = all_words
	poss = all_poss
      else # annotation covers multiple words, join them
	words =
	  all_words.take(first_i) +
	  [all_words[first_i, num_i].join(' ')] +
	  all_words.drop(first_i + num_i)
	poss =
	  all_poss.take(first_i) +
	  [all_poss[first_i, num_i].join('_')] +
	  all_poss.drop(first_i + num_i)
      end
      i = first_i
    rescue
      $stderr.puts "annotation = #{annotation.inspect}"
      $stderr.puts "all_words = #{all_words.inspect}"
      $stderr.puts "first_i = #{first_i.inspect}"
      $stderr.puts "num_i = #{num_i.inspect}"
      raise
    end

    # words on either side
    features += adjacent_words(words, i) if (Config[:adjacent_words])
    
    # POS window
    features +=
      pos_window_features(pos_window(poss, i)) if (Config[:pos_window])
    
    Config[:pos_to_penns].
    each_pair.
    # make sure features are always in the same order
    sort_by { |pos, penns| pos }.
    each { |pos, penns|
      if (pos != :verb and Config[:pos_to_word_radii].key?(pos))
	features +=
	  nearby_words_of_pos(penns, i, *Config[:pos_to_word_radii][pos],
	  		      words, poss)
      end
    }
    
    # verb neighborhood features
    verb_features = nil
    if (Config[:verb_neighborhood_type] == :radius)
      # get a specific number of verbs to the left and right
      features +=
	nearby_words_of_pos(Config[:pos_to_penns][:verb], i,
			    *Config[:pos_to_word_radii][:verb],
			    words, poss)
    else
      # get verb indices in order of how close they are
      verb_indices =
	each_nearby_index_of_pos(
	  Config[:pos_to_penns][:verb], i, poss
	)
      # maybe stably sort them by another criterion
      case Config[:verb_neighborhood_type]
	when :nearest
	  # do nothing
	when :most_common
	  verb_indices =
	    verb_indices.stable_sort_by { |i|
	      -verb_freq(utt.tokens[i].other[:wn_lemma])
	    }
	when :least_common
	  verb_indices =
	    verb_indices.stable_sort_by { |i|
	      verb_freq(utt.tokens[i].other[:wn_lemma])
	    }
	else
	  raise "Invalid verb neighborhood type"
      end
      # make sure we have the right number of features, and add them
      verb_indices = verb_indices.take(Config[:verb_neighborhood_size])
      features += verb_indices.collect { |i| words[i] } +
	[nil] * (Config[:verb_neighborhood_size] - verb_indices.size)
    end

    features.collect { |feature| (feature.nil? ? :NIL : feature.intern) }
  }
end

