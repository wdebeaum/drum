require_relative 'intrasentential_features'

TrivialAnnotation =
  Annotation::PartOfSpeech.new(penn_pos: :NN, start: 0, end: 1, sofa: 'a')
TrivialSentence = Sentence.new(annotations: [TrivialAnnotation])
# return the number of features that would be returned by
# get_intrasentential_features in the current configuration
def num_intrasentential_features
  # TODO memoization?
  get_intrasentential_features(TrivialSentence,[TrivialAnnotation])[0].size
end

# Track state needed for features that cross sentence boundaries.
# FIXME most of this ignores the possibility of overlapping tags, etc.
class DiscourseContext
  def initialize
    @current_sentence = nil
    @word_to_last_tag = {}
    # FIXME features get computed multiple times for the same token
    @word_to_last_features = {}
    @last_tagged_word = nil
    @word_to_sense_to_count = {}
  end

  # set the current sentence
  def current_sentence=(sentence)
    unless (@current_sentence.nil?)
      chosen_ones = @current_sentence.annotations.select { |a| a.chosen? }
      if (Config[:previous_sense_of_same_word])
	chosen_ones.each { |annotation|
	  @word_to_last_tag[annotation.normalized_word] = annotation.ont_type
	}
      end
      if (Config[:previous_sense_of_any_word] and not chosen_ones.empty?)
	annotation = chosen_ones[-1]
	@last_tagged_word =
	  annotation.normalized_word + '#' + annotation.ont_type.to_s
      end
      if (Config[:previous_local_features_of_same_word])
	featureses =
	  get_intrasentential_features(@current_sentence, chosen_ones)
	chosen_ones.each_with_index { |annotation, index|
	  @word_to_last_features[annotation.normalized_word] = featureses[index]
	}
      end
      if (Config[:most_frequent_sense_of_same_word] or
	  not Config[:previous_senses_bins].empty?)
	chosen_ones.each { |annotation|
	  word, sense = annotation.normalized_word, annotation.ont_type
	  @word_to_sense_to_count[word] ||= {}
	  @word_to_sense_to_count[word][sense] ||= 0
	  @word_to_sense_to_count[word][sense] += 1
	}
      end
    end
    @current_sentence = sentence
  end

  # return the last Annotation in the current sentence before annotation for
  # which the block returns true
  def find_prev_annotation(annotation, &block)
    @current_sentence.
      annotations.
      select { |a| a.end <= annotation.start }.
      reverse.
      find(&block)
  end

  # return the last sense tag seen in this Discourse for the word annotated by
  # annotation (before that annotation)
  def previous_sense_of_same_word(annotation)
    word = annotation.normalized_word
    prev_annotation =
      find_prev_annotation(annotation) { |a|
        a.chosen? and a.normalized_word == word
      }
    if (prev_annotation.nil?)
      @word_to_last_tag[word]
    else
      prev_annotation.ont_type
    end
  end

  # return the last local feature list seen in this Discourse for the word
  # annotated by annotation (before that annotation)
  def previous_local_features_of_same_word(annotation)
    word = annotation.normalized_word
    prev_annotation =
      find_prev_annotation(annotation) { |a|
	a.normalized_word == word
      }
    if (prev_annotation.nil?)
      @word_to_last_features[word] ||
        ([nil] * num_intrasentential_features())
    else
      get_intrasentential_features(@current_sentence, [prev_annotation])[0]
    end
  end

  # return the last sense-tagged word seen in this Discourse before the given
  # Annotation
  def previous_sense_of_any_word(annotation)
    prev_annotation = find_prev_annotation(annotation) { |a| a.chosen? }
    if (prev_annotation.nil?)
      @last_tagged_word
    else
      prev_annotation.normalized_word + '#' + prev_annotation.ont_type.to_s
    end
  end

  # get the number of times each sense of the current word has been seen before
  # this annotation
  def sense_to_count_at(annotation)
    word = annotation.normalized_word
    sense_to_count = (@word_to_sense_to_count[word] || {}).dup
    @current_sentence.annotations.each { |a|
      break if (a.end > annotation.start)
      next unless (a.chosen? and a.normalized_word == word)
      sense = a.ont_type
      sense_to_count[sense] ||= 0
      sense_to_count[sense] += 1
    }
    return sense_to_count
  end

  def most_frequent_sense_of_same_word(annotation)
    sense_to_count = sense_to_count_at(annotation)
    # get a sense with the maximum count
    top_sense, top_count = *sense_to_count.each_pair.max_by { |p| p[1] }
    # only return it if it exists and is the only sense with the maximum count
    unless (top_sense.nil?)
      sense_to_count.delete(top_sense)
      return top_sense if ((sense_to_count.values.max || 0) < top_count)
    end
    return nil
  end

  def previous_senses_bins(annotation)
    sense_to_count = sense_to_count_at(annotation)
    count_threshold =
      if (Config[:bin_most_frequent_senses])
        sense_to_count.values.max || 1
      else
	1
      end
    bin_counts =
      Config[:previous_senses_bins].collect { |bin|
	if (bin == :other)
	  ( (sense_to_count.keys -
	      Config[:previous_senses_bins].collect { |b| b.to_s }
	    ).
	    collect { |sid| sense_to_count[sid] || 0 } + [0]).
	    max
	else
	  sense_to_count[bin.to_s] || 0
	end
      }
    bin_counts.collect { |count| count >= count_threshold }
  end

  # get feature values for the given Annotation in the current Sentence
  def features(annotation)
    [
      *(Config[:previous_sense_of_same_word] ?
          [previous_sense_of_same_word(annotation)] : []),
      *(Config[:previous_sense_of_any_word] ?
          [previous_sense_of_any_word(annotation)] : []),
      *(Config[:previous_local_features_of_same_word] ?
          previous_local_features_of_same_word(annotation) : []),
      *(Config[:most_frequent_sense_of_same_word] ?
          [most_frequent_sense_of_same_word(annotation)] : []),
      *((not Config[:previous_senses_bins].empty?) ?
          previous_senses_bins(annotation) : [])
    ].collect { |feature|
      # make sure all feature values are symbols
      case feature
        when nil
	  :NIL
	when Symbol
	  feature
	else
	  feature.to_s.intern
      end
    }
  end
end

