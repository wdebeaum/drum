Infinity = 1.0/0.0

def resource_filename(filename)
  File.dirname(__FILE__) + '/' + filename
end

Config = {
  #
  # features to use
  #
  
  # words to either side of the word to be tagged
  :adjacent_words => true,
  :adjacent_words_radii => [2,1],
  
  # sequence of parts of speech around the word to be tagged
  :pos_window => true,
  # if true, the POS sequence A B C D E will generate features A_B B C D D_E
  # for the POS window
  :join_pos => false,
  # how many POSs to include in the POS window on each side (the middle POS is
  # always included)
  :pos_window_radii => [1,1],
  # list of POSs to skip over when building the POS window
  :skip_poss => %w{JJ RB},
  # should we map the full Penn POS tag set to a simpler set (NN,VB,JJ,RB,O)?
  :simplify_pos => true,
  # list of files containing words whose POS we should erase (set to nil)
  :no_pos_words_files => %w{stopwords.vocab}.collect { |f| resource_filename(f) },

  # the sense tag we gave this word the last time we saw it in this dialogue
  :previous_sense_of_same_word => false,

  # the last sense tag we gave to any word within the dialogue
  :previous_sense_of_any_word => false,

  # the intrasentential features of this word the last time we saw it in this
  # dialogue
  :previous_local_features_of_same_word => false,

  # the sense tag we gave to this word most frequently so far in this dialogue
  :most_frequent_sense_of_same_word => false,

  # binary features indicating whether a previous instance of the same word in
  # this dialogue was tagged with a sense in a specific bin (bins can be any
  # sense tag, or :other)
  :previous_senses_bins => [],

  # add a condition to previous_senses_bins that the previous sense tag is (one
  # of) the most frequent in the dialogue so far (the frequency for the :other
  # bin is the maximum frequency of any of the senses that fall into it).
  :bin_most_frequent_senses => false,

  # nearby words of specific parts of speech

  # map from POS groups used for nearby_words_of_pos to Penn POS tags
  :pos_to_penns => {
    :noun	=> %w{NN NNS},
    :verb	=> %w{VB VBD VBG VBN VBP VBZ},
    :adj	=> %w{JJ JJR JJS},
    :adv_prep	=> %w{IN RB RBR RBS TO RP}
  },
  # map from POS groups to left and right radii for window to use as features
  :pos_to_word_radii => {
    :noun => [0,0],
    :verb => [0,0], # only used if :verb_neighborhood_type=>:radius
    :adj => [0,0],
    :adv_prep => [0,0]
  },

  # special settings for verb window
  :verb_neighborhood_type => :radius, # :nearest, :most_common, :least_common
  :verb_neighborhood_size => nil, # 1,2,3

  #
  # other settings
  #
  
  # number of parallel threads to use
  :num_threads => 3,
  
  # maximum number of instances to train a single word classifier on
  :instances_per_word => Infinity,

  # random seed to use when sampling word instances (so that runs on the same
  # data with different features can be more reliably compared). Set to nil for
  # default RNG behavior.
  :random_seed => 20140225,

  # number of words to each side any feature may look at (including skipped
  # words)
  :word_horizons => [Infinity, Infinity],

  # where model files live
  :model_dir => resource_filename('models'),
  
  # how should we divide up the models?
  # :for_all - make only one model for all words and classes
  # :per_word - make a model for each (normalized) word to be classified
  # :per_word_pos_pair - make a model for each (word, part of speech) pair
  # :per_class - make a binary classifier for each class
  :one_model => :for_all,

  # should we count the POS from the input as part of the word when naming
  # classifiers?
  :include_pos_in_word => false,

  # should we overwrite sense tags that were already in the input (true), or
  # preserve them (false)?
  :overwrite_existing_sense_tags => true,

  # should we output only the most likely sense tag in sid (true), or output
  # the full probability distribution in sdist (false)?
  :output_only_top_sense => false
}

class Hash
  # like Hash#merge, but recurse on Hash values
  def merge_rec(other)
    self.merge(other) { |key, self_val, other_val|
      if (Hash === self_val and Hash === other_val)
	self_val.merge_rec(other_val)
      else
	other_val
      end
    }
  end
end

def config_has_discourse_features?(config=Config)
  config.values_at(
    :previous_sense_of_same_word,
    :previous_sense_of_any_word,
    :previous_local_features_of_same_word,
    :most_frequent_sense_of_same_word
  ).any? || (not config[:previous_senses_bins].empty?)
end

# configuration with no features and no limits
ConfigNoFeatures = Config.merge_rec({
  :adjacent_words => false,
  :pos_window => false,
  :join_pos => false,
  :skip_poss => [],
  :previous_senses_file => nil,
  :previous_sense_of_same_word => false,
  :previous_sense_of_any_word => false,
  :previous_local_features_of_same_word => false,
  :most_frequent_sense_of_same_word => false,
  :previous_senses_bins => [],
  :pos_to_word_radii => {
    :noun => [0,0],
    :verb => [0,0],
    :adj => [0,0],
    :adv_prep => [0,0]
  },
  :instances_per_word => Infinity,
  :word_horizons => [Infinity, Infinity],
})

