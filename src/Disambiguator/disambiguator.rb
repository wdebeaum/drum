#!/usr/bin/env ruby

require 'TripsModule/trips_module'
require_relative 'config'
require_relative 'intrasentential_features'
require_relative 'discourse_features'
require_relative 'aode'
require_relative 'models'
require_relative 'kqml_values_at'

class Disambiguator
  include TripsModule

  def initialize(argv)
    super
    @name = :Disambiguator
    init()
    add_handler(KQML.from_s('(request &key :content (disambiguate . *))'),
    		method(:handle_disambiguate))
    if (Config[:one_model] == :for_all)
      @model = load_model_file("#{Config[:model_dir]}/ALL.model")
    else
      @models = Models.new(Config[:model_dir])
    end
  end

  def handle_disambiguate(msg)
    tags, text = msg[:content].values_at(:tags, :text)
    discourse = Discourse.from_text_tagger(tags, text)
    new_annotations = disambiguate_discourse(discourse)
#    new_annotations.select! { |a| a.chosen? } 
    send_msg(KQML[:reply, content: new_annotations.collect { |a| a.to_text_tagger }] + KQML.in_reply_to(msg))
  end

  def disambiguate_discourse(discourse)
    new_annotations = []
    context = DiscourseContext.new
    discourse.sentences.each { |sentence|
      context.current_sentence = sentence
      sense_annotations =
        sentence.annotations.
	select { |a| a.kind_of?(Annotation::Sense) }.
	uniq { |a| [a.start, a.end] }
      featureses = get_intrasentential_features(sentence, sense_annotations)
      $stderr.puts "featureses = #{featureses.inspect}"
      sense_annotations.each_with_index { |annotation, index|
        begin
	  $stderr.puts "annotation = #{annotation.inspect}"
	  features = featureses[index] + context.features(annotation)
	  $stderr.puts "features = #{features.inspect}"
	  distribution = {}
	  if (Config[:one_model] == :per_class)
	    @models.each_pair { |ont_type, model|
	      binary_distribution = model.distribution_for_instance(features)
	      distribution[ont_type] = binary_distribution[ont_type]
	    }
	    distribution = normalize_distribution(distribution)
	  else
	    case Config[:one_model]
	      when :for_all
		model = @model
	      when :per_word
		model = @models[annotation.normalized_word]
	      when :per_word_pos_pair
		model = @models[annotation.normalized_word + '__' + simplify_pos(annotation)] # FIXME is this a POS annotation or a sense?
	      else raise "bogus value for Config[:one_model]: #{Config[:one_model].inspect}"
	    end
	    distribution = model.distribution_for_instance(features)
	  end
	  chosen_sense = distribution.each_pair.max_by { |p| p[1] }[0]
	  $stderr.puts "distribution = #{distribution.inspect}"
	  distribution.each_pair { |sense, score|
	    new_annotation =
	      Annotation::Sense.new(
		start: annotation.start, end: annotation.end,
		sofa: annotation.sofa,
		ont_type: [sense],
		score: score,
		chosen: (sense == chosen_sense)
	      )
	    sentence.add_annotation(new_annotation)
	    new_annotations.push new_annotation
	  }
	rescue
	  $stderr.puts $!
	end
      }
    }
    return new_annotations
  end
end

Disambiguator.new(ARGV).run if ($0 == __FILE__)

