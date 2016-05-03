#!/usr/bin/env ruby

require 'TripsModule/trips_module'
require_relative 'config'
require_relative 'intrasentential_features'
require_relative 'discourse_features'
require_relative 'aode'
require_relative 'models'
require_relative 'kqml_values_at'

class Trainer
  include TripsModule

  def initialize(argv)
    super
    @name = :Trainer
    @id_to_ont_type = {}
    init()
    add_handler(KQML.from_s('(request &key :content (train . *))'),
    		method(:handle_train))
  end

  def handle_train(msg)
    corpus, model_dir = msg[:content].values_at(:corpus, :model_dir)
    model_dir ||= Config[:model_dir]
    # for now, assume CRAFT corpus format
    corpus = CraftCorpus.new(path: corpus)
    train_on_corpus_without_tt(corpus, model_dir)
    send_msg(KQML[:reply, content: [:ok]] + KQML.in_reply_to(msg))
  end

  def train_on_corpus_without_tt(corpus, model_dir)
    if (Config[:one_model] == :for_all)
      model = AODE.new
      corpus.each { |discourse|
        context = DiscourseContext.new
        discourse.sentences.each { |sentence|
	  context.current_sentence = sentence
	  sense_annotations =
	    sentence.annotations.
	    select { |a| a.kind_of?(Annotation::Sense) }.
	    collect { |a| fill_ont_type_from_dsi(a) }
	  # TODO remove duplicate spans?
	  # also, how to choose which sense to take?
	  # still need to set chosen=true in order for prev sense features to
	  # work
	  featureses = get_intrasentential_features(sentence, sense_annotations)
	  sense_annotations.each_with_index { |annotation, index|
	    features =
	      [annotation.ont_type] +
	      featureses[index] +
	      context.features(annotation)
	    model.train_instance(features)
	  }
	}
      }
      model.finish_training!
      save_model_file(model, "#{model_dir}/ALL.model")
    else
      # first get the list of instances (feature lists) to train each model with
      model_to_instances = {}
      corpus.each { |discourse|
        context = DiscourseContext.new
        discourse.sentences.each { |sentence|
	  context.current_sentence = sentence
	  sense_annotations =
	    sentence.annotations.
	    select { |a| a.kind_of?(Annotation::Sense) }.
	    collect { |a| fill_ont_type_from_dsi(a) }
	  pos_annotations =
	    sentence.annotations.
	    select { |a| a.kind_of?(Annotation::PartOfSpeech) }
	  # TODO see above
	  featureses = get_intrasentential_features(sentence, sense_annotations)
	  sense_annotations.each_with_index { |annotation, index|
	    features =
	      [annotation.ont_type] +
	      featureses[index] +
	      context.features(annotation)
	    model_key =
	      case Config[:one_model]
		when :per_word
		  annotation.normalized_word
		when :per_word_pos_pair
		  pos_annotation = pos_annotations.find { |a| annotation == a }
		  annotation.normalized_word +
		    (pos_annotation.nil? ? '' :
		      ('#' + pos_annotation.penn_pos[0].to_s))
		when :per_class
		  annotation.ont_type
		else
		  raise "bogus :one_model setting: #{Config[:one_model]}"
	      end
	    (model_to_instances[model_key] ||= []) << features
	  }
	}
      }
      # TODO Config[:instances_per_word]
      # then train models one by one (rather than keeping all of them in memory
      # at once)
      model_to_instances.each_pair { |model_key, instances|
        model = AODE.new
	instances.each { |features| model.train_instance(features) }
	model.finish_training!
	save_model_file(model, "#{model_dir}/#{Models.basename_for_key(model_key)}.model")
      }
    end
  end

  def get_ont_type_from_dsl_mapping(id, annotation)
    return @id_to_ont_type[id] if (@id_to_ont_type.key?(id))
    ont_type = :"ont::referential-sem"
    # TODO batch this call somehow
    dsl_reply = send_and_wait(
      KQML[:request, receiver: :DeepSemLex, content:
	KQML[:"get-trips-ont-mappings", :"concept-ids" => [id.intern]]])
    ont_types = dsl_reply[:mappings].to_a.collect { |m| m[:to] }.uniq
    case ont_types.size
      when 0
	$stderr.puts "Warning: got no ont_types from DSL for id #{id}, using ONT::referential-sem for annotation #{annotation}"
      when 1
	ont_type = ont_types[0]
      else
	$stderr.puts "Warning: got multiple ont_types from DSL for id #{id}, using the first for annotation #{annotation}: #{ont_types.inspect}"
	ont_type = ont_types[0]
    end
    @id_to_ont_type[id] = ont_type
    return ont_type
  end

  # given a sense annotation with no ont_type, return a version with an
  # ont_type derived from its domain_specific_info (roughly the same way
  # TextTagger does it, calling on DSL to do the mapping from IDs to ONT types)
  def fill_ont_type_from_dsi(annotation)
    if (annotation.domain_specific_info.kind_of?(KQML) and
        annotation.domain_specific_info.positional_arguments[0] == :drum and
        annotation.domain_specific_info.key?(:id))
      id = annotation.domain_specific_info[:id].to_s
      id.sub!(/^CL:/i, 'CO:') # can't use CL because that's Common Lisp
      ont_type = :"ont::referential-sem"
      case id
        when /^(CO|CHEBI|GO|SO):/i
	  ont_type = get_ont_type_from_dsl_mapping(id, annotation)
	when /^NCBITaxon:/i
	  ont_type = :"ont::organism"
	when /^EntrezGene:/i
	  ont_type = :"ont::gene"
	when /^PR:/i
	  ont_type = :"ont::protein"
	else
	  raise "unknown package for dsi id: #{id}"
      end
      Annotation::Sense.new(
        start: annotation.start, end: annotation.end, sofa: annotation.sofa,
	domain_specific_info: annotation.domain_specific_info,
	penn_pos: annotation.penn_pos,
	ont_type: ont_type
      )
    else
      raise "no dsi id?!"
    end
  end

  def train_on_corpus_with_tt(corpus, model_dir)
    if (Config[:one_model] == :for_all)
      model = AODE.new
      corpus.each { |gold_discourse|
        sofa = gold_discourse.sentences[0].annotations[0].sofa
	tt_reply = send_and_wait(
	  KQML[:request, receiver: :TextTagger, content:
	    KQML[:tag, text: sofa, format: :native]])
	tt_discourse =
	  Discourse.from_text_tagger(tt_reply.positional_arguments)
        context = DiscourseContext.new
	# FIXME! assumes gold and TT sentence segmentation are identical
        gold_discourse.sentences.zip(tt_discourse.sentences) { |gold_sentence, tt_sentence|
	  context.current_sentence = gold_sentence
	  # TODO
	}
      }
      save_model_file(model, "#{model_dir}/ALL.model")
    else
      raise "TODO"
    end
  end
end

Trainer.new(ARGV).run if ($0 == __FILE__)

