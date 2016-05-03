module HashInitialized
  def initialize(h={})
    h.each_pair { |k, v| instance_variable_set('@' + k.to_s, v) }
  end
end

# Collection of information about a part of a sentence, meant to be taken
# together. Corresponds to a "tag" in TextTagger, and very roughly to Token in
# RASTA 1.x.
class Annotation
  include HashInitialized

  attr_reader(
    # character indices for the boundaries of the part we're annotating
    :start, :end,
    # string that start/end index into (stolen from UIMA's Subject OF Analysis)
    :sofa,
    # where this annotation came from
    :source,
    # stuff we need to carry through that we don't generally care about the
    # internal structure of
    :domain_specific_info
  )

  def length
    @end - @start
  end

  def text
    raise "bad @start/@end in #{self.class}:\n  @start=#{@start.inspect}\n  @end=#{@end.inspect}\n  @sofa.length=#{@sofa.length}\nsofa:\n#{@sofa}\n" if (@start.nil? or @end.nil? or not ((0..@sofa.length).include?(@start) and (0..@sofa.length).include?(@end)))
    @sofa[@start, self.length]
  end

  def lex
    text.strip
  end

  def normalized_word
    self.lex.downcase.gsub(/\s+/, ' ')
  end

  def inspect
    "#<#{self.class} start=#{@start} end=#{@end} text=#{self.text.inspect}>"
  end
  alias_method :to_s, :inspect

  # sort by inc. end then by dec. start (the same as TextTagger)
  def <=>(other)
    (@end <=> other.end) || (other.start <=> @start)
  end
  include Comparable

  def contains?(other)
    @sofa == other.sofa and
    @start <= other.start and
    @end >= other.end
  end

  def chosen? ; false ; end

  class Sentence < Annotation
  end

  class PartOfSpeech < Annotation
    attr_reader :penn_pos
    def tag_verb ; :pos ; end
  end

  class Sense < Annotation
    attr_reader :ont_type, :penn_pos
    attr_accessor :score
    attr_writer :chosen
    def chosen? ; @chosen ; end
  end

  TextTaggerArgMap = {
    :start => :start,
    :end => :end,
    :"penn-pos" => :penn_pos,
    :lftype => :ont_type,
    :source => :source,
    :"domain-specific-info" => :domain_specific_info,
    :score => :score
  }

  def self.from_text_tagger(tag, text)
    args = {sofa: text}
    TextTaggerArgMap.each_pair { |tt,da|
      # FIXME ont_types are inconsistently lists
      args[da] = tag[tt] if (tag.key?(tt))
    }
    raise "missing start/end for tag #{tag}" unless (args.key?(:start) and args.key?(:end))
    ( case tag[0]
	when :pos
	  PartOfSpeech
	when :sense
	  Sense
	when :sentence
	  Sentence
	else
	  raise "unhandled tag type: #{tag[0]}"
      end
    ).new(args)
  end

  def tag_verb
    self.class.to_s.sub(/^.*::/,'').downcase.intern
  end

  def to_text_tagger
    args = {lex: self.lex}
    TextTaggerArgMap.each_pair { |tt,da|
      iv = '@' + da.to_s
      if (instance_variable_defined?(iv))
	args[tt] = instance_variable_get(iv)
      end
    }
    KQML[self.tag_verb, args]
  end
end

# Unit of language with a set of Annotations, e.g. to be used for a POS window.
# Corresponds to Utt in RASTA 1.x.
class Sentence
  include HashInitialized

  attr_reader :annotations

  def self.from_text_tagger(tags, text)
    Sentence.new(
      annotations: tags.collect { |tag|
        Annotation.from_text_tagger(tag, text)
      }.sort
    )
  end

  # insert an annotation in its correct sorted position
  def add_annotation(annotation)
    # FIXME this searches linearly when we could do binary or even radix search
    @annotations.insert(@annotations.index { |a| annotation < a }, annotation)
  end
end

require 'set'

# Sequence of related Sentences, e.g. to be used for previous-sense context.
# Corresponds to Dialogue in RASTA 1.x.
class Discourse
  include HashInitialized

  attr_reader :sentences

  # Make a new Discourse object from a list of KQML TextTagger tags and the
  # text string they index into.
  def self.from_text_tagger(tags, text)
    sentences = []
    annotations =
      tags.to_a.select { |tag|
        [:pos, :sense, :sentence].include?(tag[0])
      }.collect { |tag|
        Annotation.from_text_tagger(tag, text)
      }.sort
    annotations.each { |sentence|
      next unless (sentence.kind_of?(Annotation::Sentence))
      sentences.push(
        Sentence.new(
	  annotations: annotations.select { |a| sentence.contains?(a) }
	)
      )
    }
    $stderr.puts "Warning: no sentences in discourse!" if (sentences.empty?)
    Discourse.new(sentences: sentences)
  end

  # Make a new Discourse object from a file in the xmi/ccp/ subdirectory of the
  # CRAFT 1.0 corpus.
  def self.from_craft(filename)
    tsv_in = IO.popen("xsltproc CRAFT-to-tsv.xsl \"#{filename}\"")
    sentence_annotations = []
    annotations = []
    sofa = nil
    in_sofa = false
    while (line = tsv_in.gets)
      if (line == "_BEGIN_SOFA_\n")
	in_sofa = true
	sofa = ''
	next
      elsif (in_sofa and line == "_END_SOFA_\n")
        in_sofa = false
	sofa = sofa[0,sofa.length-1] # remove extra newline
	next
      elsif (in_sofa)
        sofa += line
	next
      end
      fields = line.chomp.split(/\t/)
      case fields[0]
        when 'text' # should always come first
	  fields.size == 2 or raise "wrong number of fields"
	  sofa = fields[1]
	when 'pos'
	  fields.size == 4 or raise "wrong number of fields"
	  raise "no text found yet for file #{filename}" if (sofa.nil?)
	  start, finish = fields[1].to_i, fields[2].to_i
	  raise "zero-length pos tag in file #{filename}:\n#{line}" if (start == finish)
	  annotations <<
	    Annotation::PartOfSpeech.new(
	      start: start, end: finish, sofa: sofa,
	      penn_pos: [fields[3].intern]
	    )
	when 'sense'
	  fields.size == 4 or raise "wrong number of fields"
	  raise "no text found yet for file #{filename}" if (sofa.nil?)
	  start, finish = fields[1].to_i, fields[2].to_i
	  raise "zero-length sense tag in file #{filename}:\n#{line}" if (start == finish)
	  annotations <<
	    Annotation::Sense.new(
	      start: start, end: finish, sofa: sofa,
	      domain_specific_info: KQML[:drum, id: fields[3].intern]
	    )
	when 'sentence'
	  fields.size == 3 or raise "wrong number of fields"
	  raise "no text found yet for file #{filename}" if (sofa.nil?)
	  start, finish = fields[1].to_i, fields[2].to_i
	  raise "zero-length sentence tag in file #{filename}:\n#{line}" if (start == finish)
	  sentence_annotations <<
	    Annotation::Sentence.new(start: start, end: finish, sofa: sofa)
      end
    end
    raise "no text found for file #{filename}" if (sofa.nil?)
    annotations.sort!
    sentence_annotations.sort!
    Discourse.new(
      sentences:
	sentence_annotations.collect { |sentence|
	  Sentence.new(
	    annotations: annotations.select { |a| sentence.contains?(a) }
	  )
	}
    )
  end
end

# Set of Sentences or Discourses, e.g. to be read from a file/directory and
# used for training.
#class Corpus
#end
# just make specific kinds of corpora with #each defined

# CRAFT 1.0
class CraftCorpus
  include HashInitialized

  attr_reader :path

  def each
    return to_enum unless (block_given?)
    Dir["#{path}/xmi/ccp/all/*.txt.xmi"].each { |filename|
      yield Discourse.from_craft(filename)
    }
  end
end

