# A KQML list with separate positional and keyword arguments. Nested lists,
# strings, symbols, numbers, booleans, and nil are translated back and forth
# between KQML and Ruby equivalents. Note that Ruby false goes to KQML nil, but
# KQML nil goes to Ruby nil, so there is some loss of information there.
# Fortunately nil will still work right in Ruby conditionals. Also note that
# since keyword arguments are stored in a Hash, ordering and duplicates are not
# preserved. And, of course, comments (between ; and newline) and whitespace
# are not preserved.
class KQML
  attr_reader(
    :positional_arguments, # :: Array
    :keyword_arguments # :: Hash
  )

  # Initialize with the given separate positional and keyword arguments,
  # translating nested Arrays and Hashes to KQMLs as well.
  def initialize(pa, ka)
    @positional_arguments = pa.collect { |a|
      case a
        when Array
	  KQML[*a]
        when Hash
          KQML[a]
        else
	  a
      end
    }
    @keyword_arguments = Hash[*ka.to_a.collect { |key, value|
      [key.to_s.sub(/^:/,'').intern,
	case value
	  when Array
	    KQML[*value]
	  when Hash
	    KQML[value]
	  else
	    value
	end
      ]
    }.flatten]
  end

  # Like KQML.new with positional args splatted. If the last argument is a
  # Hash, it's treated as keyword arguments; otherwise it's the last positional
  # argument.
  def self.[](*args)
    if (Hash === args[-1])
      KQML.new(args[0, args.length-1], args[-1])
    else
      KQML.new(args, {})
    end
  end

  # With an Integer index, get the positional argument; with a Symbol, get the
  # keyword argument.
  def [](index)
    case index
      when Integer
        @positional_arguments[index]
      when Symbol
        @keyword_arguments[index]
      else
	raise "Bogus KQML index type: #{index.class}"
    end
  end

  # See #[].
  def []=(index, value)
    case index
      when Integer
        @positional_arguments[index] = value
      when Symbol
        @keyword_arguments[index] = value
      else
	raise "Bogus KQML index type: #{index.class}"
    end
  end

  def key?(key)
    @keyword_arguments.key?(key)
  end

  def values_at(*keys)
    keys.collect { |k| self[k] }
  end

  def empty?
    @positional_arguments.empty? and @keyword_arguments.empty?
  end

  def to_kqml
    self
  end

  def to_kqml_string
    '(' + (
      @positional_arguments.collect { |a| a.to_kqml_string } +
      @keyword_arguments.to_a.collect { |key, value|
        key.to_kqml_keyword + ' ' + value.to_kqml_string
      }.sort
    ).join(' ') + ')'
  end

  def inspect
    to_kqml_string
  end

  def to_s
    to_kqml_string
  end

  def to_a
    if (@keyword_arguments.empty?)
      @positional_arguments
    else
      @positional_arguments + [@keyword_arguments]
    end
  end

  def eql?(other)
    self.class.eql?(other.class) and
    @positional_arguments.eql?(other.positional_arguments) and
    @keyword_arguments.eql?(other.keyword_arguments)
  end

  def hash
    @positional_arguments.hash + @keyword_arguments.hash
  end

  # Concatenate positional and keyword arguments separately.
  def +(other)
    KQML.new(@positional_arguments + other.positional_arguments,
             @keyword_arguments.merge(other.keyword_arguments))
  end

  # A useful function for replying to a message.
  # Usage: write_kqml(KQML[:tell, :content => [...]] + KQML.in_reply_to(msg))
  def self.in_reply_to(msg)
    return (msg.key?(:sender)?
             KQML[:receiver => msg[:sender]] : KQML[]) +
           (msg.key?(:"reply-with")?
             KQML[:"in-reply-to" => msg[:"reply-with"]] : KQML[])
  end

  # Subscription pattern matching.
  def ===(other)
    return false unless (self.class.eql?(other.class))
    num_args =
      if (@positional_arguments[-1] == :"&key")
	return false unless (@positional_arguments.length == other.positional_arguments.length + 1)
	@positional_arguments.length-1
      elsif (@positional_arguments[-2,2] == [:".", :"*"])
	@positional_arguments.length-2
      else
	return false unless (@positional_arguments.length == other.positional_arguments.length)
	@positional_arguments.length
      end
    (0...num_args).to_a.all? { |i|
      if (KQML === self[i])
	self[i] === other[i]
      elsif (:"*" == self[i])
        true
      else
	self[i] == other[i]
      end
    } and
    @keyword_arguments.keys.all? { |k|
      other.keyword_arguments.key?(k) and
      if (KQML === self[k])
	self[k] === other[k]
      elsif (:"*" == self[k])
        true
      else
	self[k] == other[k]
      end
    }
  end
end

class Object
  # Override this to set the KQML string representation of your object, or
  # override #to_kqml (which this calls if it exists) to return something that
  # has a #to_kqml_string method. You may also want to define #from_kqml and/or
  # #from_kqml_string class methods to do the inverse operation.
  def to_kqml_string
    if (respond_to?(:to_kqml))
      to_kqml.to_kqml_string
    else
      inspect
    end
  end

  def to_kqml_keyword
    s = to_s
    ((s !~ /^:/ ? ':' : '') + s).intern.to_kqml_string
  end
end

class Symbol
  def to_kqml_string
    package, name = nil, nil
    if (to_s =~ /^:/)
      package, name = 'KEYWORD', $'
    elsif (to_s =~ /::/)
      package, name = $`, $'
    else
      name = to_s
    end
    package = '|' + package + '|' if ((not package.nil?) and package =~ /[\s:`,'"\(\)]/)
    name = '|' + name + '|' if (name =~ /[\s:`,'"\(\)]/)
    case package
      when nil
        name
      when 'KEYWORD'
        ':' + name
      else
	package + '::' + name
    end
  end
end

class String
  def to_kqml_string
    '"' + self.gsub(/\\/){"\\\\"}.gsub(/"/,"\\\"") + '"'
  end
end

class TrueClass
  def to_kqml_string
    't'
  end
end

class FalseClass
  def to_kqml_string
    'nil'
  end
end

module Enumerable
  def to_kqml_string
    '(' + collect { |e| e.to_kqml_string }.join(' ') + ')'
  end
end

class Hash
  def to_kqml_string
    KQML[self].to_kqml_string
  end
end

require 'treetop'
Treetop.load File.dirname(__FILE__) + '/kqml_parser'

class KQML
  Parser = KQMLSyntaxParser.new
  ParserMutex = Mutex.new
  # Parse KQML from the given string and return its Ruby equivalent.
  def self.from_s(s)
    ParserMutex.synchronize {
      tree = Parser.parse(s)
      raise "KQML parse error: #{Parser.failure_reason}" if (tree.nil?)
      return tree.to_ruby
    }
  end
end

class IO
  # Write the KQML representation of an object to self. Calls
  # kqml.to_kqml_string, which by default calls kqml.to_kqml to get a KQML
  # object or one of the more basic types that define #to_kqml_string directly.
  def write_kqml(kqml)
    write(kqml.to_kqml_string + "\n")
  end

  def obviously_will_not_parse?(str)
    # won't parse if there's an odd number of unescaped "s
    # (FIXME "s can appear inside pipequotes, and vice versa)
    return ((str.scan(/(?<!\\)(\\\\)*\"/).size % 2) != 0)
  end

  # Read from self, parsing one KQML item, and return its Ruby equivalent.
  # Assumes this is the only way you'll ever read from this IO, since it reads
  # line-by-line and stores the remainder in an instance variable.
  def read_kqml
    @kqml_parser ||= KQMLSyntaxParser.new # avoid using KQML::Parser for concurrency
    begin
      @unparsed_input ||= gets
      return nil if (@unparsed_input.nil?)
      tree = nil
      # FIXME assumes all messages start and end on line boundaries
      ownp = obviously_will_not_parse?(@unparsed_input)
      unless (ownp)
	tree = @kqml_parser.parse(@unparsed_input, :consume_all_input => false)
      end
      while (tree.nil?)
	if ((ownp or @kqml_parser.failure_index == @unparsed_input.length) and
	    not eof?)
	  @unparsed_input += readline
	  ownp = obviously_will_not_parse?(@unparsed_input)
	  unless (ownp)
	    tree =
	      @kqml_parser.parse(@unparsed_input, :consume_all_input => false)
	  end
	elsif (ownp)
	  raise "KQML obviously will not parse (odd number of quote marks)"
	else
	  raise "KQML parse error: #{@kqml_parser.failure_reason}"
	end
      end
      @unparsed_input =
        @unparsed_input[@kqml_parser.index,
	                @unparsed_input.length - @kqml_parser.index]
      @unparsed_input = nil if (@unparsed_input.nil? or @unparsed_input =~ /^\s*$/)
      return tree.to_ruby
    ensure
      @kqml_parser.send(:index=, 0)
    end
  end
end

