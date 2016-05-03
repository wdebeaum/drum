require 'uri'
require_relative 'aode'

# Manage a collection of models so that the most frequently used models stay in
# RAM, but the amount of RAM used is limited. The interface is modeled after a
# Hash mapping unescaped filenames to loaded model objects.
class Models
  # an empirical estimate of how many bytes of RAM a model will use per byte of
  # disk space
  RamPerDisk = 12

  def initialize(model_dir = File.dirname(__FILE__) + '/models',
  		 max_usage = 160_000_000)
    @model_dir, @max_usage = model_dir, max_usage
    @current_usage = 0
    @filename_to_model = {}
    @filename_to_usage = {}
    @recent_filenames = []
    @mutex = Mutex.new
    # load filenames and usages
    self.each_key('*', false) { |filename|
      @filename_to_usage[filename] =
        File.size("#{model_dir}/#{filename}.model") * RamPerDisk
    }
  end

  # TODO rename other variables in this file to better emphasize the
  # differences among filename (full path), basename, and key
  def self.basename_for_key(key)
    URI.escape(key, /[^\w-]/)
  end
  
  def include?(filename)
    @filename_to_usage.key?(Models.basename_for_key(filename))
  end
  alias_method :key?, :include?

  def [](filename)
    filename = Models.basename_for_key(filename)
    raise "no model named #{filename}" unless (@filename_to_usage.key?(filename))
    m = nil
    @mutex.synchronize {
      if (@filename_to_model.key?(filename))
	@recent_filenames.delete(filename)
	m = @filename_to_model[filename]
      else
	m = load(filename)
      end
      @recent_filenames << filename
    }
    return m 
  end

  # Iterate over [filename, model] pairs, optionally filtering filenames with a
  # shell glob pattern. If unescape=true, the yielded filenames will be
  # unescaped (but the glob must still match the escaped version).
  def each_pair(glob='*', unescape=true)
    return to_enum(__METHOD__, glob, unescape) unless (block_given?)
    Dir["#{@model_dir}/#{glob}.model"].each { |path|
      path =~ /([^\/]+)\.model$/ or raise "WTF"
      filename = $1
      unescaped_filename = URI.unescape(filename)
      yield((unescape ? unescaped_filename : filename),self[unescaped_filename])
    }
  end
  alias_method :each, :each_pair
  
  # like each_pair.collect { |k,v| k }, but don't load the models
  def each_key(glob='*', unescape=true)
    return to_enum(__METHOD__, glob, unescape) unless (block_given?)
    Dir["#{@model_dir}/#{glob}.model"].each { |path|
      path =~ /([^\/]+)\.model$/ or raise "WTF"
      filename = $1
      yield (unescape ? URI.unescape(filename) : filename)
    }
  end

private

  def load(filename)
    # first make sure we have space
    evict(@current_usage + @filename_to_usage[filename] - @max_usage)
    # then load the model
    m = @filename_to_model[filename] =
      load_model_file("#{@model_dir}/#{filename}.model")
    @current_usage += @filename_to_usage[filename]
    return m
  end

  # evict the least recently used models until min_eviction bytes have been
  # freed (note this will only actually evict a model from memory if there are
  # no other references to it)
  def evict(min_eviction)
    until (min_eviction <= 0 or @recent_filenames.empty?)
      least_recent_filename = @recent_filenames.shift
      @filename_to_model.delete(least_recent_filename)
      lr_usage = @filename_to_usage[least_recent_filename]
      @current_usage -= lr_usage
      min_eviction -= lr_usage
    end
    GC.start
  end
end

def load_model_file(path)
  File.open(path, 'r') { |f|
    return Marshal.load(f)
  }
end

def save_model_file(model, path)
  File.open(path, 'w') { |f|
    return Marshal.dump(model, f)
  }
end

