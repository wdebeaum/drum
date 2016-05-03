require 'socket'
require 'KQML/kqml'

module TripsModule # two different senses of "module"
  DefaultHost = '127.0.0.1'
  DefaultPort = 6200
  MaxPortTries = 100

  include Socket::Constants

  def initialize(argv)
    @argv = argv
    @auto_connect = true
  end

  def get_parameter(param, default=nil)
    @argv.each_index { |i|
      return @argv[i+1] if (@argv[i] == param)
    }
    return default
  end

  def parameter_true?(param)
    value = get_parameter(param)
    return ((not value.nil?) and (value =~ /true|t|yes/i))
  end

  def handle_common_parameters
    @host, @port = DefaultHost, DefaultPort
    if ((value = get_parameter("-connect")) != nil)
      if (value =~ /^(?:true|t|yes)$/i)
	@auto_connect = true
      elsif (value =~ /^(?:false|nil|no)$/i)
        @auto_connect = false
      elsif (value =~ /:/)
        @host, @port = $`, $'.to_i
      else
	@host = value
      end
    else
      @auto_connect = true
    end

    @scan_for_port = parameter_true?("-scan")
    @name = get_parameter("-name", (@name==nil)? nil : @name.to_s.downcase)
    @group_name = get_parameter("-group", (@group_name==nil)? nil : @group_name.downcase)
    @debugging_enabled = parameter_true?("-debug")
  end

  def connect
    if (@auto_connect)
      tries = @scan_for_port ? MaxPortTries : 1
      begin
        @socket = Socket.new(AF_INET, SOCK_STREAM, 0)
        @socket.connect(Socket.pack_sockaddr_in(@port, @host))
	@socket.set_encoding("UTF-8")
        @kqml_in = @socket
        @kqml_out = @socket
      rescue
        tries -= 1
        if (tries > 0)
	  @port += 1
	  retry
	else
	  raise "Couldn't connect to #{@host}:\n" + $!.message
	end
      end
    else # connect to stdin/out
      @kqml_in = $stdin
      @kqml_out = $stdout
      @kqml_in.set_encoding("UTF-8")
      @kqml_out.set_encoding("UTF-8")
    end
  end

  def register
    unless (@name.nil?)
      msg = KQML[:register, :name => @name.intern]
      msg[:groupName] = @group_name unless (@group_name.nil?)
      send_msg(msg)
    end
  end

  def init
    @handlers = {}
    @pending_messages = []
    @continuations = {}
    handle_common_parameters()
    connect()
    register()
  end

  def debug(msg)
    $stderr.puts(msg) if (@debugging_enabled)
  end

  def add_handler(pattern, handler, subscribe=true)
    send_msg(KQML[:subscribe, :content => pattern]) if (subscribe)
    @handlers[pattern] = handler
  end

  def remove_handler(pattern)
    @handlers.delete(pattern)
  end

  def send_with_continuation(msg, cont)
    reply_id = nil
    begin
      reply_id = (@name + rand(1000000).to_s).intern
    end while (@continuations.key?(reply_id))
    @continuations[reply_id] = cont
    send_msg(msg + KQML[:"reply-with" => reply_id])
  end

  def send_and_wait(msg)
    reply_id = nil
    begin
      reply_id = (@name + rand(1000000).to_s).intern
    end while (@continuations.key?(reply_id))
    send_msg(msg + KQML[:"reply-with" => reply_id])
    until ((message = @kqml_in.read_kqml).nil?)
      if (message[:"in-reply-to"] == reply_id)
	return message[:content]
      else
	@pending_messages << message
      end
    end
    return nil
  end

  def send_msg(msg)
    @kqml_out.write_kqml(msg)
  end

  def handle_message(message)
    if (message.key?(:"in-reply-to"))
      reply_id = message[:"in-reply-to"]
      if (@continuations.key?(reply_id))
	cont = @continuations[reply_id]
	if (cont.nil?)
	  send_msg(KQML[:error, :content => [:"duplicate-reply", reply_id]])
	else
	  @continuations.delete(reply_id)
	  cont[message]
	end
      else
	send_msg(KQML[:error, :content => [:"bad-reply-id", reply_id]])
      end
    else # not a reply, call handler
      pat = @handlers.keys.find { |pattern| pattern === message }
      if (pat.nil?)
	$stderr.puts "No handler for message: " + message.to_kqml_string
      else
	@handlers[pat].call(message)
      end
    end
  end

  def get_next_message
    @pending_messages.shift || @kqml_in.read_kqml
  end

  def run
    send_msg(KQML[:tell, :content => [:"module-status", :ready]])
    while ((message = get_next_message) != nil)
      begin
        handle_message(message)
      rescue
        send_msg(KQML[:error, :content => [$!.to_s, $!.backtrace.join("\n")]] + KQML.in_reply_to(message))
      end
    end
  end
end

