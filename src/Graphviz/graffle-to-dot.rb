#!/usr/bin/ruby
# graffle-to-dot.rb - convert an OmniGraffle .graffle XML plist file representing an LF graph to a Graphviz .dot file
# William de Beaumont

# USAGE: graffle-to-dot.rb <foo.graffle >foo.dot

require "rubygems"
require "plist"
require "session"



class Node
  attr_accessor :id, :label
  def to_dot
    %Q{  "#{@id}" [label=#{@label.inspect}]\n}
  end
end

class Edge
  attr_accessor :label, :from, :to
  def to_dot
    %Q{  "#{@from}" -> "#{@to}" [label=#{@label.inspect}]\n}
  end
end

def get_label(element)
  if (element.key?("Text") and element["Text"].key?("Text"))
    text = element["Text"]["Text"]
    text.gsub(/\\\S+/,'').gsub(/[{}]/,'').gsub(/^.*;/,'').gsub(/^\s+/,'')
  else
    $stderr.puts "Element #{element["ID"]} missing label!"
    element["ID"]
  end
end

filedata = $stdin.read
sh = Session.new
type = ''
sh.execute("file -b -", :stdin => filedata, :stdout => type)
if (type =~ /^gzip compressed data/)
# in an ideal world, this would work. unfortunately it doesn't
#  filedata = Zlib::Inflate.inflate(filedata)
  gunzipped = ''
  sh.execute("gunzip -c -", :stdin => filedata, :stdout => gunzipped)
  filedata = gunzipped
end

graffle = Plist::parse_xml(filedata)["GraphicsList"]
dot = {};
graffle.each { |element|
  if (element.key?("Line")) # edge label
    id = "N" + element["Line"]["ID"].to_s
    dot[id] = Edge.new unless (dot.key?(id))
    dot[id].label = get_label(element)
  elsif (element.key?("Head")) # edge
    id = "N" + element["ID"].to_s
    dot[id] = Edge.new unless (dot.key?(id))
    dot[id].to = "N" + element["Head"]["ID"].to_s
    if (element.key?("Tail"))
      dot[id].from = "N" + element["Tail"]["ID"].to_s
    else
      $stderr.puts "Edge #{id} missing Tail node pointer!"
      dot[id].from = id + "Tail"
    end
  elsif (element.key?("Tail"))
    id = element["ID"].to_s
    $stderr.puts "Edge #{id} missing Head node pointer!"
    dot[id] = Edge.new unless (dot.key?(id))
    dot[id].from = "N" + element["Tail"]["ID"].to_s
    dot[id].to = id + "Head"
  else # node
    id = "N" + element["ID"].to_s
    dot[id] = Node.new
    dot[id].id = id
    dot[id].label = get_label(element)
  end
}

# normalize labels
dot.keys.each { |id|
  if (dot[id].kind_of?(Node))
    if (dot[id].label =~ /^\s*\(?\s*([A-Z:_\-]+)\s+(?:\(:\*\s+)?([A-Z:_\-]+)(?:\s+([A-Za-z:_\-]+))?\s*\)?\s*\)?\s*$/)
      # a node representing an LF term
      indicator, lftype, word = $1, $2, $3
      if ($3 == nil)
	dot[id].label = "(#{indicator} #{lftype})"
      else
	dot[id].label = "(#{indicator} (:* #{lftype} #{word}))"
      end
    # else TODO normalize non-term node labels?
    end
  else # edge
    if (dot[id].label == nil)
      $stderr.puts "WARNING: edge #{id} has no label!"
    elsif (dot[id].label =~ /[a-z0-9\-]+/i)
      dot[id].label = ":#{$&}"
    else
      $stderr.puts "WARNING: strange label for edge #{id}: #{dot[id].label}"
    end
  end
}

print "digraph Terms {\n"
print "  node [shape=none]\n"
dot.keys.sort.each { |id| print dot[id].to_dot }
print "}\n"

