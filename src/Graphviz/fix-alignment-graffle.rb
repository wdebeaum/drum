#!/usr/bin/ruby
# fix-alignment-graffle.rb - fix the node labels of an LF alignment graph converted to .graffle format by OmniGraffle

# USAGE:
# fix-alignment-graffle.rb <broken-alignment.graffle >fixed-alignment.graffle

require "rubygems"
require "plist"
require "session"

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

graffle = Plist::parse_xml(filedata)
gl = graffle["GraphicsList"]
gl.each { |element|
  if (element.key?("Text") and element["Text"].key?("Text") and
      element["Text"]["Text"] =~ /<\/table>/)
    # fix the label...
    # set the color table
    element["Text"]["Text"].sub!(/\{\\colortbl;.*?\}/,
      '{\colortbl;\red255\green255\blue255;\red0\green100\blue0;\red128\green0\blue0;}')
    # convert the red parts
    element["Text"]["Text"].gsub!(/<font color="MAROON">(.*?)<\/font>/) { |m|
      "\\cf3 #{$1}\\cf2 "
    }
    # remove all other HTML tags
    element["Text"]["Text"].gsub!(/<.*?>/, '')
    # replace newlines with spaces, so we don't get extra indentation courtesy of to_plist :-P
    element["Text"]["Text"].gsub!(/[\r\n]/, ' ')

    # fix the dimensions
    x1,y1,width,height = element["Bounds"].scan(/-?\d*\.?\d+/).collect { |s| s.to_f }
    x1 += (width - 200) / 2
    width = 200
    element["Bounds"] = "{{#{x1}, #{y1}}, {#{width}, #{height}}}"
  end
}

print graffle.to_plist

