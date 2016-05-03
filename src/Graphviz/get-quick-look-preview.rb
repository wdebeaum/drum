#!/usr/bin/ruby
# get-quick-look-preview.rb - Get the QuickLookPreview attribute from a (possibly gzipped) Mac plist file
# (for OmniGraffle Pro 5 .graffle files, this is a PDF version of the graph)
# 2008-06-25
# William de Beaumont

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

print graffle["QuickLookPreview"].read
