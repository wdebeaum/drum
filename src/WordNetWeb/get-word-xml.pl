#!/usr/bin/perl

use CGI qw/:standard/;
require "get-xml.ph";

use strict vars;

my $lang = 'en';
if (param('lang') =~ /^(es)$/) {
  $lang = $&;
  connect_to_db($lang . 'wn.db');
} else {
  connect_to_db();
}
print header(-type=>'text/xml', -charset=>'UTF-8');
print <<EOPI;
<?xml version="1.0" encoding="UTF-8"?>
<?xml-stylesheet type="text/xsl" href="wn-xml-to-xhtml.xsl"?>
EOPI
my $search = lc(param('search'));
my $show_sense_keys = param('show_sense_keys');
if ($search =~ /^([^:%\s]+)%\d:\d\d:\d\d:[^:%\s]*:(?:\d\d)?$/) { # sense key
  print_word_xml('', $1, $search, $show_sense_keys, $lang);
} else { # word
  $search =~ s/\s+/_/g;
  print_word_xml('', $search, undef, $show_sense_keys, $lang);
}
disconnect_from_db();

