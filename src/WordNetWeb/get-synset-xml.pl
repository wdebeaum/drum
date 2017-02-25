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
print qq(<?xml version="1.0" encoding="UTF-8"?>\n);
print_synset_xml('', scalar(param('synset_offset')), scalar(param('ss_type')), $lang);
disconnect_from_db();

